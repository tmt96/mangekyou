use crate::ast::*;
use crate::parser::*;
use crate::value_utils::*;
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    passes::*,
    types::*,
    values::*,
    FloatPredicate,
};
use std::collections::HashMap;

type CodegenResult<T> = Result<T, String>;

pub struct IRGenerator<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    pass_manager: PassManager<FunctionValue<'ctx>>,
    named_values: HashMap<String, BasicValueEnum<'ctx>>,
    op_precedence_map: HashMap<AstBinaryOp, i32>,
}

impl<'ctx> IRGenerator<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("jit");
        let named_values = HashMap::new();

        let pass_manager = PassManager::create(&module);
        // Do simple "peephole" optimizations and bit-twiddling optzns.
        pass_manager.add_instruction_combining_pass();
        // Reassociate expressions.
        pass_manager.add_reassociate_pass();
        // Eliminate Common SubExpressions.
        pass_manager.add_gvn_pass();
        // Simplify the control flow graph (deleting unreachable blocks, etc).
        pass_manager.add_cfg_simplification_pass();
        pass_manager.initialize();

        Self {
            context,
            builder,
            module,
            pass_manager,
            named_values,
            op_precedence_map: Self::init_op_pred_map(),
        }
    }

    fn init_op_pred_map() -> HashMap<AstBinaryOp, i32> {
        [
            (AstBinaryOp::Lt, 10),
            (AstBinaryOp::Gt, 10),
            (AstBinaryOp::Eq, 10),
            (AstBinaryOp::Add, 20),
            (AstBinaryOp::Sub, 20),
            (AstBinaryOp::Mul, 40),
            (AstBinaryOp::Div, 40),
        ]
        .iter()
        .cloned()
        .collect()
    }

    pub fn compile(&mut self, inst: &str) -> CodegenResult<Option<AnyValueEnum<'ctx>>> {
        let mut parser = Parser::from_source(inst, &self.op_precedence_map);
        parser.parse()?.codegen(self)
    }
}

pub trait CodeGen<'ctx> {
    type GeneratedType;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<Self::GeneratedType>;
}

impl<'ctx> CodeGen<'ctx> for BinaryExpr {
    type GeneratedType = AnyValueEnum<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<Self::GeneratedType> {
        let BinaryExpr { op, lhs, rhs } = self;
        let lhs = lhs.codegen(generator)?;
        let rhs = rhs.codegen(generator)?;
        if let (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) = (lhs, rhs) {
            let result = match op {
                AstBinaryOp::Add => generator.builder.build_float_add(lhs, rhs, "addtmp").into(),
                AstBinaryOp::Sub => generator.builder.build_float_sub(lhs, rhs, "subrmp").into(),
                AstBinaryOp::Mul => generator.builder.build_float_mul(lhs, rhs, "multmp").into(),
                AstBinaryOp::Div => generator.builder.build_float_div(lhs, rhs, "divtmp").into(),
                AstBinaryOp::Lt => {
                    let tmp_value = generator.builder.build_float_compare(
                        FloatPredicate::ULT,
                        lhs,
                        rhs,
                        "lttmp",
                    );
                    generator
                        .builder
                        .build_unsigned_int_to_float(
                            tmp_value,
                            generator.context.f64_type(),
                            "lttmp",
                        )
                        .into()
                }
                AstBinaryOp::Gt => {
                    let tmp_value = generator.builder.build_float_compare(
                        FloatPredicate::UGT,
                        lhs,
                        rhs,
                        "gttmp",
                    );
                    generator
                        .builder
                        .build_unsigned_int_to_float(
                            tmp_value,
                            generator.context.f64_type(),
                            "gttmp",
                        )
                        .into()
                }
                AstBinaryOp::Eq => {
                    let tmp_value = generator.builder.build_float_compare(
                        FloatPredicate::UEQ,
                        lhs,
                        rhs,
                        "eqtmp",
                    );
                    generator
                        .builder
                        .build_unsigned_int_to_float(
                            tmp_value,
                            generator.context.f64_type(),
                            "eqtmp",
                        )
                        .into()
                }
                AstBinaryOp::Custom(op) => {
                    let func = generator
                        .module
                        .get_function(&format!("binary{}", op))
                        .ok_or_else(|| "Binary operator not found".to_string())?;
                    generator
                        .builder
                        .build_call(func, &[lhs.into(), rhs.into()], "binop")
                        .as_any_value_enum()
                }
            };
            Ok(result)
        } else {
            Err(format!(
                "Expected two number values, found {:?} and {:?}",
                lhs, rhs
            ))
        }
    }
}

impl<'ctx> CodeGen<'ctx> for UnaryExpr {
    type GeneratedType = AnyValueEnum<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<Self::GeneratedType> {
        let op_value = self.operand.codegen(generator)?;
        let func = generator
            .module
            .get_function(&format!("unary{}", self.op))
            .ok_or_else(|| "Unary operator not found".to_string())?;
        Ok(generator
            .builder
            .build_call(func, &[op_value.as_basic_value_enum()?], "unop")
            .as_any_value_enum())
    }
}

impl<'ctx> CodeGen<'ctx> for CallExpr {
    type GeneratedType = CallSiteValue<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<Self::GeneratedType> {
        let CallExpr { callee, args } = self;
        let callee_func = generator
            .module
            .get_function(callee)
            .ok_or_else(|| "Unknown function referenced".to_string())?;
        if callee_func.count_params() as usize != args.len() {
            return Err("Incorrect number of args passed".to_string());
        }
        let args_list: Result<Vec<_>, _> = args
            .iter()
            .map(|arg| arg.codegen(generator)?.as_basic_value_enum())
            .collect();

        Ok(generator
            .builder
            .build_call(callee_func, &args_list?, "calltmp"))
    }
}

impl<'ctx> CodeGen<'ctx> for IfExpr {
    type GeneratedType = PhiValue<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<Self::GeneratedType> {
        let IfExpr {
            cond,
            then_branch,
            else_branch,
        } = self;
        let zero_const = generator.context.f64_type().const_float(0.0);
        let cond = cond.codegen(generator)?;
        let cond = generator.builder.build_float_compare(
            FloatPredicate::ONE,
            cond.into_float_value(),
            zero_const,
            "ifcond",
        );

        let function = generator
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let then_bb = generator.context.append_basic_block(function, "then");
        let else_bb = generator.context.append_basic_block(function, "else");
        let merge_bb = generator.context.append_basic_block(function, "ifcont");
        generator
            .builder
            .build_conditional_branch(cond, then_bb, else_bb);

        generator.builder.position_at_end(then_bb);
        let then_value = then_branch.codegen(generator)?.as_basic_value_enum()?;
        generator.builder.build_unconditional_branch(merge_bb);
        let then_bb = generator.builder.get_insert_block().unwrap();

        generator.builder.position_at_end(else_bb);
        let else_value = else_branch.codegen(generator)?.as_basic_value_enum()?;
        generator.builder.build_unconditional_branch(merge_bb);
        let else_bb = generator.builder.get_insert_block().unwrap();

        generator.builder.position_at_end(merge_bb);
        let phi_value = generator
            .builder
            .build_phi(generator.context.f64_type(), "iftmp");
        phi_value.add_incoming(&[(&then_value, then_bb), (&else_value, else_bb)]);
        Ok(phi_value)
    }
}

impl<'ctx> CodeGen<'ctx> for ForExpr {
    type GeneratedType = PhiValue<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<Self::GeneratedType> {
        let ForExpr {
            var_name,
            start,
            end,
            step,
            body,
        } = self;
        let start_val = start.codegen(generator)?.as_basic_value_enum()?;
        let function = generator
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let preheader_bb = generator.builder.get_insert_block().unwrap();

        let loop_bb = generator.context.append_basic_block(function, "loop");
        generator.builder.build_unconditional_branch(loop_bb);
        generator.builder.position_at_end(loop_bb);

        let variable = generator
            .builder
            .build_phi(generator.context.f64_type(), var_name);
        variable.add_incoming(&[(&start_val, preheader_bb)]);

        let old_value = generator
            .named_values
            .insert(var_name.to_owned(), variable.as_basic_value());

        body.codegen(generator)?;
        let step_value = match step {
            Some(step) => step.codegen(generator)?,
            None => generator.context.f64_type().const_float(1.0).into(),
        };
        let next_var = generator.builder.build_float_add(
            variable.get_incoming(0).unwrap().0.into_float_value(),
            step_value.into_float_value(),
            "nextvar",
        );

        let end_cond = end.codegen(generator)?;
        let end_cond = generator.builder.build_float_compare(
            FloatPredicate::ONE,
            end_cond.into_float_value(),
            generator.context.f64_type().const_float(0.0),
            "loopcond",
        );
        let loop_end_bb = generator.builder.get_insert_block().unwrap();
        let after_bb = generator.context.append_basic_block(function, "afterloop");
        generator
            .builder
            .build_conditional_branch(end_cond, loop_bb, after_bb);
        generator.builder.position_at_end(after_bb);

        variable.add_incoming(&[(&next_var, loop_end_bb)]);
        if let Some(old_val) = old_value {
            generator.named_values.insert(var_name.to_owned(), old_val);
        } else {
            generator.named_values.remove(var_name);
        }

        Ok(variable)
    }
}

impl<'ctx> CodeGen<'ctx> for Expr {
    type GeneratedType = AnyValueEnum<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<Self::GeneratedType> {
        match self {
            Expr::Number(i) => Ok(generator.context.f64_type().const_float(*i).into()),
            Expr::Variable(name) => generator
                .named_values
                .get(name)
                .map(|val| val.as_any_value_enum())
                .ok_or_else(|| format! {"Value not found: {}", name}),
            Expr::Binary(expr) => expr.codegen(generator),
            Expr::Unary(expr) => expr.codegen(generator),
            Expr::Call(expr) => expr.codegen(generator).map(|val| val.as_any_value_enum()),
            Expr::If(expr) => expr.codegen(generator).map(|val| val.into()),
            Expr::For(expr) => expr.codegen(generator).map(|val| val.into()),
        }
    }
}

impl<'ctx> CodeGen<'ctx> for Prototype {
    type GeneratedType = FunctionValue<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
        let Prototype {
            name,
            args,
            prototype_type,
            precedence,
        } = self;
        let double_type = generator.context.f64_type();
        let arg_types = vec![double_type.as_basic_type_enum(); args.len()];
        if let PrototypeType::Binary = prototype_type {
            generator.op_precedence_map.insert(
                AstBinaryOp::Custom(self.get_op_name()?),
                precedence.unwrap_or(30),
            );
        }

        let fn_type = double_type.fn_type(&arg_types, false);
        let func = generator
            .module
            .add_function(name, fn_type, Some(Linkage::External));

        for (i, arg) in func.get_param_iter().enumerate() {
            arg.set_name(&self.args[i])
        }
        Ok(func)
    }
}

impl<'ctx> CodeGen<'ctx> for Function {
    type GeneratedType = FunctionValue<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<FunctionValue<'ctx>> {
        let func_value = generator
            .module
            .get_function(&self.proto.get_name())
            .unwrap_or(self.proto.codegen(generator)?);

        let block = generator.context.append_basic_block(func_value, "entry");
        generator.builder.position_at_end(block);

        generator.named_values = func_value
            .get_params()
            .iter()
            .map(|arg| (arg.get_name(), *arg))
            .collect();

        let body = self.body.codegen(generator)?;
        generator
            .builder
            .build_return(Some(&body.as_basic_value_enum()?));

        if func_value.verify(true) {
            generator.pass_manager.run_on(&func_value);
            Ok(func_value)
        } else {
            unsafe {
                func_value.delete();
            }
            Err("Invalid generated function".to_string())
        }
    }
}

impl<'ctx> CodeGen<'ctx> for AstNode {
    type GeneratedType = Option<AnyValueEnum<'ctx>>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> CodegenResult<Self::GeneratedType> {
        match self {
            AstNode::FunctionNode(func) => func.codegen(generator).map(|val| Some(val.into())),
            AstNode::PrototypeNode(func) => func.codegen(generator).map(|val| Some(val.into())),
            _ => Ok(None),
        }
    }
}
