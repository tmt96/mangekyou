use crate::ast::*;
use crate::parser::*;
use crate::token::*;
use crate::value_utils::*;
use inkwell::{
    builder::Builder,
    context::Context,
    module::{Linkage, Module},
    types::*,
    values::*,
    FloatPredicate,
};
use std::collections::HashMap;

pub struct IRGenerator<'ctx> {
    parser: Parser<'ctx>,
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    named_values: HashMap<String, BasicValueEnum<'ctx>>,
}

impl<'ctx> IRGenerator<'ctx> {
    pub fn from_parser(context: &'ctx Context, parser: Parser<'ctx>) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("jit");
        let named_values = HashMap::new();
        Self {
            parser,
            context,
            builder,
            module,
            named_values,
        }
    }

    pub fn from_source(context: &'ctx Context, source: &'ctx str) -> Self {
        Self::from_parser(context, Parser::from_source(source))
    }

    pub fn compile_single_inst(&mut self) -> Result<Option<AnyValueEnum<'ctx>>, String> {
        self.parser.parse()?.codegen(self)
    }

    pub fn compile_loop(&mut self) -> Result<Vec<AnyValueEnum<'ctx>>, String> {
        let mut val_vec = Vec::new();
        loop {
            let val = self.compile_single_inst()?;
            if let Some(val) = val {
                val_vec.push(val);
            } else {
                return Ok(val_vec);
            }
        }
    }

    fn codegen_op(
        &self,
        op: BinaryOp,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> AnyValueEnum<'ctx> {
        match op {
            BinaryOp::Add => self.builder.build_float_add(lhs, rhs, "addtmp").into(),
            BinaryOp::Sub => self.builder.build_float_sub(lhs, rhs, "subrmp").into(),
            BinaryOp::Mul => self.builder.build_float_mul(lhs, rhs, "multmp").into(),
            BinaryOp::Div => self.builder.build_float_div(lhs, rhs, "divtmp").into(),
            BinaryOp::Lt => self
                .builder
                .build_float_compare(FloatPredicate::ULT, lhs, rhs, "lttmp")
                .into(),
            BinaryOp::Gt => self
                .builder
                .build_float_compare(FloatPredicate::UGT, lhs, rhs, "gttmp")
                .into(),
        }
    }
}

pub trait CodeGen<'ctx> {
    type GeneratedType;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> Result<Self::GeneratedType, String>;
}

impl<'ctx> CodeGen<'ctx> for Expr {
    type GeneratedType = AnyValueEnum<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> Result<AnyValueEnum<'ctx>, String> {
        match self {
            Expr::Number(i) => Ok(generator.context.f64_type().const_float(*i).into()),
            Expr::Variable(name) => generator
                .named_values
                .get(name)
                .map(|val| val.as_any_value_enum())
                .ok_or_else(|| format! {"Value not found: {}", name}),
            Expr::Binary { op, lhs, rhs } => {
                let lhs = lhs.codegen(generator)?;
                let rhs = rhs.codegen(generator)?;
                match (lhs, rhs) {
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => {
                        Ok(generator.codegen_op(*op, lhs, rhs))
                    }
                    _ => Err(format!(
                        "Expected two number values, found {:?} and {:?}",
                        lhs, rhs
                    )),
                }
            }
            Expr::Call { callee, args } => {
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
                    .build_call(callee_func, &args_list?, "calltmp")
                    .as_any_value_enum())
            }
        }
    }
}

impl<'ctx> CodeGen<'ctx> for Prototype {
    type GeneratedType = FunctionValue<'ctx>;

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> Result<FunctionValue<'ctx>, String> {
        let Prototype { name, args } = self;
        let double_type = generator.context.f64_type();
        let arg_types = vec![double_type.as_basic_type_enum(); args.len()];

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

    fn codegen(&self, generator: &mut IRGenerator<'ctx>) -> Result<FunctionValue<'ctx>, String> {
        let func_value = generator
            .module
            .get_function(&self.proto.get_name())
            .unwrap_or(self.proto.codegen(generator)?);

        // if !func_value.is_null() {
        //     return Err("Function cannot be redefined.".to_string());
        // }

        let block = generator.context.append_basic_block(func_value, "entry");
        generator.builder.position_at_end(&block);

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

    fn codegen(
        &self,
        generator: &mut IRGenerator<'ctx>,
    ) -> Result<Option<AnyValueEnum<'ctx>>, String> {
        match self {
            AstNode::FunctionNode(func) => func.codegen(generator).map(|val| Some(val.into())),
            AstNode::PrototypeNode(func) => func.codegen(generator).map(|val| Some(val.into())),
            _ => Ok(None),
        }
    }
}
