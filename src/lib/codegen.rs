use crate::ast::*;
use crate::token::*;
use inkwell::{builder::Builder, context::Context, module::Module, values::*, FloatPredicate};
use std::collections::HashMap;

pub struct IRGenerator<'ctx> {
    context: &'ctx Context,
    builder: Builder<'ctx>,
    module: Module<'ctx>,
    named_values: HashMap<String, AnyValueEnum<'ctx>>,
}

impl<'ctx> IRGenerator<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let builder = context.create_builder();
        let module = context.create_module("jit");
        let named_values = HashMap::new();
        Self {
            context,
            builder,
            module,
            named_values,
        }
    }

    pub fn codegen_expr(&self, expr: &Expr) -> Result<AnyValueEnum<'ctx>, String> {
        match expr {
            Expr::Number(i) => Ok(self.context.f64_type().const_float(*i).as_any_value_enum()),
            Expr::Variable(name) => self
                .named_values
                .get(name)
                .copied()
                .ok_or_else(|| format! {"Value not found: {}", name}),
            Expr::Binary { op, lhs, rhs } => {
                let lhs = self.codegen_expr(lhs)?;
                let rhs = self.codegen_expr(rhs)?;
                match (lhs, rhs) {
                    (AnyValueEnum::FloatValue(lhs), AnyValueEnum::FloatValue(rhs)) => {
                        Ok(self.codegen_op(*op, lhs, rhs))
                    }
                    _ => Err(format!(
                        "Expected two number values, found {:?} and {:?}",
                        lhs, rhs
                    )),
                }
            }
            Expr::Call { callee, args } => {
                let callee_func = self
                    .module
                    .get_function(callee)
                    .ok_or_else(|| "Unknown function referenced".to_string())?;
                if callee_func.count_params() as usize != args.len() {
                    return Err("Incorrect number of args passed".to_string());
                }
                let args_list: Result<Vec<_>, _> = args
                    .iter()
                    .map(|arg| {
                        let val = self.codegen_expr(arg)?;
                        Self::as_basic_value_enum(val)
                    })
                    .collect();

                Ok(self
                    .builder
                    .build_call(callee_func, &args_list?, "calltmp")
                    .as_any_value_enum())
            }
            _ => Err("Invalid binary operator".to_string()),
        }
    }

    fn codegen_op(
        &self,
        op: BinaryOp,
        lhs: FloatValue<'ctx>,
        rhs: FloatValue<'ctx>,
    ) -> AnyValueEnum<'ctx> {
        match op {
            BinaryOp::Add => self
                .builder
                .build_float_add(lhs, rhs, "addtmp")
                .as_any_value_enum(),
            BinaryOp::Sub => self
                .builder
                .build_float_sub(lhs, rhs, "subrmp")
                .as_any_value_enum(),
            BinaryOp::Mul => self
                .builder
                .build_float_mul(lhs, rhs, "multmp")
                .as_any_value_enum(),
            BinaryOp::Div => self
                .builder
                .build_float_div(lhs, rhs, "divtmp")
                .as_any_value_enum(),
            BinaryOp::Lt => self
                .builder
                .build_float_compare(FloatPredicate::ULT, lhs, rhs, "lttmp")
                .as_any_value_enum(),
            BinaryOp::Gt => self
                .builder
                .build_float_compare(FloatPredicate::UGT, lhs, rhs, "gttmp")
                .as_any_value_enum(),
        }
    }

    fn as_basic_value_enum(value: AnyValueEnum) -> Result<BasicValueEnum, String> {
        match value {
            AnyValueEnum::ArrayValue(val) => Ok(val.as_basic_value_enum()),
            AnyValueEnum::IntValue(val) => Ok(val.as_basic_value_enum()),
            AnyValueEnum::FloatValue(val) => Ok(val.as_basic_value_enum()),
            AnyValueEnum::StructValue(val) => Ok(val.as_basic_value_enum()),
            AnyValueEnum::VectorValue(val) => Ok(val.as_basic_value_enum()),
            AnyValueEnum::PointerValue(val) => Ok(val.as_basic_value_enum()),
            _ => Err(format!("Expected a basic value, found {:?}", value)),
        }
    }
}
