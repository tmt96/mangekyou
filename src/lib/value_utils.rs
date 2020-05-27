use inkwell::values::*;

pub trait AnyValueExt<'ctx> {
    fn as_basic_value_enum(self) -> Result<BasicValueEnum<'ctx>, String>;
}

impl<'ctx> AnyValueExt<'ctx> for AnyValueEnum<'ctx> {
    fn as_basic_value_enum(self) -> Result<BasicValueEnum<'ctx>, String> {
        match self {
            AnyValueEnum::ArrayValue(val) => Ok(val.into()),
            AnyValueEnum::IntValue(val) => Ok(val.into()),
            AnyValueEnum::FloatValue(val) => Ok(val.into()),
            AnyValueEnum::StructValue(val) => Ok(val.into()),
            AnyValueEnum::VectorValue(val) => Ok(val.into()),
            AnyValueEnum::PointerValue(val) => Ok(val.into()),
            _ => Err(format!("Expected a basic value, found {:?}", self)),
        }
    }
}

pub trait BasicValueExt {
    fn get_name(&self) -> String;
}

impl BasicValueExt for BasicValueEnum<'_> {
    fn get_name(&self) -> String {
        match self {
            BasicValueEnum::ArrayValue(val) => val.get_name().to_string_lossy().into_owned(),
            BasicValueEnum::IntValue(val) => val.get_name().to_string_lossy().into_owned(),
            BasicValueEnum::FloatValue(val) => val.get_name().to_string_lossy().into_owned(),
            BasicValueEnum::PointerValue(val) => val.get_name().to_string_lossy().into_owned(),
            BasicValueEnum::StructValue(val) => val.get_name().to_string_lossy().into_owned(),
            BasicValueEnum::VectorValue(val) => val.get_name().to_string_lossy().into_owned(),
        }
    }
}
