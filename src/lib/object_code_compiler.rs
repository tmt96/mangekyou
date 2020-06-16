use crate::codegen::*;
use inkwell::{context::Context, targets::*, OptimizationLevel};
use std::io::{stdin, stdout, Write};
use std::path::Path;

pub fn compile() -> Result<(), String> {
    let init_config = InitializationConfig::default();
    Target::initialize_native(&init_config)?;
    let target_triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&target_triple).map_err(|err| err.to_string())?;

    let target_machine = target
        .create_target_machine(
            &target_triple,
            "generic",
            "",
            OptimizationLevel::Default,
            RelocMode::Default,
            CodeModel::Default,
        )
        .unwrap();

    let context = Context::create();
    let generator = IRGenerator::new(&context);
    let module = generator.get_module();
    module.set_data_layout(&target_machine.get_target_data().get_data_layout());
    module.set_triple(&target_triple);

    target_machine
        .write_to_file(module, FileType::Object, Path::new("./output.o"))
        .map_err(|err| err.to_string())?;

    Ok(())
}
