use anyhow::Context;
use argh::FromArgs;
use std::fmt::Debug;
use std::fs;
use std::process::Command as ProcessCommand;

#[derive(FromArgs, PartialEq, Debug)]
/// Top-level command.
struct Command {
    #[argh(subcommand)]
    command: SubCommand,
}

#[derive(FromArgs, PartialEq, Debug)]
#[argh(subcommand)]
enum SubCommand {
    Make(MakeSubCommand),
    Run(RunSubCommand),
}

#[derive(FromArgs, PartialEq, Debug)]
/// Make subcommand.
#[argh(subcommand, name = "make")]
struct MakeSubCommand {
    #[argh(positional)]
    /// the main Elm file to compile.
    source: String,

    #[argh(option)]
    /// specify the name of the resulting JS file.
    /// For example
    /// --output=assets/elm.js to generate the JS at assets/elm.js or
    /// --output=/dev/null to generate no output at all!
    output: String,

    #[argh(switch)]
    /// turn on the time-travelling debugger. It allows you to rewind and replay events.
    /// The events can be imported/exported into a file,
    /// which makes for very precise bug reports!
    debug: bool,

    #[argh(switch)]
    /// turn on optimizations to make code smaller and faster.
    /// For example, the compiler renames record fields to be as short as possible
    /// and unboxes values to reduce allocation.
    optimize: bool,
}

#[derive(FromArgs, PartialEq, Debug)]
/// Run subcommand.
#[argh(subcommand, name = "run")]
struct RunSubCommand {
    #[argh(switch)]
    /// whether to fooey
    fooey: bool,
}

fn main() -> anyhow::Result<()> {
    let Command { command }: Command = argh::from_env();
    match command {
        SubCommand::Make(make_args) => make_subcommand(make_args)?,
        SubCommand::Run(run_args) => run_subcommand(run_args)?,
    }
    Ok(())
}

/// Validate the subcommand and call the `elm` binary with the same arguments if valid.
fn make_subcommand(make_args: MakeSubCommand) -> anyhow::Result<()> {
    // Validate output path
    if make_args.output.is_empty() {
        anyhow::bail!("Output path cannot be empty");
    }

    // Construct the elm command
    let mut cmd = ProcessCommand::new("elm");
    cmd.arg("make");

    // Add source file
    cmd.arg(&make_args.source);

    // Add output option
    cmd.arg("--output").arg(&make_args.output);

    // Add debug flag if set
    if make_args.debug {
        cmd.arg("--debug");
    }

    // Add optimize flag if set
    if make_args.optimize {
        cmd.arg("--optimize");
    }

    // Execute the command and stream output
    let status = cmd.status().context("Failed to execute elm make command")?;
    if !status.success() {
        anyhow::bail!("Compilation failed!")
    }

    // Read the elm compiled file
    let compiled_file = fs::read_to_string(&make_args.output)?;

    // Convert the compiled output into an ES module
    let es_module = into_es_module(&compiled_file);

    // Apply kernel patching of the ES module
    // to be able to call the uplc_wasm code synchronously.
    let patched_module = kernel_patching_uplc_wasm(&es_module);

    // Overwrite the compiled output file
    fs::write(&make_args.output, &patched_module)?;

    Ok(())
}

fn run_subcommand(run_args: RunSubCommand) -> anyhow::Result<()> {
    todo!()
}

/// Kernel patching the ES module with uplc_wasm
fn kernel_patching_uplc_wasm(elm_js: &str) -> String {
    let header = r#"
let evalScriptsCostsKernel = (_) => {
  console.log("Not replaced yet");
  return $elm$core$Result$Err('evalScriptsCostsKernel');
};
    "#;
    let old_body = "return $elm$core$Result$Err('evalScriptsCostsKernel');";
    let new_body = "return evalScriptsCostsKernel(args);";
    let footer = r#"
export const elmCardanoKernelPatching = (uplc_wasm) => {
  evalScriptsCostsKernel = (_) => {
    console.log("Kernel patching has worked!");
    return $elm$core$Result$Err('evalScriptsCostsKernel');
  };
};
    "#;
    [header, &elm_js.replacen(old_body, new_body, 1), footer].join("\n")
}

/// Convert a JS file resulting from an Elm compilation into an ES module.
fn into_es_module(elm_js: &str) -> String {
    // Remove the function wrapping at the beginning.
    // Remove everything thing before the "use strict".
    let use_strict_offset = elm_js.find("'use strict'").unwrap();
    // replace ';}(this));' by ';' at the end.
    let last_this_offset = elm_js.rfind("}(this").unwrap();
    [
        "const scope = {};",
        &elm_js[use_strict_offset..last_this_offset],
        "export const { Elm } = scope;",
    ]
    .join("\n")
}
