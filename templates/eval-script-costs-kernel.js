let uplc_wasm_module;
let evalScriptsCostsKernel = (elm_args) => {
  try {
    if (!uplc_wasm_module) {
      if (typeof window !== "undefined" && window.document) {
        // Browser environment
        if (!("uplc_wasm" in window)) {
          throw new Error(
            "Missing uplc_wasm module in window. Make sure to dynamically import the Elm app after window.uplc_wasm was populated.",
          );
        }
        uplc_wasm_module = window.uplc_wasm;
      } else if (
        typeof process !== "undefined" &&
        process.versions &&
        process.versions.node
      ) {
        // Node.js environment
        uplc_wasm_module = require("./pkg-uplc-wasm/pkg-node/uplc_wasm.js");
      } else {
        throw new Error("Unknown environment");
      }
    }
    const args = elm_args.a; // elm uses a specific structure for its data
    const fromHexString = (hexString) =>
      Uint8Array.from(
        hexString.match(/.{1,2}/g).map((byte) => parseInt(byte, 16)),
      );
    let redeemers = uplc_wasm_module.eval_phase_two_raw(
      fromHexString(args.tx_bytes), // tx_bytes: &[u8],
      args.utxos_refs_bytes.map(fromHexString), // utxos_refs_bytes: Vec<js_sys::Uint8Array>,
      args.utxos_outputs_bytes.map(fromHexString), // utxos_outputs_bytes: Vec<js_sys::Uint8Array>,
      fromHexString(args.cost_mdls_bytes), // cost_mdls_bytes: &[u8],
      BigInt(args.cpu_budget), // cpu_budget: u64,
      BigInt(args.mem_budget), // mem_budget: u64,
      BigInt(args.slot_config_zero_time), // slot_config_zero_time: u64,
      BigInt(args.slot_config_zero_slot), // slot_config_zero_slot: u64,
      args.slot_config_slot_length, // slot_config_slot_length: u32,
    );
    const uint8ArrayToHexString = (uint8Array) => {
      return Array.from(uint8Array)
        .map((i) => i.toString(16).padStart(2, "0"))
        .join("");
    };
    const redeemersAsHex = redeemers.map(uint8ArrayToHexString);
    return $elm$core$Result$Ok(_List_fromArray(redeemersAsHex));
  } catch (error) {
    return $elm$core$Result$Err(
      "Script evaluation failed with error: " + error,
    );
  }
};
