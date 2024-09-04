import IOCapAxi_Exposers :: *;
import Tests :: *;

(* synthesize *)
module mkSimpleIOCapExposerV2_Tb(SimpleIOCapExposerTb);
    let keyStores <- mkKeyStoreShim;
    let exposer4x32Impl <- mkSimpleIOCapExposerV2(tpl_2(keyStores));

    interface keyStoreShim = tpl_1(keyStores);
    interface exposer4x32 = exposer4x32Impl;
endmodule
