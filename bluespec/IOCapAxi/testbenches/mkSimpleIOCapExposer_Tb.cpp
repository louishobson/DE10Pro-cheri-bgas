#include <verilated.h>
#include "VmkSimpleIOCapExposer_Tb.h"

#include "exposer_tests_uvm.h"
#include "exposer_tests_legacy.h"

int main(int argc, char** argv) {
    int ccapresult_success = EXIT_SUCCESS;

    std::vector<TestBase*> tests = {
        // Test valid caps are accepted - DONE below
        // new ValidKeyValidCapValidWrite<VmkSimpleIOCapExposer_Tb>(),
        // new ValidKeyValidCapValidRead<VmkSimpleIOCapExposer_Tb>(),
        // new ValidReadThenValidWrite<VmkSimpleIOCapExposer_Tb>(),
        // TODO Test caps with invalid keys are rejected - DONE below
        // TODO test valid cap with 1 cav
        // TODO test valid cap with 2 cav
        // Test valid cap with out-of-cap-bounds access - DONE below
        // new OOBWrite_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        // new OOBRead_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        // Test valid cap with mismatched perms - DONE below
        // new MismatchedPerms_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        // Test invalid caps (i.e. bad signatures) with valid keys are rejected - DONE below
        // new InvalidSig_Passthrough<VmkSimpleIOCapExposer_Tb>(),

        // TODO test that invalid caps don't let their flits through (contingent on switch flip) - DONE below
        // TODO test the above for reads and writes - DONE? below

        // TODO test inbalanced completions > starts behaviour

        // Test new-epoch when no accesses are pending
        new NewEpoch_NoAccesses<VmkSimpleIOCapExposer_Tb>(),
        // Test new-epoch when an access hasn't started checking
        new NewEpoch_PreAccess<VmkSimpleIOCapExposer_Tb>(),
        // New-epoch while processing an access (either at the same cycle as the access starts, or while it's processing)
        // will delay the completion of the epoch to after the access completes
        new NewEpoch_SameCycle<VmkSimpleIOCapExposer_Tb>(),
        new NewEpoch_PostAccess<VmkSimpleIOCapExposer_Tb>(),
        // New-Epoch between accesses that arrive on consecutive cycles will delay the second until the first has completed
        new NewEpoch_BetweenAccesses<VmkSimpleIOCapExposer_Tb>(),
        // TODO test the above for reads and writes



        // UVM-style testing
        // TODO add tests for above todos, consider revocation
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<VmkSimpleIOCapExposer_Tb>(CCapPerms_Read)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<VmkSimpleIOCapExposer_Tb>(CCapPerms_Write)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<VmkSimpleIOCapExposer_Tb>(CCapPerms_ReadWrite)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<VmkSimpleIOCapExposer_Tb>(CCapPerms_Read)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<VmkSimpleIOCapExposer_Tb>(CCapPerms_Write)
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidCapBadPerms<VmkSimpleIOCapExposer_Tb>()
        ),
        new ExposerUVMishTest(
            new UVMValidKeyBadSigCap<VmkSimpleIOCapExposer_Tb>()
        ),
        new ExposerUVMishTest(
            new UVMInvalidKeyAccess<VmkSimpleIOCapExposer_Tb>(CCapPerms_ReadWrite)
        ),
        // 5 cycles of revocations
        // TODO test this with valid and invalid transactions!
        new ExposerUVMishTest(
            new UVMTransactionsBetweenRevocations<VmkSimpleIOCapExposer_Tb>(5)
        )
    };
    for (auto* test : tests) {
        if (!test->run(argc, argv)) {
            ccapresult_success = EXIT_FAILURE;
        }
    }

    return ccapresult_success;
}