#include <verilated.h>
#include "VmkSimpleIOCapExposerV4_noblock_Tb.h"

#include "exposer_tests_uvm.h"
#include "exposer_tests_legacy.h"

using TheDUT = VmkSimpleIOCapExposerV4_noblock_Tb;
constexpr bool expectPassthroughInvalidTransactions = true;

int main(int argc, char** argv) {
    std::vector<TestBase*> tests = {
        // UVM-style testing
        // TODO add tests for above todos, consider revocation
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_11>(CCapPerms_Read),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_11>(CCapPerms_Write),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapValidAccess<TheDUT, CapType::Cap2024_11>(CCapPerms_ReadWrite),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, CapType::Cap2024_11>(CCapPerms_Read),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidInitialCapOOBAccess<TheDUT, CapType::Cap2024_11>(CCapPerms_Write),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyValidCapBadPerms<TheDUT, CapType::Cap2024_11>(),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMValidKeyBadSigCap<TheDUT, CapType::Cap2024_11>(),
            expectPassthroughInvalidTransactions
        ),
        new ExposerUVMishTest(
            new UVMInvalidKeyAccess<TheDUT, CapType::Cap2024_11>(CCapPerms_ReadWrite),
            expectPassthroughInvalidTransactions
        ),
        // 5 cycles of revocations
        // TODO test this with valid and invalid transactions!
        // TODO figure out how to do more accurate UVM revocation testing
        new ExposerUVMishTest(
            new UVMTransactionsBetweenRevocations<TheDUT, CapType::Cap2024_11>(5),
            expectPassthroughInvalidTransactions
        ),

        new ExposerUVMishTest(
            new UVMStreamOfNValidTransactions<TheDUT, CapType::Cap2024_11>(CCapPerms_ReadWrite, 100),
            expectPassthroughInvalidTransactions
        )
    };

    return tb_main(tests, argc, argv);
}