#include <verilated.h>
#include "VmkCap2024_11_Decode_Comb_Tb.h"

#include "decode_tests_uvm.h"

using TheDUT = VmkCap2024_11_Decode_Comb_Tb;

int main(int argc, char** argv) {
    std::vector<TestBase*> tests = {
        new DecoderUVMishTest_11<TheDUT>(new ManyRandomValidCaps_11<TheDUT>(2000)),  
        new DecoderUVMishTest_11<TheDUT>(new ManyLibRustRandomValidCaps_11<TheDUT>(2000)),  
        new DecoderUVMishTest_11<TheDUT>(new ManyRandomBits<TheDUT>(2000)),  
    };

    for (auto edge_case = 0; edge_case < ccap2024_11_rand_edge_case_num(); edge_case++) {
        tests.push_back(
            new DecoderUVMishTest_11<TheDUT>(new ManyLibRustEdgeCaseCaps_11<TheDUT>(2000, edge_case))
        );
    }

    return tb_main(tests, argc, argv);
}