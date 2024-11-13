#include <verilated.h>
#include "VmkCap2024_02_Decode_Comb_Tb.h"

#include "decode_tests_uvm.h"

using TheDUT = VmkCap2024_02_Decode_Comb_Tb;

int main(int argc, char** argv) {
    std::vector<TestBase*> tests = {
        new DecoderUVMishTest_02<TheDUT>(new ManyRandomValidCaps_02<TheDUT>(2000)),  
        new DecoderUVMishTest_02<TheDUT>(new ManyRandomBits<TheDUT>(2000)),  
    };

    return tb_main(tests, argc, argv);
}