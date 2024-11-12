#include <verilated.h>
#include "VmkCap2024_11_Decode_Comb_Tb.h"

#include "decode_tests_uvm.h"

using TheDUT = VmkCap2024_11_Decode_Comb_Tb;

int main(int argc, char** argv) {
    std::vector<TestBase*> tests = {
        new DecoderUVMishTest<TheDUT>(new ManyRandomValidCaps<TheDUT>(2000)),  
    };

    return tb_main(tests, argc, argv);
}