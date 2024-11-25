#include <verilated.h>
#include "VmkCap2024_02_Decode_FastFSM_Tb.h"

#include "decode_tests_uvm.h"

using TheDUT = VmkCap2024_02_Decode_FastFSM_Tb;

int main(int argc, char** argv) {
    std::vector<TestBase*> tests = {
        new DecoderUVMishTest<TheDUT, CapType::Cap2024_02>(new ManyRandomValidCaps<TheDUT, CapType::Cap2024_02>(2000)),  
        new DecoderUVMishTest<TheDUT, CapType::Cap2024_02>(new ManyRandomBits<TheDUT>(2000)),  
    };

    return tb_main(tests, argc, argv);
}