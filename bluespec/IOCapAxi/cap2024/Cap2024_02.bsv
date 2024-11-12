package Cap2024_02;

import AesLib::*;
import Cap2024::*;

// The actual capability is a 128-bit structure
typedef struct {
    // There's one invalid bit pattern hence Maybe()
    // not just CapPermsChain
    // TODO remove this Maybe and only allow unpacking CapCheckResult#(Cap2024_02) from Bits
    Maybe#(CapPermsChain) chain;
    Bit#(23) secret_key_id;

    UInt#(9) range_y_minus_one;
    UInt#(9) range_x;

    UInt#(15) index;
    UInt#(4) index_size_div;

    Bit#(59) b_c;
    Bit#(5) encoded_elem_width;
} Cap2024_02 deriving (Bits, Eq, FShow);

// TODO manually derive Bits so we know exactly which bit ranges are used

// Helper input struct for things that check the signature+text of a capability
typedef struct {
    Bit#(128) signature;
    Cap2024_02 text;
} CapCheckRequest deriving (Bits, Eq, FShow);

// The actual Cap2024_02 zeroes out certain parts when it's created as root or 1cav.
instance UpToTwoCaveats#(Cap2024_02);
    function Cap2024_02 atRoot(Cap2024_02 val);
        return Cap2024_02 {
            encoded_elem_width: val.encoded_elem_width,
            b_c: val.b_c,
            index_size_div: 0,
            index: 0,
            range_x: 0,
            range_y_minus_one: 0,
            secret_key_id: val.secret_key_id,
            // If val.chain is Invalid, we shouldn't really be called - but use Invalid here
            // otherwise do atRoot(validValChain)
            chain: maybeMap(atRoot, val.chain)
        };
    endfunction

    function Maybe#(Cap2024_02) atCav1(Cap2024_02 val);
        // Treat invalid cav-chains as something that has a root but no cav1 or cav2
        if (val.chain matches tagged Valid .maybeCav1Chain &&& atCav1(maybeCav1Chain) matches tagged Valid .cav1Chain)
            return tagged Valid Cap2024_02 {
                encoded_elem_width: val.encoded_elem_width,
                b_c: val.b_c,
                index_size_div: val.index_size_div,
                index: val.index,
                range_x: 0,
                range_y_minus_one: 0,
                secret_key_id: val.secret_key_id,
                chain: tagged Valid cav1Chain
            };
        else
            return tagged Invalid;
    endfunction

    function Maybe#(Cap2024_02) atCav2(Cap2024_02 val);
        // Treat invalid cav-chains as something that has a root but no cav1 or cav2
        if (val.chain matches tagged Valid .maybeCav2Chain &&& atCav2(maybeCav2Chain) matches tagged Valid .*)
            return tagged Valid val;
        else
            return tagged Invalid;
    endfunction
endinstance

instance Cap#(Cap2024_02);
    function AesState stateOfCap(Cap2024_02 cap);
        return unpack(pack(cap));
    endfunction
endinstance


endpackage