#include <verilated.h>
#include "VmkSimpleIOCapExposer_Tb.h"

#include "key_manager.h"
#include "exposer.h"

#include "tb.h"
#include "capabilities.h"
#include "util.h"

#include <fmt/ranges.h>

#include <deque>
#include <memory>
#include <random>

#define DEFAULT_SEED 12398723198234ull

struct AxiParams {
    uint64_t address;
    uint8_t transfer_width;
    uint8_t n_transfers;
};

struct CapWithRange {
    CCap2024_02 cap;
    uint64_t cap_base;
    uint64_t cap_len;
    bool cap_is_almighty;

    AxiParams valid_transfer_params(uint8_t transfer_width, uint8_t n_transfers) const {
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }
        return AxiParams {
            .address = cap_base,
            .transfer_width = transfer_width,
            .n_transfers = n_transfers,
        };
    }
};

template<class DUT>
struct ExposerCycleTest : public CycleTest<DUT, ShimmedExposerInput, ShimmedExposerOutput> {
    virtual CapWithRange test_random_initial_resource_cap(const U128& key, uint32_t secret_id, CCapPerms perms) {
        CapWithRange data{};

        data.cap = random_initial_resource_cap(this->rng, key, secret_id, perms);
        if (ccap_read_range(&data.cap, &data.cap_base, &data.cap_len, &data.cap_is_almighty) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        
        return data;
    }
};

#define CANPUT_INPUT(name) ((dut.RDY_## name ##_put != 0) && (dut. name ##_canPut != 0))
#define PUT_INPUT(name, value) do {                  \
    dut.EN_## name ##_put = 1;        \
    dut. name ##_put_val = (value); \
    assert(dut.RDY_## name ##_put);   \
    assert(dut. name ##_canPut);    \
} while(0);
#define NOPUT_INPUT(name) dut.EN_## name ##_put = 0;
#define CANPEEK_OUTPUT(from) (dut.from ##_canPeek)
#define PEEK_OUTPUT(from) dut. from ##_peek
#define POP_OUTPUT(from, into) \
    assert(dut. from ##_canPeek); \
    assert(dut.RDY_## from ##_drop); \
    dut.EN_## from ##_drop = 1; \
    into = dut. from ##_peek;

template<class DUT>
class KeyManagerShimStimulus {
public:
    std::unordered_map<key_manager::KeyId, U128> secrets; // Fake keymanager proxy.

    KeyManagerShimStimulus() : secrets() {}
    // Observe the key manager inputs (epoch fulfilments and )
    virtual void driveInputsForKeyMgr(DUT& dut, uint64_t tick) = 0;
};

template<class DUT>
class BasicKeyManagerShimStimulus : public KeyManagerShimStimulus<DUT> {
    std::deque<key_manager::KeyResponse> keyResponses;
public:
    BasicKeyManagerShimStimulus() : KeyManagerShimStimulus<DUT>() {}
    virtual void driveInputsForKeyMgr(DUT& dut, uint64_t tick) {
        if (!keyResponses.empty() && CANPUT_INPUT(keyStoreShim_keyResponses)) {
            auto keyResponse = keyResponses.front();
            keyResponses.pop_front();
            assert(key_manager::Tuple2_KeyId_MaybeKey::unpack(keyResponse.asBluespec().pack()) == keyResponse.asBluespec());
            PUT_INPUT(keyStoreShim_keyResponses, verilate_array(keyResponse.asBluespec().pack()));
        } else {
            NOPUT_INPUT(keyStoreShim_keyResponses);
        }

        if (CANPEEK_OUTPUT(keyStoreShim_keyRequests)) {
            key_manager::KeyId requested = 0;
            POP_OUTPUT(keyStoreShim_keyRequests, requested);
            if (this->secrets.contains(requested)) {
                keyResponses.push_back(key_manager::KeyResponse {
                    .keyId = requested,
                    .key = this->secrets[requested]
                });
            } else {
                keyResponses.push_back(key_manager::KeyResponse {
                    .keyId = requested,
                    .key = std::nullopt
                });
            }
        }
    }
};

template<class DUT>
class SanitizedMemStimulus {
public:
    // Observe the sanitized AW/W/AR outputs and drive the sanitize B/R inputs in response
    virtual void driveBAndRInputs(DUT& dut, uint64_t tick) = 0;
};

template<class DUT>
class BasicSanitizedMemStimulus : public SanitizedMemStimulus<DUT> {
    // Flits which have arrived before their corresponding AW
    std::deque<axi::SanitizedAxi::WFlit_data32> unexpectedWFlits;
    std::deque<uint64_t> wFlitsExpectedPerBurst;
    std::deque<axi::SanitizedAxi::BFlit_id4> bInputs;
    std::deque<axi::SanitizedAxi::RFlit_id4_data32> rInputs;
public:
    virtual void driveBAndRInputs(DUT& dut, uint64_t tick) {
        if (!bInputs.empty() && CANPUT_INPUT(exposer4x32_sanitizedOut_b)) {
            auto bInput = bInputs.front();
            bInputs.pop_front();
            PUT_INPUT(exposer4x32_sanitizedOut_b, bInput.pack());
        } else {
            NOPUT_INPUT(exposer4x32_sanitizedOut_b);
        }

        if (!rInputs.empty() && CANPUT_INPUT(exposer4x32_sanitizedOut_r)) {
            auto rInput = rInputs.front();
            rInputs.pop_front();
            PUT_INPUT(exposer4x32_sanitizedOut_r, rInput.pack());
        } else {
            NOPUT_INPUT(exposer4x32_sanitizedOut_r);
        }

        if (CANPEEK_OUTPUT(exposer4x32_sanitizedOut_aw)) {
            VlWide<4> flit;
            POP_OUTPUT(exposer4x32_sanitizedOut_aw, flit);
            auto awFlit = axi::SanitizedAxi::AWFlit_id4_addr64_user0::unpack(stdify_array(flit));
            wFlitsExpectedPerBurst.push_back(awFlit.awlen + 1);
        }

        if (CANPEEK_OUTPUT(exposer4x32_sanitizedOut_w)) {
            uint64_t flit;
            POP_OUTPUT(exposer4x32_sanitizedOut_w, flit);
            auto wFlit = axi::SanitizedAxi::WFlit_data32::unpack(flit);
            unexpectedWFlits.push_back(wFlit);
        }

        // Resolve W flits
        while (!wFlitsExpectedPerBurst.empty() && !unexpectedWFlits.empty()) {
            auto wFlit = unexpectedWFlits.front();
            unexpectedWFlits.pop_front();

            if (wFlitsExpectedPerBurst.front() == 0) {
                throw std::logic_error("BasicSanitizedMemStimulus had a burst of 0 flits expected");
            } else if (wFlitsExpectedPerBurst.front() == 1) {
                wFlitsExpectedPerBurst.pop_front();
                assert(wFlit.wlast == 1);
            } else {
                wFlitsExpectedPerBurst.front() -= 1;
                assert(wFlit.wlast == 0);
            }
        }

        if (CANPEEK_OUTPUT(exposer4x32_sanitizedOut_ar)) {
            VlWide<4> flit;
            POP_OUTPUT(exposer4x32_sanitizedOut_ar, flit);
            auto arFlit = axi::SanitizedAxi::ARFlit_id4_addr64_user0::unpack(stdify_array(flit));
            // Assume the arFlits use 32-bit data
            for (uint32_t i = 0; i < arFlit.arlen + 1; i++) {
                rInputs.push_back(axi::SanitizedAxi::RFlit_id4_data32 {
                    .rlast = ((i == arFlit.arlen) ? 1 : 0),
                    .rresp = (uint8_t)axi::AXI4_Resp::Okay,
                    .rdata = i, // TODO better RDATA generation
                    .rid = arFlit.arid,
                });
            }
        }
    }
};

template<class DUT>
class ExposerStimulus : public StimulusGenerator<DUT> {
protected:
    std::mt19937 rng;
public:
    std::unique_ptr<KeyManagerShimStimulus<DUT>> keyMgr;
protected:
    std::unique_ptr<SanitizedMemStimulus<DUT>> sanitizedMem;

    std::deque<axi::IOCapAxi::AWFlit_id4_addr64_user3> awInputs;
    std::deque<axi::IOCapAxi::WFlit_data32> wInputs;
    std::deque<axi::IOCapAxi::ARFlit_id4_addr64_user3> arInputs;

    virtual CapWithRange test_random_initial_resource_cap(uint32_t secret_id, CCapPerms perms) {
        CapWithRange data{};

        data.cap = random_initial_resource_cap(this->rng, keyMgr->secrets[secret_id], secret_id, perms);
        if (ccap_read_range(&data.cap, &data.cap_base, &data.cap_len, &data.cap_is_almighty) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        
        return data;
    }
    void enqueueReadBurst(CCap2024_02& cap, AxiParams& axi_params, uint8_t id) {
        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);

        arInputs.push_back(axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = id & 0xF,
        });
        arInputs.push_back(axi::IOCapAxi::packCap1_ar(cap128, sig128));
        arInputs.push_back(axi::IOCapAxi::packCap2_ar(cap128, sig128));
        arInputs.push_back(axi::IOCapAxi::packCap3_ar(cap128, sig128));
    }
    void enqueueWriteBurst(CCap2024_02& cap, AxiParams& axi_params, uint8_t id) {
        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);

        awInputs.push_back(axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = id & 0xF,
        });
        awInputs.push_back(axi::IOCapAxi::packCap1_aw(cap128, sig128));
        awInputs.push_back(axi::IOCapAxi::packCap2_aw(cap128, sig128));
        awInputs.push_back(axi::IOCapAxi::packCap3_aw(cap128, sig128));

        for (int i = 0; i < axi_params.n_transfers; i++) {
            wInputs.push_back(axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            });
        }
    }

public:
    ExposerStimulus(KeyManagerShimStimulus<DUT>* keyMgr, SanitizedMemStimulus<DUT>* sanitizedMem, uint64_t seed = DEFAULT_SEED) :
        rng(seed), keyMgr(keyMgr), sanitizedMem(sanitizedMem), awInputs(), wInputs(), arInputs() {}

    virtual ~ExposerStimulus() = default;
    virtual void driveInputsForTick(DUT& dut, uint64_t tick) {
        keyMgr->driveInputsForKeyMgr(dut, tick);
        sanitizedMem->driveBAndRInputs(dut, tick);

        if (!awInputs.empty() && CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw)) {
            auto awInput = awInputs.front();
            awInputs.pop_front();
            PUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw, verilate_array(awInput.pack()));
        } else {
            NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_aw);
        }

        if (!wInputs.empty() && CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_w)) {
            auto wInput = wInputs.front();
            wInputs.pop_front();
            PUT_INPUT(exposer4x32_iocapsIn_axiSignals_w, wInput.pack());
        } else {
            NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_w);
        }

        if (!arInputs.empty() && CANPUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar)) {
            auto arInput = arInputs.front();
            arInputs.pop_front();
            PUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar, verilate_array(arInput.pack()));
        } else {
            NOPUT_INPUT(exposer4x32_iocapsIn_axiSignals_ar);
        }
    }
};

#undef POP_OUTPUT
#undef PEEK_OUTPUT
#undef CANPEEK_OUTPUT
#undef NOPUT_INPUT
#undef PUT_INPUT
#undef CANPUT_INPUT


template<class DUT>
class ExposerScoreboard : public Scoreboard<DUT> {
    std::unordered_map<key_manager::KeyId, U128>& secrets; // Fake keymanager proxy. THIS SCOREBOARD ASSUMES KEYS DONT CHANGE

    std::deque<key_manager::Epoch> expectedEpochCompletions;

    std::vector<axi::IOCapAxi::AWFlit_id4_addr64_user3> awInProgress;
    std::deque<axi::SanitizedAxi::AWFlit_id4_addr64_user0> expectedAw;

    // W flits received on the input where we don't know if their associated AWs were correctly authed.
    // The DUT cannot possibly know if W flits in this queue are correctly authed or not.
    std::deque<axi::IOCapAxi::WFlit_data32> wInProgress;
    // The status of future groups of W flits. e.g. if you recieve a valid AW transaction specifying 3 W flits, store (true, 3) so the next 3 W flits received will automatically be counted as valid.
    std::deque<std::pair<bool, uint64_t>> wflitValidity; // tuple[0] = is valid, tuple[1] = count
    // The W flits we expect to see on the output, i.e. only valid ones
    std::deque<axi::SanitizedAxi::WFlit_data32> expectedW;

    std::vector<axi::IOCapAxi::ARFlit_id4_addr64_user3> arInProgress;
    std::deque<axi::SanitizedAxi::ARFlit_id4_addr64_user0> expectedAr;

    // B and R responses have no ordering guarantees *except* B is ordered w.r.t. B when the IDs are the same, ditto for R.
    // => for each, keep a map of ID -> ordered list of responses.
    // Whenever we receive a response, look in the map at the given ID. if the vector is empty, there wasn't supposed to be a response. Otherwise the first pop()-ed element must match.
    std::array<std::deque<axi::IOCapAxi::BFlit_id4>, 16> expectedB;
    std::array<std::deque<axi::IOCapAxi::RFlit_id4_data32>, 16> expectedR;

    uint64_t expectedGoodWrite = 0;
    uint64_t expectedBadWrite = 0;
    uint64_t expectedGoodRead = 0;
    uint64_t expectedBadRead = 0;
    uint64_t signalledGoodWrite = 0;
    uint64_t signalledBadWrite = 0;
    uint64_t signalledGoodRead = 0;
    uint64_t signalledBadRead = 0;

    std::vector<ShimmedExposerInput> inputs;
    std::vector<ShimmedExposerOutput> outputs;

    virtual void onKeyMngrNewEpoch(key_manager::Epoch newEpoch) {
        throw test_failure("ExposerScoreboard base class can't handle getting new key epoch!");
    }
    virtual void onKeyMngrKeyResponse(key_manager::KeyResponse response) {
        std::optional<U128> expected = std::nullopt;
        if (secrets.contains(response.keyId)) {
            expected = secrets[response.keyId];
        }
        if (expected != response.key) {
            throw test_failure(fmt::format("ExposerScoreboard base class saw a new value {} for key ID {} that doesn't match expected value {}", response.key, response.keyId, expected));
        }
    }

    void resolveAwFlit(std::optional<axi::IOCapAxi::AWFlit_id4_addr64_user3> newIncomingFlit) {
        if (newIncomingFlit) {
            awInProgress.push_back(newIncomingFlit.value());
        }
        if (awInProgress.size() > 4) {
            throw test_failure(fmt::format("ExposerScoreboard got nonsensical set of aw flits - too many somehow? {}", awInProgress));
        }
        if (awInProgress.size() == 4) {
            if (
                (awInProgress[0].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Start) ||
                (awInProgress[1].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap1) ||
                (awInProgress[2].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap2) ||
                (awInProgress[3].awuser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap3)
            ) {
                throw test_failure(fmt::format("ExposerScoreboard got nonsensical set of aw flits - incorrect user flags {}", awInProgress));
            }
            U128 data{.top = 0, .bottom = 0};
            U128 sig{.top = 0, .bottom = 0};
            unpackCap1_aw(data, sig, awInProgress[1]);
            unpackCap2_aw(data, sig, awInProgress[2]);
            unpackCap3_aw(data, sig, awInProgress[3]);
            CCap2024_02 cap;
            sig.to_le(cap.signature);
            data.to_le(cap.data);
            uint32_t secret_key_id;

            // Find the address range 
            uint64_t axiBase = awInProgress[0].awaddr;
            uint64_t axiTop;
            try {
                axiTop = axiBase + axi::burst_byte_length((axi::AXI4_Burst)awInProgress[0].awburst, awInProgress[0].awsize, awInProgress[0].awlen);
            } catch (std::runtime_error& ex) {
                throw test_failure(fmt::format("ExposerScoreboard got nonsensical set of aw flits - {} - {}", awInProgress, ex.what()));
            }
            
            uint64_t base = 0;
            uint64_t len = 0;
            bool len64 = false;
            CCapPerms perms = CCapPerms_ReadWrite;
            bool capIsValid = (ccap_read_secret_id(&cap, &secret_key_id) == CCapResult_Success) &&
                                (ccap_read_range(&cap, &base, &len, &len64) == CCapResult_Success) &&
                                (ccap_read_perms(&cap, &perms) == CCapResult_Success) && 
                                ((perms & CCapPerms_Write) != 0);
            if (capIsValid && secrets.contains(secret_key_id & 0xFF)) {
                CCapU128 secret_key;
                secrets[secret_key_id & 0xFF].to_le(secret_key);
                capIsValid = (ccap_check_signature(&cap, &secret_key) == CCapResult_Success);
            } else {
                capIsValid = false;
            }

            // TODO this might not work if axiBase+axiLen = end of addrspace?
            bool rangeIsValid = len64 || (axiBase >= base && axiTop <= (base + len));
            // If the capability and ranges are valid, expect an AW flit to come out *and* the right number of W flits!
            if (capIsValid && rangeIsValid) {
                expectedAw.push_back(axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
                    .awregion = awInProgress[0].awregion,
                    .awqos = awInProgress[0].awqos,
                    .awprot = awInProgress[0].awprot,
                    .awcache = awInProgress[0].awcache,
                    .awlock = awInProgress[0].awlock,
                    .awburst = awInProgress[0].awburst,
                    .awsize = awInProgress[0].awsize,
                    .awlen = awInProgress[0].awlen,
                    .awaddr = awInProgress[0].awaddr,
                    .awid = awInProgress[0].awid,
                });
                // and expect the right number of W flits to be passed through
                wflitValidity.push_back(std::make_pair(true, axi::len_to_n_transfers(awInProgress[0].awlen)));
                // and expect the performance counter to bump
                expectedGoodWrite++;
            } else {
                // Otherwise expect a B flit with a BAD response to come out
                expectedB[awInProgress[0].awid].push_back(axi::IOCapAxi::BFlit_id4 {
                    .bresp = (uint8_t)axi::AXI4_Resp::SlvErr,
                    .bid = awInProgress[0].awid
                });
                // and drop the W flits
                wflitValidity.push_back(std::make_pair(false, axi::len_to_n_transfers(awInProgress[0].awlen)));
                // and expect the performance counter to bump
                expectedBadWrite++;
            }

            // We've now handled the set of AW flits, and can drop them
            awInProgress.clear();
        }
    }

    void resolveArFlit(std::optional<axi::IOCapAxi::ARFlit_id4_addr64_user3> newIncomingFlit) {
        if (newIncomingFlit) {
            arInProgress.push_back(newIncomingFlit.value());
        }
        if (arInProgress.size() > 4) {
            throw test_failure(fmt::format("ExposerScoreboard got nonsensical set of ar flits - too many somehow? {}", arInProgress));
        }
        if (arInProgress.size() == 4) {
            if (
                (arInProgress[0].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Start) ||
                (arInProgress[1].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap1) ||
                (arInProgress[2].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap2) ||
                (arInProgress[3].aruser != (uint8_t)axi::IOCapAxi::IOCapAxi_User::Cap3)
            ) {
                throw test_failure(fmt::format("ExposerScoreboard got nonsensical set of ar flits - incorrect user flags {}", arInProgress));
            }
            U128 data{.top = 0, .bottom = 0};
            U128 sig{.top = 0, .bottom = 0};
            unpackCap1_ar(data, sig, arInProgress[1]);
            unpackCap2_ar(data, sig, arInProgress[2]);
            unpackCap3_ar(data, sig, arInProgress[3]);
            CCap2024_02 cap;
            sig.to_le(cap.signature);
            data.to_le(cap.data);
            uint32_t secret_key_id;

            // Find the address range 
            uint64_t axiBase = arInProgress[0].araddr;
            uint64_t axiTop;
            try {
                axiTop = axiBase + axi::burst_byte_length((axi::AXI4_Burst)arInProgress[0].arburst, arInProgress[0].arsize, arInProgress[0].arlen);
            } catch (std::runtime_error& ex) {
                throw test_failure(fmt::format("ExposerScoreboard got nonsensical set of ar flits - {} - {}", arInProgress, ex.what()));
            }
            
            uint64_t base = 0;
            uint64_t len = 0;
            bool len64 = false;
            CCapPerms perms = CCapPerms_ReadWrite;
            bool capIsValid = (ccap_read_secret_id(&cap, &secret_key_id) == CCapResult_Success) &&
                                (ccap_read_range(&cap, &base, &len, &len64) == CCapResult_Success) &&
                                (ccap_read_perms(&cap, &perms) == CCapResult_Success) && 
                                ((perms & CCapPerms_Read) != 0);
            if (capIsValid && secrets.contains(secret_key_id & 0xFF)) {
                CCapU128 secret_key;
                secrets[secret_key_id & 0xFF].to_le(secret_key);
                capIsValid = (ccap_check_signature(&cap, &secret_key) == CCapResult_Success);
            } else {
                capIsValid = false;
            }

            // TODO this might not work if axiBase+axiLen = end of addrspace?
            bool rangeIsValid = len64 || (axiBase >= base && axiTop <= (base + len));
            // If the capability and ranges are valid, expect an AR flit to come out
            if (capIsValid && rangeIsValid) {
                expectedAr.push_back(axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
                    .arregion = arInProgress[0].arregion,
                    .arqos = arInProgress[0].arqos,
                    .arprot = arInProgress[0].arprot,
                    .arcache = arInProgress[0].arcache,
                    .arlock = arInProgress[0].arlock,
                    .arburst = arInProgress[0].arburst,
                    .arsize = arInProgress[0].arsize,
                    .arlen = arInProgress[0].arlen,
                    .araddr = arInProgress[0].araddr,
                    .arid = arInProgress[0].arid,
                });
                // and expect the performance counter to bump
                expectedGoodRead++;
            } else {
                // Otherwise expect a B flit with a BAD response to come out
                expectedR[arInProgress[0].arid].push_back(axi::IOCapAxi::RFlit_id4_data32 {
                    .rlast = 1,
                    .rresp = (uint8_t)axi::AXI4_Resp::SlvErr,
                    .rdata = 0xaaaaaaaa, // Bluespec uses this to mean ?
                    .rid = arInProgress[0].arid,
                });
                // and expect the performance counter to bump
                expectedBadRead++;
            }

            // We've now handled the set of AR flits, and can drop them
            arInProgress.clear();
        }
    }

    void resolveWFlits(std::optional<axi::IOCapAxi::WFlit_data32> newIncomingFlit) {
        if (newIncomingFlit) {
            wInProgress.push_back(newIncomingFlit.value());
        }
        // If there are outstanding future groups of W flits, compare them to the wInProgress.
        while (!wflitValidity.empty() && !wInProgress.empty()) {
            auto [groupValid, groupLen] = *wflitValidity.begin();
            while (!wInProgress.empty() && groupLen > 0) {
                const axi::IOCapAxi::WFlit_data32 iocapFlit = wInProgress.front();
                wInProgress.pop_front();
                // Handle the w flit: if it's in a valid group make a new expected flit
                if (groupValid) {
                    expectedW.push_back(axi::SanitizedAxi::WFlit_data32 {
                        .wlast = iocapFlit.wlast,
                        .wstrb = iocapFlit.wstrb,
                        .wdata = iocapFlit.wdata,
                    });
                }
                // Otherwise drop it and do nothing!
                groupLen--;
            }
            // If we exhausted the group, pop it off
            if (groupLen == 0) {
                wflitValidity.pop_front();
            } else {
                // Otherwise, update the new length
                wflitValidity.front() = std::make_pair(groupValid, groupLen);
            }
        }
        if (!wflitValidity.empty() && !wInProgress.empty()) {
            throw std::logic_error("Screwed up resolveWFlits");
        }
    }

public:
    ExposerScoreboard(std::unordered_map<key_manager::KeyId, U128>& secrets) : secrets(secrets),
        expectedEpochCompletions(),
        awInProgress(),
        expectedAw(),
        wInProgress(),
        wflitValidity(),
        expectedW(),
        arInProgress(),
        expectedAr(),
        expectedB(),
        expectedR() {}
    virtual ~ExposerScoreboard() = default;
    // Should raise a test_failure on failure
    virtual void monitorAndScore(DUT& dut, uint64_t tick) {
        ShimmedExposerOutput output{0};
        output.time = tick;
        pull_output(dut, output); // TODO apply backpressure?
        if (output.is_notable())
            outputs.push_back(output);

        if (output.keyManager.finishedEpoch) {
            if (expectedEpochCompletions.empty()) {
                throw test_failure(fmt::format("ExposerScoreboard got unexpected finishedEpoch:\nexpected None\ngot: {}\n", output.keyManager.finishedEpoch.value()));
            } else {
                auto expected = expectedEpochCompletions.front();
                expectedEpochCompletions.pop_front();
                if (output.keyManager.finishedEpoch.value() != expected) {
                    throw test_failure(fmt::format("ExposerScoreboard got unexpected finishedEpoch:\nexpected: {}\ngot: {}\n", expected, output.keyManager.finishedEpoch.value()));
                }
            }
        }

        if (output.keyManager.bumpPerfCounterGoodWrite) {
            signalledGoodWrite++;
        }
        if (output.keyManager.bumpPerfCounterBadWrite) {
            signalledBadWrite++;
        }
        if (output.keyManager.bumpPerfCounterGoodRead) {
            signalledGoodRead++;
        }
        if (output.keyManager.bumpPerfCounterBadRead) {
            signalledBadRead++;
        }

        if (output.clean_flit_aw) {
            if (expectedAw.empty()) {
                throw test_failure(fmt::format("ExposerScoreboard got unexpected aw flit:\nexpected None\ngot: {}\n", output.clean_flit_aw.value()));
            } else {
                auto expected = expectedAw.front();
                expectedAw.pop_front();
                if (output.clean_flit_aw.value() != expected) {
                    throw test_failure(fmt::format("ExposerScoreboard got unexpected aw flit:\nexpected: {}\ngot: {}\n", expected, output.clean_flit_aw.value()));
                }
            }
        }

        if (output.clean_flit_ar) {
            if (expectedAr.empty()) {
                throw test_failure(fmt::format("ExposerScoreboard got unexpected ar flit:\nexpected None\ngot: {}\n", output.clean_flit_ar.value()));
            } else {
                auto expected = expectedAr.front();
                expectedAr.pop_front();
                if (output.clean_flit_ar.value() != expected) {
                    throw test_failure(fmt::format("ExposerScoreboard got unexpected ar flit:\nexpected: {}\ngot: {}\n", expected, output.clean_flit_ar.value()));
                }
            }
        }

        if (output.clean_flit_w) {
            if (expectedW.empty()) {
                throw test_failure(fmt::format("ExposerScoreboard got unexpected w flit:\nexpected None, had {} unresolved and {} groups\ngot: {}\n", wInProgress, wflitValidity, output.clean_flit_w.value()));
            } else {
                auto expected = expectedW.front();
                expectedW.pop_front();
                if (output.clean_flit_w.value() != expected) {
                    throw test_failure(fmt::format("ExposerScoreboard got unexpected w flit:\nexpected {}, had {} unresolved and {} groups\ngot: {}\n", expected, wInProgress, wflitValidity, output.clean_flit_w.value()));
                }
            }
        }

        if (output.iocap_flit_b) {
            auto& expectedForId = expectedB[output.iocap_flit_b.value().bid];
            if (expectedForId.empty()) {
                throw test_failure(fmt::format("ExposerScoreboard got unexpected b flit:\nexpected None\nall expected: {}\ngot: {}\n", expectedB, output.iocap_flit_b.value()));
            } else {
                auto expected = expectedForId.front();
                expectedForId.pop_front();
                if (output.iocap_flit_b.value() != expected) {
                    throw test_failure(fmt::format("ExposerScoreboard got unexpected b flit:\nexpected: {}\nall expected: {}\ngot: {}\n", expected, expectedB, output.iocap_flit_b.value()));
                }
            }
        }

        if (output.iocap_flit_r) {
            auto& expectedForId = expectedR[output.iocap_flit_r.value().rid];
            if (expectedForId.empty()) {
                throw test_failure(fmt::format("ExposerScoreboard got unexpected r flit:\nexpected None\nall expected: {}\ngot: {}\n", expectedR, output.iocap_flit_r.value()));
            } else {
                auto expected = expectedForId.front();
                expectedForId.pop_front();
                if (output.iocap_flit_r.value() != expected) {
                    throw test_failure(fmt::format("ExposerScoreboard got unexpected r flit:\nexpected: {}\nall expected: {}\ngot: {}\n", expected, expectedR, output.iocap_flit_r.value()));
                }
            }
        }

        ShimmedExposerInput input{0};
        input.time = tick;
        observe_input(dut, input);
        if (input.is_notable())
            inputs.push_back(input);

        if (input.keyManager.newEpochRequest) {
            onKeyMngrNewEpoch(input.keyManager.newEpochRequest.value());
        }

        if (input.keyManager.keyResponse) {
            onKeyMngrKeyResponse(input.keyManager.keyResponse.value());
        }

        // Add incoming B and R flits from the sanitized-side to the list of expected outputs
        if (input.clean_flit_b) {
            auto expected = input.clean_flit_b.value();
            expectedB[expected.bid].push_back(axi::IOCapAxi::BFlit_id4 {
                .bresp = expected.bresp,
                .bid = expected.bid
            });
        }
        if (input.clean_flit_r) {
            auto expected = input.clean_flit_r.value();
            expectedR[expected.rid].push_back(axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = expected.rlast,
                .rresp = expected.rresp,
                .rdata = expected.rdata,
                .rid = expected.rid,
            });
        }

        // Resolve AW flits to see if there's a new valid/invalid burst
        // If invalid, it should return a B flit. If we received a staitized B flit with the same ID on the same cycle, that one comes first in the ordering because it must be the result of a valid AW on a previous cycle/came before.
        resolveAwFlit(input.iocap_flit_aw);
        // Resolve AR flits to see if there's a new valid/invalid burst
        // Same as above for R flits
        resolveArFlit(input.iocap_flit_ar);

        // Add incoming W flits from the IOCap-side to the list of expected output W flits, if possible.
        resolveWFlits(input.iocap_flit_w);
    }
    // Should raise a test_failure on failure
    virtual void endTest() override {
        // TODO log inputs and outputs
        if (
            !expectedEpochCompletions.empty() ||
            !awInProgress.empty() ||
            !expectedAw.empty() ||
            !wInProgress.empty() ||
            !expectedW.empty() ||
            !arInProgress.empty() ||
            !expectedAr.empty() ||
            // Check foreach ID in {B, R}
            std::any_of(expectedB.begin(), expectedB.end(), [](auto& expectedForId) { return !expectedForId.empty(); }) ||
            std::any_of(expectedR.begin(), expectedR.end(), [](auto& expectedForId) { return !expectedForId.empty(); }) ||
            (expectedGoodWrite != signalledGoodWrite) ||
            (expectedBadWrite != signalledBadWrite) ||
            (expectedGoodRead != signalledGoodRead) ||
            (expectedBadRead != signalledBadRead)
        ) {
            throw test_failure(fmt::format(
                "ExposerScoreboard expected more flits:\n"
                "epoch completions: {}\n"
                "aw: {} in progress, {} expected\n"
                "w: {} in progress, {} expected\n"
                "ar: {} in progress, {} expected\n"
                "b: {}\n"
                "r: {}\n"
                "perf counters exp/act:\n"
                "good write {}/{}\n"
                "bad write {}/{}\n"
                "good read {}/{}\n"
                "bad read {}/{}\n"
                ,
                expectedEpochCompletions,
                awInProgress, expectedAw,
                wInProgress, expectedW,
                arInProgress, expectedAr,
                expectedB,
                expectedR,
                expectedGoodWrite, signalledGoodWrite,
                expectedBadWrite, signalledBadWrite,
                expectedGoodRead, signalledGoodRead,
                expectedBadRead, signalledBadRead
            ));
        }
    }
};

template<class DUT>
class ExposerUVMishTest: public UVMishTest<DUT> {
public:
    ExposerUVMishTest(ExposerStimulus<DUT>* stimulus) :
        UVMishTest<DUT>(
            new ExposerScoreboard<DUT>(stimulus->keyMgr->secrets),
            stimulus
        ) {}
};

template<class DUT>
class UVMValidKeyValidInitialCapValidAccess : public ExposerStimulus<DUT> {
    CCapPerms perms;
    
public:
    virtual ~UVMValidKeyValidInitialCapValidAccess() = default;
    virtual std::string name() override {
        const char* perm_str = "Unk";
        switch (perms) {
            case CCapPerms_ReadWrite:
                perm_str = "ReadWrite";
                break;
            case CCapPerms_Write:
                perm_str = "Write";
                break;
            case CCapPerms_Read:
                perm_str = "Read";
                break;
            default:
                throw std::logic_error("Invalid perms");
        }
        return fmt::format("Valid-Key Valid-Cap Valid-{}", perm_str);
    }
    UVMValidKeyValidInitialCapValidAccess(CCapPerms perms, uint64_t seed = DEFAULT_SEED) : ExposerStimulus<DUT>(
        new BasicKeyManagerShimStimulus<DUT>(),
        new BasicSanitizedMemStimulus<DUT>(),
        seed
    ), perms(perms) {
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(this->rng);

        this->keyMgr->secrets[secret_id] = key;
        auto cap_data = this->test_random_initial_resource_cap(secret_id, perms);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
        if (perms & CCapPerms_Read) {
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
        if (perms & CCapPerms_Write) {
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual uint64_t getEndTime() override {
        // 100 cycles should do it
        return 1000;
    }
};

template<class DUT>
class UVMValidKeyValidInitialCapOOBAccess : public ExposerStimulus<DUT> {
    CCapPerms perms;
    
public:
    virtual ~UVMValidKeyValidInitialCapOOBAccess() = default;
    virtual std::string name() override {
        const char* perm_str = "Unk";
        switch (perms) {
            case CCapPerms_ReadWrite:
                perm_str = "ReadWrite";
                break;
            case CCapPerms_Write:
                perm_str = "Write";
                break;
            case CCapPerms_Read:
                perm_str = "Read";
                break;
            default:
                throw std::logic_error("Invalid perms");
        }
        return fmt::format("Valid-Key Valid-Cap OOB-{}", perm_str);
    }
    UVMValidKeyValidInitialCapOOBAccess(CCapPerms perms, uint64_t seed = DEFAULT_SEED) : ExposerStimulus<DUT>(
        new BasicKeyManagerShimStimulus<DUT>(),
        new BasicSanitizedMemStimulus<DUT>(),
        seed
    ), perms(perms) {
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(this->rng);

        this->keyMgr->secrets[secret_id] = key;
        auto cap_data = this->test_random_initial_resource_cap(secret_id, perms);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
        axi_params.address = cap_data.cap_base - 4096;
        if (perms & CCapPerms_Read) {
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
        if (perms & CCapPerms_Write) {
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual uint64_t getEndTime() override {
        // 100 cycles should do it
        return 1000;
    }
};

template<class DUT>
class UVMInvalidKeyAccess : public ExposerStimulus<DUT> {
    CCapPerms perms;
    
public:
    virtual ~UVMInvalidKeyAccess() = default;
    virtual std::string name() override {
        const char* perm_str = "Unk";
        switch (perms) {
            case CCapPerms_ReadWrite:
                perm_str = "ReadWrite";
                break;
            case CCapPerms_Write:
                perm_str = "Write";
                break;
            case CCapPerms_Read:
                perm_str = "Read";
                break;
            default:
                throw std::logic_error("Invalid perms");
        }
        return fmt::format("Invalid-Key {}", perm_str);
    }
    UVMInvalidKeyAccess(CCapPerms perms, uint64_t seed = DEFAULT_SEED) : ExposerStimulus<DUT>(
        new BasicKeyManagerShimStimulus<DUT>(),
        new BasicSanitizedMemStimulus<DUT>(),
        seed
    ), perms(perms) {
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(this->rng);

        this->keyMgr->secrets[secret_id] = key;
        // Use the wrong secret key ID
        auto cap_data = this->test_random_initial_resource_cap(90, perms);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
        axi_params.address = cap_data.cap_base - 4096;
        if (perms & CCapPerms_Read) {
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
        if (perms & CCapPerms_Write) {
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual uint64_t getEndTime() override {
        // 100 cycles should do it
        return 1000;
    }
};

template<class DUT>
class UVMValidKeyValidCapBadPerms : public ExposerStimulus<DUT> {
public:
    virtual ~UVMValidKeyValidCapBadPerms() = default;
    virtual std::string name() override {
        return "Valid-Key Valid-Cap BadPerms";
    }
    UVMValidKeyValidCapBadPerms(uint64_t seed = DEFAULT_SEED) : ExposerStimulus<DUT>(
        new BasicKeyManagerShimStimulus<DUT>(),
        new BasicSanitizedMemStimulus<DUT>(),
        seed
    ) {
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(this->rng);

        this->keyMgr->secrets[secret_id] = key;
        {
            auto cap_data = this->test_random_initial_resource_cap(secret_id, CCapPerms_Read);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
        {
            auto cap_data = this->test_random_initial_resource_cap(secret_id, CCapPerms_Write);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual uint64_t getEndTime() override {
        // 100 cycles should do it
        return 1000;
    }
};

template<class DUT>
class UVMValidKeyBadSigCap : public ExposerStimulus<DUT> {
public:
    virtual ~UVMValidKeyBadSigCap() = default;
    virtual std::string name() override {
        return "Valid-Key BadSig-Cap";
    }
    UVMValidKeyBadSigCap(uint64_t seed = DEFAULT_SEED) : ExposerStimulus<DUT>(
        new BasicKeyManagerShimStimulus<DUT>(),
        new BasicSanitizedMemStimulus<DUT>(),
        seed
    ) {
        const uint8_t axi_id = 0b1011;
        const key_manager::KeyId secret_id = 111;
        const U128 key = U128::random(this->rng);

        this->keyMgr->secrets[secret_id] = key;
        const U128 badSignature = {
            .top = 0x01020304050607,
            .bottom = 0x08090a0b0c0d0e0f,
        };
        {
            auto cap_data = this->test_random_initial_resource_cap(secret_id, CCapPerms_Read);
            badSignature.to_le(cap_data.cap.signature);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            this->enqueueReadBurst(cap_data.cap, axi_params, axi_id);
        }
        {
            auto cap_data = this->test_random_initial_resource_cap(secret_id, CCapPerms_Write);
            badSignature.to_le(cap_data.cap.signature);
            auto axi_params = cap_data.valid_transfer_params(32, 20);
            this->enqueueWriteBurst(cap_data.cap, axi_params, axi_id);
        }
    }
    virtual uint64_t getEndTime() override {
        // 100 cycles should do it
        return 1000;
    }
};

template<class DUT>
struct ValidKeyValidCapValidWrite : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "Valid-Key Valid-Cap Valid-Write";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        auto cap_data = this->test_random_initial_resource_cap(key, 111, CCapPerms_Write);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
       
        U128 cap128 = U128::from_le(cap_data.cap.data);
        U128 sig128 = U128::from_le(cap_data.cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct ValidKeyValidCapValidRead : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "Valid-Key Valid-Cap Valid-Read";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random readable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        auto cap_data = this->test_random_initial_resource_cap(key, 111, CCapPerms_Read);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
       
        U128 cap128 = U128::from_le(cap_data.cap.data);
        U128 sig128 = U128::from_le(cap_data.cap.signature);

        // TODO
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = 0b1011,
        };
        inputs[110].iocap_flit_ar = axi::IOCapAxi::packCap1_ar(cap128, sig128);
        inputs[120].iocap_flit_ar = axi::IOCapAxi::packCap2_ar(cap128, sig128);
        inputs[130].iocap_flit_ar = axi::IOCapAxi::packCap3_ar(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        
        // Then it will at some point later shove it out, while bumping the bad-read counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterGoodRead = true;
        outputs[280].clean_flit_ar = axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = 0b1011,
        };

        // Get the given amount of read responses
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[280 + (i * 10)].clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32 {
                .rlast = (i == axi_params.n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
            // R pass through a separate pipeline stage to register completions
            // see rule recv_r
            // => latency = 20
            outputs[280 + 20 + (i * 10)].iocap_flit_r = axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = (i == axi_params.n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
        }

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct ValidReadThenValidWrite : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "Valid-Key Valid-Cap Valid-Read then Valid-Key Valid-Cap Valid-Write";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random read/write capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        auto cap_data = this->test_random_initial_resource_cap(key, 111, CCapPerms_ReadWrite);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
       
        U128 cap128 = U128::from_le(cap_data.cap.data);
        U128 sig128 = U128::from_le(cap_data.cap.signature);
        
        // Send the flits to authenticate the first access
        inputs[100].iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = 0b1011,
        };
        inputs[110].iocap_flit_ar = axi::IOCapAxi::packCap1_ar(cap128, sig128);
        inputs[120].iocap_flit_ar = axi::IOCapAxi::packCap2_ar(cap128, sig128);
        inputs[130].iocap_flit_ar = axi::IOCapAxi::packCap3_ar(cap128, sig128);

        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);
        // Send the given amount of write data flits
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            /// TODO should be blocked until the transaction has been measured as good
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over.
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };

        // Then it will at some point later shove it out, while bumping the counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterGoodRead = true;
        outputs[280].clean_flit_ar = axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .arlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .araddr = axi_params.address,
            .arid = 0b1011,
        };

        // Get the given amount of read responses
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[280 + (i * 10)].clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32 {
                .rlast = (i == axi_params.n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
            // R pass through a separate pipeline stage to register completions
            // see rule recv_r
            // => latency = 20
            outputs[280 + 20 + (i * 10)].iocap_flit_r = axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = (i == axi_params.n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
        }


        // The write starts processing just after the read.
        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[160].keyManager.keyRequest = 111;
        // and we will hand it over.
        inputs[180].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key,
        };
        // Because they are separated by a single cycle, the keyresponse for the *first* access will cover *both* transactions.
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // The delay will be short, because it will realize the key is invalid and short-circuit the loss
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct OOBWrite_Passthrough : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "Valid-Key Valid-Cap OOB-Write - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = initial_resource_cap_exact(key, 0xdeadbeef0000, 0x1000, false, 111, CCapPerms_Write);
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 4;
        // Start in bounds, end OOB
        uint64_t access_base =  0xdeadbeef0000 - 0x1000*2;
        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = access_base,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterBadWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = access_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            // TODO shouldn't be passed through, the write was invalid!
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct OOBRead_Passthrough : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "Valid-Key Valid-Cap OOB-Read - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = initial_resource_cap_exact(key, 0xdeadbeef0000, 0x1000, false, 111, CCapPerms_Read);
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 4;
        // Start in bounds, end OOB
        uint64_t access_base =  0xdeadbeef0000 - 0x1000*2;

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(transfer_width),
            .arlen = axi::n_transfers_to_len(n_transfers),
            .araddr = access_base,
            .arid = 0b1011,
        };
        inputs[110].iocap_flit_ar = axi::IOCapAxi::packCap1_ar(cap128, sig128);
        inputs[120].iocap_flit_ar = axi::IOCapAxi::packCap2_ar(cap128, sig128);
        inputs[130].iocap_flit_ar = axi::IOCapAxi::packCap3_ar(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterBadRead = true;
        outputs[280].clean_flit_ar = axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(transfer_width),
            .arlen = axi::n_transfers_to_len(n_transfers),
            .araddr = access_base,
            .arid = 0b1011,
        };

        // Get the given amount of read responses
        for (int i = 0; i < n_transfers; i++) {
            inputs[280 + (i * 10)].clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32 {
                .rlast = (i == n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
            // R pass through a separate pipeline stage to register completions
            // see rule recv_r
            // => latency = 20
            outputs[280 + 20 + (i * 10)].iocap_flit_r = axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = (i == n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
        }

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct MismatchedPerms_Passthrough : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "Valid-Key Valid-Cap BadPerms - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        auto cap_data = this->test_random_initial_resource_cap(key, 111, CCapPerms_Read);
        auto axi_params = cap_data.valid_transfer_params(32, 20);
       
        U128 cap128 = U128::from_le(cap_data.cap.data);
        U128 sig128 = U128::from_le(cap_data.cap.signature);
        
        // Attempt a write with a read capability
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterBadWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(axi_params.transfer_width),
            .awlen = axi::n_transfers_to_len(axi_params.n_transfers),
            .awaddr = axi_params.address,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < axi_params.n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == axi_params.n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct InvalidSig_Passthrough : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "Valid-Key BadSig-Cap Write - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        U128 cap{
            .top = 0x00abcdef'11abcdef,
            .bottom = 0x22abcdef'33abcdef
        };
        U128 sig{
            .top = 0x44abcdef'55abcdef,
            .bottom = 0x66abcdef'77abcdef
        };

        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(32),
            .awlen = axi::n_transfers_to_len(1),
            .awaddr = 0xdeadbeefdeadbeef,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap, sig);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap, sig);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap, sig);
        // This will arrive at 135, exiting the AddressChannelCapUnwrapper at 145

        // The above capability claims to use KeyId 111 (TODO actually put it through a cap lib and check)
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over (the value doesn't matter)
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = sig
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterBadWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(32),
            .awlen = axi::n_transfers_to_len(1),
            .awaddr = 0xdeadbeefdeadbeef,
            .awid = 0b1011,
        };

        inputs[140].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
            .wlast = 1,
            .wstrb = 0b1111,
            .wdata = 0xfefefefe,
        };
        outputs[150].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
            .wlast = 1,
            .wstrb = 0b1111,
            .wdata = 0xfefefefe,
        };

        // Eventually we get a write response
        inputs[400].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[420].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_NoAccesses : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "New Epoch - No Accesses";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        inputs[100].keyManager.newEpochRequest = 1;
        outputs[120].keyManager.finishedEpoch = 0;

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_PreAccess : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "New Epoch - Pre-Access";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = random_initial_resource_cap(this->rng, key, 111, CCapPerms_Write);
        uint64_t cap_base = 0;
        uint64_t cap_len = 0;
        bool cap_is_almighty = false;
        if (ccap_read_range(&cap, &cap_base, &cap_len, &cap_is_almighty) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 20;
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // Just before it receives the final flit, it will also receive a new epoch request.
        inputs[120].keyManager.newEpochRequest = 1;
        // It will complete the epoch because it's not currently processing anything.
        // More specifically, it hasn't started requesting or using key manager data which is marked by the epoch.
        outputs[140].keyManager.finishedEpoch = 0;

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[600].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_SameCycle : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "New Epoch - Same Cycle as Access";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = random_initial_resource_cap(this->rng, key, 111, CCapPerms_Write);
        uint64_t cap_base = 0;
        uint64_t cap_len = 0;
        bool cap_is_almighty = false;
        if (ccap_read_range(&cap, &cap_base, &cap_len, &cap_is_almighty) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 20;
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // When it receives the final flit, it will also receive a new epoch request.
        inputs[130].keyManager.newEpochRequest = 1;
        // It will not respond, completing the finish, until the transaction finishes.

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[600].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].keyManager.finishedEpoch = 0;

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_PostAccess : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "New Epoch - Post-Access";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = random_initial_resource_cap(this->rng, key, 111, CCapPerms_Write);
        uint64_t cap_base = 0;
        uint64_t cap_len = 0;
        bool cap_is_almighty = false;
        if (ccap_read_range(&cap, &cap_base, &cap_len, &cap_is_almighty) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 20;
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the access
        inputs[100].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };
        inputs[110].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);

        // Just after it receives the final flit, it will also receive a new epoch request.
        inputs[140].keyManager.newEpochRequest = 1;
        // The request is in progress => won't complete the epoch until

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // Overall the authenticated flit passes through four fifos:
        // - At 145 it passes through a mkSizedBypassFIFOF awIn.out (0 cycle delay)
        // - into awPreCheckBuffer, a mkFIFOF (1 cycle), exits @ 155
        // - the keyResponse applies immediately
        // - into awChecker.checkRequest, a mkFIFOF (1 cycle) exits @ 165
        // It then splits into parallel lanes, because the key was valid:
        // - decodeIn (mkFIFOF, 2 cycle)  - sigCheckIn (mkFIFOF, 1 cycle) exits @ 175
        // - decodeOut (mkFIFOF, 2 cycle) - AES (8? cycles)
        // -           resps (mkFIFOF, 1 cycle) enters @ 255, exits @ 265
        //             we set the perf counter in check_aw and enqueue into awOut @265
        // -           awOut (mkFIFOF, 1 cycle) exits @ 275
        // -           we pick it up at 280
        outputs[270].keyManager.bumpPerfCounterGoodWrite = true;
        outputs[280].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Send the given amount of flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // Eventually we get a write response
        inputs[600].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[620].keyManager.finishedEpoch = 0;

        return {inputs.asVec(), outputs.asVec()};
    }
};

template<class DUT>
struct NewEpoch_BetweenAccesses : public ExposerCycleTest<DUT> {
    virtual std::string name() override {
        return "Valid-Key Valid-Cap Valid-Read; New Epoch; Invalid-Key Write - Passthrough";
    }
    virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
        ShimmedExposerInputsMaker inputs;
        ShimmedExposerOutputsMaker outputs;

        // Generate a random key, a random writable capability, and figure out the pow2 transfer width/n_transfers available
        U128 key = U128::random(this->rng);
        CCap2024_02 cap = random_initial_resource_cap(this->rng, key, 111, CCapPerms_ReadWrite);
        uint64_t cap_base = 0;
        uint64_t cap_len = 0;
        bool cap_is_almighty = false;
        if (ccap_read_range(&cap, &cap_base, &cap_len, &cap_is_almighty) != CCapResult_Success) {
            throw std::runtime_error("Failed to ccap_read_range");
        }
        uint8_t transfer_width = 32;
        uint8_t n_transfers = 20;
        if (!cap_is_almighty) {
            while (cap_len < transfer_width) {
                transfer_width = transfer_width >> 1;
            }
            if (transfer_width == 0) {
                throw std::runtime_error("Bad cap_len");
            }
            if (cap_len < (transfer_width * n_transfers)) {
                n_transfers = cap_len / transfer_width;
            }
            if (n_transfers == 0) {
                throw std::runtime_error("Bad cap_len");
            }
        }

        U128 cap128 = U128::from_le(cap.data);
        U128 sig128 = U128::from_le(cap.signature);
        
        // Send the flits to authenticate the first access
        inputs[100].iocap_flit_ar = axi::IOCapAxi::ARFlit_id4_addr64_user3 {
            .aruser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(transfer_width),
            .arlen = axi::n_transfers_to_len(n_transfers),
            .araddr = cap_base,
            .arid = 0b1011,
        };
        inputs[110].iocap_flit_ar = axi::IOCapAxi::packCap1_ar(cap128, sig128);
        inputs[120].iocap_flit_ar = axi::IOCapAxi::packCap2_ar(cap128, sig128);
        inputs[130].iocap_flit_ar = axi::IOCapAxi::packCap3_ar(cap128, sig128);

        inputs[100 + 10].iocap_flit_aw = axi::IOCapAxi::AWFlit_id4_addr64_user3 {
            .awuser = uint8_t(axi::IOCapAxi::IOCapAxi_User::Start),
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };
        inputs[110 + 10].iocap_flit_aw = axi::IOCapAxi::packCap1_aw(cap128, sig128);
        inputs[120 + 10].iocap_flit_aw = axi::IOCapAxi::packCap2_aw(cap128, sig128);
        inputs[130 + 10].iocap_flit_aw = axi::IOCapAxi::packCap3_aw(cap128, sig128);
        // Send the given amount of write data flits
        for (int i = 0; i < n_transfers; i++) {
            inputs[140 + (i * 10)].iocap_flit_w = axi::IOCapAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
            /// TODO should be blocked until the transaction has been measured as good
            outputs[150 + (i * 10)].clean_flit_w = axi::SanitizedAxi::WFlit_data32 {
                .wlast = (i == n_transfers - 1),
                .wstrb = 0b1111,
                .wdata = 0xfefefe00 + i,
            };
        }

        // When it receives the final flit of the first access, just before the final flit of the second access,
        // it will also receive a new epoch request.
        inputs[130].keyManager.newEpochRequest = 1;
        // This epoch will not complete until the *first* has completed, and the second will be delayed until the epoch completes

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[150].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        // For the first access, return *valid*.
        inputs[160].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = key
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        outputs[270].keyManager.bumpPerfCounterGoodRead = true;
        outputs[280].clean_flit_ar = axi::SanitizedAxi::ARFlit_id4_addr64_user0 {
            .arburst = uint8_t(axi::AXI4_Burst::Incr),
            .arsize = axi::transfer_width_to_size(transfer_width),
            .arlen = axi::n_transfers_to_len(n_transfers),
            .araddr = cap_base,
            .arid = 0b1011,
        };

        // Get the given amount of read responses
        for (int i = 0; i < n_transfers; i++) {
            inputs[280 + (i * 10)].clean_flit_r = axi::SanitizedAxi::RFlit_id4_data32 {
                .rlast = (i == n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
            // R pass through a separate pipeline stage to register completions
            // see rule recv_r
            // => latency = 20
            outputs[280 + 20 + (i * 10)].iocap_flit_r = axi::IOCapAxi::RFlit_id4_data32 {
                .rlast = (i == n_transfers - 1),
                .rresp = uint8_t(axi::AXI4_Resp::Okay),
                .rdata = 0xfefefe00 + i,
                .rid = 0b1011,
            };
        }
        // Only once the .rlast read has arrived has the transaction, and the epoch, completed.
        outputs[280 + 20 + ((n_transfers - 1) * 10)].keyManager.finishedEpoch = 0;
        // The write can now be processed by the exposer.
        uint64_t write_check_start = 280 + 20 + ((n_transfers - 1) * 10);

        // The above capability claims to use KeyId 111
        // so the exposer should ask for it
        outputs[write_check_start + 10].keyManager.keyRequest = 111;
        // and we will hand it over
        // NOTE: THIS MAY BE A VALUE FROM THE CURRENT OR NEXT EPOCH!
        // For the second access, return *invalid*.
        inputs[write_check_start + 10 + 20].keyManager.keyResponse = key_manager::KeyResponse {
            .keyId = 111,
            .key = std::nullopt,
        };
        // Then it will at some point later shove it out, while bumping the bad-write counter.
        // The clean_flit will come out one cycle after the perf counter is bumped, because it passes through a FIFO.
        // The delay will be short, because it will realize the key is invalid and short-circuit the loss
        outputs[write_check_start + 60].keyManager.bumpPerfCounterBadWrite = true;
        // TODO shouldn't come out
        outputs[write_check_start + 70].clean_flit_aw = axi::SanitizedAxi::AWFlit_id4_addr64_user0 {
            .awburst = uint8_t(axi::AXI4_Burst::Incr),
            .awsize = axi::transfer_width_to_size(transfer_width),
            .awlen = axi::n_transfers_to_len(n_transfers),
            .awaddr = cap_base,
            .awid = 0b1011,
        };

        // Eventually we get a write response
        inputs[write_check_start + 200].clean_flit_b = axi::SanitizedAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };
        outputs[write_check_start + 220].iocap_flit_b = axi::IOCapAxi::BFlit_id4 {
            .bresp = uint8_t(axi::AXI4_Resp::Okay),
            .bid = 0b1011,
        };

        return {inputs.asVec(), outputs.asVec()};
    }
};

// Template for further test creation

// template<class DUT>
// struct TODO : public ExposerCycleTest<DUT> {
//     virtual std::string name() override {
//         return "TODO";
//     }
//     virtual std::pair<ShimmedExposerInputs, ShimmedExposerOutputs> stimuli() {
//         ShimmedExposerInputsMaker inputs;
//         ShimmedExposerOutputsMaker outputs;
//
//         // TODO
//
//         return {inputs.asVec(), outputs.asVec()};
//     }
// };

int main(int argc, char** argv) {
    int ccapresult_success = EXIT_SUCCESS;

    std::vector<TestBase*> tests = {
        // Test valid caps are accepted
        new ValidKeyValidCapValidWrite<VmkSimpleIOCapExposer_Tb>(),
        new ValidKeyValidCapValidRead<VmkSimpleIOCapExposer_Tb>(),
        new ValidReadThenValidWrite<VmkSimpleIOCapExposer_Tb>(),
        // TODO Test caps with invalid keys are rejected - DONE below
        // TODO test valid cap with 1 cav
        // TODO test valid cap with 2 cav
        // Test valid cap with out-of-cap-bounds access
        new OOBWrite_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        new OOBRead_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        // Test valid cap with mismatched perms
        new MismatchedPerms_Passthrough<VmkSimpleIOCapExposer_Tb>(),
        // Test invalid caps (i.e. bad signatures) with valid keys are rejected
        new InvalidSig_Passthrough<VmkSimpleIOCapExposer_Tb>(),

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
        )
    };
    for (auto* test : tests) {
        if (!test->run(argc, argv)) {
            ccapresult_success = EXIT_FAILURE;
        }
    }

    return ccapresult_success;
}