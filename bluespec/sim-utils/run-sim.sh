#!/bin/bash

# Parameters
# $1: run name (alphanumeric with _, or -)
# $2: simulation name (alphanumeric with _, or -)
# $3: hex file
# $4: permanent output folder - results are saved in $3/$1

RUN_NAME="$1"
SIM_NAME="$2"
HEX_FILE="$3"
OUT_ROOT="$4"

SIM_DIR="/tmp/run-sim/$RUN_NAME/$SIM_NAME"
OUT_DIR="$OUT_ROOT/$RUN_NAME/$SIM_NAME"
CTX_DIR="$SIM_DIR/sim_0.0"

# Check run name
if [[ ! "$RUN_NAME" =~ ^[a-zA-Z0-9][a-zA-Z0-9_\\.\\-]*$ ]]; then
    echo "$1: Error: bad run name '$RUN_NAME'"
    exit 1
fi

# Check simulation name
if [[ ! "$SIM_NAME" =~ ^[a-zA-Z0-9][a-zA-Z0-9_\\.\\-]*$ ]]; then
    echo "$1: Error: bad simulation name '$SIM_NAME'"
    exit 2
fi

# Check hex file exists
if [[ ! -e "$HEX_FILE" ]]; then
    echo "$1: Error: non-existent memory hex file '$HEX_FILE'"
    exit 3
fi

# Check the output root folder exists
if [[ ! -d "$OUT_ROOT" ]]; then
    echo "$1: Error: non-existent root output folder '$OUT_ROOT'"
    exit 4
fi

# Check the output directory doesn't exist
# Don't count this as an error
if [[ -d "$OUT_DIR" ]]; then
    echo "$1: Warning: output folder already exists '$OUT_DIR'"
    echo "Done with $RUN_NAME/$SIM_NAME"
    exit 0
fi

# Create the sim and output directories
mkdir -p $CTX_DIR

# Create the stdout fifo
rm -f $CTX_DIR/sim_stdout
mkfifo $CTX_DIR/sim_stdout 

# Wait with a timeout
# $1: the PID
# $2: the timeout
wait_timeout () {
    timeout $2 tail -s 1 -f --pid=$1 /dev/null
    return $?
}

# Termination sequence 
termination_sequence () {
    # Terminate server
    if ps -p $SIM_PID > /dev/null; then
        echo "Interrupting server..."
        kill -s INT $SIM_PID && if ! wait_timeout $SIM_PID 5s; then
            echo "Server did not stop! Retrying..."
            kill -s INT $SIM_PID && if ! wait_timeout $SIM_PID 5s; then
                echo "Server still did not stop! Terminating..."
                kill -s TERM $SIM_PID
            fi
        fi
    fi
    wait
    echo "All child processes stopped"

    # Copy logs
    echo "Copying logs..."
    mkdir -p $OUT_DIR
    cp $CTX_DIR/sim_stderr $CTX_DIR/fmem_uart_stdout $CTX_DIR/fmem_uart_stderr $SIM_DIR/server.log $SIM_DIR/simulation.info $SIM_DIR/time.log $OUT_DIR/
    mv $CTX_DIR/sim_stdout.gz $OUT_DIR/

    # Done
    echo "Done with $RUN_NAME/$SIM_NAME"
    exit 0
}

# Handle sigint and sigterm gracefully
handle_signal () {
    trap - SIGINT
    trap - SIGTERM
    echo "Received interrupt for $RUN_NAME/$SIM_NAME simulation!"
    termination_sequence
}
trap handle_signal SIGINT
trap handle_signal SIGTERM

# Compress sim_stdout
gzip > $CTX_DIR/sim_stdout.gz < $CTX_DIR/sim_stdout & STDOUT_PID=$!

# Start CHERI-BGAS
CHERI_BGAS_PC_RESET_VALUE=c0000000 CHERI_BGAS_DDRB_HEX_INIT="$HEX_FILE" ./cheri-bgas-sim.py -r $SIM_DIR -t 1 1 > $SIM_DIR/server.log & SIM_PID=$!
echo "Started $RUN_NAME/$SIM_NAME simulator"

# Time how long simulation runs for
{ time tail -s 1 -f --pid $SIM_PID /dev/null; } 2> $SIM_DIR/time.log &

# Wait for simulation termination (or a signal)
wait_timeout $STDOUT_PID 3h
echo "Simulation finished for $RUN_NAME/$SIM_NAME"
termination_sequence
