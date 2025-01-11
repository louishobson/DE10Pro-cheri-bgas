#!/bin/bash

# Parameters
# $1: run name (alphanumeric with _, or -)
# $2: simulation name (alphanumeric with _, or -)
# $3: hex file
# $4: permanent output folder - results are saved in $3/$1
# $5: optional simulator script

RUN_NAME="$1"
SIM_NAME="$2"
HEX_FILE="$3"
OUT_ROOT="$4"
SIM_SCPT="$5"

SIM_DIR="/tmp/ldh35-sims/$RUN_NAME/$SIM_NAME"
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

# Check that the simulation binary exists
if [[ -z "$SIM_SCPT" ]]; then
    SIM_SCPT="../build/simdir/sim_CHERI_BGAS"
fi
if [[ ! -e "$SIM_SCPT" ]]; then
    echo "$1: Error: non-existent simulation script '$SIM_SCPT'"
    exit 5
fi

# Take out a temporary lock to stop spam starting simulations
LOCK_FD=3
eval "exec $LOCK_FD> /tmp/ldh35-sims.lock"
flock $LOCK_FD
{ sleep 3s && flock -uo $LOCK_FD; } &

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
    printf "\n" >> $SIM_DIR/time.log
    date | tee -a $SIM_DIR/time.log
}

# Copy logs out of the sim dir
copy_logs () {
    echo "Copying logs..."
    mkdir -p $OUT_DIR
    cp $CTX_DIR/sim_stderr $CTX_DIR/fmem_uart_stdout $CTX_DIR/fmem_uart_stderr $SIM_DIR/server.log $SIM_DIR/simulation.info $SIM_DIR/time.log $OUT_DIR/
    mv $CTX_DIR/sim_stdout.gz $OUT_DIR/
}

# Handle sigint and sigterm gracefully
handle_signal () {
    trap - SIGINT
    trap - SIGTERM
    echo "Received interrupt for $RUN_NAME/$SIM_NAME simulation!"
    termination_sequence
    copy_logs
    echo "Done with $RUN_NAME/$SIM_NAME"
    exit 0
}
trap handle_signal SIGINT
trap handle_signal SIGTERM

# Start reading output
gzip > $CTX_DIR/sim_stdout.gz < $CTX_DIR/sim_stdout & STDOUT_PID=$!

# Start CHERI-BGAS
CHERI_BGAS_PC_RESET_VALUE=c0000000 CHERI_BGAS_DDRB_HEX_INIT="$HEX_FILE" ./cheri-bgas-sim.py -s "$SIM_SCPT" -r $SIM_DIR -t 1 1 > $SIM_DIR/server.log & SIM_PID=$!
echo "Started $RUN_NAME/$SIM_NAME simulator"
date | tee $SIM_DIR/time.log
echo "Sim directory is $SIM_DIR"

# Time how long simulation runs for
{ time tail -s 1 -f --pid $SIM_PID /dev/null; } 2>> $SIM_DIR/time.log &

# Wait for simulation termination (or a signal)
START_TIME=$(date +%s)
wait_timeout $STDOUT_PID 1h
END_TIME=$(date +%s)

# Simulation stopped, so terminate the server
echo "Simulation stopped for $RUN_NAME/$SIM_NAME!"
termination_sequence

# Did we run for more than a minute? If not, something probably broke
if (( $END_TIME - $START_TIME < 60 )); then
    mkdir -p "$OUT_ROOT/$RUN_NAME"
    OUT_DIR=$(mktemp -dp "$OUT_ROOT/$RUN_NAME" $SIM_NAME.errXXXXXX)
    echo "Simulation failure: redirecting logs to '$OUT_DIR'"
fi

# Copy logs and exit
copy_logs
echo "Done with $RUN_NAME/$SIM_NAME"
exit 0

