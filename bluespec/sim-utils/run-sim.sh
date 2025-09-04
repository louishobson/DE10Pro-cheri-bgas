#!/bin/bash

# Parameters
# $1: run name (alphanumeric with _, or -)
# $2: simulation name (alphanumeric with _, or -)
# $3: hex file
# $4: permanent output folder - results are saved in $3/$1
# $5: optional simulator script
# $6: optional max simulation run time
# $7: optional wither "no-uart" or "uart"

RUN_NAME="$1"
SIM_NAME="$2"
HEX_FILE="$3"
OUT_ROOT="$4"
SIM_SCPT="$5"
MAX_TIME="$6"
PNT_UART="$7"

SIM_DIR="/tmp/ldh35-sims/$RUN_NAME/$SIM_NAME"
OUT_DIR="$OUT_ROOT/$RUN_NAME/$SIM_NAME"
CTX_DIR="$SIM_DIR/sim_0.0"

MIN_RUN_TIME=20 # 1800

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

# Set the maximum run time to a day if not specified
if [[ -z "$MAX_TIME" ]]; then
    MAX_TIME="1d"
fi

# Check UART print flag
if [[ -z "$PNT_UART" ]]; then
    PNT_UART=0
elif [[ "$PNT_UART" == "show-uart" ]]; then
    PNT_UART=1
elif [[ "$PNT_UART" == "no-uart" ]]; then
    PNT_UART=0
else
    echo "$1: Error: invalid UART option '$PNT_UART'"
    exit 7
fi

# Wait with a timeout
# $1: the PID
# $2: the timeout
wait_timeout () {
    timeout $2 tail -s $3 -f --pid=$1 /dev/null
    return $?
}

# Termination sequence 
termination_sequence () {
    # Terminate UART output
    stop_uart
    # Terminate server
    if ps -p $SIM_PID > /dev/null; then
        echo "Interrupting server..."
        kill -s INT $SIM_PID && if ! wait_timeout $SIM_PID 5s 0.5; then
            echo "Server did not stop! Retrying..."
            kill -s INT $SIM_PID && if ! wait_timeout $SIM_PID 5s 0.5; then
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
    stop_uart
    echo "Received interrupt for $RUN_NAME/$SIM_NAME simulation!"
    termination_sequence
    OUT_DIR=$(mktemp -dp "$OUT_ROOT/$RUN_NAME" $SIM_NAME.intXXXXXX)
    echo "Redirecting $RUN_NAME/$SIM_NAME logs to '$OUT_DIR'"
    copy_logs
    echo "Done with $RUN_NAME/$SIM_NAME"
    exit 0
}

# Start and stop UART
start_uart () {
    # Output simulation STDOUT
    if (( $PNT_UART )); then
        echo -e "\n======== UART Output Starts ========"
        tail -n0 -f "$CTX_DIR/fmem_uart_stdout" 2>/dev/null & UART_PID=$!
    fi
}
stop_uart () {
    if (( $PNT_UART )); then
        if ps -p $UART_PID > /dev/null; then
            kill -s TERM $UART_PID
            wait $UART_PID
            echo -e "\n======== UART Output Ends ========\n"
        fi
    fi
}

# Actually do simulation
# Take out a temporary lock to stop spam starting simulations
LOCK_FD=3
eval "exec $LOCK_FD> /tmp/ldh35-sims.lock"
do_simulation () {
    # Try to lock
    flock $LOCK_FD
    { sleep 5s && flock -u $LOCK_FD; } &

    # Create the sim and output directories
    mkdir -p $CTX_DIR
    mkdir -p $OUT_ROOT/$RUN_NAME

    # Create the stdout fifo
    rm -f $CTX_DIR/sim_stdout
    mkfifo $CTX_DIR/sim_stdout 

    # Create the fmem_uart stdout file
    touch $CTX_DIR/fmem_uart_stdout

    # Start reading output
    gzip > $CTX_DIR/sim_stdout.gz < $CTX_DIR/sim_stdout & STDOUT_PID=$!

    # Set up traps
    trap handle_signal SIGINT
    trap handle_signal SIGTERM

    # Start CHERI-BGAS
    CHERI_BGAS_PC_RESET_VALUE=c0000000 CHERI_BGAS_DDRB_HEX_INIT="$HEX_FILE" ./cheri-bgas-sim.py -s "$SIM_SCPT" -r $SIM_DIR -t 1 1 > $SIM_DIR/server.log & SIM_PID=$!
    echo
    echo "Started $RUN_NAME/$SIM_NAME simulator"
    date | tee $SIM_DIR/time.log
    echo "Maximum execution time set to $MAX_TIME"
    echo "Sim directory is $SIM_DIR"

    # Time how long simulation runs for
    { time tail -s 1 -f --pid $SIM_PID /dev/null; } 2>> $SIM_DIR/time.log &

    # Start UART output
    start_uart

    # Wait for simulation termination (or a signal)
    START_TIME=$(date +%s)
    wait_timeout $STDOUT_PID $MAX_TIME 10
    SIM_TIMEOUT=$?
    END_TIME=$(date +%s)

    # Simulation stopped
    stop_uart
    echo "Simulation stopped for $RUN_NAME/$SIM_NAME!"
    return $SIM_TIMEOUT
}

RETRIES=2
while true; do
    # Do simulation
    do_simulation
    SIM_TIMEOUT=$?
    termination_sequence

    # Did we time out or run for more than 30 minutes? If not, something probably broke
    if (( !$SIM_TIMEOUT && ($END_TIME - $START_TIME < $MIN_RUN_TIME) )); then
        echo "Simulation failure for $RUN_NAME/$SIM_NAME!"
        # Copy logs to an error directory
        OUT_DIR_SAVE=$OUT_DIR
        OUT_DIR=$(mktemp -dp "$OUT_ROOT/$RUN_NAME" $SIM_NAME.failXXXXXX)
        echo "Redirecting $RUN_NAME/$SIM_NAME logs to '$OUT_DIR'"
        copy_logs

        # Retry some number of times
        if (( $RETRIES )); then
            echo "Retrying $RUN_NAME/$SIM_NAME..."
            RETRIES=$(( $RETRIES - 1 ))
            OUT_DIR=$OUT_DIR_SAVE
            sleep 10s # Let everything calm down before retrying
            continue
        else
            echo "Giving up on $RUN_NAME/$SIM_NAME :("
            break
        fi
    fi

    # Copy logs and exit
    copy_logs
    echo "Done with $RUN_NAME/$SIM_NAME"
    break
done