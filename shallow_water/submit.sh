# Process arguments
if [ $# -lt 3 ]; then
    echo "submit.sh precision resolution experiment_id"
    exit 1
fi

PREC=$1
RES=$2
EXPID=$3

# Root directory
SWROOT=/vol0004/share/ra000005/user/hatfield/fugaku_toys/shallow_water

# Binary path
BIN=$SWROOT/bin/main.${PREC}

# Run directory
RUNDIR=${SWROOT}/run_${PREC}_${EXPID}

# Create rundir
mkdir -p ${RUNDIR}

cd ${RUNDIR}

# Link restart files
for field in u v h; do
    ln -sf ${SWROOT}/restarts/${field}.${RES}x${RES}.restart.txt ${field}.restart.in.txt
done

# Create job script
cat <<EOF > ${RUNDIR}/job.sh
#!/bin/bash

#PJM -L "node=1"
#PJM -L "elapse=60:00"

cd $RUNDIR

${SWROOT}/bin/main.${PREC}

EOF

# Submit
pjsub job.sh
