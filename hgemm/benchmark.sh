#!/bin/bash

# Create job script
jobscript=$(cat <<EOF
#!/bin/bash

#PJM -L "elapse=10:00"
#PJM -L "node=1"

./BINARY
EOF
)

for problem_size in 50 100 200 500 1000 2000 5000; do
    echo "Submitting for $problem_size"
    make clean && PROBLEM_SIZE=$problem_size make -s
    mv main main_${problem_size}
    echo "${jobscript/BINARY/main_${problem_size}}" > job_${problem_size}.sh
    pjsub job_${problem_size}.sh
done
