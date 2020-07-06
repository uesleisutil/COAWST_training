cp ../../coawstM .
cp ../../Projects/Sandy/* .

mpirun -np 2 ./coawstM ./coupling_sandy.in &> log.out &
