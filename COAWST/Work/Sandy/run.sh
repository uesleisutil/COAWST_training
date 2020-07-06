cp ../../coawstM .
cp ../../Projects/Sandy/* .
cp ../../*TBL .
cp ../../WRF/run/ozone_plev.formatted .
cp ../../WRF/run/ozone_lat.formatted .
cp ../../WRF/run/ozone.formatted .
cp ../../WRF/run/RRTMG_LW_DATA .
cp ../../WRF/run/RRTMG_SW_DATA .

mpirun -np 2 ./coawstM ./coupling_sandy.in &> log.out &
