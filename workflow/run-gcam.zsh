#!/bin/zsh
#SBATCH -A IHESD
#SBATCH -t 180
#SBATCH -N 1
 
job=$SLURM_JOB_NAME
  
   
echo 'Library config:'
ldd ./gcam.exe
    
date
time ./gcam.exe -Cconfig-$job.xml -Llog_conf.xml
date
