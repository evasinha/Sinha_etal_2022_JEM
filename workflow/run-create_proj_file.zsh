#!/bin/zsh
#SBATCH -A IHESD
#SBATCH -t 180
#SBATCH -N 1
#SBTACH -J create_gcam_prj
 
job=$SLURM_JOB_NAME
   
date
Rscript fert_constrain_create_proj_file.R
date
