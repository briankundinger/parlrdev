#!/bin/sh
#SBATCH --job-name=slurmr-job-7016688014b
#SBATCH --output=/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-7016688014b/02-output-%A-%a.out
#SBATCH --array=1-2
#SBATCH --job-name=slurmr-job-7016688014b
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
/usr/lib/R/bin/Rscript  /home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-7016688014b/00-rscript.r
