#!/bin/sh
#SBATCH --job-name=slurmr-job-71774ab71597
#SBATCH --output=/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-71774ab71597/02-output-%A-%a.out
#SBATCH --array=1-2
#SBATCH --job-name=slurmr-job-71774ab71597
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
/usr/lib/R/bin/Rscript  /home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-71774ab71597/00-rscript.r
