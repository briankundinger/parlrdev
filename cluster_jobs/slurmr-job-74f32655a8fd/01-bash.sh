#!/bin/sh
#SBATCH --job-name=slurmr-job-74f32655a8fd
#SBATCH --output=/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-74f32655a8fd/02-output-%A-%a.out
#SBATCH --array=1-2
#SBATCH --job-name=slurmr-job-74f32655a8fd
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
/usr/lib/R/bin/Rscript  /home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-74f32655a8fd/00-rscript.r
