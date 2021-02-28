#!/bin/sh
#SBATCH --job-name=slurmr-job-613e48cc61cd
#SBATCH --output=/home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-613e48cc61cd/02-output-%A-%a.out
#SBATCH --array=1-10
#SBATCH --job-name=slurmr-job-613e48cc61cd
#SBATCH --cpus-per-task=1
#SBATCH --ntasks=1
/usr/lib/R/bin/Rscript  /home/users/bak47/projects/parlrdev/cluster_jobs/slurmr-job-613e48cc61cd/00-rscript.r
