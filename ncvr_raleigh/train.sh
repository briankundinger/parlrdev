#!/bin/bash
#SBATCH --array=1-2
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=training
#SBATCH --time=24:00:00
#SBATCH --mem-per-cpu=128G
#
# modules
#
module load R
Rscript train.R
