#!/bin/bash
#SBATCH --job-name=combine_hash
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --time=24:00:00
#SBATCH --mem=128G

#
# modules
#
module load R
Rscript %x.R
