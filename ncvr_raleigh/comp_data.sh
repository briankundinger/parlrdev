#!/bin/bash
#SBATCH --array=1-50
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=comp_data
#SBATCH --time=24:00:00
#SBATCH --mem=128GB

#
# modules
#
module load R
Rscript comp_data.R
