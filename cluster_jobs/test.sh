#!/bin/bash
#SBATCH --array=1-78
#SBATCH --mail-type=end
#SBATCH --mail-user=bak47@duke.edu
#SBATCH --output=logs/%x_%a.out
#SBATCH --error=logs/%x_%a.err
#SBATCH --job-name=test
#SBATCH --time=24:00:00
#SBATCH --mem=128G

#
# modules
#
module load R
Rscript test.R
