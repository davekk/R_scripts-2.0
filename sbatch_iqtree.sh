#!/bin/bash
#SBATCH --job-name=iqtree_busco
#SBATCH --output=/home/dkiambi/iqtree/iqtree_busco.%j.out
#SBATCH --error=/home/dkiambi/iqtree/iqtree_busco.%j.err
#SBATCH --cpus-per-task=20         # 16 threads
#SBATCH --partition=batch          # best for CPU scaling

# Load IQ-TREE (adjust if using conda/mamba instead of modules)
module load iqtree/2.4.0

# Go to job directory
cd /home/dkiambi/iqtree

# Run IQ-TREE with checkpoint support
iqtree2 -s supermatrix.faa -p partition.txt -m MFP+MERGE -T 20 -pre merge_test -n 0

# Extract the best model from the log
BEST_MODEL=$(grep -m 1 "Best-fit model according to BIC" merge_test.log | awk '{print $NF}')

echo ">>> Best model selected: $BEST_MODEL"
#
# Step 2: Optimal tree search with bootstraps + SH-aLRT
echo ">>> Running final tree search with $BEST_MODEL..."
iqtree2 -s supermatrix.faa -p merge_test.best_scheme.nex -m $BEST_MODEL -B 1000 --bnni -alrt 1000 -T 20 -pre final_tree

echo ">>> IQ-TREE finished successfully!"