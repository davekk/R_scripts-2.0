mamba activate rrna_env

# make sure output directories exist
mkdir -p busco_phylo/single_copy_buscos
# Extract complete single-copy BUSCO genes that are present in all genomes
# First, get list of complete single-copy BUSCOs present in all genomes
for g in BUSCO/results/*; do
    name=$(basename $g)
    for f in $g/run_saccharomycetes_odb12/busco_sequences/single_copy_busco_sequences/*.faa; do
        busco=$(basename $f .faa)
        cp $f busco_phylo/single_copy_buscos/${name}_${busco}.faa
    done
done

#Step 2. Group sequences per BUSCO gene
cd busco_phylo
mkdir -p grouped

# Group by BUSCO ID

# Extract BUSCO IDs from filenames
mkdir -p grouped

for f in single_copy_buscos/*.faa; do
    species=$(basename "$f" | cut -d"_" -f1)       # e.g. Asc-rubescens
    buscoid=$(basename "$f" | cut -d"_" -f2- | sed 's/\.faa$//')  # e.g. sac12_1002at4891
    # Rewrite header to add species prefix
    sed "s/^>/>${species}_${buscoid}_/" "$f" >> grouped/${buscoid}.faa
done

#Step 3. Multiple sequence alignment
mkdir -p aligned

find grouped -name "*.faa" \
  | parallel -j 8 "mafft --retree 1 --maxiterate 0 {} > aligned/{/.}.aln.faa"

#Step 4 Trim alignments
mkdir -p trimmed
find cleaned -name "*.aln.faa" \
  | parallel -j 8 "clipkit {} -m gappy -o trimmed/{/.}.clipkit.faa"


# Step 5 (Simplify to >Species_BUSCOID)
mkdir -p cleaned

for f in trimmed/*.clipkit.faa; do
       awk '/^>/{sub(/_sac[0-9]+_[0-9]+at[0-9]+.*/, "", $0)}1' "$f" > cleaned/$(basename "$f")
done
#Verify uniqueness of headers in each file
for f in cleaned/*.aln.clipkit.faa; do
    echo "Checking $f"
    seqkit seq -n "$f" | sort | uniq -d
done

#check how many seq per locus
for f in cleaned/*.aln.clipkit.faa; do
    count=$(seqkit seq -n "$f" | wc -l)
    echo "$(basename "$f"): $count sequences"
done | column -t

#filter to drop loci with <70% coverage (22)
mkdir -p filtered

threshold=22   # 70% of 31 species ≈ 21.7

for f in cleaned/*.aln.clipkit.faa; do
    count=$(seqkit seq -n "$f" | wc -l)
    if [ "$count" -ge "$threshold" ]; then
        cp "$f" filtered/
    fi
done

#Step 5. Concatenate alignments
mkdir -p concat

AMAS.py concat -i filtered/*.aln.clipkit.faa -f fasta -d aa -p concat/partition.txt -t concat/supermatrix.faa --part-format raxml

cd concat

seqkit seq supermatrix.faa | grep -v ">" | grep -o . | sort | uniq -c

#Step 6. Phylogenetic inference with IQ-TREE

iqtree3 -s supermatrix.faa -p partition.txt -m MFP+MERGE -B 1000 -alrt 1000 -T 8 --bnni
