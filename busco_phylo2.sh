# mamba create -n funannotate2 gfftk gapmm2 minimap2 miniprot snap "augustus==3.5.0" glimmerhmm diamond trnascan-se table2asn gb-io buscolite

# mamba create -n busco6 -c conda-forge -c bioconda busco=6.0.0

# mamba create -n rrna_env -c conda-forge -c bioconda mafft clipkit seqkit parallel amas iqtree

mamba activate funannotate2

# standardizing naming for genome files
for f in new_geno_rmod/*.clean.fna; do
    mv "$f" "${f// /_}"
done

#genome clean for gene annotation  
for f in new_geno_rmod/*.f*a; do bn=$(basename "$f");echo "running file $(basename "$f")"; funannotate2 sort -f "$f" -o new_geno_rmod/"${bn%%.*}.sort.fna"  --cpus 6; echo "Done processing $bn"; done

mamba deactivate

mamba activate busco6

#test busco that code
PYTHONWARNINGS="ignore" busco -i new_geno_rmod/ -l saccharomycetes_odb12 -o busco_S2 -m genome --cpu 2


for f in new_genome_mask/*.fna; do
    bn=$(basename "$f" .clean.fna)   # strip .clean.fna from the name
	echo "▶ Running BUSCO on $bn with fungi_odb12"
    PYTHONWARNINGS="ignore" busco -i "$f"  -l /mnt/d/sacc_genomes/new_busco/busco_downloads/lineages/fungi_odb12 -o new_busco/"${bn}_fungi_odb12" -m genome --cpu 6
    echo "✔ Finished $bn"
done
mamba deactivate

mamba activate rrna_env
# make sure output directories exist
mkdir -p new_busco_phylo/single_copy_buscos
# Extract complete single-copy BUSCO genes that are present in all genomes
# First, get list of complete single-copy BUSCOs present in all genomes
for g in new_busco/results/*; do
    name=$(basename $g)
    for f in $g/run_*_odb12/busco_sequences/single_copy_busco_sequences/*.faa; do
        busco=$(basename $f .faa)
        cp $f new_busco_phylo/single_copy_buscos/${name}_${busco}.faa
    done
done


#Step 2. Group sequences per BUSCO gene
cd new_busco_phylo

# Extract BUSCO IDs from filenames
mkdir -p grouped

for f in single_copy_buscos/*.faa; do
    # Extract species = first 2 terms of filename
    species=$(basename "$f" | awk -F'_' '{print $1"_"$2}')
    buscoid=$(basename "$f" | sed 's/.*fungi_odb[0-9]\+_//; s/\.faa$//')
    
    # Rewrite header: keep only species name
    sed "s/^>.*/>${species}/" "$f" >> grouped/${buscoid}.faa
done

#Step 3. Multiple sequence alignment
mkdir -p aligned

find grouped -type f -name '*.faa' -print0 \
  | parallel -0 -j 8 'id=$(basename {} .faa); mafft --auto {} > aligned/${id}.aln.faa'		  
#Step 4 Trim alignments

mkdir -p trimmed
find aligned -name "*.aln.faa" \
  | parallel -j 8 "clipkit {} -m gappy -o trimmed/{/.}.clipkit.faa"



#Verify uniqueness of headers in each file
for f in trimmed/*.aln.clipkit.faa; do
    echo "Checking $f"
    seqkit seq -n "$f" | sort | uniq -d
done

#check how many seq per locus
for f in trimmed/*.aln.clipkit.faa; do
    count=$(seqkit seq -n "$f" | wc -l)
    echo "$(basename "$f"): $count sequences"
done | column -t
# more efficient with grep
for f in trimmed/*.aln.clipkit.faa; do
    count=$(grep -c "^>" "$f")
    echo "$(basename "$f"): $count sequences"
done | column -t

# Count sequences per locus

#filter to drop loci with <70% coverage (22)
mkdir -p filtered

#threshold=22   # 70% of 31 species ≈ 21.7
threshold=15   # 70% of 15 species ≈ 10.5
for f in trimmed/*.aln.clipkit.faa; do
    count=$(seqkit seq -n "$f" | wc -l)
    if [ "$count" -ge "$threshold" ]; then
        cp "$f" filtered/
    fi
done
# more effecient with grep
for f in trimmed/*.aln.clipkit.faa; do
    # count sequences using grep (faster than seqkit)
    count=$(grep -c "^>" "$f")

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
