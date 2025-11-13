mamba activate rrna_env

#For Each Gene, Separate Full from Partial
#    18S: ~1800 bp

 #   ITS (incl. 5.8S): ~600 bp (highly variable, check your data!)

#    5.8S: ~160 bp

#    28S: ~3300 bp
# For 18S
seqkit seq -m 1700 -g ../All_sacc_asc_18s.fa > 18s_full.fa
seqkit seq -M 1700 -g ../All_sacc_asc_18s.fa > 18s_partial.fa

# For 5.8S
seqkit seq -m 150 -g ../All_sacc_asc_5-8s.fa > 5.8s_full.fa
seqkit seq -M 150 -g ../All_sacc_asc_5-8s.fa > 5.8s_partial.fa

# For ITS (adjust threshold as needed)
seqkit seq -m 500 -g ../All_sacc_asc_its.fa > its_full.fa
seqkit seq -M 500 -g ../All_sacc_asc_its.fa > its_partial.fa

# For 28S
seqkit seq -m 3000 -g ../All_sacc_asc_28s.fa > 28s_full.fa
seqkit seq -M 3000 -g ../All_sacc_asc_28s.fa > 28s_partial.fa
#Align Full-Length Sequences for Each Gene
# Align 18S and 28S with MAFFT
mafft --auto --thread 4 18s_full.fa > 18s_full_aln.fa
mafft --auto 28s_full.fasta > 28s_full_aln.fasta

# Align ITS. --adjustdirection is helpful if sequences are reverse complemented.
mafft --auto --thread 4 --reorder its_full.fasta > its_full_aln.fasta 
# Process alignment for all genes

for gene in *_full.fa; do
    echo "Aligning and trimming $gene..."
    
    # Align
    mafft --auto --thread 4 "$gene" > "${gene%.fa}_aln.fa"
    
    # Trim
    clipkit "${gene%.fa}_aln.fa" -o "${gene%.fa}_aln_trimmed.fa"
done #

# Concatenate and create partition file (AMAS.py)
AMAS.py concat -i *full_aln_trimmed.fa -f fasta -d dna -p partitions.txt -t concatenated_full.fa --part-format raxml

# Build initial phylogeny with full-data taxa
iqtree3 -s concatenated_full.fa -spp partitions.txt -m MFP+MERGE -T AUTO -bb 1000 -alrt 1000 -pre initial_full -T 8



