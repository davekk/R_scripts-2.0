mamba activate rrna_env

# 1.1 check basic stat
seqkit stat Alpa_sacc.fa
 
# 1.3 Remove exact duplicates (keeping first)
#seqkit rmdup -s Alpa_sacc.fa -o Alpa_sacc.faa

# 1.4 Make headers simple (optional) — keep original names in lookup file
seqkit replace -p " " -r "_" Alpa_sacc.fa -o Alpa_sacc.clean.fa

# 
mafft --thread 8 --maxiterate 1000 --localpair Alpa_sacc.clean.fa > Alpa_sacc.aln.fa


# example: conservative trimming keeping informative sites
clipkit Alpa_sacc.aln.fa -m smart-gap -o Alpa_sacc.clipkit.fa

# alternative: stricter
# clipkit gene.aln.faa -m kpic-gappy -o gene.clipkit.gappy.faa


# basic high-confidence run
iqtree3 -s Alpa_sacc.clipkit.fa -m MFP -bb 1000 -alrt 1000 -nt 8 -pre gene_iqtree -bnni

