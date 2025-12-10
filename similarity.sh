# mamba create -n funannotate2 gfftk gapmm2 minimap2 miniprot snap "augustus==3.5.0" glimmerhmm diamond trnascan-se table2asn gb-io buscolite clustalo

mamba activate funannotate2

# pwd /mnt/d/sacc_genomes/cAMP_msa
# ls
# sok2.faa  tpk1.faa  tpk2.faa

clustalo -i seqs.faa --distmat-out=distmat.txt --full --threads=4
for f in *.faa; do base="${f%.*}"
    echo "Processing $base"
clustalo -i "$f" --distmat-out="${base}_dist.tsv" --full --threads=4
  done
  
  