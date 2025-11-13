mamba create -n rrna_env
mamba activate rrna_env
mamba install -c bioconda barrnap itsx bedtools seqkit

mkdir -p barrnap_results ITSx_results
for genome in ncbi_blast_genomes/*.fna; do
    sample=$(basename $genome .fna)
    outdir=barrnap_results/$sample
    mkdir -p $outdir
    barrnap --kingdom euk --threads 8 --reject 0.2\
       --outseq $outdir/${sample}.rRNA.fasta \
       "$genome" > $outdir/${sample}.gff
done

for genome in ncbi_blast_genomes/*.fna; do
    sample=$(basename $genome .fna)
  outdir=ITSx_results/$sample
  mkdir -p "$outdir"/chunks
  echo ">>> Processing $sample"

  # 1. Validate input genome
  if [ ! -s "$genome" ]; then
    echo "Input genome $genome is empty or missing, skipping"
    continue
  fi
  seqkit stats "$genome" > "$outdir"/$sample.genome_stats.txt
  if ! grep -q ">" "$genome"; then
    echo "Input genome $genome is not a valid FASTA file, skipping"
    continue
  fi

  # 2. Split into ≤100 kb pieces
  seqkit split2 -s 100000 "$genome" -O "$outdir"/chunks 2> "$outdir"/$sample.split.log
  if [ $? -ne 0 ]; then
    echo "seqkit split failed for $sample, check $outdir/$sample.split.log"
    echo "Falling back to full genome ITSx"
    ITSx -i "$genome" -o "$outdir"/$sample --taxa Fungi --cpu 8 --preserve T --evalue 1e-4 \
      2> "$outdir"/$sample.itsx_full.log
    if [ $? -ne 0 ]; then
      echo "ITSx on full genome failed for $sample, check $outdir/$sample.itsx_full.log"
      continue
    fi
  else
    # Check if chunks were created
    if [ -z "$(ls -A $outdir/chunks/*.fasta 2>/dev/null)" ]; then
      echo "No chunks created for $sample, falling back to full genome ITSx"
      ITSx -i "$genome" -o "$outdir"/$sample --taxa Fungi --cpu 8 --preserve T --evalue 1e-4 \
        2> "$outdir"/$sample.itsx_full.log
      if [ $? -ne 0 ]; then
        echo "ITSx on full genome failed for $sample, check $outdir/$sample.itsx_full.log"
        continue
      fi
    else
      # 3. Run ITSx on chunks
      ITSx -i "$outdir"/chunks/*.fasta \
           -o "$outdir"/$sample \
           --taxa Fungi --cpu 8 --preserve T --evalue 1e-4 \
           2> "$outdir"/$sample.itsx.log
      if [ $? -ne 0 ]; then
        echo "ITSx failed for $sample, check $outdir/$sample.itsx.log"
        echo "Trying Eukarya profiles"
        ITSx -i "$outdir"/chunks/*.fasta \
             -o "$outdir"/${sample}_euk \
             --taxa Eukarya --cpu 8 --preserve T --evalue 1e-4 \
             2> "$outdir"/${sample}_euk.itsx.log
        if [ -s "$outdir"/${sample}_euk.positions.txt ]; then
          mv "$outdir"/${sample}_euk.* "$outdir"/$sample.
          echo "Eukarya profiles succeeded for $sample"
        else
          echo "No ITS regions detected with Eukarya profiles for $sample"
        fi
      fi
    fi
  fi

  # 4. Combine ITS1 + 5.8S + ITS2
  if [[ -s "$outdir"/$sample.ITS1.fasta && -s "$outdir"/$sample.ITS2.fasta ]]; then
    cat "$outdir"/$sample.ITS1.fasta \
        "$outdir"/$sample.5_8S.fasta \
        "$outdir"/$sample.ITS2.fasta \
      | seqkit seq -w 0 \
      > "$outdir"/${sample}.ITS_combined.fasta
    echo ">>> Combined ITS written to $outdir/${sample}.ITS_combined.fasta"
  else
    echo ">>> No complete ITS found for $sample, check $outdir/$sample.positions.txt"
  fi

  # 5. Report 18S/28S
  if [[ -s "$outdir"/$sample.18S.fasta || -s "$outdir"/$sample.28S.fasta ]]; then
    echo ">>> 18S/28S detected for $sample, check $outdir/$sample.{18S,28S}.fasta"
    seqkit stats "$outdir"/$sample.{18S,28S}.fasta >> "$outdir"/$sample.rRNA_stats.txt
  else
    echo ">>> No 18S/28S detected for $sample"
  fi
done