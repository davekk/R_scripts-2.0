for file in *.f*a; do base="${file%.fasta}"; echo makeblastdb -in "$file" -dbtype nucl -out "${base}_db" -title "$base"; done

for file in *.f*a; do base=$(basename "$file" .fasta); makeblastdb -in "$file" -dbtype nucl -out "${base}_db" -title "$base"; done


mamba create -n busco -c conda-forge -c bioconda busco=5.4.3
mamba activate busco

mamba create -n busco6 -c conda-forge -c bioconda busco=6.0.0
mamba activate busco6

mamba create -n funannotate2 gfftk gapmm2 minimap2 miniprot snap "augustus==3.5.0" glimmerhmm diamond trnascan-se table2asn gb-io buscolite

mamba activate funannotate2 
busco --download saccharomycetaceae_odb12

busco -i ../ncbi_blast_genomes/S288C_reference_sequence_R64-4-1_20230823.fsa -l saccharomycetes_odb10 -o busco_S2 -m genome --cpu 2

PYTHONWARNINGS="ignore"


for f in $(ls genome_masked_rmod/*.fna | tail -n +4); do bn=$(basename "$f");\
 res="${bn:0:3}-${bn#*_}"; res="${res%%[_\.]*}_sac12"; \
 echo "running file $(basename "$f")";\
 PYTHONWARNINGS="ignore" busco -i "$f" -l saccharomycetes_odb12 -o "$res" -m genome --cpu 6;\
 echo "Done processing $bn"; \
 done
 
 funannotate clean -i -o genome_masked_rmod/Ascoidea_rubescens.clean.fna

#genome clean for gene annotation  
 for f in ncbi_blast_genomes/*.f*a; do bn=$(basename "$f");echo "running file $(basename "$f")"; funannotate2 clean -f "$f" -o genome_masked_rmod/"${bn%%.*}.clean.fna"  --cpus 6; echo "Done processing $bn"; done
 
 for f in genome_masked_rmod/*.f*a; do bn=$(basename "$f");echo "${bn%%.*}";done
 
 for f in genome_masked_rmod/*.f*a; do bn=$(basename "$f");echo "running file $(basename "$f")"; funannotate2 sort -f "$f" -o genome_masked_sort/"${bn%%.*}.sort.fna"  --cpus 6; echo "Done processing $bn"; done
  
vdb-validate 

for f in $(ls); do [ -f "$f" ] && fasterq-dump --split-files --threads 6 --temp temp --outdir fastq $(f);gzip fastq/"$(f)"_*.fastq;done

for f in *; do [ -f "$f" ] &&  echo "running $
(f)"; echo "done running $(f)";done

for f in *; do [ -f "$f" ] && echo "running $f" && fasterq-dump --split-files --threads 6 --temp temp --outdir fastq $(f) && echo "done running $f"; done

fasterq-dump --split-files --threads 2 --temp

for f in *.fastq;do echo "processing $f"; done

mkdir -p ~/funannotate2_db
export FUNANNOTATE2_DB=~/funannotate2_db
funannotate2 install -d all
echo 'export FUNANNOTATE2_DB=~/funannotate2_db' >> ~/.bashrc
source ~/.bashrc

funannotate2 train -f genome_masked_rmod/Saccharomycopsis_vini_v1_genomic.clean.fna -s "Saccharomycopsis vini" -o fun2_out/Saccharomycopsis_vini --cpus 6 --augustus_species saccharomyces_cerevisiae_S288C

for f in ../genome_masked_rmod/*_genomic.clean.fna; do funannotate2 train -f "$f" -s "$(basename "$f" _genomic.clean.fna | sed 's/_/ /g')" -o ../fun2_out/$(basename "$f" _genomic.clean.fna) --cpus 6; done

mamba create -n BUSCO_phylogenomics -c bioconda busco_phylogenomics
conda activate BUSCO_phylogenomics

mamba activate BUSCO_phylogenomics

BUSCO_phylogenomics.py -i BUSCO/results -o output_busco_phylogenomics -t 6 --percent_single_copy 70

for file in $(find ./BUSCO/results/ -type f -name "short_summary*"); do cp $file ./output_busco_phylogenomics/summary; done

for file in $(find . -name "full_table*.tsv"); do
grep -v "^#" ${file} | awk '$2=="Complete" {print $1}' >> complete_busco_ids.txt;
done

sort complete_busco_ids.txt |uniq -c > complete_busco_ids_with_counts.txt
awk '$NF > 2 {print $1}' complete_busco_ids_with_counts.txt > final_busco_ids.txt

mkdir -p busco_aa
mkdir -p busco_nt

for dir in $(find . -type d -name "single_copy_busco_sequences"); do
  sppname=$(basename $(dirname $dir)|cut -f 2-3 -d "_" | sed 's/_/ /g');
  abbrv=$(echo $sppname | sed 's/\(\w\w\w\)\w*\( \|$\)/\1/g')
  for file in ${dir}/*.faa; do
    cp $file busco_aa/${abbrv}_${file}
    sed -i 's/^>/>'${abbrv}'|/g' busco_aa/${abbrv}_${file}
	cut -f 1 -d ":" busco_aa/${abbrv}_${file} | tr '[:lower:]' '[:upper:]' > busco_aa/${abbrv}_${file}.1
	mv busco_aa/${abbrv}_${file}.1 busco_aa/${abbrv}_${file}
  done
  for file in ${dir}/*.fna; do
    cp $file busco_nt/${abbrv}_${file}
    sed -i 's/^>/>'${abbrv}'|/g' busco_nt/${abbrv}_${file}
	cut -f 1 -d ":" busco_nt/${abbrv}_${file} | tr '[:lower:]' '[:upper:]' > busco_nt/${abbrv}_${file}.1
	mv busco_nt/${abbrv}_${file}.1 busco_nt/${abbrv}_${file}  done
done


# f2 fails at training stage
# we tryb braker3
#
mamba create -n braker_env \
mamba activate braker_env

mamba install -c bioconda -y \
  braker3 \
  hisat2 \
  samtools \
  augustus \
  perl-app-cpanminus \
  diamond \
  bamtools \
  bedtools \
  blast \
  gffread \ 
  compleasm\
	python=3.8  # GFF3/FASTA extraction
  biopython  --yes  

cpanm Logger::Simple YAML::XS Hash::Merge Parallel::ForkManager File::Which
Set Up Directories:


#Create directories for organization.
mkdir -p hisat2/alignments hisat2/braker

#Build HISAT2 Index for Each Genome:
#Loop through each genome to create a HISAT2 index.
for genome in genome_masked_rmod/*.clean.fna; do
    base=$(basename $genome .clean.fna)
    hisat2-build -p 8 $genome hisat2/index/${base}_index
done

#Align RNA-Seq to Each Genome:
#Loop through each genome and each RNA-Seq sample to align paired-end reads using HISAT2, then convert to sorted BAM with samtools.

# Step 3: Align RNA-Seq to each genome
for genome in genome_masked_rmod/*.clean.fna; do
    genome_base=$(basename $genome .clean.fna)
    for sample in RNA_SEQ/*_1.fastq; do
        sample_base=$(basename $sample _1.fastq)
        hisat2 -p 8 --dta -x hisat2/index/${genome_base}_index \
        -1 RNA_SEQ/${sample_base}_1.fastq \
        -2 RNA_SEQ/${sample_base}_2.fastq | \
        samtools view -@ 8 -bS - | samtools sort -@8 -o hisat2/alignments/${genome_base}_${sample_base}.bam
        samtools index hisat2/alignments/${genome_base}_${sample_base}.bam
    done
done

#Merge BAM Files for Each Genome:
#For each genome, merge BAM files from all related species’ RNA-Seq to create a single BAM file for BRAKER3.

for genome in genome_masked_rmod/*.clean.fna; do
    genome_base=$(basename $genome .clean.fna)
    bam_files=$(ls hisat2/alignments/${genome_base}_*.bam)
    samtools merge -@ 8 -f hisat2/alignments/${genome_base}_merged.bam $bam_files
    samtools index hisat2/alignments/${genome_base}_merged.bam
done


###
for dir in BUSCO/results/*/run_saccharomycetes_odb12/busco_sequences/single_copy_busco_sequences; do
    species=$(basename "$(dirname "$(dirname "$(dirname "$dir")")")")
    cat "$dir"/*.faa > "hisat2/protein_faa/${species}_busco_proteins.faa"
done


##
outdir="hisat2/BUSCO_minus_self"
mkdir -p "$outdir"

shoenii=("Sac-shoeniiCD502_sac12" "Sac-shoeniiCD506_sac12" "Sac-shoenii_sac12")
orthodb="saccharomycetes_protein.fa"  # <-- path to your OrthoDB protein file

for s in hisat2/protein_faa/*.faa; do
    species=${s##*/}; species=${species%_busco_proteins.faa}
    outfile="$outdir/${species}_minus_self_plus_orthodb.faa"
    > "$outfile"

    # Add BUSCO proteins from all other species (with special shoenii handling)
    for f in hisat2/protein_faa/*.faa; do
        other=${f##*/}; other=${other%_busco_proteins.faa}
        [[ $other == $species ]] && continue
        [[ " ${shoenii[*]} " == *" $species "* && " ${shoenii[*]} " == *" $other "* ]] && continue
        awk -v p="$other" '/^>/{sub(/^>/,">" p "|"); print; next} {print}' "$f" >> "$outfile"
    done

    # Append OrthoDB proteins with a fixed prefix
    awk '/^>/{sub(/^>/,">OrthoDB|"); print; next} {print}' "$orthodb" >> "$outfile"

    echo "Created: $outfile"
done

## deduplicates, length-filters with seqkit, and clusters with cd-hit

for f in hisat2/BUSCO_minus_self/*.faa; do
    base=${f##*/}; base=${base%_busco_proteins.faa}
    seqkit rmdup -s "$f" | seqkit seq -m 30  | cd-hit -i /dev/stdin -o "hisat2/protein_faa/${base}_cleaned.faa" -c 0.95 -T 8 -M 16000
done
# Build a master protein set once (OrthoDB + all BUSCOs, deduped & length-filtered)
cat saccharomycetes_protein.fa multi_species_busco_proteins.faa | seqkit rmdup -s | seqkit seq -m 30 > master_pan_proteins.fa

# Input master protein set (already deduped + cleaned)
MASTER="master_pan_proteins.fa"
OUTDIR="per_species_proteins_minus_self"

mkdir -p "$OUTDIR"

# Loop over unique species prefixes in headers

#remove duplicates

for f in hisat2/BUSCO_minus_self/*.faa; do
    base=${f##*/}; base=${base%_busco_proteins.faa}
    tmpfile=$(mktemp)

    seqkit rmdup -s "$f" | seqkit seq -m 30 > "$tmpfile"

    cd-hit -i "$tmpfile" \
           -o "hisat2/protein_faa/${base}_cleaned.faa" \
           -c 0.95 -T 8 -M 16000 \
           > /dev/null 2>&1  # hide cd-hit output

    rm "$tmpfile"
    echo "Cleaned & clustered: ${base}"
done
#Run BRAKER3 for Each Genome:
#Loop through each genome to run BRAKER3 with the merged BAM and OrthoDB proteins, optimized for fungi.

#
git clone https://github.com/gatech-genemark/GeneMark-ETP.git
GENEMARK_PATH=/home/dave/GeneMark-ETP/bin
export PATH="$GENEMARK_PATH:$PATH"

# Step 7: Run BRAKER3 with OrthoDB v12 Saccharomyces proteins
for genome in genome_masked_rmod/*.clean.fna; do
    genome_base=$(basename $genome .clean.fna)
    if [ -f hisat2/alignments/${genome_base}_merged.bam ]; then
        # RNA-Seq + Saccharomyces proteins mode
        braker.pl --genome=$genome \
        --bam=hisat2/alignments/${genome_base}_merged.bam \
        --prot_seq=odb12_saccharomyces_proteins.fasta \
        --fungus --threads=8 --busco_lineage=fungi_odb10 \
        --makehub --workingdir=hisat2/braker/${genome_base}
    else
        # Saccharomyces proteins-only mode
        braker.pl --genome=$genome \
        --prot_seq=odb12_saccharomyces_proteins.fasta \
        --fungus --threads=8 --busco_lineage=fungi_odb10 \
        --makehub --workingdir=hisat2/braker/${genome_base}
    fi
done


 
 ##
 for g in genomes/*.masked.fa; do \
  base=$(basename "$g" .masked.fa); \
  p=proteins/${base}.faa; b=bams/${base}.bam; out=results/${base}_braker; mkdir -p "$out"; \
  if [[ -f "$b" ]]; then \
    braker.pl --genome="$g" --bam="$b" --prot_seq="$p" --softmasking --etpmode --threads $THREADS --species="$base" --workingdir="$out"; \
  else \
    braker.pl --genome="$g" --prot_seq="$p" --softmasking --epmode --threads $THREADS --species="$base" --workingdir="$out"; \
  fi; \
  tsebra.py -g "$out/augustus.hints.gff3" "$out/genemark.gtf" -e "$out/hintsfile.gff" -o "$out/${base}_tsebra_final.gff3"; \
done
 
 #braker.pl --genome=genome_masked_rmod/Saccharomycopsis_crataegensisv1_genomic.clean.fna --bam=hisat2/allignment/Saccharomycopsis_crataegensis_DRR212833.bam --prot_seq=hisat2/protein_faa/Sac-crataegensisv1_sac12_minus_self_plus_orthodb.faa_cleaned.faa --fungus --threads=8 --busco_lineage=saccharomycetes_odb12 --makehub --email=bluelaundrymeru@gmail.com --workingdir=hisat2/braker/Saccharomycopsis_crataegensis

  

mamba create -n repeatmask  repeatmodeler repeatmasker genometools-genometools ltr_retriever tr
mamba activate repeatmask

BuildDatabase -name yeastDB genome.fasta

for f in ncbi_blast_genomes/*.fna; do
  bn=$(basename "$f" .fna)
  echo ">>> Processing $bn"

  # 1. Build RepeatModeler DB and run
  BuildDatabase -name te_sac/index/${bn}_db "$f"
  RepeatModeler -database te_sac/index/${bn}_db -pa 8 -LTRStruct

  # 2. Genome index for LTRharvest
  gt suffixerator -db "$f" -indexname te_sac/index/${bn} \
    -tis -suf -lcp -des -ssp -sds -dna

  # 3. Run LTRharvest
  gt ltrharvest -index te_sac/index/${bn} \
    -minlenltr 100 -maxlenltr 5000 \
    -mintsd 4 -maxtsd 6 -similar 80 -vic 10 -seqids yes \
    > te_sac/index/${bn}.ltrharvest.out

  # 4. LTR_retriever refinement
  LTR_retriever -genome "$f" -inharvest te_sac/index/${bn}.ltrharvest.out

  # 5. Merge libraries
  cat te_sac/index/${bn}_db*/consensi.fa.classified ${bn}.LTRlib.fa \
    > te_sac/index/${bn}.combined.fa

  # 6. Final masking
  RepeatMasker -pa 8 -gff -xsmall \
    -lib te_sac/index/${bn}.combined.fa "$f"

  echo ">>> Done with $bn"
done

