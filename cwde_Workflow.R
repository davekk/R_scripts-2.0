# ========================================================
# CWDE Annotation and Visualization Pipeline (Optimized)
# Verified R packages: rhmmer, Biostrings, rtracklayer, readr
# ========================================================
# ===============================
# 1. Package Setup
# ===============================
library(dplyr)
library(tidyr)
library(stringr)
library(magrittr)
library(ggplot2)
library(reshape2)

# ===============================
# 2. Environment Setup
# ===============================
rm(list = ls())
setwd('D:/sacc_genomes/cwge/')

# ===============================
# 3. Load HMMER outputs
# ===============================
load_hmmer <- function(file) {
  rhmmer::read_domtblout(file) %>%
    select(query_name, domain_name, domain_accession,
           ali_from, ali_to, env_from, env_to,
           domain_ievalue, domain_score, acc, description)
}

merops_dom <- load_hmmer("Merops/B007/merops_scan.domtblout")
pfam_dom   <- load_hmmer("Merops/B007/pfam_scan.domtblout")

# ===============================
# 4. Load CAZyme outputs
# ===============================
cazy_pep      <- Biostrings::readAAStringSet("CAZyme_out/B007/CAZyme.pep")
cazy_diamond  <- readr::read_tsv("CAZyme_out/B007/diamond.out")
cazy_h        <- load_hmmer("CAZyme_out/B007/h.out")
cazy_overview <- readr::read_tsv("CAZyme_out/B007/overview.txt")
cazy_signalp <- readr::read_table2( "CAZyme_out/B007/signalp.out",
  col_names = c("query_name", "SP1", "len1", "SP2", "len2", "SP3", "len3",
    "SP4", "len4", "SP5", "Y_flag", "threshold"))

# ===============================
# 5. Load InterProScan output
# ===============================
interpro <- readr::read_tsv("Merops/b7interproscan_out", col_names = c(
  "protein_id","md5","length","analysis","signature_acc","signature_desc",
  "start","end","score","status","date","interpro_acc","interpro_desc",
  "GO_terms","pathways"))

# ===============================
# 6. Load SignalP output
# ===============================
signalp_gff  <- rtracklayer::import("signalp/B007/output.gff3")
signalp_json <- jsonlite::fromJSON("signalp/B007/output.json", flatten = TRUE)

signalp_df <- lapply(signalp_json$SEQUENCES, function(x) {
  data.frame(
    query_name     = if(!is.null(x$Name)) x$Name else NA_character_,
    signal_peptide = if(!is.null(x$Prediction)) x$Prediction else NA_character_,
    cleavage_site  = if(!is.null(x$CS_pos) && x$CS_pos != "") x$CS_pos else NA_character_,
    protein_types  = if(!is.null(x$Protein_types)) paste(x$Protein_types, collapse = ";") else NA_character_,
    stringsAsFactors = FALSE
  )
}) %>% bind_rows()

signalp_mature <- Biostrings::readAAStringSet("signalp/B007/output_mature.fasta")

# ===============================
# 7. Load Pannzer2 annotations
# ===============================
pannzer_anno <- readr::read_tsv("pannzer2_out/B007/anno.out")
names(pannzer_anno)[1] <- "query_name"
pannzer_de   <- readr::read_tsv("pannzer2_out/B007/DE.out")
pannzer_go   <- readr::read_tsv("pannzer2_out/B007/GO.out")

# ===============================
# 8. Load DeepTMHMM predictions
# ===============================
lines       <- readLines("Merops/B007/deeptmhmm.txt")
meta_lines  <- lines[grepl("^#", lines)]
pred_lines  <- lines[!grepl("^#", lines)]

meta_summary <- data.frame(raw = meta_lines) %>%
  mutate(protein = str_remove(str_extract(raw, "^#\\s*\\S+"), "^#\\s*"),
         key_val = str_trim(str_remove(raw, "^#\\s*\\S+\\s*"))) %>%
  separate(key_val, into = c("key","value"), sep = ":", fill = "right") %>%
  mutate(key = str_trim(key), value = str_trim(value)) %>%
  filter(!is.na(value) & value != "") %>%
  pivot_wider(id_cols = protein, names_from = key, values_from = value)

tmh_count <- read.table(text = pred_lines, header = FALSE, stringsAsFactors = FALSE) %>%
  setNames(c("protein", "method", "topology", "start", "end")) %>%
  mutate(start = as.integer(start), end = as.integer(end)) %>%
  filter(topology == "TMhelix") %>%
  group_by(protein) %>%
  summarise(TMH_count = n(), TMH_length = sum(end - start + 1), .groups = "drop")

# ===============================
# 9. Process Domains and Annotations
# ===============================
merops_best <- merops_dom %>%
  group_by(query_name) %>%
  slice_min(domain_ievalue, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(query_name, merops_family = domain_name, merops_acc = domain_accession, domain_ievalue)

pfam_summary  <- pfam_dom %>%
  group_by(query_name) %>%
  summarise(pfam_domains = paste(unique(domain_name), collapse = ";"), .groups = "drop")
cazy_summary  <- cazy_h %>%
  group_by(query_name) %>%
  summarise(cazy_families = paste(unique(domain_name), collapse = ";"), .groups = "drop")
pannzer_summary <- pannzer_anno %>%
  group_by(query_name) %>%
  summarise(description = paste(unique(desc), collapse = "; "), .groups = "drop")

# CAZyme SignalP predictions
cazy_signalp_flag <- cazy_signalp %>%
  select(query_name, threshold) %>%
  mutate(signalp_secreted_cazy = grepl("SignalP", threshold))

all_proteins <- unique(c(merops_dom$query_name, pfam_dom$query_name, cazy_h$query_name, pannzer_anno$query_name))
all_proteins_df <- data.frame(query_name = all_proteins, stringsAsFactors = FALSE)

signalp_df <- data.frame(query_name = all_proteins) %>%
  left_join(data.frame(query_name = names(signalp_mature), signalp_secreted = TRUE), by = "query_name") %>%
  mutate(signalp_secreted = ifelse(is.na(signalp_secreted), FALSE, signalp_secreted))

# ===============================
# 10. Merge Master Annotation
# ===============================
master_anno <- 
  all_proteins_df %>%
  left_join(signalp_df, by = "query_name") %>%
  left_join(cazy_signalp_flag, by = "query_name") %>%
  mutate(signalp_secreted_final = ifelse(is.na(signalp_secreted_cazy), 
                                         signalp_secreted, signalp_secreted_cazy))%>%
  left_join(merops_best, by = "query_name") %>%
  left_join(pfam_summary, by = "query_name") %>%
  left_join(cazy_summary, by = "query_name") %>%
  left_join(pannzer_summary, by = "query_name") %>%
  left_join(meta_summary, by = c("query_name" = "protein")) %>%
  left_join(tmh_count, by = c("query_name" = "protein")) %>%
  mutate(
    TMH_count = ifelse(is.na(TMH_count), 0L, TMH_count),
    cwde_candidate = (!is.na(cazy_families) | !is.na(merops_family)) & signalp_secreted_final,
    has_merops     = !is.na(merops_family),
    has_cazy       = !is.na(cazy_families),
    secreted_cwde  = cwde_candidate & TMH_count == 0L,
    enzyme_type    = dplyr::case_when(
      has_merops & has_cazy ~ "MEROPS+CAZyme",
      has_merops & !has_cazy ~ "MEROPS_only",
      !has_merops & has_cazy ~ "CAZyme_only",
      TRUE ~ "none")  )


# ===============================
# 11. CWDE Summary Statistics
# ===============================
cwde_summary <- master_anno %>%
  group_by(enzyme_type) %>%
  summarise(n = n(),
            secreted = sum(signalp_secreted),
            .groups = "drop")

# Hypergeometric test
N_total    <- nrow(master_anno)
K_cwde     <- sum(master_anno$cwde_candidate)
M_secreted <- sum(master_anno$signalp_secreted)
x_overlap  <- sum(master_anno$cwde_candidate & master_anno$signalp_secreted)
pval <- phyper(x_overlap - 1, M_secreted, N_total - M_secreted, K_cwde, lower.tail = FALSE)

# ===============================
# 12. Visualization Functions
# ===============================
# CWDE counts
ggplot(cwde_summary, aes(x = enzyme_type, y = n, fill = enzyme_type)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = n), vjust = -0.5) +
  scale_fill_manual(values = c("MEROPS_only"="orange","CAZyme_only"="green","MEROPS+CAZyme"="purple","none"="grey")) +
  labs(title="CWDE Distribution by Type", x="Enzyme Type", y="Number of Proteins") +
  theme_minimal(base_size = 14)

# Fraction secreted
ggplot(cwde_summary, aes(x = enzyme_type, y = secreted / n, fill = enzyme_type)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title="Fraction of Secreted CWDE Proteins by Type", x="Enzyme Type", y="Fraction Secreted") +
  theme_minimal(base_size = 14)
# ===============================
# 13. UpSet Plot for sets
# ===============================
upset_df <- master_anno %>%
  select(query_name, has_merops, has_cazy, signalp_secreted_final)

ComplexUpset::upset(upset_df, 
     c("has_merops", "has_cazy", "signalp_secreted_final"),
      width_ratio = 0.2,
      min_size = 1,
      stripes = 'white',
      name = 'Protein Sets') +
  theme_minimal(base_size = 14)

# ===============================
# 14. Heatmaps for top families
# ===============================
top_merops <- master_anno %>%
  filter(secreted_cwde & !is.na(merops_family)) %>%
  count(merops_family, sort = TRUE) %>% slice_head(n = 10) %>% pull(merops_family)

top_cazy <- master_anno %>%
  filter(secreted_cwde & !is.na(cazy_families)) %>%
  separate_rows(cazy_families, sep = ";") %>%
  count(cazy_families, sort = TRUE) %>% slice_head(n = 10) %>% pull(cazy_families)

# MEROPS matrix
merops_matrix <- master_anno %>%
  filter(secreted_cwde & merops_family %in% top_merops) %>%
  select(query_name, merops_family) %>% mutate(present = 1) %>% distinct() %>%
  dcast(query_name ~ merops_family, value.var = "present", fill = 0)

# CAZyme matrix
cazy_matrix <- master_anno %>%
  filter(secreted_cwde & !is.na(cazy_families)) %>%
  select(query_name, cazy_families) %>%
  separate_rows(cazy_families, sep = ";") %>%
  filter(cazy_families %in% top_cazy) %>% mutate(present = 1) %>% distinct() %>%
  dcast(query_name ~ cazy_families, value.var = "present", fill = 0)

# Combined matrix
combined_matrix <- merge(merops_matrix, cazy_matrix, by = "query_name", all = TRUE)
combined_matrix[is.na(combined_matrix)] <- 0
combined_mat <- as.matrix(combined_matrix[,-1])
rownames(combined_mat) <- combined_matrix$query_name

pheatmap::pheatmap(combined_mat, color = c("white", "darkgreen"),
         cluster_rows = TRUE, cluster_cols = TRUE,
         fontsize_row = 7, fontsize_col = 9,
         main = "Top MEROPS + CAZyme Families (Secreted CWDEs)")

# UpSet for top families
upset_matrix <- combined_mat %>% as.data.frame()
upset_matrix$query_name <- rownames(upset_matrix)

ComplexUpset::upset(upset_matrix,
                    colnames(upset_matrix)[-ncol(upset_matrix)],
                    width_ratio = 0.2,
                    min_size = 1,
                    stripes = 'white',
                    name = 'Top Families') +
  theme_minimal(base_size = 12)
