# Install required packages if not already installed
#install.packages(c("ape", "phangorn"))
#BiocManager::install("ggtree")

library(ape)
library(phangorn)
library(ggtree)
library(ggplot2)
#
rm(list=ls())
#setwd('D:/sacc_genomes/busco_phylo/concat/')
setwd('D:/sacc_genomes/ste3_alpha/')
# Read the ML tree and consensus tree
bs_tree <- read.tree("Alpa_sacc.clipkit.fa.contree" )  # contains bootstrap values

outgroup <- c("Ascoidea_tarda", "Ascoidea_rubescens_DSM_1968")

# Root on outgroups
rooted <- root(bs_tree, outgroup = outgroup, resolve.root = TRUE)


#
rooted <- ladderize(rooted)
# --- Standardize node labels to 0–100 ---
clean_labels <- function(lbl) {
  if (is.na(lbl) || lbl == "" || lbl == "Root") return("")   # blank for root or NA
  if (grepl("/", lbl)) {
    parts <- strsplit(lbl, "/")[[1]]
    uf <- suppressWarnings(as.numeric(parts[2]))  # take UFBoot (2nd number)
    if (!is.na(uf)) return(as.character(round(uf))) else return("")
  } else {
    num <- suppressWarnings(as.numeric(lbl))
    if (!is.na(num)) return(as.character(round(num))) else return("")
  }
}
# share same mature peptide
subgroups <- data.frame(
  tip = c("S. synnaedendra NRRL Y-7466", "S. microspora NRRL Y-7404",   # big group A
          "S. fermentans NRRL Y-17710", "S. schoenii CD506","S. schoenii NRRL Y-17595",
          "S. javanensis NRRL Y-1483", "S. babjevae CLIB 1639", "S. oosterbeekiorum CBS 14943",               # big group B
          "S. crataegensis NRRL Y-5902", "S. amapae NRRL Y-17845", "S. fibuligera MUCL 14481"),               # 3 individuals
  group = c(rep("GroupA", 2), 
            rep("GroupB", 6), 
            "Outlier1", "Outlier2", "Outlier3"))

# Convert to factor with controlled order
subgroups$group <- factor(
  subgroups$group,
  levels = c("GroupA", "GroupB", "Outlier1", "Outlier2", "Outlier3"))
rooted$node.label <- vapply(rooted$node.label, clean_labels, character(1))
# --- Clean tip labels (replace "_" with " ") ---
rooted$tip.label <- gsub("_", " ", rooted$tip.label)

ggtree(rooted) %<+% subgroups +
    geom_tiplab(aes(color = group, fontface = ifelse(!is.na(group), "bold", "plain")),
                     size = 3, hjust = 0, align = TRUE, linetype = "dotted", offset = 0.5) +
  geom_label2(aes(subset = !isTip & node != getRoot(rooted), label = label),
                size = 2.5, fill = "white", label.size = 0.2, hjust = 1.1) +
    theme_tree2() +
  xlim_tree(1.5 * max(phytools::nodeHeights(rooted))) +  # plenty of extra margin
    #ggtitle("Maximum Likelihood tree (IQ-TREE, SH-aLRT / UFBoot)") +
    theme(plot.title = element_text(hjust = 0.5, size = 14),legend.position = "none",
          axis.line.x  = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x  = element_blank())  +
   scale_color_manual(values = c("GroupA"   = "#D81B60",  # strong magenta/red
                                "GroupB"   = "#1E88E5",  # strong blue
                                "Outlier1" = "#66CDAA",  # light green
                                "Outlier2" = "#4CAF50",  # green
                                "Outlier3" = "#87CEEB"  )) +# light blue
  
    # add scale bar (branch length key)
  geom_treescale(x = 0, y = -1, offset = 0.5, fontsize = 3)
  ##
  





