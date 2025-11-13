library(ape)
library(phangorn)
library(ggtree)
library(ggplot2)
#
rm(list=ls())

setwd('D:/sacc_genomes/busco_phylo/concat/')

outgroup <- c("Asc-rubescens", "Asc-tarda")

# Read maximum likelihood tree
#ml_tree <- read.tree("final_tree.treefile")

# Read consensus tree
cons_tree <- read.tree("final_tree.contree")

# Root on outgroups
rooted <- root(cons_tree, outgroup = outgroup, resolve.root = TRUE)
#rooted <- root(ml_tree, outgroup = outgroup, resolve.root = TRUE)
# ml_tree$tip.label[29] <- "Sac-NRRL_Y-5750"
 cons_tree$tip.label[29] <- "Sac-NRRL_Y-5750"

# laderize it 
rooted <- ladderize(rooted)

readxl::read_excel("renamed.xlsx") -> name_tree
rooted$tip.label <- name_tree$Value

# --- Clean tip labels (replace "_" with " ") ---
rooted$tip.label <- gsub("Sac-", "S. ", rooted$tip.label)
rooted$tip.label <- gsub("Asc-", "Ascoidea ", rooted$tip.label)


ggtree(rooted) +
    # Tip labels with dotted alignment
    geom_tiplab(aes(label = label),size = 3, hjust = 0, align = TRUE, linetype = "dotted", 
      offset = 0.5, fontface = "italic") +
  
  # normal node labels (non-root)
  geom_text2(aes(subset = !isTip & node != getRoot(rooted), label = label),size = 2.5, hjust=1, nudge_y = 0.5) +
  # Bootstrap values with repel (skip root)
  
    # Layout & theme
    theme_tree2() +
    xlim_tree(2.1 * max(phytools::nodeHeights(rooted))) +
    theme(plot.title = element_text(hjust = 0.5, size = 14),legend.position = "none",
      axis.line.x = element_blank(),
      axis.ticks.x = element_blank(),
      axis.text.x = element_blank()) +
    
    # Scale bar
    geom_treescale(x = 3, y = -1, offset = 0.5, fontsize = 3)
