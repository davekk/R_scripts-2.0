require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(magrittr) # pipe and related stuff
library(data.table)
require(readxl)
library(purrr)
library(writexl)

rm(list=ls())
setwd('D:/Geisenheim_PhD/microscopy_analysis/raw data ste/')

## ste/ camp
tbl <-
  list.files(pattern = "\\.xlsx$") %>% 
  purrr::map_df(~read_xlsx(.))


#tbl <-read_xlsx('run 6 3 hr raw ste.xlsx')

tbl  %<>% replace(is.na(.), 0) %>%  # remove all NAs
  mutate(Sample = as.factor(Sample), # factorize shit
         Treatment = as.factor(Treatment)) %>%
          rename( # remove the anoying names in colnames
            Cells_with_pegs = `Cells with pegs`,
            Total_cells = `Total cells`,
            Rounded_cells = `Rounded cells`
          ) %>% # make all trt to be standadized
  mutate(
    Treatment = case_when(
      grepl("alpha", Treatment, ignore.case = TRUE) ~ "alpha",
      TRUE ~ as.character(Treatment)
    ),
    # keep Sample factor order as in original data
    Sample = factor(Sample, levels = unique(Sample)),
    # set Treatment order: none first, then alpha, then others
    Treatment = factor(Treatment, levels = c("None", "alpha", setdiff(unique(Treatment), c("None","alpha"))))
  ) %>%
  filter(!(Sample != "WT" & Spore > 0)) # drop contanimnated samples

tbl_ypd <- tbl %>%
  filter(!grepl("csm", `Image Folder`, ignore.case = TRUE))
tbl_csm <- tbl %>%
  filter(grepl("csm", `Image Folder`, ignore.case = TRUE))


# ---- 1. Statistical analysis ----
analyze_response <- function(data, response_var, wt_name = "WT") {
  response_var <- trimws(response_var)   # remove accidental spaces
  
  if (!(response_var %in% names(data))) {
    stop(paste("Column", response_var, "not found. Available:",
               paste(names(data), collapse = ", ")))
  }
  if (!(wt_name %in% levels(data$Sample))) {
    stop(paste("Reference group", wt_name, "not found in Sample levels:",
               paste(levels(data$Sample), collapse = ", ")))
  }
  
  test_res <- data %>%
    group_by(Treatment) %>%
    pairwise_t_test(
      formula = as.formula(sprintf("`%s` ~ Sample", response_var)),
      ref.group = wt_name,
      p.adjust.method = "bonferroni"
    )
  
  y_max <- max(data[[response_var]], na.rm = TRUE)
  test_res <- test_res %>%
    mutate(
      y.position = seq(from = y_max * 1.05,
                       by = y_max * 0.05,
                       length.out = n()),
      # keep only stars
      p.label = p.adj.signif
    )
  
  return(test_res)
}


# ---- 2. Plotting ----
plot_response <- function(data, response_var, stats_res, y_lab = response_var) {
  # force WT first, others follow in their original order
  sample_levels <- c("WT", "△ste2", "△ste3", "△ste2/△ste3")  
  
  #   Facet labels mapping
  facet_labels <- c("None" = "None", "alpha" = "α")
 
   data <- data %>%
    mutate(
      Sample = factor(Sample, levels = sample_levels),
      Treatment = factor(Treatment, levels = names(facet_labels))
    )
  
  p <- ggplot(data, aes(x = Sample, y = !!sym(response_var), color = Sample)) +
   labs(title = paste("WT vs others by Treatment:", response_var),
         x = "Sample",  y = y_lab  ) +# <-- customizable y-axis label
    geom_violin(trim = FALSE, aes(fill = Sample), alpha = 0.6, color = NA)+
    geom_jitter(aes(color = Sample), width = 0.15, size = 2, alpha = 0.7, show.legend = FALSE)+
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8, color = "black") +
    stat_pvalue_manual(stats_res, label = "p.adj.signif", tip.length = 0.01) +
#  facet_wrap(~Treatment, scales = "free_x") +
    facet_wrap(~Treatment, scales = "free_x", labeller = labeller(Treatment = facet_labels)) +
        theme_classic(base_size = 16) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +  
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    strip.text = element_text(size = 14, face = "bold"),
    strip.background = element_blank(),  # remove the default box
    legend.position = "none"
  )  + geom_hline(
    yintercept = -0.5,     # adjust slightly below facet label (tweak as needed)
    color = "black",
    linewidth = 0.4
  )
  return(p)
}

# ---- 3. Wrapper ----

make_analysis <- function(data, response_var, wt_name = "WT", y_lab = NULL) {
  response_var <- trimws(response_var)
  
  stats_res <- analyze_response(data, response_var, wt_name = wt_name)
  print(stats_res)   # show test table
  
  if (is.null(y_lab)) {
    y_lab <- response_var   # default if not provided
  }
  
  p <- plot_response(data, response_var, stats_res, y_lab = y_lab)
  print(p)   # plot
  
  return(list(results = stats_res, plot = p))
}


spore_out <- make_analysis(tbl, "Spore", wt_name = "WT")
pegs_out  <- make_analysis(tbl, "Cells_with_pegs", wt_name = "WT")
shmoo_out <- make_analysis(tbl, "Shmoo", wt_name = "WT")

spore_csm_out <- make_analysis(tbl_csm, "Spore", wt_name = "WT",y_lab = "Spore count")
pegs_csm_out  <- make_analysis(tbl_csm, "Cells_with_pegs", wt_name = "WT", y_lab = "Cells with Pegs")
shmoo_csm_out <- make_analysis(tbl_csm, "Shmoo", wt_name = "WT",y_lab = "MCBE")

spore_ypd_out <- make_analysis(tbl_ypd, "Spore", wt_name = "WT")
pegs_ypd_out  <- make_analysis(tbl_ypd, "Cells_with_pegs", wt_name = "WT")
shmoo_ypd_out <- make_analysis(tbl_ypd, "Shmoo", wt_name = "WT")



