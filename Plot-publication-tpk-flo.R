require(tidyr); # data frame manipulation
require(dplyr); # data frame manipulation
require(magrittr) # pipe and related stuff
library(data.table)
require(readxl)
library(purrr)
library(rstatix)
library(ggpubr)

rm(list=ls())
setwd('D:/Geisenheim_PhD/microscopy_analysis/raw data camp/')

## flo8/ camp
tbl <-
  list.files(pattern = "\\.xlsx$") %>% 
  purrr::map_df(~read_xlsx(.)) %>%
  mutate(Treatment = coalesce(Treatment, "None"))

#
  tpk <- 
 #  sok <- 
 #flo <- 
   tbl  %>% 
  mutate(across(everything(), ~replace_na(., 0))) %>% # remove all NAs
  mutate(Name = stringr::str_replace(Name, "^2X-", "")) %>% # remove "2X-" at start
    mutate(Name = case_when( Name %in% c("G336", "G336_2") ~ "G336", TRUE ~ Name )) %>%
   mutate(Sample = if_else(stringr::str_detect(Sample, "WT"), "WT", Sample),
         Sample = factor(Sample, levels = unique(Sample)),
        Name = factor(Name, levels = unique(Name)),
         
         # Standardize Treatment
         Treatment = case_when(
           grepl("cAMP", Treatment, ignore.case = TRUE) ~ "cAMP",
           TRUE ~ as.character(Treatment)    ),
         Treatment = factor(Treatment, levels = c("None", "cAMP", 
                                                  setdiff(unique(Treatment), c("None", "cAMP"))))) %>%
  
  mutate(`Cells with pegs` = coalesce(`Cells with pegs`, 0) +
           coalesce(`cells with pegs`, 0)) %>%
  select(-`cells with pegs`) %>%
    rename( # remove the anoying names in colnames
      MCBE = Shmoo ) %>% # make all trt to be standadized
  # Drop unwanted treatments
   
   # filter(!stringr::str_detect(Treatment, stringr::regex("Alpha|cAMP", ignore_case = TRUE))) %>%
    filter(!grepl("Alpha", Treatment, ignore.case = TRUE))%>%
  # Keep only Samples that have both None and cAMP
  group_by(Sample) %>%
# filter(stringr::str_detect(Sample, "WT") | stringr::str_detect(Sample, "flo")) 
# filter(stringr::str_detect(Sample, "WT") | stringr::str_detect(Sample, "sok")) 
 filter(stringr::str_detect(Sample, "WT") | stringr::str_detect(Sample, "tpk")) 

# split starved and non-starved samples  
tbl_ypd <- sok %>%
  filter(!grepl("csm", `Image Folder`, ignore.case = TRUE))
  
tbl_csm <- sok %>%
  filter(grepl("csm", `Image Folder`, ignore.case = TRUE))


# ---- 1. Statistical analysis ----
analyze_response <- function(data, response_var, wt_name ) {
  response_var <- trimws(response_var)   # remove accidental spaces
  
  if (!(response_var %in% names(data))) {
    stop(paste("Column", response_var, "not found. Available:",
               paste(names(data), collapse = ", ")))
  }
  if (!(wt_name %in% levels(data$Name))) {
    stop(paste("Reference group", wt_name, "not found in Name levels:",
               paste(levels(data$Name), collapse = ", ")))
  }
  
  test_res <- data %>%
    group_by(Treatment) %>%
    pairwise_t_test(
      formula = as.formula(sprintf("`%s` ~ Name", response_var)),
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
 # sample_levels <- c("WT", "△tpk1", "△tpk2", "△tpk1/△tpk2","△sok2","△flo8") #  
  name_levels <- c("B007", sort(setdiff(unique(data$Name), "B007")))
  #name_levels <- c("B007", "G413", "G415") 
   #Facet labels mapping 
  facet_labels <- c("None" = "None", "cAMP" = "cAMP")
  
  data <- data %>%
    mutate(
      Name = factor(Name, levels = name_levels),
      Treatment = factor(Treatment, levels = names(facet_labels))
    )
  
  p <- ggplot(data, aes(x = Name, y = !!sym(response_var), color = Sample)) +
    labs(title =paste("WT vs others by Treatment:", response_var),
         x = "Name",y = y_lab  ) +# <-- customizable y-axis label
    
    geom_violin(trim = FALSE, aes(fill = Sample), alpha = 0.6, color = NA)+
    geom_jitter(aes(color = Sample), width = 0.15, size = 2, alpha = 0.7, show.legend = FALSE)+
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8, color = "black") +
    stat_pvalue_manual(stats_res, label = "p.adj.signif", tip.length = 0.01) +
  #  facet_wrap(~Sample, scales = "free_x",labeller = labeller(Treatment = facet_labels)) +
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

make_analysis <- function(data, response_var, wt_name, y_lab = NULL) {
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

# ~ sample
spore_out <- make_analysis(sok, "Spore", wt_name = "B007")
pegs_out  <- make_analysis(sok, "Cells with pegs", wt_name = "B007")
#spore_out <- 
pegs_out  <- 
#shmoo_out <-
  tbl_csm %>% 
  filter(Name %in% c("B007", "G312", "G313"))%>%
  #filter(Name %in% c("B007", "G296", "G297", "G298", "G299", "G519","G520"))%>%
#make_analysis( "Spore", wt_name = "B007")
make_analysis( "Cells with pegs", wt_name = "B007")
 make_analysis( "MCBE", wt_name = "B007")

spore_csm_out <- make_analysis(tbl_csm, "Spore", wt_name = "B007",y_lab = "Spore count")
pegs_csm_out  <- make_analysis(tbl_csm, "Cells with pegs", wt_name = "B007")  
#spore_csm_out <- 
pegs_csm_out  <-   
#shmoo_csm_out <-   
  tbl_csm %>% filter(Name %in% c("B007", "G312", "G313"))%>%
  make_analysis("Spore", wt_name = "B007",y_lab = "Spore ")
make_analysis("Cells with pegs", wt_name = "B007")
make_analysis( "MCBE", wt_name = "B007")

spore_ypd_out <- make_analysis(tbl_ypd, "Spore", wt_name = "B007")
pegs_ypd_out  <- make_analysis(tbl_ypd, "Cells with pegs", wt_name = "B007")
shmoo_ypd_out <- make_analysis(tbl_ypd, "MCBE", wt_name = "B007")

# ~ treatment
# ---- 1. Statistical analysis: treatment effect within each sample ----
analyze_treatment_effect <- function(data, response_var, sample_levels = NULL) {
  response_var <- trimws(response_var)
  
  if (!response_var %in% names(data)) stop("Column not found: ", response_var)
  
  if (!is.null(sample_levels)) {
    data <- data %>% mutate(Sample = factor(Sample, levels = sample_levels))
  }
  
  # pairwise t-test between treatments within each Sample
  test_res <- data %>%
    group_by(Sample) %>%
    pairwise_t_test(
      formula = as.formula(sprintf("`%s` ~ Treatment", response_var)),
      p.adjust.method = "bonferroni"
    ) %>%
    mutate(
      y.position = seq(from = max(data[[response_var]], na.rm = TRUE)*1.05,
                       by = max(data[[response_var]], na.rm = TRUE)*0.05,
                       length.out = n()),
      p.label = p.adj.signif
    )
  return(test_res)
}

# ---- 2. Plotting treatment effect ----
plot_treatment_effect <- function(data, response_var, stats_res, y_lab = response_var) {
  sample_levels <- c("WT", "△tpk1", "△tpk2","△tpk1/△tpk2","△sok2","△flo8")
  data <- data %>% mutate(Sample = factor(Sample, levels = sample_levels))
  
  facet_labels <- c("None" = "None", "cAMP" = "cAMP")
  
  p <- ggplot(data, aes(x = Treatment, y = !!sym(response_var), color = Treatment)) +
    labs(title = paste("Treatment effect per Sample:", response_var),
         x = "Treatment", y = y_lab) +
    geom_violin(trim = FALSE, aes(fill = Treatment), alpha = 0.6, color = NA) +
    geom_jitter(width = 0.15, size = 2, alpha = 0.7, show.legend = FALSE) +
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8, color = "black") +
    stat_pvalue_manual(stats_res, label = "p.adj.signif", tip.length = 0.01) +
    facet_wrap(~Sample, scales = "free_x") +
    theme_classic(base_size = 16) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.position = "none"
    )
  return(p)
}

# ---- 3. Wrapper for treatment effect ----
make_treatment_effect_analysis <- function(data, response_var, y_lab = NULL) {
  stats_res <- analyze_treatment_effect(data, response_var)
  print(stats_res)
  
  if (is.null(y_lab)) y_lab <- response_var
  
  p <- plot_treatment_effect(data, response_var, stats_res, y_lab = y_lab)
  print(p)
  
  return(list(results = stats_res, plot = p))
}

# ---- 4. Run for 3 measures ----

spore_out2 <- make_treatment_effect_analysis(tbl, "Spore")
pegs_out2  <- make_treatment_effect_analysis(tbl, "Cells_with_pegs",y_lab = "Cells with Pegs")
shmoo_out2 <- make_treatment_effect_analysis(tbl, "Shmoo",y_lab = "MCBE")

spore_csm_out2 <- make_treatment_effect_analysis(tbl_csm, "Spore", y_lab = "Spore count")
pegs_csm_out2  <- make_treatment_effect_analysis(tbl_csm, "Cells_with_pegs",y_lab = "Cells with Pegs")
shmoo_csm_out2 <- make_treatment_effect_analysis(tbl_csm, "Shmoo", y_lab = "MCBE")

spore_ypd_out2 <- make_treatment_effect_analysis(tbl_ypd, "Spore")
pegs_ypd_out2  <- make_treatment_effect_analysis(tbl_ypd, "Cells_with_pegs", y_lab = "Cells with Pegs")
shmoo_ypd_out2 <- make_treatment_effect_analysis(tbl_ypd, "Shmoo", y_lab = "MCBE")

