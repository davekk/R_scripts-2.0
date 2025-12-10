require(dplyr); # data frame manipulation
require(magrittr) # pipe and related stuff
require(readxl)
library(rstatix)
library(ggpubr)
library(purrr)

rm(list=ls())
setwd('D:/Geisenheim_PhD/microscopy_analysis/raw_sacc/')

# read into r the full folder
tbl <- list.files(pattern = "\\.xlsx$") %>% 
  map_df(read_xlsx) %>% 
  filter(    Name %in% c("B001", "B007", "G020", "G063", "G064"),
    Treatment %in% c("None", "Alpha", "Aso-Alpha")) %>% 
    select(-Sample) %>% 
  replace(is.na(.), 0) %>% 
  mutate(    Name      = recode(Name, "G063" = "CD502", "G064" = "CD506") %>% as.factor(),
    Treatment = recode(Treatment, "Aso-Alpha" = "Alpha") %>% as.factor()) %>% 
    rename(    Cells_with_pegs = `Cells with pegs`,
    Total_cells     = `Total cells`)

##
# ---- 1. Statistical analysis ----
analyze_treatment_effect <- function(data, response_var) {
  response_var <- trimws(response_var)
  
  if (!(response_var %in% names(data))) {
    stop(paste("Column", response_var, "not found. Available:",
               paste(names(data), collapse = ", ")))
  }
  
  test_res <- data %>%
    group_by(Name) %>%
    rstatix::t_test(as.formula(sprintf("`%s` ~ Treatment", response_var))) %>%
    adjust_pvalue(method = "bonferroni") %>%
    rstatix::add_significance("p.adj") %>%
    mutate(
      group1 = "Control",
      group2 = "α",
      y.position = sapply(Name, function(nm) {
        max(data[[response_var]][data$Name == nm], na.rm = TRUE) * 1.1
      }),
      p.label = p.adj.signif
    ) %>%
    ungroup()
  
  return(test_res)
}

# ---- 2. Plotting ----
plot_treatment_effect <- function(data, response_var, stats_res, y_lab = response_var) {
  
  # fallback: make pretty label if none given
  if (is.null(y_lab)) {
    y_lab <- str_to_title(gsub("_", " ", response_var))
  }
  
  data <- data %>%
  
    mutate( Name = factor(Name, levels = c("B007", "G020", "CD502", "CD506", "B001")),
            Treatment = recode(Treatment, "None" = "Control", "Alpha" = "α"),
            Treatment = factor(Treatment, levels = c("Control", "α")) )  # enforce order
   
  
  ggplot(data, aes(x = Treatment, y = !!sym(response_var), color = Treatment)) +
    geom_violin(trim = FALSE, aes(fill = Treatment), alpha = 0.6, color = NA) +
    geom_jitter(width = 0.15, size = 2, alpha = 0.7, show.legend = FALSE) +
    geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.8, color = "black") +
    ggpubr::stat_pvalue_manual(stats_res, label = "p.label", tip.length = 0.01) +
        facet_wrap(~Name, scales = "free_x", strip.position = "top") +
        labs(   title = paste("Treatment effect (Control vs α):", response_var),
      x = "Treatment", y = y_lab
    ) +
    theme_classic(base_size = 16) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
    theme(
      axis.text.x = element_text(angle = 30, hjust = 1, face = "bold"),
      axis.text.y = element_text(face = "bold"),
      strip.text  = element_text(size = 14, face = "bold"),
      strip.background = element_blank(),
      legend.position  = "none"  )+
    annotate("segment",x=Inf,xend=-Inf,y=Inf,yend=Inf,color="black",lwd=1)
}

# ---- 3. Wrapper ----
make_treatment_analysis <- function(data, response_var, y_lab = NULL) {
  response_var <- trimws(response_var)
  
  stats_res <- analyze_treatment_effect(data, response_var)
  print(stats_res)
  
  if (is.null(y_lab)) y_lab <- response_var
  
  p <- plot_treatment_effect(data, response_var, stats_res, y_lab = y_lab)
  print(p)
  
  return(list(results = stats_res, plot = p))
}


pegs_out <- make_treatment_analysis(tbl, response_var = "Cells_with_pegs",
                                    y_lab = "Pegged cells (%)")
spore_out <- make_treatment_analysis(tbl, response_var = "Spore", y_lab = "Spore")
#shmoo_out <- 
  make_treatment_analysis(tbl, response_var = "Shmoo",y_lab = "MCBE")
  

