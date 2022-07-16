##############################################################################################################
# Mareen Pigorsch, Stefan Gehrig
##############################################################################################################
############################ Plot Bias for different scenarios ###############################################
##############################################################################################################
load("results_biomarker.RData")
load("results_eventproportion.RData")
load("results_samplesize.RData")
load("results_sensitivity.RData")

library(ggplot2)
library(purrr)
library(dplyr)
library(tidyr)
library(patchwork)
library(cowplot)

results <- c(results_biomarker, 
             results_eventproportion,
             results_samplesize, 
             results_sensitivity)

names(results) <- c(paste(rep("(a) Biomarker", length(results_biomarker)), 1:length(results_biomarker), sep = "_"),
                    paste(rep("(b) Event proportion", length(results_eventproportion)), 1:length(results_eventproportion), sep = "_"),
                    paste(rep("(c) Sample size", length(results_samplesize)), 1:length(results_samplesize), sep = "_"),
                    paste(rep("(d) Sensitivity", length(results_sensitivity)), 1:length(results_sensitivity), sep = "_"))

df <- map_dfr(results, .id = "condition", ~.x$prob) %>% 
  pivot_longer(cols = c(Treat_New, Treat_Split, Treat_Ref), names_to = "design", values_to = "estimate") %>% 
  mutate(Treat_dev = estimate-Treat_True) %>% 
  mutate(design = ifelse(design == "Treat_Split", "Split of control into training and test (Split)", 
                      ifelse(design == "Treat_Ref", "External biomarker study (Reference)",
                             "Prognostic-efficacy-combination design (New)"))) %>% 
  separate(condition, into = c("parameter", "condition"), sep = "_") %>% 
  mutate(condition = case_when(
    parameter == "(a) Biomarker" ~ paste0("Bio", condition),
    parameter == "(b) Event proportion" ~ paste0("Ep", condition),
    parameter == "(c) Sample size" ~ paste0("Sam", condition),
    TRUE ~ paste0("Sen", condition)
  ))

limit_plot <- df %>% 
  group_by(parameter, condition) %>% 
  summarise(q_lower = quantile(Treat_dev, 0.25)-1.5*IQR(Treat_dev),
            q_upper = quantile(Treat_dev, 0.75)+1.5*IQR(Treat_dev),
            .groups = "drop") %>%
  summarise(max(c(abs(min(q_lower)), abs(max(q_upper))))) %>% 
  pull

plots <- map(as.list(unique(df$parameter)),
      function(x) {
        df %>% filter(parameter == x) %>%
          ggplot(aes(y = Treat_dev,  x = condition, fill = design)) +
          geom_boxplot(colour = "black", alpha = 0.75, lwd = 0.5, fatten = 0.9,
                       outlier.shape = NA) +
          labs(y = "Bias of estimated treatment effect",
               x = "Scenario",
               subtitle = x) +
          geom_hline(yintercept = 0, lwd = 0.5, color = "red") +
          scale_fill_grey(name = "Cut-off from") +
          theme_minimal(base_size = 12) +
          theme(legend.position = "bottom") +
          guides(fill = guide_legend(ncol = 1)) +
          coord_cartesian(ylim = c(-limit_plot, limit_plot))
      })

legend <- get_legend(plots[[1]])
plots <- map(plots, ~.x + theme(panel.grid.major = element_line(linetype = "dashed", color = "lightgrey"),
                                panel.grid.minor = element_blank(),
                                legend.position = "none",
                                axis.text.x = element_text(angle = 45, hjust = 1)))

tiff(filename = paste("Plot_bias.tiff"), compression = "lzw+p",
     width = 5000, height = 5500, res = 650)
(plots[[1]] | plots[[2]]) / 
  (plots[[3]] | plots[[4]]) / 
  legend + plot_layout(heights = c(1,1,0.2))
dev.off()
