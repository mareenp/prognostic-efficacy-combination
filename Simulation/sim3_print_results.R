#set working directory
setwd("...")

# load results
load("results_eventrate.RData")
load("results_samplesize.RData")
load("results_biomarker.RData")
load("results_sensitivity.RData")

# 1-CutOff_bio, 2-specificity, 3-prob, 4-groupsize

col_names <- c("p_C_BP_True", "p_C_BP_Ref", "p_C_BP_New", "p_I_BP_True", "p_I_BP_Ref", "p_I_BP_New", "treatment_True", "treatment_Ref", "treatment_New",
               "SD_p_C_BP_True", "SD_p_C_BP_Ref", "SD_p_C_BP_New", "SD_p_I_BP_True", "SD_p_I_BP_Ref", "SD_p_I_BP_New", 
               "Bias_Treat_Ref", "Bias_Treat_New", "RMSE_Treat_Ref", "RMSE_Treat_New", "CutOff_True", "CutOff_Ref", "CutOff_New", 
               "groupsize_C_BP_True", "groupsize_C_BP_Ref", "groupsize_C_BP_New", "groupsize_I_BP_True", "groupsize_I_BP_Ref", "groupsize_I_BP_New", 
               "specificity_C_True", "specificity_C_Ref", "specificity_C_New", "specificity_I_True", "specificity_I_Ref", "specificity_I_New", 
               "sensitivity_C_True", "sensitivity_C_Ref", "sensitivity_C_New", "sensitivity_I_True", "sensitivity_I_Ref", "sensitivity_I_New")

table_results6 <- setNames(data.frame(matrix(ncol = 40, nrow = 6)),col_names)
table_results7 <- setNames(data.frame(matrix(ncol = 40, nrow = 7)),col_names)
table_results8 <- setNames(data.frame(matrix(ncol = 40, nrow = 8)), col_names)
table_results9 <- setNames(data.frame(matrix(ncol = 40, nrow = 9)), col_names)
summary_results <- list(eventrate = table_results7, biomarker = table_results9, samplesize = table_results8, sensitivity = table_results6)


for (i in 1:7) {
  nsim <- nrow(results_eventrate[[i]]$prob)
  summary_results$eventrate$p_C_BP_True[i] <- round(mean(results_eventrate[[i]]$prob$C_bp_True),3)
  summary_results$eventrate$p_C_BP_Ref[i] <- round(mean(results_eventrate[[i]]$prob$C_bp_Ref),3)
  summary_results$eventrate$p_C_BP_New[i] <- round(mean(results_eventrate[[i]]$prob$C_bp_New),3)
  summary_results$eventrate$p_I_BP_True[i] <- round(mean(results_eventrate[[i]]$prob$I_bp_True),3)
  summary_results$eventrate$p_I_BP_Ref[i] <- round(mean(results_eventrate[[i]]$prob$I_bp_Ref),3)
  summary_results$eventrate$p_I_BP_New[i] <- round(mean(results_eventrate[[i]]$prob$I_bp_New),3)
  summary_results$eventrate$treatment_True[i] <- round(mean(results_eventrate[[i]]$prob$Treat_True),3)
  summary_results$eventrate$treatment_Ref[i] <- round(mean(results_eventrate[[i]]$prob$Treat_Ref),3)
  summary_results$eventrate$treatment_New[i] <- round(mean(results_eventrate[[i]]$prob$Treat_New),3)

  summary_results$eventrate$SD_p_C_BP_True[i] <- round(sd(results_eventrate[[i]]$prob$C_bp_True),3)
  summary_results$eventrate$SD_p_C_BP_Ref[i] <- round(sd(results_eventrate[[i]]$prob$C_bp_Ref),3)
  summary_results$eventrate$SD_p_C_BP_New[i] <- round(sd(results_eventrate[[i]]$prob$C_bp_New),3)
  summary_results$eventrate$SD_p_I_BP_True[i] <- round(sd(results_eventrate[[i]]$prob$I_bp_True),3)
  summary_results$eventrate$SD_p_I_BP_Ref[i] <- round(sd(results_eventrate[[i]]$prob$I_bp_Ref),3)
  summary_results$eventrate$SD_p_I_BP_New[i] <- round(sd(results_eventrate[[i]]$prob$I_bp_New),3)

  summary_results$eventrate$Bias_Treat_Ref[i] <- round(mean(results_eventrate[[i]]$prob$Treat_Ref-results_eventrate[[i]]$prob$Treat_True),4)
  summary_results$eventrate$Bias_Treat_New[i] <- round(mean(results_eventrate[[i]]$prob$Treat_New-results_eventrate[[i]]$prob$Treat_True),4)
  summary_results$eventrate$RMSE_Treat_Ref[i] <- round(sqrt(mean((results_eventrate[[i]]$prob$Treat_Ref-results_eventrate[[i]]$prob$Treat_True)^2)),4)
  summary_results$eventrate$RMSE_Treat_New[i] <- round(sqrt(mean((results_eventrate[[i]]$prob$Treat_New-results_eventrate[[i]]$prob$Treat_True)^2)),4)
  
  summary_results$eventrate$groupsize_C_BP_True[i] <- round(mean(results_eventrate[[i]]$groupsize$C_bp_True),1)
  summary_results$eventrate$groupsize_I_BP_True[i] <- round(mean(results_eventrate[[i]]$groupsize$I_bp_True),1)
  summary_results$eventrate$groupsize_C_BP_Ref[i] <- round(mean(results_eventrate[[i]]$groupsize$C_bp_Ref),1)
  summary_results$eventrate$groupsize_I_BP_Ref[i] <- round(mean(results_eventrate[[i]]$groupsize$I_bp_Ref),1)
  summary_results$eventrate$groupsize_C_BP_New[i] <- round(mean(results_eventrate[[i]]$groupsize$C_bp_New),1)
  summary_results$eventrate$groupsize_I_BP_New[i] <- round(mean(results_eventrate[[i]]$groupsize$I_bp_New),1)
  summary_results$eventrate$CutOff_True[i] <- round(mean(results_eventrate[[i]]$CutOff_bio$True),3)
  summary_results$eventrate$CutOff_Ref[i] <- round(mean(results_eventrate[[i]]$CutOff_bio$Ref),3)
  summary_results$eventrate$CutOff_New[i] <- round(mean(results_eventrate[[i]]$CutOff_bio$New),3)
  
  summary_results$eventrate$specificity_C_True[i] <- round(mean(results_eventrate[[i]]$specificity$C_True),3)
  summary_results$eventrate$specificity_C_Ref[i] <- round(mean(results_eventrate[[i]]$specificity$C_Ref),3)
  summary_results$eventrate$specificity_C_New[i] <- round(mean(results_eventrate[[i]]$specificity$C_New),3)
  summary_results$eventrate$specificity_I_True[i] <- round(mean(results_eventrate[[i]]$specificity$I_True),3)
  summary_results$eventrate$specificity_I_Ref[i] <- round(mean(results_eventrate[[i]]$specificity$I_Ref),3)
  summary_results$eventrate$specificity_I_New[i] <- round(mean(results_eventrate[[i]]$specificity$I_New),3)
  summary_results$eventrate$sensitivity_C_True[i] <- round(mean(results_eventrate[[i]]$sensitivity$C_True),3)
  summary_results$eventrate$sensitivity_C_Ref[i] <- round(mean(results_eventrate[[i]]$sensitivity$C_Ref),3)
  summary_results$eventrate$sensitivity_C_New[i] <- round(mean(results_eventrate[[i]]$sensitivity$C_New),3)
  summary_results$eventrate$sensitivity_I_True[i] <- round(mean(results_eventrate[[i]]$sensitivity$I_True),3)
  summary_results$eventrate$sensitivity_I_Ref[i] <- round(mean(results_eventrate[[i]]$sensitivity$I_Ref),3)
  summary_results$eventrate$sensitivity_I_New[i] <- round(mean(results_eventrate[[i]]$sensitivity$I_New),3)
  
  summary_results$eventrate$SD_CutOff_Ref[i] <- round(sd(results_eventrate[[i]]$CutOff_bio$Ref),3)
  summary_results$eventrate$SD_CutOff_New[i] <- round(sd(results_eventrate[[i]]$CutOff_bio$New),3)
  
}

for (i in 1:8) {
  nsim <- nrow(results_samplesize[[i]]$prob)
  summary_results$samplesize$p_C_BP_True[i] <- round(mean(results_samplesize[[i]]$prob$C_bp_True),3)
  summary_results$samplesize$p_C_BP_Ref[i] <- round(mean(results_samplesize[[i]]$prob$C_bp_Ref),3)
  summary_results$samplesize$p_C_BP_New[i] <- round(mean(results_samplesize[[i]]$prob$C_bp_New),3)
  summary_results$samplesize$p_I_BP_True[i] <- round(mean(results_samplesize[[i]]$prob$I_bp_True),3)
  summary_results$samplesize$p_I_BP_Ref[i] <- round(mean(results_samplesize[[i]]$prob$I_bp_Ref),3)
  summary_results$samplesize$p_I_BP_New[i] <- round(mean(results_samplesize[[i]]$prob$I_bp_New),3)
  summary_results$samplesize$treatment_True[i] <- round(mean(results_samplesize[[i]]$prob$Treat_True),3)
  summary_results$samplesize$treatment_Ref[i] <- round(mean(results_samplesize[[i]]$prob$Treat_Ref),3)
  summary_results$samplesize$treatment_New[i] <- round(mean(results_samplesize[[i]]$prob$Treat_New),3)
  
  summary_results$samplesize$SD_p_C_BP_True[i] <- round(sd(results_samplesize[[i]]$prob$C_bp_True),3)
  summary_results$samplesize$SD_p_C_BP_Ref[i] <- round(sd(results_samplesize[[i]]$prob$C_bp_Ref),3)
  summary_results$samplesize$SD_p_C_BP_New[i] <- round(sd(results_samplesize[[i]]$prob$C_bp_New),3)
  summary_results$samplesize$SD_p_I_BP_True[i] <- round(sd(results_samplesize[[i]]$prob$I_bp_True),3)
  summary_results$samplesize$SD_p_I_BP_Ref[i] <- round(sd(results_samplesize[[i]]$prob$I_bp_Ref),3)
  summary_results$samplesize$SD_p_I_BP_New[i] <- round(sd(results_samplesize[[i]]$prob$I_bp_New),3)
  
  summary_results$samplesize$Bias_Treat_Ref[i] <- round(mean(results_samplesize[[i]]$prob$Treat_Ref-results_samplesize[[i]]$prob$Treat_True),4)
  summary_results$samplesize$Bias_Treat_New[i] <- round(mean(results_samplesize[[i]]$prob$Treat_New-results_samplesize[[i]]$prob$Treat_True),4)
  summary_results$samplesize$RMSE_Treat_Ref[i] <- round(sqrt(mean((results_samplesize[[i]]$prob$Treat_Ref-results_samplesize[[i]]$prob$Treat_True)^2)),4)
  summary_results$samplesize$RMSE_Treat_New[i] <- round(sqrt(mean((results_samplesize[[i]]$prob$Treat_New-results_samplesize[[i]]$prob$Treat_True)^2)),4)
  
  summary_results$samplesize$groupsize_C_BP_True[i] <- round(mean(results_samplesize[[i]]$groupsize$C_bp_True),1)
  summary_results$samplesize$groupsize_I_BP_True[i] <- round(mean(results_samplesize[[i]]$groupsize$I_bp_True),1)
  summary_results$samplesize$groupsize_C_BP_Ref[i] <- round(mean(results_samplesize[[i]]$groupsize$C_bp_Ref),1)
  summary_results$samplesize$groupsize_I_BP_Ref[i] <- round(mean(results_samplesize[[i]]$groupsize$I_bp_Ref),1)
  summary_results$samplesize$groupsize_C_BP_New[i] <- round(mean(results_samplesize[[i]]$groupsize$C_bp_New),1)
  summary_results$samplesize$groupsize_I_BP_New[i] <- round(mean(results_samplesize[[i]]$groupsize$I_bp_New),1)
  summary_results$samplesize$CutOff_True[i] <- round(mean(results_samplesize[[i]]$CutOff_bio$True),3)
  summary_results$samplesize$CutOff_Ref[i] <- round(mean(results_samplesize[[i]]$CutOff_bio$Ref),3)
  summary_results$samplesize$CutOff_New[i] <- round(mean(results_samplesize[[i]]$CutOff_bio$New),3)
  
  summary_results$samplesize$specificity_C_True[i] <- round(mean(results_samplesize[[i]]$specificity$C_True),3)
  summary_results$samplesize$specificity_C_Ref[i] <- round(mean(results_samplesize[[i]]$specificity$C_Ref),3)
  summary_results$samplesize$specificity_C_New[i] <- round(mean(results_samplesize[[i]]$specificity$C_New),3)
  summary_results$samplesize$specificity_I_True[i] <- round(mean(results_samplesize[[i]]$specificity$I_True),3)
  summary_results$samplesize$specificity_I_Ref[i] <- round(mean(results_samplesize[[i]]$specificity$I_Ref),3)
  summary_results$samplesize$specificity_I_New[i] <- round(mean(results_samplesize[[i]]$specificity$I_New),3)
  summary_results$samplesize$sensitivity_C_True[i] <- round(mean(results_samplesize[[i]]$sensitivity$C_True),3)
  summary_results$samplesize$sensitivity_C_Ref[i] <- round(mean(results_samplesize[[i]]$sensitivity$C_Ref),3)
  summary_results$samplesize$sensitivity_C_New[i] <- round(mean(results_samplesize[[i]]$sensitivity$C_New),3)
  summary_results$samplesize$sensitivity_I_True[i] <- round(mean(results_samplesize[[i]]$sensitivity$I_True),3)
  summary_results$samplesize$sensitivity_I_Ref[i] <- round(mean(results_samplesize[[i]]$sensitivity$I_Ref),3)
  summary_results$samplesize$sensitivity_I_New[i] <- round(mean(results_samplesize[[i]]$sensitivity$I_New),3)
  
  summary_results$samplesize$SD_CutOff_Ref[i] <- round(sd(results_samplesize[[i]]$CutOff_bio$Ref),3)
  summary_results$samplesize$SD_CutOff_New[i] <- round(sd(results_samplesize[[i]]$CutOff_bio$New),3)
}

for (i in 1:9) {
  nsim <- nrow(results_biomarker[[i]]$prob)
  summary_results$biomarker$p_C_BP_True[i] <- round(mean(results_biomarker[[i]]$prob$C_bp_True),3)
  summary_results$biomarker$p_C_BP_Ref[i] <- round(mean(results_biomarker[[i]]$prob$C_bp_Ref),3)
  summary_results$biomarker$p_C_BP_New[i] <- round(mean(results_biomarker[[i]]$prob$C_bp_New),3)
  summary_results$biomarker$p_I_BP_True[i] <- round(mean(results_biomarker[[i]]$prob$I_bp_True),3)
  summary_results$biomarker$p_I_BP_Ref[i] <- round(mean(results_biomarker[[i]]$prob$I_bp_Ref),3)
  summary_results$biomarker$p_I_BP_New[i] <- round(mean(results_biomarker[[i]]$prob$I_bp_New),3)
  summary_results$biomarker$treatment_True[i] <- round(mean(results_biomarker[[i]]$prob$Treat_True),3)
  summary_results$biomarker$treatment_Ref[i] <- round(mean(results_biomarker[[i]]$prob$Treat_Ref),3)
  summary_results$biomarker$treatment_New[i] <- round(mean(results_biomarker[[i]]$prob$Treat_New),3)
  
  summary_results$biomarker$SD_p_C_BP_True[i] <- round(sd(results_biomarker[[i]]$prob$C_bp_True),3)
  summary_results$biomarker$SD_p_C_BP_Ref[i] <- round(sd(results_biomarker[[i]]$prob$C_bp_Ref),3)
  summary_results$biomarker$SD_p_C_BP_New[i] <- round(sd(results_biomarker[[i]]$prob$C_bp_New),3)
  summary_results$biomarker$SD_p_I_BP_True[i] <- round(sd(results_biomarker[[i]]$prob$I_bp_True),3)
  summary_results$biomarker$SD_p_I_BP_Ref[i] <- round(sd(results_biomarker[[i]]$prob$I_bp_Ref),3)
  summary_results$biomarker$SD_p_I_BP_New[i] <- round(sd(results_biomarker[[i]]$prob$I_bp_New),3)
  
  summary_results$biomarker$Bias_Treat_Ref[i] <- round(mean(results_biomarker[[i]]$prob$Treat_Ref-results_biomarker[[i]]$prob$Treat_True),4)
  summary_results$biomarker$Bias_Treat_New[i] <- round(mean(results_biomarker[[i]]$prob$Treat_New-results_biomarker[[i]]$prob$Treat_True),4)
  summary_results$biomarker$RMSE_Treat_Ref[i] <- round(sqrt(mean((results_biomarker[[i]]$prob$Treat_Ref-results_biomarker[[i]]$prob$Treat_True)^2)),4)
  summary_results$biomarker$RMSE_Treat_New[i] <- round(sqrt(mean((results_biomarker[[i]]$prob$Treat_New-results_biomarker[[i]]$prob$Treat_True)^2)),4)
  
  summary_results$biomarker$groupsize_C_BP_True[i] <- round(mean(results_biomarker[[i]]$groupsize$C_bp_True),1)
  summary_results$biomarker$groupsize_I_BP_True[i] <- round(mean(results_biomarker[[i]]$groupsize$I_bp_True),1)
  summary_results$biomarker$groupsize_C_BP_Ref[i] <- round(mean(results_biomarker[[i]]$groupsize$C_bp_Ref),1)
  summary_results$biomarker$groupsize_I_BP_Ref[i] <- round(mean(results_biomarker[[i]]$groupsize$I_bp_Ref),1)
  summary_results$biomarker$groupsize_C_BP_New[i] <- round(mean(results_biomarker[[i]]$groupsize$C_bp_New),1)
  summary_results$biomarker$groupsize_I_BP_New[i] <- round(mean(results_biomarker[[i]]$groupsize$I_bp_New),1)
  summary_results$biomarker$CutOff_True[i] <- round(mean(results_biomarker[[i]]$CutOff_bio$True),3)
  summary_results$biomarker$CutOff_Ref[i] <- round(mean(results_biomarker[[i]]$CutOff_bio$Ref),3)
  summary_results$biomarker$CutOff_New[i] <- round(mean(results_biomarker[[i]]$CutOff_bio$New),3)
  
  summary_results$biomarker$specificity_C_True[i] <- round(mean(results_biomarker[[i]]$specificity$C_True),3)
  summary_results$biomarker$specificity_C_Ref[i] <- round(mean(results_biomarker[[i]]$specificity$C_Ref),3)
  summary_results$biomarker$specificity_C_New[i] <- round(mean(results_biomarker[[i]]$specificity$C_New),3)
  summary_results$biomarker$specificity_I_True[i] <- round(mean(results_biomarker[[i]]$specificity$I_True),3)
  summary_results$biomarker$specificity_I_Ref[i] <- round(mean(results_biomarker[[i]]$specificity$I_Ref),3)
  summary_results$biomarker$specificity_I_New[i] <- round(mean(results_biomarker[[i]]$specificity$I_New),3)
  summary_results$biomarker$sensitivity_C_True[i] <- round(mean(results_biomarker[[i]]$sensitivity$C_True),3)
  summary_results$biomarker$sensitivity_C_Ref[i] <- round(mean(results_biomarker[[i]]$sensitivity$C_Ref),3)
  summary_results$biomarker$sensitivity_C_New[i] <- round(mean(results_biomarker[[i]]$sensitivity$C_New),3)
  summary_results$biomarker$sensitivity_I_True[i] <- round(mean(results_biomarker[[i]]$sensitivity$I_True),3)
  summary_results$biomarker$sensitivity_I_Ref[i] <- round(mean(results_biomarker[[i]]$sensitivity$I_Ref),3)
  summary_results$biomarker$sensitivity_I_New[i] <- round(mean(results_biomarker[[i]]$sensitivity$I_New),3)
  
  summary_results$biomarker$SD_CutOff_Ref[i] <- round(sd(results_biomarker[[i]]$CutOff_bio$Ref),3)
  summary_results$biomarker$SD_CutOff_New[i] <- round(sd(results_biomarker[[i]]$CutOff_bio$New),3)
}

for (i in 1:6) {
  nsim <- nrow(results_sensitivity[[i]]$prob)
  summary_results$sensitivity$p_C_BP_True[i] <- round(mean(results_sensitivity[[i]]$prob$C_bp_True),3)
  summary_results$sensitivity$p_C_BP_Ref[i] <- round(mean(results_sensitivity[[i]]$prob$C_bp_Ref),3)
  summary_results$sensitivity$p_C_BP_New[i] <- round(mean(results_sensitivity[[i]]$prob$C_bp_New),3)
  summary_results$sensitivity$p_I_BP_True[i] <- round(mean(results_sensitivity[[i]]$prob$I_bp_True),3)
  summary_results$sensitivity$p_I_BP_Ref[i] <- round(mean(results_sensitivity[[i]]$prob$I_bp_Ref),3)
  summary_results$sensitivity$p_I_BP_New[i] <- round(mean(results_sensitivity[[i]]$prob$I_bp_New),3)
  summary_results$sensitivity$treatment_True[i] <- round(mean(results_sensitivity[[i]]$prob$Treat_True),3)
  summary_results$sensitivity$treatment_Ref[i] <- round(mean(results_sensitivity[[i]]$prob$Treat_Ref),3)
  summary_results$sensitivity$treatment_New[i] <- round(mean(results_sensitivity[[i]]$prob$Treat_New),3)
  
  summary_results$sensitivity$SD_p_C_BP_True[i] <- round(sd(results_sensitivity[[i]]$prob$C_bp_True),3)
  summary_results$sensitivity$SD_p_C_BP_Ref[i] <- round(sd(results_sensitivity[[i]]$prob$C_bp_Ref),3)
  summary_results$sensitivity$SD_p_C_BP_New[i] <- round(sd(results_sensitivity[[i]]$prob$C_bp_New),3)
  summary_results$sensitivity$SD_p_I_BP_True[i] <- round(sd(results_sensitivity[[i]]$prob$I_bp_True),3)
  summary_results$sensitivity$SD_p_I_BP_Ref[i] <- round(sd(results_sensitivity[[i]]$prob$I_bp_Ref),3)
  summary_results$sensitivity$SD_p_I_BP_New[i] <- round(sd(results_sensitivity[[i]]$prob$I_bp_New),3)
  
  summary_results$sensitivity$Bias_Treat_Ref[i] <- round(mean(results_sensitivity[[i]]$prob$Treat_Ref-results_sensitivity[[i]]$prob$Treat_True),4)
  summary_results$sensitivity$Bias_Treat_New[i] <- round(mean(results_sensitivity[[i]]$prob$Treat_New-results_sensitivity[[i]]$prob$Treat_True),4)
  summary_results$sensitivity$RMSE_Treat_Ref[i] <- round(sqrt(mean((results_sensitivity[[i]]$prob$Treat_Ref-results_sensitivity[[i]]$prob$Treat_True)^2)),4)
  summary_results$sensitivity$RMSE_Treat_New[i] <- round(sqrt(mean((results_sensitivity[[i]]$prob$Treat_New-results_sensitivity[[i]]$prob$Treat_True)^2)),4)
  
  summary_results$sensitivity$groupsize_C_BP_True[i] <- round(mean(results_sensitivity[[i]]$groupsize$C_bp_True),1)
  summary_results$sensitivity$groupsize_I_BP_True[i] <- round(mean(results_sensitivity[[i]]$groupsize$I_bp_True),1)
  summary_results$sensitivity$groupsize_C_BP_Ref[i] <- round(mean(results_sensitivity[[i]]$groupsize$C_bp_Ref),1)
  summary_results$sensitivity$groupsize_I_BP_Ref[i] <- round(mean(results_sensitivity[[i]]$groupsize$I_bp_Ref),1)
  summary_results$sensitivity$groupsize_C_BP_New[i] <- round(mean(results_sensitivity[[i]]$groupsize$C_bp_New),1)
  summary_results$sensitivity$groupsize_I_BP_New[i] <- round(mean(results_sensitivity[[i]]$groupsize$I_bp_New),1)
  summary_results$sensitivity$CutOff_True[i] <- round(mean(results_sensitivity[[i]]$CutOff_bio$True),3)
  summary_results$sensitivity$CutOff_Ref[i] <- round(mean(results_sensitivity[[i]]$CutOff_bio$Ref),3)
  summary_results$sensitivity$CutOff_New[i] <- round(mean(results_sensitivity[[i]]$CutOff_bio$New),3)
  
  summary_results$sensitivity$specificity_C_True[i] <- round(mean(results_sensitivity[[i]]$specificity$C_True),3)
  summary_results$sensitivity$specificity_C_Ref[i] <- round(mean(results_sensitivity[[i]]$specificity$C_Ref),3)
  summary_results$sensitivity$specificity_C_New[i] <- round(mean(results_sensitivity[[i]]$specificity$C_New),3)
  summary_results$sensitivity$specificity_I_True[i] <- round(mean(results_sensitivity[[i]]$specificity$I_True),3)
  summary_results$sensitivity$specificity_I_Ref[i] <- round(mean(results_sensitivity[[i]]$specificity$I_Ref),3)
  summary_results$sensitivity$specificity_I_New[i] <- round(mean(results_sensitivity[[i]]$specificity$I_New),3)
  summary_results$sensitivity$sensitivity_C_True[i] <- round(mean(results_sensitivity[[i]]$sensitivity$C_True),3)
  summary_results$sensitivity$sensitivity_C_Ref[i] <- round(mean(results_sensitivity[[i]]$sensitivity$C_Ref),3)
  summary_results$sensitivity$sensitivity_C_New[i] <- round(mean(results_sensitivity[[i]]$sensitivity$C_New),3)
  summary_results$sensitivity$sensitivity_I_True[i] <- round(mean(results_sensitivity[[i]]$sensitivity$I_True),3)
  summary_results$sensitivity$sensitivity_I_Ref[i] <- round(mean(results_sensitivity[[i]]$sensitivity$I_Ref),3)
  summary_results$sensitivity$sensitivity_I_New[i] <- round(mean(results_sensitivity[[i]]$sensitivity$I_New),3)
  
  summary_results$sensitivity$SD_CutOff_Ref[i] <- round(sd(results_sensitivity[[i]]$CutOff_bio$Ref),3)
  summary_results$sensitivity$SD_CutOff_New[i] <- round(sd(results_sensitivity[[i]]$CutOff_bio$New),3)
}


summary_results$eventrate
summary_results$samplesize
summary_results$biomarker
summary_results$sensitivity

write.table(as.data.frame(summary_results$eventrate), file="summary_eventrate.csv", sep=";", row.names = FALSE)
write.table(as.data.frame(summary_results$samplesize), file="summary_samplesize.csv", sep=";", row.names = FALSE)
write.table(as.data.frame(summary_results$biomarker), file="summary_biomarker.csv", sep=";", row.names = FALSE)
write.table(as.data.frame(summary_results$sensitivity), file="summary_sensitivity.csv", sep=";", row.names = FALSE)




