# packages
#install.packages("dplyr", "tibble", "pROC", "caret")
library(dplyr)
library(tibble)
library(pROC)
library(caret)

### Abbreviations 
# n - number of cases per arm, N - total sample size
# p_c - probability of events in control arm
# p_i - probability of events in intervention arm
##### biomarker
# meanlog_0 - meanlog of biomarker distribution in non-profiters
# meanlog_1 - meanlog of biomarker distribution in potential profiters
# sdlog_0 - sdlog of biomarker distribution in non-profiters
# sdlog_1 - sdlog of biomarker distribution in potential profiters
# sen - sensitivity in percent
# bp - biomarker positive --> subgroup that would be recommended to intervention due to biomarker Cut-Off
# C - Control arm
# I - Intervention arm
# New_CV - using Cut-Off from Control group with cross-validation (New design with CV)



################################################################################
######################### APPLICATION FUNCTION #################################
pec_d_cv <- function(data, outcome, group, biomarker, sen, ref_group = 0, int_group = 1){
  # seed
  set.seed(242)
  
  group_vals <- data[[group]]
  if (!all(c(ref_group, int_group) %in% group_vals)) {
    stop("The given values for 'ref_group' or 'int_group' were not found in group variable!")
  }
  
  # Rename variables
  data <- data %>% 
    rename(
      outcome = !!sym(outcome), 
      group = !!sym(group), 
      biomarker = !!sym(biomarker)
    )

  #### save into
  CutOff_bio <- tibble(New_CV=rep(NA,1)) 
  #estimated probabilities & treatment effect in the biomarker-positive groups
  prob <- tibble(C_bp=rep(NA,1), 
                 I_bp=rep(NA,1),
                 Treat=rep(NA,1))

  ####### Analysis for New_CV - new design including cross-validation
  data_C <- data %>% filter(group == ref_group)
  data_I <- data %>% filter(group == int_group)
  
  roc_CV <- function(dat_C_train, dat_C_test, data_I, sen) {
  roc_CV <- roc(outcome ~ biomarker, data = dat_C_train, quiet = TRUE, direction="<")
  cutoff_CV <- max(roc_CV$thresholds[roc_CV$sensitivities>=sen])
  
  prob_I_bp <- mean(data_I$outcome[data_I$biomarker>=cutoff_CV]) 
  prob_C_bp <- mean(dat_C_test$outcome[dat_C_test$biomarker>=cutoff_CV]) 
  prob_Treat <- prob_C_bp - prob_I_bp
  
  return(list(cutoff = cutoff_CV, prob_I = prob_I_bp, prob_C = prob_C_bp, 
              prob_Treat = prob_Treat))
  }

  # create folds for 10-times 10-fold cross-validation
  folds <- createMultiFolds(data_C$event_exp, k = 10, times = 10) # 'times' for repeated CV
  # Analysis for every Fold
  fold_results <- lapply(folds, function(train_index) {
    dat_C_train <- data_C[train_index, ]
    dat_C_test  <- data_C[-train_index, ]
    roc_CV(dat_C_train, dat_C_test, data_I, sen)
  })
  CutOff_bio$New_CV <- mean(sapply(fold_results, function(x) x$cutoff), na.rm=T)
  prob$C_bp <- mean(sapply(fold_results, function(x) x$prob_C), na.rm=T) 
  prob$I_bp <- mean(sapply(fold_results, function(x) x$prob_I), na.rm=T) 
  prob$Treat <- mean(sapply(fold_results, function(x) x$prob_Treat), na.rm=T)

result <- list(CutOff_bio=CutOff_bio, prob=prob)
return(result)
}

################################################################################
##### simulate example data
n=750
p_c=0.15
p_i=0.1
meanlog_0=3.0
meanlog_1=4.0
sdlog_0=0.5
sdlog_1=0.5
N=2*n

# seed
set.seed(842)

data <- tibble(event_exp=rep(-1,N), event_true=rep(-1,N), biomarker=rep(-1,N), group_i=rep(-1,N),
               bp_True=rep(-1,N), bp_New=rep(-1,N), bp_Ref=rep(-1,N), bp_Split=rep(-1,N))

######### simulation of events and corresponding biomarkers ##################
## event with eventrate of control
data$event_exp[1:N] <- data$event_true  <- rbinom(N,1,p_c)
## biomarker
# lower biomarker for those who DONT have an event_exp
data$biomarker[data$event_exp==0] <- rlnorm(sum(data$event_exp==0), meanlog = meanlog_0, sdlog = sdlog_0)
# higher biomarker for those who DO have an event_exp
data$biomarker[data$event_exp==1] <- rlnorm(sum(data$event_exp==1), meanlog = meanlog_1, sdlog = sdlog_1) 
# define groups control & intervention
data$group_i[1:n] <- 0 # Control group
data$group_i[(n+1):N] <- 1 # Intervention group
# actually events in intervention group
data$event_true[data$event_exp[(n+1):N]==1 & data$group_i==1] <- rbinom(sum(data$event_exp[(n+1):N]==1),1,(p_i/p_c))


################################################################################
##### APPLICATION
pec_d_cv(data=data, outcome="event_true", group="group_i", biomarker="biomarker", sen=0.95, ref_group = 0, int_group = 1)

