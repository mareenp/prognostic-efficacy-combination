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
# itnumber - number of iterations
# sen - sensitivity in percent
# bp - biomarker positive --> subgroup that would be recommended to intervention due to biomarker Cut-Off
# C - Control arm
# I - Intervention arm
# True - using the True calculated biomarker Cut-Off knowing the distribution of the biomarker
# New - using Cut-Off from Control group (New design)
# Ref - using Cut-Off from External sample (Reference design)
# Split - using Cut-Off from Control group, splitting Control into training and test (Split design)
# New_CV - using Cut-Off from Control group with cross-validation (New design with CV)


simu <- function(n, p_c, p_i, meanlog_0, meanlog_1, sdlog_0, sdlog_1, sen, itnumber){
N <- 2*n

# matrix ind_matrix for generating random numbers for sampling Control into 
# "Training" and "Test" for split sample method as additional comparator for new method
set.seed(17082022)
ind_matrix <- matrix(rep(seq(1,n), itnumber),ncol = n)
for (i in 1:itnumber) {
  ind_matrix[i,] <- sample(n, n)
}

sim_data <- list()
# seed
set.seed(242)

########################## save results into #########################################################
#Biomarker Cut-Off)
CutOff_bio <- tibble(True=rep(NA,itnumber), New=rep(NA,itnumber), 
                     Ref=rep(NA,itnumber), Split=rep(NA,itnumber),
                     New_CV=rep(NA,itnumber)) 
#estimated probabilities & treatment effect in the biomarker-positive groups
prob <- tibble(C_bp_True=rep(NA,itnumber), I_bp_True=rep(NA,itnumber),
               C_bp_New=rep(NA,itnumber), I_bp_New=rep(NA,itnumber),
               C_bp_Ref=rep(NA,itnumber), I_bp_Ref=rep(NA,itnumber),
               C_bp_Split=rep(NA,itnumber), I_bp_Split=rep(NA,itnumber),
               C_bp_New_CV=rep(NA,itnumber), I_bp_New_CV=rep(NA,itnumber),
               Treat_True=rep(NA,itnumber), Treat_New=rep(NA,itnumber), 
               Treat_Ref=rep(NA,itnumber), Treat_Split=rep(NA,itnumber),
               Treat_New_CV=rep(NA,itnumber))


##############################################################################################################
#########------------------------------- simulation -----------------------------------------#################
##############################################################################################################
for(i in 1:itnumber){
  if(i %% 10000 == 0) cat("iteration", i, "of", itnumber,"\n")
  ##############################################################################################################
  # event_exp  - expected events without intervention
  # event_true - actual events; the same as event_exp for control-group, 
  #              partly different for intervention group (-> for the ones who benefit from intervention)

  # study sample
  data <- tibble(event_exp=rep(-1,N), event_true=rep(-1,N), biomarker=rep(-1,N), group_i=rep(-1,N),
                 bp_True=rep(-1,N), bp_New=rep(-1,N), bp_Ref=rep(-1,N), bp_Split=rep(-1,N))
  # another sample for external biomarker study
  data_E <- tibble(event_exp=rep(-1,n), biomarker=rep(-1,n))
  
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
  ## data for external biomarker study with n
  data_E$event_exp[1:n] <-  data_E$event_true <- rbinom(n,1,p_c)
  data_E$biomarker[data_E$event_exp==0] <- rlnorm(sum(data_E$event_exp==0), meanlog = meanlog_0, sdlog = sdlog_0)
  data_E$biomarker[data_E$event_exp==1] <- rlnorm(sum(data_E$event_exp==1), meanlog = meanlog_1, sdlog = sdlog_1) 
  
  #save current simulated data
  sim_data[[i]] <- data
  
  ####### Split Control 
  # split Control into "Training" for cut-off definition 
  # and "Test" for treatment estimation
  ind <- ind_matrix[i,]
  data$Split <- NA
  data$Split[data$group_i==0][ind[1:(n/2)]] <- "Training"
  data$Split[data$group_i==0][ind[(n/2+1):n]] <- "Test"
  ### data_Test with all individuals from intervention, only "Test" individuals from Control
  data_Test <- data[data$group_i==1 | data$Split=="Test",]

  ##########################################################################################
  ################ ROC: Cut-Off values in Control & Intervention ###########################
  roc_New <- roc(event_exp ~ biomarker, data = data, subset=(group_i==0), direction="<", quiet = TRUE)
  roc_Ref <- roc(event_exp ~ biomarker, data = data_E, direction="<", quiet = TRUE)
  roc_Split <- roc(event_exp ~ biomarker, data = data[data$Split=="Training",], direction="<", quiet = TRUE)

  ### biomarker Cut-Off for a sensitivity >=sen ######
  # biomarker Cut-Off due to control arm (New design) -> used for both arms afterwards
  CutOff_bio$New[i] <- max(roc_New$thresholds[roc_New$sensitivities>=sen])
  # true calculated biomarker Cut-Off for comparison
  CutOff_bio$True[i] <- exp(qnorm(1-sen,mean=meanlog_1, sd=sdlog_1))
  # biomarker Cut-Off due to an external sample (Reference design) for comparison
  CutOff_bio$Ref[i] <- max(roc_Ref$thresholds[roc_Ref$sensitivities>=sen])
  # biomarker Cut-Off due to sample split of control arm
  CutOff_bio$Split[i] <- max(roc_Split$thresholds[roc_Split$sensitivities>=sen])

  ### probability of events ##############################################################
  ## take biomarker Cut-Off defined due to new design for definition in C and I
  prob$C_bp_New[i] <- mean(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$New[i]]) 
  prob$I_bp_New[i] <- mean(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$New[i]]) 
  ## for comparison - using true Cut-Off
  prob$C_bp_True[i] <- mean(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$True[i]]) 
  prob$I_bp_True[i] <- mean(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$True[i]])
  ## for comparison - using Cut-Off derived from external sample (Reference design)
  prob$C_bp_Ref[i] <- mean(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$Ref[i]]) 
  prob$I_bp_Ref[i] <- mean(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$Ref[i]]) 
  ## for comparison - using Cut-Off derived from Sample Split
  prob$C_bp_Split[i] <- mean(data_Test$event_true[data_Test$group_i==0 & data_Test$biomarker>=CutOff_bio$Split[i]]) 
  prob$I_bp_Split[i] <- mean(data_Test$event_true[data_Test$group_i==1 & data_Test$biomarker>=CutOff_bio$Split[i]]) 
  # Treatment effect
  prob$Treat_True[i] <- prob$C_bp_True[i] - prob$I_bp_True[i]
  prob$Treat_New[i] <- prob$C_bp_New[i] - prob$I_bp_New[i]
  prob$Treat_Ref[i] <- prob$C_bp_Ref[i] - prob$I_bp_Ref[i] 
  prob$Treat_Split[i] <- prob$C_bp_Split[i] - prob$I_bp_Split[i] 
}

#### Analysis for New_CV - new design including cross-validation
for(i in 1:itnumber){
  data_C <- sim_data[[i]] %>% subset(group_i==0)
  data_I <- sim_data[[i]] %>% subset(group_i==1)
  
  roc_CV <- function(dat_C_train, dat_C_test, data_I, sen) {
    roc_CV <- roc(event_exp ~ biomarker, data = dat_C_train, quiet = TRUE, direction="<")
    cutoff_CV <- max(roc_CV$thresholds[roc_CV$sensitivities>=sen])
  
    prob_I_bp <- mean(data_I$event_true[data_I$biomarker>=cutoff_CV]) 
    prob_C_bp <- mean(dat_C_test$event_true[dat_C_test$biomarker>=cutoff_CV]) 
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
  CutOff_bio$New_CV[i] <- mean(sapply(fold_results, function(x) x$cutoff), na.rm=T)
  prob$C_bp_New_CV[i] <- mean(sapply(fold_results, function(x) x$prob_C), na.rm=T) 
  prob$I_bp_New_CV[i] <- mean(sapply(fold_results, function(x) x$prob_I), na.rm=T) 
  prob$Treat_New_CV[i] <- mean(sapply(fold_results, function(x) x$prob_Treat), na.rm=T)
  }

result <- list(CutOff_bio=CutOff_bio, prob=prob)
return(result)
}

################################################################################################################
################################################################################################################



