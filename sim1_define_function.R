#set working directory
setwd("...")

# packages
#install.packages("ggplot2")
#install.packages("dplyr")
#install.packages("tibble")
#install.packages("pROC")
library(ggplot2)
library(dplyr)
library(tibble)
library(pROC)

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
# bn - biomarker negative --> subgroup that would not be recommended to intervention due to biomarker Cut-Off
# C - Control arm
# I - Intervention arm
# True - using the True calculated biomarker Cut-Off knowing the distribution of the biomarker
# New - using Cut-Off from Control group (New design)
# Ref - using Cut-Off from External sample (Reference design)

simu <- function(n, p_c, p_i, meanlog_0, meanlog_1, sdlog_0, sdlog_1, sen, itnumber){
N <- 2*n
# seed
set.seed(242)

########################## save results into #########################################################
#Biomarker Cut-Off)
CutOff_bio <- tibble(True=rep(NA,itnumber), New=rep(NA,itnumber), Ref=rep(NA,itnumber)) 
specificity <- tibble(C_True=rep(NA,itnumber), C_New=rep(NA,itnumber), C_Ref=rep(NA,itnumber),
                      I_True=rep(NA,itnumber), I_New=rep(NA,itnumber), I_Ref=rep(NA,itnumber))
sensitivity <- tibble(C_True=rep(NA,itnumber), C_New=rep(NA,itnumber), C_Ref=rep(NA,itnumber),
                      I_True=rep(NA,itnumber), I_New=rep(NA,itnumber), I_Ref=rep(NA,itnumber))
#estimated probabilities & treatment effect in the different biomarker-defined groups
prob <- tibble(C_bn_True=rep(NA,itnumber), C_bp_True=rep(NA,itnumber), 
               I_bn_True=rep(NA,itnumber), I_bp_True=rep(NA,itnumber),
               C_bn_New=rep(NA,itnumber), C_bp_New=rep(NA,itnumber), 
               I_bn_New=rep(NA,itnumber), I_bp_New=rep(NA,itnumber),
               C_bn_Ref=rep(NA,itnumber), C_bp_Ref=rep(NA,itnumber), 
               I_bn_Ref=rep(NA,itnumber), I_bp_Ref=rep(NA,itnumber),
               Treat_True=rep(NA,itnumber), Treat_New=rep(NA,itnumber), Treat_Ref=rep(NA,itnumber))
#groupsize of the groups that are defined due to the biomarker Cut-Offs
groupsize <- tibble(C_bn_True=rep(NA,itnumber), C_bp_True=rep(NA,itnumber), 
                    I_bn_True=rep(NA,itnumber), I_bp_True=rep(NA,itnumber),
                    C_bn_New=rep(NA,itnumber), C_bp_New=rep(NA,itnumber), 
                    I_bn_New=rep(NA,itnumber), I_bp_New=rep(NA,itnumber),
                    C_bn_Ref=rep(NA,itnumber), C_bp_Ref=rep(NA,itnumber), 
                    I_bn_Ref=rep(NA,itnumber), I_bp_Ref=rep(NA,itnumber))

##############################################################################################################
#########------------------------------- simulation -----------------------------------------#################
##############################################################################################################
for(i in 1:itnumber){
  if(i %% 100 == 0) cat("iteration", i, "of", itnumber,"\n")
  ##############################################################################################################
  # event_exp  - expected events without intervention
  # event_true - actual events; the same as event_exp for control-group, 
  #              partly different for intervention group (-> for the ones who benefit from intervention)

  # study sample
  data <- tibble(event_exp=rep(-1,N), event_true=rep(-1,N), biomarker=rep(-1,N), group_i=rep(-1,N),
                 bp_True=rep(-1,N), bp_New=rep(-1,N), bp_Ref=rep(-1,N))
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
  
  ##########################################################################################
  ################ ROC: Cut-Off values in Control & Intervention ###########################
  roc_New <- roc(event_exp ~ biomarker, data = data, subset=(group_i==0), direction="<")
  roc_Ref <- roc(event_exp ~ biomarker, data = data_E, direction="<")

  ### biomarker Cut-Off for a sensitivity >=sen ######
  # biomarker Cut-Off due to control arm (New design) -> used for both arm afterwards
  CutOff_bio$New[i] <- max(roc_New$thresholds[roc_New$sensitivities>=sen])
  # true calculated biomarker Cut-Off for comparison
  CutOff_bio$True[i] <- exp(qnorm(1-sen,mean=meanlog_1, sd=sdlog_1))
  # biomarker Cut-Off due to an external sample (Reference design) for comparison
  CutOff_bio$Ref[i] <- max(roc_Ref$thresholds[roc_Ref$sensitivities>=sen])
  ## specificity separate for control and for intervention 
  specificity$C_True[i] <- sum(data$event_exp==0 & data$biomarker<CutOff_bio$True[i] & data$group_i==0) / sum(data$event_exp==0 & data$group_i==0)
  specificity$C_New[i] <- sum(data$event_exp==0 & data$biomarker<CutOff_bio$New[i] & data$group_i==0) / sum(data$event_exp==0 & data$group_i==0)
  specificity$C_Ref[i] <- sum(data$event_exp==0 & data$biomarker<CutOff_bio$Ref[i] & data$group_i==0) / sum(data$event_exp==0 & data$group_i==0)
  specificity$I_True[i] <- sum(data$event_exp==0 & data$biomarker<CutOff_bio$True[i] & data$group_i==1) / sum(data$event_exp==0 & data$group_i==1)
  specificity$I_New[i] <- sum(data$event_exp==0 & data$biomarker<CutOff_bio$New[i] & data$group_i==1) / sum(data$event_exp==0 & data$group_i==1)
  specificity$I_Ref[i] <- sum(data$event_exp==0 & data$biomarker<CutOff_bio$Ref[i] & data$group_i==1) / sum(data$event_exp==0 & data$group_i==1)
  ## sensitivity separate for control and for intervention 
  sensitivity$C_True[i] <- sum(data$event_exp==1 & data$biomarker>=CutOff_bio$True[i] & data$group_i==0) / sum(data$event_exp==1 & data$group_i==0)
  sensitivity$C_New[i] <- sum(data$event_exp==1 & data$biomarker>=CutOff_bio$New[i] & data$group_i==0) / sum(data$event_exp==1 & data$group_i==0)
  sensitivity$C_Ref[i] <- sum(data$event_exp==1 & data$biomarker>=CutOff_bio$Ref[i] & data$group_i==0) / sum(data$event_exp==1 & data$group_i==0)
  sensitivity$I_True[i] <- sum(data$event_exp==1 & data$biomarker>=CutOff_bio$True[i] & data$group_i==1) / sum(data$event_exp==1 & data$group_i==1)
  sensitivity$I_New[i] <- sum(data$event_exp==1 & data$biomarker>=CutOff_bio$New[i] & data$group_i==1) / sum(data$event_exp==1 & data$group_i==1)
  sensitivity$I_Ref[i] <- sum(data$event_exp==1 & data$biomarker>=CutOff_bio$Ref[i] & data$group_i==1) / sum(data$event_exp==1 & data$group_i==1)
  ### Groupdefinition bp/bn due to the 3 Cut-Offs
  data$bp_True[data$biomarker>=CutOff_bio$True[i]] <- rep(1,sum(data$biomarker>=CutOff_bio$True[i]))
  data$bp_True[data$biomarker<CutOff_bio$True[i]] <- rep(0,sum(data$biomarker<CutOff_bio$True[i]))  
  data$bp_New[data$biomarker>=CutOff_bio$New[i]] <- rep(1,sum(data$biomarker>=CutOff_bio$New[i]))
  data$bp_New[data$biomarker<CutOff_bio$New[i]] <- rep(0,sum(data$biomarker<CutOff_bio$New[i]))
  data$bp_Ref[data$biomarker>=CutOff_bio$Ref[i]] <- rep(1,sum(data$biomarker>=CutOff_bio$Ref[i]))
  data$bp_Ref[data$biomarker<CutOff_bio$Ref[i]] <- rep(0,sum(data$biomarker<CutOff_bio$Ref[i]))
  
  ### probability of events ##############################################################
  ## take biomarker Cut-Off defined due to new design for definition in C and I
  prob$C_bn_New[i] <- mean(data$event_true[data$group_i==0 & data$biomarker<CutOff_bio$New[i]])
  prob$C_bp_New[i] <- mean(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$New[i]]) 
  prob$I_bn_New[i] <- mean(data$event_true[data$group_i==1 & data$biomarker<CutOff_bio$New[i]]) 
  prob$I_bp_New[i] <- mean(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$New[i]]) 
  ## for comparison - using true Cut-Off
  prob$C_bn_True[i] <- mean(data$event_true[data$group_i==0 & data$biomarker<CutOff_bio$True[i]])
  prob$C_bp_True[i] <- mean(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$True[i]]) 
  prob$I_bn_True[i] <- mean(data$event_true[data$group_i==1 & data$biomarker<CutOff_bio$True[i]])
  prob$I_bp_True[i] <- mean(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$True[i]]) 
  ## for comparison - using Cut-Off derived from external sample (Reference design)
  prob$C_bn_Ref[i] <- mean(data$event_true[data$group_i==0 & data$biomarker<CutOff_bio$Ref[i]])
  prob$C_bp_Ref[i] <- mean(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$Ref[i]]) 
  prob$I_bn_Ref[i] <- mean(data$event_true[data$group_i==1 & data$biomarker<CutOff_bio$Ref[i]])
  prob$I_bp_Ref[i] <- mean(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$Ref[i]]) 
  # Treatment effect
  prob$Treat_True[i] <- prob$C_bp_True[i] - prob$I_bp_True[i]
  prob$Treat_New[i] <- prob$C_bp_New[i] - prob$I_bp_New[i] 
  prob$Treat_Ref[i] <- prob$C_bp_Ref[i] - prob$I_bp_Ref[i] 
  
  ### groupsize of the defined biomarker positive & biomarker negative groups ######################
  groupsize$C_bn_New[i] <- length(data$event_true[data$group_i==0 & data$biomarker<CutOff_bio$New[i]]) 
  groupsize$C_bp_New[i] <- length(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$New[i]])
  groupsize$I_bn_New[i] <- length(data$event_true[data$group_i==1 & data$biomarker<CutOff_bio$New[i]]) 
  groupsize$I_bp_New[i] <- length(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$New[i]])
  ## with true Cut-Off
  groupsize$C_bn_True[i] <- length(data$event_true[data$group_i==0 & data$biomarker<CutOff_bio$True[i]])
  groupsize$C_bp_True[i] <- length(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$True[i]]) 
  groupsize$I_bn_True[i] <- length(data$event_true[data$group_i==1 & data$biomarker<CutOff_bio$True[i]])
  groupsize$I_bp_True[i] <- length(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$True[i]])
  ## with Cut-Off from external sample (Reference design)
  groupsize$C_bn_Ref[i] <- length(data$event_true[data$group_i==0 & data$biomarker<CutOff_bio$Ref[i]])
  groupsize$C_bp_Ref[i] <- length(data$event_true[data$group_i==0 & data$biomarker>=CutOff_bio$Ref[i]]) 
  groupsize$I_bn_Ref[i] <- length(data$event_true[data$group_i==1 & data$biomarker<CutOff_bio$Ref[i]])
  groupsize$I_bp_Ref[i] <- length(data$event_true[data$group_i==1 & data$biomarker>=CutOff_bio$Ref[i]])
}
result <- list(CutOff_bio=CutOff_bio, specificity=specificity, sensitivity=sensitivity, prob=prob, groupsize=groupsize)
return(result)
}

################################################################################################################
################################################################################################################



