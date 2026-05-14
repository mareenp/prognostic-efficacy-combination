#load function
source("../sim1b_define_function_CV.R")

library(future.apply)
library(parallel)

options(future.rng.onMisuse = "ignore")

### --------------- parallelization with future_apply --------------------- ###
# function to run one simulation scenario
run_scenario <- function(scn){
  res_scn <- simu(n=scn$n, 
                  p_c=scn$p_c, 
                  p_i=scn$p_i, 
                  meanlog_0=scn$meanlog_0, 
                  meanlog_1=scn$meanlog_1, 
                  sdlog_0=scn$sdlog_0, 
                  sdlog_1=scn$sdlog_1, 
                  sen=scn$sen, 
                  itnumber=scn$itnumber)
  
  return(res_scn)
}


############# samplesize --- scenarios Sam1-Sam8 --- ##########################
scenarios <- data.frame(matrix(c(c(n=150, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),   
                                 c(n=200, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=250, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=300, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=400, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=1500, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=5000, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)),
                               nrow = 8, byrow = T))
colnames(scenarios) <- c("n", "p_c", "p_i", "meanlog_0", "meanlog_1", 
                         "sdlog_0", "sdlog_1", "sen", "itnumber")

# parallelize scenarios
plan(multisession, workers=detectCores()*.75)
results_samplesize <- future_lapply(1:nrow(scenarios), function(i) run_scenario(scn=scenarios[i,]), future.seed = F)
plan(sequential)
### save
save(file="results_samplesize.RData", list="results_samplesize")


############# sensitivity --- scenarios Sen1-Sen6 --- #########################
scenarios <- data.frame(matrix(c(c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.9, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.8, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.7, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.5, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.3, itnumber=20000)),
                               nrow = 6, byrow = T))
colnames(scenarios) <- c("n", "p_c", "p_i", "meanlog_0", "meanlog_1", 
                         "sdlog_0", "sdlog_1", "sen", "itnumber")

# parallelize scenarios
plan(multisession, workers=detectCores()*.75)
results_sensitivity <- future_lapply(1:nrow(scenarios), function(i) run_scenario(scn=scenarios[i,]), future.seed = F)
plan(sequential)
#### save
save(file="results_sensitivity.RData", list="results_sensitivity")

############## eventproportion --- scenarios Er1-Er7 --- ############################

scenarios <- data.frame(matrix(c(c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.2, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.25, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.3, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.4, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.5, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.6, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)),
                               nrow = 7, byrow = T))
colnames(scenarios) <- c("n", "p_c", "p_i", "meanlog_0", "meanlog_1", 
                         "sdlog_0", "sdlog_1", "sen", "itnumber")

# parallelize scenarios
plan(multisession, workers=detectCores()*.75)
results_eventproportion <- future_lapply(1:nrow(scenarios), function(i) run_scenario(scn=scenarios[i,]), future.seed = F)
plan(sequential)
### save
save(file="results_eventproportion.RData", list="results_eventproportion")

############### biomarker --- scenarios Bio1-Bio9 --- #########################

scenarios <- data.frame(matrix(c(c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=3.3, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=3.5, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=3.8, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.3, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.5, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.8, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=5.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000),
                                 c(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=6.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)),
                               nrow = 9, byrow = T))
colnames(scenarios) <- c("n", "p_c", "p_i", "meanlog_0", "meanlog_1", 
                         "sdlog_0", "sdlog_1", "sen", "itnumber")

# parallelize scenarios
plan(multisession, workers=detectCores()*.75)
results_biomarker <- future_lapply(1:nrow(scenarios), function(i) run_scenario(scn=scenarios[i,]), future.seed = F)
plan(sequential)

### save
save(file="results_biomarker.RData", list="results_biomarker")
