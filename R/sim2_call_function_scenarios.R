#load function
source("R/sim1_define_function.R")

##################################################################################################################

### call function "simu" for the different simulation scenarios

###############################################################################
############### biomarker --- scenarios Bio1-Bio9 --- #########################
# save into
results_biomarker <- vector("list",9)
j <- 1
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=3.3, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 2
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=3.5, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 3
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=3.8, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 4
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 5
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.3, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 6
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.5, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 7
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.8, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 8
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=5.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 9
results_biomarker[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=6.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
### save
save(file="results/results_biomarker.RData", list="results_biomarker")


############## eventrate --- scenarios Er1-Er7 --- ############################
# save into
results_eventrate <- vector("list",7)
j <- 1
results_eventrate[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 2
results_eventrate[[j]] <- simu(n=750, p_c=0.2, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 3
results_eventrate[[j]] <- simu(n=750, p_c=0.25, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 4
results_eventrate[[j]] <- simu(n=750, p_c=0.3, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 5
results_eventrate[[j]] <- simu(n=750, p_c=0.4, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 6
results_eventrate[[j]] <- simu(n=750, p_c=0.5, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 7
results_eventrate[[j]] <- simu(n=750, p_c=0.6, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)

### save
save(file="results/results_eventrate.RData", list="results_eventrate")


############# samplesize --- scenarios Sam1-Sam8 --- ##########################
# save into
results_samplesize <- vector("list",8)
j <- 1
results_samplesize[[j]] <- simu(n=150, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 2
results_samplesize[[j]] <- simu(n=200, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 3
results_samplesize[[j]] <- simu(n=250, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 4
results_samplesize[[j]] <- simu(n=300, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 5
results_samplesize[[j]] <- simu(n=400, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 6
results_samplesize[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 7
results_samplesize[[j]] <- simu(n=1500, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 8
results_samplesize[[j]] <- simu(n=5000, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
### save
save(file="results/results_samplesize.RData", list="results_samplesize")

############# sensitivity --- scenarios Sen1-Sen6 --- #########################
# save into
results_sensitivity <- vector("list",6)
j <- 1
results_sensitivity[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.95, itnumber=20000)
j <- 2
results_sensitivity[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.9, itnumber=20000)
j <- 3
results_sensitivity[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.8, itnumber=20000)
j <- 4
results_sensitivity[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.7, itnumber=20000)
j <- 5
results_sensitivity[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.5, itnumber=20000)
j <- 6
results_sensitivity[[j]] <- simu(n=750, p_c=0.15, p_i=0.1, meanlog_0=3.0, meanlog_1=4.0, sdlog_0=0.5, sdlog_1=0.5, sen=0.3, itnumber=20000)
#### save
save(file="results/results_sensitivity.RData", list="results_sensitivity")
