# Master file for simulation of a single plan






#*********************************************************************************************************
#  Create decrement tables ####
#*********************************************************************************************************

source("./Model/Model_Decrements.R")
decrement_model <- get_decrements()
#decrement_model

# To do:
 # decrements for terms and disb


#*********************************************************************************************************
#  Prepare plan data ####
#*********************************************************************************************************
source("./Model/Model_PrepData.R")

salary        <- get_salary_proc()
benefit       <- get_benefit()
init_pop      <- get_initPop()
entrants_dist <- get_entrantsDist()



#*********************************************************************************************************
#  Demographics ####
#*********************************************************************************************************
gc()
source("./Model/Model_Demographics.R")
pop <- get_Population()



#*********************************************************************************************************
#  Individual actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
gc()
source("./Model/Model_IndivLiab.R")
liab <- get_indivLiab()



#*********************************************************************************************************
# Aggregate actuarial liabilities, normal costs and benenfits ####
#*********************************************************************************************************
source("./Model/Model_AggLiab.R")
AggLiab <- get_AggLiab()


# AggLiab$active[1,]
# AggLiab$la[1,]
# AggLiab$term[1,]


#*********************************************************************************************************
# Calibration ####
#*********************************************************************************************************
source("./Model/Model_Calibration.R")
AggLiab <- get_calibAggLiab()


#*********************************************************************************************************
# Investment Returns ####
#*********************************************************************************************************
source("./Model/Model_InvReturns_Temp.R")
i.r <- gen_returns()


#*********************************************************************************************************
# Simulation ####
#*********************************************************************************************************
source("./Model/Model_Sim.R")
penSim_results <- run_sim()




#*********************************************************************************************************
# Saving results ####
#*********************************************************************************************************
outputs_list <- list(planData_list = planData_list, 
                     results     = penSim_results)



#*********************************************************************************************************
# 8. Showing results ####
#*********************************************************************************************************

var_display.basic <- c("sim", "year", "FR_MA", "MA","AA", "AL", "NC", "SC","ADC","EEC",  "ERC", "C", "NC_PR", "ERC_PR", 
                       "AL.act","AL.act.v", "AL.la", "PVFB", "B", "Amort_basis", "i.r")

var_display.demo <- c("sim", "year", "nactives", "nla", "nterms")

penSim_results %>% filter(sim == -1) %>% select(one_of(var_display.basic)) %>% print
penSim_results %>% filter(sim == 0) %>% select(one_of(var_display.basic)) %>% print
penSim_results %>% filter(sim == -1) %>% select(one_of(var_display.demo))  %>% print


# Issues to fix:
 # Low growth in AL
 # Losses in sim 0, FR_MA does not rise close to 100% after 24 years.




