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

salary       <- get_salary_proc()
benefit      <- get_benefit()
init_pop     <- get_initPop()
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
source("PSERS_Model_IndivLiab.R")
gc()

liab <- get_indivLab()




