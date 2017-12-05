# Master file for simulation of a single plan


#*********************************************************************************************************
#  Create decrement tables ####
#*********************************************************************************************************

source("./Model/Model_Decrements.R")
decrement_model <- get_decrements()
#decrement_model


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


save(AggLiab, file = paste0(dir_out, "liab_", model_liabScn, "_", planData_list$inputs_singleValues$ppd_id, ".RData" ) )


# #*********************************************************************************************************
# # Investment Returns ####
# #*********************************************************************************************************
# source("./Model/Model_InvReturns_Temp.R")
# i.r <- gen_returns()
# 
# 
# #*********************************************************************************************************
# # Simulation ####
# #*********************************************************************************************************
# source("./Model/Model_Sim.R")
# penSim_results <- run_sim()
# 
# 
# 
# 
# #*********************************************************************************************************
# # Saving results ####
# #*********************************************************************************************************
# outputs_list <- list(planData_list = planData_list,
#                      results     = penSim_results)
# 
# 
# 
# #*********************************************************************************************************
# # 8. Showing results ####
# #*********************************************************************************************************
# 
# var_display.basic <- c("sim", "year", "FR_MA", "MA","AA", "AL", "NC", "SC","ADC","EEC",  "ERC", "C", "NC_PR", "ERC_PR",
#                        "AL.act","AL.act.v", "AL.la", "PVFB", "B", "Amort_basis", "i.r")
# 
# var_display.demo <- c("sim", "year", "nactives", "nla", "nterms")
# 
# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display.basic)) %>% print
# penSim_results %>% filter(sim == 0)  %>% select(one_of(var_display.basic)) %>% print
# penSim_results %>% filter(sim == -1) %>% select(one_of(var_display.demo))  %>% print
# 
# 
# # Issues to fix:
# 
# 
# #*********************************************************************************************************
# # Showing results ####
# #*********************************************************************************************************
# # save(outputs_list, file = paste0("./Outputs_funding/outputs_",planData_list$inputs_singleValues$ppd_id, ".R" ) )
# 
# 
# #*********************************************************************************************************
# # Risk measures ####
# #*********************************************************************************************************
# 
# df_all.stch <- penSim_results  %>%
#   filter(sim >= 0, year <= 2046)
# 
# df_all.stch %<>%
#   select(sim, year, AL, MA, EEC, PR, ERC_PR) %>%
#   group_by(sim) %>%
#   mutate(FR_MA     = 100 * MA / AL,
#          FR40less  = cumany(FR_MA <= 40),
#          FR100more  = cumany(FR_MA >= 100),
#          FR100more2 = FR_MA >= 100,
#          ERC_high  = cumany(ERC_PR >= 50),
#          ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10))) %>%
#   group_by(year) %>%
#   summarize(FR40less = 100 * sum(FR40less, na.rm = T)/n(),
#             FR100more = 100 * sum(FR100more, na.rm = T)/n(),
#             FR100more2= 100 * sum(FR100more2, na.rm = T)/n(),
#             ERC_high = 100 * sum(ERC_high, na.rm = T)/n(),
#             ERC_hike = 100 * sum(ERC_hike, na.rm = T)/n(),
# 
#             FR.q10   = quantile(FR_MA, 0.1,na.rm = T),
#             FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
#             FR.q50   = quantile(FR_MA, 0.5, na.rm = T),
#             FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
#             FR.q90   = quantile(FR_MA, 0.9, na.rm = T),
# 
#             ERC_PR.q10 = quantile(ERC_PR, 0.1, na.rm = T),
#             ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
#             ERC_PR.q50 = quantile(ERC_PR, 0.5, na.rm = T),
#             ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
#             ERC_PR.q90 = quantile(ERC_PR, 0.9, na.rm = T)
# 
# 
#   ) %>%
#   ungroup()
# 
# df_all.stch

# planData_list$decrements %>% head



