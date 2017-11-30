



#********************************************************************************
#                            Packages and tools ####
#********************************************************************************

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library(zoo)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
# library(xlsx)
library("btools")
options(dplyr.print_min = 60) # default is 10

source("Functions.R")



#********************************************************************************                          
#                  Global settings ####
#********************************************************************************

dir_data_ppd        <- "./Inputs_Data_PPD/"
dir_data_std        <- "./Inputs_Data_std/planData_std/"
dir_outputs_liab    <- "./Outputs_liab/"
dir_outputs_sim     <- "./Outputs_sim/"


file_ppd <- "DataPPD.RData"
load(paste0(dir_data_ppd, file_ppd))

ppd_id_all        <- PPD_data$ppd_id
ppd_id_largePlans <- c(9, 26, 83, 85, 125,
                       72, 84, 86, 140, 150,
                       10, 28, 78, 88, 108) 
ppd_id_smallPlans <- setdiff(ppd_id_all, ppd_id_largePlans)

PPD_data$MAV_ppd %>% sum

#********************************************************************************                          
#                             Loading files                                  ####
#********************************************************************************

liabScn  <- "A1"
#returnScn <- "planAssumption"
returnScn <- "return75"
#returnScn <- "lowReturn15y"
#returnScn <- "highVol"


# Define file names
fileNames <- paste0(dir_outputs_sim, "simScn_",liabScn, "_", returnScn,"/sim_", liabScn, "_", returnScn, "_", ppd_id_all, ".RData")

var_select <- c("ppd_id", "planName", "plantype", "State", "liabScn", "returnScn", "sim", "year", "AL", "MA", "AA", "FR", 
                "AL.act", "AL.la", "NC", "SC", "ERC", "EEC",  "ADC", "B", "nactives", "nla", "i", "i.r", "PR")

# Loading all data files into a list
results_allPlans <- llply(fileNames, function(x) {load(x); data_list <- outputs_list$df_results %>% select(one_of(var_select))}) %>% bind_rows()
# names(results_allPlans) <- ppd_id_all 


results_sumPPD <- 
  results_allPlans %>% 
  filter(year >= 2016) %>%  #, ppd_id %in% ppd_id_smallPlans) %>% 
  group_by(sim, year) %>% 
  summarise(AL = sum(AL, na.rm = TRUE),
            MA = sum(MA, na.rm = TRUE),
            AA = sum(AA, na.rm = TRUE),
            NC = sum(NC, na.rm = TRUE),
            SC = sum(SC, na.rm = TRUE),
            ERC= sum(ERC,na.rm = TRUE),
            EEC= sum(EEC,na.rm = TRUE),
            ADC= sum(ADC,na.rm = TRUE),
            B  = sum(B,  na.rm = TRUE),
            PR = sum(PR, na.rm = TRUE),
            nactives = sum(nactives, na.rm = TRUE),
            nla      = sum(nla, na.rm = TRUE)) %>% 
  mutate(ppd_id = 0,
         planName = "sum PPD",
         plantype = "sumPPD",
         State    = "US",
         liabScn = liabScn,
         returnScn = returnScn,
         year = year + 1)
 

############################################## 
# results_sumPPD %>% filter(sim == 0) %>% 
#   mutate(FR_MA     = 100 * MA / AL,
#          ERC_PR    = 100 * ERC / PR) %>% 
#   select(year, FR_MA, ERC_PR)
# 
# 
# x <- results_allPlans %>% filter(sim == 6, ppd_id == 9) %>% 
#   mutate(FR_MA     = 100 * MA / AL,
#          ERC_PR    = 100 * ERC / PR) %>% 
#   select(year, FR_MA, ERC_PR, i.r, i)
# x
# x$i.r %>% get_geoReturn()
# 
# results_allPlans %>% 
#   group_by(ppd_id, sim) %>% 
#   summarise(geoReturn = get_geoReturn(i.r)) %>% 
#   group_by(ppd_id) %>% 
#   summarise(geoReturn_mean = mean(geoReturn),
#             geoReturn_med  = median(geoReturn))
#   
# 
# 
# 
# 
# PPD_data$AL_ppd  %>% sum
# PPD_data$MAV_ppd %>% sum
# PPD_data[PPD_data$ppd_id %in% ppd_id_largePlans, "MAV_ppd"] %>% sum
# 
# 
# df_large <- results_allPlans %>% filter(sim == 0, ppd_id %in% ppd_id_largePlans, year == 2017)
# df_small <- results_allPlans %>% filter(sim == 0, ppd_id %in% ppd_id_smallPlans, year == 2017)
# 
# df_large$AL %>% sum
# df_small$AL %>% sum
# 
# df_large$MA %>% sum
# df_small$MA %>% sum
# 
# results_allPlans %>% filter(sim == 1, year == 2017)

###################################################

results_allPlans %<>% bind_rows(results_allPlans, results_sumPPD)

riskMeasure <- results_allPlans  %>% 
  #results_sumPPD %>% 
  filter(sim >= 0, year >=2016) %>%   
  select(ppd_id,
         planName,
         plantype,
         State,
         liabScn,
         returnScn,
         sim, year, AL, MA, ERC, PR) %>% 
  group_by(ppd_id, sim) %>% 
  mutate(FR_MA     = 100 * MA / AL,
         ERC_PR    = 100 * ERC / PR,
         FR40less  = cumany(FR_MA <= 40),
         FR60less  = cumany(FR_MA <= 60),
         FR100more = FR_MA >= 100,
         ERC_high  = cumany(ERC_PR >= 40), 
         ERC_hike  = cumany(na2zero(ERC_PR - lag(ERC_PR, 5) >= 10)),
         year = year + 1) %>% 
  group_by(ppd_id, year) %>% 
  summarize(
            liabScn   = unique(liabScn),
            returnScn = unique(returnScn),
            FR40less   = 100 * sum(FR40less, na.rm = T)/n(),
            FR60less   = 100 * sum(FR40less, na.rm = T)/n(),
            FR100more  = 100 * sum(FR100more,na.rm = T)/n(),
            ERC_high   = 100 * sum(ERC_high, na.rm = T)/n(),
            ERC_hike   = 100 * sum(ERC_hike, na.rm = T)/n(),
            
            FR.q10   = quantile(FR_MA, 0.1,  na.rm = T),
            FR.q25   = quantile(FR_MA, 0.25, na.rm = T),
            FR.q50   = quantile(FR_MA, 0.5,  na.rm = T),
            FR.q75   = quantile(FR_MA, 0.75, na.rm = T),
            FR.q90   = quantile(FR_MA, 0.9,  na.rm = T),
            
            ERC_PR.q10 = quantile(ERC_PR, 0.1,  na.rm = T),
            ERC_PR.q25 = quantile(ERC_PR, 0.25, na.rm = T),
            ERC_PR.q50 = quantile(ERC_PR, 0.5,  na.rm = T),
            ERC_PR.q75 = quantile(ERC_PR, 0.75, na.rm = T),
            ERC_PR.q90 = quantile(ERC_PR, 0.9,  na.rm = T),
            
            planName = unique(planName),
            plantype = unique(plantype),
            State  = unique(State)
            
  ) %>% 
  ungroup()

# riskMeasure %>% filter(ppd_id == 1)
# riskMeasure %>% filter(ppd_id == 0)

results_allPlans_det <- 
results_allPlans %>% filter(sim == 0 ) %>% 
  mutate(FR_MA = 100 * MA / AL,
         ERC_PR    = 100 * ERC / PR) 


assign(paste0("reportData_list_", liabScn, "_", returnScn), list(riskMeasure = riskMeasure, results_det = results_allPlans_det))


save(list = paste0("reportData_list_", liabScn, "_", returnScn), 
     file = paste0("./Analysis/reportData_", liabScn, "_", returnScn, ".RData"))










