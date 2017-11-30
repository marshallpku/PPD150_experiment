
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

file_Scn_liab <- "./Model/Scn_liab.xlsx"
df_liabScn    <- read_ExcelRange(file_Scn_liab, sheet = "liabScn" )

file_Scn_return <- "./Model/Scn_return.xlsx"
df_returnScn    <- read_ExcelRange(file_Scn_return, sheet = "returnScn" )



file_ppd <- "DataPPD.RData"
load(paste0(dir_data_ppd, file_ppd))

ppd_id_all        <- PPD_data$ppd_id
ppd_id_largePlans <- c(9, 26, 83, 85, 125,
                       72, 84, 86, 140, 150,
                       10, 28, 78, 88, 108) 
ppd_id_smallPlans <- setdiff(ppd_id_all, ppd_id_largePlans)

# plans with problematic results
# 31,73, 124, 157 asset_year == 0 
# 153, 173 deferred return = 0 
 


#********************************************************************************                          
#                  Check results ####
#********************************************************************************

load(paste0(dir_outputs_sim, "simScn_A1_planAssumption/sim_A1_planAssumption_124.RData"))


outputs_list$df_results %>% filter(sim == -1) %>% select(ppd_id, planName, State, sim, AL, FR_MA, ERC_PR, NC_PR, SC_PR, PR)
outputs_list$df_results %>% filter(sim == 0)  %>% select(ppd_id, planName, State, sim, AL, FR_MA, ERC_PR, NC_PR, SC_PR, PR, Amort_basis)
outputs_list$df_results %>% filter(sim == 1)  %>% select(ppd_id, planName, State, sim, AL, FR_MA, ERC_PR, NC_PR, PR)

outputs_list$df_riskMeasures




