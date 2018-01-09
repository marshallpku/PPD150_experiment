# Run control file for 150-plan model

# Outline
# Inputs: standardized model inputs. 
# Modeling process
# 1. Data preparation
#    - Description: 
#       - modifiy decrement tables
#       - create a complete salary table
#       - create matrices for intial members. 
#       - estimate age distribution of new entrants
#    - inputs: standardized model inputs
#    - outputs: modified inputs that can be used by the model
# 2. Demographics
#    - inputs: initial demographic
#    - outputs: 
#       - demo projection for current employees (including current-employee-turned retirees)
#       - demo projection for current retirees 
#       - demo projection for new employees (including new-employee-turned retirees)
# 3. Individual liabilities and costs 
#    - inputs: single-value inputs; benefits for initial retirees
#    - outputs:
#       - actives:  AL, NC, salary, PVFB, by start.year, ea, age
#       - retirees: AL, B, by start.year, retirement year, ea, age
#       - disability? 
# 4. Aggregate liabilities and costs
#    - inputs:  demographic projections, individual liabilities
#    - outputs: uncalibrated AL, NC, B, PVFB, salary for current employees, new employees and current retirees. 
# 5. Calibration
#    - inputs: single-value inputs, aggregate liablties and costs
#    - outputs: 
#       - calibrated AL, NC, B, PVFB, salary for current employees 
#       - calibrated AL, B for current beneficiaries. 
#       - ? Does new employees need calibration?
# 6. Combine results:
#    - inputs: calibrated results
#    - outputs: Total AL, PVFB, NC, B, salary, saved in ./Outputs_liab 
# 7. Investment scenarios 
# 8. Simulation of plan funding
# 9. Risk analysis


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
library(pdata)

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
ppd_id_largePlans <- c(9,  26, 83, 85,  125,
                       72, 84, 86, 140, 150,
                       10, 28, 78, 88,  108)
ppd_id_smallPlans <- setdiff(ppd_id_all, ppd_id_largePlans)

ppd_id_closed <-  c(3, 4, 24, 54, 55, 124, 154, 157)


#********************************************************************************                          
#                  Model: Liabilities and cash flow ####
#********************************************************************************

model_ppd_id  <-  ppd_id_largePlans
model_liabScn <- "A1"

for(model_ppd_id_ in model_ppd_id){

    # set output folder
    dir_out <- paste0(dir_outputs_liab, "liabScn_", model_liabScn, "/" )
    if(!dir.exists(dir_out)) dir.create(dir_out)

    # load plan data
    load(paste0(dir_data_std, "planData_std_", model_ppd_id_, ".RData"))
    planData_list <- get(paste0("planData_std_", model_ppd_id_ ))
    
    # load scenario data
    list_liabScn <- df_liabScn %>% filter(liabScn == model_liabScn) %>% unclass

    # combine data
    planData_list$inputs_singleValues <- 
      c(list_liabScn, planData_list$inputs_singleValues)


    # Derived values
    planData_list$inputs_singleValues$init.year <- planData_list$inputs_singleValues$fy_end
    planData_list$inputs_singleValues$range_age <- with(planData_list$inputs_singleValues, age_min:age_max)
    planData_list$inputs_singleValues$range_ea  <- with(planData_list$inputs_singleValues, ea_min:ea_max)

    # Force no_entrants = TRUE for closed plan
    if(planData_list$inputs_singleValues$ppd_id %in% ppd_id_closed) planData_list$inputs_singleValues$no_entrants <- TRUE

    source("./Model/Model_Master_liab.R")
}



# assign single values to working environment
# assign_parmsList(planData_list$inputs_singleValues , envir = environment())

# planData_list$decrements %<>% mutate(qxt = 0)
# planData_list$init_retirees %<>% mutate(nretirees = 0)
# planData_list$init_actives  %<>% mutate(nactives = 0)


#********************************************************************************                          
#                  Model: Investment and funding ####
#********************************************************************************

model_sim_liabScn <- "A1"

model_sim_returnScn <- "planAssumption"
#model_sim_returnScn <- "return75" ###
#model_sim_returnScn <- "lowReturn15y"
#model_sim_returnScn <- c("highVol","planAssumption")
#model_sim_returnScn <- c("return75", "lowReturn15y")

# model_sim_ppd_id <- ppd_id_largePlans 
# model_sim_ppd_id <- ppd_id_smallPlans 
# model_sim_ppd_id   <- ppd_id_smallPlans
model_sim_ppd_id <- ppd_id_all[-c(1:40)]

for(model_sim_returnScn_ in model_sim_returnScn){
    #model_sim_returnScn_ <- "lowReturn15y"
  
    for(model_sim_ppd_id_ in model_sim_ppd_id){
      # model_sim_ppd_id_  <- 9
      # model_sim_liabScn_ <- "A1"
  
    # set output folder
    dir_sim_out <- paste0(dir_outputs_sim, "simScn_", model_sim_liabScn,"_", model_sim_returnScn_, "/" )
    if(!dir.exists(dir_sim_out)) dir.create(dir_sim_out)
  
    # load liability data and plan data list
    load(paste0(dir_outputs_liab, "liabScn_", model_sim_liabScn, "/liab_", model_sim_liabScn, "_", model_sim_ppd_id_, ".RData")) # AggLiab loaded
    AggLiab$planData_list$inputs_singleValues$returnScn <- model_sim_returnScn_
    planData_list <- AggLiab$planData_list
   
    # load return scenario data
    returnScn_sim <- df_returnScn %>% filter(returnScn == model_sim_returnScn_) 
  
    # AggLiab$planData_list$init_amort_unadj
    # AggLiab$planData_list$inputs_singleValues
  
    source("./Model/Model_Master_sim.R")
    }
}




#********************************************************************************                          
#                             Misc ####
#********************************************************************************

# PPD_data %>% str













