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
options(dplyr.print_min = 60) # default is 10

source("Functions.R")



#********************************************************************************                          
#                  Global settings ####
#********************************************************************************

dir_data_std        <- "./Inputs_Data_std/planData_std/"
dir_outputs_liab    <- "./Outputs_liab/"
dir_outputs_funding <- "./Outputs_funding/"



#********************************************************************************                          
#                  Model: Liabilities and cash flow ####
#********************************************************************************

model_ppd_id <- 31
liabScn      <- "a" 

dir_out <- paste0(dir_outputs_liab, "liabScn_", liabScn, "/" )
if(!dir.exists(dir_out)) dir.create(dir_out)

load(paste0(dir_data_std, "planData_std_", model_ppd_id, ".RData"))

planData_list <- get(paste0("planData_std_", model_ppd_id ))

# Temp: global/model variables
planData_list$inputs_singleValues$liabScn <- liabScn
planData_list$inputs_singleValues$ncore   <- 6
planData_list$inputs_singleValues$nsim    <- 2000
planData_list$inputs_singleValues$nyear   <- 30
planData_list$inputs_singleValues$age_min <- 20
planData_list$inputs_singleValues$age_max <- 110
planData_list$inputs_singleValues$ea_min  <- 20
planData_list$inputs_singleValues$ea_max  <- 74
planData_list$inputs_singleValues$no_entrants <- FALSE
planData_list$inputs_singleValues$wf_growth   <- 0
planData_list$inputs_singleValues$model_term  <- FALSE

# Derived values
planData_list$inputs_singleValues$init.year <- planData_list$inputs_singleValues$fy_end
planData_list$inputs_singleValues$range_age <- with(planData_list$inputs_singleValues, age_min:age_max)
planData_list$inputs_singleValues$range_ea  <- with(planData_list$inputs_singleValues, ea_min:ea_max)



# assign single values to working environment
# assign_parmsList(planData_list$inputs_singleValues , envir = environment())


# planData_list$decrements %<>% mutate(qxt = 0)
# planData_list$init_retirees %<>% mutate(nretirees = 0)
# planData_list$init_actives  %<>% mutate(nactives = 0)


#********************************************************************************                          
#                  Model: Investment and funding ####
#********************************************************************************


source("./Model/Model_Master_liab.R")







