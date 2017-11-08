# Read make-shift raw data (based on PSERS) and convert it to standardized format 

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



#**************************************************
#                Loading data                  ####
#**************************************************

data_dir     <- "./Inputs_Data_std/"
data.std_dir <- "./Inputs_Data_std/"


# Single-value inputs
inputs_singleValues <- 
  read_ExcelRange(paste0(data_dir, "Data_std_inputs.xlsx"), "inputs_singleValue_h") %>% 
  unclass()
  
# Table inputs
load(paste0(data_dir, "PPD_sampleData_PSERS.RData"))




#**************************************************
#                Modifying data                ####
#**************************************************

# Modifying decrement table
decrements <-
decrement.model %>% 
  mutate(ppd_id = 93,
         ppd_planname = "93_PA_PA_PSERS",
         qxr = qxr.super,
         planname = "PSERS") %>% 
  ungroup %>% 
  select(ppd_id, ppd_planname, ea, age, everything(), -qxr.early, -qxr.super, -planname) 


# initial actives
  init_actives %<>% 
    mutate(ppd_id = 93,
           ppd_planname = "93_PA_PA_PSERS") %>% 
    ungroup %>% 
    select(ppd_id, ppd_planname, everything(), -planname)
   

# initial retirees
  init_retirees %<>% 
    filter(age >= 21) %>% 
    mutate(ppd_id = 93,
           ppd_planname = "93_PA_PA_PSERS") %>% 
    ungroup %>% 
    select(ppd_id, ppd_planname, everything(), -planname)
  
# initial amortization
  init_amort_unadj <- 
    init_amort %>% 
    mutate(ppd_id = 93,
           ppd_planname = "93_PA_PA_PSERS") %>% 
    ungroup %>% 
    select(ppd_id, ppd_planname, everything())

# initial unrecognized earnings
  init_unrecReturns_unadj <- 
    init_unrecReturns.unadj %>% 
    mutate(ppd_id = 93,
           ppd_planname = "93_PA_PA_PSERS") %>% 
    ungroup %>% 
    select(ppd_id, ppd_planname, everything())  

# salary growth rates
  salgrowth %<>% 
    mutate(ppd_id = 93,
           ppd_planname = "93_PA_PA_PSERS") %>% 
    ungroup %>% 
    select(ppd_id, ppd_planname, everything())
  

  

#**************************************************
#             Standarized data inputs          ####
#**************************************************

planData_93_PA_PA_PSERS <- 
list(
  inputs_singleValues = inputs_singleValues,
  decrements          = decrements,
  salgrowth           = salgrowth,  
  init_actives        = init_actives,
  init_retirees       = init_retirees,
  init_amort_unadj         = init_amort_unadj,
  init_unrecReturns_unadj  = init_unrecReturns_unadj
)

save(planData_93_PA_PA_PSERS, file = paste0(data.std_dir, "planData_93_PA_PA_PSERS.RData"))

#inputs_singleValues


# Structure of structure of standardized 
 # One standardized data file for each plan:
  # Naming rule: "planData_" + PPD name
  # example: planData_9_CA_CA-CALPERS.RData
 # Each data file contains 1 data list, the list has the same name as the data file
 # The data list contains the following elements
   # 1. inputsSingleValues
   # 2. decrements
   # 3. init_actives
   # 4. init_retirees
   # 5. init_amort
   # 6. init_unrecReturns
   
# Format of standardized inputs
 # Notes:N/A cells filled with 0
 
 # 1. inputsSingleValues
 
 # 2. decrements
   # vars:
    # ppd_id
    # ppd_planname
    # ea:  20~74
    # age: 20~110
    # qxm.pre
    # qxm.post
    # qxm.d
    # qxt
    # qxr
    # qxd

  # 3. init_actives
   # vars:
    # ppd_id
    # ppd_planname
    # ea: 20~74
    # age:20~74
    # nactives
    # salary
 
 # 4. init_retirees
   # vars:
    # ppd_id
    # ppd_planname
    # age:21~110  # starting with 21 because need to assume 
    # nretirees
    # benefit
 
 # 5. init_amort
   # vars:
    # ppd_id
    # ppd_planname
    # balance
    # year.remaining
    # year.est (opitional)
    # init.amount (opitional)
    # init.period (opitional)

 # 6. init_unrecReturns
   # ppd_id
   # ppd_planname
   # year
   # DeferredReturn

# 
# init_actives
# init_retirees
# init_amort
# init_unrecReturns.unadj
# 


























