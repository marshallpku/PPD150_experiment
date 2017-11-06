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
  read_ExcelRange(paste0(data_dir, "Data_inputs(2).xlsx"), "inputs_singleValue_h") %>% 
  unclass()
  
# Table inputs
load(paste0(data_dir, "PPD_sampleData_PSERS.RData"))


#**************************************************
#                Modifying data                ####
#**************************************************

# Modifying decrement table

decrements <-
decrement.model %>% 
  mutate(qxr = qxr.super,
         planname = "PSERS") %>% 
  select(planname, ea, age, everything(), -qxr.early, -qxr.super) 

decrements


#**************************************************
#             Standarized data inputs          ####
#**************************************************

list_inputs <- list(
  inputs_singleValues = inputs_singleValues,
  decrements         = decrements,
  init_actives       = init_actives,
  init_retirees      = init_retirees,
  init_amort         = init_amort,
  init_unrecReturns  = init_unrecReturns.unadj
)


save(list_inputs, file = paste0(data.std_dir, "inputs_std_PSERS.RData"))


inputs_singleValues




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
   



















