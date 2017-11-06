


#**************************************************
# Packages and functions        ####
#**************************************************
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
source("Functions_DataTools.R")

#********************************************************************************
#                   Notes for Data Processing                                ####
#********************************************************************************

# Goal
  # Data collected from the large plans will be used for
    # a) data inputs to the those plans in the PPD150 model. 
    # b) analyzing the differences in plan characteristics between different plan types(general, safety, teachers)
    # c) constructing prototypical inputs for different plan types, which will be used for constucting inputs for 
    #    smaller plans in the PPD 150 model. 


# 1. Number of actives schedule and salary schedule
 # Spread cells to the specified ranges. 
 # Save as a data frame with the following variables:
 #   planname, plantype, filled,  ea, age, yos, nactives, salary
 # Save the original grouped data (filled = TRUE) and the filled-in data (filled = TRUE)
 # Use the standard functions to spread the cells. 


# 2. Retirees schedule
 # Data should include all types of beneficiaries. 
 # Spread cells to the specified ranges
 # Save as a data frame with the following variables:
  # planname, plantype, filled, age, nactives, salary
 # Save the original grouped data (filled = TRUE) and the filled-in data (filled = TRUE)
 # Use the standard functions to spread the cells. 
 # Changes needed in the xlsx files:
  # add benperiod annual, name_N, name_V
  # agegrp, age.cell,
  # N,V


# 3. Salary growth schedule
 # Output should be in one of the following formats:
   # a) By yos: 0-54,  1y interval 
   # b) By age: 20-74, 1y interval 
   # c) data frame: ea 20-74, age 20-74 (CA PERF only)
 # All output data frames should include the following variables: "planname", "SalaryGrowthType"


# 4. Term Rates 
# Output should be in one of the following formats:
# a) By yos: 0-54,  1y interval 
# b) By age: 20-74, 1y interval
# c) Data frame: ea 20-74, age 20-74 
# All output data frames should include following variables:
  # planname, TermRatesType, termrate, age/ea/yos


# 5. Retirement Rates 
 # Output should be in one of the following formats:
   # a) By age: range specified by the plan, 1y intervals
   # b) data frame: ea, age, range specified by the plan (CA PERF only), by both by 1y intervals


# 6. Disability Rates 
 # Output should be in one of the following formats:
  # a) By age: range specified by the plan; 1y intervals
  # b) data frame: ea, age, range specified by the plan (CA PERF only); 1y intervals



# 7. Single Values
 # df_singleValues_num
 # df_singleValues_chr
 # Changes needed in the xlsx files:
   # New column: varType
   # New rows:   planname, plantype
   # Format: no commas in numbers
   # Format: no % sigh
   # units: actuall numbers 
   # schedule type: byAge, byYOS, LowYOS, Matrix





#********************************************************************************
#                 Reading data from xlsx files                     ####
#********************************************************************************

folder_name <- "./Inputs_Data_largePlans/"

get_fileName <- function(x) paste0(folder_name, "LargePlans_LoadingData_", x, ".R")

# General Plans
get_fileName("9_CA_CA-CALPERS") %>% source() # OK
get_fileName("26_FL_FL-FRS")    %>% source() # OK Check "AllNA"
get_fileName("83_NY_NY-ERS")    %>% source() # OK
get_fileName("85_OH_OH-OPERS")  %>% source() # OK
get_fileName("125_WI_WI-ETF")   %>% source() # OK ? Productivity growth = 0?


# Safety plans
get_fileName("72_NJ_NJ-PFRS")       %>% source() # OK Check "AllNA"
get_fileName("84_NY_NY-PFRS")       %>% source() # OK
get_fileName("86_OH_OH-OPF")        %>% source() # OK
get_fileName("140_CA_LACITY-LAFPP") %>% source() # OK
get_fileName("150_NY_NYC-PPF")      %>% source() # OK


# Teachers plans
get_fileName("10_CA_CA-CALSTRS") %>% source() # OK check "AllNA"
get_fileName("28_GA_GA-TRS")     %>% source() # OK
get_fileName("78_NY_NY-NYSTRS")  %>% source() # OK check "AllNA"
get_fileName("88_OH-OH-STRS")    %>% source() # OK
get_fileName("108_TX_TX-TRS")    %>% source() # OK



planNames_general <- c("9_CA_CA-CALPERS",
                       "26_FL_FL-FRS",
                       "83_NY_NY-ERS",
                       "85_OH_OH-OPERS",
                       "125_WI_WI-ETF")

planNames_safety  <- c("72_NJ_NJ-PFRS",    
                       "84_NY_NY-PFRS",     
                       "86_OH_OH-OPF",     
                       "140_CA_LACITY-LAFPP",
                       "150_NY_NYC-PPF")

planNames_teacher  <- c("10_CA_CA-CALSTRS",
                       "28_GA_GA-TRS",   
                       "78_NY_NY-NYSTRS",
                       "88_OH-OH-STRS", 
                       "108_TX_TX-TRS")

 
source(paste0(folder_name, "LargePlans_Data_standardize.R"))

    


