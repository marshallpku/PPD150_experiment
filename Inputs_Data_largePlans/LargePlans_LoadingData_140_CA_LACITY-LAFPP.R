# Read make-shift raw data (based on PSERS) and convert it to standardized format 



#**************************************************
#               
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


#**************************************************
#                Directory                  ####
#**************************************************

planname <- "140_CA_LACITY-LAFPP"
plantype <- "safety"
data_dir <- "./Inputs_Data_largePlans/"
filename <- paste0(data_dir, planname, ".xlsx")



#**************************************************
#       1. Loading single values               ####
#**************************************************

singleValues <- filename %>% get_singleValues

singleValues$num
singleValues$chr


#****************************************************************
#      2. Loading grouped actives data                       ####
#****************************************************************
ActivesSched <- fn_actives(planname_ = planname, plantype_ = plantype,  fileName_ = filename)
ActivesSched

# ActivesSched %>% filter(fillin == TRUE) %>% arrange(age, yos) %>%
#   select(age, yos, nactives) %>%
#   spread(yos, nactives)
# 
# ActivesSched %>% filter(fillin == TRUE) %>% arrange(age, yos) %>%
# summarise(n = sum(nactives, na.rm = TRUE))



#****************************************************************
#      3. Loading  beneficiaries data                        ####
#****************************************************************
RetireeSched <- fn_ret.ben(planname_ = planname, plantype_ = plantype,  fileName_ = filename)
RetireeSched


#****************************************************************
#      4. Loading salary growth data                        ####
#****************************************************************

SalaryGrowthType <- singleValues$chr[singleValues$chr$varname == "SalaryGrowthType", "value"]

if(SalaryGrowthType != "Matrix"){
  
df_SalaryGrowth_raw <- 
  read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "SalaryGrowthSched_SingleCol") %>% 
  filter(!is.na(grate))

SalaryGrowthSched <- df_SalaryGrowth_raw %>% 
  mutate(planname = planname,
         plantype = plantype,
         SalaryGrowthType = SalaryGrowthType) %>% 
  select(planname, plantype, SalaryGrowthType, everything())
} 

SalaryGrowthSched



#****************************************************************
#      5.  Loading termination rate data                     ####
#****************************************************************

TermRatesType <- singleValues$chr[singleValues$chr$varname == "TermRatesType", "value"]

## Single column values 
if(TermRatesType != "Matrix"){
  
  # Loading data
  df_TermRatesSched_singleCol <- 
    read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "TermRatesSched_SingleCol") %>% 
    filter(!is.na(termrate))
  
  # specify range for yos or age
  TermRates_index <- names(df_TermRatesSched_singleCol)[1]
  if(TermRates_index == "age") TermRatesSched_range <- data.frame(age = 20:74) 
  if(TermRates_index == "yos") TermRatesSched_range <- data.frame(age = 0:54) 
  
  # Data process: PLAN SPECIFIC 
  df_TermRatesSched_singleCol <- splong(df_TermRatesSched_singleCol, "age", 20:55)

  # Merging Data
  df_TermRatesSched_singleCol <-  
    TermRatesSched_range %>% 
    left_join(df_TermRatesSched_singleCol)
  
  # Final Processing: PLAN SPECIFIC 
  df_TermRatesSched_singleCol %<>% 
    mutate(termrate = ifelse(age>=55, 0, termrate)) # plan specific
}


## Low YOS values
if(TermRatesType == "LowYOS"){

  df_TermRatesSched_LowYOS <- 
    read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "TermRatesSched_LowYOS") %>% 
    filter(!is.na(termrate.lowYOS))
  
  LowYOS_cut <- max(df_TermRatesSched_LowYOS$yos)
}


## Create the final table (age by yos)
  
TermRatesSched <- expand.grid(age = 20:74, ea = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  left_join(df_TermRatesSched_singleCol)

if(TermRatesType == "LowYOS"){
  TermRatesSched %<>% 
    left_join(df_TermRatesSched_LowYOS) %>% 
      mutate(planname      = planname,
             plantype      = plantype,
             termrate      = ifelse(yos <= LowYOS_cut, termrate.lowYOS, termrate),
             TermRatesType = TermRatesType ) %>% 
      select(planname, plantype, TermRatesType, termrate, everything(), -termrate.lowYOS)
}
TermRatesSched


#****************************************************************
#      6.  Loading retirement rate data                     ####
#****************************************************************

RetRatesType <- singleValues$chr[singleValues$chr$varname == "RetRatesType", "value"]

if(RetRatesType != "Matrix"){
  
  # Loading Data
  df_RetRatesSched_singleCol <- 
    read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "RetirementRatesSched_SingleCol") %>% 
    filter(!is.na(retrate))

  } 

RetRatesSched <- df_RetRatesSched_singleCol %>% 
  mutate(planname     = planname, 
         plantype     = plantype,
         RetRatesType = RetRatesType) %>% 
  select(planname, plantype, RetRatesType, age, retrate)

RetRatesSched


#****************************************************************
#      7.  Loading disability data                     ####
#****************************************************************

DisbRatesType <- singleValues$chr[singleValues$chr$varname == "DisbRatesType", "value"]

if(DisbRatesType != "Matrix"){
  
  # Loading Data
  df_DisbRatesSched_singleCol <- 
    read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "DisbRatesSched_SingleCol") %>% 
    filter(!is.na(disbrate))
  
  # Specify range for yos or age
  DisbRatesSched_range <- data.frame(age = 20:74) 
  
  # Data process: PLAN SPECIFIC 
  disb.agemin <- df_DisbRatesSched_singleCol$age %>% min
  disb.agemax <- df_DisbRatesSched_singleCol$age %>% max
  
  df_DisbRatesSched_singleCol <- splong(df_DisbRatesSched_singleCol, "age", disb.agemin:disb.agemax) 
  
  # Merging Data
  df_DisbRatesSched_singleCol <-  
    DisbRatesSched_range %>% 
    left_join(df_DisbRatesSched_singleCol)
  
  # Final Processing: PLAN SPECIFIC 
  df_DisbRatesSched_singleCol %<>% 
    mutate(disbrate = ifelse(age<=disb.agemax, disbrate, disbrate[age == disb.agemax] ), # plan specific
           planname      = planname,
           plantype      = plantype,
           DisbRatesType = DisbRatesType) %>% 
    select(planname, plantype, DisbRatesType, everything())       
} 

## Low YOS values 
 # --


## Create the final table (age by yos)
DisbRatesSched <- df_DisbRatesSched_singleCol
DisbRatesSched


#**************************************************
#                 Saving Data                  ####
#**************************************************

data_largePlan <- list(
  planname           = planname,
  singleValues_num   = singleValues$num,
  singleValues_chr   = singleValues$chr,
  
  ActivesSched = ActivesSched,
  RetireeSched = RetireeSched,
  
  SalaryGrowthSched = SalaryGrowthSched,
  TermRatesSched    = TermRatesSched,
  RetRatesSched     = RetRatesSched,
  DisbRatesSched    = DisbRatesSched
)

save(data_largePlan, file = paste0(data_dir, "DataLargePlan_", planname,  ".RData"))




