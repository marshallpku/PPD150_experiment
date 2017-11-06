# Read make-shift raw data (based on PSERS) and convert it to standardized format 


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


#**************************************************
#                Directory                     ####
#**************************************************

planname <- "86_OH_OH-OPF"
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

ActivesSched %>% filter(fillin == TRUE) %>% arrange(age, yos) %>%
  select(age, yos, nactives) %>%
  spread(yos, nactives)

ActivesSched %>% filter(fillin == TRUE) %>% arrange(age, yos) %>%
summarise(n = sum(nactives, na.rm = TRUE))



#****************************************************************
#      3. Loading  beneficiaries data                        ####
#****************************************************************
RetireeSched <- fn_ret.ben(planname_ = planname, plantype_ = plantype,  fileName_ = filename)
RetireeSched


#****************************************************************
#      4. Loading salary growth data                        ####
#****************************************************************

SalaryGrowthType <- singleValues$chr[singleValues$chr$varname == "SalaryGrowthType", "value"]

# SingleCol
df_SalaryGrowth_raw <- 
  read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "SalaryGrowthSched_SingleCol") %>% 
  filter(!is.na(yos))

# # imputation:
#  df_SalaryGrowth_raw %<>% 
#    splong("age", 25:60)


SalaryGrowthSched <- data.frame(yos = 0:54) %>% 
  mutate(planname = planname,
         plantype = plantype, 
         SalaryGrowthType = SalaryGrowthType) %>% 
  left_join(df_SalaryGrowth_raw) %>% 
  # mutate(grate = ifelse(age > 60, grate[age == 60], grate),
  #        grate = ifelse(age < 25, grate[age == 25], grate)) %>% 
  select(planname, plantype, SalaryGrowthType, yos, grate)
SalaryGrowthSched  

# CALPERS (Matrix)
# df_SalaryGrowth_raw %<>% 
#   gather(yos, value, -ea) %>% 
#   mutate(yos = str_extract(yos, "\\d+") %>% as.numeric()) %>% 
#   splong("yos", 0:30)
# 
# ea.cut <- df_SalaryGrowth_raw$ea
# 
# SalaryGrowthSched <- 
#   expand.grid(age = 20:74, ea = 20:74) %>% 
#   filter(age >= ea) %>% 
#   mutate(planname = planname,
#          plantype = plantype, 
#          SalaryGrowthType = SalaryGrowthType,
#          yos = age - ea,
#          ea.index = findInterval(ea, ea.cut),
#          ea.index = ea.cut[ea.index]) %>% 
#   left_join(df_SalaryGrowth_raw %>% rename(ea.index = ea, grate = value)) %>% 
#   mutate(grate = ifelse(yos <= 30, grate, grate[yos==30])) %>% 
#   select(planname, plantype, SalaryGrowthType, ea, age, yos, grate)

# LAFPP (already completed)
# if(SalaryGrowthType != "Matrix"){
#   
# df_SalaryGrowth_raw <- 
#   read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "SalaryGrowthSched_SingleCol") %>% 
#   filter(!is.na(grate))
# 
# SalaryGrowthSched <- df_SalaryGrowth_raw %>% 
#   mutate(planname = planname,
#          SalaryGrowthType = SalaryGrowthType) %>% 
#   select(planname, SalaryGrowthType, everything())
# } 
# 
# SalaryGrowthSched


#****************************************************************
#      5.  Loading termination rate data                     ####
#****************************************************************

TermRatesType <- singleValues$chr[singleValues$chr$varname == "TermRatesType", "value"]

# #******************************************************************************************
# # Template for Matrix
# 
df_TermRatesSched <-
  read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "TermRatesSched_Matrix") %>%
  filter(!is.na(age.cell)) %>%
  select(-type, -agegrp) %>%
  rename(age = age.cell)
  
df_TermRatesSched %<>%   
  gather(yos, termrate, -age) %>%
  mutate(yos = str_extract(yos, "\\d+") %>% as.numeric()) %>%
  arrange(age, yos) %>%
  splong("age", 20:74) %>%
  group_by(yos) %>%
  mutate(termrate = ifelse(age < 25, termrate[age == 25], termrate),
         termrate = ifelse(age > 60, termrate[age == 60], termrate)) %>%
  ungroup
df_TermRatesSched


TermRatesSched <- expand.grid(age = 20:74, ea = 20:74) %>%
    filter(age >= ea) %>%
    mutate(yos = age - ea) %>%
    left_join(df_TermRatesSched) %>%
  group_by(age) %>%
  mutate(termrate = ifelse(yos <= 10, termrate, termrate[yos == 10]),
         planname      = planname,
         plantype      = plantype,
         TermRatesType = TermRatesType ) %>%
  ungroup %>%
  select(planname, plantype, TermRatesType, everything(), termrate)
TermRatesSched
# # TermRatesSched %>% filter(age ==40)
  
#******************************************************************************************
# Template for singleCol or LowYOS
#
# Single column values
# if(TermRatesType != "Matrix"){
# 
#   # Loading data
#   df_TermRatesSched_singleCol <-
#     read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "TermRatesSched_SingleCol") %>%
#     filter(!is.na(termrate))
# 
#   # specify range for yos or age
#   TermRates_index <- names(df_TermRatesSched_singleCol)[1]
#   if(TermRates_index == "age") TermRatesSched_range <- data.frame(age = 20:74)
#   if(TermRates_index == "yos") TermRatesSched_range <- data.frame(age = 0:54)
# 
#   # Data process: PLAN SPECIFIC
#   df_TermRatesSched_singleCol <- splong(df_TermRatesSched_singleCol, "age", 25:60)
# 
#   # Merging Data
#   df_TermRatesSched_singleCol <-
#     TermRatesSched_range %>%
#     left_join(df_TermRatesSched_singleCol)
# 
#   # Final Processing: PLAN SPECIFIC
#   df_TermRatesSched_singleCol %<>%
#     mutate(termrate = ifelse(age < 25, termrate, termrate[age == 25]),
#            termrate = ifelse(age > 60, termrate, termrate[age == 60])) # plan specific
# }
# 
# 
# ## Low YOS values
# if(TermRatesType == "LowYOS"){
# 
#   df_TermRatesSched_LowYOS <-
#     read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "TermRatesSched_LowYOS") %>%
#     filter(!is.na(termrate.lowYOS))
# 
#   LowYOS_cut <- max(df_TermRatesSched_LowYOS$yos)
# }
# 
# 
# ## Create the final table (age by yos)
# 
# TermRatesSched <- expand.grid(age = 20:74, ea = 20:74) %>%
#   filter(age >= ea) %>%
#   mutate(yos = age - ea) %>%
#   left_join(df_TermRatesSched_singleCol)
# 
# 
# 
# if(TermRatesType == "LowYOS"){
#   TermRatesSched %<>%
#     left_join(df_TermRatesSched_LowYOS) %>%
#       mutate(termrate      = ifelse(yos <= LowYOS_cut, termrate.lowYOS, termrate) ) %>%
#       select(termrate, everything(), -termrate.lowYOS)
# }
# 
# TermRatesSched %<>%
#   mutate(planname      = planname,
#          plantype      = plantype,
#          TermRatesType = TermRatesType ) %>%
#   select(planname, plantype, TermRatesType, age, ea, yos, termrate)
#***********************************************************************************
  


#****************************************************************
#      6.  Loading retirement rate data                     ####
#****************************************************************

RetRatesType <- singleValues$chr[singleValues$chr$varname == "RetRatesType", "value"]

# # Single Col (already complete)
# df_RetRatesSched_singleCol <-
#       read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "RetirementRatesSched_SingleCol") %>%
#       filter(!is.na(retrate))
# 
# RetRatesSched <- 
#     df_RetRatesSched_singleCol %>%
#     mutate(planname = planname,
#            plantype = plantype,
#            RetRatesType = RetRatesType) %>%
#     select(planname, plantype, RetRatesType, age, retrate)
# RetRatesSched

#**********************************************************************************************
## Template for Matrix  * 
#************************
#
# df_RetRatesSched_raw <- 
#   read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "RetirementRatesSched_Matrix")
# 
# df_RetRatesSched_raw %<>% 
#   select(age = age.cell, X10, X25) %>% 
#   filter(!is.na(age)) %>% 
#   gather(yos, retrate, -age) %>% 
#   mutate(yos = str_extract(yos, "\\d+") %>% as.numeric()) %>% 
#   splong("age", 20:74) %>% 
#   mutate(retrate = ifelse(retrate >=0, retrate, 0),
#          retrate = ifelse(yos == 25 & age <50, 0, retrate)) %>%
#   group_by(yos) %>% 
#   mutate(retrate = ifelse(age > 70, retrate[age == 70], retrate)) %>% 
#   ungroup %>% 
#   splong("yos", 0:54) %>% 
#   group_by(age) %>% 
#   mutate(retrate = ifelse(yos < 10, retrate[yos == 10], retrate),
#          retrate = ifelse(yos > 25, retrate[yos == 25], retrate)) %>% 
#   arrange(age, yos) %>% 
#   ungroup
#   
# 
# RetRatesSched <- 
#   expand.grid(age = 20:74, ea = 20:74) %>% 
#   filter(age >= ea) %>% 
#   mutate(planname = planname,
#          plantype = plantype, 
#          RetRatesType = RetRatesType,
#          yos = age - ea) %>% 
#   left_join(df_RetRatesSched_raw) %>% 
#   select(planname, plantype, RetRatesType, ea, age, yos, retrate)
# 
# RetRatesSched
#**********************************************************************************************

#**********************************************************************************************
## Template for singleCol or LowYOS *
#************************************ 

if(RetRatesType != "Matrix"){

  # Loading Data
  df_RetRatesSched_singleCol <-
    read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "RetirementRatesSched_SingleCol") %>%
    filter(!is.na(retrate))

  # Imputation: Plan specific
  df_RetRatesSched_singleCol %<>%
    splong("age", 48:70)
  }

RetRatesSched <- df_RetRatesSched_singleCol %>%
  mutate(planname     = planname,
         plantype     = plantype,
         RetRatesType = RetRatesType) %>%
  select(planname, plantype,RetRatesType, age, retrate)

RetRatesSched
#**********************************************************************************************


#****************************************************************
#      7.  Loading disability data                     ####
#****************************************************************

DisbRatesType <- singleValues$chr[singleValues$chr$varname == "DisbRatesType", "value"]

df_DisbRatesSched_singleCol <- 
      read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "DisbRatesSched_SingleCol") %>%
      filter(!is.na(disbrate))

# Imputation: Plan specific
df_DisbRatesSched_singleCol %<>%
  splong("age", 20:64)

DisbRatesSched <- 
  data.frame(age = 20:74) %>% 
  left_join(df_DisbRatesSched_singleCol) %>%
  mutate(  disbrate = ifelse(age < 20, disbrate[age == 20], disbrate),
           disbrate = ifelse(age > 64, disbrate[age == 64], disbrate),
           planname      = planname,
           plantype      = plantype,
           DisbRatesType = DisbRatesType) %>%
    select(planname, plantype, DisbRatesType, everything())
DisbRatesSched

#**********************************************************************************************
## Template for singleCol or LowYOS *
#************************************ 
# 
# if(DisbRatesType != "Matrix"){
#   
#   # Loading Data
#   df_DisbRatesSched_singleCol <- 
#     read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "DisbRatesSched_SingleCol") %>% 
#     filter(!is.na(disbrate))
#   
#   # Specify range for yos or age
#   DisbRatesSched_range <- data.frame(age = 20:74) 
#   
#   # Data process: PLAN SPECIFIC 
#   disb.agemin <- df_DisbRatesSched_singleCol$age %>% min
#   disb.agemax <- df_DisbRatesSched_singleCol$age %>% max
#   
#   df_DisbRatesSched_singleCol <- splong(df_DisbRatesSched_singleCol, "age", disb.agemin:disb.agemax) 
#   
#   # Merging Data
#   df_DisbRatesSched_singleCol <-  
#     DisbRatesSched_range %>% 
#     left_join(df_DisbRatesSched_singleCol)
#   
#   # Final Processing: PLAN SPECIFIC 
#   df_DisbRatesSched_singleCol %<>% 
#     mutate(disbrate = ifelse(age<=disb.agemax, disbrate, disbrate[age == disb.agemax] ), # plan specific
#            planname      = planname,
#            plantype      = plantype,
#            DisbRatesType = DisbRatesType) %>% 
#     select(planname, plantype, DisbRatesType, everything())       
# } 
# 
# ## Low YOS values 
#  # --
# 
# 
# ## Create the final table (age by yos)
# DisbRatesSched <- df_DisbRatesSched_singleCol
# DisbRatesSched
#**********************************************************************************************

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





