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

planname <- "84_NY_NY-PFRS"
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
#      2.1 Loading grouped salary data                       ####
#****************************************************************

if(singleValues$num[singleValues$num$varname == "SalarySched_byAgeGrp", "value"]){
salaryByAge1 <- fn_ret.ben(planname_ = planname, plantype_ = plantype,  fileName_ = filename, sheet = "SalarySched_byAgeGrp1") %>% 
  select(-nactives)
salaryByAge1

salaryByAge2 <- read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "SalarySched_byAgeGrp2") %>% 
  mutate(planname = planname, plantype = plantype, fillin = TRUE)
salaryByAge2

salaryByAge <-  bind_rows(
  salaryByAge1,
  salaryByAge2,
  salaryByAge2 %>% mutate(fillin = FALSE)
) %>% 
  arrange(fillin, age)
salaryByAge
} else {
  salaryByAge <- NA
}


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
 df_SalaryGrowth_raw %<>%
   splong("yos", 0:54)

SalaryGrowthSched <- data.frame(yos = 0:54) %>% 
  mutate(planname = planname,
         plantype = plantype, 
         SalaryGrowthType = SalaryGrowthType) %>% 
  left_join(df_SalaryGrowth_raw) %>% 
  mutate(grate = ifelse(yos > 25, grate[yos == 25], grate),
         grate = ifelse(yos < 0,  grate[yos == 0], grate)) %>%
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
# df_TermRatesSched <-
#   read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "TermRatesSched_Matrix") %>%
#   filter(!is.na(age.cell)) %>%
#   select(-type, -agegrp) %>%
#   rename(age = age.cell)
#   
# df_TermRatesSched %<>%   
#   gather(yos, termrate, -age) %>%
#   mutate(yos = str_extract(yos, "\\d+") %>% as.numeric()) %>%
#   arrange(age, yos) %>%
#   splong("age", 20:74) %>%
#   group_by(yos) %>%
#   mutate(termrate = ifelse(age < 25, termrate[age == 25], termrate),
#          termrate = ifelse(age > 60, termrate[age == 60], termrate)) %>%
#   ungroup
# df_TermRatesSched
# 
# 
# TermRatesSched <- expand.grid(age = 20:74, ea = 20:74) %>%
#     filter(age >= ea) %>%
#     mutate(yos = age - ea) %>%
#     left_join(df_TermRatesSched) %>%
#   group_by(age) %>%
#   mutate(termrate = ifelse(yos <= 10, termrate, termrate[yos == 10]),
#          planname      = planname,
#          plantype      = plantype,
#          TermRatesType = TermRatesType ) %>%
#   ungroup %>%
#   select(planname, plantype, TermRatesType, everything(), termrate)
# TermRatesSched
# # TermRatesSched %>% filter(age ==40)
  
#******************************************************************************************
# Template for singleCol or LowYOS
#
# Single column values
if(TermRatesType != "Matrix"){

  # Loading data
  df_TermRatesSched_singleCol <-
    read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "TermRatesSched_SingleCol") %>%
    filter(!is.na(termrate))

  # specify range for yos or age
  TermRates_index <- names(df_TermRatesSched_singleCol)[1]
  if(TermRates_index == "age") TermRatesSched_range <- data.frame(age = 20:74)
  if(TermRates_index == "yos") TermRatesSched_range <- data.frame(yos = 0:54)

  # Data process: PLAN SPECIFIC
  df_TermRatesSched_singleCol <- splong(df_TermRatesSched_singleCol, "yos", 0:15)

  # Merging Data
  df_TermRatesSched_singleCol <-
    TermRatesSched_range %>%
    left_join(df_TermRatesSched_singleCol)

  # Final Processing: PLAN SPECIFIC
  df_TermRatesSched_singleCol %<>%
    mutate(termrate = ifelse(yos < 5,  termrate[yos == 5], termrate),
           termrate = ifelse(yos > 15, termrate[yos == 15], termrate)) # plan specific
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
      mutate(termrate = ifelse(yos <= LowYOS_cut, termrate.lowYOS, termrate) ) %>%
      select(termrate, everything(), -termrate.lowYOS)
}

TermRatesSched %<>%
  mutate(planname      = planname,
         plantype      = plantype,
         TermRatesType = TermRatesType ) %>%
  select(planname, plantype, TermRatesType, age, ea, yos, termrate)
TermRatesSched
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
#   select(age = age.cell, everything(),  -type, -agegrp) %>%
#   filter(!is.na(age)) %>%
#   gather(yos, retrate, -age) %>%
#   mutate(yos = str_extract(yos, "\\d+") %>% as.numeric()) %>%
#   splong("age", 20:74) %>%
#   # PLAN SPECIFIC ADJUSTMENTS
#   mutate(retrate = ifelse(retrate >= 0, retrate, 0),
#          retrate = ifelse(age     < 55, 0, retrate)) %>%
#   group_by(yos) %>%
#   mutate(retrate = ifelse(age > 65, retrate[age == 65], retrate)) %>%
#   
#   ungroup %>%
#   splong("yos", 0:54) %>%
#   group_by(age) %>%
#   mutate(retrate = ifelse(yos < 20, retrate[yos == 20], retrate),
#          retrate = ifelse(yos > 65, retrate[yos == 65], retrate)) %>%
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
    # splong("yos", 20:54)
    splong("yos", 0:54) %>% 
    mutate(retrate = ifelse(yos < 20, 0, retrate),
           retrate = ifelse(yos > 30, retrate[yos == 30], retrate))
  }

RetRatesSched <- df_RetRatesSched_singleCol %>%
  mutate(planname     = planname,
         plantype     = plantype,
         RetRatesType = RetRatesType) %>%
  select(planname, plantype,RetRatesType, yos, retrate)

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
  splong("age", 35:50)

DisbRatesSched <-
  data.frame(age = 20:74) %>%
  left_join(df_DisbRatesSched_singleCol) %>%
  mutate(  disbrate = ifelse(age < 35, disbrate[age == 35], disbrate),
           disbrate = ifelse(age > 50, disbrate[age == 50], disbrate),
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

#**********************************************************************************************
## Template for Matrix  * 
#************************
#
# df_DisbRatesSched_raw <-
#   read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "DisbRatesSched_Matrix")
# 
# df_DisbRatesSched_raw %<>%
#   select(age = age.cell, X5, X15) %>%
#   filter(!is.na(age)) %>%
#   gather(yos, disbrate, -age) %>%
#   mutate(yos = str_extract(yos, "\\d+") %>% as.numeric()) %>%
#   splong("age", 20:74) %>%
#   mutate(disbrate = ifelse(disbrate >=0, disbrate, 0)
#          #disbrate = ifelse(yos == 25 & age <50, 0, disbrate)
#          ) %>%
#   group_by(yos) %>%
#   mutate(disbrate = ifelse(age > 60, disbrate[age == 60], disbrate)) %>%
#   ungroup %>%
#   splong("yos", 0:54) %>%
#   group_by(age) %>%
#   mutate(disbrate = ifelse(yos < 5, disbrate[yos == 5], disbrate),
#          disbrate = ifelse(yos > 15, disbrate[yos == 15], disbrate)) %>%
#   arrange(age, yos) %>%
#   ungroup
# 
# 
# DisbRatesSched <-
#   expand.grid(age = 20:74, ea = 20:74) %>%
#   filter(age >= ea) %>%
#   mutate(planname = planname,
#          plantype = plantype,
#          DisbRatesType = DisbRatesType,
#          yos = age - ea) %>%
#   left_join(df_DisbRatesSched_raw) %>%
#   select(planname, plantype, DisbRatesType, ea, age, yos, disbrate)
# 
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
  DisbRatesSched    = DisbRatesSched,
  
  SalarySched_byAgeGrp = salaryByAge
)


# if(singleValues$num[singleValues$num$varname == "SalarySched_byAgeGrp", "value"]){
#   data_largePlan$SalarySched_byAgeGrp = salaryByAge
# }
#data_largePlan$SalarySched_byAgeGrp

save(data_largePlan, file = paste0(data_dir, "DataLargePlan_", planname,  ".RData"))



