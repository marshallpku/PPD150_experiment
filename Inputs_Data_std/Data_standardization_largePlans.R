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


#****************************************************************
#          Description of standardized data                  ####
#****************************************************************

# Structure of structure of standardized 
# One standardized data file for each plan:
# Naming rule: "planData_" + PPD name
#              example: planData_9_CA_CA-CALPERS.RData
# Each data file contains 1 data list, the list has the same name as the data file
# The data list contains the following elements
#   1. inputsSingleValues
#   2. decrements
#   3. init_actives
#   4. init_retirees
#   5. init_amort
#   6. init_unrecReturns

# Format of standardized inputs
# Notes:N/A cells filled with 0

# 1. inputsSingleValues
## plan vars
# ppd_id
# ppd_planname
# planName
# StateAbbrev
# plantype
# fy_end     (the calendar year that the fy in valuation ends in)
# year_av

# *AL          
# *AL_act      
# *AL_retired

# *payroll
# *FR_MAV: ppd
# *FR_AAV: ppd
# *B:      ppd
# init_nactives: from large plan data
# init_retirees: from large plan data

# *i: ppd
# *prod
# *infl
# *startingSal_growth

## use backup values for now: Ask Hao to find AV values
# bfactor_current
# bfactor_entrants
# vest_yos
# retage_normal
# retage_early
# retage_max
# earlyRet_reduction
# cola
# FAS_year

## Use PPD values
# *asset_year
# *amort_openclosed
# *amort_pctdol
# *amort_year
# *salgrowth_amort
# *EEC_pct.current
# EEC_pct.entrants: need assumption
# ERC_restriction

## optional vars
# PVB
# PVB_active
# PVB_retired
# *UAAL:  ppd

## model vars
# ncore
# nsim
# nyear
# age_min
# age_max
# ea_min
# ea_max
# no_entrance
# wf_growth

# Also needed:
# 1. backup for missing values
# 2. figure out a way to override plan parameters


# 2. decrements
# vars:
# ppd_id
# ppd_planname
# ea:  20~74
# age: 20~110
# qxm.pre
# qxm.post
# qxm.d
# qxm.term
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

# 5. salary scale
# vars:
# ppd_id
# ppd_planname
# age:20-110 or YOS:0-54
# salgrowth

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

#################################################################


#**************************************************
#          Global settings                     ####
#**************************************************
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
                        "88_OH_OH-STRS", 
                        "108_TX_TX-TRS")

planNames_all <- c(planNames_general, planNames_safety, planNames_teacher)



#**************************************************************************************
#                Data from large plan AVs and CAFRs                                ####
#**************************************************************************************

data_dir  <- "./Inputs_Data_largePlans/"
file_name <- "largePlans_dataOutputs.RData"
load(paste0(data_dir, file_name))

# planName_extract <- "9_CA_CA-CALPERS"

init_actives <- 
  largePlans_dataOutputs$largePlans_actives_scales_byPlan %>% 
    rename(ppd_planname = planname) %>% 
    mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
    select(ppd_id, ppd_planname, ea, age, nactives, salary) %>% 
    ungroup

init_nretirees <- 
  largePlans_dataOutputs$largePlans_retirees_scales_byPlan %>% 
  rename(ppd_planname = planname) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, age, nretirees, benefit) %>% 
  filter(age>=21, age<=110) %>% 
  ungroup


salgrowth <- 
  largePlans_dataOutputs$largePlans_salaryScale_byPlan %>% 
  rename(ppd_planname = planname,
         salgrowth = grate) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, ea, age, salgrowth) %>% 
  ungroup


decrements <- expand.grid(age = 20:74, ea = 20:74) %>% 
  filter(age >= ea) %>% 
  left_join(largePlans_dataOutputs$largePlans_retRates_byPlan  %>% select(planname, plantype, ea, age, qxr = retrate))  %>% 
  left_join(largePlans_dataOutputs$largePlans_termRates_byPlan %>% select(planname, plantype, ea, age, qxt = termrate)) %>% 
  left_join(largePlans_dataOutputs$largePlans_disbRates_byPlan %>% select(planname, plantype, ea, age, qxd = disbrate)) %>% 
  rename(ppd_planname = planname) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, plantype, everything()) %>% 
  arrange(ppd_id, ppd_planname)


inputs_singleValues_largePlans_num <- 
largePlans_dataOutputs$largePlans_singleValues_num %>% 
  select(ppd_planname = planname, plantype, FY, varname, value) %>%
  group_by(ppd_planname) %>% 
  mutate(FY = FY[!is.na(FY)] %>% max ) %>% 
  spread(varname, value) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, plantype, FY,
         AL_act = AL_active,
         AL_retired,
         AL = AL_total,
         payroll,
         
         infl = inflation,
         prod = prod_growth,
         salgrowth_amort = payroll_growth,
         
         PVB_act = PVB_active,
         PVB_retired,
         PVFNC_act = PVFNC_active)
        
inputs_singleValues_largePlans_chr <- 
largePlans_dataOutputs$largePlans_singleValues_chr %>% 
  select(ppd_planname = planname, varname, value) %>%
  group_by(ppd_planname) %>% 
  spread(varname, value) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, plantype,
         erc_rule)



#**************************************************************************************
#                Data of large plans from PPD                                      ####
#**************************************************************************************

data_dir_ppd  <- "./Inputs_Data_PPD/"
file_name_ppd <- "DataPPD.RData"
load(paste0(data_dir_ppd, file_name_ppd)) # PPD_trim loaded

PPDsingleValues_forLargePlans <- 
  PPD_data %>% 
  filter(ppd_id %in% as.numeric(str_extract(planNames_all, "\\d+"))) %>% 
  select(ppd_id, planName, StateAbbrev, 
         
         AL_ppd,
         payroll_ppd,
         
         B     = B_ppd,
         B_avg = B_avg_best_ppd,

         FR_MAV,
         FR_AAV,
         i = i_ppd,

         asset_year       = asset_year_ppd,
         amort_openclosed = amort_openclosed_ppd,
         amort_pctdol     = amort_pctdol_ppd,
         amort_year       = amort_year_ppd,
         
         EEC = EEC_ppd,
         EEC_pct.current = EEC_rate_ppd_calc
         ) %>% 
  mutate(EEC_pct.entrants = EEC_pct.current)
  
PPDsingleValues_forLargePlans



#**************************************************************************************
#                Use NMR values for benefit provisions                             ####
#**************************************************************************************

data_dir_backup  <- "./Inputs_Data_std/"
file_name_backup <- "Data_backupValues.xlsx"

singleValues_benefitProv_backup <- 
  read_ExcelRange(paste0(data_dir_backup, file_name_backup), sheet = "benefitProvisions" )




#**************************************************************************************
#                Combine single value variables                                    ####
#**************************************************************************************


inputsSingleValues <- 
  left_join(inputs_singleValues_largePlans_num,
            inputs_singleValues_largePlans_chr) %>% 
  left_join(PPDsingleValues_forLargePlans) %>% 
  mutate(benProv_join  = "benProv_join") %>% 
  left_join(singleValues_benefitProv_backup) %>% 
  select(-benProv_join) %>% 
  mutate(fy_end = FY)

inputsSingleValues %>% names

# inputsSingleValues %>% 
#   select(planName, AL, AL_ppd, payroll, payroll_ppd)




#**************************************************************************************
#                    Unrecognized returns                                   ####
#**************************************************************************************


get_unrecReturns <- function(ppd_id, fy_end, unrec_tot, asset_year){
  # ppd_id = 1
  # fy_end = 2016
  # unrec_tot = 100
  # asset_year = 5
  
  data.frame(year = fy_end + seq_len(asset_year),
             DeferredReturn = unrec_tot / asset_year) %>% 
    mutate(ppd_id = ppd_id) %>% 
    select(ppd_id, everything())
}

df_unrecReturns <- 
inputsSingleValues %>% 
  select(ppd_id, ppd_planname,fy_end,  AL, FR_MAV, FR_AAV, asset_year) %>% 
  mutate(MAV = AL * FR_MAV,
         AAV = AL * FR_AAV,
         unrec_tot = MAV - AAV,
         asset_year = ifelse(is.na(asset_year), 5, asset_year))

unrecReturns_largePlans <- 
bind_rows(
  mapply(get_unrecReturns, 
         ppd_id = df_unrecReturns$ppd_id,
         fy_end = df_unrecReturns$fy_end,
         unrec_tot = df_unrecReturns$unrec_tot,
         asset_year = df_unrecReturns$asset_year,
         SIMPLIFY = FALSE)
)





