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
# 1. inputsSingleValues
# 2. decrements
# 3. init_actives
# 4. init_retirees
# 5. init_amort
# 6. init_unrecReturns

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

planNames_large <- c(planNames_general, planNames_safety, planNames_teacher)
planid_large <- as.numeric(str_extract(planNames_large, "\\d+")) 


#**************************************************************************************
#                Data of large plans from PPD                                      ####
#**************************************************************************************

data_dir_ppd  <- "./Inputs_Data_PPD/"
file_name_ppd <- "DataPPD.RData"
load(paste0(data_dir_ppd, file_name_ppd)) # PPD_trim loaded

PPD_data %>% names

PPDsingleValues_forPPDplans <- 
  PPD_data %>% 
  select(ppd_id, planName, StateAbbrev, 
         plantype,
         
         AL_ppd_src,
         AL = AL_ppd,
         # AL_act
         # AL_retired
         
         payroll = payroll_ppd,
         FR_MAV,
         FR_AAV,
         B     = B_ppd,
         B_avg = B_avg_best_ppd,
         init_nactives = init_nactives_ppd,
         init_nretirees = init_nretirees_ppd,

         i    = i_ppd,
         infl = infl_ppd, 
         # prod value
         # startingSal_growth
         
  
         asset_year       = asset_year_ppd,
         amort_openclosed = amort_openclosed_ppd,
         amort_pctdol     = amort_pctdol_ppd,
         amort_year       = amort_year_ppd,
         # salgrowth_amort  = 
         EEC = EEC_ppd,
         EEC_pct.current = EEC_rate_ppd_calc
  ) %>% 
  mutate(amort_year = round(amort_year),
         EEC_pct.entrants = EEC_pct.current,
         fy_end = str_extract(AL_ppd_src, "\\d+") %>% as.numeric
         )

# PPDsingleValues_forLargePlans %>% select(-EEC, -planName)
# PPDsingleValues_forPPDplans %>% head

#**************************************************************************************
#                         Check data quality                                       ####
#**************************************************************************************

# We only model plans in which the following vaiables are non-missing:
#  AL, payroll, FR_MAV, FR_AAV, B, init_actives, init_nretirees, 

planid_ppdAll <- PPDsingleValues_forPPDplans$ppd_id
planid_small  <- setdiff(planid_ppdAll, planid_large)

PPDsingleValues_forSmallPlans <-  
  PPDsingleValues_forPPDplans %>% 
  filter(ppd_id %in% planid_small) %>% 
  mutate(include = !is.na(AL)& !is.na(payroll)& !is.na(FR_MAV)& !is.na(FR_AAV)& !is.na(B)& !is.na(init_nactives)& !is.na(init_nretirees))


#**************************************************************************************
#         Fill missing values with backup values (except AL_act, AL_retired)       ####
#**************************************************************************************
# varialbes that need backup value:
 # infl, asset_year, amort_openclosed, amort_pctdol, amort_year, EEC_pct.current,  EEC_pct.current, EEC_pct.entrants

# Temp: use temparary values before checking AV CAFR for actual values
PPDsingleValues_forSmallPlans%<>% 
  mutate(infl = ifelse(is.na(infl), mean(infl, na.rm = TRUE), infl), 
         
         amort_pctdol     = ifelse(is.na(amort_pctdol), "open", amort_pctdol),
         amort_openclosed = ifelse(is.na(amort_openclosed), "open", amort_openclosed),
         amort_year       = ifelse(is.na(amort_year), 15, amort_year),
         asset_year       = ifelse(is.na(asset_year), 5, asset_year),
         
         EEC_pct.current = ifelse(is.na(EEC_pct.current), mean(EEC_pct.current, na.rm = TRUE), EEC_pct.current ), 
         EEC_pct.entrants= EEC_pct.current,
         
         prod = 0.0075,
         
         startingSal_growth = infl + prod,
         salgrowth_amort    = infl + prod,
         
         amort_year = ifelse(ppd_id == 101, 20, amort_year) # South Dakota RS see AV2016 n60. (Frozen EAN)
         
         )

# PPDsingleValues_forSmallPlans %>%
#  summarise_all(funs(sum(!is.na(.))))

#**************************************************************************************
#                Use NMR values for benefit provisions                             ####
#**************************************************************************************

data_dir_backup  <- "./Inputs_Data_std/"
file_name_backup <- "Data_backupValues.xlsx"

singleValues_benefitProv_backup <- 
  read_ExcelRange(paste0(data_dir_backup, file_name_backup), sheet = "benefitProvisions")



#**************************************************************************************
#                Combine single value variables                                    ####
#**************************************************************************************


inputsSingleValues_smallPlans <- 
  PPDsingleValues_forSmallPlans %>% 
  mutate(benProv_join  = "benProv_join") %>% 
  left_join(singleValues_benefitProv_backup) %>% 
  select(-benProv_join) 


#**************************************************************************************
#                     Estiamte AL_active, AL_retired                                  ####
#**************************************************************************************

# Use naive estimate for now: 
 # AL_act 40%,
 # AL_retired 60%

inputsSingleValues_smallPlans %<>% 
  mutate(AL_active  = AL * 0.4,
         AL_retired = AL * 0.6)


#**************************************************************************************
#                                      decrements                                  ####
#**************************************************************************************
# Use type average decrements based on large plans
# Mortality: use RP2014


data_dir  <- "./Inputs_Data_largePlans/"
file_name <- "largePlans_dataOutputs.RData"
load(paste0(data_dir, file_name))

dir_mortality <- "./Inputs_Data_std/RP2014/"
file_mortality <- "mortality_RP2014_PPD.RData"
load(paste0(dir_mortality, file_mortality))


decrements_smallPlans <- expand.grid(ppd_id = planid_small, age = 20:110, ea = 20:74) %>% 
  filter(age >= ea) %>% 
  left_join(inputsSingleValues_smallPlans %>% select(ppd_id, plantype)) %>% 
  left_join(largePlans_dataOutputs$largePlans_retRates_byType  %>% select(plantype, ea, age, qxr = retrate_average))  %>% 
  left_join(largePlans_dataOutputs$largePlans_termRates_byType %>% select(plantype, ea, age, qxt = termrate_average)) %>% 
  left_join(largePlans_dataOutputs$largePlans_disbRates_byType %>% select(plantype, ea, age, qxd = disbrate_average)) %>% 
  left_join(mortality_RP2014_PPD) %>% 
  arrange(ppd_id, ea, age)

#decrements %>% head


#**************************************************************************************
#                                      initial actives                                ####
#**************************************************************************************

init_actives_smallPlans <- 
  expand.grid(ppd_id = planid_small, age = 20:74, ea = 20:74) %>% 
  filter(age >= ea) %>% 
  left_join(inputsSingleValues_smallPlans %>% 
            select(ppd_id, plantype, init_nactives, payroll)) %>% 
  left_join(largePlans_dataOutputs$largePlans_actives_scales_byType %>% 
              select(plantype, age, ea, nactives_scale.type, salary_scale.type )) %>% 
  group_by(ppd_id) %>% 
  mutate(nactives = na2zero(nactives_scale.type * unique(init_nactives)/sum(nactives_scale.type, na.rm = TRUE)),
         salary   = na2zero(salary_scale.type * unique(payroll)/sum(salary_scale.type * nactives, na.rm = TRUE))) %>% 
  select(ppd_id, age, ea, nactives, salary) %>% 
  arrange(ppd_id, ea, age) %>% 
  ungroup()


#**************************************************************************************
#                                     initial retirees                             ####
#**************************************************************************************

  init_retirees_smallPlans <- 
    expand.grid(ppd_id = planid_small, age = 21:110) %>% 
    left_join(inputsSingleValues_smallPlans %>% 
                select(ppd_id, plantype, init_nretirees, B)) %>% 
    left_join(largePlans_dataOutputs$largePlans_retirees_scales_byType %>% 
                select(plantype, age, nretirees_scale.type, benefit_scale.type )) %>% 
    group_by(ppd_id) %>% 
    mutate(nretirees = na2zero(nretirees_scale.type * unique(init_nretirees)/sum(nretirees_scale.type, na.rm = TRUE)),
           benefit   = na2zero(benefit_scale.type * unique(B)/sum(benefit_scale.type * nretirees, na.rm = TRUE))) %>% 
    select(ppd_id, age, nretirees, benefit) %>% 
    arrange(ppd_id, age) %>% 
  ungroup()  
  
  
#**************************************************************************************
#                    Unrecognized returns                                   ####
#**************************************************************************************

get_unrecReturns <- function(ppd_id, fy_end, unrec_tot, asset_year){
    # ppd_id = 1
    # fy_end = 2016
    # unrec_tot = 100
    # asset_year = 5
    if(asset_year == 1){
      df_out <- data.frame(year = fy_end + 1, DeferredReturn = 0)
    } else {
      df_out <- 
      data.frame(year = fy_end + seq_len(asset_year - 1),
                 DeferredReturn = unrec_tot / (asset_year - 1)) %>% 
        mutate(ppd_id = ppd_id) %>% 
        select(ppd_id, everything())
    }
  df_out
  }
  
df_unrecReturns <- 
    inputsSingleValues_smallPlans %>% 
    select(ppd_id, fy_end, AL, FR_MAV, FR_AAV, asset_year) %>% 
    mutate(MAV = AL * FR_MAV,
           AAV = AL * FR_AAV,
           unrec_tot = MAV - AAV,
           asset_year = ifelse(asset_year == 0, 1, asset_year))
  #asset_year = ifelse(is.na(asset_year), 5, asset_year))

unrecReturns_smallPlans <- 
  bind_rows(
    mapply(get_unrecReturns, 
           ppd_id = df_unrecReturns$ppd_id,
           fy_end = df_unrecReturns$fy_end,
           unrec_tot = df_unrecReturns$unrec_tot,
           asset_year = df_unrecReturns$asset_year,
           SIMPLIFY = FALSE)
    )

inputsSingleValues_smallPlans %>% filter(amort_year == 0)


#**************************************************************************************
#                    Initial amortization basis                                    ####
#**************************************************************************************


amort_LG_balance <- function(ppd_id, fy, p, j, m, g, year.pass, method = "cd"){
  # m <- 10
  # p <- 100
  # i <- 0.075
  # g <- 0.03
  # year.pass <- 3
  
  if(m != 0){
  payments <- amort_LG(p, j, m, g, method = method)
  balance <- numeric(m+1)
  balance[1] <- p 
  for(k in 2:(m+1)){
    balance[k] = (balance[k-1] - payments[k-1]) * (1+j)
  }
  df_out <- data.frame(ppd_id = ppd_id, fy = fy, balance = balance[year.pass+1])

  } else df_out <- data.frame(ppd_id = ppd_id, fy = fy, balance = 0)
  df_out
}

init_amort_smallPlans <- 
  PPD_data_forInitAmort %>% 
  filter(ppd_id %in% as.numeric(str_extract(planid_small, "\\d+"))) %>% 
  group_by(ppd_id) %>% 
  left_join(PPDsingleValues_forSmallPlans %>% 
              select(ppd_id, i, amort_year, salgrowth_amort )) %>% 
  mutate(year.remaining = amort_year - (max(fy) - fy ),
         year.pass      = amort_year - year.remaining) %>% 
  select(ppd_id, fy, i, amort_year, salgrowth_amort, year.pass, year.remaining)

# init_amort_smallPlans
  
amort_balance_1 <- 
  mapply(amort_LG_balance, 
         init_amort_smallPlans$ppd_id, 
         init_amort_smallPlans$fy,
         1,
         init_amort_smallPlans$i,
         init_amort_smallPlans$amort_year,
         init_amort_smallPlans$salgrowth_amort,
         init_amort_smallPlans$year.pass, 
         SIMPLIFY = F) %>% 
  bind_rows() # %>% 
  #rename(balance_pct = balance)

init_amort_smallPlans %<>%
  left_join(amort_balance_1) 


## finalize output
init_amort_smallPlans %<>% 
  select(ppd_id,  balance, year.remaining, year.est = fy, init_period = amort_year) %>% 
  filter(!is.na(balance))




#**************************************************************************************
#                                 salary growth                                    ####
#**************************************************************************************

salgrowth_smallPlans <- 
  expand.grid(ppd_id = planid_small, age = 20:74, ea = 20:74) %>% 
  filter(age >= ea) %>% 
  left_join(inputsSingleValues_smallPlans %>% 
              select(ppd_id, plantype)) %>% 
  left_join(largePlans_dataOutputs$largePlans_salaryScale_byType) %>% 
  select(ppd_id, ea, age, plantype, salgrowth = grate_average)
# salgrowth_smallPlans 


#**************************************************************************************
#                                   saving data####
#**************************************************************************************

dir_out <- "./Inputs_Data_std/planData_std/"

get_planData <- function(ppd_id_select){
  
  assign(paste0("planData_std_", ppd_id_select), 
         list(
           inputs_singleValues = inputsSingleValues_smallPlans %>% filter(ppd_id == ppd_id_select) %>% unclass,
           decrements   = decrements_smallPlans    %>% filter(ppd_id == ppd_id_select),
           salgrowth    = salgrowth_smallPlans     %>% filter(ppd_id == ppd_id_select),
           init_actives = init_actives_smallPlans  %>% filter(ppd_id == ppd_id_select),
           init_retirees = init_retirees_smallPlans %>% filter(ppd_id == ppd_id_select),
           init_amort_unadj   = init_amort_smallPlans    %>% filter(ppd_id == ppd_id_select),
           init_unrecReturns_unadj = unrecReturns_smallPlans %>% filter(ppd_id == ppd_id_select)
         )
  )
  
  save(list = paste0("planData_std_", ppd_id_select), file = paste0(dir_out, "planData_std_", ppd_id_select, ".RData"))
  
}

sapply(planid_small, get_planData)

# load("./Inputs_Data_std/planData_std/planData_std_9.RData")
# planData_std_9$init_unrecReturns





