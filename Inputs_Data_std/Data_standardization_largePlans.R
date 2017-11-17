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

planNames_all <- c(planNames_general, planNames_safety, planNames_teacher)


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
         EEC_pct.current = EEC_rate_ppd_calc,
         
         init_actives = init_nactives_ppd,
         init_nretirees = init_nretirees_ppd
  ) %>% 
  mutate(EEC_pct.entrants = EEC_pct.current)

# PPDsingleValues_forLargePlans %>% select(-EEC, -planName)


# Temp: use temparary values before checking AV CAFR for actual values
PPDsingleValues_forLargePlans %<>% 
  mutate(amort_openclosed = ifelse(is.na(amort_openclosed), "open", amort_openclosed),
         amort_year       = ifelse(is.na(amort_year), 15, amort_year),
         asset_year       = ifelse(is.na(asset_year), 5, asset_year))






#**************************************************************************************
#                Use NMR values for benefit provisions                             ####
#**************************************************************************************

data_dir_backup  <- "./Inputs_Data_std/"
file_name_backup <- "Data_backupValues.xlsx"

singleValues_benefitProv_backup <- 
  read_ExcelRange(paste0(data_dir_backup, file_name_backup), sheet = "benefitProvisions" )




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

# init_actives %>% filter(ea == 20, age ==20)
# init_actives %>% filter(ppd_id == 125)

init_retirees <- 
  largePlans_dataOutputs$largePlans_retirees_scales_byPlan %>% 
  rename(ppd_planname = planname) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, age, nretirees, benefit) %>% 
  filter(age>=21, age<=110) %>% 
  ungroup

# init_nretirees %>% filter(ppd_id == 78)

salgrowth <- 
  largePlans_dataOutputs$largePlans_salaryScale_byPlan %>% 
  rename(ppd_planname = planname,
         salgrowth = grate) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, ea, age, salgrowth) %>% 
  ungroup

# salgrowth %>% filter(ea == 20, age ==20)


decrements <- expand.grid(planname = planNames_all, age = 20:110, ea = 20:74) %>% 
  filter(age >= ea) %>% 
  left_join(largePlans_dataOutputs$largePlans_retRates_byPlan  %>% select(planname, ea, age, qxr = retrate))  %>% 
  left_join(largePlans_dataOutputs$largePlans_termRates_byPlan %>% select(planname, plantype, ea, age, qxt = termrate)) %>% 
  left_join(largePlans_dataOutputs$largePlans_disbRates_byPlan %>% select(planname, ea, age, qxd = disbrate)) %>% 
  rename(ppd_planname = planname) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, plantype, everything()) %>% 
  arrange(plantype,  ppd_planname)

# decrements %>% filter(ea == 20, age ==20)



inputs_singleValues_largePlans_num <- 
largePlans_dataOutputs$largePlans_singleValues_num %>% 
  select(ppd_planname = planname, plantype, FY, varname, value) %>%
  group_by(ppd_planname) %>% 
  mutate(FY = FY[!is.na(FY)] %>% max ) %>% 
  spread(varname, value) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, plantype, FY,
         AL_active = AL_active,
         AL_retired,
         AL = AL_total,
         payroll,
         
         infl = inflation,
         prod = prod_growth,
         salgrowth_amort = payroll_growth,
         
         PVB_act = PVB_active,
         PVB_retired,
         PVFNC_act = PVFNC_active) %>% 
  mutate(AL_retired_old = AL_retired,
         AL_retired = AL - AL_active,
         startingSal_growth = infl + prod) %>% 
  ungroup()
        
inputs_singleValues_largePlans_chr <- 
largePlans_dataOutputs$largePlans_singleValues_chr %>% 
  select(ppd_planname = planname, varname, value) %>%
  group_by(ppd_planname) %>% 
  spread(varname, value) %>% 
  mutate(ppd_id = str_extract(ppd_planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, ppd_planname, plantype,
         erc_rule) %>% 
  ungroup()



#**************************************************************************************
#                         Add mortality table                                    ####
#**************************************************************************************
dir_mortality <- "./Inputs_Data_std/RP2014/"
file_mortality <- "mortality_RP2014_PPD.RData"
load(paste0(dir_mortality, file_mortality))

decrements %<>%
  left_join(mortality_RP2014_PPD)

mortality_RP2014_PPD %>% head

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

# inputsSingleValues %>% names
# inputsSingleValues %>% 
#   select(planName, AL, AL_ppd, payroll, payroll_ppd)




#**************************************************************************************
#          Fill in missing values in large plan data from AVs and CAFRsL: Part1    ####
#**************************************************************************************

## Fill missing values: sinle values
inputs_singleValues_largePlans_num %>% 
  mutate(prod = ifelse(is.na(prod), mean(prod, na.rm = TRUE), prod),
         salgrowth_amort = ifelse(is.na(salgrowth_amort), infl + prod, salgrowth_amort))

inputs_singleValues_largePlans_num
inputs_singleValues_largePlans_chr


## Fill in missing values: Retirement rates for "10_CA_CA-CALSTRS"
 # use average rates for teachers plan
decrements %<>% 
  left_join(largePlans_dataOutputs$largePlans_retRates_byType %>% 
              filter(plantype == "teacher") %>%  
              select(ea, age, retrate_average)) %>% 
  mutate(qxr = ifelse(ppd_id == 10, retrate_average, qxr)) %>% 
  select(-retrate_average)
decrements %>% filter(ea == 20, age ==20)



## distribution of init retirees for 10_CA_CA-CALSTRS and 78_NY_NY-NYSTRS
 # Use init_retirees and B from PPD and scales based on type averages 

init_retirees %<>% 
  left_join(inputsSingleValues  %>% 
              select(ppd_id, plantype, init_nretirees, B)) %>% 
  left_join(largePlans_dataOutputs$largePlans_retirees_scales_byType %>% 
              select(plantype, age, nretirees_scale.type, benefit_scale.type )) %>% 
  group_by(ppd_id) %>% 
  mutate(nretirees = ifelse(ppd_id %in% c(10, 78), nretirees_scale.type * unique(init_nretirees)/sum(nretirees_scale.type, na.rm = T), nretirees),
         benefit   = ifelse(ppd_id %in% c(10, 78), benefit_scale.type * unique(B)/sum(benefit_scale.type * nretirees, na.rm = T), benefit)) %>% 
  select(ppd_id, ppd_planname, age, nretirees, benefit) %>% 
  ungroup()

# init_nretirees %>% 
#   filter(ppd_id %in% c(10, 78)) %>% 
#   group_by(ppd_id) %>% 
#   summarise(nr = sum(nretirees),
#             b  = sum(nretirees * benefit))
# 
# inputsSingleValues  %>% 
#   select(ppd_id, plantype, init_nretirees, B) %>% 
#   filter(ppd_id %in% c(10, 78))

# left_join(
# init_nretirees %>%
#   group_by(ppd_id) %>%
#   summarise(nret_AV = sum(nretirees),
#             B_AV  = sum(nretirees * benefit)),
# 
# inputsSingleValues  %>%
#   select(ppd_id, plantype, init_nretirees, B)) %>% 
#   mutate(diff_nret = nret_AV / init_nretirees,
#          diff_B    = B_AV / B)
#   




## distribution of initial salaries for 
 # 10_CA_CA-CALSTRS; 
 # 83_NY_NY-ERS
 # 84_NY_NY-PFRS
 # 125_WI_WI-ETF
 # Use payroll from PPD and scales based on type averages 

init_actives %<>% 
  left_join(inputsSingleValues  %>% 
              select(ppd_id, plantype, payroll)) %>% 
  left_join(largePlans_dataOutputs$largePlans_actives_scales_byType %>% 
              select(plantype, age, ea, salary_scale.type )) %>% 
  group_by(ppd_id) %>% 
  mutate(salary = ifelse(ppd_id %in% c(10,83,84, 125), salary_scale.type * unique(payroll)/sum(salary_scale.type * nactives, na.rm = T), salary)) %>% 
  select(ppd_id, ppd_planname, age, ea, nactives, salary) %>% 
  ungroup()


# left_join(
# init_actives %>%
#   group_by(ppd_id) %>%
#   summarise(payroll_AV  = sum(nactives * salary),
#             nactives_AV  = sum(nactives)),
# 
# inputsSingleValues  %>%
#   select(ppd_id, plantype, payroll, init_actives) %>%
#   arrange(ppd_id)) %>% 
#   
#   mutate(diff_nact = nactives_AV / init_actives,
#          diff_PR   = payroll_AV / payroll)
#   



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
  # data.frame(year = fy_end + seq_len(asset_year),
  #            DeferredReturn = unrec_tot / asset_year) %>% 
  #   mutate(ppd_id = ppd_id) %>% 
  #   select(ppd_id, everything())
}

df_unrecReturns <- 
inputsSingleValues %>% 
  select(ppd_id, ppd_planname,fy_end,  AL, FR_MAV, FR_AAV, asset_year) %>% 
  mutate(MAV = AL * FR_MAV,
         AAV = AL * FR_AAV,
         unrec_tot = MAV - AAV,
         asset_year = ifelse(asset_year == 0, 1, asset_year))
         #asset_year = ifelse(is.na(asset_year), 5, asset_year))

unrecReturns_largePlans <- 
bind_rows(
  mapply(get_unrecReturns, 
         ppd_id = df_unrecReturns$ppd_id,
         fy_end = df_unrecReturns$fy_end,
         unrec_tot = df_unrecReturns$unrec_tot,
         asset_year = df_unrecReturns$asset_year,
         SIMPLIFY = FALSE)
)

# unrecReturns_largePlans

#**************************************************************************************
#                    Initial amortization basis                                    ####
#**************************************************************************************


data_dir_ppd  <- "./Inputs_Data_PPD/"
file_name_ppd <- "DataPPD.RData"
load(paste0(data_dir_ppd, file_name_ppd)) # PPD_trim loaded

# init_amort_largePlans %>% head

init_amort_largePlans <- 
PPD_data_forInitAmort %>% 
  filter(ppd_id %in% as.numeric(str_extract(planNames_all, "\\d+"))) %>% 
  group_by(ppd_id) %>% 
  mutate(NormCostRate_tot_fill = ifelse(rep(sum(!is.na(NormCostRate_tot)), n()) == 0, NA, spline(fy, NormCostRate_tot, xout = fy)$y),
         ReqContRate_tot_fill  = ifelse(rep(sum(!is.na(ReqContRate_tot)), n())  == 0, NA, spline(fy, ReqContRate_tot, xout = fy)$y),
         UAAL_GASB_fill        = ifelse(rep(sum(!is.na(UAAL_GASB)), n())        == 0, NA, spline(fy, UAAL_GASB, xout = fy)$y),
         payroll_fill        = ifelse(rep(sum(!is.na(payroll)), n())            == 0, NA, spline(fy, payroll, xout = fy)$y)
         ) %>% 
  select(ppd_id, PlanName, fy,
         NormCostRate_tot,
         NormCostRate_tot_fill,
         ReqContRate_tot,
         ReqContRate_tot_fill,
         UAAL_GASB,
         UAAL_GASB_fill,
         payroll,
         payroll_fill)
         
# Mannually adjust some unreasonable interpolated values
init_amort_largePlans %<>% 
  mutate(NormCostRate_tot_fill = ifelse(ppd_id == 78  & fy ==  2016, NormCostRate_tot_fill[fy == 2015], NormCostRate_tot_fill),
         NormCostRate_tot_fill = ifelse(ppd_id == 83  & fy %in% 2012:2016, NormCostRate_tot_fill[fy == 2011], NormCostRate_tot_fill),
         NormCostRate_tot_fill = ifelse(ppd_id == 140 & fy ==  2001, NormCostRate_tot_fill[fy == 2002], NormCostRate_tot_fill),
         
         ReqContRate_tot_fill  = ifelse(ppd_id == 10  & fy ==  2016, ReqContRate_tot_fill[fy == 2015], ReqContRate_tot_fill),
         ReqContRate_tot_fill  = ifelse(ppd_id == 78  & fy ==  2016, ReqContRate_tot_fill[fy == 2015], ReqContRate_tot_fill),
         ReqContRate_tot_fill  = ifelse(ppd_id == 140 & fy ==  2001, ReqContRate_tot_fill[fy == 2002], ReqContRate_tot_fill),
         
         UAAL_GASB_fill  = ifelse(ppd_id == 9   & fy ==  2016, UAAL_GASB_fill[fy == 2015], UAAL_GASB_fill),
         UAAL_GASB_fill  = ifelse(ppd_id == 86  & fy ==  2016, UAAL_GASB_fill[fy == 2015], UAAL_GASB_fill),
         UAAL_GASB_fill  = ifelse(ppd_id == 150 & fy %in% 2015:2016, UAAL_GASB_fill[fy == 2014], UAAL_GASB_fill)
         )

# Calculate estimated amoritzation basis
init_amort_largePlans %<>% 
  left_join(inputsSingleValues %>% select(ppd_id, i, amort_year, amort_openclosed, amort_pctdol, salgrowth_amort)) %>% 
  mutate(SC_est = lag(ReqContRate_tot_fill - NormCostRate_tot_fill) * payroll,
         Amort.basis_initial = UAAL_GASB_fill - (lag(UAAL_GASB_fill - SC_est)) * (1 + i))  
  

# TEMP: fill missing values in amortization methods
init_amort_largePlans %<>% 
  mutate(amort_openclosed = ifelse(is.na(amort_openclosed), "open", amort_openclosed),
         amort_year       = ifelse(is.na(amort_year), 15, amort_year))
         #salgrowth_amort  = ifelse(is.na(salgrowth_amort), 0.03, salgrowth_amort))
  

  
# Calculate remaining amortizatin balance

amort_LG_balance <- function(ppd_id, fy, p, j, m, g, year.pass, method = "cd"){
  # m <- 10
  # p <- 100
  # i <- 0.075
  # g <- 0.03
  # year.pass <- 3
  
  payments <- amort_LG(p, j, m, g, method = method)
  balance <- numeric(m+1)
  balance[1] <- p 
  for(k in 2:(m+1)){
    balance[k] = (balance[k-1] - payments[k-1]) * (1+j)
  }
  data.frame(ppd_id = ppd_id, fy = fy, balance = balance[year.pass+1])
}

init_amort_largePlans %<>%
  mutate(year.remaining = amort_year - (max(fy) - fy ),
         year.pass      = amort_year - year.remaining)

amort_balance_largePlans <- 
   mapply(amort_LG_balance, 
          init_amort_largePlans$ppd_id, 
          init_amort_largePlans$fy,
          init_amort_largePlans$Amort.basis_initial,
          init_amort_largePlans$i,
          init_amort_largePlans$amort_year,
          init_amort_largePlans$salgrowth_amort,
          init_amort_largePlans$year.pass, 
          SIMPLIFY = F) %>% 
     bind_rows()

init_amort_largePlans %<>%
  left_join(amort_balance_largePlans)


# Simplified method for 83, 84, 150
amort_balance_1 <- 
  mapply(amort_LG_balance, 
         init_amort_largePlans$ppd_id, 
         init_amort_largePlans$fy,
         1,
         init_amort_largePlans$i,
         init_amort_largePlans$amort_year,
         init_amort_largePlans$salgrowth_amort,
         init_amort_largePlans$year.pass, 
         SIMPLIFY = F) %>% 
  bind_rows() %>% 
  rename(balance_pct = balance)

init_amort_largePlans %<>%
  left_join(amort_balance_1) %>% 
  mutate(balance = ifelse(ppd_id %in% c(83, 84, 150), balance_pct, balance))


## finalize output
init_amort_largePlans %<>% 
  select(ppd_id, PlanName,  balance, year.remaining, year.est = fy, init_amount = Amort.basis_initial, init_period = amort_year) %>% 
  filter(!is.na(balance))




inputsSingleValues %>% names




#**************************************************************************************
#                                 salary growth                                    ####
#**************************************************************************************

salgrowth <- 
largePlans_dataOutputs$largePlans_salaryScale_byPlan %>% 
  mutate(ppd_id = str_extract(planname, "\\d+") %>% as.numeric()) %>% 
  select(ppd_id, plantype, ea, age, salgrowth = grate)




#**************************************************************************************
#                                   saving data####
#**************************************************************************************

dir_out <- "./Inputs_Data_std/planData_std/"

get_planData <- function(ppd_id_select){

assign(paste0("planData_std_", ppd_id_select), 
   list(
     inputs_singleValues = inputsSingleValues %>% filter(ppd_id == ppd_id_select) %>% unclass,
     decrements   = decrements  %>% filter(ppd_id == ppd_id_select),
     salgrowth    = salgrowth   %>% filter(ppd_id == ppd_id_select),
     init_actives = init_actives %>% filter(ppd_id == ppd_id_select),
     init_retirees = init_retirees%>% filter(ppd_id == ppd_id_select),
     init_amort_unadj   = init_amort_largePlans   %>% filter(ppd_id == ppd_id_select),
     init_unrecReturns_unadj = unrecReturns_largePlans %>% filter(ppd_id == ppd_id_select)
   )
  )

  save(list = paste0("planData_std_", ppd_id_select), file = paste0(dir_out, "planData_std_", ppd_id_select, ".RData"))

}

sapply(as.numeric(str_extract(planNames_all, "\\d+")), get_planData)

# load("./Inputs_Data_std/planData_std/planData_std_9.RData")
# planData_std_9$init_unrecReturns











