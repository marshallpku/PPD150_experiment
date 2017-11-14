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
  
  # AL
  # AL_act
  # AL_retired

  # payroll
  # FR_MAV
  # FR_AAV
  # B
  # init_nactives
  # init_retirees
  
  # i
  # prod
  # infl
  # startingSal_growth
  
  # bfactor_current
  # bfactor_entrants
  # vest_yos
  # retage_normal
  # retage_early
  # retage_max
  # earlyRet_reduction
  # cola
  # FAS_year
  
  # asset_year
  # amort_openclosed
  # amort_pctdol
  # amort_year
  # salgrowth_amort
  # EEC_pct.current
  # EEC_pct.entrants
  # ERC_restriction

  ## optional vars
  # PVB
  # PVB_active
  # PVB_retired
  # UAAL

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
#                Loading data                  ####
#**************************************************






















