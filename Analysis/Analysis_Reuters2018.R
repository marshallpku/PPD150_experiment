
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
library(xlsx)
library("btools")
options(dplyr.print_min = 60) # default is 10

source("Functions.R")
source("Functions_DataTools.R")


#**************************************************
#                  Notes                     ####
#**************************************************

# PPD data:
library(pdata)
load("Inputs_Data_PPD/DataPPD.RData")

ppd_2018 <- read_excel("Inputs_Data_PPD/PPD_PlanLevel_Feb2018.xlsx")

df <- 
ppd %>% select(PlanName, fy, 
               alternatives, 
               equities_tot,
               equities_domestic,
               FixedIncome_tot,
               RealEstate,
               other,
               ActFundedRatio_GASB) %>% 
  group_by(fy)

Var <- quo(alternatives)
Var <- quo(equities_tot)
Var <- quo(equities_domestic)
Var <- quo(RealEstate)
Var <- quo(FixedIncome_tot)
Var <- quo(FixedIncome_domestic)
Var <- quo(ActFundedRatio_GASB)

df %>% 
  # select_(PlanName, fy )
  summarise(
    q25 = 100*quantile(!!Var,0.25, na.rm = TRUE),
    q50 = 100*median(!!Var , na.rm = TRUE),
    q75 = 100*quantile(!!Var,0.75, na.rm = TRUE),
    n.nonNA = sum(!is.na(!!Var))) %>% 
  kable(digits = 1)













  
  














