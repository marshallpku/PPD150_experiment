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


# For now use RP2014 mortality table with different gender ratio for different plan types:
 # general: 55% female
 # safety : 10% female
 # tacher : 70% female


#****************************************************************
#                 Loading Mortaltiy data                     ####
#****************************************************************

dir_RP2014 <- "./Inputs_Data_std/RP2014/"

# Import mortality data
data_raw_tot <- read_excel(paste0(dir_RP2014, "research-2014-rp-mort-tab-rates-exposure.xlsx"), sheet = "Total Dataset", skip = 3)[, c(-2, -6)] 
data_raw_wc  <- read_excel(paste0(dir_RP2014, "research-2014-rp-mort-tab-rates-exposure.xlsx"), sheet = "White Collar", skip = 3)[, c(-2, -5)] # exclude an empty column

names(data_raw_tot) <- c("age", "qxm.employee.M", "qxm.healthyRet.M", "qxm.disbRet.M", "qxm.employee.F", "qxm.healthyRet.F", "qxm.disbRet.F" )
names(data_raw_wc)  <- c("age", "qxm.wcEmployee.M", "qxm.wcHealthyRet.M", "qxm.wcEmployee.F", "qxm.wcHealthyRet.F")





#****************************************************************
#                 Type specific mortality                    ####
#****************************************************************

shareFemale_general <- 0.55
shareFemale_safety  <- 0.1
shareFemale_teacher <- 0.7

mortality_general <- 
  data_raw_tot %>% 
  mutate(qxm.pre  = qxm.employee.F   * shareFemale_general, qxm.employee.M   * (1 - shareFemale_general),
         qxm.post = qxm.healthyRet.F * shareFemale_general, qxm.healthyRet.M * (1 - shareFemale_general),
         qxm.d    = qxm.disbRet.F    * shareFemale_general, qxm.disbRet.M    * (1 - shareFemale_general),
         plantype = "general") %>%
  select(plantype, age, qxm.pre, qxm.post, qxm.d)
  
mortality_safety <- 
  data_raw_tot %>% 
  mutate(qxm.pre  = qxm.employee.F   * shareFemale_safety, qxm.employee.M   * (1 - shareFemale_safety),
         qxm.post = qxm.healthyRet.F * shareFemale_safety, qxm.healthyRet.M * (1 - shareFemale_safety),
         qxm.d    = qxm.disbRet.F    * shareFemale_safety, qxm.disbRet.M    * (1 - shareFemale_safety),
         plantype = "safety") %>% 
  select(plantype, age, qxm.pre, qxm.post, qxm.d)

mortality_teacher <- 
  data_raw_tot %>% 
  mutate(qxm.pre  = qxm.employee.F   * shareFemale_teacher, qxm.employee.M   * (1 - shareFemale_teacher),
         qxm.post = qxm.healthyRet.F * shareFemale_teacher, qxm.healthyRet.M * (1 - shareFemale_teacher),
         qxm.d    = qxm.disbRet.F    * shareFemale_teacher, qxm.disbRet.M    * (1 - shareFemale_teacher),
         plantype = "teacher") %>% 
  select(plantype, age, qxm.pre, qxm.post, qxm.d)



#****************************************************************
#                 combine tables                    ####
#****************************************************************

mortality_RP2014_PPD <- 
  bind_rows(mortality_general,
            mortality_safety,
            mortality_teacher) %>% 
  mutate(qxm.post = ifelse(is.na(qxm.post), qxm.pre, qxm.post),
         qxm.term = qxm.post)
  

save(mortality_RP2014_PPD, file = paste0(dir_RP2014, "mortality_RP2014_PPD.RData"))







