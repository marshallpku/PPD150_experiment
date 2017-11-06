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




#**************************************************
#                Directory                  ####
#**************************************************

data_dir     <- "./Inputs_largePlans_raw/"
#data.std_dir <- "./Inputs_std_temp/"




#**************************************************
#       1. Loading single values                ####
#**************************************************

planname <- "140_LA_City_LAFPP"

# Single values
df_singeValues_raw <- 
  read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "singleValues") %>% 
  filter(!is.na(varname)) %>% 
  mutate(planname = planname)


# Create separate df for numeric and non-numereic variables. 

df_singleValues_num <- 
    df_singeValues_raw %>% 
    filter(varType %in% c("numeric", "logical")) %>% 
    mutate(value = as.numeric(value),
           value = ifelse(category == "targets", value * units, value),
           FY    = str_extract("Jun 30 2016", "\\d{4}") %>% as.numeric) %>%
    select(planname, category, varname, description, value, FY)

df_singleValues_num
      
df_singleValues_cha <- 
  df_singeValues_raw %>% 
  filter(!varType %in% c("numeric", "logical")) %>% 
  mutate(FY    = str_extract("Jun 30 2016", "\\d{4}") %>% as.numeric) %>%
  select(planname, category, varname, description, value, FY)
df_singleValues_chr



#****************************************************************
#      2. Loading grouped actives data                       ####
#****************************************************************

get_bound <- function(range, bound = c("l","u")){
  # range must be in the form of "XX-YY", "X" is a single digit, and "XX" <= "YY".
  switch(bound,
         l =  str_extract(range, "\\d+-") %>% gsub("\\D+", "",.) %>% as.integer,
         u =  str_extract(range, "-\\d+") %>% gsub("\\D+", "",.) %>% as.integer)
}

# Steps:
# 1. loading raw data
# 2. Get colum groups
# 3. get the data in long format.

# 1. Loading raw data
 df_ActivesSched_raw <- 
  read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "ActivesSched") %>% 
  filter(!is.na(type))

df_ActivesSched_raw

 # Get indeces for columns and rows. In most cases row index is age and column index is yos.  
actives_col_index <- df_ActivesSched_raw$type[grepl("_grp", df_ActivesSched_raw$type)] %>% str_replace("_grp","")
actives_row_index <- names(df_ActivesSched_raw)[grepl("_grp", names(df_ActivesSched_raw))] %>% str_replace("_grp","")


# 2. Get column groups

col_cuts <- df_ActivesSched_raw %>% filter(str_detect(type, "_grp")) %>%
  select(starts_with("X")) %>%
  gather(col_cell, col_grp) %>%
  mutate(col_cell=as.integer(gsub("[^0-9]", "", col_cell)),
         col_lb = get_bound(col_grp, "l"),
         col_ub = get_bound(col_grp, "u"))
col_cuts


# 3. Get the data in long format 

df_ActivesSched <- 
df_ActivesSched_raw %>% 
  filter(!str_detect(type, "_grp"))  %>% 
  rename_(row_cell = paste0(actives_row_index,"_cell"),
          row_grp  = paste0(actives_row_index,"_grp")) %>% 
  select(type, row_cell, row_grp, starts_with("X")) %>%
  gather(col_cell, value, -type, -row_cell, -row_grp) %>%
  mutate(col_cell = as.integer(gsub("[^0-9]", "", col_cell)),
         row_cell = as.integer(row_cell),
         value    = as.numeric(value),
         #age = age.cell,
         #yos = yos.cell,
         planname = planname) %>%
  filter(!is.na(value)) %>% 
  spread(type, value) %>%
  left_join(col_cuts %>% select(col_cell, col_grp)) %>% 
  arrange(row_cell, col_cell) %>% 
  select(planname, row_cell, row_grp, col_cell, col_grp, nactives, salary) 

df_ActivesSched



#****************************************************************
#      3. Loading  beneficiaries data                        ####
#****************************************************************

df_RetireeSched_raw <- 
  read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "RetireesSched") %>% 
  filter(!is.na(age_grp))

df_RetireeSched <- df_RetireeSched_raw



#****************************************************************
#      4. Loading salary growth data                        ####
#****************************************************************

SalaryGrowthType <- df_singleValues_char[df_singleValues_char$varname == "SalaryGrowthType", "value"]

if(SalaryGrowthType != "Matrix"){
  
df_SalaryGrowth_raw <- 
  read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "SalaryGrowthSched_SingleCol") %>% 
  filter(!is.na(grate))

df_SalaryGrowth <- df_SalaryGrowth_raw
} 

df_SalaryGrowth



#****************************************************************
#      5.  Loading termination rate data                     ####
#****************************************************************

TermRatesType <- df_singleValues_char[df_singleValues_char$varname == "TermRatesType", "value"]

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
  
df_TermRatesSched <- expand.grid(age = 20:74, ea = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  left_join(df_TermRatesSched_singleCol)

if(TermRatesType == "LowYOS"){
  df_TermRatesSched %<>% 
    left_join(df_TermRatesSched_LowYOS) %>% 
    mutate(termrate = ifelse(yos <= LowYOS_cut, termrate.lowYOS, termrate)) %>% 
    select(-termrate.lowYOS)
}
  
df_TermRatesSched


#****************************************************************
#      6.  Loading retirement rate data                     ####
#****************************************************************

RetRatesType <- df_singleValues_char[df_singleValues_char$varname == "RetRatesType", "value"]

if(RetRatesType != "Matrix"){
  
  # Loading Data
  df_RetRatesSched_singleCol <- 
    read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "RetirementRatesSched_SingleCol") %>% 
    filter(!is.na(retrate))

  } 

df_RetRatesSched <- df_RetRatesSched_singleCol

df_RetRatesSched


#****************************************************************
#      7.  Loading disability data                     ####
#****************************************************************

DisbRatesType <- df_singleValues_char[df_singleValues_char$varname == "DisbRatesType", "value"]

if(DisbRatesType != "Matrix"){
  
  # Loading Data
  df_DisbRatesSched_singleCol <- 
    read_ExcelRange(paste0(data_dir, planname, ".xlsx"), "DisbRatesSched_SingleCol") %>% 
    filter(!is.na(disbrate))
  
  # Specify range for yos or age
  DisbRatesSched_range <- data.frame(age = 20:74) 
  
  # Data process: PLAN SPECIFIC 
  disb.min <- df_DisbRatesSched_singleCol$age %>% min
  disb.max <- df_DisbRatesSched_singleCol$age %>% max
  
  df_DisbRatesSched_singleCol <- splong(df_DisbRatesSched_singleCol, "age", disb.min:disb.max) 
  
  # Merging Data
  df_DisbRatesSched_singleCol <-  
    DisbRatesSched_range %>% 
    left_join(df_DisbRatesSched_singleCol)
  
  # Final Processing: PLAN SPECIFIC 
  df_DisbRatesSched_singleCol %<>% 
    mutate(disbrate = ifelse(age<=disb.max, disbrate, disbrate[age == disb.max] )) # plan specific
} 

## Low YOS values 
 # --


## Create the final table (age by yos)
df_DisbRatesSched <- df_DisbRatesSched_singleCol




#**************************************************
#                 Saving Data                  ####
#**************************************************

data_largePlan <- list(
  planname           = planname,
  df_singleValues_num = df_singleValues_num,
  df_singleValues_char = df_singleValues_char,
  
  df_ActivesSched     = df_ActivesSched,
  df_RetireeSched     = df_RetireeSched,
  
  df_SalaryGrowth     = df_SalaryGrowth,
  df_TermRatesSched   = df_TermRatesSched,
  df_RetRatesSched    = df_RetireeSched,
  df_DisbRatesSched   = df_DisbRatesSched
)

save(data_largePlan, file = paste0(data_dir, "DataLargePlan_", planname,  ".RData"))





