
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

# From Large Plan Data:
  # 1. Salary scale by type
  # 2. Retirement scale by type
  # 3. Decrement scale by type

# From PPD data:
  # Funded status, compared with all plans
  # Funding method
  # discount rate
  # Government contributions vs ADC
  # Salary



#**************************************************
#          Global settings                     ####
#**************************************************
data_dir <- "./Inputs_Data_largePlans/"


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


#**********************************************************************************************
##  Defining color and theme for publication format of Rockefeller Institute of Government ####
#**********************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}


#**************************************************
#          Loading large plan data             ####
#**************************************************

# Define file names
fileNames <- paste0(data_dir, "DataLargePlan_", planNames_all, ".RData")


# Loading all data files into a list
dataList_allPlans <- llply(fileNames, function(x) {load(x); data_list <- data_largePlan})


# Combine data for all plans for each element in the data file
get_table <- function(tableName, DataList = dataList_allPlans) ldply(DataList, function(x) x[[tableName]])
# A function that extracts and combines the same element from all component lists in dataList_allPlans
# get_table("ActivesSched") # Test

tableNames <- c("singleValues_num", "singleValues_chr", 
                "ActivesSched", "RetireeSched",
                "SalaryGrowthSched",
                "TermRatesSched",
                "RetRatesSched",
                "DisbRatesSched")
names(tableNames) <- tableNames #alply will inherit the names

dataList_allPlans2 <- alply(tableNames, 1, get_table, .dims = TRUE)

#dataList_allPlans2$DisbRatesSched %>% filter(str_detect(planname, "LAFPP"))


#**************************************************
#          # Single values                     ####
#**************************************************

largePlans_singleValues_num <- dataList_allPlans2$singleValues_num
largePlans_singleValues_chr <-dataList_allPlans2$singleValues_chr


df_largePlans_types <- 
  largePlans_singleValues_chr %>% filter(str_detect(varname, "Type")) %>% 
  select(planname, plantype, varname, value) %>%
  spread(varname, value)



#**************************************************
#                # Salary scales               ####
#**************************************************
## Weights: 
#  Plan weights within type
#  Plan weights for all
# Use nactives as weights

planWeight_nactives <- dataList_allPlans2$ActivesSched %>%
  filter(age %in% 20:74, ea %in% 20:74, age >= ea, fillin == TRUE ) %>% 
  select(planname, ea, age, nactives)


## Expanding salary scales to by age and YOS
plannames_grate_Matrix <- (largePlans_singleValues_chr %>% filter(varname == "SalaryGrowthType", value == "Matrix"))$planname
plannames_grate_byAge  <- (largePlans_singleValues_chr %>% filter(varname == "SalaryGrowthType", value == "byAge"))$planname
plannames_grate_byYOS  <- (largePlans_singleValues_chr %>% filter(varname == "SalaryGrowthType", value == "byYOS"))$planname

# Matrix
largePlans_salaryScale_Matrix <- 
  expand.grid(planname = plannames_grate_Matrix, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, ea, age) %>% 
  left_join(dataList_allPlans2$SalaryGrowthSched %>% filter(SalaryGrowthType == "Matrix"))
#largePlans_salaryScale_Matrix

# by age  
largePlans_salaryScale_byAge <- 
  expand.grid(planname = plannames_grate_byAge, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, age) %>% 
  left_join(dataList_allPlans2$SalaryGrowthSched %>% 
              filter(SalaryGrowthType == "byAge") %>% 
              select(-ea, -yos)) %>% 
  mutate(yos = age - ea)
# largePlans_salaryScale_byAge

# by YOS
largePlans_salaryScale_byYOS <- 
  expand.grid(planname = plannames_grate_byYOS, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  arrange(planname, age) %>% 
  left_join(dataList_allPlans2$SalaryGrowthSched %>% 
              filter(SalaryGrowthType == "byYOS") %>% 
              select(-ea, -age))
# largePlans_salaryScale_byAge

# Merging all types and adding weights
largePlans_salaryScale <- 
  bind_rows(largePlans_salaryScale_Matrix,
            largePlans_salaryScale_byAge,
            largePlans_salaryScale_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives)) 
largePlans_salaryScale_byPlan <- largePlans_salaryScale

# Modify grate at yos = 5 in 150 NYC PPF: 36% to 4%
largePlans_salaryScale %<>%
  mutate(grate = ifelse(planname == "150_NY_NYC-PPF" & yos == 5, 0.04, grate))



## Weighted average rates

# Average across all types
largePlans_salaryScale_all <- 
  largePlans_salaryScale %>% #filter(planname != "9_CA_CA-CALPERS") %>%
  group_by(ea, age) %>% 
  summarize(grate_weighted = weighted.mean(grate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            grate_average  = mean(grate, na.rm = TRUE)) %>% 
  mutate(plantype = "alltypes")

largePlans_salaryScale_all %>% filter(ea == 55)

# Average by type
largePlans_salaryScale_type <- 
  largePlans_salaryScale %>% 
  group_by(plantype, ea, age) %>% 
  summarize(grate_weighted = weighted.mean(grate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            grate_average  = mean(grate, na.rm = TRUE))

largePlans_salaryScale_type %>% filter(plantype == "general", ea == 25)


largePlans_salaryScale <- 
  bind_rows(largePlans_salaryScale_all,
            largePlans_salaryScale_type) %>% 
  select(plantype, everything()) 


##  Checking results
largePlans_salaryScale %>%
  group_by(plantype, ea) %>% 
  filter(age %in% (unique(ea) + seq(0, 50, 10)))  %>% 
  as.data.frame

# compariosn across types
largePlans_salaryScale %>% 
  filter(ea %in% c(25), age %in% 20:60,
         plantype != "alltypes") %>% 
  ggplot(aes(x =age, y = grate_average, color  = plantype)) + 
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() + 
  geom_point() 



largePlans_salaryScale_byPlan %>% 
  filter(ea %in% c(25), age %in% 20:60,
         plantype == "teacher") %>% 
  ggplot(aes(x =age, y = grate, color  = planname)) + 
  #facet_wrap( ~ ea, ncol = 3) +
  geom_line() + 
  geom_point() 

largePlans_salaryScale_byPlan %>% 
  filter(ea %in% c(25), age %in% 20:60,
         plantype == "general") %>% 
  ggplot(aes(x =age, y = grate, color  = planname)) + 
  #facet_wrap( ~ ea, ncol = 3) +
  geom_line() + 
  geom_point() 




#**************************************************
#                # Retirement rates            ####
#**************************************************

## Weights: 
#  Plan weights within type
#  Plan weights for all
# Use nactives as weights

planWeight_nactives <- dataList_allPlans2$ActivesSched %>%
  filter(age %in% 20:74, ea %in% 20:74, age >= ea, fillin == TRUE ) %>% 
  select(planname, ea, age, nactives)


## Expanding salary scales to by age and YOS
plannames_retrate_Matrix <- (largePlans_singleValues_chr %>% filter(varname == "RetRatesType", value == "Matrix"))$planname
plannames_retrate_byAge  <- (largePlans_singleValues_chr %>% filter(varname == "RetRatesType", value == "byAge"))$planname
plannames_retrate_byYOS  <- (largePlans_singleValues_chr %>% filter(varname == "RetRatesType", value == "byYOS"))$planname

plannames_retrate_Matrix 
plannames_retrate_byAge  
plannames_retrate_byYOS  



# Matrix
largePlans_retRates_Matrix <- 
  expand.grid(planname = plannames_retrate_Matrix, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, ea, age) %>%
  left_join(df_largePlans_types %>% select(planname, plantype, RetRatesType)) %>% 
  left_join(dataList_allPlans2$RetRatesSched %>% 
              filter(RetRatesType == "Matrix") %>% 
              select(-plantype, RetRatesType))
largePlans_retRates_Matrix 

# by age  
largePlans_retRates_byAge <- 
  expand.grid(planname = plannames_retrate_byAge, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, age) %>% 
  left_join(df_largePlans_types %>% select(planname, plantype, RetRatesType)) %>% 
  left_join(dataList_allPlans2$RetRatesSched %>% 
              filter(RetRatesType == "byAge") %>% 
              select(-ea, -yos, -plantype, RetRatesType)) %>% 
  mutate(yos = age - ea)
# largePlans_retRates_byAge

# by YOS
largePlans_retRates_byYOS <- 
  expand.grid(planname = plannames_retrate_byYOS, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  arrange(planname, age) %>% 
  left_join(df_largePlans_types %>% select(planname, plantype, RetRatesType)) %>% 
  left_join(dataList_allPlans2$RetRatesSched %>% 
              filter(RetRatesType == "byYOS") %>% 
              select(-ea, -age, -plantype, RetRatesType))
# largePlans_retRates_byYOS 

# Merging all types and adding weights
largePlans_retRates <- 
  bind_rows(largePlans_retRates_Matrix,
            largePlans_retRates_byAge,
            largePlans_retRates_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives),
         retrate   = na2zero(retrate)) 
# largePlans_retRates %>% filter(is.na(plantype))
largePlans_retRates_byPlan <- largePlans_retRates

## Weighted average rates

# Average across all types
largePlans_retRates_all <- 
  largePlans_retRates %>% #filter(planname != "9_CA_CA-CALPERS") %>%
  group_by(ea, age) %>% 
  summarize(retrate_weighted = weighted.mean(retrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            retrate_average  = mean(retrate, na.rm = TRUE)) %>% 
  mutate(plantype = "alltypes")
# largePlans_retRates_all %>% filter(ea == 55)

# Average by type
largePlans_retRates_type <- 
  largePlans_retRates %>% 
  group_by(plantype, ea, age) %>% 
  summarize(retrate_weighted = weighted.mean(retrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            retrate_average  = mean(retrate, na.rm = TRUE))
# largePlans_retRates_type %>% filter(plantype == "general", ea == 25)


largePlans_retRates <- 
  bind_rows(largePlans_retRates_all,
            largePlans_retRates_type) %>% 
  select(plantype, everything()) 


# ##  Checking results
largePlans_retRates %>%
  group_by(plantype, ea) %>%
  filter(age %in% (unique(ea) + seq(0, 50, 10)))  %>%
  as.data.frame

# compariosn across types
largePlans_retRates %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60) %>%
  ggplot(aes(x =age, y = retrate_average, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()




largePlans_retRates %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60,
         plantype != "alltypes") %>%
  ggplot(aes(x =age, y = retrate_weighted, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

largePlans_retRates_byPlan %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60,
         plantype == "teacher") %>%
  ggplot(aes(x =age, y = retrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


largePlans_retRates_byPlan %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60,
         plantype == "general") %>%
  ggplot(aes(x =age, y = retrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()



# Teachers plan Late retirement 
largePlans_retRates %>%
  filter(ea %in% c(30), age %in% 20:60,
         plantype != "alltypes") %>%
  ggplot(aes(x =age, y = retrate_weighted, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()




#**************************************************
#                # Termination rates            ####
#**************************************************

## Weights: 
#  Plan weights within type
#  Plan weights for all
# Use nactives as weights

planWeight_nactives <- dataList_allPlans2$ActivesSched %>%
  filter(age %in% 20:74, ea %in% 20:74, age >= ea, fillin == TRUE ) %>% 
  select(planname, ea, age, nactives)


## Expanding salary scales to by age and YOS
plannames_termrate_Matrix <- (largePlans_singleValues_chr %>% filter(varname == "TermRatesType", value %in% c("Matrix", "LowYOS")))$planname
plannames_termrate_byAge  <- (largePlans_singleValues_chr %>% filter(varname == "TermRatesType", value == "byAge"))$planname
plannames_termrate_byYOS  <- (largePlans_singleValues_chr %>% filter(varname == "TermRatesType", value == "byYOS"))$planname

plannames_termrate_Matrix 
plannames_termrate_byAge  
plannames_termrate_byYOS  


# Matrix and lowYOS
largePlans_termRates_Matrix <- 
  expand.grid(planname = plannames_termrate_Matrix, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, ea, age) %>% 
  left_join(dataList_allPlans2$TermRatesSched %>% 
              filter(TermRatesType %in% c("Matrix", "LowYOS")))
#largePlans_TermRates_Matrix

# by age  
largePlans_termRates_byAge <- 
  expand.grid(planname = plannames_termrate_byAge, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, age) %>% 
  left_join(dataList_allPlans2$TermRatesSched %>% 
              filter(TermRatesType == "byAge") %>% 
              select(-ea, -yos)) %>% 
  mutate(yos = age - ea)
# largePlans_salaryScale_byAge

# by YOS
largePlans_termRates_byYOS <- 
  expand.grid(planname = plannames_termrate_byYOS, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  arrange(planname, age) %>% 
  left_join(dataList_allPlans2$TermRatesSched %>% 
              filter(TermRatesType == "byYOS") %>% 
              select(-ea, -age))
# largePlans_retRates_byYOS


# Merging all types and adding weights
largePlans_termRates <- 
  bind_rows(largePlans_termRates_Matrix,
            largePlans_termRates_byAge,
            largePlans_termRates_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives),
         rerate   = na2zero(termrate)) 
largePlans_termRates_byPlan <- largePlans_termRates 


## Weighted average rates

# Average across all types
largePlans_termRates_all <- 
  largePlans_termRates %>% #filter(planname != "9_CA_CA-CALPERS") %>%
  group_by(ea, age) %>% 
  summarize(termrate_weighted = weighted.mean(termrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            termrate_average  = mean(termrate, na.rm = TRUE)) %>% 
  mutate(plantype = "alltypes")

largePlans_termRates_all %>% filter(ea == 55)

# Average by type
largePlans_termRates_type <- 
  largePlans_termRates %>% 
  group_by(plantype, ea, age) %>% 
  summarize(termrate_weighted = weighted.mean(termrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            termrate_average  = mean(termrate, na.rm = TRUE))

largePlans_termRates_type %>% filter(plantype == "general", ea == 25)


largePlans_termRates <- 
  bind_rows(largePlans_termRates_all,
            largePlans_termRates_type) %>% 
  select(plantype, everything()) 



# ##  Checking results
largePlans_termRates %>%
  group_by(plantype, ea) %>%
  filter(age %in% (unique(ea) + seq(0, 50, 10)))  %>%
  as.data.frame

# compariosn across types
largePlans_termRates %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype != "alltypes") %>%
  ggplot(aes(x =age, y = termrate_weighted, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


largePlans_termRates_byPlan %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype == "teacher") %>%
  ggplot(aes(x =age, y = termrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

largePlans_termRates_byPlan %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype == "general") %>%
  ggplot(aes(x =age, y = termrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


largePlans_termRates %>%
  filter(ea %in% c(30), age %in% 20:60, plantype != "alltypes") %>%
  ggplot(aes(x =age, y = termrate_weighted, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


#**************************************************
#                # Disability rates            ####
#**************************************************

## Weights: 
#  Plan weights within type
#  Plan weights for all
# Use nactives as weights

planWeight_nactives <- dataList_allPlans2$ActivesSched %>%
  filter(age %in% 20:74, ea %in% 20:74, age >= ea, fillin == TRUE ) %>% 
  select(planname, ea, age, nactives)


## Expanding salary scales to by age and YOS
plannames_disbrate_Matrix <- (largePlans_singleValues_chr %>% filter(varname == "DisbRatesType", value %in% c("Matrix", "LowYOS")))$planname
plannames_disbrate_byAge  <- (largePlans_singleValues_chr %>% filter(varname == "DisbRatesType", value == "byAge"))$planname
plannames_disbrate_byYOS  <- (largePlans_singleValues_chr %>% filter(varname == "DisbRatesType", value == "byYOS"))$planname

plannames_disbrate_Matrix 
plannames_disbrate_byAge  
plannames_disbrate_byYOS  


## Remove 150 NYC PPF 
#plannames_disbrate_byAge <- setdiff(plannames_disbrate_byAge, "150_NY_NYC-PPF")




# Matrix and lowYOS
largePlans_disbRates_Matrix <- 
  expand.grid(planname = plannames_disbrate_Matrix, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, ea, age) %>% 
  left_join(dataList_allPlans2$DisbRatesSched %>% 
              filter(DisbRatesType %in% c("Matrix", "LowYOS")))
#largePlans_disbRates_Matrix

# by age  
largePlans_disbRates_byAge <- 
  expand.grid(planname = plannames_disbrate_byAge, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, age) %>% 
  left_join(dataList_allPlans2$DisbRatesSched %>% 
              filter(DisbRatesType == "byAge") %>% 
              select(-ea, -yos)) %>% 
  mutate(yos = age - ea)
# largePlans_disbRates_byAge

# by YOS
largePlans_disbRates_byYOS <- 
  expand.grid(planname = plannames_disbrate_byYOS, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  arrange(planname, age) %>% 
  left_join(dataList_allPlans2$DisbRatesSched %>% 
              filter(DisbRatesType == "byYOS") %>% 
              select(-ea, -age))
# largePlans_disbRates_byYOS


# Merging all types and adding weights
largePlans_disbRates <- 
  bind_rows(largePlans_disbRates_Matrix,
            largePlans_disbRates_byAge,
            largePlans_disbRates_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives),
         rerate   = na2zero(disbrate)) 

largePlans_disbRates_byPlan <- 
  bind_rows(largePlans_disbRates_Matrix,
            largePlans_disbRates_byAge,
            largePlans_disbRates_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives),
         rerate   = na2zero(disbrate)) 



## Weighted average rates

# Average across all types
largePlans_disbRates_all <- 
  largePlans_disbRates %>% #filter(planname != "9_CA_CA-CALPERS") %>%
  filter(planname != "150_NY_NYC-PPF") %>% 
  group_by(ea, age) %>% 
  summarize(disbrate_weighted = weighted.mean(disbrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            disbrate_average  = mean(disbrate, na.rm = TRUE)) %>% 
  mutate(plantype = "alltypes")

largePlans_disbRates_all %>% filter(ea == 55)

# Average by type
largePlans_disbRates_type <- 
  largePlans_disbRates %>% 
  filter(planname != "150_NY_NYC-PPF") %>% 
  group_by(plantype, ea, age) %>% 
  summarize(disbrate_weighted = weighted.mean(disbrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            disbrate_average  = mean(disbrate, na.rm = TRUE))

largePlans_disbRates_type %>% filter(plantype == "general", ea == 25)


largePlans_disbRates <- 
  bind_rows(largePlans_disbRates_all,
            largePlans_disbRates_type) %>% 
  select(plantype, everything()) 



##  Checking results
largePlans_disbRates %>%
  group_by(plantype, ea) %>%
  filter(age %in% (unique(ea) + seq(0, 50, 10)))  %>%
  as.data.frame

# compariosn across types
largePlans_disbRates %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype != "alltypes") %>%
  ggplot(aes(x =age, y =disbrate_weighted, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

# compariosn across types
largePlans_disbRates %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype != "alltypes") %>%
  ggplot(aes(x =age, y =disbrate_average, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


largePlans_disbRates %>%
  # filter(plantype == "alltypes") %>%
  filter(plantype == "general") %>%
  # filter(plantype == "teacher") %>%
  # filter(plantype == "safety") %>%
  gather(avgtype, value, -plantype, -ea, -age) %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60) %>%
  ggplot(aes(x =age, y = value, color  = avgtype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


# Summary 


# Comparing plan types
# Teacher plans: Lowest rates, but not much lower than general 
# Safety plans : much higher than other plans (with NYC PPF removed)

# Comparing weighted and simple average
# all similar
# simple average is more smoothed

# Thoughts on modeling:
# Different plan types use different rates
# use simple average


# Further check why teacher plans have higher rates than safety plans

largePlans_disbRates_byPlan %>% 
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype == "teacher") %>%
  ggplot(aes(x =age, y =disbrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

# Disability rates for CALSTERS is much higher than other plans. OK to keep for group average


largePlans_disbRates_byPlan %>% 
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype == "safety") %>%
  ggplot(aes(x =age, y =disbrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

# Disability rates for NYC PPF is much higher than other plans. Should remove from the calc of group average for safety


largePlans_disbRates_byPlan %>% 
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype == "general") %>%
  ggplot(aes(x =age, y =disbrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

# Disability rates for 85 OH OH OPERS are much higher than other general plans. OK to keep for group average
# Disability rates for 26 FL FL FRS   are much lower than other general plans.  OK to keep for group average





#**************************************************
#                # Final outputs            ####
#**************************************************

# Teachers plan Late retirement 


fig_title <- "Assumed service retirement rates of different plan types \nfor an employee who joins the workforce at age 30"
fig_subtitle <- "Based on the 5 largest plans of each plan type in PPD"
fig1 <- 
  largePlans_retRates %>%
  filter(ea %in% c(30), age %in% 20:65,
         plantype != "alltypes") %>%
  ggplot(aes(x =age, y = 100*retrate_weighted, color  = plantype)) + theme_bw() + RIG.theme()+
  #facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point() + 
  scale_x_continuous(breaks = seq(30, 65, 5)) +
  scale_y_continuous(breaks = seq(0, 50, 5)) +
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red), name = "Plan Type")+
  labs(title = fig_title,
       subtitle = fig_subtitle,
       x = "Age",
       y = "Retirement rate (%)")
  

fig_title <- "Assumed termination rates of different plan types \nfor an employee who joins the workforce at age 30"
fig_subtitle <- "Based on the 5 largest plans of each plan type in PPD"
fig2 <- 
largePlans_termRates %>%
  filter(ea %in% c(30), age %in% 20:65, plantype != "alltypes") %>%
  ggplot(aes(x =age, y = 100*termrate_weighted, color  = plantype)) + theme_bw() + RIG.theme()+
  # facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()+scale_x_continuous(breaks = seq(30, 65, 5)) +
  scale_y_continuous(breaks = seq(0, 50, 2)) +
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red), name = "Plan Type")+
  labs(title = fig_title,
       subtitle = fig_subtitle,
       x = "Age",
       y = "Termination rate (%)")

ggsave(filename = "Analysis/Graphs_RAND2018/fig1.png", fig1, width = 10*0.6, height = 8*0.6)
ggsave(filename = "Analysis/Graphs_RAND2018/fig2.png", fig2, width = 10*0.6, height = 8*0.6)




# PPD data:
library(pdata)
load("Inputs_Data_PPD/DataPPD.RData")

ppd_2018 <- read_excel("Inputs_Data_PPD/PPD_PlanLevel_Feb2018.xlsx")

df <- 
ppd %>% 
  select(PlanName, fy, planf,
         FR   = ActFundedRatio_GASB,
         AA = ActAssets_GASB,
         AL = ActLiabilities_GASB,
         UAAL = UAAL_GASB,
         NC_tot = NormCostRate_tot, 
         NC_ER  = NormCostRate_ER,
         ADCrate_ER = ReqContRate_ER,
         ADC      = RequiredContribution,
         ADC_paid = PercentReqContPaid, 
         payroll,
         benefits_tot = expense_TotBenefits,
         actives_tot,
         beneficiaries_tot,
         
         disc = InvestmentReturnAssumption_GASB,
         
         smoothingPeriod = AssetSmoothingPeriod_GASB, 
         openclosedf, 
         pctdollf, 
         AmortPeriod = UAALAmortPeriod_GASB
         ) %>% 
  mutate(plantype = str_extract(planf, "General|Teacher|Safety"),
         teacherPlan = ifelse(plantype == "Teacher", TRUE, FALSE))

df %>% 
  group_by(fy) %>% 
  summarise_all(funs(sum(!is.na(.)))) 


# Funded ratio

df %>% 
  filter(fy == 2015) %>% 
  group_by(teacherPlan) %>% 
  summarise(FR_med = median(FR, na.rm = TRUE),
            FR_tot = sum(AA, na.rm = TRUE) / sum(AL, na.rm= TRUE))

# Amortization
df1 <- 
df %>% 
  filter(fy == 2015) %>% 
  group_by(teacherPlan, openclosedf) %>% 
  summarize(uaal = sum(UAAL, na.rm = TRUE))
df2 <- df1 %>% group_by(teacherPlan) %>% summarise(uaal_tot = sum(uaal))

df1 <- left_join(df1, df2) %>% 
  mutate(uaal_pct = 100 * uaal/uaal_tot)
df1


df3 <- 
  df %>% 
  filter(fy == 2015) %>% 
  mutate(amortPeriod30 = AmortPeriod >=30) %>% 
  group_by(teacherPlan, amortPeriod30) %>% 
  summarize(uaal = sum(UAAL, na.rm = TRUE))
df4 <- df3 %>% group_by(teacherPlan) %>% summarise(uaal_tot = sum(uaal))

df3 <- left_join(df3, df4) %>% 
  mutate(uaal_pct = 100 * uaal/uaal_tot)
df3


# Government contributions

df %>% 
  filter(fy == 2015) %>% 
  group_by(teacherPlan) %>%
  mutate(ADC_paid = ADC_paid * ADC) %>% 
  summarise(ADC  = sum(ADC, na.rm = TRUE),
            ADC_paid = sum(ADC_paid, na.rm = TRUE)) %>% 
  mutate(ADC_paid_pct = ADC_paid/ADC,
         ADC_shortfall = ADC - ADC_paid)
   

# salary and benefit
df %>% 
  filter(fy == 2015) %>%
  mutate(sal.avg = payroll/actives_tot,
         ben.avg = benefits_tot / beneficiaries_tot) %>% 
  group_by(plantype) %>% 
  summarise(sal.avg.med = median(sal.avg, na.rm = TRUE),
            ben.avg.med = median(ben.avg, na.rm = TRUE),
            NC_tot = median(NC_tot, na.rm = TRUE),
            NC_ER  = median(NC_ER, na.rm = TRUE))













  
  














