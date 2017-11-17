# This program load data for large plans and 
#  1) Create standardized data file for each large plan, which will be used for modeling large plans
#  2) Create standardized data for 3 major plan types based on large plans, which will be used in modeling smaller plans


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
#                  Tools                       ####
#**************************************************



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



#**************************************************
#          Loading large plan data             ####
#**************************************************

# Define file names
fileNames <- paste0(data_dir, "DataLargePlan_", planNames_all, ".RData")


# Loading all data files into a list
dataList_allPlans <- llply(fileNames, function(x) {load(x); data_list <- data_largePlan})
names(dataList_allPlans) <- planNames_all



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


#**************************************************
#          # Single values                     ####
#**************************************************

largePlans_singleValues_num <- dataList_allPlans2$singleValues_num
largePlans_singleValues_chr <-dataList_allPlans2$singleValues_chr


# df for plan types, used later
df_largePlans_types <- 
  largePlans_singleValues_chr %>% filter(str_detect(varname, "Type")) %>% 
  select(planname, plantype, varname, value) %>%
  spread(varname, value)
  
#**************************************************
#          # ActivesSched                      ####
#**************************************************

## Goal
  # Compute distribution for nactives
  # Compute scales for salaries

## Notes:
  # Only use data in the range of age 20~74 and entry age 20~74

## Summary: 
   # Safety plans tend to have lower average ages and high average YOS.
   # General plans and teachers plans have very similar average ages and YOS. (general plans have slightly higher 
   # average ages.)


# Compute scales by plan
largePlans_actives_scales_byPlan <- dataList_allPlans2$ActivesSched %>%
  filter(age %in% 20:74, ea %in% 20:74, age >= ea, fillin == TRUE ) %>% 
  group_by(planname) %>% 
  mutate(nactives = na2zero(nactives),
         salary   = na2zero(salary),
         nactives_scale = nactives / sum(nactives, na.rm = TRUE),
         salary_scale   = salary / weighted.mean(salary, nactives, na.rm = TRUE))



# Compute scales by plan type
largePlans_actives_scales1 <- 
  dataList_allPlans2$ActivesSched %>%
  filter(age %in% 20:74, ea %in% 20:74, age >= ea,  fillin == TRUE ) %>% 
  group_by(plantype, ea, age) %>% 
  mutate(nactives = na2zero(nactives),
         salary   = na2zero(salary)) %>% 
  summarise(nactives.type = sum(nactives, na.rm = TRUE) %>% na2zero(),
            salary.type   = weighted.mean(salary, nactives) %>% na2zero) %>% 
  group_by(plantype) %>% 
  mutate(nactives_scale.type = nactives.type / sum(nactives.type, na.rm = TRUE),
         salary_scale.type   = salary.type / weighted.mean(salary.type, nactives.type, na.rm = TRUE))


# Compute scales for all
largePlans_actives_scales2 <- # 
  dataList_allPlans2$ActivesSched %>%
  filter(age %in% 20:74, ea %in% 20:74, age >= ea, fillin == TRUE ) %>% 
  group_by(ea, age) %>% 
  mutate(nactives = na2zero(nactives),
         salary   = na2zero(salary)) %>% 
  summarise(nactives.type = sum(nactives, na.rm = TRUE) %>% na2zero(),
            salary.type   = weighted.mean(salary, nactives) %>% na2zero) %>% 
  ungroup %>% 
  mutate(plantype = "allTypes",
         nactives_scale.type = nactives.type / sum(nactives.type, na.rm = TRUE),
         salary_scale.type   = salary.type / weighted.mean(salary.type, nactives.type, na.rm = TRUE))


largePlans_actives_scales_byType <- bind_rows(largePlans_actives_scales1,
                                       largePlans_actives_scales2)

largePlans_actives_scales_byType


## Compute overall group averages  
# Check average ages by plan
largePlans_actives_scales_byPlan %>% 
  group_by(planname) %>% 
  summarize(avg_age = weighted.mean(age, nactives, na.rm = TRUE),
            avg_yos = weighted.mean(yos, nactives, na.rm = TRUE)) %>% 
  arrange(avg_yos)

# Check average ages by plan type
largePlans_actives_scales_byPlan %>% 
  group_by(plantype) %>% 
  summarize(avg_age = weighted.mean(age, nactives, na.rm = TRUE),
            avg_yos = weighted.mean(yos, nactives, na.rm = TRUE)) %>% 
  arrange(avg_age)



# Final output
largePlans_actives_scales_byPlan
largePlans_actives_scales_byType



# # plotting by age
# largePlans_actives_byAge <-
#   largePlans_actives %>%
#     filter(fillin == TRUE) %>%
#     group_by(planname, age) %>%
#     summarize(nactives = sum(nactives, na.rm = TRUE),
#               salary   = sum(salary*nactives, na.rm = TRUE)/sum(nactives, na.rm = TRUE)) %>%
#     mutate(nactives_scale = nactives / sum(nactives, na.rm = TRUE),
#            salary_scale   = salary / weighted.mean(salary, nactives, na.rm = TRUE)) %>%
#     ungroup() %>%
#     mutate(planname = factor(planname, levels = planNames_all))
# 
# largePlans_actives_byAge %>%
#   ggplot(aes(x = age, y = nactives_scale)) +
#   facet_wrap( ~planname, ncol = 5) +
#   geom_line() +
#   geom_point()
# 
# 
# largePlans_actives_byAge %>%
#   ggplot(aes(x = age, y = salary_scale)) +
#   facet_wrap( ~planname, ncol = 5) +
#   geom_line() +
#   geom_point()
# 
# # plotting by YOS
# largePlans_actives_byYOS <-
#   largePlans_actives %>%
#   filter(fillin == TRUE) %>%
#   group_by(planname, yos) %>%
#   summarize(nactives = sum(nactives, na.rm = TRUE),
#             salary   = sum(salary*nactives, na.rm = TRUE)/sum(nactives, na.rm = TRUE)) %>%
#   mutate(nactives_scale = nactives / sum(nactives, na.rm = TRUE),
#          salary_scale   = salary / weighted.mean(salary, nactives, na.rm = TRUE)) %>%
#   ungroup() %>%
#   mutate(planname = factor(planname, levels = planNames_all))
# 
# largePlans_actives_byYOS %>%
#   ggplot(aes(x = yos, y = nactives_scale)) +
#   facet_wrap( ~planname, ncol = 5) +
#   geom_line() +
#   geom_point()
# 
# 
# largePlans_actives_byYOS %>%
#   ggplot(aes(x = yos, y = salary_scale)) +
#   facet_wrap( ~planname, ncol = 5) +
#   geom_line() +
#   geom_point()




#**************************************************
#                # Retirees                    ####
#**************************************************

## Goal: 
 # distribution of retirees
 # distribution of benefits

# Notes:
 # Only use data in the range of age 20~110.

# Summary: 
# Safety plans have the lowest average ages and high average benefit.
# General plans have the hightest average age and the lowest average benefit. 
# Difference between plantypes are significant. 
# benefit scales look quite irregular, may want to use 5-year groups


# Compute scales by plan
largePlans_retirees_scales_byPlan <- dataList_allPlans2$RetireeSched %>%
  filter(age %in% 20:110, fillin == TRUE ) %>% 
  group_by(planname) %>% 
  mutate(nretirees = na2zero(nretirees),
         benefit   = na2zero(benefit),
         nretirees_scale = nretirees / sum(nretirees, na.rm = TRUE),
         benefit_scale   = benefit / weighted.mean(benefit, nretirees, na.rm = TRUE))


# Compute scales by plan type
largePlans_retirees_scales1 <- 
  dataList_allPlans2$RetireeSched %>%
  filter(age %in% 20:110, fillin == TRUE ) %>% 
  group_by(plantype, age) %>% 
  mutate(nretirees = na2zero(nretirees),
         benefit   = na2zero(benefit)) %>% 
  summarise(nretirees.type = sum(nretirees, na.rm = TRUE) %>% na2zero(),
            benefit.type   = weighted.mean(benefit, nretirees) %>% na2zero) %>% 
  group_by(plantype) %>% 
  mutate(nretirees_scale.type = nretirees.type / sum(nretirees.type, na.rm = TRUE),
         benefit_scale.type   = benefit.type / weighted.mean(benefit.type, nretirees.type, na.rm = TRUE))

# Compute scales for all
largePlans_retirees_scales2 <- # 
  dataList_allPlans2$RetireeSched %>%
  filter(age %in% 20:110, fillin == TRUE ) %>%
  group_by(age) %>% 
  mutate(nretirees = na2zero(nretirees),
         benefit   = na2zero(benefit)) %>% 
  summarise(nretirees.type = sum(nretirees, na.rm = TRUE) %>% na2zero(),
            benefit.type   = weighted.mean(benefit, nretirees) %>% na2zero) %>% 
  ungroup %>% 
  mutate(plantype = "allTypes",
         nretirees_scale.type = nretirees.type / sum(nretirees.type, na.rm = TRUE),
         benefit_scale.type   = benefit.type / weighted.mean(benefit.type, nretirees.type, na.rm = TRUE))


largePlans_retirees_scales_byType <- bind_rows(largePlans_retirees_scales1,
                                       largePlans_retirees_scales2)
#largePlans_retirees_scales_byType$plantype %>% unique




# ## Plotting the distributions
# 
# # Age dist of retirees
# largePlans_retirees_scales1 %>% 
#   ggplot(aes(x = age, y = nretirees_scale.type, color = plantype)) +
#   geom_line() +
#   geom_point()
# 
# # Age dist of benefits
# largePlans_retirees_scales %>% 
#   ggplot(aes(x = age, y = benefit_scale.type, color = plantype)) +
#   geom_line() +
#   geom_point()
# 
# 
# 
# 
# ## Compute overall group averages  
# # Check average ages by plan
# largePlans_retirees_scalesByPlan %>% 
#   group_by(planname) %>% 
#   summarize(avg_age = weighted.mean(age, nretirees, na.rm = TRUE),
#             avg_ben = weighted.mean(benefit, nretirees, na.rm = TRUE)) %>% 
#   arrange(avg_ben)
# 
# # Check average ages by plan type
# largePlans_retirees_scalesByPlan %>% 
#   group_by(plantype) %>% 
#   summarize(avg_age = weighted.mean(age, nretirees, na.rm = TRUE),
#             avg_ben = weighted.mean(benefit, nretirees, na.rm = TRUE)) %>% 
#   arrange(avg_ben)
# 


# final output
largePlans_retirees_scales_byPlan
largePlans_retirees_scales_byType


#**************************************************
#                # Salary scales               ####
#**************************************************

## Goal
  # constructing average salary scales for each plan type.

## Notes
  # Weights used when constructing type averages 
   #  a) weighted using number of actives
   #  b) simple averages across plans
  # Only use data in the range of age 20~74 and entry age 20~74

## Summary:
  # Diffences in grates across types are the largest in low YOS: 
   # teacher plans have the highest growth and safety plans have the lowest growth
   # Differeces narrow as YOS increases: growth in teacher plans decreases the fastest, 
   # and becomes the lowest in high YOS range
   
  # Simple and weighted averages
   # For low entry ages, the difference between simple average and weighed average is small
   # For high enty ages (>35), weigted averages are much higher than simple averages (LAFPP effect?) 
   # Simple average lines are smoother than weighted average lines
   # Plan to use simple averages in the first version of the model, because it is less affected
   # by the values of large plans, which may not be representative of smaller plans. 

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
  left_join(df_largePlans_types %>% select(planname, plantype, SalaryGrowthType)) %>% 
  left_join(dataList_allPlans2$SalaryGrowthSched %>% 
              filter(SalaryGrowthType == "Matrix") %>% 
              select(-plantype, -SalaryGrowthType))
# largePlans_salaryScale_Matrix

 # by age  
largePlans_salaryScale_byAge <- 
  expand.grid(planname = plannames_grate_byAge, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, age) %>% 
  left_join(df_largePlans_types %>% select(planname, plantype, SalaryGrowthType)) %>% 
  left_join(dataList_allPlans2$SalaryGrowthSched %>% 
              filter(SalaryGrowthType == "byAge") %>% 
              select(-ea, -yos, -plantype, -SalaryGrowthType)) %>% 
  mutate(yos = age - ea)
  # largePlans_salaryScale_byAge

 # by YOS
largePlans_salaryScale_byYOS <- 
  expand.grid(planname = plannames_grate_byYOS, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  arrange(planname, age) %>% 
  left_join(df_largePlans_types %>% select(planname, plantype, SalaryGrowthType)) %>% 
  left_join(dataList_allPlans2$SalaryGrowthSched %>% 
              filter(SalaryGrowthType == "byYOS") %>% 
              select(-ea, -age, -plantype, -SalaryGrowthType))
  # largePlans_salaryScale_byAge

 # Merging all types and adding weights
largePlans_salaryScale_byPlan <- 
  bind_rows(largePlans_salaryScale_Matrix,
            largePlans_salaryScale_byAge,
            largePlans_salaryScale_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives)) 


# Modify grate at yos = 5 in 150 NYC PPF: 36% to 4%
largePlans_salaryScale_byPlan %<>%
  mutate(grate = ifelse(planname == "150_NY_NYC-PPF" & yos == 5, 0.04, grate))



## Weighted average rates

 # Average across all types
largePlans_salaryScale_all <- 
  largePlans_salaryScale_byPlan %>% #filter(planname != "9_CA_CA-CALPERS") %>%
  group_by(ea, age) %>% 
  summarize(grate_weighted = weighted.mean(grate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            grate_average  = mean(grate, na.rm = TRUE)) %>% 
  mutate(plantype = "alltypes")

  largePlans_salaryScale_all %>% filter(ea == 55)

 # Average by type
largePlans_salaryScale_type <- 
  largePlans_salaryScale_byPlan %>% 
  group_by(plantype, ea, age) %>% 
  summarize(grate_weighted = weighted.mean(grate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            grate_average  = mean(grate, na.rm = TRUE))

  largePlans_salaryScale_type %>% filter(plantype == "general", ea == 25)

 
largePlans_salaryScale_byType <- 
  bind_rows(largePlans_salaryScale_all,
            largePlans_salaryScale_type) %>% 
  select(plantype, everything()) 
  


##  Checking results
largePlans_salaryScale_byType %>%
  group_by(plantype, ea) %>% 
  filter(age %in% (unique(ea) + seq(0, 50, 10)))  %>% 
  as.data.frame

# compariosn across types
largePlans_salaryScale_byType  %>% 
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60) %>% 
  ggplot(aes(x =age, y = grate_average, color  = plantype)) + 
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() + 
  geom_point() 

largePlans_salaryScale_byType  %>% 
  filter(plantype == "alltypes") %>% 
  # filter(plantype == "general") %>% 
  # filter(plantype == "teacher") %>%
  # filter(plantype == "safety") %>% 
  gather(avgtype, value, -plantype, -ea, -age) %>% 
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60) %>%
  ggplot(aes(x =age, y = value, color  = avgtype)) + 
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() + 
  geom_point() 


# Final output
largePlans_salaryScale_byPlan
largePlans_salaryScale_byType


#**************************************************
#                # Retirement rates            ####
#**************************************************

## Goal
 # Constructing average retirement rates for each plan type


## Notes
 # Weights used when constructing type averages 
 #  a) weighted using number of actives
 #  b) simple averages across plans
 # Only use data in the range of age 20~74 and entry age 20~74
 

## Summary

 # Comparing plan types   
   # saftey:  plans have very high rates
   # teacher: plans have the lowest rates; sharp rises in age 55 and 60
   # general: peak on ~62, and 70
   # difference between plans are large.  
 
 # Comparing weighted average and simpel average
   # general: difference not large, not very smooth, 
   # teacher: large differences for low ea; relatively smooth; 
   # safety:  large differences for all ea, averages are smoother
 
 # Thougths on modeling
   # Different plan types use different rates
   # use simple averages
   # Need to think about peaks in rates:
   # general plans: ok, no very large peaks 
   # teacher plans: may be ok to use, align peaks with early and full retirement ages?
   # safety plans:  Need to think about how to deal with the peaks; 
   # peaks may appear at different ages for different ea.(ea + 25) which plan has this feature? 
 
 
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
largePlans_retRates_byPlan <- 
  bind_rows(largePlans_retRates_Matrix,
            largePlans_retRates_byAge,
            largePlans_retRates_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives),
         retrate   = na2zero(retrate)) 
# largePlans_retRates %>% filter(is.na(plantype))


## Weighted average rates

# Average across all types
largePlans_retRates_all <- 
  largePlans_retRates_byPlan %>% #filter(planname != "9_CA_CA-CALPERS") %>%
  group_by(ea, age) %>% 
  summarize(retrate_weighted = weighted.mean(retrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            retrate_average  = mean(retrate, na.rm = TRUE)) %>% 
  mutate(plantype = "alltypes")
# largePlans_retRates_all %>% filter(ea == 55)

# Average by type
largePlans_retRates_type <- 
  largePlans_retRates_byPlan %>% 
  group_by(plantype, ea, age) %>% 
  summarize(retrate_weighted = weighted.mean(retrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            retrate_average  = mean(retrate, na.rm = TRUE))
# largePlans_retRates_type %>% filter(plantype == "general", ea == 25)


largePlans_retRates_byType <- 
  bind_rows(largePlans_retRates_all,
            largePlans_retRates_type) %>% 
  select(plantype, everything()) 


# ##  Checking results
largePlans_retRates_byType %>%
  group_by(plantype, ea) %>%
  filter(age %in% (unique(ea) + seq(0, 50, 10)))  %>%
  as.data.frame

# compariosn across types
largePlans_retRates_byType %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60) %>%
  ggplot(aes(x =age, y = retrate_average, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


largePlans_retRates_byType %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60) %>%
  ggplot(aes(x =age, y = retrate_weighted, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


largePlans_retRates_byType %>%
  # filter(plantype == "alltypes")  %>%
  # filter(plantype == "general")   %>%   # difference not large, not very smooth, peak on ~62, and 70
  # filter(plantype == "teacher")     %>%   # large differences for low ea; relatively smooth; sharp rises in age 55 and 60
   filter(plantype == "safety")    %>%   # large differences for all ea, averages are smoother
  gather(avgtype, value, -plantype, -ea, -age) %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 40:74) %>%
  ggplot(aes(x =age, y = value, color  = avgtype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


# Final output
largePlans_retRates_byPlan 
largePlans_retRates_byType

#**************************************************
#                # Termination rates            ####
#**************************************************

## Goal
 # Construct average termination rates for each plan type 

## Notes
 # Weights used when constructing type averages 
 #  a) weighted using number of actives
 #  b) simple averages across plans
 # Only use data in the range of age 20~74 and entry age 20~74

## Summary:

 # Comparing plan types
  # Teacher plans: generally have higher term rates, especially for low and high YOS
  # Safety plans : generally have very low term rates
  # general plans: in the middle, with great variation: 9_CALPERS and 83_NYERS 
  #                have much lower term rates than other plans
    
 # Comparing weighted and simple average
  # Very similar, simple averages are a bit more smoothed. 
 
 # Thoughts on modeling:
  # Different plan types use different rates
  # use simple average
 

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
  left_join(df_largePlans_types %>% select(planname, plantype, TermRatesType)) %>% 
  left_join(dataList_allPlans2$TermRatesSched %>% 
              filter(TermRatesType %in% c("Matrix", "LowYOS")) %>% 
              select(-plantype, -TermRatesType))
#largePlans_TermRates_Matrix


# by age  
largePlans_termRates_byAge <- 
  expand.grid(planname = plannames_termrate_byAge, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, age) %>% 
  left_join(df_largePlans_types %>% select(planname, plantype, TermRatesType)) %>% 
  left_join(dataList_allPlans2$TermRatesSched %>% 
              filter(TermRatesType == "byAge") %>% 
              select(-ea, -yos, -plantype, -TermRatesType)) %>% 
  mutate(yos = age - ea)
# largePlans_salaryScale_byAge

# by YOS
largePlans_termRates_byYOS <- 
  expand.grid(planname = plannames_termrate_byYOS, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  arrange(planname, age) %>%
  left_join(df_largePlans_types %>% select(planname, plantype, TermRatesType)) %>% 
  left_join(dataList_allPlans2$TermRatesSched %>% 
              filter(TermRatesType == "byYOS") %>% 
              select(-plantype, -TermRatesType))



# Merging all types and adding weights
largePlans_termRates_byPlan <- 
  bind_rows(largePlans_termRates_Matrix,
            largePlans_termRates_byAge,
            largePlans_termRates_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives),
         termrate   = na2zero(termrate)) 



## Weighted average rates

# Average across all types
largePlans_termRates_all <- 
  largePlans_termRates_byPlan %>% #filter(planname != "9_CA_CA-CALPERS") %>%
  group_by(ea, age) %>% 
  summarize(termrate_weighted = weighted.mean(termrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            termrate_average  = mean(termrate, na.rm = TRUE)) %>% 
  mutate(plantype = "alltypes")
#largePlans_termRates_all %>% filter(ea == 55)


# Average by type
largePlans_termRates_type <- 
  largePlans_termRates_byPlan %>% 
  group_by(plantype, ea, age) %>% 
  summarize(termrate_weighted = weighted.mean(termrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            termrate_average  = mean(termrate, na.rm = TRUE))

# largePlans_termRates_type %>% filter(plantype == "general", ea == 25)


largePlans_termRates_byType <- 
  bind_rows(largePlans_termRates_all,
            largePlans_termRates_type) %>% 
  select(plantype, everything()) 



# ##  Checking results
largePlans_termRates_byType %>%
  group_by(plantype, ea) %>%
  filter(age %in% (unique(ea) + seq(0, 50, 10)))  %>%
  as.data.frame

# compariosn across types
largePlans_termRates_byType %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype != "alltypes") %>%
  ggplot(aes(x =age, y = termrate_weighted, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

largePlans_termRates_byType %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype != "alltypes") %>%
  ggplot(aes(x =age, y = termrate_average, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


largePlans_termRates_byType %>%
  # filter(plantype == "alltypes") %>%
  # filter(plantype == "general") %>%
  # filter(plantype == "teacher") %>%
  # filter(plantype == "safety") %>%
  gather(avgtype, value, -plantype, -ea, -age) %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60) %>%
  ggplot(aes(x =age, y = value, color  = avgtype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


# Comparison across plans
largePlans_termRates_byPlan %>%
  filter(ea %in% c(20, 30, 40), age %in% 20:60) %>%
  ggplot(aes(x =age, y = termrate, color  = planname)) +
  facet_wrap(ea ~ plantype, ncol = 3) +
  geom_line() +
  geom_point()

largePlans_termRates_byPlan %>%
  filter(ea %in% c(20, 30, 40), age %in% 20:60, plantype == "general") %>%
  ggplot(aes(x =age, y = termrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

largePlans_termRates_byPlan %>%
  filter(ea %in% c(20, 30, 40), age %in% 20:60, plantype == "teacher") %>%
  ggplot(aes(x =age, y = termrate, color  = planname)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()



# Final Outputs
largePlans_termRates_byPlan
largePlans_termRates_byType

#**************************************************
#                # Disability rates            ####
#**************************************************

## Goal
# Construct average disability rates for each plan type 

## Notes
 # Weights used when constructing type averages 
 #  a) weighted using number of actives
 #  b) simple averages across plans
 # Only use data in the range of age 20~74 and entry age 20~74

## Summary 
 
 # Comparing across plans
  # Disability rates for CALSTERS is much higher than other plans. OK to keep for group average
  # Disability rates for NYC PPF is much higher than other plans. Should remove from the calc of group average for safety
  # Disability rates for 85 OH OH OPERS are much higher than other general plans. OK to keep for group average
  # Disability rates for 26 FL FL FRS   are much lower than other general plans.  OK to keep for group average


 # Comparing plan types
  # Teacher plans: Lowest rates, but not much lower than general 
  # Safety plans : much higher than other plans (with NYC PPF removed)
 
 # Comparing weighted and simple average
  # all similar
  # simple average is more smoothed
 
 # Thoughts on modeling:
  # Different plan types use different rates
  # use simple average
  # Remove NYC PPF in the calculation


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
# plannames_disbrate_byAge <- setdiff(plannames_disbrate_byAge, "150_NY_NYC-PPF")


# Matrix and lowYOS
largePlans_disbRates_Matrix <- 
  expand.grid(planname = plannames_disbrate_Matrix, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, ea, age) %>% 
  left_join(df_largePlans_types %>% select(planname, plantype, DisbRatesType)) %>% 
  left_join(dataList_allPlans2$DisbRatesSched %>% 
              filter(DisbRatesType %in% c("Matrix", "LowYOS")) %>% 
              select(-plantype, -DisbRatesType))
#largePlans_disbRates_Matrix

# by age  
largePlans_disbRates_byAge <- 
  expand.grid(planname = plannames_disbrate_byAge, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  arrange(planname, age) %>% 
  left_join(df_largePlans_types %>% select(planname, plantype, DisbRatesType)) %>% 
  left_join(dataList_allPlans2$DisbRatesSched %>% 
              filter(DisbRatesType == "byAge") %>% 
              select(-ea, -yos, -plantype, -DisbRatesType)) %>% 
  mutate(yos = age - ea)
# largePlans_disbRates_byAge

# by YOS
largePlans_disbRates_byYOS <- 
  expand.grid(planname = plannames_disbrate_byYOS, ea = 20:74, age = 20:74) %>% 
  filter(age >= ea) %>% 
  mutate(yos = age - ea) %>% 
  arrange(planname, age) %>% 
  left_join(df_largePlans_types %>% select(planname, plantype, DisbRatesType)) %>% 
  left_join(dataList_allPlans2$DisbRatesSched %>% 
              filter(DisbRatesType == "byYOS") %>% 
              select(-ea, -age, -plantype, -DisbRatesType))
# largePlans_disbRates_byYOS


# Merging all types and adding weights

largePlans_disbRates_byPlan <- 
  bind_rows(largePlans_disbRates_Matrix,
            largePlans_disbRates_byAge,
            largePlans_disbRates_byYOS) %>%
  left_join(planWeight_nactives) %>% 
  mutate(nactives = na2zero(nactives),
         disbrate   = na2zero(disbrate)) 



## Weighted average rates

# Average across all types
largePlans_disbRates_all <- 
  largePlans_disbRates_byPlan %>% #filter(planname != "9_CA_CA-CALPERS") %>%
  filter(planname != "150_NY_NYC-PPF") %>% 
  group_by(ea, age) %>% 
  summarize(disbrate_weighted = weighted.mean(disbrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            disbrate_average  = mean(disbrate, na.rm = TRUE)) %>% 
  mutate(plantype = "alltypes")

#largePlans_disbRates_all %>% filter(ea == 55)

# Average by type
largePlans_disbRates_type <- 
  largePlans_disbRates_byPlan %>% 
  filter(planname != "150_NY_NYC-PPF") %>% 
  group_by(plantype, ea, age) %>% 
  summarize(disbrate_weighted = weighted.mean(disbrate, nactives + 1e-10, na.rm = TRUE), # add 1e-10 to avoid the case where all weights are 0
            disbrate_average  = mean(disbrate, na.rm = TRUE))

#largePlans_disbRates_type %>% filter(plantype == "general", ea == 25)


largePlans_disbRates_byType <- 
  bind_rows(largePlans_disbRates_all,
            largePlans_disbRates_type) %>% 
  select(plantype, everything()) 



##  Checking results
largePlans_disbRates_byType %>%
  group_by(plantype, ea) %>%
  filter(age %in% (unique(ea) + seq(0, 50, 10)))  %>%
  as.data.frame

# compariosn across types
largePlans_disbRates_byType %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype != "alltypes") %>%
  ggplot(aes(x =age, y =disbrate_weighted, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()

# compariosn across types
largePlans_disbRates_byType %>%
  filter(ea %in% c(20, 25, 30, 35, 40, 45), age %in% 20:60, plantype != "alltypes") %>%
  ggplot(aes(x =age, y =disbrate_average, color  = plantype)) +
  facet_wrap( ~ ea, ncol = 3) +
  geom_line() +
  geom_point()


largePlans_disbRates_byType %>%
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


## Final outputs
largePlans_disbRates_byPlan
largePlans_disbRates_byType


#***************************************************************************************
#      Exploring how to split AL_active and AL_retiree for smaller plans            ####
#***************************************************************************************

# Goal:
  # Develop a method that can split the total AL of an plan into 2 parts: AL of actives and AL of retirees, based on 
  # other aggregage variables of the plan available from PPD, such as # of actives and retirees, average age/yos, and other plan characteristics. 

# Approach:
  # variables to check:
     # active-to-beneficiaries ratio
     # ? average age of actives  CHECK PPD
     # ? average yos of actives  CHECK PPD
     # ? average age of retirees CHECK PPD


df1 <- largePlans_singleValues_num %>% 
  filter(varname %in% c("AL_active", "AL_retired", "AL_total")) %>% 
  select(planname, plantype, varname, value) %>% 
  spread(varname, value) %>% 
  arrange(plantype, planname) %>% 
  mutate(pct_AL_actives = 100 * AL_active / (AL_total))

df1 %>% 
  summarize(pct_AL_actives = sum(AL_active) / sum(AL_total))

df1 %>% group_by(plantype) %>% 
  summarize(pct_AL_actives = sum(AL_active) / sum(AL_total))

df2 <- dataList_allPlans2$ActivesSched %>%
  filter(age %in% 20:74, ea %in% 20:74, age >= ea, fillin == TRUE ) %>% 
  group_by(planname) %>% 
  summarise(nactives.tot = sum(nactives, na.rm = TRUE))


df3 <- dataList_allPlans2$RetireeSched %>%
  filter(age %in% 20:110, fillin == TRUE ) %>% 
  group_by(planname) %>% 
  summarise(nretirees.tot = sum(nretirees, na.rm = TRUE))


df <- df1 %>% 
  left_join(df2) %>% 
  left_join(df3) %>% 
  filter(!planname %in% c("10_CA_CA-CALSTRS", "78_NY_NY-NYSTRS")) %>% 
  mutate(pct_nactives = 100 * nactives.tot / (nactives.tot + nretirees.tot)) %>% 
  arrange(pct_AL_actives) 

df %>% arrange(plantype, pct_AL_actives)

df %>% 
  ggplot(aes(x = pct_nactives, y = pct_AL_actives)) + 
  geom_point() +
  facet_grid(.~plantype) + 
  geom_smooth(method = lm, se = F)


# ideas:
  # regression with plantype dummies
  # regression with average age and average yos
  # double check # of actives and retirees with PPD data


lm(pct_AL_actives ~ pct_nactives, data = df %>% filter(plantype == "teacher")) %>% summary
lm(pct_AL_actives ~ pct_nactives, data = df %>% filter(plantype == "teacher")) %>% summary


# Method to use:
  # A. Naive method: use a uniform ratio of ~0.415 
  # B. Use type specific ratio, two possibilities:
      #  1. calc with total AL of each type
      #  2. simple average of plan ratios within type
  # C. Use regression of %AL_actives on %nactives
      # need to exclude NYC

# Sensitivity analysis:
  # How outcome variables vary under different approaches. 


# May want to do:
  # do the regression approach using # of actives and # of retirees from PPD
 


#***************************************************************************************
#     Comparion with NMR                                                            ####
#***************************************************************************************

# Need to compare scales calculated from Large plans to those in NMR appendix

# 1. Scale of number of actives

largePlans_actives_scales_byType %>% head

vec.ageGrp <- c(20, seq(26, 76, 5))
vec.yosGrp <- c(0, seq(6, 56, 5))

largePlans_actives_scales.grp <- 
largePlans_actives_scales_byType %>% 
  mutate(yos = age - ea,
         age.grp = findInterval(age, vec.ageGrp),
         yos.grp = findInterval(yos, vec.yosGrp),
         age_yos.grp = paste(age.grp, yos.grp, sep = "_")) %>% 
  group_by(plantype, age_yos.grp) %>% 
  summarise(nactives.type.grp = sum(nactives.type, na.rm = T),
            salary.type.grp   = sum(nactives.type * salary.type ,na.rm = T) / sum(nactives.type,na.rm = T)) %>% 
  group_by(plantype) %>% 
  mutate(nactives.scale.type.grp = nactives.type.grp / sum(nactives.type.grp,na.rm = T),
         salary.scale.type.grp   = salary.type.grp / (sum(nactives.type.grp * salary.type.grp,na.rm = T)/sum(nactives.type.grp,na.rm = T))) %>% 
  separate(age_yos.grp, c("age.grp", "yos.grp"), sep = "_", convert = TRUE) %>% 
  arrange(age.grp, yos.grp) 
  
largePlans_actives_scales.grp %>% head  


largePlans_actives_scales.grp.split <- 
     largePlans_actives_scales.grp %>% split(largePlans_actives_scales.grp$plantype)


fn.nactive.scale <- function(df) {
  df %>% 
  select(age.grp, yos.grp, nactives.scale.type.grp) %>% 
  spread(yos.grp, nactives.scale.type.grp, fill = 0) %>% 
  as.data.frame
}
fn.salary.scale <- function(df) {
  df %>% 
    select(age.grp, yos.grp, salary.scale.type.grp) %>% 
    spread(yos.grp, salary.scale.type.grp, fill = 0) %>% 
    as.data.frame
}


nactives_scales.spread <- llply(largePlans_actives_scales.grp.split, fn.nactive.scale)
salary_scales.spread   <- llply(largePlans_actives_scales.grp.split, fn.salary.scale)


largePlans_actives_scales.grp %>% 
  #filter(plantype == "allTypes") %>% 
  ggplot(aes(x = yos.grp, y = age.grp)) + facet_grid(.~plantype) + 
  geom_tile(aes(fill = nactives.scale.type.grp), color = "white")+
  scale_fill_gradient(limit = c(0.005, 0.1), low = "red", high = "green") 

largePlans_actives_scales.grp %>% 
  #filter(plantype == "allTypes") %>% 
  ggplot(aes(x = yos.grp, y = age.grp)) + facet_grid(.~plantype) + 
  geom_tile(aes(fill = salary.scale.type.grp), color = "white")+
  scale_fill_gradient(limit = c(0.5, 1.5), low = "red", high = "green") 



# 2. Scale of retirees and benefits

ret.ageGrp <- c(seq(50, 90, 5))
ret.ageGrp

largePlans_retirees_scales_byType %>% head

largePlans_retirees_scales.grp <- 
largePlans_retirees_scales_byType %>% 
  mutate(age.grp = findInterval(age, ret.ageGrp)) %>% 
  group_by(plantype, age.grp) %>% 
  summarize(nretirees.grp = sum(nretirees.type, na.rm = T),
            benefit.grp   = sum(nretirees.type * benefit.type, na.rm = T)/sum(nretirees.type)) %>% 
  group_by(plantype) %>% 
  mutate(nretiree.scale.grp = nretirees.grp /  sum(nretirees.grp)) %>% 
  select(plantype, age.grp, nretiree.scale.grp, benefit.grp) %>% 
  as.data.frame()

largePlans_retirees_scales.grp  


# 3. Salary growth rate

largePlans_salaryScale.grp <- 
largePlans_salaryScale_byType %>% 
  select(-grate_weighted) %>% 
  filter(ea %in% c(20, 30, 40), age %in% seq(20, 75, 5)) %>% 
  spread(ea, grate_average, fill = 0) %>% 
  as.data.frame
  


# 4. Separation rates (term rates  + retirement rates. )

largePlans_retRates.grp <- 
  largePlans_retRates_byType %>%
  select(-retrate_weighted) %>% 
  filter(ea %in% c(20, 30, 40), age %in% seq(20, 75, 5)) %>% 
  #spread(ea, retrate_average, fill = 0) %>% 
  as.data.frame
largePlans_retRates.grp

largePlans_termRates.grp <- 
  largePlans_termRates_byType %>% 
  select(-termrate_weighted) %>% 
  filter(ea %in% c(20, 30, 40), age %in% seq(20, 75, 5)) %>% 
  # spread(ea, termrate_average, fill = 0) %>% 
  as.data.frame
largePlans_termRates.grp

largePlans_sep.grp <- 
  largePlans_retRates.grp %>% 
  left_join(largePlans_termRates.grp) %>% 
  mutate(seprate = retrate_average + termrate_average) %>% 
  gather(var, value, -plantype, -ea, -age) %>% 
  mutate(var = paste(var, ea, sep = "_")) %>% 
  select(-ea) %>% 
  spread(var, value, fill = 0)
  
largePlans_sep.grp




largePlans_salaryScale_byType# largePlans_disbRates %>% 
#   select(-disbrate_weighted) %>% 
#   filter(ea %in% c(20, 30, 40), age %in% seq(20, 75, 5)) %>% 
#   spread(ea, disbrate_average, fill = 0) %>% 
#   as.data.frame




write.xlsx(nactives_scales.spread$allTypes, file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "nactives_allTypes", row.names = FALSE)
write.xlsx(nactives_scales.spread$general,  file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "nactives_general" , row.names = FALSE, append = TRUE)
write.xlsx(nactives_scales.spread$safety,   file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "nactives_safety"  , row.names = FALSE, append = TRUE)
write.xlsx(nactives_scales.spread$teacher,  file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "nactives_teacher" , row.names = FALSE, append = TRUE)

write.xlsx(salary_scales.spread$allTypes,   file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "salary_allTypes", row.names = FALSE, append = TRUE)
write.xlsx(salary_scales.spread$general,    file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "salary_general" , row.names = FALSE, append = TRUE)
write.xlsx(salary_scales.spread$safety,     file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "salary_safety"  , row.names = FALSE, append = TRUE)
write.xlsx(salary_scales.spread$teacher,    file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "salary_teacher" , row.names = FALSE, append = TRUE)

write.xlsx(largePlans_retirees_scales.grp,  file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "retirees" , row.names = FALSE, append = TRUE)

write.xlsx(largePlans_salaryScale.grp,      file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "salaryScale" , row.names = FALSE, append = TRUE)

write.xlsx(largePlans_sep.grp,              file = paste0(data_dir, "NMR_Comparison_raw.xlsx"), sheetName = "separation" , row.names = FALSE, append = TRUE)


#**************************************************
#                # Final outputs            ####
#**************************************************

# Final outputs

largePlans_dataOutputs <- list(
  dataList_allPlans = dataList_allPlans, # data for single plans
  
  largePlans_singleValues_num = largePlans_singleValues_num,
  largePlans_singleValues_chr = largePlans_singleValues_chr,
  
  largePlans_actives_scales_byPlan = largePlans_actives_scales_byPlan,
  largePlans_actives_scales_byType = largePlans_actives_scales_byType,
  
  largePlans_retirees_scales_byPlan = largePlans_retirees_scales_byPlan,
  largePlans_retirees_scales_byType = largePlans_retirees_scales_byType,
  
  
  largePlans_salaryScale_byPlan = largePlans_salaryScale_byPlan,
  largePlans_retRates_byPlan    = largePlans_retRates_byPlan,
  largePlans_termRates_byPlan   = largePlans_termRates_byPlan,
  largePlans_disbRates_byPlan   = largePlans_disbRates_byPlan,
  
  largePlans_salaryScale_byType = largePlans_salaryScale_byType,
  largePlans_retRates_byType    = largePlans_retRates_byType,  
  largePlans_termRates_byType   = largePlans_termRates_byType, 
  largePlans_disbRates_byType   = largePlans_disbRates_byType
  )

save(largePlans_dataOutputs, file = paste0(data_dir, "largePlans_dataOutputs.RData"))









