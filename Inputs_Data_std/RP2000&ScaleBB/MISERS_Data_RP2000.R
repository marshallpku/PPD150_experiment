# This script constructs mortality tables for the MI-SERS model based on RP2000 tables and Scale BB from SOA 

# Mortality tables:

# Healthy actives 
  # RP-2000 Combined Healthy Mortality Table, adjusted for improvements to 2015 using scale BB. 
  # For men, 50% of the male table rates were used. For women, 50% of the female table rates were used. 
  # Duty and non-duty death
    # Conservation officers: 80% non-duty
    # Correction officers:   70% non-duty
    # Others:                90% non-duty           

# Healthy post-retirement
  # RP-2000 Combined Healthy Mortality Table, adjusted for improvements to 2015 using scale BB. 

# Disabled 
  # RP-2000 Combined Healthy Mortality Table, adjusted for improvements to 2015 using scale BB, set-forward 10-years

#*********************************************************************************************************
#                      ## Import Data  ####
#*********************************************************************************************************

# Import mortality data

data_raw_healthyMale   <- readWorksheetFromFile("Data_inputs/RP2000&ScaleBB/RP2000-male-CombinedHealthy.xlsx", 
                                                sheet = "Sheet1", header=FALSE, region="a25:b144", colTypes="character") %>% 
                          mutate_each(funs(as.numeric))
data_raw_healthyFemale <- readWorksheetFromFile("Data_inputs/RP2000&ScaleBB/RP2000-female-CombinedHealthy.xlsx", 
                                                sheet = "Sheet1", header=FALSE, region="a25:b144", colTypes="character") %>% 
                          mutate_each(funs(as.numeric))

names(data_raw_healthyMale)   <- c("age", "qxm.male")
names(data_raw_healthyFemale) <- c("age", "qxm.female")



# Import Scale BB
scaleBB_male   <- readWorksheetFromFile("Data_inputs/RP2000&ScaleBB/ScaleBB-male.xlsx", 
                                                sheet = "Sheet1", header=FALSE, region="a25:b125", colTypes="character") %>% 
                  mutate_each(funs(as.numeric))
names(scaleBB_male)   <- c("age", "scale.male")


scaleBB_female <- readWorksheetFromFile("Data_inputs/RP2000&ScaleBB/ScaleBB-female.xlsx", 
                                                sheet = "Sheet1", header=FALSE, region="a25:b125", colTypes="character") %>% 
                  mutate_each(funs(as.numeric))
names(scaleBB_female)   <- c("age", "scale.female")


#*********************************************************************************************************
#                      ## Compute RP2000 mortality projected to 2015  ####
#*********************************************************************************************************

mortality_MISERS <- data.frame(age = 20:120) %>% 
  left_join(data_raw_healthyMale) %>%
  left_join(data_raw_healthyFemale) %>% 
  left_join(scaleBB_male) %>% 
  left_join(scaleBB_female) %>% 
  
  mutate(# project mortality into 2015 using scale BB
         qxm.male.proj2015   = qxm.male * (1 - scale.male)^15,
         qxm.female.proj2015 = qxm.female * (1 - scale.female)^15,

         # Use 50% of the projected rates for active members
         qxm.pre.male     = ifelse(age == max(age), 1, 0.5 * qxm.male.proj2015),
         qxm.pre.female   = ifelse(age == max(age), 1, 0.5 * qxm.female.proj2015),
         
         # Use the projected rates for retirees
         qxm.post.male       =  qxm.male.proj2015,
         qxm.post.female     =  qxm.female.proj2015,
         
         # Set the projected rates 10 years forward for disabled (age 60 uses mortality rate for age 70)
         qxm.d.male   =  ifelse(age > max(age) - 10, 1, 
                                lead(qxm.male.proj2015, 10)),
         qxm.d.female =  ifelse(age > max(age) - 10, 1, 
                                lead(qxm.female.proj2015, 10))
         )
  
  
# check results against sample mortality provided in AV2015 and experience study 2013
mortality_MISERS %>% filter(age %in% seq(20, 65, 5)) %>% select(age, qxm.pre.male,  qxm.pre.female) %>% kable(digits = 4)
mortality_MISERS %>% filter(age %in% seq(50, 80, 5)) %>% select(age, qxm.post.male, qxm.post.female) %>% kable(digits = 4)
mortality_MISERS %>% filter(age %in% seq(50, 80, 5)) %>% select(age, qxm.d.male, qxm.d.female) %>% kable(digits = 4)
# All match

save(mortality_MISERS, file = "Data_inputs/MISERS_mortality.RData")










