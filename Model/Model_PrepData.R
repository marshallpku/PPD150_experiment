## Preparing data for modeling 

# Road Map

# 1. Salary scale
  # get.salgrowth
  # get_scale
  # fill_startSal
  # get_salary
  # get_salary_proc: Top level function that runs the four functions above
  
# 2. Benefit for initial retirees 
  # get_benefit

# 3. Initial population
  # get_initPop 

# 4. Distribution of new entrants
  # get_entrantsDist


# Summary of outputs

# salary
# benefit
# init_pop
# entrants_dist



#*************************************************************************************************************
#                         Salary 1.  Create salary scale                   #####                  
#*************************************************************************************************************
# No need to modify salary growth table for PSERS
get_salgrowth <- function() planData_list$salgrowth


#*************************************************************************************************************
#                         Salary 2.  Create complete salary scale                                    #####                  
#*************************************************************************************************************

get_scale <- function(salgrowth_ = planData_list$salgrowth,
                      paramlist  = planData_list$inputs_singleValues){
  
  # This function generates a complete salary scale for all combos of starting year, entry ages and ages relevant to
  # to model. 
  # 
  # Salary levels at year 1 are set to 1. For future workers (entry year greater than 1) whose span of career
  # do not include year 1, assumption about their starting salary levels is needed. Curretnly we assume starting salary
  # grows at inflation rate plus productivity growth unless stated otherwise.  
  
  
  # Run the section below when developing new features. 
   # salgrowth_ = planData_list$salgrowth
   # paramlist  = planData_list$inputs_singleValues

  assign_parmsList(paramlist, envir = environment()) # environment() returns the local environment of the function.
  
  
  SS.all <- expand.grid(start.year = (1 - (age_max - age_min)):nyear, 
                        ea = range_ea, 
                        age = age_min:(retage_max - 1)) %>% 
    filter(age >= ea, 
           start.year + (retage_max - 1 - ea) >= 1 ) %>% # workers must stay in workforce at least up to year 1. 
    # arrange(start.year, ea, age) %>%
    mutate(yos = age - ea) %>% 
    left_join(salgrowth_) %>%
    group_by(start.year, ea) %>% 
    mutate(year = start.year + (age - ea),
           growth.start = (1 + startingSal_growth)^(start.year - 1),  # assume starting salary grows at the rate of inflation + productivity for all entry ages 
           scale = cumprod(ifelse(age == ea, 1, lag(1 + salgrowth))), # salgrowth is from data salgrowth
           scale = ifelse(start.year <= 1, scale/scale[year == 1],    # Salary levels before starting year are scaled based on salary in the starting year.
                          scale * growth.start)) %>% 
    ungroup %>% 
    mutate(year = year + init.year - 1,
           start.year = start.year + init.year - 1 # convert to fiscal year. 
    ) %>% 
    select(start.year, ea, age, year, scale)
  
  return(SS.all)
}

# SS.all <- get_scale()


#*************************************************************************************************************
#                         Salary 3. Supplement the inital salary table with all starting salary        #####                  
#*************************************************************************************************************

fill_startSal <- function(actives          = planData_list$init_actives,
                          paramlist        = planData_list$inputs_singleValues
                          ){
  # This function generate a table of initial salary (year 1) which include all starting salary levels (age = ea)
  # If the starting salary is missing from the actives data frame, spline function is used to interpolate and/or 
  # extraploate the missing values. 
  
  
  # Run the section below when developing new features.
    # actives          = planData_list$init_actives
    # paramlist        = planData_list$inputs_singleValues

  assign_parmsList(paramlist, envir = environment()) # environment() returns the local environment of the function.
   
  sal <- actives %>% select(age, ea, salary)

  sal.start <- splong(sal, "ea", range_ea) %>% filter(age == ea) %>% select(-age) %>% splong("ea", range_ea) %>% mutate(age = ea)
  
  sal <- rbind(sal, sal.start) 
  
  sal <- sal[!duplicated(sal[c("age","ea")]),]
  # sal %>% spread(age, salary)
  
  return(sal)
  
}
#init_sal <-  fill_startSal()

#*************************************************************************************************************
#                          Salary 4. Create complete salary history                                    #####                  
#*************************************************************************************************************

get_salary <- function(SS.all_,   #  =  SS.all,
                       init_sal_, #  =  init_sal,
                       paramlist = planData_list$inputs_singleValues
                       ){
  
  # Run the section below when developing new features.
    # SS.all_   =  SS.all
    # init_sal_  =  init_sal
    # paramlist =  planData_list$inputs_singleValues

  
  
  assign_parmsList(paramlist, envir = environment()) # environment() returns the local environment of the function.
 
  salary <- SS.all_ %>% left_join(init_sal_) %>% 
    group_by(start.year, ea) %>% 
    mutate(sx = ifelse(start.year <= init.year, salary[year == init.year] * scale, 
                       salary[age == ea]* scale),
           sx = na2zero(sx)) %>% 
    select(start.year, ea, age, year, sx) %>% 
    arrange(start.year, ea, age)
  
  return(salary)
}

# salary <- get_salary(SS.all, init_sal) 

# salary %>% filter(start.year < 2015) %>% arrange(start.year, ea, age)
# salary

#*************************************************************************************************************
#                          Salary 5.  The top level function for creating initial salary              #####                  
#*************************************************************************************************************

get_salary_proc <- function(){ 

# Inputs
 # Tier_select
 # salgrowth
 # w.salgrwoth.method
 # init_actives 
 # paramlist
 # Global_paramlist

salgrowth.fn <- get_salgrowth()

SS.all.fn    <- get_scale(salgrowth.fn)

init_sal.fn  <- fill_startSal()

salary.fn <- get_salary(SS.all.fn, init_sal.fn)

return(salary.fn)
}

#salary <- get_salary_proc()
#salary

#*************************************************************************************************************
#                               Import initial retirement benefit table                          #####                  
#*************************************************************************************************************
get_benefit <- function(
    retirees  = planData_list$init_retirees,
    paramlist = planData_list$inputs_singleValues
  ){
  
 # Run the section below for development.
  # retirees  = planData_list$init_retirees 
  # paramlist = planData_list$inputs_singleValues
  
  assign_parmsList(paramlist, envir = environment())

  avgben <- retirees %>% select(age, benefit)  
  
  benefit <- avgben %>% 
    # filter(age>=r.max) %>% 
    mutate(year       = init.year,
           
           # assumptions just for programming purpose:
           ea         = min(age) - 1,   
           age.r      = age,            
           start.year = year - (age - ea)) %>% 
    select(start.year, ea, age, age.r, benefit)

  return(benefit)
}

# get_benefit()


#*************************************************************************************************************
#                               Generating inital population                                             #####                  
#*************************************************************************************************************

get_initPop <- function ( actives           = planData_list$init_actives,
                          retirees          = planData_list$init_retirees,
                          paramlist         = planData_list$inputs_singleValues){
  
  # Import and standardize the total number of actives and retirees.  
  
  # Run the section below when developing new features.
       # actives           = planData_list$init_actives
       # retirees          = planData_list$init_retirees
       # paramlist         = planData_list$inputs_singleValues
  
  assign_parmsList(paramlist, envir = environment())

  
  # Initial actives
  init_actives <- actives %>% select(ea, age, nactives)
  init_actives <- 
    expand.grid(ea = range_ea, age = range_age) %>% 
    left_join(init_actives) %>% 
    spread(age, nactives, fill = 0) %>% select(-ea) %>% as.matrix 
  #init_actives
  
  # Initial retirees
  init_retirees <- retirees %>% select(age, nretirees) %>% mutate(ea = min(age) - 1) 
  init_retirees <- 
    expand.grid(ea = range_ea, age = range_age) %>% 
    left_join(init_retirees) %>% 
    spread(age, nretirees, fill = 0) %>% select(-ea) %>% as.matrix
  #init_retirees
  

  return(list(actives = init_actives, retirees = init_retirees))
}
#get_initPop()

#*************************************************************************************************************
#                            Infering ditribution of entrants from low yos actives                       #####                  
#*************************************************************************************************************

get_entrantsDist <- function(actives   = planData_list$init_actives,
                             paramlist = planData_list$inputs_singleValues,
                             simple = FALSE){
  # Simple imputation rule is applied under the following two circumstances:
  # 1. parameter "simple" is set to TRUE
  # 2. negative weights are generated by the regular rule. 
  
  # Run the section below when developing new features.
   # actives   = planData_list$init_actives
   # paramlist = planData_list$inputs_singleValues
   # simple = FALSE
  assign_parmsList(paramlist, envir = environment())
  
  nact <- actives %>% select(age, ea, nactives)

  # ## Distributon by simple rule
  # nact1 <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives)) %>% right_join(data.frame(ea = range_ea))
  # N <- 1
  # while(any(is.na(nact1$avg_ent))) {
  #   if(N <= length(nact1)) nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lag(avg_ent) , avg_ent)) else
  #     nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lead(avg_ent) , avg_ent))
  #   N <- N + 1
  #   }
    

  nact <- splong(nact, "ea", range_ea) %>% splong("age", range_ea) %>% filter(age >= ea)
  # nact %>% spread(age, nactives)
  
  ent <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives))
  
  neg_ea <- ent[which(ent$avg_ent < 0), "ea"]
  
  if(any(ent$avg_ent < 0)){warning("Negative inferred value(s) in the following entry age(s): " , as.character(neg_ea), "\n",
                                   "  Simple imputation rule is applied")
    ent <- nact1                          
  }
  
  # ent %<>% mutate(avg_ent = ifelse(avg_ent < 0, 0, avg_ent))
  
  # if(simple) ent <- nact1
  
  dist <- lowess(ent$avg_ent, f= 0.1)$y
  dist <- dist/sum(dist)
  
  return(dist)
}

# get_entrantsDist()








