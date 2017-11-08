

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

# 5. functions for get results for a given tier
  # get_benefit_tier
  # get_initPop_tier
  # get_entrantsDist_tier



# Summary of outputs

# salary
# benefit
# init_pop
# entrants_dist

# 

get_tierData <- function(df, tier, grouping = paramlist$Grouping) df %<>% filter(grepl(tier, planname), grepl(grouping, planname))

#get_tierData(init_actives_all, "tF") %>% print


#*************************************************************************************************************
#                         Salary 1.  Create salary scale                   #####                  
#*************************************************************************************************************
# No need to modify salary growth table for PSERS
get_salgrowth <- function() salgrowth

 

#*************************************************************************************************************
#                         Salary 2.  Create complete salary scale                                    #####                  
#*************************************************************************************************************

get_scale <- function(
   .salgrowth = salgrowth,
   .paramlist = paramlist,
   .Global_paramlist  = Global_paramlist
  ){
  
  # This function generates a complete salary scale for all combos of starting year, entry ages and ages relevant to
  # to model. 
  # 
  # Salary levels at year 1 are set to 1. For future workers (entry year greater than 1) whose span of career years
  # do not include year 1, assumption about their starting salary levels is needed. Curretnly we assume starting salary
  # grows at inflation rate. 
  
  
  # Run the section below when developing new features. 
  # .salgrowth        = salgrowth
  # .paramlist = paramlist
  # .Global_paramlist  = Global_paramlist
  
  
   assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
   assign_parmsList(.paramlist,        envir = environment())
  # 
  

  
  #.salgrowth <- .salgrowth %>% select(yos, salgrowth_w) # %>% filter(planname == planname_sscale) %>% select(-planname)
  
  SS.all <- expand.grid(start.year = (1 - (max.age - min.age)):nyear, ea = range_ea, age = min.age:(r.max - 1)) %>% 
    filter(age >= ea, start.year + (r.max - 1 - ea) >= 1 ) %>% # workers must stay in workforce at least up to year 1. 
    # arrange(start.year, ea, age) %>%
    mutate(yos = age - ea) %>% 
    left_join(.salgrowth) %>%
    group_by(start.year, ea) %>% 
    mutate(year = start.year + (age - ea),
           growth.start = (1 + startingSal_growth)^(start.year - 1),  # assume starting salary grows at the rate of inflation for all entry ages 
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

SS.all <- get_scale()

#*************************************************************************************************************
#                         Salary 3. Supplement the inital salary table with all starting salary        #####                  
#*************************************************************************************************************

fill_startSal <- function(.actives,         # = tailored_demoData$actives,
                          .paramlist        = paramlist,
                          .Global_paramlist = Global_paramlist
                          ){
  # This function generate a table of initial salary (year 1) which include all starting salary levels (age = ea)
  # If the starting salary is missing from the actives data frame, spline function is used to interpolate and/or 
  # extraploate the missing values. 
  
  
  # Run the section below when developing new features.
  #   .actives          = salary %>% select(age = age_cell, yos = yos_cell, salary) %>% mutate(ea = age - yos)
  #   .paramlist        = paramlist
  #   .Global_paramlist = Global_paramlist
  
   assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
   assign_parmsList(.paramlist,        envir = environment())  
   
  sal <- .actives %>% select(age, ea, salary)

  
  sal.start <- splong(sal, "ea", range_ea) %>% filter(age == ea) %>% select(-age) %>% splong("ea", range_ea) %>% mutate(age = ea)
  
  sal <- rbind(sal, sal.start) 
  
  sal <- sal[!duplicated(sal[c("age","ea")]),]
  # sal %>% spread(age, salary)
  
  return(sal)
  
}

# get_tierData(init_actives_all, "t6") %>%  fill_startSal %>% ungroup %>% arrange(ea, age)



#*************************************************************************************************************
#                          Salary 4. Create complete salary history                                    #####                  
#*************************************************************************************************************

get_salary <- function(.SS.all = SS.all,
                       .init_sal =  init_sal,
                        .paramlist = paramlist,
                        .Global_paramlist  = Global_paramlist
                       ){
  
  # Run the section below when developing new features.
  #   .SS.all = SS.all
  #   .init_sal =  init_sal
  #   .paramlist = paramlist
  #   .Global_paramlist  = Global_paramlist
  
  
   assign_parmsList(.Global_paramlist, envir = environment()) # environment() returns the local environment of the function.
   assign_parmsList(.paramlist,        envir = environment())  
  
  #avgpay <- actives %>% filter(planname == .planname_actives) %>% select(age, ea, salary)
  
  salary <- .SS.all %>% left_join(.init_sal) %>% 
    group_by(start.year, ea) %>% 
    mutate(sx = ifelse(start.year <= init.year, salary[year == init.year] * scale, 
                       salary[age == ea]* scale)) %>% 
    select(start.year, ea, age, year, sx)
  
  
  return(salary)
}

# salary <- get_salary() 
# salary %>% filter(start.year < 2015) %>% arrange(start.year, ea, age)



#*************************************************************************************************************
#                          Salary 5.  The top level function for creating initial salary              #####                  
#*************************************************************************************************************

get_salary_proc <- function(Tier_select_){ 

# Inputs
 # Tier_select
 # salgrowth
 # w.salgrwoth.method
 # init_actives 
 # paramlist
 # Global_paramlist

salgrowth.fn <- get_salgrowth()

SS.all.fn    <- get_scale(salgrowth.fn)

init_sal.fn  <- fill_startSal(get_tierData(init_actives_all, Tier_select_))

salary.fn <- get_salary(SS.all.fn, init_sal.fn) %>% 
  mutate(sx = na2zero(sx))

return(salary.fn)
}


# x <-  get_salary_proc("tCD") %>% arrange(start.year, ea, age)



#*************************************************************************************************************
#                               Import initial retirement benefit table from AV                          #####                  
#*************************************************************************************************************
get_benefit <- function(
   .retirees,
   .paramlist = paramlist,
   .Global_paramlist  = Global_paramlist
  ){
  
  
  #.retirees <- retirees %>% select(age = age_cell, benefit) 
  
   assign_parmsList(.Global_paramlist, envir = environment())
   assign_parmsList(.paramlist,        envir = environment())  
  
  avgben <- .retirees %>% select(age, benefit)  
  
  benefit <- avgben %>% 
    # filter(age>=r.max) %>% 
    mutate(year       = init.year,
           ea         = min(age) - 1,
           age.r      = age,
           start.year = year - (age - ea)) %>% 
    select(start.year, ea, age, age.r, benefit)
  # benefit %>% select(-year) %>% spread(age, benefit)
  
  return(benefit)
}


get_benefit.disb <- function(
  .disb, 
  .paramlist = paramlist,
  .Global_paramlist  = Global_paramlist
){
  
  
  #.retirees <- retirees %>% select(age = age_cell, benefit) 
  
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment())  
  
  avgben <- .disb %>% select(age, benefit)  
  
  benefit <- avgben %>% 
    # filter(age>=r.max) %>% 
    mutate(year       = init.year,
           ea         = min.age,
           age.disb   = age,
           start.year = year - (age - ea)) %>% 
    select(start.year, ea, age, age.disb, benefit.disb = benefit)
  # benefit %>% select(-year) %>% spread(age, benefit)
  
  return(benefit)
}



# get_tierData(init_retirees_all, "tCD") %>% get_benefit()
# get_tierData(init_disb_all, "tCD") %>% get_benefit.disb()



#*************************************************************************************************************
#                               Generating inital population                                             #####                  
#*************************************************************************************************************

get_initPop <- function (.actives,       
                         .retirees,   
                         .terminated, 
                         .disb,
                         .paramlist        = paramlist,
                         .Global_paramlist = Global_paramlist,
                         trim = TRUE
                         ){
  # Import and standardize the total number of actives and retirees.  
  
  # Run the section below when developing new features.
       # .actives           = get_tierData(init_actives_all, Tier_select_)
       # .retirees          = get_tierData(init_retirees_all, Tier_select_)
       # .terminated        = get_tierData(init_terms_all, Tier_select_)
       # 
       #  trim = TRUE   
  #   .paramlist        = paramlist
  #   .Global_paramlist = Global_paramlist
  
   assign_parmsList(.Global_paramlist, envir = environment())
   assign_parmsList(.paramlist,        envir = environment())
  
  init_actives <- .actives %>% select(ea, age, nactives)
  
  if(trim) init_actives %<>% filter(ea %in% range_ea, age %in% min.age:(r.max - 1))
  
  init_actives <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_actives) 
    #mutate(nactives = n_init_actives * nactives/sum(nactives, na.rm = TRUE)) %>%
     
 
  
  init_actives %<>% spread(age, nactives, fill = 0) %>% select(-ea) %>% as.matrix 
  
  
  init_retirees <- .retirees %>% select(age, nretirees.la) %>% mutate(ea = min(age) - 1) 
  init_retirees <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_retirees) %>% 
    #mutate(nretirees = n_init_retirees * nretirees/sum(nretirees, na.rm = TRUE)) %>% 
    spread(age, nretirees.la, fill = 0) %>% select(-ea) %>% as.matrix
  
  
  init_terms <- .terminated %>% select(ea, age, nterms)
  init_terms <-  expand.grid(ea = range_ea, age = range_age) %>% left_join(init_terms) %>% 
    #mutate(nactives = n_init_actives * nactives/sum(nactives, na.rm = TRUE)) %>%
    spread(age, nterms, fill = 0) %>% select(-ea) %>% as.matrix 
  
  init_disb <- .disb %>% select(age, ndisb.la) %>% mutate(ea = min.age) 
  init_disb <- expand.grid(ea = range_ea, age = range_age) %>% left_join(init_disb) %>% 
    #mutate(nretirees = n_init_retirees * nretirees/sum(nretirees, na.rm = TRUE)) %>% 
    spread(age, ndisb.la, fill = 0) %>% select(-ea) %>% as.matrix
  
  
  return(list(actives = init_actives, retirees = init_retirees, terms = init_terms, disb = init_disb))
}


# x <- get_initPop(get_tierData(init_actives_all, Tier_select),
#             get_tierData(init_retirees.la_all, Tier_select),
#             get_tierData(init_terms_all, Tier_select),
#             get_tierData(init_disb.la_all, Tier_select)
#             )

#x$actives
# x$retirees %>% sum


#*************************************************************************************************************
#                            Infering ditribution of entrants from low yos actives                       #####                  
#*************************************************************************************************************


get_entrantsDist <- function(.actives,          #= tailored_demoData$actives,
                             #.planname,         #= paramlist$planname_actives,
                             #.range_ea = range_ea,         #= paramlist$range_ea,
                             .paramlist        = paramlist,
                             .Global_paramlist = Global_paramlist,
                             simple = FALSE){
  # Simple imputation rule is applied under the following two circumstances:
  # 1. parameter "simple" is set to TRUE
  # 2. negative weights are generated by the regular rule. 
  
     # .actives          = get_tierData(init_actives_all, Tier_select)
     # .range_ea = range_ea
     #  simple = F
     #  .paramlist        = paramlist
     #  .Global_paramlist = Global_paramlist
  
  assign_parmsList(.Global_paramlist, envir = environment())
  assign_parmsList(.paramlist,        envir = environment())   
  
  nact <- .actives %>% select(age, ea, nactives)
  #nact %>% spread(age, nactives)
  
  # ## Distributon by simple rule
  # nact1 <- nact %>% filter(age - ea <= 4) %>% group_by(ea) %>% summarise(avg_ent = mean(nactives)) %>% right_join(data.frame(ea = range_ea))
  # N <- 1
  # while(any(is.na(nact1$avg_ent))) {
  #   if(N <= length(nact1)) nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lag(avg_ent) , avg_ent)) else
  #     nact1 %<>% mutate(avg_ent = ifelse(is.na(avg_ent), lead(avg_ent) , avg_ent))
  #   N <- N + 1
  #   }
    

  nact <- splong(nact, "ea", range_ea) %>% splong("age", range_ea) %>% filter(age >= ea)
  #nact <- splong(nact, "ea", range_ea) %>% filter(age >= ea)
  nact %>% spread(age, nactives)
  
  
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

# entrants_dist <- get_entrantsDist(init_actives)
# entrants_dist



#*************************************************************************************************************
#                                       Functions to create tier specific data                    #####                  
#*************************************************************************************************************

get_benefit_tier <- function(Tier_select_, grouping_ = paramlist$Grouping){
  
  # cola <- tier.param[Tier_select_, "cola"]
  
  #init_actives       <- get_tierData(init_actives_all, Tier_select_)
  init_retirees      <- get_tierData(init_retirees.la_all, Tier_select_)
  #init_beneficiaries <- get_tierData(init_beneficiaries_all, Tier_select_)
  #init_terminated    <- init_terminated_all %>%  filter(grepl(Tier_select, planname, Tier_select_))
  init_retirees %>% get_benefit() # %>% mutate(benefit = benefit * 0.989589)    # mutate(benefit = benefit * (1 + cola))

}

get_benefit.disb_tier <- function(Tier_select_, grouping_ = paramlist$Grouping){
  
  # cola <- tier.param[Tier_select_, "cola"]
  init_disb      <- get_tierData(init_disb.la_all, Tier_select_)
  init_disb %>% get_benefit.disb() # %>% mutate(benefit.disb = benefit.disb * 0.989589 )  # %>% mutate(benefit.disb = benefit.disb * (1 + cola))
  
}



get_initPop_tier <- function(Tier_select_, grouping_ =  paramlist$Grouping){
  #Tier_select_ = Tier_select
  #grouping_ =  paramlist$Grouping
  
  init_actives        <- get_tierData(init_actives_all, Tier_select_)
  init_retirees       <- get_tierData(init_retirees.la_all, Tier_select_)
  #init_beneficiaries <- get_tierData(init_beneficiaries_all, Tier_select_)
  # init_terminated     <- init_terms_all %>%  filter(grepl(Tier_select_, planname))
  init_terminated     <- get_tierData(init_terms_all, Tier_select_)
  init_disb           <- get_tierData(init_disb.la_all,  Tier_select_)
  
  get_initPop(init_actives, init_retirees, init_terminated, init_disb)
  
}


get_entrantsDist_tier <- function(Tier_select_, grouping_ =  paramlist$Grouping){
  init_actives       <- get_tierData(init_actives_all, Tier_select_)
  get_entrantsDist(init_actives)
}


# get_benefit_tier("tE")
# get_initPop_tier("tE")
# get_entrantsDist_tier("tE")







