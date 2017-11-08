
# Inputs:
  # "Data_inputs/LAFPP_PlanInfo.RData", for decrements
  # "Data_inputs/LAFPP_MemberData.RData", for gender and occupation ratios


# Final outputs
 # 1. decrement.model
 # 2. mortality.post.model


get_decrements <- function(Tier_select,
                           .Global_paramlist = Global_paramlist,
                           .paramlist = paramlist){

  
# Tier_select <- "tCD"
# .Global_paramlist = Global_paramlist
# .paramlist = paramlist

assign_parmsList(.Global_paramlist, envir = environment())
assign_parmsList(.paramlist,        envir = environment())



#*************************************************************************************************************
#                                Prepare mortality tables for PSERS                        #####                  
#*************************************************************************************************************

#Note on qxm.deathBen
  # qxm.deathBen applies to beneficiaires of benenfits for death before retirement.
  # For now (Jun 26, 2016), qxm.deathBen is a weighted average of mortalities of survivors.
  # Weights are constant over time: 95% female survivors and 5% male survivors. For now, we do not take into 
    # account the effect difference in mortality of males and females on gender ratio over time.  
  # Wives are 3 years younger than husbands. 

mortality.model <- data.frame(age = range_age) %>% 
  left_join(mortality_PSERS) %>% 
  mutate(qxm.pre = qxm.pre.male  * pct.male + qxm.pre.female  * pct.female,   # mortality for actives
         
         # PSERS: expand qxm.d.male/female with qxm.d.male/female
         qxm.d.male   = ifelse(age < 50, qxm.pre.male,   qxm.d.male),
         qxm.d.female = ifelse(age < 50, qxm.pre.female, qxm.d.female),
         qxm.d        = (qxm.d.male    * pct.male + qxm.d.female  * pct.female),
         
         # PSERS: expand qxm.post.male/female with qxm.pre.male/female
         qxm.post.male   = ifelse(age < 50, qxm.pre.male,   qxm.post.male),
         qxm.post.female = ifelse(age < 50, qxm.pre.female, qxm.post.female)
         
         
         # qxm.deathBen = ifelse(age > max(age - 3), 1, lead(qxm.post.female, 3)) * pct.male + 
         #                ifelse(age < min(age + 3), qxm.post.male[age == min(age)],lag(qxm.post.male, 3)) * pct.female,
         # qxm.deathBen = ifelse(age == max(age), 1, qxm.deathBen)
         ) %>% 
  select(age, qxm.pre, qxm.post.male, qxm.post.female, qxm.d)

# mortality.model

## Compute present values of life annuity(with cola) at each retirement age, using uni-sex mortality with age dependent weights
  # Why using age dependent weights:
    # Using the age dependent weights, the number of total members calculated using aggregate members and weighted mortality would be the same
    # as that obtained by calculating the members for males and females separately and summing them up. This is required by the the actuarially 
    # equivalence between life annuity and contingent annuity. 
 

mortality.post.model <- expand.grid(age   = range_age, 
                                    age.r = range_age # modified for init retirees of PSERS 
                                    # min(range_age.r):max.age
                                    ) %>% 
  left_join(mortality.model) %>%
  filter(age >= age.r) %>% 
  group_by(age.r) %>%  
  mutate(
    pxm.post.male   = 1 - qxm.post.male,
    pxm.post.female = 1 - qxm.post.female,
    
    pRxm.male     = pct.male   * ifelse(age == min(age), 1, lag(cumprod(pxm.post.male))),  # proportion of male left in each year after retirement
    pRxm.female   = pct.female * ifelse(age == min(age), 1, lag(cumprod(pxm.post.female))),# same thing, for female
    
    w.male   = pRxm.male / (pRxm.male + pRxm.female),
    w.female = pRxm.female / (pRxm.male + pRxm.female),
    
    qxm.post.W = qxm.post.male * w.male + qxm.post.female * w.female, # dynamically weighted mortality
    pxm.post.W = 1 - qxm.post.W,
    
    COLA.scale = (1 + tier.param[Tier_select,"cola"])^(row_number() - 1 ),
    B =  COLA.scale,
    ax.r.W     =  get_tla(pxm.post.W, i, COLA.scale),
    liab.la.W = B * ax.r.W    # "la" for life annuity. liability (also PV) for $1's benefit payment at retirement. 
  )  %>% 
  mutate_all(funs(ifelse(is.nan(.), 0, .))) %>% 
  select(age.r, age, qxm.post.W, pxm.post.W, ax.r.W)

# mortality.post.model

# Construct mortality rate for terms: 
 # before r.vben: qxm.pre
 # after r.vben:  qxm.post.W with age.r == r.vben

mortality.model %<>% left_join(mortality.post.model %>% ungroup %>%  
                               filter(age.r == tier.param[Tier_select,"r.vben"]) %>% 
                               select(age, qxm.post.term = qxm.post.W)) %>% 
                     mutate(qxm.term = ifelse(age < tier.param[Tier_select,"r.vben"], qxm.pre, qxm.post.term)) %>% 
                     select(-qxm.post.term)



# disability rate                            
disbrates.model <- disbRates %>%  
  mutate(qxd = qxd.male   * pct.male + 
               qxd.female * pct.female) %>% 
  select(age, qxd)
                  
disbrates.model


# term rates
termrates.model <- termRates %>% 
  mutate(qxt = qxt.male * pct.male + 
               qxt.female * pct.female) %>% 
  select(age, ea, yos, qxt)
                                
termrates.model


# retirement rates
retrates.model  <- retRates %>% 
  mutate(qxr.early  = qxr.male.early * pct.male + qxr.female.early * pct.female,
         qxr.super  = qxr.male.super * pct.male + qxr.female.super * pct.female) %>% 
  select(age, qxr.early, qxr.super)
retrates.model                                     



#*************************************************************************************************************
#                      2. Putting together decrements and calculate surviving rates  ####
#*************************************************************************************************************

# Create decrement table and calculate probability of survival
decrement.model <- expand.grid(age = range_age, ea = range_ea) %>% 
  mutate(yos = age - ea) %>% 
  filter(age >= ea) %>% 
  left_join(mortality.model) %>%                  # mortality 
  left_join(termrates.model)  %>%                 # termination
  left_join(disbrates.model)  %>%                 # disability
  left_join(retrates.model) %>%                   # early retirement
  select(ea, age, everything()) %>%          
  arrange(ea, age)  %>%
  colwise(na2zero)(.) %>% 
  group_by(ea) 

decrement.model





#*************************************************************************************************************
#                      3. Adjustments to decrement tables  ####
#*************************************************************************************************************

## Combining early retirement rates and supaerannuation retirement rates

## Superannuation eligibility
 # Tier C/D
   #- age 62 or
   #- age >= 60, yos >=30, or
   #- yos >= 35
 
 # Tier E/F
   #- age >= 65, yos >= 3
   #- age + yos >= 92, yos >= 35
   #- age >=74  (Model assumption)
# Early retirement eligibility
 # age >= 55, yos >= 25


decrement.model %<>% 
  group_by(ea) %>% 
  mutate(# Early retirement
         elig_early = ifelse(age >= 55 & yos >=25, 1, 0),
         
         # superannuation retirement
         elig_super_tCD = ifelse( age >=62 | 
                                  (age >=60 & yos >=30) | 
                                  yos >= 35,
                                  1, 0),
         elig_super_tE  = ifelse( age>=74 |
                                  (age >= 65 & yos >=3) |
                                  (age + yos >= 92 & yos >= 35),
                                  1, 0),
         elig_super_tF  = elig_super_tE,
         
         # The age first eligible for superannuation in a ea group
         age_superFirst_tCD = age[min(which(elig_super_tCD == 1))],
         age_superFirst_tE = age[min(which(elig_super_tE == 1))],
         age_superFirst_tF = age[min(which(elig_super_tF == 1))], 
         
         # Combined retirement rates
         qxr_tCD = ifelse(elig_super_tCD == 1, qxr.super, 
                          ifelse(elig_early == 1, qxr.early, 0)),
         qxr_tE = ifelse(elig_super_tE == 1, qxr.super, 
                          ifelse(elig_early == 1, qxr.early, 0)),
         qxr_tF = ifelse(elig_super_tF == 1, qxr.super, 
                          ifelse(elig_early == 1, qxr.early, 0)),
         
         
         # Vesting
         elig_vest_tCD = ifelse(yos >= 5,  1, 0),
         elig_vest_tE  = ifelse(yos >= 10, 1, 0),
         elig_vest_tF  = ifelse(yos >= 10, 1, 0),
         
         # Mortality for death benefit recievers (assume eligible if vested)
         qxm.deathBen_tCD = ifelse(age < 50, qxm.pre, qxm.post.male * pct.male + qxm.post.female * pct.female),
         qxm.deathBen_tE  = qxm.deathBen_tCD,
         qxm.deathBen_tF  = qxm.deathBen_tCD,
         
         # Mortality for vested terms 
         qxm.term = ifelse(age < 50, qxm.pre, qxm.post.male * pct.male + qxm.post.female * pct.female)

         )
 

# Adjustment to term rates
# 1. extend qxt beyond age 61. (max in the AV)?
# 2. Coerce termination rates to 0 when eligible for early retirement or reaching than r.full(when we assume terms start to receive benefits). 

decrement.model %<>% mutate(
  qxt_tCD = ifelse(elig_early == 0 & elig_super_tCD == 0, qxt, 0 ),
  qxt_tE  = ifelse(elig_early == 0 & elig_super_tE == 0, qxt, 0 ),
  qxt_tF  = ifelse(elig_early == 0 & elig_super_tF == 0, qxt, 0 )
)
  






#*************************************************************************************************************
#                                   4. Selecting tier decrements ####
#*************************************************************************************************************

  decrement.model %<>% 
    mutate_(qxr = paste0("qxr_", Tier_select),
            qxt = paste0("qxt_", Tier_select),
            qxm.deathBen = paste0("qxm.deathBen_", Tier_select),
            
            elig_super     = paste0("elig_super_",     Tier_select),
            age_superFirst = paste0("age_superFirst_", Tier_select),
            
            elig_vest     = paste0("elig_vest_",     Tier_select)
            )




#*************************************************************************************************************
#                                   4. Modify retirement rates ####
#*************************************************************************************************************

# Adjustment to the decrement table:
  # Move qxr.a backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is lead(qxr.a(t))*(1 - qxt.a(t-1) - qxm.a(t-1) - qxd.a(t-1))
  # For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
  # which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
  # whether they will die at r.max)      

pct.ca <- pct.ca.M * pct.male + pct.ca.F * pct.female
pct.la <- 1 - pct.ca

decrement.model %<>% group_by(ea) %>%  
  mutate(qxr = ifelse(age == r.max - 1,
                             1 - qxt - qxm.pre - qxd, 
                             lead(qxr) * (1 - qxt - qxm.pre - qxd)), # Total probability of retirement
         qxr.la = ifelse(age == r.max, 0 , qxr * pct.la),  # Prob of opting for life annuity
         qxr.ca = ifelse(age == r.max, 0 , qxr * pct.ca),   # Prob of opting for contingent annuity
         
         qxd.la = ifelse(age == r.max, 0 , qxd * pct.la),  # Prob of opting for life annuity
         qxd.ca = ifelse(age == r.max, 0 , qxd * pct.ca)
         
         # qxd.la = ifelse(age == r.max, 0 , qxd * 1),  # Prob of opting for life annuity
         # qxd.ca = ifelse(age == r.max, 0 , qxd * 0)
         
)   
         

######!!!! need to construct retirement age dependent mortality for life annuitants.
  # For retired(".r"), the only target status is "dead". Note that in practice retirement mortality may differ from the regular mortality.
  #mutate(qxm.la.r   = qxm.r) 


#*************************************************************************************************************
#                                            5. compute various survival rates ####
#*************************************************************************************************************


decrement.model %<>% 
  group_by(ea) %>% 
  mutate( pxm.pre = 1 - qxm.pre,
          pxm.deathBen = 1 - qxm.deathBen,
          pxm.d        = 1 - qxm.d,
          pxm.term     = 1 - qxm.term,
          
          pxT     = 1 - qxt - qxd - qxm.pre - qxr,                            
          
          pxRm        = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxm.pre))), # prob of surviving up to r.max, mortality only
          px_r.full_m = order_by(-age, cumprod(ifelse(age >= tier.param[Tier_select, "r.vben"], 1, pxm.pre))), # Should be deleted later
          px_r.vben_m = order_by(-age, cumprod(ifelse(age >= tier.param[Tier_select, "r.vben"], 1, pxm.pre))),
          px_r.vsuper_m = order_by(-age, cumprod(ifelse(age >= age_superFirst, 1, pxm.pre)))
          
          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
  ) %>% 
  mutate_each(funs(na2zero))


list(decrement.model = decrement.model,
     mortality.post.model = mortality.post.model)

}


# get_decrements("t5")

# decrement.model %>% select(ea, age, px_r.vsuper_m, elig_super)








