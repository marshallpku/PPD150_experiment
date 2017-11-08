# This script calculates individdual liabilities and normal costs for PSERS members. 

# Road map

# 1.  Preparation

# 2.1 AL and NC of life annuity and contingent annuity for actives
# 2.2 AL and benefit for retirees with life annuity

# 3.1 AL and NC of deferred benefits for actives
# 3.2 AL and benefits for vested terminated members

# 4.1 AL and NC of benefit for death before retirement
# 4.2 AL and benefits for vested terminated members

# 5.1 AL and NC of disability benefit
# 5.2 AL and benefits for disability benefit

# 6. Selecting variables for the chosen actuarial method





get_indivLiab <- function(
                         decrement_model_ = decrement_model,
                         salary_          = salary,
                         benefit_         = benefit,
                         # init_terms_all_  = init_terms_all,
                         paramlist = planData_list$inputs_singleValues){

# Inputs
  decrement_model_ = decrement_model
  salary_          = salary
  benefit_         = benefit
  # init_terms_all_  = init_terms_all,
  paramlist = planData_list$inputs_singleValues
  

assign_parmsList(paramlist, envir = environment()) # environment() returns the local environment of the function.

# parameters to be added
# fasyears <- tier.param[Tier_select_, "fasyears"]
# cola     <- tier.param[Tier_select_, "cola"]
# bfactor  <- tier.param[Tier_select_, "bfactor"]
# 

#*************************************************************************************************************
#                               1. Preparation                        #####                  
#*************************************************************************************************************

min.year <- min(init.year - (age_max - (retage_max - 1)), 
                init.year - (retage_max - 1 - ea_min)) 
                # min(init.year - (benefit_$age - (r.min - 1))))

## Track down to the year that is the smaller one of the two below: 
# the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
# the year a r.max year old active in year 1 enter the workforce at age min.ea 


#liab.active 
liab.active <- expand.grid(start.year = min.year:(init.year + nyear - 1) , 
                           ea = range_ea, age = range_age) %>%
  filter(start.year + max.age - ea >= init.year, age >= ea) %>%  # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  arrange(start.year, ea, age) %>% 
  left_join(salary_) %>%
  left_join(DC_rate.tot) %>% 
# left_join(.benefit) %>% # must make sure the smallest age in the retirement benefit table is smaller than the single retirement age. (smaller than r.min with multiple retirement ages)
  left_join(decrement.model_) %>% 
  left_join(mortality.post.model_ %>% filter(age == age.r) %>% select(age, ax.r.W)) %>%
  left_join(liab.ca_ %>% filter(age == age.r) %>% select(age, liab.ca.sum.1)) %>% 
  left_join(liab.disb.ca_ %>% filter(age == age.disb) %>% select(age, liab.disb.ca.sum.1 = liab.ca.sum.1)) %>% 
  group_by(start.year, ea) %>%
  
  # filter(start.year == 2015, ea == 73) %>% 
  
  # Calculate salary and benefits
  mutate(
    yos= age - ea,
    
    # PSERS: DC contribution 
    Tier = Tier_select_,
    reform_all = DC_reform_all,  # if DC reform is applied to all current and future members
    DC_rate.tot = ifelse((Tier %in% c("tNE", "tNF")) | reform_all, DC_rate.tot,0), # (Tier %in% c("tNE", "tNF")) * DC_rate.tot,
    DC_EEC      = sx * EEC_DC.rate,
    DC_ERC      = sx * max(0, (DC_rate.tot - EEC_DC.rate)),
    
    # years of service
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary

    n  = pmin(yos, fasyears),                          # years used to compute fas
    fas= ifelse(yos < fasyears, Sx/n, (Sx - lag(Sx, fasyears))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),     # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    Bx = na2zero(bfactor * yos * fas),                  # accrued benefits, note that only Bx for ages above r.min are necessary under EAN.
    bx = lead(Bx) - Bx,                           # benefit accrual at age x

    # actuarial present value of future benefit, for $1's benefit in the initial year. 
    ax.deathBen = get_tla(pxm.deathBen, i, COLA.scale),    # Since retirees die at max.age for sure, the life annuity with COLA is equivalent to temporary annuity with COLA up to age max.age. 
    ax.disb.la  = get_tla(pxm.d, i, COLA.scale),     
    ax.vben     = get_tla(pxm.term, i, COLA.scale),   
    # ax.r.W.ret is already in mortality.post.model_
    
    # ax.r = get_tla(pxm.r, i, COLA.scale),       # ax calculated with mortality table for retirees. 
    
    axR = c(get_tla(pxT[age < r.max], i), rep(0, max.age - r.max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < r.max], i,  sx[age < r.max]), rep(0, max.age - r.max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    #   axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    #   axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    # axr = ifelse(ea >= r.vben, 0, c(get_tla(pxT[age < r.vben], i), rep(0, max.age - r.vben + 1))),                   # Similar to axR, but based on r.vben.  For calculation of term benefits when costs are spread up to r.vben.        
    # axrs= ifelse(ea >= r.vben, 0, c(get_tla(pxT[age < r.vben], i,  sx[age<r.vben]), rep(0, max.age - r.vben + 1))),  # Similar to axRs, but based on r.vben. For calculation of term benefits when costs are spread up to r.vben.

    axr = ifelse(ea >= age_superFirst, 0, c(get_tla(pxT[age < age_superFirst], i), rep(0, max.age - unique(age_superFirst) + 1))),                             # Similar to axR, but based  on age_superFirst. For calculation of term benefits when costs are spread up to age_superFirst. (vary across groups)       
    axrs= ifelse(ea >= age_superFirst, 0, c(get_tla(pxT[age < age_superFirst], i,  sx[age < age_superFirst]), rep(0, max.age - unique(age_superFirst) + 1))),  # Similar to axRs, but based on age_superFirst. For calculation of term benefits when costs are spread up to age_superFirst. (vary across groups)
    
    ayx = c(get_tla2(pxT[age <= r.max], i), rep(0, max.age - r.max)),                     # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= r.max], i,  sx[age <= r.max]), rep(0, max.age - r.max))   # need to make up the length of the vector up to age max.age
  )


# liab.active %>% select(Tier, ea, age, DC_EEC, DC_ERC, DC_rate.tot) %>% filter(start.year == 2017, ea == 20)



#*************************************************************************************************************
#                        2.1  ALs and NCs of life annuity and contingent annuity for actives             #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.laca = ifelse(elig_super == 1, 1,
                    ifelse(elig_super == 0 & elig_early == 1, (1 - 0.03 * (age_superFirst - age)),  0)),
          # gx.laca = 0,
  Bx.laca  = gx.laca * Bx,  # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
  TCx.la   = lead(Bx.laca) * qxr.la * lead(ax.r.W) * v,         # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
  TCx.ca   = lead(Bx.laca) * qxr.ca * lead(liab.ca.sum.1) * v,  # term cost of contingent annuity at the internal retirement age x (start to claim benefit at age x + 1)
  TCx.laca = TCx.la + TCx.ca,
  
  # For PERSE: PV of DB benefit
  DB.PVBx  = Bx * ax.r.W,
  DB.value = Bx.laca * ax.r.W,
  
  # TCx.r = Bx.r * qxr.a * ax,
  PVFBx.laca  = c(get_PVFB(pxT[age <= r.max], v, TCx.laca[age <= r.max]), rep(0, max.age - r.max)),
  
  ## NC and AL of UC
  # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
  # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
  
  # # NC and AL of PUC
  # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
  # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
  # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),

  # NC and AL of EAN.CD
  NCx.EAN.CD.laca = ifelse(age < r.max, PVFBx.laca[age == min(age)]/ayx[age == r.max], 0),
  ALx.EAN.CD.laca = PVFBx.laca - NCx.EAN.CD.laca * axR,
  
  # NC and AL of EAN.CP
  NCx.EAN.CP.laca   = ifelse(age < r.max, sx * PVFBx.laca[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
  PVFNC.EAN.CP.laca = NCx.EAN.CP.laca * axRs,
  ALx.EAN.CP.laca   = PVFBx.laca - PVFNC.EAN.CP.laca
  ) 



#*************************************************************************************************************
#                       2.2   ALs and benefits for retirees with life annuity                        #####                  
#*************************************************************************************************************

# Calculate AL and benefit payment for retirees having retired at different ages.
liab.la <- rbind(
  # grids for initial retirees in year 1
    # To simplified the calculation, it is assmed that all initial retirees entered the workforce at age r.min - 1 and 
    # retiree right in year 1. This assumption will cause the retirement age and yos of some of the retirees not compatible with the eligiblility rules,
    # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial retirees/beneficiaries.
  # expand.grid(ea         = min(benefit_$age) - 1,
  #             age.r      = benefit_$age, # This ensures that year of retirement is year 1.
  #             start.year = init.year - (benefit_$age - (r.min - 1)),
  #             age        = range_age[range_age >= min(benefit_$age)]) %>%
  # filter(age >= ea,
  #        age >= age.r),
  
  expand.grid(
    age.r      = benefit_$age, # This ensures that year of retirement is year 1.
    age        = range_age[range_age >= min(benefit_$age)]) %>%
    mutate(ea         = min(benefit_$age) - 1,
           start.year = init.year - (age.r - ea)) %>% 
    filter(# age >= ea,
           age >= age.r),
  

  # grids for who retire after year 1.
  expand.grid(ea         = range_ea[range_ea < r.max],
              age.r      = r.min:r.max,
              start.year = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
              age        = range_age[range_age >=r.min]) %>%
    filter(age   >= ea,
           age.r >= ea,
           age   >= age.r,
           start.year + (age.r - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
           start.year + age - ea     >= init.year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start.year,ea,age.r,age")




liab.la <- liab.la[!duplicated(liab.la %>% select(start.year, ea, age, age.r ))]
 
 
liab.la <- merge(liab.la,
                 select(liab.active, start.year, ea, age, Bx.laca, COLA.scale, gx.laca, sx) %>% data.table(key = "ea,age,start.year"),
                 all.x = TRUE, 
                 by = c("ea", "age","start.year")) %>%
           arrange(start.year, ea, age.r) %>% 
           as.data.frame %>% 
           left_join(select(mortality.post.model_, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
           left_join(benefit_)

# liab.la %>% filter(start.year == 1990, ea == 35, age.r == 62)


liab.la %<>% as.data.frame  %>% # filter(start.year == -41, ea == 21, age.retire == 65) %>%
  # filter(ea == 54) %>%
  group_by(start.year, ea, age.r) %>%
  mutate(
    COLA.scale = (1 + cola)^(age - min(age)), # COLA.scale from liab.active may not cover all starting years.    
    year   = start.year + age - ea,
    year.r = start.year + age.r - ea, # year of retirement
    Bx.laca  = ifelse(is.na(Bx.laca), 0, Bx.laca),  # just for safety
    B.la   = ifelse(year.r <= init.year,
                    benefit[year == init.year] * COLA.scale / COLA.scale[year == init.year],      # Benefits for initial retirees
                    Bx.laca[age == age.r] * COLA.scale / COLA.scale[age == age.r]),               # Benefits for retirees after year 1
    ALx.la = B.la * ax.r.W.ret                                                                    # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA

    
  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init.year, len = nyear) ) %>%
  select(year, ea, age, year.r, age.r, start.year, B.la, ALx.la, sx) %>% 
  arrange(age.r, start.year, ea, age)


# Spot check
# liab.la %>% filter(start.year == 2016, ea == 21, age.r == 55) %>% as.data.frame()





#*************************************************************************************************************
#                         3.1 AL and NC of deferred benefits for actives                        #####
#*************************************************************************************************************

# Calculate normal costs and liabilities of deferred retirement benefits
# Vested terms begin to receive deferred retirement benefit at r.vben, after which they are considered as retirees in the AV.(But still terms in the model)
# Notes on deferred retirement benefits for vested terms.
# 1. Note that the PVFB and AL are different at age r.min - 1. This is very different from the case for retirement benefits with single retirement age, where PVFB = AL for EAN actuarial methods
#    at age r.max
# 2. During each year with a positive probability of termination, a proportion of the active member liability will be shifted to vested term liabilities as active members quit their jobs. At each
#    new period, changes in the liability side are: reduction in PVFB, increase in AL for terminated and increase in -PVFNC(by NC). Note the first two parts cancel out, so the
#    increase in liability side is equal to NC. If the amount of NC is fully contributed to the asset side, the balance sheet will remain balanced.
#
# CAUTION!: There will be a problem if actives entering after r.min can get vested, when PVFB is only amortized up to age r.min

liab.active %<>%
  mutate(gx.v = ifelse(elig_vest == 1, 1, 0),  # actives become vested after reaching v.yos years of yos
         
         # For PSERS: Vested terms begin to receive benefit 1 year after termination. (use the same method as disability benefit.)
         Bx.v  = gx.v * bfactor * yos * fas * 0.777, 
         
         # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
         TCx.v      = qxt * v * lead(Bx.v) *  lead(ax.vben) , # term cost of life annuity at the disability age x (start to claim benefit at age x + 1)
         
         PVFBx.v  = c(get_PVFB(pxT[age <= r.max], v, TCx.v[age <= r.max]), rep(0, max.age - r.max)),
        
         ##### WARNING: r.vben replaced by r.max below in the calculation of NC and AL
             #          axr replace by axR; axrs replaced by axRs
         
         
         # Bx.v = ifelse(ea < r.vben, 
         #               gx.v * bfactor * yos * fas, 0), # initial annuity amount when the vested term retires at age r.vben, when a employee is vested at a certain age.
         # 
         # TCx.v   = ifelse(ea < r.vben, Bx.v * qxt * lead(px_r.vben_m) * v^(r.vben - age) * ax.r.W[age == r.vben], 0),             # term cost of vested termination benefits. We assume term rates are 0 after r.vben.
         # PVFBx.v = ifelse(ea < r.vben, c(get_PVFB(pxT[age < r.vben], v, TCx.v[age < r.vben]), rep(0, max.age - r.vben + 1)), 0),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).

         
         
         
         # # NC and AL of PUC
         # TCx.vPUC = TCx.v / (age - min(age)),
         # NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         # ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),

         # NC and AL of EAN.CD
         NCx.EAN.CD.v = ifelse(age < r.max, PVFBx.v[age == min(age)]/ayx[age == r.max], 0), # for testing spreading NC.v up to r.vben
         ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axR,

         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < r.max, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]) * sx, 0),  # for testing spreading NC.v up to r.vben
         ALx.EAN.CP.v = PVFBx.v - NCx.EAN.CP.v * axRs
  ) 
  


#*************************************************************************************************************
#                       3.2 AL for vested terminatede members                        #####
#*************************************************************************************************************

## Calculate AL and benefit payment for initial vested terms.

init_terminated_ %<>%  
  mutate(year = init.year,
         age.term = age - 1,         # assume all terms are terminated in init.year - 1.
         yos = age - ea,
         start.year = year - (age - ea))

# init_terminated_

liab.term.init <- expand.grid(ea         = unique(init_terminated_$ea),
                              age.term   = unique(init_terminated_$age.term),
                              start.year = unique(init_terminated_$start.year),
                              age = range_age) %>%
  filter(start.year + age - ea >= 1,
         age >= ea,
         age.term >= ea) %>%
  left_join(init_terminated_ %>% select(ea, age.term, start.year, yos, benefit.term = benefit)) %>%
  left_join(select(liab.active, start.year, ea, age, COLA.scale, pxRm, px_r.vben_m, px_r.vsuper_m, ax.vben, pxm.term)) %>%
  # left_join(mortality.post.model_ %>% filter(age.r == r.vben) %>% select(age, ax.r.W.term = ax.r.W)) %>%
  group_by(start.year, ea, age.term) %>%

  mutate(
    year = start.year + age - ea,
    age.ben  = age.term + 1,   # ifelse(age[year == init.year] > r.vben, age[year == init.year], r.vben), # Age at which term starts to receive benefit. 

    year.term = year[age == age.term],

    COLA.scale = (1 + cola)^(age - min(age)),        # COLA.scale in liab.active does not trace back long enough
    ax.vben     = get_tla(pxm.term, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
    
    # PSERS for now    
    Bx.v  = benefit.term, #
    B.v   = benefit.term[year == init.year] * COLA.scale / COLA.scale[year == init.year], 
      
      # ifelse(year.term <= init.year,
      #              benefit.term[year == init.year] * COLA.scale / COLA.scale[year == init.year],  # Benefits for initial retirees
      #              Bx.v[age == age.term]   * COLA.scale / COLA.scale[age  == age.term]),  # Benefits for disability retirees after year 1
    
    ALx.v   = B.v * ax.vben     
    
    # 
    # B.v   = ifelse(age.ben > r.vben, 0,   ifelse(age >= r.vben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.vben], 0)),  # Benefit payment after r.vben, for age.ben == r.vben
    # B.v   = ifelse(age.ben == r.vben, B.v, ifelse(age >= age.ben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == age.ben], 0)), # for age.ben > r.vben
    # ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term)] * ax.r.W.term[age == r.vben] * px_r.vben_m * v^(r.vben - age), # liab before receiving benefits
    #                B.v * ax.r.W.term)
    ) %>%                                                                                     # liab after receiving benefits      
  ungroup %>%
  select(ea, age, start.year, year, year.term, B.v, ALx.v) %>%
  filter(year %in% seq(init.year, len = nyear),
         year.term == init.year - 1)

# liab.term.init %>% filter(start.year == 2007, ea == 55, age.term == 63) %>% data.frame()


##  Calculate AL and benefit payment for vested terms terminating at different ages.
# Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab.term <- expand.grid(# start.year   = (init.year - (r.vben - 1 - min.age)):(init.year + nyear - 1), # 2015
                         start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
                         ea = range_ea[range_ea < r.max], 
                         age = range_age, 
                         age.term = range_age[range_age < r.max]) %>% 
  filter(start.year + max.age - ea >= init.year, 
         age >= ea, age.term >= ea,
         age >= age.term) %>% # drop redundant combinations of start.year and ea.
  data.table(key = "ea,age,start.year,age.term") 


liab.term <- merge(liab.term,
                   select(liab.active, start.year, year, ea, age, Bx.v, COLA.scale, pxRm, px_r.vben_m, px_r.vsuper_m, ax.vben, pxm.term) %>% data.table(key = "ea,age,start.year"),
                   all.x = TRUE, by = c("ea", "age","start.year")) %>% as.data.frame
             # left_join(mortality.post.model_ %>% filter(age.r == r.vben) %>% select(age, ax.r.W.term = ax.r.W))   # load present value of annuity for retirement age r.vben

liab.term %<>% as.data.frame %>%
  group_by(start.year, ea, age.term) %>%
  mutate(year.term = year[age == age.term],
         
         COLA.scale = (1 + cola)^(age - min(age)),        # COLA.scale in liab.active does not trace back long enough
         ax.vben     = get_tla(pxm.term, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
         
         # PSERS for now
         B.v     = Bx.v[age == age.term] * COLA.scale / COLA.scale[age == age.term],          # Benefits for disability retirees after year 1
         ALx.v   = B.v * ax.vben     

         # B.v   = ifelse(age >= r.vben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.vben], 0),  # Benefit payment after r.vben
         # ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term)] * ax.r.W.term[age == r.vben] * px_r.vben_m * v^(r.vben - age),
         #                B.v * ax.r.W.term)

  ) %>%
  ungroup  %>%
  # select(#-start.year, -age.term,
  #        -Bx.v, -ax.r.W, -COLA.scale, -pxRm) %>%
  select(ea, age, start.year, year, year.term, B.v, ALx.v) %>%
  # select(-age.term, -Bx.v, -ax.r.W.term, -COLA.scale, -pxRm, - px_r.vben_m, -age.r, -px_r.vsuper_m, -ax.vben, -pxm.term) %>%
  filter(year %in% seq(init.year, len = nyear)) 




liab.term <-  bind_rows(list(liab.term.init,                                  # Using rbind produces duplicated rows with unknown reasons. Use bind_rows from dplyr instead.
                             filter(liab.term, year.term != init.year - 1)))



#*************************************************************************************************************
#                        4.1  ALs and NCs of benefit for death before retirement, for actives                  #####                  
#*************************************************************************************************************
# QSS: qualified Surviving Spouse
# pct.QSS <- pct.ca.F * pct.female + pct.ca.M * pct.male
# pct.QSS



# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.death  = ifelse(elig_super == 1, 1,
                      ifelse(elig_super == 0 & elig_early == 1, (1 - 0.03 * (age_superFirst - age)),  0)),
          
          # gx.death = 0,
          
          Bx.death  = gx.death * yos * bfactor * fas, # annuity that would have been effective if the member retired on the 
          
          # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
          # For PSERS: 1. Lump sum death benefit equal to PV of future benefit (Bx.death * ax.deathBen);
          #            2. Death benefit are assumed to be claimed 1 year after death      
          TCx.death = qxm.pre * v * lead(Bx.death) *  lead(ax.deathBen), # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)

          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.death  = c(get_PVFB(pxT[age <= r.max], v, TCx.death[age <= r.max]), rep(0, max.age - r.max)),
          
          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          
          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
          
          
          ## Under EAN methods, costs are spread up to r.max
          # NC and AL of EAN.CD
          NCx.EAN.CD.death = ifelse(age < r.max, PVFBx.death[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.death = PVFBx.death - NCx.EAN.CD.death * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.death   = ifelse(age < r.max, sx * PVFBx.death[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.death = NCx.EAN.CP.death * axRs,
          ALx.EAN.CP.death   = PVFBx.death - PVFNC.EAN.CP.death
  ) 



#*************************************************************************************************************
#                       4.2   ALs and benefits for QSS for death benefit before retirement               #####                  
#*************************************************************************************************************

liab.death <- rbind(
  # grids for who die after year 1.
  expand.grid(ea           = range_ea[range_ea < r.max],
              age.death = min.age:r.max,
              start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
              age          = range_age) %>%
    filter(age   >= ea,
           age.death >= ea,
           age   >= age.death,
           start.year + (age.death - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
           start.year + age - ea >= init.year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start.year,ea,age.death,age")


liab.death <- merge(liab.death,
                 select(liab.active, start.year, ea, age, Bx.death, COLA.scale, gx.death, ax.deathBen, pxm.deathBen) %>% data.table(key = "ea,age,start.year"),
                 all.x = TRUE, 
                 by = c("ea", "age","start.year")) %>%
  arrange(start.year, ea, age.death) %>% 
  as.data.frame 
  #%>% 
  # left_join(select(mortality.post.model_, age, age.r, ax.r.W.ret = ax.r.W)) %>%  #  load present value of annuity for all retirement ages, ax.r.W in liab.active cannot be used anymore. 
  #left_join(benefit_)


liab.death %<>% as.data.frame  %>% 
  group_by(start.year, ea, age.death) %>%
  mutate(
    
    # COLA.scale = (1 + cola)^(age - min(age)),         # COLA.scale in liab.active does not trace back long enough
    ax.deathBen = get_tla(pxm.deathBen, i, COLA.scale), # COLA.scale in liab.active does not trace back long enough
    
    year       = start.year + age - ea,
    year.death = start.year + age.death - ea, # year of death of the active
    Bx.death   = ifelse(is.na(Bx.death), 0, Bx.death),  # just for safety
    
    # For PSERS: Lump sum death benefit equal to PV of future benefit. Benefit claim 1 year after death
    B.death    = ifelse(age == age.death, Bx.death * ax.deathBen, 0),   # Bx.death[age == age.death] * COLA.scale / COLA.scale[age == age.death],               # Benefits for retirees after year 1
    ALx.death  = ifelse(age == age.death, B.death, 0)                   # B.death * ax.deathBen                                                                # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA
    
  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init.year, len = nyear) ) %>%
  select(year, ea, age, year.death, age.death, start.year, B.death, ALx.death) %>% 
  arrange(age.death, start.year, ea, age)



#*************************************************************************************************************
#                        5.1  ALs and NCs of disability benefit, for actives                  #####                  
#*************************************************************************************************************



# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.disb  = ifelse(yos < 5, 0, 
                     ifelse(yos < 17,  min(17/yos, (age_superFirst - ea)/yos),  1)
                     ),
          Bx.disb  = gx.disb * bfactor * yos * fas, 
          
          # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
          TCx.disb.la = qxd.la * v * lead(Bx.disb) *  lead(ax.disb.la) , # term cost of life annuity at the disability age x (start to claim benefit at age x + 1)
          TCx.disb.ca = qxd.ca * v * lead(Bx.disb) *  lead(liab.disb.ca.sum.1),
          TCx.disb.laca = TCx.disb.la + TCx.disb.ca,
          
          # TCx.r = Bx.r * qxr.a * ax,
          PVFBx.disb  = c(get_PVFB(pxT[age <= r.max], v, TCx.disb.laca[age <= r.max]), rep(0, max.age - r.max)),
          
          ## NC and AL of UC
          # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
          # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
          
          # # NC and AL of PUC
          # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
          # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
          # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
          
          # NC and AL of EAN.CD
          NCx.EAN.CD.disb = ifelse(age < r.max, PVFBx.disb[age == min(age)]/ayx[age == r.max], 0),
          ALx.EAN.CD.disb = PVFBx.disb - NCx.EAN.CD.disb * axR,
          
          # NC and AL of EAN.CP
          NCx.EAN.CP.disb   = ifelse(age < r.max, sx * PVFBx.disb[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
          PVFNC.EAN.CP.disb = NCx.EAN.CP.disb * axRs,
          ALx.EAN.CP.disb   = PVFBx.disb - PVFNC.EAN.CP.disb
  ) 



#*************************************************************************************************************
#                       5.2   ALs and benefits for disability benefit               #####                  
#*************************************************************************************************************

liab.disb.la <- rbind(
  # grids for initial retirees in year 1
  # To simplified the calculation, it is assmed that all initial disabled entered the workforce at age min.age and 
  # become disabled in year 1. This assumption will cause the age of disability and yos of some of the disabled not compatible with the eligiblility rules,
  # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial disabled.
  expand.grid(age.disb   = benefit.disb_$age, # This ensures that year of retirement is year 1.
              age        = range_age) %>%
    mutate(ea            = unique(benefit.disb_$ea),
           start.year    = init.year - (age.disb - ea)) %>% 
    filter(age >= age.disb),
  
  # grids for who die after year 1.
  expand.grid(ea           = range_ea[range_ea < r.max],
              age.disb     = min.age:r.max,
              start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
              age          = range_age) %>%
    filter(age   >= ea,
           age.disb >= ea,
           age   >= age.disb,
           start.year + (age.disb - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
           start.year + age - ea >= init.year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start.year,ea,age.disb,age")




liab.disb.la <- liab.disb.la[!duplicated(liab.disb.la %>% select(start.year, ea, age, age.disb ))]


liab.disb.la <- merge(liab.disb.la,
                    select(liab.active, start.year, ea, age, Bx.disb, COLA.scale, gx.disb, ax.disb.la #pxm.d
                           ) %>% data.table(key = "ea,age,start.year"),
                    all.x = TRUE, 
                    by = c("ea", "age","start.year")) %>%
  arrange(start.year, ea, age.disb) %>% 
  as.data.frame %>% 
  left_join(benefit.disb_)


liab.disb.la %<>% as.data.frame  %>% 
  left_join(decrement.model_ %>% select(ea, age, pxm.d)) %>%  # getting pxm.d from liab.active causes problem
  group_by(start.year, ea, age.disb) %>%
  mutate(
    year       = start.year + age - ea,
    COLA.scale = (1 + cola)^(age - min(age)),    # COLA.scale in liab.active does not trace back long enough
    ax.disb.la = get_tla(pxm.d, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
    year.disb  = start.year + age.disb - ea,     # year of disability of the active
    Bx.disb    = ifelse(is.na(Bx.disb), 0, Bx.disb),  # just for safety
    B.disb.la     = ifelse(year.disb <= init.year,
                           benefit.disb[year == init.year] * COLA.scale / COLA.scale[year == init.year],  # Benefits for initial retirees
                           Bx.disb[age == age.disb] * COLA.scale / COLA.scale[age == age.disb]),          # Benefits for disability retirees after year 1
    ALx.disb.la   = B.disb.la * ax.disb.la                                                                # Liability for remaining diability benefits, PV of all future benefit adjusted with COLA
    
  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init.year, len = nyear) ) %>%
  select(year, ea, age, year.disb, age.disb, start.year, B.disb.la, ALx.disb.la, pxm.d) 
  #%>%   arrange(age.disb, start.year, ea, age)



#*************************************************************************************************************
#                 # 6.  Choosing AL and NC variables corresponding to the chosen acturial methed             #####
#*************************************************************************************************************


liab.active %<>% ungroup %>% select(start.year, year, ea, age, everything())


ALx.laca.method   <- paste0("ALx.", actuarial_method, ".laca")
NCx.laca.method   <- paste0("NCx.", actuarial_method, ".laca")

ALx.death.method   <- paste0("ALx.", actuarial_method, ".death")
NCx.death.method   <- paste0("NCx.", actuarial_method, ".death")

ALx.disb.method   <- paste0("ALx.", actuarial_method, ".disb")
NCx.disb.method   <- paste0("NCx.", actuarial_method, ".disb")

ALx.v.method <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method <- paste0("NCx.", actuarial_method, ".v")



var.names <- c("sx", ALx.laca.method, NCx.laca.method, 
                     ALx.v.method, NCx.v.method, 
                     ALx.death.method, NCx.death.method,
                     ALx.disb.method, NCx.disb.method,
                     "PVFBx.laca", "PVFBx.v", "PVFBx.death", "PVFBx.disb", "Bx.laca", "Bx.disb", "DC_EEC", "DC_ERC", "DB.value", "DB.PVBx")
liab.active %<>% 
  filter(year %in% seq(init.year, len = nyear)) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename_("ALx.laca"   = ALx.laca.method,  "NCx.laca"   = NCx.laca.method, 
          "ALx.v"      = ALx.v.method,     "NCx.v"      = NCx.v.method,
          "ALx.death"  = ALx.death.method, "NCx.death"  = NCx.death.method,
          "ALx.disb"   = ALx.disb.method,  "NCx.disb"   = NCx.disb.method
           )   # Note that dplyr::rename_ is used. 



## Final outputs
  # liab.active
  # liab.la
  # liab.term


liab <- list(active = liab.active, 
             la = liab.la, 
             term = liab.term, 
             death = liab.death, 
             disb.la = liab.disb.la)

}

