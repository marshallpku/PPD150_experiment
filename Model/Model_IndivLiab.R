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
                         decrement_model_ = decrement_model$decrement.model,
                         salary_          = salary,
                         benefit_         = benefit,
                         # init_terms_all_  = init_terms_all,
                         paramlist = planData_list$inputs_singleValues){

# Inputs
  # decrement_model_ = decrement_model$decrement.model
  # salary_          = salary
  # benefit_         = benefit
  # # init_terms_all_  = init_terms_all,
  # paramlist = planData_list$inputs_singleValues
  
assign_parmsList(paramlist, envir = environment()) # environment() returns the local environment of the function.

# parameters to be added
# fasyears <- tier.param[Tier_select_, "fasyears"]
# cola     <- tier.param[Tier_select_, "cola"]
# bfactor  <- tier.param[Tier_select_, "bfactor"]
# 
v <- 1/(1 + i)
actuarial_method <- "EAN.CP"

#*************************************************************************************************************
#                               1. Preparation                        #####                  
#*************************************************************************************************************

min.year <- min(init.year - (age_max - (retage_max - 1)), 
                init.year - (retage_max - 1 - ea_min)) 
                # min(init.year - (benefit_$age - (r.min - 1))))

## Track down to the year that is the smaller one of the two below: 
# the year a 120-year-old retiree in year 1 entered the workforce at age r.max - 1 (remeber ea = r.max - 1 is assigned to all inital retirees)
# the year a r.max year old active in year 1 enter the workforce at age min.ea 

# EEC_pct.entrants <- EEC_pct.future
# bfactor_current <- bfactor_avg

#liab.active 
liab.active <- 
  expand.grid(start.year = min.year:(init.year + nyear - 1) , 
                      ea = range_ea, age = range_age) %>%
  filter(start.year + age_max - ea >= init.year, # drop redundant combinations of start.year and ea. (delet those who never reach year 1.) 
         age >= ea) %>%  
  mutate(year = start.year + age - ea) %>%  # year index in the simulation)
  arrange(start.year, ea, age) %>% 
  left_join(salary_) %>%
  left_join(decrement_model_) %>% 
  group_by(start.year, ea) %>%
  
  # Calculate salary and benefits
  mutate(
    yos= age - ea,
    
    # Employee contribution (different rates for current future members) 
    EEC_rate = ifelse(start.year <= init.year, EEC_pct.current, EEC_pct.entrants),
    EEC      = sx * EEC_rate,
    
    
    Sx = ifelse(age == min(age), 0, lag(cumsum(sx))),  # Cumulative salary
    n  = pmin(yos, FAS_year),                          # years used to compute fas
    fas= ifelse(yos < FAS_year, Sx/n, (Sx - lag(Sx,FAS_year))/n), # final average salary
    fas= ifelse(age == min(age), 0, fas),
    COLA.scale = (1 + cola)^(age - min(age)),     # later we can specify other kinds of COLA scale. Note that these are NOT COLA factors. They are used to derive COLA factors for different retirement ages.
    
  
    # Benefit level, with different benefit factors for current and future members 
    benfactor = ifelse(start.year <= init.year, bfactor_current, bfactor_entrants),
    benfactor = ifelse(yos  == 0, 0, benfactor),
    Bx = na2zero(cumsum(benfactor) * fas),
    # Bx = na2zero(bfactor * yos * fas), # old code for accrued benefits with single bfactor, note that only Bx for ages above r.min are necessary under EAN.
    bx = lead(Bx) - Bx,                  # benefit accrual at age x

    # actuarial present value of future benefit, for $1's benefit in the initial year. 
    ax.vben     = get_tla(pxm.term, i, COLA.scale),   
    ax.r        = get_tla(pxm.post, i, COLA.scale),       # ax calculated with mortality table for retirees. 
    
    axR = c(get_tla(pxT[age < retage_max], i), rep(0, age_max - retage_max + 1)),                        # aT..{x:r.max-x-|} discount value of r.max at age x, using composite decrement       
    axRs= c(get_tla(pxT[age < retage_max], i,  sx[age < retage_max]), rep(0, age_max - retage_max + 1)),       # ^s_aT..{x:r.max-x-|}
    
    # axr = ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i), rep(0, max.age - r.min + 1))),                 # Similar to axR, but based on r.min.  For calculation of term benefits when costs are spread up to r.min.        
    # axrs= ifelse(ea >= r.min, 0, c(get_tla(pxT[age < r.min], i, sx[age<r.min]), rep(0, max.age - r.min + 1))),  # Similar to axRs, but based on r.min. For calculation of term benefits when costs are spread up to r.min.
    
    axr = ifelse(ea >= retage_normal, 0, c(get_tla(pxT[age < retage_normal], i), rep(0, age_max - retage_normal + 1))),                   # Similar to axR, but based on r.vben.  For calculation of term benefits when costs are spread up to r.vben.
    axrs= ifelse(ea >= retage_normal, 0, c(get_tla(pxT[age < retage_normal], i,  sx[age < retage_normal]), rep(0, age_max - retage_normal + 1))),  # Similar to axRs, but based on r.vben. For calculation of term benefits when costs are spread up to r.vben.

    ayx = c(get_tla2(pxT[age <= retage_max], i), rep(0, age_max - retage_max)),                          # need to make up the length of the vector up to age max.age
    ayxs= c(get_tla2(pxT[age <= retage_max], i,  sx[age <= retage_max]), rep(0, age_max - retage_max))   # need to make up the length of the vector up to age max.age
  )


# liab.active %>% select(Tier, ea, age, DC_EEC, DC_ERC, DC_rate.tot) %>% filter(start.year == 2017, ea == 20)


#*************************************************************************************************************
#                        2.1  ALs and NCs of life annuity            #####                  
#*************************************************************************************************************

# Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
liab.active %<>%   
  mutate( gx.la = ifelse(yos < ret_yos, 0,  
                         ifelse(age > retage_normal, 1, 
                                ifelse(age %in% retage_early:retage_normal, 1 - (retage_normal - age) * earlyRet_reduction, 0))), # reduction factor for early retirement benefits. Early retirement has a penalty factor on benefit. 
          # gx.laca = 0,
  Bx.la    = gx.la * Bx,                                 # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
  TCx.la   = qxr * lead(Bx.la)* lead(ax.r) * v,       # term cost of life annuity at the internal retirement age x (start to claim benefit at age x + 1)
  PVFBx.la  = c(get_PVFB(pxT[age <= retage_max], v, TCx.la[age <= retage_max]), rep(0, age_max - retage_max)),
  
  ## NC and AL of UC
  # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
  # NCx.UC = bx * c(get_NC.UC(pxT[age <= retage_max], v, TCx.r1[age <= retage_max]), rep(0, 45)),
  # ALx.UC = Bx * c(get_PVFB(pxT[age <= retage_max], v, TCx.r1[age <= retage_max]), rep(0, 45)),
  
  # # NC and AL of PUC
  # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
  # NCx.PUC = c(get_NC.UC(pxT[age <= retage_max], v, TCx.rPUC[age <= retage_max]),  rep(0, age_max - retage_max)),
  # ALx.PUC = c(get_AL.PUC(pxT[age <= retage_max], v, TCx.rPUC[age <= retage_max]), rep(0, age_max - retage_max)),

  # NC and AL of EAN.CD
  # NCx.EAN.CD.la = ifelse(age < retage_max, PVFBx.la[age == min(age)]/ayx[age == retage_max], 0),
  # ALx.EAN.CD.la = PVFBx.la - NCx.EAN.CD.la * axR,
  
  # NC and AL of EAN.CP
  NCx.EAN.CP.la   = ifelse(age < retage_max, sx * PVFBx.la[age == min(age)]/(sx[age == min(age)] * ayxs[age == retage_max]), 0),
  PVFNC.EAN.CP.la = NCx.EAN.CP.la * axRs,
  ALx.EAN.CP.la   = PVFBx.la - PVFNC.EAN.CP.la
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

  expand.grid(
    age.r      = benefit_$age, # This ensures that year of retirement is year 1.
    age        = range_age[range_age >= min(benefit_$age)]) %>%
    mutate(ea         = min(benefit_$age) - 1,
           start.year = init.year - (age.r - ea)) %>% 
    filter(# age >= ea,
           age >= age.r),
  
  # grids for who retire after year 1.
  expand.grid(ea         = range_ea[range_ea < retage_max],
              age.r      = retage_early:retage_max,
              start.year = (init.year + 1 - (retage_max - min(range_ea))):(init.year + nyear - 1),
              age        = range_age[range_age >= retage_early]) %>%
    filter(age   >= ea,
           age.r >= ea,
           age   >= age.r,
           start.year + (age.r - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
           start.year + age - ea     >= init.year + 1) # not really necessary since we already have age >= age.r
) %>%
  data.table(key = "start.year,ea,age.r,age")

liab.la <- liab.la[!duplicated(liab.la %>% select(start.year, ea, age, age.r))]
 
 
liab.la <- merge(liab.la,
                 select(liab.active, start.year, ea, age, Bx.la, COLA.scale, gx.la, sx) %>% data.table(key = "ea,age,start.year"),
                 all.x = TRUE, 
                 by = c("ea", "age","start.year")) %>%
           arrange(start.year, ea, age.r) %>% 
           as.data.frame %>% 
           left_join(benefit_) %>% 
           left_join(decrement_model_ %>% select(ea, age, pxm.post))

# liab.la %>% filter(start.year == 1990, ea == 35, age.r == 62)


liab.la %<>% as.data.frame  %>% 
  group_by(start.year, ea, age.r) %>%
  mutate(
    COLA.scale = (1 + cola)^(age - min(age)), # COLA.scale from liab.active may not cover all starting years.    
    ax.r        = get_tla(pxm.post, i, COLA.scale),
    year   = start.year + age - ea,
    year.r = start.year + age.r - ea, # year of retirement
    Bx.la  = na2zero(Bx.la),  # just for safety
    B.la   = ifelse(year.r <= init.year,
                      benefit[year == init.year] * COLA.scale / COLA.scale[year == init.year],    # Benefits for initial retirees
                      Bx.la[age == age.r] * COLA.scale / COLA.scale[age == age.r]),               # Benefits for retirees after year 1
    ALx.la = B.la * ax.r                                                                          # Liability for remaining retirement benefits, PV of all future benefit adjusted with COLA

    
  ) %>% ungroup %>%
  # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
  filter(year %in% seq(init.year, len = nyear) ) %>%
  select(year, ea, age, year.r, age.r, start.year, B.la, ALx.la, sx, ax.r, pxm.post) %>% 
  arrange(age.r, start.year, ea, age)


# Spot check
# liab.la %>% filter(start.year == 2016, ea == 21, age.r == 55) %>% as.data.frame()
# liab.la %>% filter(start.year == 1961)




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

if(model_term){
liab.active %<>%
  mutate(gx.v = ifelse(yos >= vest_yos, 1, 0), # actives become vested after reaching v.yos years of yos
         # gx.v = 0,
         
         Bx.v    = ifelse(ea < retage_normal, Bx * gx.v, 0), # initial annuity amount when the vested term retires at age r.vben, when a employee is vested at a certain age.
         TCx.v   = ifelse(ea < retage_normal, Bx.v * qxt * lead(px_retage_normal_m) * v^(retage_normal - age) * ax.vben[age == retage_normal], 0),               # term cost of vested termination benefits. We assume term rates are 0 after r.vben.
         PVFBx.v = ifelse(ea < retage_normal, c(get_PVFB(pxT[age < retage_normal], v, TCx.v[age < retage_normal]), rep(0, age_max - retage_normal + 1)), 0),  # To be compatible with the cases where workers enter after age r.min, r.max is used instead of r.min, which is used in textbook formula(winklevoss p115).

         
         # # NC and AL of PUC
         # TCx.vPUC = TCx.v / (age - min(age)),
         # NCx.PUC.v = c(get_NC.UC(pxT[age <= r.max],  v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),
         # ALx.PUC.v = c(get_AL.PUC(pxT[age <= r.max], v, TCx.vPUC[age <= r.max]), rep(0, max.age - r.max)),

         # NC and AL of EAN.CD
         # NCx.EAN.CD.v = ifelse(age < retage_normal, PVFBx.v[age == min(age)]/ayx[age == retage_normal], 0), # for testing spreading NC.v up to r.vben
         # ALx.EAN.CD.v = PVFBx.v - NCx.EAN.CD.v * axr,

         # NC and AL of EAN.CP
         NCx.EAN.CP.v = ifelse(age < retage_normal, PVFBx.v[age == min(age)]/(sx[age == min(age)] * ayxs[age == retage_normal]) * sx, 0),  # for testing spreading NC.v up to r.vben
         ALx.EAN.CP.v = PVFBx.v - NCx.EAN.CP.v * axrs
  ) 

  } else {
  liab.active %<>% 
    mutate(Bx.v = 0,
           PVFBx.v = 0,
           NCx.EAN.CP.v = 0,
           ALx.EAN.CP.v = 0)
}  


#*************************************************************************************************************
#                       3.2 AL for vested terminatede members                        #####
#*************************************************************************************************************

# ## Calculate AL and benefit payment for initial vested terms.
# 
# init_terminated_ %<>%  
#   mutate(year = init.year,
#          age.term = age - 1,         # assume all terms are terminated in init.year - 1.
#          yos = age - ea,
#          start.year = year - (age - ea))
# 
# # init_terminated_
# 
# liab.term.init <- expand.grid(ea         = unique(init_terminated_$ea),
#                               age.term   = unique(init_terminated_$age.term),
#                               start.year = unique(init_terminated_$start.year),
#                               age = range_age) %>%
#   filter(start.year + age - ea >= 1,
#          age >= ea,
#          age.term >= ea) %>%
#   left_join(init_terminated_ %>% select(ea, age.term, start.year, yos, benefit.term = benefit)) %>%
#   left_join(select(liab.active, start.year, ea, age, COLA.scale, pxRm, px_r.vben_m, px_r.vsuper_m, ax.vben, pxm.term)) %>%
#   # left_join(mortality.post.model_ %>% filter(age.r == r.vben) %>% select(age, ax.r.W.term = ax.r.W)) %>%
#   group_by(start.year, ea, age.term) %>%
# 
#   mutate(
#     year = start.year + age - ea,
#     age.ben  = age.term + 1,   # ifelse(age[year == init.year] > r.vben, age[year == init.year], r.vben), # Age at which term starts to receive benefit. 
# 
#     year.term = year[age == age.term],
# 
#     COLA.scale = (1 + cola)^(age - min(age)),        # COLA.scale in liab.active does not trace back long enough
#     ax.vben     = get_tla(pxm.term, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
#     
#     # PSERS for now    
#     Bx.v  = benefit.term, #
#     B.v   = benefit.term[year == init.year] * COLA.scale / COLA.scale[year == init.year], 
#       
#       # ifelse(year.term <= init.year,
#       #              benefit.term[year == init.year] * COLA.scale / COLA.scale[year == init.year],  # Benefits for initial retirees
#       #              Bx.v[age == age.term]   * COLA.scale / COLA.scale[age  == age.term]),  # Benefits for disability retirees after year 1
#     
#     ALx.v   = B.v * ax.vben     
#     
#     # 
#     # B.v   = ifelse(age.ben > r.vben, 0,   ifelse(age >= r.vben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == r.vben], 0)),  # Benefit payment after r.vben, for age.ben == r.vben
#     # B.v   = ifelse(age.ben == r.vben, B.v, ifelse(age >= age.ben, Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == age.ben], 0)), # for age.ben > r.vben
#     # ALx.v = ifelse(age <  r.vben, Bx.v[age == unique(age.term)] * ax.r.W.term[age == r.vben] * px_r.vben_m * v^(r.vben - age), # liab before receiving benefits
#     #                B.v * ax.r.W.term)
#     ) %>%                                                                                     # liab after receiving benefits      
#   ungroup %>%
#   select(ea, age, start.year, year, year.term, B.v, ALx.v) %>%
#   filter(year %in% seq(init.year, len = nyear),
#          year.term == init.year - 1)
# 
# # liab.term.init %>% filter(start.year == 2007, ea == 55, age.term == 63) %>% data.frame()


##  Calculate AL and benefit payment for vested terms terminating at different ages.
# Merge by using data.table: does not save much time, but time consumpton seems more stable than dplyr. The time consuming part is the mutate step.
liab.term <- expand.grid(# start.year   = (init.year - (r.vben - 1 - min.age)):(init.year + nyear - 1), # 2015
                         start.year   = (init.year + 1 - (retage_max - min(range_ea))):(init.year + nyear - 1),
                         ea = range_ea[range_ea < retage_max], 
                         age = range_age, 
                         age.term = range_age[range_age < retage_max]) %>% 
  filter(start.year + age_max - ea >= init.year, 
         age >= ea, age.term >= ea,
         age >= age.term) %>% # drop redundant combinations of start.year and ea.
  data.table(key = "ea,age,start.year,age.term") 



if(model_term){
liab.term <- merge(liab.term,
                   select(liab.active, start.year, year, ea, age, Bx.v, COLA.scale, px_retage_normal_m, pxm.term, ax.r) %>% data.table(key = "ea,age,start.year"),
                   all.x = TRUE, by = c("ea", "age","start.year")) %>% 
             left_join(decrement_model_ %>% select(ea, age, pxm.term)) %>% 
             as.data.frame
            

liab.term %<>% as.data.frame %>%
  group_by(start.year, ea, age.term) %>%
  mutate(year.term = year[age == age.term],
         
         COLA.scale = (1 + cola)^(age - min(age)),        # COLA.scale in liab.active does not trace back long enough
         ax.vben     = get_tla(pxm.term, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
         
         B.v   = ifelse(age >= retage_normal, 
                          Bx.v[age == unique(age.term)] * COLA.scale/COLA.scale[age == retage_normal], # Benefit payment after r.vben
                          0),  
         ALx.v = ifelse(age <  retage_normal, 
                          Bx.v[age == unique(age.term)] * ax.r[age == retage_normal] * px_retage_normal_m * v^(retage_normal - age),
                          B.v * ax.vben)

  ) %>%
  ungroup  %>%
  select(ea, age, start.year, year, year.term, B.v, ALx.v) %>%
  # select(-age.term, -Bx.v, -ax.r.W.term, -COLA.scale, -pxRm, - px_r.vben_m, -age.r, -px_r.vsuper_m, -ax.vben, -pxm.term) %>%
  filter(year %in% seq(init.year, len = nyear)) 

} else {
  liab.term %<>%
    as.data.frame %>% 
    group_by(start.year, ea, age.term) %>%
    mutate(year      = start.year + (age - ea),
           year.term = year[age == age.term],
           B.v = 0,
           ALx.v = 0) %>% 
    select(ea, age, start.year, year, year.term, B.v, ALx.v) %>%
    filter(year %in% seq(init.year, len = nyear)) 
}
# liab.term %>% head

# 
# liab.term <-  bind_rows(list(liab.term.init,                                  # Using rbind produces duplicated rows with unknown reasons. Use bind_rows from dplyr instead.
#                              filter(liab.term, year.term != init.year - 1)))


#*************************************************************************************************************
#                        4.1  ALs and NCs of disability benefit, for actives                  #####                  
#*************************************************************************************************************
# 
# # Calculate normal costs and liabilities of retirement benefits with multiple retirement ages  
# liab.active %<>%   
#   mutate( gx.disb  = ifelse(yos < 5, 0, 
#                      ifelse(yos < 17,  min(17/yos, (age_superFirst - ea)/yos),  1)
#                      ),
#           Bx.disb  = gx.disb * bfactor * yos * fas, 
#           
#           # This is the benefit level if the employee starts to CLAIM benefit at age x, not internally retire at age x. 
#           TCx.disb.la = qxd.la * v * lead(Bx.disb) *  lead(ax.disb.la) , # term cost of life annuity at the disability age x (start to claim benefit at age x + 1)
#           TCx.disb.ca = qxd.ca * v * lead(Bx.disb) *  lead(liab.disb.ca.sum.1),
#           TCx.disb.laca = TCx.disb.la + TCx.disb.ca,
#           
#           # TCx.r = Bx.r * qxr.a * ax,
#           PVFBx.disb  = c(get_PVFB(pxT[age <= r.max], v, TCx.disb.laca[age <= r.max]), rep(0, max.age - r.max)),
#           
#           ## NC and AL of UC
#           # TCx.r1 = gx.r * qxe * ax,  # term cost of $1's benefit
#           # NCx.UC = bx * c(get_NC.UC(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
#           # ALx.UC = Bx * c(get_PVFB(pxT[age <= r.max], v, TCx.r1[age <= r.max]), rep(0, 45)),
#           
#           # # NC and AL of PUC
#           # TCx.rPUC = ifelse(age == min(age), 0, (Bx / (age - min(age)) * gx.r * qxr.a * ax.r)), # Note that this is not really term cost 
#           # NCx.PUC = c(get_NC.UC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]),  rep(0, max.age - r.max)),
#           # ALx.PUC = c(get_AL.PUC(pxT[age <= r.max], v, TCx.rPUC[age <= r.max]), rep(0, max.age - r.max)),
#           
#           # NC and AL of EAN.CD
#           NCx.EAN.CD.disb = ifelse(age < r.max, PVFBx.disb[age == min(age)]/ayx[age == r.max], 0),
#           ALx.EAN.CD.disb = PVFBx.disb - NCx.EAN.CD.disb * axR,
#           
#           # NC and AL of EAN.CP
#           NCx.EAN.CP.disb   = ifelse(age < r.max, sx * PVFBx.disb[age == min(age)]/(sx[age == min(age)] * ayxs[age == r.max]), 0),
#           PVFNC.EAN.CP.disb = NCx.EAN.CP.disb * axRs,
#           ALx.EAN.CP.disb   = PVFBx.disb - PVFNC.EAN.CP.disb
#   ) 
# 
# 
# 
# #*************************************************************************************************************
# #                       4.2   ALs and benefits for disability benefit               #####                  
# #*************************************************************************************************************
# 
# liab.disb.la <- rbind(
#   # grids for initial retirees in year 1
#   # To simplified the calculation, it is assmed that all initial disabled entered the workforce at age min.age and 
#   # become disabled in year 1. This assumption will cause the age of disability and yos of some of the disabled not compatible with the eligiblility rules,
#   # but this is not an issue since the main goal is to generate the correct cashflow and liablity for the initial disabled.
#   expand.grid(age.disb   = benefit.disb_$age, # This ensures that year of retirement is year 1.
#               age        = range_age) %>%
#     mutate(ea            = unique(benefit.disb_$ea),
#            start.year    = init.year - (age.disb - ea)) %>% 
#     filter(age >= age.disb),
#   
#   # grids for who die after year 1.
#   expand.grid(ea           = range_ea[range_ea < r.max],
#               age.disb     = min.age:r.max,
#               start.year   = (init.year + 1 - (r.max - min(range_ea))):(init.year + nyear - 1),
#               age          = range_age) %>%
#     filter(age   >= ea,
#            age.disb >= ea,
#            age   >= age.disb,
#            start.year + (age.disb - ea) >= init.year + 1, # retire after year 2, LHS is the year of retirement
#            start.year + age - ea >= init.year + 1) # not really necessary since we already have age >= age.r
# ) %>%
#   data.table(key = "start.year,ea,age.disb,age")
# 
# 
# 
# 
# liab.disb.la <- liab.disb.la[!duplicated(liab.disb.la %>% select(start.year, ea, age, age.disb ))]
# 
# 
# liab.disb.la <- merge(liab.disb.la,
#                     select(liab.active, start.year, ea, age, Bx.disb, COLA.scale, gx.disb, ax.disb.la #pxm.d
#                            ) %>% data.table(key = "ea,age,start.year"),
#                     all.x = TRUE, 
#                     by = c("ea", "age","start.year")) %>%
#   arrange(start.year, ea, age.disb) %>% 
#   as.data.frame %>% 
#   left_join(benefit.disb_)
# 
# 
# liab.disb.la %<>% as.data.frame  %>% 
#   left_join(decrement.model_ %>% select(ea, age, pxm.d)) %>%  # getting pxm.d from liab.active causes problem
#   group_by(start.year, ea, age.disb) %>%
#   mutate(
#     year       = start.year + age - ea,
#     COLA.scale = (1 + cola)^(age - min(age)),    # COLA.scale in liab.active does not trace back long enough
#     ax.disb.la = get_tla(pxm.d, i, COLA.scale),  # COLA.scale in liab.active does not trace back long enough
#     year.disb  = start.year + age.disb - ea,     # year of disability of the active
#     Bx.disb    = ifelse(is.na(Bx.disb), 0, Bx.disb),  # just for safety
#     B.disb.la     = ifelse(year.disb <= init.year,
#                            benefit.disb[year == init.year] * COLA.scale / COLA.scale[year == init.year],  # Benefits for initial retirees
#                            Bx.disb[age == age.disb] * COLA.scale / COLA.scale[age == age.disb]),          # Benefits for disability retirees after year 1
#     ALx.disb.la   = B.disb.la * ax.disb.la                                                                # Liability for remaining diability benefits, PV of all future benefit adjusted with COLA
#     
#   ) %>% ungroup %>%
#   # select(start.year, year, ea, age, year.retire, age.retire,  B.r, ALx.r)# , ax, Bx, COLA.scale, gx.r)
#   filter(year %in% seq(init.year, len = nyear) ) %>%
#   select(year, ea, age, year.disb, age.disb, start.year, B.disb.la, ALx.disb.la, pxm.d) 
#   #%>%   arrange(age.disb, start.year, ea, age)
# 


#*************************************************************************************************************
#                 # 5.  Choosing AL and NC variables corresponding to the chosen acturial methed             #####
#*************************************************************************************************************

liab.active %<>% ungroup %>% select(start.year, year, ea, age, everything())

ALx.la.method   <- paste0("ALx.", actuarial_method, ".la")
NCx.la.method   <- paste0("NCx.", actuarial_method, ".la")

ALx.v.method <- paste0("ALx.", actuarial_method, ".v")
NCx.v.method <- paste0("NCx.", actuarial_method, ".v")


var.names <- c("sx", ALx.la.method, NCx.la.method, 
                     ALx.v.method, NCx.v.method, 
                    #  ALx.disb.method, NCx.disb.method,
                     "PVFBx.la", "PVFBx.v", "Bx.la", "EEC")  # "PVFBx.disb","Bx.disb"
liab.active %<>% 
  filter(year %in% seq(init.year, len = nyear)) %>%
  select(year, ea, age, one_of(var.names)) %>%
  rename_("ALx.la"   = ALx.la.method,  "NCx.la"   = NCx.la.method, 
          "ALx.v"    = ALx.v.method,   "NCx.v"    = NCx.v.method
          # "ALx.disb"   = ALx.disb.method,  "NCx.disb"   = NCx.disb.method
           )   # Note that dplyr::rename_ is used. 



## Final outputs
  # liab.active
  # liab.la
  # liab.term


liab <- list(active = liab.active, 
             la = liab.la, 
             term = liab.term
             #disb.la = liab.disb.la
             )

}

