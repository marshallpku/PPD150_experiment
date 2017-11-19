
# Inputs:
  # "Data_inputs/LAFPP_PlanInfo.RData", for decrements
  # "Data_inputs/LAFPP_MemberData.RData", for gender and occupation ratios


# Final outputs
 # 1. decrement.model
 # 2. mortality.post.model


get_decrements <- function(paramlist  = planData_list$inputs_singleValues,
                           decrements = planData_list$decrements){

  
# paramlist  <-  planData_list$inputs_singleValues
# decrements <-  planData_list$decrements
  
assign_parmsList(paramlist , envir = environment())
range_age <- age_min:age_max
range_ea  <- ea_min:ea_max


#*************************************************************************************************************
#                      1. Standardize format again (just for safety)  ####
#*************************************************************************************************************

decrement.model <- expand.grid(age = range_age, ea = range_ea) %>% 
  filter(age >= ea) %>% 
  left_join(decrements) %>% 
  mutate(ppd_id = ppd_id,
         # planName = planName,
         yos = age - ea) %>% 
  colwise(na2zero)(.) %>% 
  select(ppd_id, ea, age, yos, everything())
  
# decrement.model

#*************************************************************************************************************
#                      2. Adjustments to decrement tables  ####
#*************************************************************************************************************

# Adjustment to term rates
# 1. qxm = 1 at max age
# 2. Coerce termination rates to 0 when 
  # vested and eligible for early retirement or 
  # eligible for full retirement no matter vested or not. 


decrement.model %<>% 
  group_by(ea) %>% 
  mutate(qxm.pre  = ifelse(age ==  age_max, 1, qxm.pre),
         qxm.post = ifelse(age ==  age_max, 1, qxm.post),
         qxm.d    = ifelse(age ==  age_max, 1, qxm.d),
         qxm.term = ifelse(age ==  age_max, 1, qxm.term))



decrement.model %<>% 
  mutate(qxt = ifelse( (yos >= vest_yos & age >= retage_early)|(age >= retage_normal), 0, qxt)
)
  
# Adjustment to retirement rates
decrement.model %<>% 
  mutate(qxr = ifelse( (yos >= ret_yos & age >= retage_early)|(age >= retage_normal), qxr, 0)
  )


#*************************************************************************************************************
#                       3. Modify retirement rates ####
#*************************************************************************************************************

# Adjustment to the decrement table:
  # Move qxr.a backward by 1 period.(qxr at t is now assigned to t - 1), the probability of retirement at t - 1 is lead(qxr.a(t))*(1 - qxt.a(t-1) - qxm.a(t-1) - qxd.a(t-1))
  # For the age right before the max retirement age (r.max - 1), probability of retirement is 1 - qxm.a - qxd.a - qxt.a,
  # which means all active members who survive all other risks at (r.max - 1) will enter the status "retired" for sure at age r.max (and collect the benefit regardless 
  # whether they will die at r.max)      



decrement.model %<>% group_by(ea) %>%  
  mutate(qxr = ifelse(age == retage_max - 1,
                             1 - qxt - qxm.pre - qxd, 
                             lead(qxr) * (1 - qxt - qxm.pre - qxd)), # Total probability of retirement
         
         qxr = ifelse(age >= retage_max, 0, qxr)
)   
         

decrement.model %<>% group_by(ea) %>% 
  mutate(qxm.term = ifelse(age<= 74, qxm.pre, qxm.post))




#*************************************************************************************************************
#                                            5. Compute various survival rates ####
#*************************************************************************************************************

decrement.model %<>% 
  group_by(ea) %>% 
  mutate( pxm.pre  = 1 - qxm.pre,
          pxm.post = 1 - qxm.post,
          pxm.d    = 1 - qxm.d,
          pxm.term = 1 - qxm.term,
          
          pxT     = 1 - qxt - qxd - qxm.pre - qxr,                            
          
          pxRm        = order_by(-age, cumprod(ifelse(age >= retage_max, 1,    pxm.pre))),       # prob of surviving up to retage_max, mortality only
          px_retage_normal_m = order_by(-age, cumprod(ifelse(age >= retage_normal, 1, pxm.pre))) # prob of surviving up to retage_normal, mortality only

          # px65T = order_by(-age, cumprod(ifelse(age >= r.max, 1, pxT))), # prob of surviving up to r.max, composite rate
          # p65xm = cumprod(ifelse(age <= r.max, 1, lag(pxm))))            # prob of surviving to x from r.max, mortality only
  ) %>% 
  mutate_all(funs(na2zero))


list(decrement.model = decrement.model)
}







