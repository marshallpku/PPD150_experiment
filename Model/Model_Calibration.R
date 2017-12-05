# Calibrate liabilities and benefit flows from the model to match the actual values


get_calibAggLiab <- function( AggLiab_ = AggLiab, 
                              paramlist = planData_list$inputs_singleValues,
                              planData_list_ = planData_list){
  
  
  # Run the section below when developing new features.  
  # AggLiab_ = AggLiab
  # paramlist = planData_list$inputs_singleValues
  
  assign_parmsList(paramlist, envir = environment())
  
  
  
  #*************************************************************************************************************
  #                                     ## Calibrating liabilities for current actives   ####
  #************************************************************************************************************* 
  
  # Target: reported/estimated value of AL for current actives in year 1
  # Method: 
    # 1. calculate the calib factor: calib_factor = AL_act_actual / AL_act_model
    # 2. multiply AL_act, NC_act, PVFB_act by the calib factor
  
  target_AL_actives    <- planData_list$inputs_singleValues$AL_active
  calib_factor_actives <- target_AL_actives / AggLiab_$active.current[1, "ALx.av.sum"]
  
  AggLiab_actives.current <- 
    AggLiab_$active.current %>% 
    as.data.frame %>% 
    mutate_at(vars(-year, -PR.sum, -EEC.sum, -nactives), funs(.*calib_factor_actives)) 
  
  AggLiab_la.current.new <- 
    AggLiab_$la.current.new %>% 
    as.data.frame %>% 
    mutate_at(vars(-year, -nla), funs(.*calib_factor_actives)) 

    
  #*************************************************************************************************************
  #                                     ## Calibrating liabilities for current actives   ####
  #************************************************************************************************************* 
  # Use the same calibrating factor to calibrate AL for new entrants
  
  
  AggLiab_actives.entrants <- 
  AggLiab_$active.entrants %>% 
    as.data.frame %>% 
    mutate_at(vars(-year, -PR.sum, -EEC.sum, -nactives), funs(.*calib_factor_actives)) 
  
  
  AggLiab_la.entrants <- 
    AggLiab_$la.entrants %>% 
    as.data.frame %>% 
    mutate_at(vars(-year, -nla), funs(.*calib_factor_actives)) 
  
  
  
  
  #*************************************************************************************************************
  #                                     ## Calibrating liabilities for current retirees   ####
  #************************************************************************************************************* 
  
  # Target: 
  #  1. Total benefit payment in year 1
  #  2. reported/estiamted value of Al for current retires in year 1
  #
  # Method:
  #  1. calibrate the year 1 benefit: calib_factor_benY1 = benY1_actual/benY1_model; get calibrated year-1 benefit
  #  2. calibrate the benefit cash flow using a polynomial (1+g)^i, i = 0, 1, ..., (nyear - 1), such that the PV of the benefit cash flow is 
  #     equal to the reported/estimated AL for current retirees in year 1. 
  
  target_benY1       <- planData_list$inputs_singleValues$B
  target_AL_retirees <- planData_list$inputs_singleValues$AL_retired
  
  calib_factor_benY1 <- target_benY1 / AggLiab_$la.current.init[1, "B.la.sum"]
  
  AggLiab_la.current.init <- 
    AggLiab_$la.current.init %>% 
    as.data.frame %>% 
    mutate(B.la.sum = B.la.sum * calib_factor_benY1)

  
  AL_model           <- AggLiab_$la.current.init[1, "ALx.la.sum"]
  B_series.original  <- AggLiab_$la.current.init[, "B.la.sum"]

  AL_sub  <- sum(B_series.original*((1+i)^(-(0:(length(B_series.original)-1)))))
  diff_AL <- AL_model - AL_sub 
  
  B_series_calib1 <- c(B_series.original, diff_AL*(1+i)^(-length(B_series.original))) * calib_factor_benY1
  # B_series
  
  
  fun_objective <- function(g){
    (target_AL_retirees - sum(B_series_calib1 *1/((1+i)^(0:(length(B_series_calib1 )-1))) * g^(0:(length(B_series_calib1 )-1))))^2
  }
  
  
  optim_ub <- ifelse(ppd_id %in% c(19, 136, 150), 1.5, 10)
  optim_lb <- ifelse(ppd_id %in% c(19, 136, 150), 0.5, -10)
  
  # optim_ub <- 10
  # optim_lb <- -10
  
  g_out <- optimize(fun_objective, c(optim_lb, optim_ub))
  g <- g_out$minimum
  
  B_series_calib2 <- B_series_calib1 * g^(0:(length(B_series_calib1)-1))
  AL_Y1 <- sum(B_series_calib2*1/((1+i)^(0:(length(B_series_calib2)-1))))
  
  
  Err.AL_al.current.init <- AL_Y1 /  target_AL_retirees - 1
  Err.AL_al.current.init
  
  AggLiab_la.current.init$B.la.sum <- B_series_calib2[-length(B_series_calib2)] 
  AggLiab_la.current.init$ALx.la.sum[1] <- AL_Y1
  
  for(j in 2:nrow(AggLiab_la.current.init)){
    AggLiab_la.current.init$ALx.la.sum[j] <- 
      (AggLiab_la.current.init$ALx.la.sum[j - 1] - AggLiab_la.current.init$B.la.sum[j - 1]) * (1 + i)
  }

  
  
  #*************************************************************************************************************
  #                                     ## Calibrating liabilities for current retirees   ####
  #************************************************************************************************************* 
  
  AggLiab_$active.current.calib         <- AggLiab_actives.current  %>% as.matrix
  AggLiab_$active.la.carrent.new.calib  <- AggLiab_la.current.new   %>% as.matrix
  AggLiab_$active.la.carrent.init.calib <- AggLiab_la.current.init  %>% as.matrix
  AggLiab_$active.entrants.calib        <- AggLiab_actives.entrants %>% as.matrix
  AggLiab_$la.entrants.calib            <- AggLiab_la.entrants      %>% as.matrix
  
  
  AggLiab_$active.calib <- 
  bind_rows(AggLiab_actives.current,  
            AggLiab_actives.entrants) %>% 
    group_by(year) %>% 
    summarise_all(funs(sum(., na.rm = TRUE))) %>% 
    as.matrix

  AggLiab_$la.calib <- 
    bind_rows(AggLiab_la.current.new ,
              AggLiab_la.current.init,
              AggLiab_la.entrants ) %>% 
    group_by(year) %>% 
    summarise_all(funs(sum(., na.rm = TRUE))) %>% 
    as.matrix

  AggLiab_$calib_factor_actives <- calib_factor_actives
  AggLiab_$calib_factor_benY1   <- calib_factor_benY1
  AggLiab_$calib_g              <- g
  AggLiab_$calib_errorAL        <- Err.AL_al.current.init
  
  AggLiab_$planData_list <- planData_list_
  
  return(AggLiab_)

}