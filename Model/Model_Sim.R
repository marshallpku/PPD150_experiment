# This script conducts the simulation of the finance of UCRP


run_sim <- function(AggLiab_ = AggLiab,
                    i.r_ = i.r,
                    init_amort_raw_ = planData_list$init_amort_unadj, 
                    init_unrecReturns.unadj_ = planData_list$init_unrecReturns_unadj,
                    paramlist = planData_list$inputs_singleValues){

  # Run the section below when developing new features.
     # i.r_            = i.r
     # AggLiab_        = AggLiab
     # init_amort_raw_ = planData_list$init_amort_unadj
     # init_unrecReturns.unadj_ = planData_list$init_unrecReturns_unadj
     # paramlist      = planData_list$inputs_singleValues

     assign_parmsList(paramlist, envir = environment())

  # Variables to be added:
     useAVamort <- TRUE
     ConPolicy  <- "ADC"
     smooth_method <- "method1"
     nonNegC <- TRUE
     EEC_fixed <- TRUE

  # use calibrated values   
     AggLiab_$active <- AggLiab_$active.calib
     AggLiab_$la     <- AggLiab_$la.calib
     
  
  #*************************************************************************************************************
  #                                     Defining variables in simulation ####
  #*************************************************************************************************************  
  
  # Now we do the actuarial valuations 
  # In each period, following values will be caculated:
  # AL: Total Actuarial liability, which includes liabilities for active workers and pensioners.
  # NC: Normal Cost  
  # MA: Market value of assets.
  # AA: Actuarial value of assets.
  # EAA:Expected actuarial value of assets.
  # UAAL: Unfunded accrued actuarial liability, defined as AL - NC
  # EUAAL:Expected UAAL.
  # PR: payroll 
  # LG: Loss/Gain, total loss(positive) or gain(negative), Caculated as LG(t+1) = (UAAL(t) + NC(t))(1+i) - C - Ic - UAAL(t+1), 
  # AM: Amount to be amortized at period t. 
  # i is assumed interest rate. ELs of each period will be amortized seperately.  
  # SC: Supplement cost 
  # ADC: actuarially required contribution by employer. NC + SC - EEC
  # C : Actual contribution
  # C_ADC: shortfall in paying ADC
  # B : Total beneift Payment   
  # Ic: Assumed interest from contribution, equal to i*C if C is made at the beginning of time period. i.r is real rate of return. 
  # Ia: Assumed interest from AA, equal to i*AA if the entire asset is investible. 
  # Ib: Assumed interest loss due to benefit payment, equal to i*B if the payment is made at the beginning of period
  # I.r : Total ACTUAL interet gain, I = i.r*(AA + C - B), if AA is all investible, C and B are made at the beginning of period.
  # Funded Ratio: AA / AL
  # C_PR: contribution as % of payroll
  
  # Formulas
  # AL(t), NC(t), B(t) at each period are calculated using the workforce matrix and the liability matrix.
  # MA(t+1) = AA(t) + I(t) + C(t) - B(t), AA(1) is given
  # EAA(t+1)= AA(t) + EI(t)
  # AA(t+1) = (1-w)*EAA(t+1) + w*MA(t+1)
  # I.r(t) = i.r(t)*[AA(t) + C(t) - B(t)]
  # Ia(t) = i * AA(t)
  # Ib(t) = i * B(t)
  # Ic(t) = i * C(t)
  # EI(t) = Ia(t) - Ib(t) + Ic(t)
  # ADC   = NC(t) + SC(t)
  # ADC.ER = NC(t) + SC(t) - EEC(t)
  # C(t) = NC(t) + SC(t)
  # UAAL(t) = AL(t) - AA(t)
  # EUAAL(t) = [UAAL(t-1) + NC(t-1)](1+i(t-1)) - C(t-1) - Ic(t-1)
  # LG(t) =   UAAL(t) - EUAAL for t>=2 ; LG(1) = -UAAL(1) (LG(1) may be incorrect, need to check)
  # More on LG(t): When LG(t) is calculated, the value will be amortized thourgh m years. This stream of amortized values(a m vector) will be 
  # placed in SC_amort[t, t + m - 1]
  # SC = sum(SC_amort[,t])
  # ExF = B(j) - C(j)
  
  # About gains and losses
  # In this program, the only source of gain or loss is the difference between assumed interest rate i and real rate of return i.r,
  # which will make I(t) != Ia(t) + Ic(t) - Ib(t)
  
  
  # Set up data frame
  penSim0 <- data.frame(year = init.year:(init.year + nyear - 1)) %>%
    mutate(AL   = 0, #
           MA   = 0, #
           AA   = 0, #
           EAA  = 0, #
           FR   = 0, #
           ExF  = 0, # 
           UAAL = 0, #
           EUAAL= 0, #
           LG   = 0, #
           Amort_basis  = 0, # amount to be amortized: AM(t) = LG(t) + [ADC(t - 1) - C(t-1)]*[1 + i(t-1)], i.e. actuarial loss/gain plus shortfall in paying NC+SC in last period(plus interests) 
           # Switch_amort = 0, 
           NC   = 0, #
           SC   = 0, #
           EEC  = 0, #
           ERC  = 0, #
           ADC  = 0, #
           ADC.ER = 0, #
           C    = 0, #
           C_ADC= 0, #
           B    = 0, #                        
           I.r  = 0, #                        
           I.e  = 0, #
           I.dif= 0,
           Ia   = 0, #                         
           Ib   = 0, #                         
           Ic   = 0, #  
           i    = i,
           i.r  = 0,
           PR   = 0,
           ADC_PR = 0,
           C_PR = 0,
           nactives  = 0,
           nretirees = 0,
           nterms    = 0)
  # penSim0 <- as.list(penSim0)
  
  
  # Vector used in asset smoothing
  s.vector <- seq(0,1,length = asset_years + 1)[-(asset_years+1)]; s.vector  # a vector containing the porportion of 
  
  #*************************************************************************************************************
  #                                 Defining variables in simulation  ####
  #*************************************************************************************************************
  
  
  # AL(j)
  penSim0$AL.act.la   <- AggLiab_$active[, "ALx.la.sum"]
  penSim0$AL.act.v    <- AggLiab_$active[, "ALx.v.sum"]
  # penSim0$AL.act.disb <- AggLiab_$active[, "ALx.disb.sum"]
  penSim0$AL.act      <-  with(penSim0, AL.act.la + AL.act.v) # + penSim0$AL.act.disb)
  
  penSim0$AL.la    <- AggLiab_$la[,   "ALx.la.sum"]
  penSim0$AL.term  <- AggLiab_$term[, "ALx.v.sum"]
  #penSim0$AL.disb.la  <- AggLiab_$disb.la[, "ALx.disb.la.sum"]
  
  penSim0$AL       <- with(penSim0, AL.act + AL.la +  AL.term) # + AL.disb.la + AL.disb.ca)
  
  
  # NC(j)
  penSim0$NC.la   <- AggLiab_$active[, "NCx.la.sum"]
  penSim0$NC.v    <- AggLiab_$active[, "NCx.v.sum"]
  # penSim0$NC.disb <- AggLiab_$active[, "NCx.disb.sum"] 
  penSim0$NC      <-  with(penSim0, NC.la + NC.v) #  + NC.disb)
  
  
  # PVFB(j)
  penSim0$PVFB.la   <- AggLiab_$active[, "PVFBx.la.sum"]
  penSim0$PVFB.v    <- AggLiab_$active[, "PVFBx.v.sum"]
  # penSim0$PVFB.disb <- AggLiab_$active[, "PVFBx.disb.sum"] 
  penSim0$PVFB      <-  with(penSim0, PVFB.la + PVFB.v) # + PVFB.disb) #Note this is the total PVFB for actives. PVFB for retirees/beneficiaries are the same as AL.
  
  # B(j)
  penSim0$B.la    <- AggLiab_$la[, "B.la.sum"]
  penSim0$B.v     <- AggLiab_$term[, "B.v.sum"]
  #penSim0$B.disb.la  <- AggLiab_$disb.la[, "B.disb.la.sum"]
  penSim0$B       <- with(penSim0, B.la + B.v) # + B.disb.la)
  
  # PR(j)
  penSim0$PR <- AggLiab_$active[, "PR.sum"]
  
  # EEC(j)
  penSim0$EEC <- AggLiab_$active[, "EEC.sum"]

  
  # nactives, nretirees, nterms
  penSim0$nactives  <- AggLiab_$active[,  "nactives"]
  penSim0$nla       <- AggLiab_$la[, "nla"]
  penSim0$nterms    <- AggLiab_$term[, "nterms"]
  #penSim0$ndisb.la  <- AggLiab_$disb.la[,  "ndisb.la"]
  
  
  penSim0 <- as.list(penSim0) # Faster to extract elements from lists than frame data frames.
  
  
  #*************************************************************************************************************
  #                                  Setting up initial amortization payments ####
  #*************************************************************************************************************  
  
  # matrix representation of amortization: better visualization but larger size
  amort_year_max <- max(init_amort_raw_$year.remaining, amort_year)
  SC_amort0      <- matrix(0, nyear +  amort_year_max, nyear + amort_year_max)
  # SC_amort0
  
  # data frame representation of amortization: much smaller size, can be used in real model later.
  # SC_amort <- expand.grid(year = 1:(nyear + m), start = 1:(nyear + m))
  
  # Amortization payment amounts for all prior years. 
  SC_amort.init <- matrix(0, nrow(init_amort_raw_), nyear + amort_year_max )
  
  
  # Adjustment factor for initial amortization payments (PSERS specific)
      # Factor is defined as the initial model UAAL as a proportion of UAAL in AV2015.
      # WARNING: Does not work with "method 2" for AA. 
   
  init_MA  <- "AL_pct"
  MA_0_pct <- FR_MAV  
  init_AA  <- "AL_pct"
  AA_0_pct <- FR_AAV 
  useAVamort <- TRUE
  
   MA.year1 <- switch(init_MA, 
                        MA0 = MA_0,                        # Use preset value
                        AL = penSim0$AL[1],                # Assume inital fund equals inital liability.
                        AL_pct = penSim0$AL[1] * MA_0_pct) # Inital MA is a proportion of inital AL
   
   AA.year1  <- ifelse(init_AA == "AL_pct",         penSim0$AL[1] * AA_0_pct, # Initial AA as a % of initial AL
                           ifelse(init_AA == "AA0", AA_0,                     # preset value of AA
                                                    with(penSim0, MA.year1))  # # Assume inital AA equals inital liability.
   )
                                  
   AL.year1 <- penSim0$AL[1]
   UAAL.year1 <- AL.year1 - AA.year1
   
   factor.initAmort <- UAAL.year1/sum(init_amort_raw_$balance)  

   init_amort_raw_ %<>% 
     mutate(balance = balance * factor.initAmort)
   
   if(useAVamort){
     SC_amort.init.list <- mapply(amort_LG, p = init_amort_raw_$balance , m = init_amort_raw_$year.remaining, method = amort_pctdol,
                                 MoreArgs = list(i = i, g = salgrowth_amort, end = FALSE), SIMPLIFY = F)
     
     for(j in 1:nrow(SC_amort.init)){
       SC_amort.init[j, 1:init_amort_raw_$year.remaining[j]] <- SC_amort.init.list[[j]]
     }
   }
  #SC_amort.init
   
  nrow.initAmort <- nrow(SC_amort.init)
  SC_amort0 <- rbind(SC_amort.init, SC_amort0)
  # # The amortization basis of year j should be placed in row nrow.initAmort + j - 1. 
  # # save(SC_amort0, file = "SC_amort0.RData")  
  
  
  #*************************************************************************************************************
  #                                       Simuation  ####
  #*************************************************************************************************************
    
  cl <- makeCluster(ncore) 
  registerDoParallel(cl)
  
  
  penSim_results <- foreach(k = -1:nsim, .packages = c("dplyr", "tidyr")) %dopar% {
    #k <- 1
    # initialize
    penSim   <- penSim0
    SC_amort <- SC_amort0
    
    if(k == -1) SC_amort[,] <- 0
    
    penSim[["i.r"]] <- i.r_[, as.character(k)]
    
    source("Functions.R")
    
    for (j in 1:nyear){
        
        # j <- 1
        # j <- 60

      # MA(j) and EAA(j) 
      if(j == 1) {penSim$MA[j]  <- ifelse(k == -1, penSim$AL[j],                   # k = -1 is for testing model consistency
                                          switch(init_MA, 
                                                 MA0 = MA_0,                        # Use preset value
                                                 AL  = penSim$AL[j],                # Assume inital fund equals inital liability.
                                                 AL_pct = penSim$AL[j] * MA_0_pct) # Inital MA is a proportion of inital AL
                                          ) 
                 penSim$EAA[j] <- penSim$MA[j]
                                  # switch(init_EAA,
                                  #        AL = EAA_0,                       # Use preset value 
                                  #        MA = penSim$MA[j])                # Assume inital EAA equals inital market value.
                 
                 penSim$AA[j]  <- ifelse(init_AA == "AL_pct" & k != -1, penSim$AL[j] * AA_0_pct, 
                                             ifelse(init_AA == "AA0" & k != -1, AA_0,
                                                    switch(smooth_method,
                                                           method1 =  with(penSim, MA[j]),   # we may want to allow for a preset initial AA.
                                                           method2 =  with(penSim, (1 - w) * EAA[j] + w * MA[j])
                                                    ) 
                                          )
      )
      } else {
        penSim$MA[j]  <- with(penSim, MA[j - 1] + I.r[j - 1] + C[j - 1] - B[j - 1])
        penSim$EAA[j] <- with(penSim, AA[j - 1] + I.e[j - 1] + C[j - 1] - B[j - 1])
        penSim$AA[j]  <- switch(smooth_method,
                                method1 = with(penSim, MA[j] - sum(s.vector[max(asset_years + 2 - j, 1):asset_years] * I.dif[(j-min(j, asset_years + 1)+1):(j-1)])),
                                method2 = with(penSim, (1 - w) * EAA[j] + w * MA[j]) 
        )
      }
      
      
      ## Initial unrecognized returns
      if((init_AA %in% c("AL_pct", "AA0")) & k != -1){

        # Adjusting initila unrecognized returns
        init_unrecReturns.adj <-  mutate(init_unrecReturns.unadj_, DeferredReturn = DeferredReturn * (penSim$MA[1] - penSim$AA[1])/sum(DeferredReturn),
                                                                   DeferredReturn.annualTot = sum(DeferredReturn) - cumsum(DeferredReturn) # Initial unrecognized return to be subtracted from AA in each year
                                         )

        # Adjust AA for inital unrecognized returns
        if((j - 1 + init.year) %in% init_unrecReturns.adj$year) penSim$AA[j] <- penSim$AA[j] - with(init_unrecReturns.adj, DeferredReturn.annualTot[year == (j - 1 + init.year)])
            

      }
        
      
      ## Apply corridor for MA, MA must not deviate from AA by more than 40%. 
      
      #penSim$AA[j] <- with(penSim, ifelse(AA[j] > s.upper * MA[j], MA[j], AA[j])) 
      #penSim$AA[j] <- with(penSim, ifelse(AA[j] < s.lower * MA[j], MA[j], AA[j]))
    

      # UAAL(j)
      penSim$UAAL[j]    <- with(penSim, AL[j] - AA[j])
      # penSim$UAAL.MA[j] <- with(penSim, AL[j] - MA[j])
      
      
      # LG(j)
      # Note that what is amortized at time t is the sum of 1) actuarial loss/gain(LG) during t -1, and 2) shortfall in paying ADC(C_ADC) at (t-1)
      if (j == 1){
        penSim$EUAAL[j] <- 0
        penSim$LG[j] <- with(penSim,  UAAL[j])        # This is the intial underfunding, rather than actuarial loss/gain if the plan is established at period 1. 
        penSim$Amort_basis[j] <- with(penSim, LG[j])  # This will not be used for LAFPP since the amortization scheme for year 1 is provided by SC_amort.(from AV2016)
        
      } else {
        penSim$EUAAL[j] <- with(penSim, (UAAL[j - 1] + NC[j - 1])*(1 + i[j - 1]) - C[j - 1] - Ic[j - 1])
        penSim$LG[j]    <- with(penSim,  UAAL[j] - EUAAL[j])
        penSim$Amort_basis[j]    <- with(penSim,  LG[j] - (C_ADC[j - 1]) * (1 + i[j - 1]))
      }   
      
      # # Amortize LG(j)
    
      #if(j > 1){
      if(j > ifelse(useAVamort, 1, 0)){
        # if useAVamort is TRUE, AV amort will be used for j = 1, not the one calcuated from the model. This may cause inconsistency in the model results
        if(amort_openclosed == "closed") SC_amort[nrow.initAmort + j - 1, j:(j + amort_year - 1)] <- amort_LG(penSim$Amort_basis[j], i, amort_year, salgrowth_amort, end = FALSE, method = amort_pctdol)
        }
      #}
      
      
      # Supplemental cost in j
      penSim$SC[j] <- switch(amort_openclosed,
                             closed = sum(SC_amort[, j]),
                             open   = amort_LG(penSim$UAAL[j], i, amort_year, salgrowth_amort, end = FALSE, method = amort_method)[1])
      
      
      # penSim$EEC[j] <- 

      # ADC(j)
      
      if(nonNegC){
        penSim$ADC[j]    <- with(penSim, max(0, NC[j] + SC[j])) 
        penSim$ADC.ER[j] <- with(penSim, ifelse(ADC[j] > EEC[j], ADC[j] - EEC[j], 0)) 
        
        # Adjustment of EEC
        if(!EEC_fixed) penSim$EEC[j] <- with(penSim, ifelse(ADC[j] > EEC[j], EEC[j], ADC[j])) # penSim$EEC[j] <- with(penSim, EEC[j]) else
        
      } else {
        # Allow for negative ADC and C  
        penSim$ADC[j]    <- with(penSim, NC[j] + SC[j]) 
        
        if(EEC_fixed) {penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) # EEC is fixed
        # EEC is not fixed
          # 1. when ADC > EEC. Employees pay fixed EEC and employer pays the rest
        } else if(with(penSim, ADC[j] > EEC[j])) {
          penSim$ADC.ER[j] <- with(penSim, ADC[j] - EEC[j]) 
          # 2. when 0 < ADC < EEC. Employees pay the entire ADC and employer pays 0. 
        } else if(with(penSim, ADC[j] <= EEC[j] & ADC[j] > 0)) {
          penSim$ADC.ER[j] <- 0
          penSim$EEC[j]    <- with(penSim, ADC[j])
          # 3. when ADC < 0, employees pay zero and employer pays nagative value (withdraw -ADC)
        } else if(with(penSim, ADC[j] <= 0)) {
          penSim$ADC.ER[j] <- with(penSim, ADC[j])
          penSim$EEC[j]    <- 0
        }
        
      }
      
      
      # ERC
      penSim$ERC[j] <- switch(ConPolicy,
                              ADC     = with(penSim, ADC.ER[j]),                          # Full ADC
                              ADC_cap = with(penSim, min(ADC.ER[j], PR_pct_cap * PR[j])), # ADC with cap. Cap is a percent of payroll 
                              Fixed   = with(penSim, PR_pct_fixed * PR[j])                # Fixed percent of payroll
      ) 
    
      
      # C(j)
      penSim$C[j] <- with(penSim, EEC[j] + ERC[j])
      
      
      # C(j) - ADC(j)
      penSim$C_ADC[j] <- with(penSim, C[j] - ADC[j])
      
      # Ia(j), Ib(j), Ic(j)
      penSim$Ia[j] <- with(penSim,  MA[j] * i[j])
      penSim$Ib[j] <- with(penSim,  B[j] * i[j])
      penSim$Ic[j] <- with(penSim,  C[j] * i[j])
      
      
      # I.e(j)
      # penSim$I.e[j] <- with(penSim, Ia[j] + Ic[j] - Ib[j])
      penSim$I.e[j] <- with(penSim, i[j] *(MA[j] + C[j] - B[j]))
      
      # I.r(j)
      penSim$I.r[j] <- with(penSim, i.r[j] *( MA[j] + C[j] - B[j])) # C[j] should be multiplied by i.r if assuming contribution is made at year end. 
      
      # I.dif(j) = I.r(j) - I.e(j):  used in asset smoothing 
      penSim$I.dif[j] <- with(penSim, I.r[j] - I.e[j])
      
    }
    
    # penSim_results[[k]] <- penSim
    as.data.frame(penSim)
  }
  
  stopCluster(cl)
  
  
  
  
  #*************************************************************************************************************
  #                                  Combining results into a data frame.   ####
  #*************************************************************************************************************
  
  
  penSim_results <- bind_rows(penSim_results) %>% 
    mutate(ppd_id  = ppd_id,
           ppd_planname = ppd_planname,
           sim     = rep(-1:nsim, each = nyear),
           FR      = 100 * AA / exp(log(AL)),
           FR_MA   = 100 * MA / exp(log(AL)),
           UAAL_PR = 100 * UAAL / PR,
           MA_PR   = 100 * MA / PR,
           AA_PR   = 100 * AA / PR,
           AL_PR   = 100 * AL / PR,
           AL.act_PR    = 100 * AL.act / PR,
           AL.la_PR    = 100 * AL.la / PR, 
           AL.term_PR   = 100 * AL.term / PR, 
           ADC_PR  = 100 * ADC / PR,
           NC_PR   = 100 * NC / PR,
           NC.la_PR    = 100 * NC.la / PR,
           NC.v_PR   = 100 * NC.v / PR,
           SC_PR   = 100 * SC / PR, 
           ERC_PR  = 100 * ERC / PR,
           EEC_PR  = 100 * EEC / PR, 
           C_PR    = 100 * C / PR,
           B_PR    = 100 * B / PR,
           ExF     = C - B,
           ExF_PR  = 100 * ExF / PR,
           ExF_MA  = 100 * ExF / MA, 
           PR.growth = ifelse(year > 1, 100 * (PR / lag(PR) - 1), NA)
           
           ) %>%
    select(ppd_id, ppd_planname, sim, year, everything())
  
  return(penSim_results)
  
}


# 
 # start_time_loop <- proc.time()
 # 
 # penSim_results <- run_sim()
 # 
 # end_time_loop <- proc.time()
 # Time_loop <- end_time_loop - start_time_loop 
 # Time_loop


