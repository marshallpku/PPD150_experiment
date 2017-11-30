# This module create investment return series. 

gen_returns <- function( paramlist = planData_list$inputs_singleValues,
                         returnPar = returnScn_sim
                         #returnScenarios_ = returnScenarios
                         ){
   
  # paramlist =  planData_list$inputs_singleValues
  # returnPar = returnScn_sim
  
  assign_parmsList(paramlist, envir = environment())
  
  # set parameters
  return_planAssumption <- paramlist$i 
  returnPar %<>%
    mutate(r.geoMean = ifelse(r.geoMean_type == "userInput", r.geoMean, return_planAssumption),
           r.sd      = ifelse(r.sd_type      == "userInput",  r.sd, (2*SharpeRatio - (4*SharpeRatio^2 - 8 * (r.geoMean - riskFree_rate))^0.5 )/2),
           r.arithMean = r.geoMean + r.sd^2/2)
  
  # generate returns
  set.seed(1234)
  i.r <- cbind(
    rep(return_planAssumption, nyear),
    with(returnPar, create_returns(r.geoMean, 0, period)),
    replicate(nsim, with(returnPar, create_returns(r.arithMean, r.sd, period)))
  )
  colnames(i.r) <- -1:nsim
  
  #  i.r[,2]
  
return(i.r)
}

