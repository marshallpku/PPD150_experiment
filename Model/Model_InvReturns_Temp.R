# This module create investment return series. 

gen_returns <- function( paramlist = planData_list$inputs_singleValues
                         #returnScenarios_ = returnScenarios
                         ){
#   
  # paramlist =  planData_list$inputs_singleValues
  # returnScenarios_ = returnScenarios
  # seed    = 1234) {
  
  
  
  assign_parmsList(paramlist, envir = environment())
  
  
  return_type <- "simple"
  ir.sd   <- 0.12
  ir.mean <-  i + ir.sd^2/2
  
  #set.seed(seed)
  #i.r <- matrix(rnorm(nyear  *nsim, mean = ir.mean, sd = ir.sd),nrow = nyear, ncol = nsim)
  
  if(return_type == "simple"){
    set.seed(1234)
    i.r <- matrix(rnorm(nyear*nsim, ir.mean, ir.sd), nyear, nsim)
    i.r <- cbind(rep(ir.mean - ir.sd^2/2, nyear), i.r)
    colnames(i.r) <- 0:nsim
  }
  
  
  if (return_type == "internal"){
    # return_scenario <- "RS4"
    # nsim = 5
    
    returnScenarios_local <- returnScenarios_ %>% filter(scenario == return_scenario)
    set.seed(1234)
    i.r <- cbind(
      with(returnScenarios_local, create_returns(return_det, 0, period)),
      replicate(nsim, with(returnScenarios_local, create_returns(r.mean, r.sd, period)))
    )
    colnames(i.r) <- 0:nsim
  }
  


i.r <- cbind(rep(i, nyear),                  # Check consistency
             #rep(ir.mean - ir.sd^2/2, nyear), # Deterministic run
             i.r)
colnames(i.r) <- c(-1:nsim)

return(i.r)
}
#gen_returns()[,2] 
