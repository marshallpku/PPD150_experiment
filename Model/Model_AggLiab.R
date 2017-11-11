# This script calculate aggregate annual ALs, NCs and benefit payments for PSERS.


get_AggLiab <- function( liab_ = liab,
                         pop_  = pop,
                         paramlist         = planData_list$inputs_singleValues){

  # This function calculates total AL, NC and benefits.
  
  
  # Run the section below when developing new features.  
   # liab_      = liab
   # pop_       = pop
   # paramlist  = planData_list$inputs_singleValues
  
   assign_parmsList(paramlist, envir = environment())
  
   
  # Notes on naming conventions:
   # "tot" means total AL/NA/... in each year * age * ea cell
   # "sum" means sum of AL/NA/... across age * ea in each year. 
   # "av"  menas sum of variables related to life annuity, contingent annuity, and term benefits for actives. (now plus death benefit and disability benefit)
   
  
  #*************************************************************************************************************
  #                                     ## Liabilities and NCs for actives   ####
  #*************************************************************************************************************
  
  # Join population data frames and liability data frames. 
  liab_$active <- left_join(pop_$active, liab_$active) # %>% left_join(new_retirees)
  liab_$active[-(1:3)] <- colwise(na2zero)(liab_$active[-(1:3)]) # replace NAs with 0, so summation involing missing values will not produce NAs. 
  
  liab_$active %<>%  
    mutate(ALx.la.tot    = ALx.la * number.a,
           ALx.v.tot     = ALx.v    * number.a,
           #ALx.disb.tot = ALx.disb * number.a,
           ALx.av.tot    = ALx.la.tot + ALx.v.tot,  #+ ALx.disb.tot,
           
           NCx.la.tot    = NCx.la * number.a,
           NCx.v.tot     = NCx.v    * number.a,
           #NCx.disb.tot = NCx.disb * number.a,
           NCx.av.tot    = NCx.la.tot + NCx.v.tot, #+ NCx.disb.tot,
           
           PVFBx.la.tot   = PVFBx.la * number.a,
           PVFBx.v.tot    = PVFBx.v    * number.a,
           #PVFBx.disb.tot= PVFBx.disb * number.a,
           PVFBx.av.tot   = PVFBx.la.tot + PVFBx.v.tot, # + PVFBx.disb.tot,
           
           PR.tot  = sx * number.a,
           EEC.tot = EEC * number.a)
  
  active.agg <- liab_$active %>%  
    group_by(year) %>% 
    summarise(
      ALx.la.sum = sum(ALx.la.tot, na.rm = TRUE),
      ALx.v.sum    = sum(ALx.v.tot,    na.rm = TRUE),
      #ALx.disb.sum = sum(ALx.disb.tot, na.rm = TRUE),
      ALx.av.sum   = sum(ALx.av.tot,   na.rm = TRUE), 
      
      NCx.la.sum = sum(NCx.la.tot, na.rm = TRUE),
      NCx.v.sum    = sum(NCx.v.tot,    na.rm = TRUE),
      #NCx.disb.sum = sum(NCx.disb.tot, na.rm = TRUE),
      NCx.av.sum   = sum(NCx.av.tot,   na.rm = TRUE),
      
      PVFBx.la.sum = sum(PVFBx.la.tot, na.rm = TRUE),
      PVFBx.v.sum    = sum(PVFBx.v.tot,    na.rm = TRUE),
      #PVFBx.disb.sum = sum(PVFBx.disb.tot, na.rm = TRUE),
      PVFBx.av.sum   = sum(PVFBx.av.tot,   na.rm = TRUE),
      
      PR.sum     = sum(PR.tot,  na.rm = TRUE),
      EEC.sum    = sum(EEC.tot,  na.rm = TRUE),
      
      nactives  = sum(number.a,  na.rm = TRUE)) %>% 
      as.matrix # extracting elements from matrices is much faster than from data.frame

  
  active.agg.current <- 
    liab_$active %>%  
    filter(year - (age-ea) <= init.year) %>% 
    group_by(year) %>% 
    summarise(
      ALx.la.sum = sum(ALx.la.tot, na.rm = TRUE),
      ALx.v.sum    = sum(ALx.v.tot,    na.rm = TRUE),
      #ALx.disb.sum = sum(ALx.disb.tot, na.rm = TRUE),
      ALx.av.sum   = sum(ALx.av.tot,   na.rm = TRUE), 
      
      NCx.la.sum = sum(NCx.la.tot, na.rm = TRUE),
      NCx.v.sum    = sum(NCx.v.tot,    na.rm = TRUE),
      #NCx.disb.sum = sum(NCx.disb.tot, na.rm = TRUE),
      NCx.av.sum   = sum(NCx.av.tot,   na.rm = TRUE),
      
      PVFBx.la.sum = sum(PVFBx.la.tot, na.rm = TRUE),
      PVFBx.v.sum    = sum(PVFBx.v.tot,    na.rm = TRUE),
      #PVFBx.disb.sum = sum(PVFBx.disb.tot, na.rm = TRUE),
      PVFBx.av.sum   = sum(PVFBx.av.tot,   na.rm = TRUE),
      
      PR.sum    = sum(PR.tot,  na.rm = TRUE),
      EEC.sum    = sum(EEC.tot,  na.rm = TRUE),
      
      nactives  = sum(number.a,  na.rm = TRUE)) %>% 
    as.matrix # extracting elements from matrices is much faster than from data.frame
  
  active.agg.entrants <- 
    liab_$active %>%  
    filter(year - (age-ea) > init.year) %>% 
    group_by(year) %>% 
    summarise(
      ALx.la.sum = sum(ALx.la.tot, na.rm = TRUE),
      ALx.v.sum    = sum(ALx.v.tot,    na.rm = TRUE),
      #ALx.disb.sum = sum(ALx.disb.tot, na.rm = TRUE),
      ALx.av.sum   = sum(ALx.av.tot,   na.rm = TRUE), 
      
      NCx.la.sum = sum(NCx.la.tot, na.rm = TRUE),
      NCx.v.sum    = sum(NCx.v.tot,    na.rm = TRUE),
      #NCx.disb.sum = sum(NCx.disb.tot, na.rm = TRUE),
      NCx.av.sum   = sum(NCx.av.tot,   na.rm = TRUE),
      
      PVFBx.la.sum = sum(PVFBx.la.tot, na.rm = TRUE),
      PVFBx.v.sum    = sum(PVFBx.v.tot,    na.rm = TRUE),
      #PVFBx.disb.sum = sum(PVFBx.disb.tot, na.rm = TRUE),
      PVFBx.av.sum   = sum(PVFBx.av.tot,   na.rm = TRUE),
      
      PR.sum    = sum(PR.tot,  na.rm = TRUE),
      EEC.sum    = sum(EEC.tot,  na.rm = TRUE),
      
      nactives  = sum(number.a,  na.rm = TRUE)) %>% 
    as.matrix # extracting elements from matrices is much faster than from data.frame
  
  # active.agg
  # active.agg.current
  # active.agg.entrants
  #  
   
  #*************************************************************************************************************
  #                                     ## Liabilities and benefits for retirees (life annuitants)   ####
  #*************************************************************************************************************
  
  liab_$la  <- data.table(liab_$la, key = "ea,age,year,year.r")
  pop_$la   <- data.table(pop_$la,  key = "ea,age,year,year.r")
  liab_$la  <- merge(pop_$la, liab_$la, by = c("ea", "age","year", "year.r"), all.x = TRUE)
  liab_$la  <- as.data.frame(liab_$la)
  
  
  liab_$la %<>% 
    mutate(ALx.la.tot      = ALx.la * number.la,
           B.la.tot        = B.la   * number.la)
  
  
  la.agg <- liab_$la %>% 
    group_by(year) %>% 
    summarise(ALx.la.sum      = sum(ALx.la.tot, na.rm = TRUE),
              B.la.sum        = sum(B.la.tot  , na.rm = TRUE),
              nla             = sum(number.la , na.rm = TRUE)) %>% 
    # mutate(runname = runname) %>% 
    as.matrix
  
  la.agg.current.init <- liab_$la %>% 
    filter(year - (age-ea) <= init.year, year.r <= init.year) %>% 
    group_by(year) %>% 
    summarise(ALx.la.sum      = sum(ALx.la.tot, na.rm = TRUE),
              B.la.sum        = sum(B.la.tot  , na.rm = TRUE),
              nla             = sum(number.la , na.rm = TRUE)) %>% 
    # mutate(runname = runname) %>% 
    as.matrix
  
  la.agg.current.new <- liab_$la %>% 
    filter(year - (age-ea) <= init.year, year.r > init.year) %>% 
    group_by(year) %>% 
    summarise(ALx.la.sum      = sum(ALx.la.tot, na.rm = TRUE),
              B.la.sum        = sum(B.la.tot  , na.rm = TRUE),
              nla             = sum(number.la , na.rm = TRUE)) %>% 
    # mutate(runname = runname) %>% 
    as.matrix
  
  
  la.agg.entrants <- liab_$la %>% 
    filter(year - (age-ea) > init.year) %>% 
    group_by(year) %>% 
    summarise(ALx.la.sum      = sum(ALx.la.tot, na.rm = TRUE),
              B.la.sum        = sum(B.la.tot  , na.rm = TRUE),
              nla             = sum(number.la , na.rm = TRUE)) %>% 
    # mutate(runname = runname) %>% 
    as.matrix
  
  #la.agg
  la.agg.current.init
  la.agg.current.new
  la.agg.entrants

# y <- liab_$la %>% filter(year == 2016) %>% 
#   filter(!is.na(B.la))
# 
# x <-   liab_$la %>% filter(year.r == init.year,
#                       !is.na(ALx.la)) %>%
#     mutate(start.year = year - (age - ea)) %>% 
#     ungroup %>% 
#     arrange(start.year, ea, age) %>% 
#     group_by(start.year, ea) %>% 
#     mutate(ALx.la.tot.calc = lag(ALx.la.tot -B.la.tot) * (1+i),
#            diff = ALx.la.tot.calc - ALx.la.tot) %>% 
#     filter(year %in% 2016) 
#     
# 
# x
# 
# x$ALx.la.tot %>% sum
# x$B.la.tot   %>% sum 
# sum((x$ALx.la.tot - x$B.la.tot)*(1+i))
# 
# x$ALx.la.tot %>% sum
# 
# y <- liab_$la %>% filter(year == 2016) %>% 
#   summarize(tot = sum(ALx.la.tot, na.rm = T) )
# y
# y %>% head  
# 
# la.agg 





  #*************************************************************************************************************
  #                                 ## Liabilities and benefits for vested terms.   ####
  #*************************************************************************************************************
  
  # Save 10 seconds by using data.table to merge. Update 2/2016: the latest version of left_join looks fast enough.
  liab_$term <- left_join(pop_$term, liab_$term)
  # .liab$term  <- data.table(.liab$term, key = "ea,age,year,year.term")
  # .pop$term   <- data.table(.pop$term,  key = "ea,age,year,year.term")
  # .liab$term  <- merge(.pop$term, .liab$term, by = c("ea", "age","year", "year.term"), all.x = TRUE)
  # .liab$term  <- as.data.frame(.liab$term)
  
  #.liab$term %>% filter(year == 2015, year.term == 2014, age == 40)
  

  liab_$term %<>% 
    mutate(ALx.v.tot = ALx.v * number.v,
           B.v.tot   = B.v  * number.v)
  
  
  term.agg <- liab_$term %>% 
    group_by(year) %>% 
    summarise(ALx.v.sum   = sum(ALx.v.tot, na.rm = TRUE),
              B.v.sum     = sum(B.v.tot  , na.rm = TRUE),
              nterms      = sum(number.v , na.rm = TRUE)) %>% 
    as.matrix
  
  term.agg.current <- liab_$term %>% 
    filter(year - (age-ea) <= init.year) %>% 
    group_by(year) %>% 
    summarise(ALx.v.sum   = sum(ALx.v.tot, na.rm = TRUE),
              B.v.sum     = sum(B.v.tot  , na.rm = TRUE),
              nterms      = sum(number.v , na.rm = TRUE)) %>% 
    as.matrix
  
  term.agg.entrants <- liab_$term %>% 
    filter(year - (age-ea) > init.year) %>% 
    group_by(year) %>% 
    summarise(ALx.v.sum   = sum(ALx.v.tot, na.rm = TRUE),
              B.v.sum     = sum(B.v.tot  , na.rm = TRUE),
              nterms      = sum(number.v , na.rm = TRUE)) %>% 
    as.matrix
  
  term.agg
  term.agg.current
  term.agg.entrants
  
  #*************************************************************************************************************
  #                                     ## Liabilities and benefits for disability benefit (life annuitants)   ####
  #*************************************************************************************************************
  # 
  # liab_$disb.la  <- data.table(liab_$disb.la,    key = "ea,age,year,year.disb")
  # pop_$disb.la   <- data.table(pop_$disb.la,  key = "ea,age,year,year.disb")
  # liab_$disb.la  <- merge(pop_$disb.la, liab_$disb.la, by = c("ea", "age","year", "year.disb"), all.x = TRUE)
  # liab_$disb.la  <- as.data.frame(liab_$disb.la)
  # 
  # 
  # liab_$disb.la %<>% 
  #   mutate(ALx.disb.la.tot = ALx.disb.la * number.disb.la,
  #          B.disb.la.tot   = B.disb.la   * number.disb.la,
  #          runname = runname)
  # 
  # disb.la.agg <- liab_$disb.la %>% 
  #   group_by(year) %>% 
  #   summarise(ALx.disb.la.sum   = sum(ALx.disb.la.tot, na.rm = TRUE),
  #             B.disb.la.sum     = sum(B.disb.la.tot  , na.rm = TRUE),
  #             ndisb.la          = sum(number.disb.la , na.rm = TRUE)) %>% 
  #   # mutate(runname = runname) %>% 
  #   as.matrix
  

   
  
 
  #*************************************************************************************************************
  #                                 ## Liabilities and benefits for vested terms.   ####
  #*************************************************************************************************************
  
  AggLiab <-  list(active = active.agg,
                   active.current  = active.agg.current,
                   active.entrants = active.agg.entrants,
                  
                   la               = la.agg,
                   la.current.init  = la.agg.current.init,
                   la.current.new   = la.agg.current.new,
                   la.entrants      = la.agg.entrants,
                   
                   term          = term.agg,
                   term.current  = term.agg.current,
                   term.entrants = term.agg.entrants)
          
}




#*************************************************************************************************************
#                                     ## Summing up tier values to get total values of a plan   ####
#*************************************************************************************************************

get_AggLiab_sumTiers <- function(...){
 # This function create list of aggregate values of a plan from list of tiers. 
 # ... :  a series data list of tiers.   
  
 #  AggLiab.list <- list(AggLiab.t76, AggLiab.t13, AggLiab.tm13)
  
  AggLiab.list <- list(...)
  
  AggLiab.list %<>% lapply( function(List) lapply(List, as.data.frame)) 

  nTiers <- length(AggLiab.list)
  nTypes <- length(AggLiab.list[[1]])
  TypeNames <- names(AggLiab.list[[1]])
  
  AggLiab.list2 <- vector("list", nTypes)
  names(AggLiab.list2) <- TypeNames
  
  for (j in TypeNames){
    AggLiab.list2[[j]] <- lapply(AggLiab.list, function(df){df[[j]]}) 
  }
  
  sum_tiers <- function(df){ df %<>% group_by(year) %>% 
      summarise_each(funs(sum))
  } 
  
  AggLiab_sumTiers <- AggLiab.list2 %>% 
                      lapply(bind_rows) %>% 
                      lapply(sum_tiers) %>% 
                      lapply(as.matrix)
  
  return(AggLiab_sumTiers)
}
  
  
  
  






  
  

# start_time_prep_loop <-  proc.time()
# 
# AggLiab <- get_AggLiab()
# 
# end_time_prep_loop <-  proc.time()
# Time_prep_loop <- end_time_prep_loop - start_time_prep_loop


