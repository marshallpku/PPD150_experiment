


#****************************************************************************************************
#                                       Tools                                                   #####                  
#****************************************************************************************************

# Functions for loading single values ####
get_singleValues <- function(filename){
  
  #filename <- paste0(data_dir, planname, ".xlsx")
  
  # Single values
  df_singeValues_raw <- 
    read_ExcelRange(filename, "singleValues") %>% 
    filter(!is.na(varname)) 
  
  planname <- df_singeValues_raw[df_singeValues_raw$varname == "planname", "value"]
  df_singeValues_raw %<>% 
    mutate(planname = planname)
  
  # Create separate df for numeric and non-numereic variables. 
  
  df_singleValues_num <- 
    df_singeValues_raw %>% 
    filter(varType %in% c("numeric", "logical")) %>% 
    mutate(value = as.numeric(value),
           value = ifelse(category == "targets", value * units, value),
           FY    = str_extract(year.or.date, "\\d{4}") %>% as.numeric,
           plantype = plantype) %>%
    select(planname, plantype, category, varname, description, value, FY)
  df_singleValues_num
  
  
  df_singleValues_chr <- 
    df_singeValues_raw %>% 
    filter(!varType %in% c("numeric", "logical")) %>% 
    mutate(FY    = str_extract(year.or.date, "\\d{4}") %>% as.numeric,
           plantype = plantype) %>%
    select(planname, plantype, category, varname, description, value, FY)
  df_singleValues_chr
  
  singleValues <- list(num = df_singleValues_num, chr = df_singleValues_chr )
}


# Utility functions
getcell <- function(file, sheet, cell) {
  require(XLConnect)
  value <- readWorksheetFromFile(file, sheet=sheet, header=FALSE, region=cell, colTypes="character")
  return(as.character(value))
}


xlrange <- function(file, sheet, cell1, cell2) {
  startcell <- getcell(file, sheet, cell1)
  endcell   <- getcell(file, sheet, cell2)
  range     <- paste0(startcell, ":", endcell)
  return(range)
}


get_bound <- function(range, bound = c("l","u")){
  # range must be in the form of "XX-YY", "X" is a single digit, and "XX" <= "YY".
  switch(bound,
         l =  str_extract(range, "\\d+-") %>% gsub("\\D+", "",.) %>% as.integer,
         u =  str_extract(range, "-\\d+") %>% gsub("\\D+", "",.) %>% as.integer)
}



# Load actives
import_actives <- function(file, sheet, planname){
  #   
  
   # file <- paste0(data_dir, "140_LA_City_LAFPP", ".xlsx")
   # sheet <- "ActivesSched"
   # planname <- "Actives_allTiers"
  
  # getcell("Data_inputs/LAFPP_MemberData1.xlsx", "sheet1", "B2")
  # read_excel(file, sheet = "TOC")
  
  
  range <- xlrange(file, sheet, "B2", "B3")
  
  df <- readWorksheetFromFile(file, sheet=sheet, header=TRUE, region=range, colTypes="character") %>% 
        filter(!is.na(type))
  
  
  yoscuts <- df %>% filter(type == "yosgrp") %>%
    select(starts_with("X")) %>%
    gather(yos.cell, yosgrp) %>%
    mutate(yos.cell=as.integer(gsub("[^0-9]", "", yos.cell)),
           yoslb = get_bound(yosgrp, "l"),
           yosub = get_bound(yosgrp, "u")) %>% 
    select(-yosgrp)
  yoscuts
  
  agecuts <- df %>% filter(type != "yosgrp") %>% 
    filter(type == unique(type)[1]) %>% 
    select(age.cell, agegrp) %>% 
    mutate(agelb = get_bound(agegrp, "l"),
           ageub = get_bound(agegrp, "u")) %>% 
    select(-agegrp)
  agecuts
  
  
  df %<>% filter(type != "yosgrp") %>% 
    select(type, age.cell, starts_with("X")) %>%
    gather(yos.cell, value, -type, -age.cell) %>%
    mutate(yos.cell = as.integer(gsub("[^0-9]", "", yos.cell)),
           age.cell = as.integer(age.cell),
           value    = as.numeric(value),
           age = age.cell,
           yos = yos.cell,
           planname = planname) %>%
    filter(!is.na(value)) %>% 
    spread(type, value) %>%
    arrange(age.cell, yos.cell)
  df
  
  lactives <- list()
  lactives$agecuts <- agecuts
  lactives$yoscuts <- yoscuts
  lactives$actives.yos <- df
  
  return(lactives)
}


# Load retirees, also can be used to load initial beneficiaries and initial disabled
import_retirees_byAge <- function(file, sheet, planname){
  
  # file <- paste0(path, fileName)
  
  # sheet <- "Beneficiaries"
  # planname <- Tier_select
  
  
  range <- xlrange(file, sheet, "B2", "B3")
  benperiod <- getcell(file, sheet, "B4")
  benmult <- ifelse(benperiod=="month", 12, 1)
  name_N  <- getcell(file, sheet, "B5")
  name_V  <- getcell(file, sheet, "B6")
  
  
  df <- readWorksheetFromFile(file, sheet=sheet, header=TRUE, region=range, colTypes="character")
  
  agecuts <- df %>%  
    select(age.cell, agegrp) %>% 
    mutate(agelb = get_bound(agegrp, "l"),
           ageub = get_bound(agegrp, "u")) %>% 
    select(-agegrp)
  
  df %<>%  
    select(-agegrp) %>%
    colwise(as.numeric)() %>% 
    mutate(age.cell = as.integer(age.cell),
           age = age.cell,
           V = V * benmult,
           planname = planname)
  
  
  list_out <- list()
  list_out$data <- df
  list_out$agecuts <- agecuts
  list_out$varNames <- c(name_N = name_N, name_V = name_V)
  
  return(list_out)
}


# Interpolation of actives
fillin.actives.spreadyos.splineage <- function(lactives) {
  # salary:
  #   first spread uniformly within age.cell-yos.cell group (same salary for all)
  #   then for every yos, estimate salary for each age using a spline - adjust endpoints first for plausibility
  #   finally, adjust resulting salary within each age.cell-yos.cell proportionately to hit total payroll values from grouped data
  #   then add ea to the data
  # nactives: spread uniformly within age.cell-yos.cell group (same nactives for all), then add ea to the data
  
  lactives
  
  adf <- lactives$actives.yos
  agecuts <- lactives$agecuts
  yoscuts <- lactives$yoscuts
  #eacuts <- lactives$eacuts
  minage <- min(agecuts$agelb)
  maxage <- max(agecuts$ageub)
  minyos <- min(yoscuts$yoslb)
  maxyos <- max(yoscuts$yosub)
  
  planname <- paste0(adf$planname[1])
  
  # adf %>% select(age, ea, salary) %>% spread(ea, salary)
  # adf %>% select(age, ea, nactives) %>% spread(ea, nactives)
  
  # create a master grouped data frame
  adf.g <- adf %>% select(-planname, -age, -yos, nactives.cell=nactives, salary.cell=salary) %>%
    mutate(pay.cell=nactives.cell * salary.cell) %>%
    mutate(ageidx = findInterval(age.cell, agecuts$agelb),
           age.lb = agecuts$agelb[ageidx],
           age.ub = agecuts$ageub[ageidx],
           yosidx = findInterval(yos.cell, yoscuts$yoslb),
           yos.lb = yoscuts$yoslb[yosidx],
           yos.ub = yoscuts$yosub[yosidx]) %>%
    select(age.cell, yos.cell, age.lb, age.ub, yos.lb, yos.ub, nactives.cell, salary.cell, pay.cell)
  
  # expand the grouped data frame to all allowable age-yos combinations ####
  xpnd <- function(df) {
    # expand to all age-yos combinations but only keep those where ea>=15 or, if there are no such records,
    # keep the recrods with max ea
    df2 <- expand.grid(age=df$age.lb:df$age.ub, yos=df$yos.lb:df$yos.ub) %>%
      mutate(ea=age - yos) %>%
      filter((ea >= 20) | (ea<20 & ea==max(ea))) %>%
      select(-ea)
    return(df2)
  }
  
  adf.x <- adf.g %>% rowwise() %>%
    do(cbind(., xpnd(.))) %>%
    ungroup %>%  # get rid of rowwise
    group_by(age.cell, yos.cell) %>%
    mutate(n.cell=n()) %>%
    select(age, yos, everything()) %>%
    arrange(age, yos)
  
  
  # work with the expanded data ####
  
  # we have to anchor the endpoints with reasonable values BEFORE computing the spline
  adjustends <- function(age, salary) {
    # the basic idea is that if an endpoint is NA, insert a plausible value
    
    # simple rule: if spline first or last value falls within +/ 50% of the nearest nonNA value, use spline estimate
    # otherwise use the capped value
    firstnonna <- salary[which.min(is.na(salary))]
    lastnonna <- rev(salary)[which.min(is.na(rev(salary)))]
    bound <- .5
    firstrange <- c(firstnonna * bound, firstnonna * (1 + bound))
    lastrange <- c(lastnonna * bound, lastnonna * (1 + bound))
    cap <- function(sal, range) {
      cappedval <- max(sal, range[1])
      cappedval <- min(cappedval, range[2])
      return(cappedval)
    }
    
    salary.est <- spline(age, salary, xout=age)$y # what does spline think naively?
    salary.adjusted <- salary
    
    if(is.na(salary[1])) salary.adjusted[1] <- cap(salary.est[1], firstrange)
    ilast <- length(salary)
    if(is.na(salary[ilast])) salary.adjusted[ilast] <- cap(salary.est[ilast], firstrange)
    
    return(salary.adjusted)
  }
  
  # test out adjustends
  # fs <- function(age, sal) return(spline(age, sal, xout=age)$y) # spline doesn't seem to work with dplyr if not in function
  # # various salaries to try out
  # salary <- seq(20, 50, length.out = 10)
  # salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 80)
  # salary <- c(20, NA, 30, NA, 40, NA, 50, NA, NA, 30)
  # salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, 30)
  # salary <- c(NA, NA, 30, NA, 40, NA, 50, NA, NA, NA)
  # salary <- c(NA, 10, 30, NA, 40, NA, 50, 80, NA, NA)
  # age <- 21:30
  # d <- data_frame(age, salary, saladj=adjustends(age, salary)) %>%
  #   mutate(sal.spline=fs(age, salary),
  #          saladj.spline=fs(age, saladj))
  # d
  # qplot(age, value, data=gather(d, variable, value, -age), colour=variable, geom=c("point", "line")) + scale_x_continuous(breaks=0:100) + geom_hline(y=0)
  
  
  spline.y2 <- function(age, salary, safesalary) {
    # safesalary is what we use if salary has no data
    if(all(is.na(salary))) {
      print("AllNA")
      salary <- safesalary
    }
    salary.adjusted <- adjustends(age, salary)
    
    sp.out <- spline(age, salary.adjusted, xout=age)
    salout <- sp.out$y
    return(salout)
  }
  
  adf.x3 <- adf.x %>% ungroup %>% # MUST be ungrouped or ifelse won't work if there is only one rec in a group
    mutate(nactives=nactives.cell / n.cell, # always spread nactives uniformly
           salary.group=ifelse(age==age.cell & yos==yos.cell, salary.cell, NA),
           salary.group=ifelse(salary.group==0, NA, salary.group),
           salary.agecell=ifelse(age==age.cell, salary.cell, NA)) %>% # Yimeng's first step
    group_by(yos) %>%
    arrange(age) %>%
    mutate(salary.spline.adjep=spline.y2(age, salary.agecell, salary.cell)) %>% # Yimeng's 2nd step with endpoint adjustment
    group_by(age.cell, yos.cell) %>%
    mutate(planname=planname,
           pay.unadj=sum(salary.spline.adjep * nactives),
           adjust=pay.cell / pay.unadj,
           salary.final=salary.spline.adjep * adjust,
           pay.adj=sum(salary.final * nactives),
           ea=age - yos
           #ea.cell=eacuts$stub[findInterval(ea, eacuts$lb)]
    )
  
  return(adf.x3)
}


# Interpolation of retirees
fillin.retirees <- function(list_data) {
  
  rdf <- select(list_data$data, planname, age, N, V) # keep only the vars we want
  agecuts <- list_data$agecuts
  
  planname <- paste0(rdf$planname[1], "_fillin")
  name_N <- list_data$varNames["name_N"]
  name_V <- list_data$varNames["name_V"]
  
  # add group ranges to the retirees data frame
  combo <- rdf %>%
    mutate(totben=N * V) %>%
    mutate(ageidx=findInterval(age, agecuts$agelb),
           age.lb=agecuts$agelb[ageidx],
           age.ub=agecuts$ageub[ageidx]) %>%
    arrange(age)
  
  # get avg benefits by age, via spline
  avgben <- splong(select(combo, age, V), "age", min(combo$age.lb):max(combo$age.ub))
  # force benefit to be non-negative DJB added 10/30/2015
  avgben <- avgben %>% mutate(V=ifelse(V<0, 0, V))
  
  guessdf <- data.frame(age=min(combo$age.lb):max(combo$age.ub)) %>%
    mutate(ageidx=findInterval(age, agecuts$agelb),
           age.cell=combo$age[match(ageidx, combo$ageidx)],
           N.cell=combo$N[match(ageidx, combo$ageidx)],
           V.cell=combo$V[match(ageidx, combo$ageidx)]) %>%
    group_by(age.cell) %>%
    mutate(n.cell=n(),
           N=N.cell / n.cell, # spread nretirees evenly
           adjV=avgben$V[match(age, avgben$age)], # get the spline-based avg benefit
           adjtotben=N * adjV)
  
  # refine the guess by adjusting ensure that we hit the right total benefits in each group
  guessdf2 <- guessdf %>% group_by(age.cell) %>%
    mutate(adjust=mean(N.cell * V.cell) / sum(adjtotben),
           V=adjV*adjust,
           totben=N * V)
  
  rdf.fillin <- guessdf2 %>% mutate(planname=planname) %>%
    select(planname, age.cell, age, N, V) %>%
    ungroup
  #plyr::rename(c("N" = list_data$varNames["name_N"])))
  
  names(rdf.fillin)[names(rdf.fillin) == "N"] <- name_N
  names(rdf.fillin)[names(rdf.fillin) == "V"] <- name_V
  
  return(rdf.fillin)
}



fn_actives <- function(planname_, plantype_,  fileName_, sheet = "ActivesSched"){
  
  # path_ = path
  # fileName_ = fileName
  # Tier_select = "t76"
  
  lactives <- import_actives(fileName_, sheet, planname_)
  # lactives$actives.yos %<>% filter(age - yos >= 20) # Will drop a small number of members, need to figure out how to add memebers with ea<20 back   
  
  actives_grouped <- lactives$actives.yos %>% select(planname, age, yos, nactives, salary) %>% 
    mutate(planname = planname_,
           plantype = plantype_,
           fillin   = FALSE,
           ea = age - yos)
  
  actives_fillin  <- fillin.actives.spreadyos.splineage(lactives) %>% ungroup %>% 
    select(planname, age, yos, ea,
           #age.cell, yos.cell, 
           nactives, salary=salary.final) %>% 
    mutate(planname = planname_,
           plantype = plantype_,
           fillin   = TRUE)
  
  actives_out <- bind_rows(actives_fillin, actives_grouped) %>% 
    select(planname,plantype, fillin, ea, age, yos, nactives, salary) %>% 
    arrange(fillin, ea, age)
}




fn_ret.ben <- function(planname_, plantype_, fileName_, sheet = "RetireesSched"){
  
  
  # fileName_ = fileName
  # Tier_select = "t76"
  
  ldata <- import_retirees_byAge(fileName_, sheet, planname_)
  
  df_grouped <- ldata$data %>% select(planname, age, N, V) %>% mutate(planname = planname_,
                                                                      plantype = plantype_,
                                                                      fillin   = FALSE)
  names(df_grouped)[names(df_grouped) == "N"] <- ldata$varNames["name_N"]
  names(df_grouped)[names(df_grouped) == "V"] <- ldata$varNames["name_V"]
  
  df_fillin  <- fillin.retirees(ldata) %>% ungroup %>% select(-age.cell) %>% 
    mutate(planname = planname_,
           plantype = plantype_,
           fillin   = TRUE)
  
  df_out <- bind_rows(df_fillin, df_grouped) %>% 
    select(planname, plantype, fillin, everything() )
}


x <- 20:74
v <- c(20, 30, 40) # create two bins [5,10) and [10,15)
cbind(x, findInterval(x, v))

