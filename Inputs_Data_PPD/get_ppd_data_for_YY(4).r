# Don Boyd
# 9/18/2017


#****************************************************************************************************
#                Libraries ####
#****************************************************************************************************
library("magrittr")
library("plyr") # needed for ldply; must be loaded BEFORE dplyr
library("tidyverse")
options(tibble.print_max = 60, tibble.print_min = 60) # if more than 60 rows, print 60 - enough for states
# ggplot2 tibble tidyr readr purrr dplyr

library("scales")
library("hms") # hms, for times.
library("stringr") # stringr, for strings.
library("lubridate") # lubridate, for date/times.
library("forcats") # forcats, for factors.
library("readxl") # readxl, for .xls and .xlsx files.
library("haven") # haven, for SPSS, SAS and Stata files.
#library("vctrs")
#library("precis")

library("grDevices")
library("knitr")

library("zoo") # for rollapply

library("btools") # library that I created (install from github)
library("bdata")  # library that I created (install from github)

library("pdata") # library that I created (install from github)


#****************************************************************************************************
#                Functions ####
#****************************************************************************************************
getbest <- function(df, var, priorities){
  # return an augmented data frame df with the "best" version of variable var
  # based on priorities, and also return the name of the best variable
  
  # create an ordered vector of the most-preferred to least-preferred variable
  pfy <- as.vector(t(outer(priorities, 2016:2014, paste, sep="_")))
  
  bestvar <- df[[pfy[1]]]
  bestvarname <- rep(pfy[1], length(bestvar))
  
  for(vname in pfy[2:length(pfy)]){
    bestvarname <- ifelse(is.na(bestvar), vname, bestvarname)
    bestvar <- ifelse(is.na(bestvar), df[[vname]], bestvar)
  }
  
  df[, var] <- bestvar
  df[, paste0(var, "_source")] <- bestvarname
  
  return(df)
}

#****************************************************************************************************
#                Globals ####
#****************************************************************************************************

idvars <- c("ppd_id", "fy", "PlanName", "StateAbbrev", "planf")


#****************************************************************************************************
#                Test area ####
#****************************************************************************************************
# quick aside -- how well does benefits_tot match expense_TotBenefits??
test <- ppd %>% 
  filter(fy>=2014, !is.na(benefits_tot), !is.na(expense_TotBenefits)) %>%
  select(ppd_id, PlanName, fy, benefits_tot, expense_TotBenefits) %>%
  mutate(expense_TotBenefits=-expense_TotBenefits)

# values are supposed to be in $ thousands
probs <- c(0, .1, .25, .5, .75, .9, 1)
quantile(test$benefits_tot, prob=probs) # top end is too high
quantile(test$expense_TotBenefits, prob=probs) # good

# what's going on with bad benefits_tot?
test %>% filter(benefits_tot > 10e6)
# ppd_id                                                        PlanName    fy benefits_tot expense_TotBenefits
# <int>                                                           <chr> <int>        <dbl>               <dbl>
# 1     11                                                Chicago Teachers  2016   1361882496           1351250.8
# 2     59                                                Mississippi PERS  2016   2249044736           2367709.0
# 3     95                         

# Rhode Island ERS  2016    459101632            821699.3
# 4    168 City of Miami Firefighters and Police Officers Retirement Trust  2016    227189760            141474.7

ppd %>% 
  filter(fy>=2005, ppd_id %in% c(11, 59, 95, 168)) %>%
  select(ppd_id, PlanName, fy, benefits_tot, expense_TotBenefits) %>%
  mutate(expense_TotBenefits=-expense_TotBenefits, fy=as.character(fy)) %>%
  kable(digits=0, format.args=list(big.mark = ','))

# fixes to  benefits_tot: (if 2016), by ppd_id:
#  11, 59 -- div 1000
#  95, 168 -- set to NA; 168 is weird

test2 <- test %>%
  # use NA_real_, as R throws error if plain NA is used
  mutate(benefits_tot=
           case_when(fy==2016 & ppd_id %in% c(11, 59) ~ benefits_tot / 1000,
                     fy==2016 & ppd_id %in% c(95, 168) ~ NA_real_,
                     benefits_tot==0 ~ NA_real_,
                     TRUE ~ benefits_tot),
         expense_TotBenefits=ifelse(expense_TotBenefits==0, NA_real_, expense_TotBenefits)) %>%
  filter(!is.na(benefits_tot), !is.na(expense_TotBenefits))

# maybe some odd low-end values still
probs <- c(seq(0, .1, .02), .25, .5, .75, .9, 1)
quantile(test2$benefits_tot, prob=probs)
quantile(test2$expense_TotBenefits, prob=probs)

test2 %>% filter((benefits_tot < 10e3) | (expense_TotBenefits < 10e3))
# A tibble: 3 x 5
# ppd_id                                    PlanName    fy benefits_tot expense_TotBenefits
# <int>                                       <chr> <int>        <dbl>               <dbl>
# 1    100                           South Carolina RS  2016     2766.463           2689478.0
# 2    151                          Milwaukee City ERS  2015   328296.000              9686.0
# 3    165 Detroit Employees General Retirement System  2015     9671.604            253533.5

ppd %>% 
  filter(fy>=2005, ppd_id %in% c(100, 151, 165)) %>%
  select(ppd_id, PlanName, fy, benefits_tot, expense_TotBenefits) %>%
  mutate(expense_TotBenefits=-expense_TotBenefits, fy=as.character(fy)) %>%
  kable(digits=0, format.args=list(big.mark = ','))

# fixes by ppd_id:
# 100 2016 -- benefits_tot mult 1000
# 151 2015 expense_TotBenefits NA
# 165 2014-2016 -- benefits_tot NA

test3 <- test2 %>%
  mutate(benefits_tot=
           case_when(fy==2016 & ppd_id==100 ~ benefits_tot * 1000,
                     fy %in% 2014:2016 & ppd_id==165 ~ NA_real_,
                     TRUE ~ benefits_tot),
         expense_TotBenefits=ifelse(ppd_id==151 & fy==2015, NA_real_, expense_TotBenefits)) %>%
  filter(!is.na(benefits_tot), !is.na(expense_TotBenefits))



test3 %>%
  ggplot(aes(benefits_tot / mean(benefits_tot), expense_TotBenefits / mean(expense_TotBenefits))) +
  geom_point() +
  geom_abline(slope=1, intercept=0) +
  scale_x_continuous(breaks=seq(0, 8, 1), limits=c(0, 7)) +
  scale_y_continuous(breaks=seq(0, 8, 1), limits=c(0, 7))

# there are still some oddballs but stop here for now
# is there a nonzero intercept?
summary(lm(expense_TotBenefits ~ benefits_tot, data=test3))
# not really. good. we'll use expense_TotBenefits as our backup for missing benefits_tot


test4 <- ppd %>% filter(fy >=2014) %>% 
  select(ppd_id, fy, PlanName, contrib_EE_regular, contrib_EE_PurchaseService, contrib_EE_other )



#****************************************************************************************************
#                Numeric variables ####
#****************************************************************************************************

# NOTE: I tried to put the variable names in each vector below in descending order of quality as I perceive it
alvars <- c("ActLiabilities_GASB", "TotalPensionLiability")
payvars <- c("payroll", "ActiveSalaries", "CoveredPayroll_GASB67")
mvavars <- c("MktAssets_net", "NetPosition")
avavars <- c("ActAssets_GASB", "ActAssets_Smooth", "ActAssets_AVA")
uaalvars <- "UAAL_GASB"
activesvars <- "actives_tot"
beneficiariesvars <- "beneficiaries_tot"
benefitsvars <- c("benefits_tot", "expense_TotBenefits") # expense_TotBenefits is GASB measure -- sign is reversed!
avgbenvars <- "BeneficiaryBenefit_avg"

discountvars <- "InvestmentReturnAssumption_GASB"
inflationvar <- "InflationAssumption_GASB"
eecvar       <- "contrib_EE_regular"


numvars <- c(idvars, alvars, payvars, mvavars, avavars, uaalvars, activesvars,
             beneficiariesvars, benefitsvars, avgbenvars, 
            discountvars, inflationvar, eecvar)

nonidvars <- setdiff(numvars, idvars)


# get ppd data and make some fixes determined in test area above
df1 <- ppd %>%
  filter(fy %in% 2014:2016) %>%
  select(numvars) %>%
  mutate(expense_TotBenefits=-expense_TotBenefits,
         expense_TotBenefits=ifelse(expense_TotBenefits==0, NA_real_, expense_TotBenefits),
         benefits_tot=ifelse(benefits_tot==0, NA_real_, benefits_tot)) %>%
  mutate(benefits_tot=
           case_when(fy==2016 & ppd_id %in% c(11, 59) ~ benefits_tot / 1000,
                     fy==2016 & ppd_id==100 ~ benefits_tot * 1000,
                     fy==2016 & ppd_id %in% c(95, 168) ~ NA_real_,
                     fy %in% 2014:2016 & ppd_id==165 ~ NA_real_,
                     TRUE ~ benefits_tot),
         expense_TotBenefits=ifelse(ppd_id==151 & fy==2015, NA_real_, expense_TotBenefits)) %>%
  gather(variable, value, !!nonidvars) %>%
  unite(variable, variable, fy) %>%
  spread(variable, value)


# df2a <- df1 %>%
#   do(getbest(., var="al", priorities=c("ActLiabilities_GASB", "TotalPensionLiability")))#
# df2b <- df1 %>%
#   getbest(var="al", priorities=c("ActLiabilities_GASB", "TotalPensionLiability"))#
# identical(df2a, df2b)
df2 <- df1 %>% 
  do(getbest(., var="al", priorities=c("ActLiabilities_GASB", "TotalPensionLiability"))) %>%
  do(getbest(., var="payroll", priorities=payvars)) %>%
  do(getbest(., var="assets.mv", priorities=mvavars)) %>%
  do(getbest(., var="assets.av", priorities=avavars)) %>%
  do(getbest(., var="uaal", priorities=uaalvars)) %>%
  do(getbest(., var="actives", priorities=activesvars)) %>%
  do(getbest(., var="beneficiaries", priorities=beneficiariesvars)) %>%
  do(getbest(., var="benefits", priorities=benefitsvars)) %>%
  do(getbest(., var="avgbenefit.ppd", priorities=avgbenvars)) %>% 
  do(getbest(., var="discount", priorities="InvestmentReturnAssumption_GASB")) %>% 
  do(getbest(., var="infl.ppd", priorities="InflationAssumption_GASB")) %>% 
  do(getbest(., var="eec",      priorities="contrib_EE_regular")) 


count(df2, al_source)
count(df2, payroll_source)
count(df2, assets.mv_source)
count(df2, assets.av_source)
count(df2, uaal_source)
count(df2, actives_source)
count(df2, actives_source, payroll_source) # question as to whether we want to do something about 29 mismatched years
count(df2, benefits_source)
count(df2, beneficiaries_source)
count(df2, benefits_source, beneficiaries_source) # 13+45+12=70 mismatches out of 170 records
count(df2, avgbenefit.ppd_source)
count(df2, discount_source)
count(df2, infl.ppd_source)
count(df2, eec_source)


df3 <- df2 %>%
  mutate(avgsalary=payroll / actives * 1000,
         avgbenefit=benefits / beneficiaries * 1000,
         avgbenefit.ppd=avgbenefit.ppd * 1000,
         avgbenefit_best=ifelse(is.na(avgbenefit) | is.infinite(avgbenefit), avgbenefit.ppd, avgbenefit),
         
         eecRate.ppd_calc = eec / payroll,
         eec = eec * 1000
         )

df3 %>% 
  gather(variable, value, al, payroll, assets.mv, assets.av, uaal, actives, 
         benefits, beneficiaries,
         avgsalary, avgbenefit, avgbenefit.ppd, avgbenefit_best,
         eec, eecRate.ppd_calc) %>%
  group_by(variable) %>%
  do(qtiledf(.$value)) %>%
  kable(digits=2)
# 1 missing beneficiaries, 51 missing benefits
# some pretty bad values for avgbenefit

tmp <- df3 %>% filter(avgbenefit_best > 200e3) %>%
  select(ppd_id, PlanName, planf, starts_with("ben"), starts_with("avgb"))
 # 153 Dallas Police and Fire Safety still needs serious examination? $243k avg benefit


#****************************************************************************************************
#                Character variables ####
#****************************************************************************************************

miscvars <- c("AssetSmoothingPeriod_GASB", "openclosedf", "pctdollf", "UAALAmortPeriod_GASB")
charvars <- c(idvars, miscvars)

dfchar <- ppd %>%
  filter(fy %in% 2014:2016) %>%
  select(charvars) %>%
  gather(variable, value, !!!miscvars) %>%
  unite(variable, variable, fy) %>%
  spread(variable, value)

dfchar2 <- dfchar %>%
  do(getbest(., var="assetperiod", priorities="AssetSmoothingPeriod_GASB")) %>%
  mutate(assetperiod=as.numeric(assetperiod)) %>%
  do(getbest(., var="openclosedf", priorities="openclosedf")) %>%
  do(getbest(., var="pctdollf", priorities="pctdollf")) %>%
  do(getbest(., var="amortperiod", priorities="UAALAmortPeriod_GASB")) %>%
  mutate(amortperiod=as.numeric(amortperiod))


#****************************************************************************************************
#                Combine files ####
#****************************************************************************************************
filedate <- format(Sys.time(), '%Y-%m-%d')

df_full <- df3 %>%
  left_join(dfchar2)
glimpse(df_full)
# precis(df_full)

write_csv(df_full, paste0("./PPDvars/ppd_for_Yimeng_", filedate, ".csv"))

allnames <- names(df_full)
dropvars <- c(str_subset(allnames, "2014"), str_subset(allnames, "2015"), str_subset(allnames, "2016"))
keepvars <- setdiff(allnames, dropvars)

df_trim <- df_full %>%
  select(keepvars)

write_csv(df_full, paste0("./PPDvars/ppdtrim_for_Yimeng_", filedate, ".csv"))



