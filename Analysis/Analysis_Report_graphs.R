



#********************************************************************************
#                            Packages and tools ####
#********************************************************************************

rm(list = ls())
gc()

library(knitr)
library(data.table)
library(gdata) # read.xls
library(plyr)
library(dplyr)
options(dplyr.print_min = 100) # default is 10
options(dplyr.print_max = 100) # default is 20
library(ggplot2)
library(magrittr)
library(tidyr) # gather, spread
library(foreach)
library(doParallel)
library(microbenchmark)
library(readxl)
library(stringr)
library(zoo)
library("readxl")
library("XLConnect") # slow but convenient because it reads ranges; NOTE: I had to install Java 64-bit on Windows 10 64-bit to load properly
# library(xlsx)
library("btools")
options(dplyr.print_min = 60) # default is 10

library(grid)
library(gridExtra)

source("Functions.R")


#**********************************************************************************************
##  Defining color and theme for publication format of Rockefeller Institute of Government ####
#**********************************************************************************************

RIG.blue  <- "#003598"
RIG.red   <- "#A50021"
RIG.green <- "#009900"
RIG.yellow <- "#FFFF66"
RIG.purple <- "#9966FF"
RIG.yellow.dark <- "#ffc829"
RIG.orange <- "#fc9272"

demo.color6 <- c(RIG.red,
                 RIG.orange,
                 RIG.purple,
                 RIG.green ,
                 RIG.blue,
                 RIG.yellow.dark)


RIG.theme <- function(){
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_line(size = 0.5, color = "gray80"),
        plot.title=element_text(hjust=0.5),
        plot.subtitle=element_text(hjust=0.5),
        plot.caption=element_text(hjust=0, size = 9))
}



#********************************************************************************                          
#                  Global settings ####
#********************************************************************************

dir_data_ppd        <- "./Inputs_Data_PPD/"
dir_data_std        <- "./Inputs_Data_std/planData_std/"
dir_outputs_liab    <- "./Outputs_liab/"
dir_outputs_sim     <- "./Outputs_sim/"


file_ppd <- "DataPPD.RData"
load(paste0(dir_data_ppd, file_ppd))

ppd_id_all        <- PPD_data$ppd_id
ppd_id_largePlans <- c(9, 26, 83, 85, 125,
                       72, 84, 86, 140, 150,
                       10, 28, 78, 88, 108) 
ppd_id_smallPlans <- setdiff(ppd_id_all, ppd_id_largePlans)






#********************************************************************************                          
#                             Loading files                                  ####
#********************************************************************************

load("./Analysis/reportData_A1_return75.RData")
load("./Analysis/reportData_A1_planAssumption.RData")
load("./Analysis/reportData_A1_lowReturn15y.RData")
load("./Analysis/reportData_A1_highVol.RData")




#********************************************************************************                          
#                   Deterministic result for the entire PPD                  ####
#********************************************************************************

results_det <- bind_rows(reportData_list_A1_return75$results_det,
                         reportData_list_A1_lowReturn15y$results_det,
                         reportData_list_A1_highVol$results_det)

df_fig_det <- 
results_det %>% filter(ppd_id == 0, returnScn != "highVol", year >=2017, year <= 2046) %>% 
  select(ppd_id, returnScn, year, AL, MA, AA, ERC, PR) %>% 
  mutate(FR_MA  = 100 * MA / AL, 
         ERC_PR = 100 * ERC / PR,
         returnScn = factor(returnScn, levels = c("return75", "lowReturn15y"),
                                       labels = c('Scenario 1\n"7.5 percent expected return"', 'Scenario 2\n"15 Years of Low Returns"'))) %>% 
  select(returnScn, year, FR_MA, ERC_PR, ERC) 
  

fig.title <- "Total market asset value \nas a percentage of total liability"
fig_FR_det <- 
df_fig_det %>% 
  ggplot(aes(x = year, y = FR_MA, color = returnScn, shape = returnScn)) +  
  geom_line() +
  geom_point(size = 2) +
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(50,100))  + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5),2046), name =NULL) + 
  scale_y_continuous(breaks = seq(0, 100, 10)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red, "black"),  name = NULL) + 
  scale_shape_manual(values = c(15, 16),  name = NULL) + 
  labs(title = fig.title,
       y = "Percent") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3),
         shape = guide_legend(keywidth = 1.5, keyheight = 3)) +
  theme_bw() + RIG.theme() +
  theme(legend.position="none") 
  

fig.title <- "Total employer contribution \nas a percentage of total payroll"
fig_ERC_PR_det <- 
df_fig_det %>% 
  ggplot(aes(x = year, y =ERC_PR, color = returnScn, shape = returnScn)) + 
  geom_line() +
  geom_point(size = 2) + 
  coord_cartesian(ylim = c(0,30))  + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5),2046), name =NULL) + 
  scale_y_continuous(breaks = seq(0, 100, 5)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red),  name = NULL) + 
  scale_shape_manual(values = c(15, 16),  name = NULL) + 
  labs(title = fig.title,
       y = "Percent") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3),
         shape = guide_legend(keywidth = 1.5, keyheight = 3)) +
  theme_bw() + RIG.theme() 




fig.title <- grid.text("Overall funded ratio and employer contribution rate for 170 large public pension plans\nunder different investment return scenarios", 
                   just = "center",
                   x = 0.45)
fig_det <- grid.arrange(fig_FR_det, fig_ERC_PR_det, ncol = 2, widths = c(0.75, 1),
                        top = fig.title )
fig_det %>% grid.draw()

ggsave(fig_det, file = "./Analysis/Graphs_report/fig_det.png", width = 10, height = 5)

# df_fig_det %>% 
#   ggplot(aes(x = year, y =ERC, color = returnScn, shape = returnScn)) + 
#   geom_line() +
#   geom_point()


#********************************************************************************                          
#                            Stochastic results                              ####
#********************************************************************************


results_risk <- bind_rows(reportData_list_A1_return75$riskMeasure,
                          reportData_list_A1_lowReturn15y$riskMeasure) %>% 
                mutate(returnScn = factor(returnScn, levels = c("return75", "lowReturn15y"),
                                          labels = c('Scenario 1\n"7.5 percent expected return"', 'Scenario 2\n"15 Years of Low Returns"'))) %>% 
                filter(year <= 2046)
                    
                           #reportData_list_A1_highVol$results_det)

reportData_list_A1_planAssumption$results_det %>% filter(ppd_id == 150)

reportData_list_A1_return75$riskMeasure     %>% filter(ppd_id == 0, year <= 2046)
reportData_list_A1_lowReturn15y$riskMeasure %>% filter(ppd_id == 0, year <= 2046)

reportData_list_A1_lowReturn15y$riskMeasure   %>% filter(ppd_id == ppd_id_largePlans[13])

results_risk %>% filter(ppd_id == 0) %>% kable()



# Distribution of funded ratio 
fig.title    <- "Distribution across simulations of funded ratio for the aggregate of 170 large public pension plans"
fig.subtitle <- NULL
fig_FRdist <- results_risk %>% filter(ppd_id == 0) %>% 
  select(returnScn, year, FR.q25, FR.q50, FR.q75) %>% 
  gather(type, value, -returnScn, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25")),
             shape = factor(type, levels = c("FR.q75", "FR.q50", "FR.q25")))) + 
  theme_bw() + 
  facet_grid(.~returnScn) + 
  geom_line() + 
  geom_point(size = 2) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(40,160)) + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5),2046)) + 
  scale_y_continuous(breaks = seq(0, 500, 20)) + 
  scale_color_manual(values = c(RIG.green, RIG.blue, RIG.red),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(15, 16, 17, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Total market-asset value \nas a percentage of total liability (%)") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_FRdist
fig_FRdist$data

results_risk %>% filter(ppd_id == 0)



# Distribution of ERC as % Payroll
fig.title    <- "Distribution across simulations of employer contribution rate for the aggregate of 170 large public pension plans"
fig.subtitle <- NULL
fig_ERC_PR_dist <- results_risk %>% filter(ppd_id == 0) %>% 
  select(returnScn, year, ERC_PR.q25, ERC_PR.q50, ERC_PR.q75) %>% 
  gather(type, value, -returnScn, -year) %>% 
  ggplot(aes(x = year, y = value,
             color = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")),
             shape = factor(type, levels = c("ERC_PR.q75", "ERC_PR.q50", "ERC_PR.q25")))) + 
  theme_bw() + 
  facet_grid(.~returnScn) + 
  geom_line() + 
  geom_point(size = 1.5) + 
  geom_hline(yintercept = 100, linetype = 2, size = 1) +
  coord_cartesian(ylim = c(0,35)) + 
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_y_continuous(breaks = seq(0, 500, 5)) + 
  scale_color_manual(values = c(RIG.red, RIG.blue, RIG.green),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) + 
  scale_shape_manual(values = c(17, 16, 15, 18),  name = NULL, 
                     label  = c("75th percentile", "50th percentile", "25th percentile")) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Total employer contribution \nas a percentage of total payroll (%)") + 
  theme(axis.text.x = element_text(size = 8)) + 
  RIG.theme()
fig_ERC_PR_dist


# Risk of low funded ratio
fig.title <- "Probabilities of aggregate funded ratio \nof the 170 large public pension plans \nbelow 40% in any year up to the given year"
fig.subtitle <- NULL
fig_FR40less <-  results_risk %>% filter(ppd_id == 0) %>% 
  select(returnScn, year, FR40less) %>%
  gather(type, value, -returnScn, -year) %>% 
  # mutate(type = factor(type, levels = c("FR75less", "FR60less", "FR40less"), labels = c("75%","60%", "40%" ))) %>% 
  #mutate(FR40less.det = 0) %>% 
  #gather(variable, value, -year) %>% 
  ggplot(aes(x = year, y = value, color = returnScn, shape = returnScn)) + 
  # color = runname, shape = runname)) + 
  theme_bw() + 
  geom_point(size = 1.5) + 
  geom_line() + 
  coord_cartesian(ylim = c(0,25)) + 
  scale_y_continuous(breaks = seq(0,200, 5)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red, RIG.red),  name = NULL) + 
  scale_shape_manual(values = c(17,16, 15),  name = NULL) +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme()+
  theme(legend.position="none",
        title =element_text(size=9)) 
fig_FR40less
fig_FR40less$data %>% filter(year == 2046)



# Risk of sharp increase in ERC/PR
fig.title <- "Probability of aggregate employer contribution \nof the 170 large public pension plans rising by more than 10% \nof total payroll in a 5-year period up to the given year"
fig.subtitle <- NULL
fig_ERChike <- results_risk %>% filter(ppd_id == 0) %>% 
  #mutate(runname = factor(runname, labels = c(lab_s1, lab_s2))) %>%  
  select(returnScn, year, ERC_hike) %>% 
  #mutate(ERChike.det = 0) %>% 
  #gather(variable, value, - year) %>% 
  ggplot(aes(x = year, y = ERC_hike, color = returnScn, shape = returnScn)) + theme_bw() + 
  geom_point(size = 1.5) + 
  geom_line() + 
  coord_cartesian(ylim = c(0,40)) + 
  scale_y_continuous(breaks = seq(0,200, 10)) +
  scale_x_continuous(breaks = c(2017, seq(2020, 2040, 5), 2046)) + 
  scale_color_manual(values = c(RIG.blue, RIG.red, RIG.green, RIG.purple),  name = "") + 
  scale_shape_manual(values = c(17,16, 15, 18, 19),  name = "") +
  labs(title = fig.title,
       subtitle = fig.subtitle,
       x = NULL, y = "Probability (%)") + 
  guides(color = guide_legend(keywidth = 1.5, keyheight = 3))+
  RIG.theme() +
  theme(title =element_text(size=9))
fig_ERChike
fig_ERChike$data %>% filter(year == 2046)


fig_2riskMeasures <- grid.arrange(fig_FR40less, fig_ERChike, ncol = 2, widths = c(0.64, 1))
fig_2riskMeasures %>% grid.draw()





ggsave("./Analysis/Graphs_report/fig_FRdist.png", fig_FRdist, width = 10, height = 5)
ggsave("./Analysis/Graphs_report/fig_ERC_PRdist.png", fig_ERC_PR_dist, width = 10, height = 5)

ggsave("./Analysis/Graphs_report/fig_FR40less.png", fig_FR40less, width = 7*1.2, height = 5*1.2)
ggsave("./Analysis/Graphs_report/fig_ERChike.png",  fig_ERChike,  width = 7*1.2, height = 5*1.2)


ggsave(fig_2riskMeasures , file = "./Analysis/Graphs_report/fig_2riskMeasures.png", width = 11*0.9, height = 5*0.9)




# x <- reportData_list_A1_return75$results_det %>% filter(B<0)



