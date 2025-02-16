##Creating a model of poverty persistence -----
# setwd("~/VU/Thesis")
# 
# library(haven)
# library(tidyverse)
# library(dplyr)
# FD <- read_dta("Kenya_FD_materials/FD_Kyle.dta")
#Poverty persistence measures the share of people who continue to be in poverty from the initial observation
#

##Approach ======
#First, we'll make a poverty line, for starters 50% of median aggregate expenditures per capita across all months
#Second, we will create an indicator of an individual household being in poverty for each month
# This might involve making a month variable, oh god. I might just do it in multiples of 4
#We can then graph the share in persistent poverty each month 

#Extension =======
#There is a strata variable in the data, 1 to 12, which we could use, though I don't know what it measures
#

#Subsetting ========
#What I need:
# time
#   by month
# expenditures
#   FD$InOutTransaction = 2
# unique(FD$InOutTransaction) shows negative cash flows, I assume expenditures
#   FD$CashFlow
# number of people
# I have hhsize, but not sure how to weight children or identify them, for now we'll do 1 to 1
#

##Time======
#Weeknr stats at 13 to 52, 40 weeks

date <- lubridate::ymd("2019-03-05") + lubridate::weeks(FD$WeekNr-13)

benchdate <- lubridate::ymd("2019-01-01") + lubridate::weeks(FD$WeekNr-13)

# ymd value to start of FD collection, I set it to March 5

FD$monr <- lubridate::month(date)

FD$monr_bench <- lubridate::month(benchdate)
#F YEAH, bench is useful as it orders things by observation 

datecheck <- FD %>% select(WhenTransaction, WeekNr, monr, monr_bench)

#Expenditures======
# FD$absCF <- abs(FD$CashFlow)
#Probably wont be used as we have AmountCash, AmountCredit, AmountMpesa, AmountInKind, AmountBank, AmountAdvance
#this creates a col which only has positive numbers

FDSpend <- FD %>%  
  filter(FD$InOutTransaction == 2)

#I think this is a thing for all outgoing transactions, confirmed Section 11

FDSpend$Expenditure = FDSpend$AmountCash + FDSpend$AmountCredit + 
  FDSpend$AmountMpesa + FDSpend$AmountInKind + FDSpend$AmountBank + FDSpend$AmountAdvance

#Confirmed works

FDSpend <- FDSpend %>%  select(hhid, strata, Expenditure ,WeekNr, monr, monr_bench)

#Now to do monthly expenditure
FDSpend <-  FDSpend %>% 
  group_by(hhid, monr_bench) %>%
  mutate(moExp = sum(Expenditure))

#w_FD is Sampling weight FD, not sure if I'll use it

#Population======
#In taking from the Codes, I created an excel file with the counts of hh population and the membership
#The first row is converted hhid from raw Code data
library(readxl)
hhpop <- read_excel("Kenya_FD_materials/household members.xlsx", 
                      + col_types = c("numeric", "numeric", "text", 
                                          + "numeric", "text", "text", "text", 
                                          + "text", "text", "text", "text", "text", 
                                          + "text", "text", "text", "text", "text", 
                                          + "text", "text", "text", "text", "text", 
                                          + "text", "numeric"))

hhpop$hhid <-  hhpop$id

hhcount <- hhpop %>% select(hhid,count)
#Now this is imported we will merge it with a smaller FD dataset using hhid as the reference point
#There are missing hhids in both 117 and 96, 96 still has members assigned, but neither have transactions
# 
# class(FDSpend$hhid)
# class(hhpop$id)
# 
# class(FDSpend)
# class(hhpop)

FDSpend = left_join(x = FDSpend, y = hhcount, by = "hhid")

#Per Capita Expenditure in Month====
  #Then to get per capita expenditure by dividing by hhsize (not weighted) per household ID, hhid

FDSpend = FDSpend %>% group_by(hhid, monr_bench) %>% 
  mutate(perCapE = moExp/count)


#Simple Visualization ====

library("ggplot2")
library("ggthemes")

p.mexp <- ggplot(data = FDSpend, aes(x = FDSpend$monr_bench, y = log(FDSpend$perCapE), group = hhid, color = factor(hhid))) + 
  geom_line()  +
  theme(legend.position="none") +
  labs(y= "Log Monthly Expenditure per Person", x = "Month of Study") +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks()) +
  xlim(1, 9)

p.mexp

#Model ------
#Relative Poverty Level====
#We will set an initial median value to compare against, of those which fall under the value, we mark as impovrished
FD_MoExp <- select(FDSpend, !c(Expenditure, WeekNr, monr, strata)) 
FD_MoExp <- distinct(FD_MoExp)
FD_MoExp <- FD_MoExp[!is.na(FD_MoExp$perCapE), ]
#We remove all duplicate values of monthly per cap expenditures and no value rows

# FD_MoExp = group_by(FD_MoExp, monr_bench) %>% 
#   mutate(median = weighted.median(x = perCapE, w = count, na.rm = TRUE))

detach(package:plyr)

MoMetrics <- FD_MoExp %>%  group_by(monr_bench) %>% 
  summarise(mean = weighted.mean(df,x = perCapE, w = count, na.rm = TRUE),
            median = median(x = perCapE),
            wmedian = weighted.median(x = perCapE, w = count,  na.rm = TRUE),
            wq1 = weighted.quantile(x = perCapE, w = count, probs = .25,  na.rm = TRUE),
            wq3 = weighted.quantile(x = perCapE, w = count, probs = .75,  na.rm = TRUE),
            sample = sum(count))

MoMetrics$relpov <- MoMetrics$wmedian*0.5

FD_MoExp = left_join(x = FD_MoExp, y = MoMetrics, by = "monr_bench")
#joining MoMetrics to monthly FD, now we can compare within a single df to produce a poverty dummy

FD_MoExp$poverty <- ifelse(FD_MoExp$perCapE < FD_MoExp$relpov, 1, 0)

FD_MoExp$povcount <- FD_MoExp$poverty*FD_MoExp$count

MoPoverty <- FD_MoExp %>%  group_by(monr_bench) %>% 
  summarise(povcount = sum(povcount))

MoMetrics = left_join(x = MoMetrics, y = MoPoverty, by = "monr_bench")
MoMetrics$povshare <- MoMetrics$povcount/MoMetrics$sample


#designates relative poverty line per month as half of median expenditure

#Poverty ====
#Evaluate pce of hhid in initial month

#group of hhid in poverty initially
initpoor <- FD_MoExp %>% 
  filter(monr_bench == 1) %>% 
  filter(poverty == 1)
initpoor$initialpov = 1 


#adding the hhids back in with an identifier for initial poverty
initpoor<- initpoor %>% ungroup()
initpoor <- initpoor %>% select(hhid,initialpov)
FD_MoExp <- left_join(x = FD_MoExp, y = initpoor, by = "hhid")

FD_MoExp$initialpov[is.na(FD_MoExp$initialpov)] <- 0

#new df with sum of those in initial poverty each month
MoInitPov <- FD_MoExp %>%  group_by(monr_bench) %>% 
  summarise(initpovcount = sum(initialpov*povcount))

#Getting share of initial poverty group by sample 

MoMetrics = left_join(x = MoMetrics, y = MoInitPov, by = "monr_bench")
MoMetrics$initpovshare <- MoMetrics$initpovcount/MoMetrics$sample

#New vars for each period
# test <- FD_MoExp %>% group_by(monr_bench) %>% 
#   summarise()

#Visualizaion----
#poverty persistence of initial====
p.persistence <- ggplot(data = MoMetrics, aes(x = monr_bench, 
                                              y = initpovshare)) + 
  geom_line()  +
  theme(legend.position="none") +
  labs(y= "Share of Sample in Poverty", x = "Month of Study") +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks())
p.persistence

#povety share ====
p.povertyshare <- ggplot(data = MoMetrics, aes(x = MoMetrics$monr_bench, 
                                               y = MoMetrics$povshare)) + 
  geom_line()  +
  theme(legend.position="none") +
  labs(y= "Share in Poverty", x = "Month of Study") +
  scale_y_continuous(breaks = pretty_breaks()) +
  scale_x_continuous(breaks = pretty_breaks())
p.povertyshare

#Refactoring, An actual Persistence Model ----
#Persistence model means identifying households as they move forward
#Sorting and grouping
detach(package:plyr)
FD_Mo <- FD_MoExp %>% group_by(hhid)
FD_Mo <- FD_Mo[order(FD_Mo$monr_bench),]
FD_Mo <- FD_Mo[order(FD_Mo$hhid),]

# install.packages("RcppRoll")

library("RcppRoll")
detach(package:plyr)
FD_Mo <- FD_MoExp %>% group_by(hhid)
FD_Mo$persist <- RcppRoll::roll_prod(x = FD_Mo$poverty, align = "right", n = 1, fill = 0, na.rm = FALSE)

#seeing number of months tracked by hhid
hh <- FD_Mo %>%  group_by(hhid) %>% 
  tally()
#transition function has to accomodate the variations, not all end at 10 

            # FD_Mo <- FD_Mo %>% group_by(hhid) %>% 
#   while (FD_MoExp$poverty == 1) {
#   mutate(FD_Mo$persistence = 1)
#     }
