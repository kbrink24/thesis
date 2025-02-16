setwd("~/VU/Thesis")
library(haven)
library(tidyverse)
#FD <- read_dta("Kenya_FD_materials/FD_Kyle.dta")

#In this script the objective is to create a model where we look at possible differences in cashflow between
#CSA participation and household characteristics

#taking from the glimpse

# p.lcfdist = ggplot(data=FD, aes(x=log(abs(CashFlow)), group=hhid)) +
#   geom_histogram(aes(color= hhid),binwidth=1) +
#   theme(legend.position="none")

p.lcfdist
#this shows the overall distribution of cashflows, now we differentiate between csa participation

p.lcfdist = ggplot(data=FD, aes(x=log(abs(CashFlow)), group=hhid)) +
  geom_histogram(aes(color= hhid),binwidth=1) +
  theme(legend.position="none")

#let's overlay the two groups over one another to see if there are significant visual differences in the skew


#This is not adjusted for sample size, so we'll run a regression comparing the cashflow size per group
#r.cfcsat = lm
#r.cfcsaf = lm
#not sure if it has to be two regressions, I think it does

#let's plot these regressions
#p.cfcsa = ggplot 

# as an extension, I'd like to look into controlling for household characteristics
# this includes, hhsize, gender, and uhhhhh
