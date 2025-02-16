#set up ----
# imma r now, should it be a markdown? probably, but oh well
setwd("~/VU/Thesis")

library(haven)
library(tidyverse)
library(dplyr)
FD <- read_dta("Kenya_FD_materials/FD_Kyle.dta")
# blazin' start

# let's see what we got
nrow(FD)
# 57697, daaaang
ncol(FD) 
# 103, nooooo, let's look into collapsing some of those columns with some grouping later. Ha, not gonna happen
glimpse(FD)
# oh no
summary(FD)
summary(FD$RespondentHHkey)
summary(FD$CashFlow)
FD$AbsCashflow = abs(FD$CashFlow)
summary(FD$AbsCashflow)
#it seems like hhid is a truncated RespondentHHkey, both are 122 

# key cols; hhid, strata, Respond ID, weekNr, some sort of CSA paricipation
# indicators TASK read codebook, many variables probably learn how to melt and
# forge again using dplyr. Do breakdowns by household, transaction type by weeks
# item id tracking is pretty good, lots of object types, who knew

sum(is.na(FD$TransactionTypeID))
sum(is.na(FD$hhid))
sum(is.na(FD$RespondentHHkey))
sum(is.na(FD$WeekNr))
# all of these are fully complete cols

#ideas ----
# plan of action to select relevant data and organize it in a manner which can be flexible.
  # for id we will keep  hhid, cluster, and grouping by different CSV variables. 
  # we'll characterie those with the age and gender variables in order to perform regression specification
  # weeknr variable will be used to create lagged models and possibly creating a broader seasonal category
# TASK ask remco about the seasonal conditions if this is pursued, might be a more novel thing to do. 
# might be cool to create new groupings by habits, or some sort of characterizing metric. 
# visualization through tools like leaflet would be a nice addition and concrete example of the use of the FDs
  # also it would be a shiny thing to show people. 
# TASK look into possible "profile" visualization methods. Create a moving picture of individuals, not blobs
# I WISH I had end-to-end data on these flows, would be a pain to code though because I don't know how it works
  # A dendrogram might be cool, showing the breakdown of instrument use, splits
  # A heat map, no clue how but activation of certain tools by cluster?
  # Stacked circular barplot
  # edge bundling 
# If there is any sort of geographical data
  # dynamic chloropleth map, even townships would do, or I'd otherwise randomize a location within a given area
    # is that easier said than done, yuuuuuuup.
# I should've used a freakin' text doc if I was going to write this much. "is this documentation?"


sum(is.na(FD$AmountCash))

#plotting----
# leeeeets plot things without knowing what we're doing
library(ggplot2)
p.hhwkcash = ggplot(data=FD, aes(x=FD$WeekNr, y=FD$CashFlow, group=FD$hhid)) +
  geom_point(aes(color= hhid)) +
  theme(legend.position="none")
#here we're taking cash flow by week per household key, color is done by hhid

p.hhwkcash
#I should probably find out the difference with these HH id things (RespondentHHkey)
#Our sick plot is  sick with some ailment by the looks of it.
#The stuff is panel data, making the plotting all funky coming out in lines

# I might try doing this on a household basis, which would probably leave me with:
unique(FD$hhid)
# 122, yay, don't know what I expected
# Might be worthwhile to visualize the characteristics of these households, by age, and whatnot, and # of obs.
  # I'm tired, there's literally 3 lines of comments per line of code.
  # Heck I could even show which surveyers did the best, by week
#cashflow====
#Let's do a breakdown of what most transactions are. 
#According to the preliminary graphing we've done, the spread of transactions seems to lie within a small range
summary(FD$CashFlow)
p.cfbox = ggplot(data=FD, aes(x=FD$WeekNr,y=CashFlow, group=FD$WeekNr)) +
  geom_boxplot() 

p.cfbox
#There's a problem here, there are quite large outliers, so let's find a way to
#narrow the range on both sides
cfrank = FD %>% 
  select(WeekNr,CashFlow,hhid,strata,TransactionTypeID,CashFlow) %>% 
  arrange(desc(CashFlow))
#largest types are 13, 6, 7. "Sales"

# distribution #####


#So let's graph a distribution of all financial transactions by amount
#what I basically want is the preivous graph tipped over to the left and all the dots piling on each other
p.cfdist = ggplot(data=FD, aes(x=CashFlow, group=hhid)) +
  geom_histogram(aes(color= hhid),binwidth=1000) +
  theme(legend.position="none") +
  xlim(c(-5000, 5000))
p.cfdist = ggplot(data=FD, aes(x=CashFlow, group=hhid)) +
  geom_point(aes(color= hhid),binwidth=1000) +
  theme(legend.position="none") +
  xlim(c(-5000, 5000))
p.cfdist
#this is pretty impractical as there are huge ouliers in the data, probably large purchases of buildings etc.

p.lcfdist = ggplot(data=FD, aes(x=log(CashFlow), group=hhid)) +
  geom_histogram(aes(color= hhid),binwidth=1) +
  theme(legend.position="none")
p.lcfdist
#ln(CF) removes all negative values, not representative of sample

p.lcfdist = ggplot(data=FD, aes(x=log(abs(CashFlow)), group=hhid)) +
  geom_histogram(aes(color= hhid),binwidth=1) +
  theme(legend.position="none")
p.lcfdist
#here we make all values absolute and then ln. So what we're effectively looking
#at is the volume of transaction


