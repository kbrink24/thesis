#Creating a model of poverty persistence -----
setwd("~/VU/Thesis")

library(haven)
library(tidyverse)
library(dplyr)

library(bookdown)

# S8A_hh_income ======

S8A_hh_income <- read_dta("Kenya_FD_materials/BaselineSurvey/S8A_hh_income.dta")

# Household income sources, descriptions taken from "Baseline survey correct names"
# Kindly explain the major sources of income are for the household. 
# Could you please provide an estimate of the contribution of different household members in total household income in the past 12 months
# Main income of the household S8Aq1
# Husband  S8Aq2
# Wife         S8Aq3
# Children  S8Aq4
# Other       S8Aq5
# Specify    S8Aq6

income_source <- S8A_hh_income %>% select(hhid, starts_with("S8Aq")) %>% 
  rename(Husband = S8Aq2,
         Wife = S8Aq3,
         Children = S8Aq4,
         Other = S8Aq5,
         Specify = S8Aq6)
library(tidyr)
income_source <- income_source %>% drop_na(hhid)

# For Aq1, Likely who does most work
# 1=adult males in household,
# 2=adult females in household,
# 3=male children in household,
# 4=female children in household,
# 5= male/female children in household,
# 6=hired labor,
# -66=other, specify

print(unique(S8A_hh_income$S8Aq1))
print(unique(S8A_hh_income$S8Aq6))
print(unique(S8A_hh_income$S8Aq5))

# S2_hh_demography  ====
#descriptions taken from "Baseline survey correct names"

S2_hh_demography <- read_dta("Kenya_FD_materials/BaselineSurvey/S2_hh_demography.dta")

# Gender: Male=1 / Female=0S2Aq2
# Age (years) S2Aq3
# Highest level of education attained (code A)S2Aq4
# Primary occupation (code B)S2Aq5
# Secondary occupation (code B)S2Aq6
# Labor contribution to occupations (code C)[prim]S2Aq7
# How many months has [name] been in location in past year? S2Aq8	Relation to HH   (code D) S2Aq9
# Marital status (>12yrs)(code E)S2Aq10

# Codes A: 1 = No formal schooling, 2 = Primary incomplete, 3 = Primary complete,
# 4 = Secondary incomplete, 5 = Secondary complete, 6 = Tertiary /university incomplete,
# 7 = tertiary/University complete, 8 =Adult education incomplete, 9 = Adult education complete, 10 = Don’t know

# Codes B: 1=farming crop, 2=farming livestock, 3=salaried employment,
# 4=self-employed off-farm, 5=casual laborer on-farm, 6=casual labored off-farm,
# 7=school/college, 8=non-school child, 9=herding, 10-household chores, 11=other, specify

# Codes C: 1=full time, 2=part time, 3=occasionally, 4=not a worker

# Codes D: 1= Household head, 2= Spouse, 3= Son/daughter, 4= Parent living with son/daughter,
# 5= Son/daughter in-law, 6=Grandchild, 7= other relative, 8= Hired worker 9= Other, specify_

# Codes E: 0=single, 1=married, 2=widowed, 3=divorced, 4=other, specify

# These codes will be used to isolate the sample to CSA-able income generating activities assigned to hhids

length(unique(hh_demography$hhid))
#122 unique households, there are 120 hhids in the entire full set.

# S3A_crop_prod ======

S3A_crop_prod <- read_dta("Kenya_FD_materials/BaselineSurvey/S3A_crop_prod.dta")
crop_prod <- S3A_crop_prod %>% 
  select(hhid, S3Aq1, S3Aq4, S3Aq5, S3Aq8) %>% 
  rename(plotnumber = S3Aq1,
         crop = S3Aq4,
         cropsize = S3Aq5,
         mainworker = S3Aq8) %>% 
  group_by(hhid) %>% 
  mutate(totalplot = sum(cropsize))


# farmers <- crop_prod %>% filter(totalplot > 0)
# length(unique(farmers$hhid))

# 122 farming households, ALL HHIDS are farmers
# mainworker is "Who does the most work", like S8 income source
# 1=adult males in household,
# 2=adult females in household,
# 3=male children in household,
# 4=female children in household,
# 5= male/female children in household,
# 6=hired labor,
# -66=other, specify

#CSA Participation -----

FD <- read_dta("Kenya_FD_materials/Full FD_Kyle/FD_Kyle.dta")
colnames(FD)
length(unique(FD$hhid))

Participation <- FD %>% select(hhid, strata, CSV, WeekNr, CSAWhat, CSAQuantity, CSAUnit, FarmProductType, FarmProductQuantity, FarmProductUnitID)

unique(CSAPart$CSAWhat)
class(Participation$CSAWhat)

CSAPart <- Participation %>% filter(CSAWhat > 0)
CSAPart$CSA <- 1

CSAPart <- CSAPart %>% select(hhid, CSA)

Participation <- left_join(x = Participation, y = CSAPart, by = "hhid")

Participation$CSA[is.na(Participation$CSA)] <- 0

table(Participation$CSA, Participation$CSV)

Participation <- Participation %>% group_by(hhid) %>% 
  mutate(totalPart = sum(CSA), )

Participation <- distinct(Participation)
hhPart <-  Participation %>% select(hhid, CSV, CSA)
hhPart <- distinct(hhPart)

table(hhPart$CSV)

table(hhPart$CSA)

table(hhPart$CSV, hhPart$CSA)
# 33 non CSV, 87 CSV
# 45 non CSA, 75 CSA
# 15 non CSV, non CSA
# 18 non CSV, CSA
# 30 CSV, non CSA
# 57 CSV, CSA

# let's create some group variables for each, make it easier later
hhPart<- hhPart %>% mutate(partGroup= CSV + 2*CSA)
partGroup.names <- c("non-CSV, non-CSA", "CSV, non-CSA", "non-CSV, CSA", "CSV, CSA")
partGroup.numbers <- 0:3

names(partGroup.names) <- partGroup.numbers

partGroup.index <- data.frame(partGroup.numbers, partGroup.names)

# Variables=====
# FARMPROD	'farm product'
# FARMQ		'quantity of farm product' number of unit
# FARMUN		'farm product unit' so acres, kilograms, meters etc
# CSAWHAT		'what type of CSA'
# CSAQ		'how much CSA'
# CSAUN		'Unit of CSA'
# CSACOST		'Costs of CSA'
# PURBUS		'Purchases made for business?'
# SALBUS		'Sales made for business?'

# CSAUnit is 29 unique

# CSA practices Most likely under CSAWhat
# /
#   CS01	"High yielding varieties"
# CS02	"Improved seeds"
# CS03	"Early maturing varieties"
# CS04	"Drough tolerant varieties"
# CS05	"Pest and disease tolerant varieties"
# CS06	"Cross breeding Galla goat"
# CS07	"Cross breeding Red Masai sheep"
# CS08	"Fodder storage"
# CS09	"Intercropping"
# CS10	"Crop cover"
# CS11	"Micro catchment"
# CS12	"Ridges and bounds"
# CS13	"Hedges"
# CS14	"Fertilizer"
# CS15	"Manure and compost"
# CS16	"Mango trees planted"
# CS17	"Other trees planted"
# CS18    "Improved poultry, rainbow or croilers"
# CS24	"Other activities"

# CSAWhat is likely what we want to use, 71780 rows including NA and 16 other unique variables on CSA practice

# Idenitifying households----
# CSV is bianary variable on if a hhid is in a CSV village
# So CSA participating hhids can be idenified by taking HHIDs with CSAWhat which are not NA.
# From there I can say the degree of CSA participation of a household.
# This allows for identifying the three groups of non-CSV, CSV CSAers, CSV non-CSAers. And if any, non-CSV CSAers


# Income Subsetting----
unique(FD$InOutTransaction)
idk <- FD %>% filter(InOutTransaction == 3)
#only 3 rows
idk <- FD %>% filter(InOutTransaction ==99)
#1
idk <- FD %>% filter(InOutTransaction == '')
#lots, 
idk <- FD %>% filter(InOutTransaction == 2)
#lots, spending


Income <- FD %>% select(hhid, WeekNr, strata, CSV, 
                        InOutTransaction, TransactionTypeID, 
                        TransactionKey, CashFlow, 
                        CashFlowCorrection,
                        AmountCash, AmountCredit, AmountMpesa, 
                        AmountInKind, AmountGift, AmountBank, 
                        AmountAdvance, AmountShopDeposit, 
                        AmountLaborSharing, LoanCreditType, 
                        SavingsType, FarmProductType, 
                        CSAWhat, FarmProductWhatID,
                        SavingsCurrentBalance,
                        HHMgender,HHMage)
# idk <- FD %>% filter(FD$InOutTransaction == 3) 
# #apparently 3 is advances paid
Income <- Income %>% filter(InOutTransaction != 2)
#removing spending

Income <- Income %>% mutate_at(vars(AmountCash:AmountLaborSharing), ~replace(., is.na(.), 0))
#fixing NAs to 0 so they can be added together

Income <- Income %>% 
  mutate(total = AmountCash + AmountCredit+ AmountMpesa + AmountInKind + AmountBank +
           AmountAdvance + AmountShopDeposit + AmountLaborSharing)

# Income <- Income %>% filter(FD$InOutTransaction == 1) This is a weird one, blanks include income to hh
Income <- Income %>% select(-InOutTransaction)

# negatives <- Income %>% filter(Income$CashFlow < 0)
# print(nrow(negatives))
#57 
# print(unique(Income$TransactionTypeID))
#13, 11, 5

# For now, we'll remove them
Income <- Income %>% filter(Income$total > 0)

#explorin'
# unique(Income$TransactionTypeID)
#only "13"


#Creating time variables and monthly income metrics----
Income$date <- lubridate::ymd("2019-03-05") + lubridate::weeks(Income$WeekNr)

#This gives the full date of each transaction based on the start date March 5th and given week number
#When it comes to plotting, it should be done with full date format to make sure the breaks are at the correct place
Income$dateFloor <- lubridate::floor_date(Income$date, unit = "month")
Income$month <- lubridate::month(Income$date)
Income$year <- lubridate::year(Income$date)
#gives month set at the first and year
#Here we will also put transactions within their given months, 


###INCOMPLETE

# Monthly income per household====

Income <-  Income %>% 
  group_by(hhid, dateFloor) %>%
  mutate(moInc = sum(total))

Income <- Income %>% select(hhid, dateFloor, moInc, strata)

Income <- distinct(Income)

Income <- Income %>% group_by(hhid) %>% arrange(dateFloor, .by_group = TRUE)

ungroup(Income)

# idk <- Income %>% count(hhid)
# unique(idk$n)
# idk <-Income %>% filter(CashFlow>0)
# idk <- Income %>% filter(total > 0)
# nrow(idk)
# testing, works!
# hhid1 <- Income %>% filter(hhid == 1) %>% 
#   select(month, year, hhid, moInc)

#number of observed months of each hhid
Income <- Income %>% 
  group_by(hhid) %>%
  mutate(obs = length((unique(dateFloor))))

#Mean monthly income of each household====
Income <- Income %>% 
  group_by(hhid) %>%
  mutate(mmoInc = sum(moInc) /obs)

#Standard deviation====
Income <- Income %>% 
  group_by(hhid) %>% 
  mutate(sdmoInc = sd(moInc))

#CV=====
Income <- Income %>% 
  group_by(hhid) %>% 
  mutate(CV = sdmoInc/mmoInc)



# length(unique(Income$mmoInc)), 120 checks out

#Non-transaction data, contains all CSA indicators for some reason----
# summary(Income)
# 
# notinout <- FD %>% filter(InOutTransaction == '') 
# 
# nrow(notinout)
# unique(notinout$CSAWhat)
#contains all practices, a blank field in INout is likely to just be a tracker, but there is income...

# unique(notinout$TransactionTypeID)
#10, 7, 6, 4, 1, 12


# Loan type ====
# unique(FD$LoanCreditType)
# length(unique(FD$LoanCreditType))
# LoanCreditType variable
# TYPS    loan types
# TY01	"loan repayment received"
# TY02	"credit repayment received"
# TY03	"money borrowed"
# TY04	"gift or remittances received"
# TY05	"advance payment received"
# TY06	"CBO share received"
# TY07	"loan repayment paid"
# TY08	"credit repayment paid"
# TY09	"money lent"
# TY10	"gift or remittances given"
# TY11	"advance paymetn paid"
# TY12	"CBO contribution paid"


# Bringing partcipation and income variables together----
Income <- left_join(x = Income, y = hhPart, by = "hhid")

Incomplete <- Income %>% filter(obs < 9)
#I'll be cutting these incomplete households as they may miss seasonal variations

Income <- Income %>% filter(obs > 8)
#I want at least 3/4 of a year

# length(unique(Income$hhid)) we are left with 118 households

hhchars <- Income %>% select(hhid, mmoInc, sdmoInc, CV, CSV, CSA, partGroup, povcount)
hhchars <- distinct(hhchars)
#now we only have one row per hhid

#Quintile creation -----

ungroup(hhchars)
hhchars$quintile <- ntile(hhchars$mmoInc, 5)

table(hhchars$quintile)

q <- quantile(hhchars$mmoInc, probs = c(.2, .4, 0.6, .8, 1))

breaks <- data.frame(q)


# hhchars %>% filter(mmoInc < q[1]) %>% nrow()
# hhchars %>% filter(mmoInc > q[1] & mmoInc < q[2]) %>% nrow()
# hhchars %>% filter(mmoInc > q[2] & mmoInc < q[3]) %>% nrow()
# hhchars %>% filter(mmoInc > q[3] & mmoInc < q[4]) %>% nrow()
# hhchars %>% filter(mmoInc > q[4]) %>% nrow()
#adds up, 118

# q[2] is the boundary between the second and third quintile, will be our poverty line

#Poverty line and count of months hhid is in poverty----
# q[2] is the boundary between the second and third quintile, will be our poverty line

Income <- Income %>% mutate(pov = ifelse(moInc < q[2], 1, 0))

#now count over months
Income <- Income %>% group_by(hhid) %>% mutate(povcount = sum(pov))

#Sometimes, often, always poor ====
unique(Income$povcount)
idk <- Income %>% filter(povcount == 0)
unique(idk$hhid)

###INCOMPLETE

#household metrics by csa=====
# 
# hhmet <- hhchars %>%  group_by(partGroup) %>% 
#   mutate(households = n(),
#          mCV = mean(CV),
#          gmmoInc = mean(mmoInc))
# hhmet <- hhmet %>% select(CSV:gmmoInc) 

#same can be done with

hhchars <- hhchars %>% group_by(partGroup)

pg.metrics <-  summarize(hhchars, mCV = mean(CV),
          gmmoInc = mean(mmoInc),
          households = n())


hhchars <- hhchars %>% group_by(quintile)

q.metrics <- summarize(hhchars, mCV = mean(CV),
          gmmoInc = mean(mmoInc),
          households = n())


hhchars <- hhchars %>% group_by(quintile, partGroup)

c.metrics <- summarize(hhchars, mCV = mean(CV),
          gmmoInc = mean(mmoInc),
          households = n())


# 
trunk <- hhchars %>% ungroup() %>%  select(hhid, quintile)

Income <- 
  left_join(Income, trunk, by = "hhid")

Income %>% group_by(quintile)

summarise(Income, minc = mean(moInc))




#Plots and Tables----

#Table ====

library(ggthemes)
library(xtable)

table(Income$partGroup, Income$povcount)


table(Income$partGroup, Income$pov)

t.cvpq <- hhchars %>%
  group_by(partGroup, quintile) %>%
  summarise(mCV = mean(CV), gmmoInc = mean(mmoInc), n = n())
#same as c .metrics
table()

library(ggplot2)

#Number of households by number of months spent under pov line
# p.pfcsap <- ggplot(hhchars, aes(x = povcount, fill = as.character(partGroup))) +
#   geom_bar(position = "stack") +
#   theme_linedraw() +
#   xlab("Number of Months Under Poverty Line") +
#   ylab("Number of Households") + 
#   theme(legend.title = element_text(size=10, face="bold")) +
#   scale_fill_discrete(name = "Participation", labels = partGroup.names) 

# Poverty count by participation group====
p.pfcsap <- ggplot(hhchars, aes(x = povcount, fill = as.character(partGroup))) +
  geom_bar() +
  facet_wrap(~partGroup, labeller = labeller(partGroup = partGroup.names)) +
  theme_fivethirtyeight() +
  xlab("Number of Months Under Poverty Line") +
  ylab("Number of Households") + 
  theme(legend.title = element_text(size=10, face="bold")) +
  scale_fill_discrete(name = "Participation", labels = partGroup.names) 

p.pfcsap

# participation group by initial income====
p.csapiinc <- Income %>% filter(dateFloor == "2019-03-01") %>% 
  ggplot(Income, mapping =  aes(x = CSA, y = log(moInc), group = CSA)) +
  geom_boxplot() 
  # theme_fivethirtyeight() 
  # xlab("Number of Months Under Poverty Line") +
  # ylab("Number of Households") + 
  # theme(legend.title = element_text(size=10, face="bold"))
# + scale_fill_discrete(name = "Participation", labels = partGroup.names) 

p.csapiinc

#Share of households by number of months spent under pov line, by CSA participation =====
# idk <- hhchars %>% group_by(partGroup) %>%  count(povcount)
# 
# idk <- idk %>% mutate(gfreq = sum(n))
# 
# 
# p.spfcsap <- ggplot(idk, aes(x = povcount, y = n/gfreq, fill = as.character(partGroup))) +
#   geom_bar(position = "stack") +
#   theme_linedraw() +
#   xlab("Number of Months Under Poverty Line") +
#   ylab("Number of Households") + 
#   theme(legend.title = element_text(size=10, face="bold")) +
#   scale_fill_discrete(name = "Participation", labels = partGroup.names) 
# 
# p.spfcsap

p.spfcsap <- ggplot(hhchars, aes(povcount, group = partGroup)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme_fivethirtyeight() +
  scale_y_continuous(labels=scales::percent) +
  facet_grid(~partGroup, labeller = labeller(partGroup = partGroup.names)) +
  labs(y = "Relative Frequency", x = "Months in Poverty", fill = "Months", 
       title = "Duration in Poverty by Share of Houseolds, by CSA Participation") 
p.spfcsap

#By only CSA
p.spfcsa <- ggplot(hhchars, aes(povcount, group = CSA)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme_fivethirtyeight() +
  scale_y_continuous(labels=scales::percent) +
  facet_grid(~CSA
             # , labeller = labeller(CSA = partGroup.names)
             ) +
  labs(y = "Relative Frequency", x = "Months in Poverty", fill = "Months", 
       title = "Duration in Poverty by Share of Houseolds, by CSA") 
p.spfcsa

#By only CSV
p.spfcsv <- ggplot(hhchars, aes(povcount, group = CSV)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  theme_fivethirtyeight() +
  scale_y_continuous(labels=scales::percent) +
  facet_grid(~CSV
             # , labeller = labeller(CSV = partGroup.names)
  ) +
  labs(y = "Relative Frequency", x = "Months in Poverty", fill = "Months", 
       title = "Duration in Poverty by Share of Houseolds, by CSV") 
p.spfcsv


#Share of households by number of months spent under pov line, by income quintile====

#create naming for quintiles

quintile.names <- c("First Quintile", "Second Quintile", "Third Quintile", "Fourth Quintile", "Fifth Quintile")
quintile.numbers <- 1:5

names(quintile.names) <- quintile.numbers

quintile.index <- data.frame(quintile.numbers, quintile.names)


p.spfq <- ggplot(hhchars, aes(povcount, group = quintile)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..), stat="count")) + 
  scale_y_continuous(labels=scales::percent) +
  theme_grey() +
  facet_grid(~quintile, labeller = labeller(quintile = quintile.names)) +
  labs(y = "Relative Frequency", x = "Months in Poverty", fill = "Months", 
       title = "Duration in Poverty by Share of Houseolds, by Income Quintile") 
p.spfq

#Gridding both CSA and Quintile====

p.spfcq <- ggplot(hhchars, aes(povcount)) + 
  geom_bar(aes(y = ..prop.., fill = factor(..x..), stat="count")) + 
  scale_y_continuous(labels=scales::percent) +
  theme_fivethirtyeight() +
  facet_grid(partGroup~quintile, labeller = labeller(quintile = quintile.names)) +
  labs(y = "Relative Frequency", x = "Months in Poverty", fill = "Months", 
       title = "Duration in Poverty by Share of Houseolds")
p.spfcq


# CV plots

#CV by quintile group
p.pcvq <- ggplot(c.metrics, aes(x = partGroup, y = mCV)) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_fivethirtyeight() +
  facet_grid(~quintile, labeller = labeller(quintile = quintile.names)) +
  labs(y = "Mean CV", x = "Participation", fill = "Months", 
       title = "Mean CV, by Income Quintile")
p.pcvq


#Cv by participation group
p.pcvp <- ggplot(c.metrics, aes(x = quintile, y = mCV )) + 
  geom_bar(position = "dodge", stat = "identity") +
  theme_fivethirtyeight() +
  facet_grid(~partGroup, labeller = labeller(partGroup = partGroup.names)) +
  labs(y = "CV", x = "Income Quintile", fill = "Months", 
       title = "Mean CV, by Participation") 
p.pcvp

#Non-grouped Cv, by quintile
p.cvq <- ggplot(c.metrics, aes(x = as.factor(quintile), y = mCV )) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_discrete(labels = quintile.names) +
  theme_fivethirtyeight() +
#   facet_grid(~partGroup, labeller = labeller(partGroup = partGroup.names)) +
  labs(y = "CV", x = "Income Quintile", fill = "Months", 
       title = "Mean CV, by Quintile") 
p.cvq


#Non-grouped Cv, by partic
p.cvp <- ggplot(c.metrics, aes(x = as.factor(partGroup), y = mCV )) + 
  geom_bar(position = "dodge", stat = "identity") +
  scale_x_discrete(labels = partGroup.names) +
  theme_fivethirtyeight() +
  labs(y = "CV", x = "Income Quintile", fill = "Months", 
       title = "Mean CV, by Participation Group") 
  
p.cvp
# p.idk <- ggplot(hhchars, aes(y= CV, x=quintile, fill=partGroup)) + 
#   geom_bar(position="dodge2", stat="identity")
#   
# p.idk

# p.pcv <-pg.metrics %>% ggplot(pg.metrics, aes(x = mCV, fill = as.character(partGroup))) +
#   geom_bar(position = "") +
#   theme_classic()

# library(treemap)
# 
# p.povcsap <- Income %>% treemap(index = "partGroup", vSize = "povcount", type = "depth")

