library(compute.es)
library(car)
library(ggplot2)
library(multcomp)
library(pastecs)
library(Hmisc)
library(polycor)
library(boot)
library(reshape2)
library(WRS2)
library(ez)
library(nlme)
library(MASS)
library(mvoutlier)
library(mvnormtest)
library(tidyverse)
library(effects)
library(survMisc)
library(GGally)
library(rms)
library(survival)
library(survminer)
library(psych)
library(dplyr)

##Goal: To run statistics (survival analysis) and create data visualizations for psychometric theory class project, completed in 2018 *
##Because project used proprietary data of minors involved in foster care, *
##code below has been redacted and edited to protect privacy. *
##Comments are sometimes added for clarity. 
##Lines that has been edited or added are marked with "*". *
##Some deleted lines which had proprietary information are unmarked.  *
##Deleted lines were usually results commented into the code and have been included in the final class paper. *
##Author reserves the right to use research idea for publication at a later time. * 
##Specific research idea is intellectual property of Tawny Spinelli, in collaboration with Dr. Neil Jordan. *
##Code is shared here to demonstrate proficiency in R stats coding and progress as a coder over time. *

#[n] children in residential placements in FY14-16 *

setwd("~/Desktop/[working directory]") *
UIRData <- read.csv("spellsUIRonlyFY14-FY16shareFinal.csv", stringsAsFactors = F)

UIRData$spell_opened <- as.Date(UIRData$spell_opened, format = "%m/%d/%y")
class(UIRData$spell_opened)

UIRData$spell_closed <- as.Date(UIRData$spell_closed, format = "%m/%d/%y")
class(UIRData$spell_closed)

UIRData$incident_date <- as.Date(UIRData$incident_date, format = "%m/%d/%y")
class(UIRData$incident_date)

UIRData$care_level <- as.factor(UIRData$care_level)
class(UIRData$care_level)

UIRData$party_sex <- as.factor(UIRData$party_sex)
class(UIRData$party_sex)

UIRData$party_race <- as.factor(UIRData$party_race)
class(UIRData$party_race)

UIRData$provider_id <- as.factor(UIRData$provider_id)
class(UIRData$provider_id)

UIRData$ui_type_code <- as.factor(UIRData$ui_type_code)
class(UIRData$ui_type_code)

UIRData$UIR_Type <- as.factor(UIRData$UIR_Type)
class(UIRData$UIR_Type)

#drops duplicate UIRs by child that have the same incident date and incident type
UIRData <- UIRData[!duplicated(UIRData[c("child_id","incident_date", "UIR_Type")]),]

#lowers n from [n] to [n] *
write.csv(UIRData2, file = "UIRData2.csv")

#Adds columns for first runaway date and tallies up total number of runaways
UIRData %>%
  #'group' performs each action by category specified
  group_by(child_id) %>%
  #subsets by category specified 
  filter(UIR_Type %in% c("Runaway/Missing")) %>% 
  #grabs varibale, orders by variable type, creates new variable that is equal to the number of times that variable occurred
  mutate(RAorder = order(incident_date), Num_runaways = length(incident_date)) %>%
  filter(RAorder == 1) %>%
  #pulls out individual variables
  select(child_id, First_Runaway = incident_date, Num_runaways) %>% 
  #pushes two dataframes together by category indicated; all = T == keeps all rows, but uses NA if info not given
  merge(UIRData, by = "child_id", all = T) -> 
  UIRData

#Adds column for number of incidents x days prior to run (this is set for 30 days)
UIRData %>% 
  group_by(child_id) %>%
  filter(UIR_Type != "Runaway/Missing") %>%
  #Subtracts dates
  mutate(DaysBeforeRun = (First_Runaway - incident_date)) %>%
  filter(DaysBeforeRun > 0 & DaysBeforeRun <= 30) %>% 
  mutate(NumberOfIncidentsPrior = length(incident_date)) %>% 
  select(child_id, NumberOfIncidentsPrior) %>%
  #Keeps only unique rows
  distinct(.keep_all = T) %>%
  merge(UIRData, by = "child_id", all = T) ->
  UIRData

#Sets NAs to an explicit 0 for number of incidents prior to run and number of runaways
UIRData[is.na(UIRData$NumberOfIncidentsPrior),"NumberOfIncidentsPrior"] <- 0
UIRData[is.na(UIRData$Num_runaways),"Num_runaways"] <- 0

#Creates a binary categorical variable for Run/NoRun
UIRData$RunCat <- ifelse(UIRData$Num_runaways > 0, c("Runaway"), c("Censored")) 

UIRData$RunCat <- as.factor(UIRData$RunCat)
class(UIRData$RunCat)

#Creates a binary categorical variable for age
UIRData$AgeCat <- ifelse(UIRData$age_years_spell_open < 15, c("Younger than 15"), c("15 and Older")) 

UIRData$AgeCat <- as.factor(UIRData$AgeCat)
class(UIRData$AgeCat)

#Creates a binary categorical variable for length of spell
UIRData$LengthCat <- ifelse(UIRData$spell_days < 365.25, c("Less than 1 Year"), c("1 Year and Longer")) 

UIRData$LengthCat <- as.factor(UIRData$LengthCat)
class(UIRData$LengthCat)

#Creates a variable for total number of UIRs per child
UIRData %>% 
  group_by(child_id) %>%
  mutate(NumberOfIncidentsTotal = length(incident_date)) %>% 
  select(child_id, NumberOfIncidentsTotal) %>%
  #Keeps only unique rows
  distinct(.keep_all = T) %>%
  merge(UIRData, by = "child_id", all = T) ->
  UIRData

#Creates a variable for days from incident to first run, not filtering
UIRData$DaysFromInTo1Run <- c(UIRData$First_Runaway - UIRData$incident_date)
#Negative numbers mean that the incident happened after the first run

#Creates a variable for days from entry into care to first run, not filtering
UIRData$Daysto1Run <- c(UIRData$First_Runaway - UIRData$spell_opened)
#Negative numbers mean that the incident happened after the first run

#Counts the number of UIRs in each category
#Counts numbers in race categories for reporting
count(UIRData, UIR_Type)
UIR_TypeDataFreq <- data.frame(count(UIRData, UIR_Type))
write.csv(UIR_TypeDataFreq, file = "UIR_TypeDataFreq.csv")

#saves to CSV so that I can use excel to add in more data (want to factor analysis the UIRs)
write.csv(UIRData, file = "UIRData.csv")

#Now, we have all the variables to begin the initial survival analysis for time to run 

#Running some graphs to view and understand the data
#UIR type graphed by days from entry to incident filtered by run or censored
UIRBar <- ggplot(UIRData, aes(UIR_Type, days_open_to_incident, fill = RunCat))
UIRBar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + stat_summary(fun.data = mean_cl_boot, geom = "errorbar", position=position_dodge(width=0.90), width = 0.2) + 
  labs(x = "UIR Type", y = "Days to Incident", fill = "Runaway Status") + 
   theme(axis.text.x = element_text(angle = 90, hjust = 1))

#Frequencies of UIRs, no filter
UIRBar2 <- ggplot(data=UIRData, aes(x=UIR_Type)) + geom_bar(stat="count")
UIRBar2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "UIR Type", y = "Total Number") 

#Frequencies of UIRs filtered by run or censored
UIRBar2 <- ggplot(data=UIRData, aes(x=UIR_Type, fill = RunCat)) + geom_bar(stat="count")
UIRBar2 + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x = "UIR Type", y = "Total Number") 

#Creates a dataframe to look at only unique children to tally frequency rates
childUnique <- UIRData[match(unique(UIRData$child_id), UIRData$child_id),]

#Counts numbers in race categories for reporting
count(childUnique, party_race)

#Collapses race categories 
childUnique$party_race <- as.character(childUnique$party_race)
childUnique$party_race[is.na(childUnique$party_race)] <- "Not Reported"
childUnique$party_race[childUnique$party_race %in% c("Could not be Certified","Not Reported", "Asian", "Native American", "NA", " NA", "NA ", " NA ")] <- "Other"
##Asian and Native American included here because of very low n
childUnique$party_race[childUnique$party_race %in% c("Black/AFRICAN AMERICAN")] <- "Black"

childUnique$party_race <- factor(childUnique$party_race)

#Counts numbers again after collapsing categories in race categories for reporting
count(childUnique, party_race)

#AGE: Histogram of youth age at entry
ageHistogram <- ggplot(childUnique, aes(x = childUnique$age_years_spell_open)) +
  geom_histogram() + labs(x = "Age of Youth at Entry", y = "Number of Youth") + 
  ggtitle("Frequency of Age at Entry") + theme(plot.title = element_text(hjust = .5))
ageHistogram

mean(childUnique$age_years_spell_open)
median(childUnique$age_years_spell_open)
sd(childUnique$age_years_spell_open)

#LENGTH OF CARE CONTINUOUS: Histogram of youth length of stay in days
lengthOfStayDHistogram <- ggplot(childUnique, aes(x = childUnique$spell_days)) +
  geom_histogram() + labs(x = "Length in Stay (days)", y = "Number of Youth") + 
  ggtitle("Total Youth By Length of Stay") + theme(plot.title = element_text(hjust = .5))
lengthOfStayDHistogram

#Histogram of youth length of stay in years
childUnique$spell_years <- (childUnique$spell_days/365.25)

lengthOfStayYHistogram <- ggplot(childUnique, aes(x = childUnique$spell_years)) +
  geom_histogram() + labs(x = "Length in Stay (years)", y = "Number of Youth") + 
  ggtitle("Total Youth By Length in Stay") + theme(plot.title = element_text(hjust = .5))
lengthOfStayYHistogram

mean(childUnique$spell_years)
median(childUnique$spell_years)


#Frequency bar graph of sex
genderBar <- ggplot(data=childUnique, aes(x=party_sex)) +
  geom_bar(color = "black", fill = "Purple", stat="count") + labs(x = "Sex of Youth", y = "Number of Youth")
genderBar


#Frequency bar graph of race, age (binary), and length of stay (binary)
raceBar <- ggplot(data=runSurvival, aes(x=party_race)) +
  geom_bar(color = "Black", fill = "Purple", stat="count") + labs(x = "Race of Youth", y = "Number of Youth") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Race Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
raceBar

ageBar <- ggplot(data=runSurvival, aes(x=AgeCat)) +
  geom_bar(color = "Black", fill = "Purple", stat="count") + labs(x = "Age of Youth", y = "Number of Youth") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Age Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
ageBar

ageRunBar <- ggplot(data=runSurvival, aes(x=AgeCat, fill=RunCat)) +
  geom_bar(fill = "Purple", stat="count", position = position_dodge()) 
ageRunBar

LengthBar <- ggplot(data=runSurvival, aes(x=LengthCat)) +
  geom_bar(color = "Black", fill = "Purple", stat="count") + labs(x = "Time in Care (this spell)", y = "Number of Youth") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Length of Stay Frequency") +
  theme(plot.title = element_text(hjust = 0.5))
LengthBar

CareBar <- ggplot(data=runSurvival, aes(x=care_level)) +
  geom_bar(color = "Black", fill = "Purple", stat="count") + labs(x = "Care Level", y = "Number of Youth") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Care Level") +
  theme(plot.title = element_text(hjust = 0.5))
CareBar

SexBar <- ggplot(data=runSurvival, aes(x=party_sex)) +
  geom_bar(color = "Black", fill = "Purple", stat="count") + labs(x = "Sex of Youth", y = "Number of Youth") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ggtitle("Sex of Youth") +
  theme(plot.title = element_text(hjust = 0.5))
SexBar

ageLengthBar <- ggplot(childUnique, aes(childUnique$age_years_spell_open, childUnique$NumberOfIncidentsTotal))
ageLengthBar + stat_summary(fun.y = mean, geom = "bar", position = "dodge") + 
  labs(x = "Age of Youth", y = "Number of Incidents") 

class(childUnique$NumberOfIncidentsTotal)
mean(childUnique$NumberOfIncidentsTotal)
moreThanOne <- subset(childUnique, NumberOfIncidentsTotal > 1, select = c(NumberOfIncidentsTotal))
mean(moreThanOne$NumberOfIncidentsTotal)


#########Survival Analysis
runSurvival <- subset(childUnique, Daysto1Run >= 0 | is.na(childUnique$Daysto1Run), 
                  select=c(child_id, care_level, party_race, party_sex, RunCat, AgeCat, LengthCat, Daysto1Run, spell_days, age_years_spell_open))

runSurvival$Daysto1Run <- ifelse(is.na(runSurvival$Daysto1Run), runSurvival$spell_days, runSurvival$Daysto1Run)
#runSurvival$Daysto1Run[is.na(runSurvival$Daysto1Run)] <- [n] #3 years *

runSurvival$Daysto1Run <- as.numeric(runSurvival$Daysto1Run)

count(runSurvival, Event)

#############Excluding children that have runaway before spell opened

runSurvival$Event <- runSurvival$RunCat == "Runaway"
head(childUnique, n = 3)
tail(childUnique, n = 3)

count(runSurvival)


attach(runSurvival)
Model.1 <- Surv(Daysto1Run, Event)
Model.1
Model.2 <- survfit(Model.1 ~ 1, conf.type = "plain")
summary(Model.2)
plot(Model.2, main = "K-M Estimate with 95% Confidence Bounds",
     xlab = "Time to First Run (days)",
     ylab = "Survival Function")
detach(runSurvival)

#Analysis based on 2 factor level variables
attach(runSurvival)
Model.3a <- survdiff(Surv(Daysto1Run, Event) ~ AgeCat, rho = 0) # log rank/Mantel-Cox
Model.3a

Model.3b <- survdiff(Surv(Daysto1Run, Event) ~ AgeCat, rho = 1) # Modified Gehan-Wilcoxin
Model.3b

#Median survival times Age, Race, Length, and Care level

runSurvival$SurvObj <- with(runSurvival, Surv(Daysto1Run, Event))

Model.By.Sex <- survfit(SurvObj ~ party_sex, data = runSurvival)
Model.By.Sex

Model.By.Age <- survfit(SurvObj ~ AgeCat, data = runSurvival)
Model.By.Age
mean(runSurvival$age_years_spell_open)

Model.By.Race <- survfit(SurvObj ~ party_race, data = runSurvival)
Model.By.Race

Model.By.Length <- survfit(SurvObj ~ LengthCat, data = runSurvival)
Model.By.Length

Model.By.Care <- survfit(SurvObj ~ care_level, data = runSurvival)
Model.By.Care

count(runSurvival, care_level)
count(runSurvival, RunCat)
sd(runSurvival$Daysto1Run)

##Plots for each factor (Age, Race, Length, and Care Level): AGE

pl.Age <- ggsurv(Model.By.Age,
                    lty.est = c(1,2),
                    cens.col = c("red","blue"))

pl.Age <- pl.Age + ggplot2::guides(linetype = FALSE) +
  ggplot2::scale_colour_discrete(
    name   = 'Age',
    breaks = c(0, 1),
    labels = c('15 and Older', 'Younger than 15'))


Age.Medians <- data.frame(time = c(136,136,416,416),
                             quant = c(.5, 0, .5, 0),
                             Age = c('>=15', '>=15', '<15', '<15'))
pl.Age + geom_line(data = Age.Medians,
                      aes(time, quant, group = Age),
                      col = 'darkblue', linetype = 3) +
  geom_point(data = Age.Medians,
             aes(time, quant, group = Age),
             col = 'darkblue') + ggtitle("Age and Time to Run (Red is >=15 years)") +
  theme(plot.title = element_text(hjust = 0.5))

##Plot for SEX
pl.Sex <- ggsurv(Model.By.Sex,
                 lty.est = c(1,2),
                 cens.col = c("red","blue"))

pl.Sex <- pl.Sex + ggplot2::guides(linetype = FALSE) +
  ggplot2::scale_colour_discrete(
    name   = 'Sex',
    breaks = c(0, 1),
    labels = c('Female', 'Male'))


Sex.Medians <- data.frame(time = c(176,176,282,282),
                          quant = c(.5, 0, .5, 0),
                          Sex = c('F', 'F', 'M', 'M'))
pl.Sex + geom_line(data = Sex.Medians,
                   aes(time, quant, group = Sex),
                   col = 'darkblue', linetype = 3) +
  geom_point(data = Sex.Medians,
             aes(time, quant, group = Sex),
             col = 'darkblue') + ggtitle("Sex and Time to Run (Red = F)") +
  theme(plot.title = element_text(hjust = 0.5))


##Plot for RACE

pl.Race <- ggsurv(Model.By.Race,
                 lty.est = c(1,2,3,4),)

#cens.col = c("red","blue", "green", "purple") :: Add in if desired, but it makes the graph very hard to read


##Plot for Length

pl.Length <- ggsurv(Model.By.Length,
                 lty.est = c(1,2),
                 cens.col = c("red","blue"))

pl.Length <- pl.Length + ggplot2::guides(linetype = FALSE) +
  ggplot2::scale_colour_discrete(
    name   = 'Length in Care',
    breaks = c(0, 1),
    labels = c('1 Year and Longer', 'Less than 1 Year'))


Length.Medians <- data.frame(time = c(336,336,131,131),
                          quant = c(.5, 0, .5, 0),
                          Length = c('>=1 year', '>=1 year', '<1 year', '<1 year'))
pl.Length + geom_line(data = Length.Medians,
                   aes(time, quant, group = Length),
                   col = 'darkblue', linetype = 3) +
  geom_point(data = Length.Medians,
             aes(time, quant, group = Length),
             col = 'darkblue') +
  ggtitle("Length of Care and Time to Run (Blue = <1 year)") +
  theme(plot.title = element_text(hjust = 0.5))


##Plot for Care Level

pl.Care <- ggsurv(Model.By.Care,
                  lty.est = c(1,2,3,4,5,6), cens.col = c("red","blue", "green", "purple", "orange", "yellow"))

#cens.col = c("red","blue", "green", "purple") :: Add in if desired, but it makes the graph very hard to read

pl.Care <- pl.Care + ggplot2::guides(linetype = FALSE) +
  ggplot2::scale_colour_discrete(
    name   = 'Care Level',
    breaks = c(0, 1),
    labels = c('Not Reported', 'Chronic', 'Mild', 'Moderate', 'Moderate GH', 'Severe'))


Care.Medians <- data.frame(time = c(220,220,38.5,38.5,0,0,263,263,100,100,315,315),
                           quant = c(.5, 0, .5, 0, .5, 0, .5, 0, .5, 0, .5, 0),
                           Care = c(' ', ' ', 'Chronic', 'Chronic', 'Mild', 'Mild', 'Moderate', 'Moderate', 'Moderate GH', 'Moderate GH', 'Severe', 'Severe'))
pl.Care + geom_line(data = Care.Medians,
                    aes(time, quant, group = Care),
                    col = 'darkblue', linetype = 3) +
  geom_point(data = Race.Medians,
             aes(time, quant, group = Race),
             col = 'darkblue') +
  ggtitle("Level of Care and Time to Run") +
  theme(plot.title = element_text(hjust = 0.5))


#Comparing factor level (gender or age?) for each strata (level of care/race) using Cox Proportional Hazards Model
ModelSex <- coxph(Surv(Daysto1Run, Event) ~ party_sex, data = runSurvival)
summary(ModelSex)

ModelAge <- coxph(Surv(Daysto1Run, Event) ~ AgeCat, data = runSurvival)
summary(ModelAge)

ModelAgeCont <- coxph(Surv(Daysto1Run, Event) ~ age_years_spell_open, data = runSurvival)
summary(ModelAgeCont)

runSurvival$party_race <- factor(runSurvival$party_race, levels = c("White", "Black", "Hispanic", "Other"))
ModelRace <- coxph(Surv(Daysto1Run, Event) ~ party_race, data = runSurvival)
summary(ModelRace)

ModelSpellDays <- coxph(Surv(Daysto1Run, Event) ~ spell_days, data = runSurvival)
summary(ModelSpellDays)

ModelLength <- coxph(Surv(Daysto1Run, Event) ~ LengthCat, data = runSurvival)
summary(ModelLength)

runSurvival$care_level <- factor(runSurvival$care_level, levels = c("Mild", "moderate", "moderate GH", "severe", "chronic", "Not Reported"))
ModelCare <- coxph(Surv(Daysto1Run, Event) ~ care_level, data = runSurvival)
summary(ModelCare)


ModelGenderRaceAgeLengthCare <- coxph(Surv(Daysto1Run, Event) ~ party_sex + party_race + AgeCat + LengthCat + care_level, data = runSurvival)
summary(ModelGenderRaceAgeLengthCare)

##Frequency tables with runSurvival
ageHistogram <- ggplot(runSurvival, aes(x = runSurvival$age_years_spell_open)) +
  geom_histogram() + labs(x = "Age of Youth at Entry", y = "Number of Youth") + 
  ggtitle("Frequency of Age at Entry") + theme(plot.title = element_text(hjust = .5))
ageHistogram

runSurvival$spell_years <- (runSurvival$spell_days/365.25)

lengthOfStayYHistogram <- ggplot(runSurvival, aes(x = runSurvival$spell_years)) +
  geom_histogram() + labs(x = "Length in Stay (years)", y = "Number of Youth") + 
  ggtitle("Total Youth By Length in Stay") + theme(plot.title = element_text(hjust = .5))
lengthOfStayYHistogram

mean(runSurvival$spell_days)
mean(runSurvival$spell_days/365.25)
sd(runSurvival$spell_days/365.25)

##Multilayer plots

GenderF.Data <- runSurvival[ which(runSurvival$party_sex == "F"), ]
GenderF.Data$SurvObj <- with(GenderF.Data, Surv(Daysto1Run, Event))
Model.5a <- npsurv(formula = Surv(Daysto1Run, Event) ~ AgeCat, data = GenderF.Data)
survplot(Model.5a)

GenderM.Data <- runSurvival[ which(runSurvival$party_sex == "M"), ]
GenderM.Data$SurvObj <- with(GenderM.Data, Surv(Daysto1Run, Event))
Model.5b <- npsurv(formula = Surv(Daysto1Run, Event) ~ AgeCat, data = GenderM.Data)
survplot(Model.5b)

GenderF2.Data <- runSurvival[ which(runSurvival$party_sex == "F"), ]
GenderF2.Data$SurvObj <- with(GenderF.Data, Surv(Daysto1Run, Event))
Model.5c <- npsurv(formula = Surv(Daysto1Run, Event) ~ party_race, data = GenderF2.Data)
survplot(Model.5c)



