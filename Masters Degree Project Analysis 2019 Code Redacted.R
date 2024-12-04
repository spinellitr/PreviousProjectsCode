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
library(foreign)
library(tinytex)
library(mfx)
library(forcats)
library(crunch)

##Goal: To run statistics for master's degree project, completed in 2019 *
##Because project used proprietary data of minors involved in foster care, *
##code below has been redacted and edited to protect privacy. *
##Comments are sometimes added for clarity. 
##Lines that has been edited or added are marked with "*". *
##Some deleted lines which had proprietary information are unmarked.  *
##Deleted lines were usually results commented into the code and have been included in the final paper. 
##Publication is in progress. * 
##Specific research idea is intellectual property of Tawny Spinelli, in collaboration with Dr. Neil Jordan and Dr. Cassandra Kisiel.
##Code is shared here to demonstrate proficiency in R stats coding and progress as a coder over time.

#Pull in datafile from FY 17 (at least 1 CANS, most recent, in fiscal year; TAY defined at 14.5-21 years)
#Datafile has both numerical and categorical labels for CANS

setwd("~/Desktop/[working directory name]") *
TAYnonTAYData012518 <- read.csv("TAY_nonTAY_combined_1.25.18_CatNum.csv", header = TRUE, na.strings=c("","NA"))
#n=[n] *

#drop intact CANS cases from dataset because missing days open and are separate (n=[n] dropped) *
TAYnonTAYData <-TAYnonTAYData012518[!(TAYnonTAYData012518$CANS_type =="Intact Family"),]
#n=[n] *

#create domain totals with sums; Remember: Child Strengths is reverse scored (lower is better)
TAYnonTAYData$TraumaExpTot <- TAYnonTAYData$cans1 + TAYnonTAYData$cans2 + TAYnonTAYData$cans3 + TAYnonTAYData$cans4 + TAYnonTAYData$cans5 + TAYnonTAYData$cans6 + TAYnonTAYData$cans7 + TAYnonTAYData$cans8 + TAYnonTAYData$cans9 + TAYnonTAYData$cans10 + TAYnonTAYData$cans11 + TAYnonTAYData$cans12 + TAYnonTAYData$cans13
TAYnonTAYData$TraumaStressTot <- TAYnonTAYData$cans14 + TAYnonTAYData$cans15 + TAYnonTAYData$cans16 + TAYnonTAYData$cans17 + TAYnonTAYData$cans18 + TAYnonTAYData$cans19 
TAYnonTAYData$ChildStrengthsTot <- TAYnonTAYData$cans20 + TAYnonTAYData$cans21 + TAYnonTAYData$cans22 + TAYnonTAYData$cans23 + TAYnonTAYData$cans24 + TAYnonTAYData$cans25 + TAYnonTAYData$cans26 + TAYnonTAYData$cans27 + TAYnonTAYData$cans28 + TAYnonTAYData$cans29 + TAYnonTAYData$cans30
TAYnonTAYData$LifeDomTot <- TAYnonTAYData$cans31 + TAYnonTAYData$cans32 + TAYnonTAYData$cans33 + TAYnonTAYData$cans34 + TAYnonTAYData$cans35 + TAYnonTAYData$cans36 + TAYnonTAYData$cans37 + TAYnonTAYData$cans38 + TAYnonTAYData$cans39 + TAYnonTAYData$cans40 + TAYnonTAYData$cans41 + TAYnonTAYData$cans42 + TAYnonTAYData$cans43
TAYnonTAYData$AccultTot <- TAYnonTAYData$cans44 + TAYnonTAYData$cans45 + TAYnonTAYData$cans46 + TAYnonTAYData$cans47
TAYnonTAYData$BehEmoNeedsTot <- TAYnonTAYData$cans48 + TAYnonTAYData$cans49 + TAYnonTAYData$cans50 + TAYnonTAYData$cans51 + TAYnonTAYData$cans52 + TAYnonTAYData$cans53 + TAYnonTAYData$cans54 + TAYnonTAYData$cans55 + TAYnonTAYData$cans56 + TAYnonTAYData$cans57 + TAYnonTAYData$cans58 + TAYnonTAYData$cans59 + TAYnonTAYData$cans60
TAYnonTAYData$ChildRiskTot <- TAYnonTAYData$cans61 + TAYnonTAYData$cans62 + TAYnonTAYData$cans63 + TAYnonTAYData$cans64 + TAYnonTAYData$cans65 + TAYnonTAYData$cans66 + TAYnonTAYData$cans67 + TAYnonTAYData$cans68 + TAYnonTAYData$cans69 + TAYnonTAYData$cans70 + TAYnonTAYData$cans71 
TAYnonTAYData$YoungChildTot <- TAYnonTAYData$cans72 + TAYnonTAYData$cans73 + TAYnonTAYData$cans74 + TAYnonTAYData$cans75 + TAYnonTAYData$cans76 + TAYnonTAYData$cans77 + TAYnonTAYData$cans78 + TAYnonTAYData$cans79 + TAYnonTAYData$cans80 + TAYnonTAYData$cans81 + TAYnonTAYData$cans82 + TAYnonTAYData$cans83 + TAYnonTAYData$cans84 + TAYnonTAYData$cans85 + TAYnonTAYData$cans86 
TAYnonTAYData$Tran2AdultTot <- TAYnonTAYData$cans87 + TAYnonTAYData$cans88 + TAYnonTAYData$cans89 + TAYnonTAYData$cans90 + TAYnonTAYData$cans91 + TAYnonTAYData$cans92 + TAYnonTAYData$cans93 + TAYnonTAYData$cans94 

#create domain totals with averages; Remember: Child Strengths is reverse scored (lower is better)
TAYnonTAYData$TraumaExpAve <- TAYnonTAYData$TraumaExpTot/13
TAYnonTAYData$TraumaStressAve <- TAYnonTAYData$TraumaStressTot/6
TAYnonTAYData$ChildStrengthsAve <- TAYnonTAYData$ChildStrengthsTot/11
TAYnonTAYData$LifeDomAve <- TAYnonTAYData$LifeDomTot/13
TAYnonTAYData$AccultAve <- TAYnonTAYData$AccultTot/4
TAYnonTAYData$BehEmoNeedsAve <- TAYnonTAYData$BehEmoNeedsTot/13
TAYnonTAYData$ChildRiskAve <- TAYnonTAYData$ChildRiskTot/11
TAYnonTAYData$YoungChildAve <- TAYnonTAYData$YoungChildTot/15
TAYnonTAYData$Tran2AdultAve <- TAYnonTAYData$Tran2AdultTot/8

####Double check to see what the code below is doing with missing values...There doesn't seem to be missing data in first 13 cans items?
#create new trauma 'total' variables for each CANS domain using (0s vs 1s, 2s, and 3s)

TotalTraumaExp0s <- if_else(TAYnonTAYData$cans1 == 0, 0, 1) + if_else(TAYnonTAYData$cans2 == 0, 0, 1) + if_else(TAYnonTAYData$cans3 == 0, 0, 1) + if_else(TAYnonTAYData$cans4 == 0, 0, 1) + if_else(TAYnonTAYData$cans5 == 0, 0, 1) + if_else(TAYnonTAYData$cans6 == 0, 0, 1) + if_else(TAYnonTAYData$cans7 == 0, 0, 1) + if_else(TAYnonTAYData$cans8 == 0, 0, 1) + if_else(TAYnonTAYData$cans9 == 0, 0, 1) + if_else(TAYnonTAYData$cans10 == 0, 0, 1) + if_else(TAYnonTAYData$cans11 == 0, 0, 1) + if_else(TAYnonTAYData$cans12 == 0, 0, 1) + if_else(TAYnonTAYData$cans13 == 0, 0, 1)
TAYnonTAYData$TotalTraumaExp0s <- TotalTraumaExp0s

                                   
#create new trauma 'total' variables for each CANS domain using (0s and 1s vs 2s, 3s)

TotalTraumaExp01s <- if_else(TAYnonTAYData$cans1 < 2, 0, 1) + if_else(TAYnonTAYData$cans2 < 2, 0, 1) + if_else(TAYnonTAYData$cans3 < 2, 0, 1) + if_else(TAYnonTAYData$cans4 < 2, 0, 1) + if_else(TAYnonTAYData$cans5 < 2, 0, 1) + if_else(TAYnonTAYData$cans6 < 2, 0, 1) + if_else(TAYnonTAYData$cans7 < 2, 0, 1) + if_else(TAYnonTAYData$cans8 < 2, 0, 1) + if_else(TAYnonTAYData$cans9 < 2, 0, 1) + if_else(TAYnonTAYData$cans10 < 2, 0, 1) + if_else(TAYnonTAYData$cans11 < 2, 0, 1) + if_else(TAYnonTAYData$cans12 < 2, 0, 1) + if_else(TAYnonTAYData$cans13 < 2, 0, 1)
TAYnonTAYData$TotalTraumaExp01s <- TotalTraumaExp01s

#drop any youth who has any score above 0 on CANS runaway item; 
#TAYnonTAYData <-TAYnonTAYData[!(TAYnonTAYData$cans66 > 0),]

#drop any youth who has a missing value in placement count before cans
###Make sure there are no zeros in the sample for placement count
TAYnonTAYData <- TAYnonTAYData[!is.na(TAYnonTAYData$place_cnt_bfcans),] #(dropped n=[n]) *
#n=[n] *
#Drop any youth who have zeros for placement count;
TAYnonTAYData <- TAYnonTAYData[!(TAYnonTAYData$place_cnt_bfcans==0),] #(dropped n=[n]) *
#n=[n] *

#drop any youth who has a missing value in CANSAge
#Question: could go back and see if we can detect why CANSAge is missing. Is birth date not there or is it out of range? 
#Might see a comma or extra space in the birth date which then might be the reason why it's 'missing'
TAYnonTAYData <- TAYnonTAYData[!is.na(TAYnonTAYData$CANS_age),] #(dropped n=[n]) *
#n=[n] *
TAYnonTAYData <- TAYnonTAYData[!is.na(TAYnonTAYData$gender),] #(dropped n=[n]) *
#n=[n] *

#Drop if family strengths variable is NA
TAYnonTAYData <- TAYnonTAYData[!is.na(TAYnonTAYData$cans20),] #(dropped n=[n]) *
TAYnonTAYData <- TAYnonTAYData[!is.na(TAYnonTAYData$cans20C),] #(dropped n=[n])
#n=[n] *

#Drop any youth who has been in care for less than 1 year; (dropped n=[n]) *
TAYnonTAYData <-TAYnonTAYData[!(TAYnonTAYData$yearsincare_curr < 1),]
#n=[n] *

#Drop any youth who had "Home of Parent" as placement living arrangement type (dropped n=[n]) *
TAYnonTAYData <-TAYnonTAYData[!(TAYnonTAYData$livar_type == "Home of Parent"),]
#n=[n] *

levels(TAYnonTAYData$race)
#drop any youth who has race identified as Asian or Native American (too low n for race comparisons in this study)

TAYnonTAYData <-TAYnonTAYData[!(TAYnonTAYData$race =="Asian"),] #(dropped n=[n], n=[n])*
TAYnonTAYData <-TAYnonTAYData[!(TAYnonTAYData$race =="Native American"),] #(dropped n=[n], n=[n])*
TAYnonTAYData <- TAYnonTAYData[!is.na(TAYnonTAYData$race),] #(dropped n=[n], n=[n])*
#n=[n] *


#Some descriptives: age at CANS, # of placements before CANS, length of time in care, age at entry
#Plot variables
describe(TAYnonTAYData$CANS_age, na.rm=TRUE, range=TRUE)

describe(TAYnonTAYData$curr_case_age, na.rm=TRUE)  #age at entry variable 

describe(TAYnonTAYData$yearsincare_curr, na.rm=TRUE)

describe(TAYnonTAYData$place_cnt_bfcans, na.rm=TRUE)
#mean and variant are not equal, therefore, use a negative binomial, 

#will want to test the association between this variable and placement count before CANS variable
#will I be best served by continuous or grouping? Continuous is more powerful; 
#Test with negative binomial (better captures nature of distribution of placement stability variable)
 
raceTable <- table(TAYnonTAYData$race)
raceTable

sexTable <- table(TAYnonTAYData$gender)
sexTable

openReasonTable <- table(TAYnonTAYData$case_open_reason)
openReasonTable
#Combined all abuse and all neglect reasons. <- If you're going to use this in the future, combine as combined below

#See below for combined livar type
openLivarTable <- table(TAYnonTAYData$livar_type)
openLivarTable

#Stopped saving the below file on 4/19/19
#write.csv(TAYnonTAYData, file = "TAYNonTAYData02052019.csv")

#Write csv into master's folder to keep with other masters stuff
#Stopped saving the below file on 4/19/19
#write.csv(TAYnonTAYData, file = "~/Desktop/[where it is saved].csv") *

#Save csv into master's folder to keep with other masters stuff
#Stopped saving the below file on 4/19/19
#write.csv(TAYnonTAYData, file = "~/Desktop/[where it is saved].csv") *

#Grab only needed variables
MastersData <- TAYnonTAYData %>% select(2, 10:103, 106, 107, 110, 119, 121, 126, 132, 135, 141, 145, 152, 153, 157:271)
#Save master's data file into master's folder
#write.csv(MastersData, file = "~/Desktop/[where it is saved].csv") *

#****************************************************** New Code Book ******************************************************
#old Gender = Sex
#old CANS_age (age at CANS assessment) = Age
#old curr_case_age (age at current open) = EntryAge
#old yearsincare_curr (years in care current open case) = YearsInCare
#old place_cnt_bfcans (total placement count before CANS) = PlacementsCount

names(MastersData)[names(MastersData) == "gender"] <- "Sex"
names(MastersData)[names(MastersData) == "CANS_age"] <- "Age"
names(MastersData)[names(MastersData) == "curr_case_age"] <- "EntryAge"
names(MastersData)[names(MastersData) == "yearsincare_curr"] <- "YearsInCare"
names(MastersData)[names(MastersData) == "place_cnt_bfcans"] <- "PlacementCount"

#Combine/Collapse livar type into fewer categories

MastersData$livar_type_factor <- factor(MastersData$livar_type)

library(forcats)
MastersData$livar_type_factor_new <- fct_collapse(MastersData$livar_type_factor,
                                                Foster.Care = c("Adoptive Living",
                                                                "Foster Care Boarding(B)",
                                                                "Foster Care Boarding(P)",
                                                                "Foster Home Adoptive"),
                                                Specialized.Foster.Care = c("Special Foster Care",
                                                                            "Special Foster Care Adolescent Care",
                                                                            "Special Foster Care Teen Parenting"),
                                                Group.Home.Residential = c("Group Home Living Arrangement",
                                                                           "Residential"),
                                                IL.TL = c("Independent Living",
                                                          "Transitional Living"),
                                                Home.Of.Relative = c("Home of Parent",
                                                                     "Home of Relative"),
                                                Other = c("Other/Other906"))

table(MastersData$livar_type_factor)
table(MastersData$livar_type_factor_new)
levels(MastersData$livar_type_factor_new)

names(MastersData)[names(MastersData) == "livar_type_factor_new"] <- "Living.Arrangement"
levels(MastersData$Living.Arrangement)

#Combine/Collapse 2s and 3s because of the low number of 3s identified 
levels(MastersData$cans20C)
table(MastersData$cans20C)
MastersData$cans20CFactor <- factor(MastersData$cans20C)

library(forcats)
MastersData$cans20CRel <- fct_collapse(MastersData$cans20CFactor,
                                                  Centerpiece.Strength = c("Centerpiece Strength"),
                                                  Useful.Strength = c("Strength Building Needed"),
                                                  Insufficient.Strength = c("No Identified Strength",
                                                                             "Strength Building Required"))

levels(MastersData$cans20C)
table(MastersData$cans20C)
levels(MastersData$cans20CRel)
table(MastersData$cans20CRel)

#Rename combined/collapsed CANS family strengths varible 
names(MastersData)[names(MastersData) == "cans20CRel"] <- "FamilyStrengths3"
levels(MastersData$FamilyStrengths3)
table(MastersData$FamilyStrengths3)

#Relevel CANS family strengths variable 
MastersData$FamilyStrengths3 <- factor(MastersData$FamilyStrengths3, levels = c("Insufficient.Strength", "Useful.Strength", "Centerpiece.Strength"))
levels(MastersData$FamilyStrengths3)
table(MastersData$FamilyStrengths3)

#Combine/Collapse 1s, 2s and 3s because of Ns and the evidence of non-meaningful differences between 1s, 2s, and 3s when 2s and 3s were combined. So, want to compare 0s to 1s, 2s, and 3s
levels(MastersData$FamilyStrengths3)
table(MastersData$FamilyStrengths3)
MastersData$FamilyStrengths3Factor <- factor(MastersData$FamilyStrengths3)

library(forcats)
MastersData$FamilyStrengths2 <- fct_collapse(MastersData$FamilyStrengths3, 
                                             Centerpiece.Strength = c("Centerpiece.Strength"), 
                                             NonCenterpiece.Strength = c("Insufficient.Strength", "Useful.Strength"))

levels(MastersData$FamilyStrengths3)
table(MastersData$FamilyStrengths3)
levels(MastersData$FamilyStrengths2)
table(MastersData$FamilyStrengths2)

#Relevel CANS family strengths variable 
MastersData$FamilyStrengths2 <- factor(MastersData$FamilyStrengths2, levels = c("NonCenterpiece.Strength", "Centerpiece.Strength"))
levels(MastersData$FamilyStrengths2)
table(MastersData$FamilyStrengths2)

#Recode age variale to create a categorical age variable 
#Age bands: 5-9 (4), 10-14 (4), 15-18 (3), 18-21 (3)
MastersData$AgeCat <- cut(MastersData$Age,
                          breaks = c(5,7,10,14,18,22),
                          labels = c("YoungChild(5-6)", "Child(7-9)", "Early Adolescent(10-13)", "Adolescent(14-17)", "Young Adult(18-21)"))
  
levels(MastersData$AgeCat)
table(MastersData$AgeCat)

#Relevel to ensure that levels stay in the correct order when pulled into other programs
MastersData$AgeCat <- factor(MastersData$AgeCat, levels = c("YoungChild(5-6)", "Child(7-9)", "Early Adolescent(10-13)", "Adolescent(14-17)", "Young Adult(18-21)"))
levels(MastersData$AgeCat)

#Relevel race so that R uses Hispanic as the comparison category
MastersData$race <- relevel(MastersData$race, ref = "Hispanic")
levels(MastersData$race)

#Relevel Family Strengths2 so that R uses NonCenterpiece Strength as the comparison category
MastersData$FamilyStrengths2 <- relevel(MastersData$FamilyStrengths2, ref = "NonCenterpiece.Strength")
levels(MastersData$FamilyStrengths2)

raceTable2 <- table(MastersData$race)
raceTable2

#Save master's data file into master's folder (updated date after collapsing variables)
write.csv(MastersData, file = "~/Desktop/[where it is saved].csv") *

#*********************************************** Descriptives ***************************************************************

#Ran these above, but should run them again in this dataframe

describe(MastersData$Age, na.rm=TRUE, range=TRUE)

describe(MastersData$EntryAge, na.rm=TRUE, range = TRUE)  #age at entry variable 

describe(MastersData$YearsInCare, na.rm=TRUE)

describe(MastersData$PlacementCount, na.rm=TRUE, range = TRUE)
var(MastersData$PlacementCount)
#Since the mean and variance are not equal, use a negative binomial. For negative binomial regression, the variance is always larger than the mean. Poisson regression requires that variance and mean are equal; 

raceTable <- table(MastersData$race)
raceTable

sexTable <- table(MastersData$Sex)
sexTable

ageCatTable <- table(MastersData$AgeCat)
ageCatTable

#livingArrangementTable <- table(MastersData$Living.Arrangement)
#livingArrangementTable

#Find descriptives by Non-Centerpiece Strength versus Centerpiece Strength
fs <- table(MastersData$FamilyStrengths2)
fs
prop.table(fs)

fsRace <- table(MastersData$FamilyStrengths2, MastersData$race)
fsRace
prop.table(fsRace)

fsAgeCat <- table(MastersData$FamilyStrengths2, MastersData$AgeCat)
fsAgeCat
prop.table(fsAgeCat)

fsSex <- table(MastersData$FamilyStrengths2, MastersData$Sex)
fsSex
prop.table(fsSex)

#Graph these as well

#Get descriptives of sample by family strengths category
tapply(MastersData$Age, MastersData$FamilyStrengths2, mean)
tapply(MastersData$Age, MastersData$FamilyStrengths2, sd)

tapply(MastersData$EntryAge, MastersData$FamilyStrengths2, mean)
tapply(MastersData$EntryAge, MastersData$FamilyStrengths2, sd)

tapply(MastersData$YearsInCare, MastersData$FamilyStrengths2, mean)
tapply(MastersData$YearsInCare, MastersData$FamilyStrengths2, sd)

tapply(MastersData$PlacementCount, MastersData$FamilyStrengths2, mean)
tapply(MastersData$PlacementCount, MastersData$FamilyStrengths2, sd)



#*********************************************** Analyses ***************************************************************

##Running negative binomial regressions starting with independent variables predicting dependent variable, number of placements
#https://stats.idre.ucla.edu/r/dae/negative-binomial-regression/

#Using negative binomial regression to test covariates with dependent variable 
#Covariate 1: Age at entry 
summary(mC1 <- glm.nb(MastersData$PlacementCount ~ MastersData$EntryAge, data = MastersData)) 
(mC1est <- cbind(Estimate = coef(mC1), confint(mC1)))
negbinirr(MastersData$PlacementCount ~ MastersData$EntryAge, data = MastersData, robust = TRUE) #IRRs
estmC1 <- cbind(estimate = coef(mC1), confint(mC1)) #IRRs
exp(estmC1)

#Covariate 2: Years in Care
summary(mC2 <- glm.nb(MastersData$PlacementCount ~ MastersData$YearsInCare, data = MastersData)) 
(mC2est <- cbind(Estimate = coef(mC2), confint(mC2)))
negbinirr(MastersData$PlacementCount ~ MastersData$YearsInCare, data = MastersData, robust = TRUE)

#Covariate 1x2: Years in Care x Age at Entry
summary(mC1x2 <- glm.nb(MastersData$PlacementCount ~ MastersData$YearsInCare + MastersData$EntryAge + MastersData$EntryAge*MastersData$YearsInCare, data = MastersData)) 
(mC1x2est <- cbind(Estimate = coef(mC1x2), confint(mC1x2)))
negbinirr(MastersData$PlacementCount ~ MastersData$YearsInCare + MastersData$EntryAge + MastersData$EntryAge*MastersData$YearsInCare, data = MastersData, robust = TRUE)

#Covariate 3: Living arrangement type
#summary(mC3 <- glm.nb(MastersData$PlacementCount ~ MastersData$livar_type, data = MastersData)) 
#(mC3est <- cbind(Estimate = coef(mC3), confint(mC3)))
#negbinirr(MastersData$PlacementCount ~ MastersData$livar_type, data = MastersData, robust = TRUE)


#Dependent variable analyses adjusted for the above covariates
#Sex and placement
summary(m1 <- glm.nb(MastersData$PlacementCount ~ MastersData$Sex + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData)) 
(m1est <- cbind(Estimate = coef(m1), confint(m1)))
negbinirr(MastersData$PlacementCount ~ MastersData$Sex + MastersData$EntryAge, data = MastersData, robust = TRUE)  #give IRR for formula


#Age and placement predicting number of placements
summary(m2 <- glm.nb(MastersData$PlacementCount ~ MastersData$Age + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData))
(m2est <- cbind(Estimate = coef(m2), confint(m2)))
negbinirr(MastersData$PlacementCount ~ MastersData$Age + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData, robust = TRUE)  #give IRR for formula


#Race and placement  (African American in the current reference group-see releveling above)
summary(m3 <- glm.nb(MastersData$PlacementCount ~ MastersData$race + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData)) 
(m3est <- cbind(Estimate = coef(m3), confint(m3)))


#numerical CANS Categorical family strengths variable (two categories)
summary(m6 <- glm.nb(MastersData$PlacementCount ~ MastersData$FamilyStrengths2 + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData)) 
(m6est <- cbind(Estimate = coef(m6), confint(m6)))
negbinirr(MastersData$PlacementCount ~ MastersData$FamilyStrengths2 + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData, robust = TRUE)


##Testing interactions 

summary(m7 <- glm.nb(MastersData$PlacementCount ~ MastersData$Sex + MastersData$Age + MastersData$Sex*MastersData$Age, data = MastersData)) 
(m7est <- cbind(Estimate = coef(m7), confint(m7)))
negbinirr(MastersData$PlacementCount ~ MastersData$Sex + MastersData$Age + MastersData$Sex*MastersData$Age, data = MastersData, robust = TRUE)

summary(m8 <- glm.nb(MastersData$PlacementCount ~ MastersData$Sex + MastersData$race + MastersData$Sex*MastersData$race, data = MastersData)) 
(m8est <- cbind(Estimate = coef(m8), confint(m8)))
negbinirr(MastersData$PlacementCount ~ MastersData$Sex + MastersData$race + MastersData$Sex*MastersData$race, data = MastersData, robust = TRUE)

summary(m9 <- glm.nb(MastersData$PlacementCount ~ MastersData$race + MastersData$Age + MastersData$race*MastersData$Age, data = MastersData)) 
(m9est <- cbind(Estimate = coef(m9), confint(m9)))
negbinirr(MastersData$PlacementCount ~ MastersData$race + MastersData$Age + MastersData$race*MastersData$Age, data = MastersData, robust = TRUE)

#can use the anova function to compare models
#anova(m1, m7)

#Check to see if covariates predict family strengths #Logistic regression (no IRRs here)
summary(mC10 <- glm(MastersData$FamilyStrengths2 ~ MastersData$EntryAge, data = MastersData, family = binomial()))

summary(mC10 <- glm.nb(MastersData$FamilyStrengths2 ~ MastersData$EntryAge, data = MastersData))

summary(mC11 <- glm(MastersData$FamilyStrengths2 ~ MastersData$YearsInCare, data = MastersData, family = binomial()))

summary(mC12 <- glm(MastersData$FamilyStrengths2 ~ MastersData$EntryAge + MastersData$YearsInCare + MastersData$EntryAge*MastersData$YearsInCare, data = MastersData, family = binomial()))
exp(cbind(OR = coef(mC12), confint(mC12)))
#################################################################LINE ABOVE IS CONFIDENCE INTERVALS AND ODDS RATIO OF LOGISTIC REGRESSION**********************

#Check to see if demographic factors predict family strengths 

summary(m10 <- glm(MastersData$FamilyStrengths2 ~ MastersData$Age + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData, family = binomial()))

summary(m11 <- glm(MastersData$FamilyStrengths2 ~ factor(MastersData$race) + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData, family = binomial()))

summary(m12 <- glm(MastersData$FamilyStrengths2 ~ factor(MastersData$Sex) + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData, family = binomial()))

summary(m15 <- glm(MastersData$FamilyStrengths2 ~ factor(MastersData$Sex) + factor(MastersData$race) + MastersData$Age + factor(MastersData$Sex)*factor(MastersData$race) + MastersData$Age*factor(MastersData$race) + factor(MastersData$Sex)*MastersData$Age +  factor(MastersData$Sex)*factor(MastersData$race)*MastersData$Age + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData))


#Full Model
summary(m13 <- glm.nb(MastersData$PlacementCount ~ MastersData$FamilyStrengths2 + MastersData$race + MastersData$Age + MastersData$Sex + MastersData$race*MastersData$Age + MastersData$race*MastersData$Sex + MastersData$Sex*MastersData$Age + MastersData$Age*MastersData$race*MastersData$Sex + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData))
(m13est <- cbind(Estimate = coef(m13), confint(m13)))
negbinirr(MastersData$PlacementCount ~ MastersData$FamilyStrengths2 + MastersData$race + MastersData$Age + MastersData$Sex + MastersData$race*MastersData$Age + MastersData$race*MastersData$Sex + MastersData$Sex*MastersData$Age + MastersData$Age*MastersData$race*MastersData$Sex + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData, robust = TRUE)


summary(m14 <- glm.nb(MastersData$PlacementCount ~ MastersData$race + MastersData$Age + MastersData$Sex + MastersData$race*MastersData$Age + MastersData$race*MastersData$Sex + MastersData$Sex*MastersData$Age + MastersData$Age*MastersData$race*MastersData$Sex + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData))
(m14est <- cbind(Estimate = coef(m14), confint(m14)))
negbinirr(MastersData$PlacementCount ~ MastersData$race + MastersData$Age + MastersData$Sex + MastersData$race*MastersData$Age + MastersData$race*MastersData$Sex + MastersData$Sex*MastersData$Age + MastersData$Age*MastersData$race*MastersData$Sex + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData, robust = TRUE)


#Comparing predictive models with and without family strengths
#How do you tell if it's a significant difference  (compared the AIC of both models but still not sure how to do it using anova)
#This is not working how I'd like it to work
#anova(m14, m13)


#Looking at interactions of family strengths and youth age, sex, race on placement stability 

summary(m16 <- glm.nb(MastersData$PlacementCount ~ MastersData$cans20 + MastersData$Age + MastersData$cans20 * MastersData$Age + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData))
(m16est <- cbind(Estimate = coef(m16), confint(m16)))
negbinirr(MastersData$PlacementCount ~ MastersData$cans20 + MastersData$Age + MastersData$cans20 * MastersData$Age + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData, robust = TRUE)


summary(m17 <- glm.nb(MastersData$PlacementCount ~ MastersData$cans20 + MastersData$Sex + MastersData$cans20 * MastersData$Sex + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData))
(m17est <- cbind(Estimate = coef(m17), confint(m17)))

summary(m18 <- glm.nb(MastersData$PlacementCount ~ MastersData$cans20 + MastersData$race + MastersData$cans20 * MastersData$race + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData))
(m18est <- cbind(Estimate = coef(m18), confint(m18)))

summary(m19 <- glm.nb(formula = MastersData$PlacementCount ~ MastersData$cans20 + MastersData$Age + MastersData$Sex + MastersData$race +
                        MastersData$cans20 * MastersData$race + MastersData$cans20 * 
                        MastersData$Age + MastersData$cans20 * MastersData$Sex + 
                        MastersData$cans20 * MastersData$race * MastersData$Age + 
                        MastersData$cans20 * MastersData$race * MastersData$Sex + 
                        MastersData$cans20 * MastersData$Sex * MastersData$Age + 
                        MastersData$cans20 * MastersData$Age * MastersData$race * 
                        MastersData$Sex + MastersData$EntryAge + MastersData$YearsInCare, data = MastersData))
(m19est <- cbind(Estimate = coef(m19), confint(m19)))

#Run models with Age Categories and Family Strengths
#Run models of family strengths by age cats, race, and sex to compare effect sizes of association of family strengths and placement stability across the categories to see if the effect is larger for one versus another
#Always Save

write.csv(MastersData, file = "~/Desktop/[where it is saved].csv") *

#Look into Bronfenbrenner's ecological model - might explain the relationship (potential) between family strengths and placement type?
