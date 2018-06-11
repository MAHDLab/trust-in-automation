library(plyr)
library(arm)
library(ggplot2)
library(tidyr)
library(tibble)
library(gridExtra)
library(tidyverse)
library(stargazer)
library(grid)

# Load the data
#experiment4 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 4/results03052018.csv", as.is = TRUE) # from Ryan's computer

# political science experiment
experiment4 <- read.csv("/Users/bpwaggo/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 4/results03052018.csv", as.is = TRUE)

# CJ experiment
#### See Ryan's code

####
####
#### Create some variables in similar syntax as CJ Exp
####
####

# Add in the labels for the experiment
experiment4$scenario_num <- 4

# Machine advice for Experiments
experiment4$advice1 <- 11.6
experiment4$advice2 <- 60.7
experiment4$advice3 <- 13.4
experiment4$advice4 <- 14.3

### FIRST - Calculate Distance to Advice: abs(advice - userguess)
# Exp 4
experiment4$avgHuman.DISTANCE <- (abs(experiment4$advice1 - experiment4$Average.Human_1) +
                                    abs(experiment4$advice2 - experiment4$Average.Human_2) +
                                    abs(experiment4$advice3 - experiment4$Average.Human_3) +
                                    abs(experiment4$advice4 - experiment4$Average.Human_4))/4

experiment4$randHuman.DISTANCE <- (abs(experiment4$advice1 - experiment4$Random.Human_1) +
                                     abs(experiment4$advice2 - experiment4$Random.Human_2) +
                                     abs(experiment4$advice3 - experiment4$Random.Human_3) +
                                     abs(experiment4$advice4 - experiment4$Random.Human_4))/4

experiment4$algHuman.DISTANCE <- (abs(experiment4$advice1 - experiment4$Averaging.Algorithm_1) +
                                    abs(experiment4$advice2 - experiment4$Averaging.Algorithm_2) +
                                    abs(experiment4$advice3 - experiment4$Averaging.Algorithm_3) +
                                    abs(experiment4$advice4 - experiment4$Averaging.Algorithm_4))/4

experiment4$lerner.DISTANCE <- (abs(experiment4$advice1 - experiment4$Learning.Algorithm_1) +
                                  abs(experiment4$advice2 - experiment4$Learning.Algorithm_2) +
                                  abs(experiment4$advice3 - experiment4$Learning.Algorithm_3) +
                                  abs(experiment4$advice4 - experiment4$Learning.Algorithm_4))/4

experiment4$avgHuman.DISTANCE2 <- (abs(experiment4$advice1 - experiment4$Anch.Avg.Human_1) +
                                     abs(experiment4$advice2 - experiment4$Anch.Avg.Human_2) +
                                     abs(experiment4$advice3 - experiment4$Anch.Avg.Human_3) +
                                     abs(experiment4$advice4 - experiment4$Anch.Avg.Human_4))/4

experiment4$randHuman.DISTANCE2 <- (abs(experiment4$advice1 - experiment4$Anch.Rand.Human_1) +
                                      abs(experiment4$advice2 - experiment4$Anch.Rand.Human_2) +
                                      abs(experiment4$advice3 - experiment4$Anch.Rand.Human_3) +
                                      abs(experiment4$advice4 - experiment4$Anch.Rand.Human_4))/4

experiment4$algHuman.DISTANCE2 <- (abs(experiment4$advice1 - experiment4$Anch.alg.Combine_1) +
                                     abs(experiment4$advice2 - experiment4$Anch.alg.Combine_2) +
                                     abs(experiment4$advice3 - experiment4$Anch.alg.Combine_3) +
                                     abs(experiment4$advice4 - experiment4$Anch.alg.Combine_4))/4

experiment4$lerner.DISTANCE2 <- (abs(experiment4$advice1 - experiment4$Anch.Full.Alg_1) +
                                   abs(experiment4$advice2 - experiment4$Anch.Full.Alg_2) +
                                   abs(experiment4$advice3 - experiment4$Anch.Full.Alg_3) +
                                   abs(experiment4$advice4 - experiment4$Anch.Full.Alg_4))/4

experiment4$distance <- rowMeans(cbind(experiment4$avgHuman.DISTANCE,experiment4$randHuman.DISTANCE,
                                       experiment4$algHuman.DISTANCE,experiment4$lerner.DISTANCE,
                                       experiment4$avgHuman.DISTANCE2,experiment4$randHuman.DISTANCE2,
                                       experiment4$algHuman.DISTANCE2,experiment4$lerner.DISTANCE2),na.rm=TRUE)



### SECOND (for second DV - but NOT for "Anch." conditions) -- Calculate Weight of Advice: abs(userguesst2 - userguesst1)/abs(advice - userguesst1)
# Exp 4
experiment4$avgHumanWeight <- (abs(experiment4$Initial.Forecast_1 - experiment4$Average.Human_1) / abs(experiment4$advice1 - experiment4$Initial.Forecast_1) +
                                 abs(experiment4$Initial.Forecast_2 - experiment4$Average.Human_2) / abs(experiment4$advice2 - experiment4$Initial.Forecast_2) +
                                 abs(experiment4$Initial.Forecast_3 - experiment4$Average.Human_3) / abs(experiment4$advice3 - experiment4$Initial.Forecast_3) +
                                 abs(experiment4$Initial.Forecast_4 - experiment4$Average.Human_4) / abs(experiment4$advice4 - experiment4$Initial.Forecast_4))/4

experiment4$randHumanWeight <- (abs(experiment4$Initial.Forecast_1 - experiment4$Random.Human_1) / abs(experiment4$advice1 - experiment4$Initial.Forecast_1) +
                                  abs(experiment4$Initial.Forecast_2 - experiment4$Random.Human_2) / abs(experiment4$advice2 - experiment4$Initial.Forecast_2) +
                                  abs(experiment4$Initial.Forecast_3 - experiment4$Random.Human_3) / abs(experiment4$advice3 - experiment4$Initial.Forecast_3) +
                                  abs(experiment4$Initial.Forecast_4 - experiment4$Random.Human_4) / abs(experiment4$advice4 - experiment4$Initial.Forecast_4))/4

experiment4$algHumanWeight <- (abs(experiment4$Initial.Forecast_1 - experiment4$Averaging.Algorithm_1) / abs(experiment4$advice1 - experiment4$Initial.Forecast_1) +
                                 abs(experiment4$Initial.Forecast_2 - experiment4$Averaging.Algorithm_2) / abs(experiment4$advice2 - experiment4$Initial.Forecast_2) +
                                 abs(experiment4$Initial.Forecast_3 - experiment4$Averaging.Algorithm_3) / abs(experiment4$advice3 - experiment4$Initial.Forecast_3) +
                                 abs(experiment4$Initial.Forecast_4 - experiment4$Averaging.Algorithm_4) / abs(experiment4$advice4 - experiment4$Initial.Forecast_4))/4

experiment4$lernerWeight <- (abs(experiment4$Initial.Forecast_1 - experiment4$Learning.Algorithm_1) / abs(experiment4$advice1 - experiment4$Initial.Forecast_1) +
                               abs(experiment4$Initial.Forecast_2 - experiment4$Learning.Algorithm_2) / abs(experiment4$advice2 - experiment4$Initial.Forecast_2) +
                               abs(experiment4$Initial.Forecast_3 - experiment4$Learning.Algorithm_3) / abs(experiment4$advice3 - experiment4$Initial.Forecast_3) +
                               abs(experiment4$Initial.Forecast_4 - experiment4$Learning.Algorithm_4) / abs(experiment4$advice4 - experiment4$Initial.Forecast_4))/4

experiment4$adviceWt <- rowMeans(cbind(experiment4$avgHumanWeight,experiment4$randHumanWeight,
                                       experiment4$algHumanWeight,experiment4$lernerWeight),na.rm=TRUE)


## THIRD - Calculate Brier scores as robustness check/secondary analysis
# Calculate Brier Distance to Advice
# Exp 4
experiment4$avgHumanBrier <- ((0.01*experiment4$Average.Human_1 - 0.01*experiment4$advice1)^2 +
                                (0.01*experiment4$Average.Human_2 - 0.01*experiment4$advice2)^2 +
                                (0.01*experiment4$Average.Human_3 - 0.01*experiment4$advice3)^2 +
                                (0.01*experiment4$Average.Human_4 - 0.01*experiment4$advice4)^2)/4

experiment4$randHumanBrier <- ((0.01*experiment4$Random.Human_1 - 0.01*experiment4$advice1)^2 +
                                 (0.01*experiment4$Random.Human_2 - 0.01*experiment4$advice2)^2 +
                                 (0.01*experiment4$Random.Human_3 - 0.01*experiment4$advice3)^2 +
                                 (0.01*experiment4$Random.Human_4 - 0.01*experiment4$advice4)^2)/4

experiment4$algHumanBrier <- ((0.01*experiment4$Averaging.Algorithm_1 - 0.01*experiment4$advice1)^2 +
                                (0.01*experiment4$Averaging.Algorithm_2 - 0.01*experiment4$advice2)^2 +
                                (0.01*experiment4$Averaging.Algorithm_3 - 0.01*experiment4$advice3)^2 +
                                (0.01*experiment4$Averaging.Algorithm_4 - 0.01*experiment4$advice4)^2)/4

experiment4$lernerBrier <- ((0.01*experiment4$Learning.Algorithm_1 - 0.01*experiment4$advice1)^2 +
                              (0.01*experiment4$Learning.Algorithm_2 - 0.01*experiment4$advice2)^2 +
                              (0.01*experiment4$Learning.Algorithm_3 - 0.01*experiment4$advice3)^2 +
                              (0.01*experiment4$Learning.Algorithm_4 - 0.01*experiment4$advice4)^2)/4

experiment4$avgHumanBrier2 <- ((0.01*experiment4$Anch.Avg.Human_1 - 0.01*experiment4$advice1)^2 +
                                 (0.01*experiment4$Anch.Avg.Human_2 - 0.01*experiment4$advice2)^2 +
                                 (0.01*experiment4$Anch.Avg.Human_3 - 0.01*experiment4$advice3)^2 +
                                 (0.01*experiment4$Anch.Avg.Human_4 - 0.01*experiment4$advice4)^2)/4

experiment4$randHumanBrier2 <- ((0.01*experiment4$Anch.Rand.Human_1 - 0.01*experiment4$advice1)^2 +
                                  (0.01*experiment4$Anch.Rand.Human_2 - 0.01*experiment4$advice2)^2 +
                                  (0.01*experiment4$Anch.Rand.Human_3 - 0.01*experiment4$advice3)^2 +
                                  (0.01*experiment4$Anch.Rand.Human_4 - 0.01*experiment4$advice4)^2)/4

experiment4$algHumanBrier2 <- ((0.01*experiment4$Anch.alg.Combine_1 - 0.01*experiment4$advice1)^2 +
                                 (0.01*experiment4$Anch.alg.Combine_2 - 0.01*experiment4$advice2)^2 +
                                 (0.01*experiment4$Anch.alg.Combine_3 - 0.01*experiment4$advice3)^2 +
                                 (0.01*experiment4$Anch.alg.Combine_4 - 0.01*experiment4$advice4)^2)/4

experiment4$lernerBrier2 <- ((0.01*experiment4$Anch.Full.Alg_1 - 0.01*experiment4$advice1)^2 +
                               (0.01*experiment4$Anch.Full.Alg_2 - 0.01*experiment4$advice2)^2 +
                               (0.01*experiment4$Anch.Full.Alg_3 - 0.01*experiment4$advice3)^2 +
                               (0.01*experiment4$Anch.Full.Alg_4 - 0.01*experiment4$advice4)^2)/4

experiment4$brier <- rowMeans(cbind(experiment4$avgHumanBrier,experiment4$randHumanBrier,
                                    experiment4$algHumanBrier,experiment4$lernerBrier,
                                    experiment4$avgHumanBrier2,experiment4$randHumanBrier2,
                                    experiment4$algHumanBrier2,experiment4$lernerBrier2),na.rm=TRUE)


# Combine the data from the experiments
experiments <- experiment4


# Add in treatment indicators
experiments$AvgHumanTreat <- ifelse(!is.na(experiments$avgHuman.DISTANCE) | 
                                      !is.na(experiments$avgHuman.DISTANCE2) |
                                      !is.na(experiments$avgHumanWeight), 1, 0) 

experiments$RandHumanTreat <- ifelse(!is.na(experiments$randHuman.DISTANCE) | 
                                       !is.na(experiments$randHuman.DISTANCE2) |
                                       !is.na(experiments$randHumanWeight), 1, 0) 

experiments$AlgHumanTreat <- ifelse(!is.na(experiments$algHuman.DISTANCE) | 
                                      !is.na(experiments$algHuman.DISTANCE2) |
                                      !is.na(experiments$algHumanWeight), 1, 0) 

experiments$LernerTreat <- ifelse(!is.na(experiments$lerner.DISTANCE) | 
                                    !is.na(experiments$lerner.DISTANCE2) |
                                    !is.na(experiments$lernerWeight), 1, 0) 

experiments$Anchoring <- ifelse(!is.na(experiments$avgHuman.DISTANCE2) |
                                  !is.na(experiments$randHuman.DISTANCE2) |
                                  !is.na(experiments$algHuman.DISTANCE2) |
                                  !is.na(experiments$lerner.DISTANCE2) |
                                  !is.na(experiments$avgHumanBrier2) |
                                  !is.na(experiments$randHumanBrier2) |
                                  !is.na(experiments$algHumanBrier2) |
                                  !is.na(experiments$lernerBrier2), 1, 0)


# Gender
experiments$female[experiments$Gender == "Male"] <- 0
experiments$female[experiments$Gender == "Female"] <- 1

# Education -- NOTE: there is no choice "Elementary of some high school" in the political experiments like there is in the CJ experiment; Thus, category 1 in the political Exps = HS/Trade School
experiments$edvalue[experiments$education == "High school graduate/GED" ] <- 1
experiments$edvalue[experiments$education == "Trade or vocational certification" ] <- 1
experiments$edvalue[experiments$education == "Some college, or an associate degree"] <- 2
experiments$edvalue[experiments$education == "Bachelor's degree (for example, BA, AB, BS)"] <- 2
experiments$edvalue[experiments$education == "Some graduate school" ] <- 3
experiments$edvalue[experiments$education == "Master's degree (for example, MA, MSW, MBA)" ] <- 3
experiments$edvalue[experiments$education == "Professional degree (for example, MD, JD, DDS)" ] <- 3
experiments$edvalue[experiments$education == "Doctoral degree (for example, PhD, EdD)" ] <- 3
#experiments$edvalue[experiments$education == ""] <- 0

experiments$ed <- experiments$edvalue


# Trust in Automation
experiments$toa1[experiments$Trust.Automation_1 == "Strongly agree"] <- 7
experiments$toa1[experiments$Trust.Automation_1 == "Agree"] <- 6
experiments$toa1[experiments$Trust.Automation_1 == "Somewhat agree"] <- 5
experiments$toa1[experiments$Trust.Automation_1 == "Neither agree nor disagree"] <- 4
experiments$toa1[experiments$Trust.Automation_1 == "Somewhat disagree"] <- 3
experiments$toa1[experiments$Trust.Automation_1 == "Disagree"] <- 2
experiments$toa1[experiments$Trust.Automation_1 == "Strongly disagree"] <- 1

experiments$toa2[experiments$Trust.Automation_2 == "Strongly agree"] <- 7
experiments$toa2[experiments$Trust.Automation_2 == "Agree"] <- 6
experiments$toa2[experiments$Trust.Automation_2 == "Somewhat agree"] <- 5
experiments$toa2[experiments$Trust.Automation_2 == "Neither agree nor disagree"] <- 4
experiments$toa2[experiments$Trust.Automation_2 == "Somewhat disagree"] <- 3
experiments$toa2[experiments$Trust.Automation_2 == "Disagree"] <- 2
experiments$toa2[experiments$Trust.Automation_2 == "Strongly disagree"] <- 1

experiments$toa3[experiments$Trust.Automation_3 == "Strongly agree"] <- 7
experiments$toa3[experiments$Trust.Automation_3 == "Agree"] <- 6
experiments$toa3[experiments$Trust.Automation_3 == "Somewhat agree"] <- 5
experiments$toa3[experiments$Trust.Automation_3 == "Neither agree nor disagree"] <- 4
experiments$toa3[experiments$Trust.Automation_3 == "Somewhat disagree"] <- 3
experiments$toa3[experiments$Trust.Automation_3 == "Disagree"] <- 2
experiments$toa3[experiments$Trust.Automation_3 == "Strongly disagree"] <- 1

experiments$toa4[experiments$Trust.Automation_4 == "Strongly agree"] <- 1
experiments$toa4[experiments$Trust.Automation_4 == "Agree"] <- 2
experiments$toa4[experiments$Trust.Automation_4 == "Somewhat agree"] <- 3
experiments$toa4[experiments$Trust.Automation_4 == "Neither agree nor disagree"] <- 4
experiments$toa4[experiments$Trust.Automation_4 == "Somewhat disagree"] <- 5
experiments$toa4[experiments$Trust.Automation_4 == "Disagree"] <- 6
experiments$toa4[experiments$Trust.Automation_4 == "Strongly disagree"] <- 7

experiments$toa5[experiments$Trust.Automation_5 == "Strongly agree"] <- 1
experiments$toa5[experiments$Trust.Automation_5 == "Agree"] <- 2
experiments$toa5[experiments$Trust.Automation_5 == "Somewhat agree"] <- 3
experiments$toa5[experiments$Trust.Automation_5 == "Neither agree nor disagree"] <- 4
experiments$toa5[experiments$Trust.Automation_5 == "Somewhat disagree"] <- 5
experiments$toa5[experiments$Trust.Automation_5 == "Disagree"] <- 6
experiments$toa5[experiments$Trust.Automation_5 == "Strongly disagree"] <- 7

experiments$toa6[experiments$Trust.Automation_6 == "Strongly agree"] <- 7
experiments$toa6[experiments$Trust.Automation_6 == "Agree"] <- 6
experiments$toa6[experiments$Trust.Automation_6 == "Somewhat agree"] <- 5
experiments$toa6[experiments$Trust.Automation_6 == "Neither agree nor disagree"] <- 4
experiments$toa6[experiments$Trust.Automation_6 == "Somewhat disagree"] <- 3
experiments$toa6[experiments$Trust.Automation_6 == "Disagree"] <- 2
experiments$toa6[experiments$Trust.Automation_6 == "Strongly disagree"] <- 1

experiments$toa7[experiments$Trust.Automation_7 == "Strongly agree"] <- 7
experiments$toa7[experiments$Trust.Automation_7 == "Agree"] <- 6
experiments$toa7[experiments$Trust.Automation_7 == "Somewhat agree"] <- 5
experiments$toa7[experiments$Trust.Automation_7 == "Neither agree nor disagree"] <- 4
experiments$toa7[experiments$Trust.Automation_7 == "Somewhat disagree"] <- 3
experiments$toa7[experiments$Trust.Automation_7 == "Disagree"] <- 2
experiments$toa7[experiments$Trust.Automation_7 == "Strongly disagree"] <- 1

experiments$toa8[experiments$Trust.Automation_8 == "Strongly agree"] <- 1
experiments$toa8[experiments$Trust.Automation_8 == "Agree"] <- 2
experiments$toa8[experiments$Trust.Automation_8 == "Somewhat agree"] <- 3
experiments$toa8[experiments$Trust.Automation_8 == "Neither agree nor disagree"] <- 4
experiments$toa8[experiments$Trust.Automation_8 == "Somewhat disagree"] <- 5
experiments$toa8[experiments$Trust.Automation_8 == "Disagree"] <- 6
experiments$toa8[experiments$Trust.Automation_8 == "Strongly disagree"] <- 7

experiments$trustAutomation = (experiments$toa1 + experiments$toa2 + experiments$toa3 + experiments$toa4 + experiments$toa5 + experiments$toa6 + experiments$toa7 + experiments$toa8)/8

experiments$tia <- experiments$trustAutomation


# Party
experiments$partisanship[experiments$partyLean == "I lean neither way"] <- 4
experiments$partisanship[experiments$partyLean == "Lean Democrat"] <- 3
experiments$partisanship[experiments$partyLean == "Lean Republican"] <- 5
experiments$partisanship[experiments$partyID == "Republican" & experiments$partyStrong == "No, NOT strongly"] <- 6
experiments$partisanship[experiments$partyID == "Democrat" & experiments$partyStrong == "No, NOT strongly"] <- 2
experiments$partisanship[experiments$partyID == "Republican" & experiments$partyStrong == "Yes, strongly"] <- 7
experiments$partisanship[experiments$partyID == "Democrat" & experiments$partyStrong == "Yes, strongly"] <- 1


# Age
experiments$age <- experiments$Age


# Personality: TIPI - Only on Exp. 4 - UPDATE ALL CODE BELOW
### EXTRAVERSION
#### TIPI 1
#experiments$TIPI_1[experiments$TIPI_1 == ""] <- 0
experiments$TIPI_1[experiments$TIPI_1 == "Disagree strongly"] <- 1
experiments$TIPI_1[experiments$TIPI_1 == "Disagree moderately"] <- 2
experiments$TIPI_1[experiments$TIPI_1 == "Disagree a little"] <- 3
experiments$TIPI_1[experiments$TIPI_1 == "Neither agree nor diagree"] <- 4
experiments$TIPI_1[experiments$TIPI_1 == "Agree a little"] <- 5
experiments$TIPI_1[experiments$TIPI_1 == "Agree moderately"] <- 6
experiments$TIPI_1[experiments$TIPI_1 == "Agree strongly"] <- 7
#### TIPI 6 (reversed; see Gosling et al for more on coding)
#experiments$TIPI_6[experiments$TIPI_6 == ""] <- 0
experiments$TIPI_6[experiments$TIPI_6 == "Disagree strongly"] <- 7
experiments$TIPI_6[experiments$TIPI_6 == "Disagree moderately"] <- 6
experiments$TIPI_6[experiments$TIPI_6 == "Disagree a little"] <- 5
experiments$TIPI_6[experiments$TIPI_6 == "Neither agree nor diagree"] <- 4
experiments$TIPI_6[experiments$TIPI_6 == "Agree a little"] <- 3
experiments$TIPI_6[experiments$TIPI_6 == "Agree moderately"] <- 2
experiments$TIPI_6[experiments$TIPI_6 == "Agree strongly"] <- 1


### AGREEABLENESS
#### TIPI 7
#experiments$TIPI_7[experiments$TIPI_7 == ""] <- 0
experiments$TIPI_7[experiments$TIPI_7 == "Disagree strongly"] <- 1
experiments$TIPI_7[experiments$TIPI_7 == "Disagree moderately"] <- 2
experiments$TIPI_7[experiments$TIPI_7 == "Disagree a little"] <- 3
experiments$TIPI_7[experiments$TIPI_7 == "Neither agree nor diagree"] <- 4
experiments$TIPI_7[experiments$TIPI_7 == "Agree a little"] <- 5
experiments$TIPI_7[experiments$TIPI_7 == "Agree moderately"] <- 6
experiments$TIPI_7[experiments$TIPI_7 == "Agree strongly"] <- 7
#### TIPI 2 (reversed; see Gosling et al for more on coding)
#experiments$TIPI_2[experiments$TIPI_2 == ""] <- 0
experiments$TIPI_2[experiments$TIPI_2 == "Disagree strongly"] <- 7
experiments$TIPI_2[experiments$TIPI_2 == "Disagree moderately"] <- 6
experiments$TIPI_2[experiments$TIPI_2 == "Disagree a little"] <- 5
experiments$TIPI_2[experiments$TIPI_2 == "Neither agree nor diagree"] <- 4
experiments$TIPI_2[experiments$TIPI_2 == "Agree a little"] <- 3
experiments$TIPI_2[experiments$TIPI_2 == "Agree moderately"] <- 2
experiments$TIPI_2[experiments$TIPI_2 == "Agree strongly"] <- 1


### CONSCIENTIOUSNESS
#### TIPI 3
#experiments$TIPI_3[experiments$TIPI_3 == ""] <- 0
experiments$TIPI_3[experiments$TIPI_3 == "Disagree strongly"] <- 1
experiments$TIPI_3[experiments$TIPI_3 == "Disagree moderately"] <- 2
experiments$TIPI_3[experiments$TIPI_3 == "Disagree a little"] <- 3
experiments$TIPI_3[experiments$TIPI_3 == "Neither agree nor diagree"] <- 4
experiments$TIPI_3[experiments$TIPI_3 == "Agree a little"] <- 5
experiments$TIPI_3[experiments$TIPI_3 == "Agree moderately"] <- 6
experiments$TIPI_3[experiments$TIPI_3 == "Agree strongly"] <- 7
#### TIPI 8 (reversed; see Gosling et al for more on coding)
#experiments$TIPI_8[experiments$TIPI_8 == ""] <- 0
experiments$TIPI_8[experiments$TIPI_8 == "Disagree strongly"] <- 7
experiments$TIPI_8[experiments$TIPI_8 == "Disagree moderately"] <- 6
experiments$TIPI_8[experiments$TIPI_8 == "Disagree a little"] <- 5
experiments$TIPI_8[experiments$TIPI_8 == "Neither agree nor diagree"] <- 4
experiments$TIPI_8[experiments$TIPI_8 == "Agree a little"] <- 3
experiments$TIPI_8[experiments$TIPI_8 == "Agree moderately"] <- 2
experiments$TIPI_8[experiments$TIPI_8 == "Agree strongly"] <- 1


### EMOTIONAL STABILITY
#### TIPI 9
#experiments$TIPI_9[experiments$TIPI_9 == ""] <- 0
experiments$TIPI_9[experiments$TIPI_9 == "Disagree strongly"] <- 1
experiments$TIPI_9[experiments$TIPI_9 == "Disagree moderately"] <- 2
experiments$TIPI_9[experiments$TIPI_9 == "Disagree a little"] <- 3
experiments$TIPI_9[experiments$TIPI_9 == "Neither agree nor diagree"] <- 4
experiments$TIPI_9[experiments$TIPI_9 == "Agree a little"] <- 5
experiments$TIPI_9[experiments$TIPI_9 == "Agree moderately"] <- 6
experiments$TIPI_9[experiments$TIPI_9 == "Agree strongly"] <- 7
#### TIPI 4 (reversed; see Gosling et al for more on coding)
#experiments$TIPI_4[experiments$TIPI_4 == ""] <- 0
experiments$TIPI_4[experiments$TIPI_4 == "Disagree strongly"] <- 7
experiments$TIPI_4[experiments$TIPI_4 == "Disagree moderately"] <- 6
experiments$TIPI_4[experiments$TIPI_4 == "Disagree a little"] <- 5
experiments$TIPI_4[experiments$TIPI_4 == "Neither agree nor diagree"] <- 4
experiments$TIPI_4[experiments$TIPI_4 == "Agree a little"] <- 3
experiments$TIPI_4[experiments$TIPI_4 == "Agree moderately"] <- 2
experiments$TIPI_4[experiments$TIPI_4 == "Agree strongly"] <- 1


### OPENNESS TO EXPERIENCES
#### TIPI 5
#experiments$TIPI_5[experiments$TIPI_5 == ""] <- 0
experiments$TIPI_5[experiments$TIPI_5 == "Disagree strongly"] <- 1
experiments$TIPI_5[experiments$TIPI_5 == "Disagree moderately"] <- 2
experiments$TIPI_5[experiments$TIPI_5 == "Disagree a little"] <- 3
experiments$TIPI_5[experiments$TIPI_5 == "Neither agree nor diagree"] <- 4
experiments$TIPI_5[experiments$TIPI_5 == "Agree a little"] <- 5
experiments$TIPI_5[experiments$TIPI_5 == "Agree moderately"] <- 6
experiments$TIPI_5[experiments$TIPI_5 == "Agree strongly"] <- 7
#### TIPI 10 (reversed; see Gosling et al for more on coding)
#experiments$TIPI_10[experiments$TIPI_10 == ""] <- 0
experiments$TIPI_10[experiments$TIPI_10 == "Disagree strongly"] <- 7
experiments$TIPI_10[experiments$TIPI_10 == "Disagree moderately"] <- 6
experiments$TIPI_10[experiments$TIPI_10 == "Disagree a little"] <- 5
experiments$TIPI_10[experiments$TIPI_10 == "Neither agree nor diagree"] <- 4
experiments$TIPI_10[experiments$TIPI_10 == "Agree a little"] <- 3
experiments$TIPI_10[experiments$TIPI_10 == "Agree moderately"] <- 2
experiments$TIPI_10[experiments$TIPI_10 == "Agree strongly"] <- 1


experiments$extroverted = (as.numeric(experiments$TIPI_1) + as.numeric(experiments$TIPI_6))/2
experiments$agreeableness = (as.numeric(experiments$TIPI_2) + as.numeric(experiments$TIPI_7))/2
experiments$openness = (as.numeric(experiments$TIPI_5) + as.numeric(experiments$TIPI_10))/2
experiments$conscientiousness = (as.numeric(experiments$TIPI_3) + as.numeric(experiments$TIPI_8))/2
experiments$stability = (as.numeric(experiments$TIPI_4) + as.numeric(experiments$TIPI_9))/2


# Personality: Cognition and Judgement - ONLY IN 4 - exclude from full analysis
experiments$needcog1[experiments$CogEval_1 == "Extremely characteristic"] <- 5
experiments$needcog1[experiments$CogEval_1 == "Somewhat characteristic"] <- 4
experiments$needcog1[experiments$CogEval_1 == "Uncertain"] <- 3
experiments$needcog1[experiments$CogEval_1 == "Somewhat uncharacteristic"] <- 2
experiments$needcog1[experiments$CogEval_1 == "Extremely uncharacteristic"] <- 1

experiments$needcog2[experiments$CogEval_2 == "Extremely characteristic"] <- 5
experiments$needcog2[experiments$CogEval_2 == "Somewhat characteristic"] <- 4
experiments$needcog2[experiments$CogEval_2 == "Uncertain"] <- 3
experiments$needcog2[experiments$CogEval_2 == "Somewhat uncharacteristic"] <- 2
experiments$needcog2[experiments$CogEval_2 == "Extremely uncharacteristic"] <- 1

experiments$needcog3[experiments$CogEval_3 == "Extremely characteristic"] <- 5
experiments$needcog3[experiments$CogEval_3 == "Somewhat characteristic"] <- 4
experiments$needcog3[experiments$CogEval_3 == "Uncertain"] <- 3
experiments$needcog3[experiments$CogEval_3 == "Somewhat uncharacteristic"] <- 2
experiments$needcog3[experiments$CogEval_3 == "Extremely uncharacteristic"] <- 1

experiments$needcog4[experiments$CogEval_4 == "Extremely characteristic"] <- 1
experiments$needcog4[experiments$CogEval_4 == "Somewhat characteristic"] <- 2
experiments$needcog4[experiments$CogEval_4 == "Uncertain"] <- 3
experiments$needcog4[experiments$CogEval_4 == "Somewhat uncharacteristic"] <- 4
experiments$needcog4[experiments$CogEval_4 == "Extremely uncharacteristic"] <- 5


experiments$needjudge = (experiments$needcog1 + experiments$needcog3)/2

experiments$needcog = (experiments$needcog2 + experiments$needcog4)/2


## MODELS - predicting treatment/advice impacts
model1 <- lmer(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + 
                 tia + age + ed + female + partisanship + 
                 needcog + needjudge +
                 extroverted + agreeableness + openness + conscientiousness + stability
                                   (1|ResponseId) + (1|scenario),    
               data = experiments); summary(model1)

model2.cog <- lmer(tia ~ needcog + needjudge +
                                       (1|ResponseId) + (1|scenario),    
               data = experiments[experiments$adviceWt <= 1,]); summary(model2.cog)

model2.b5 <- lmer(tia ~ extroverted + agreeableness + openness + conscientiousness + stability +
                                      (1|ResponseId) + (1|scenario),    
               data = experiments[experiments$adviceWt <= 1,]); summary(model2.b5)

model2.full <- lmer(tia ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + 
                 age + ed + female + partisanship + 
                 needcog + needjudge +
                 extroverted + agreeableness + openness + conscientiousness + stability +
                                        (1|ResponseId) + (1|scenario),    
                data = experiments[experiments$adviceWt <= 1,]); summary(model2.full)

model2.inxn <- lmer(tia ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + 
                 age + ed + female + partisanship + 
                 needcog + needjudge +
                 extroverted + agreeableness + openness + conscientiousness + stability +
               LernerTreat*extroverted + LernerTreat*agreeableness + LernerTreat*openness + LernerTreat*conscientiousness + LernerTreat*stability +
                    (1|ResponseId) + (1|scenario),              
                    data = experiments[experiments$adviceWt <= 1,]); summary(model2.inxn)



