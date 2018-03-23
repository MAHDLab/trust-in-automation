# Analysis of Trust in Automation Experiments.

# Load Needed Libraries
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
#experiment1 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 1/results01112018.csv", as.is = TRUE)
#experiment2 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 2/results01112018.csv", as.is = TRUE)
#experiment3 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 3/results01112018.csv", as.is = TRUE)
#experiment4 <- read.csv("C:/Users/Ryan/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 4/results03052018.csv", as.is = TRUE)

# From P's DB for quick access
experiment1 <- read.csv("/Users/bpwaggo/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 1/results01112018.csv", as.is = TRUE)
experiment2 <- read.csv("/Users/bpwaggo/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 2/results01112018.csv", as.is = TRUE)
experiment3 <- read.csv("/Users/bpwaggo/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 3/results01112018.csv", as.is = TRUE)
experiment4 <- read.csv("/Users/bpwaggo/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 4/results03052018.csv", as.is = TRUE)


####
####
#### Create some variables in similar syntax as CJ Exp
####
####

# Add in the labels for the experiment
experiment1$scenario_num <- 1
experiment2$scenario_num <- 2
experiment3$scenario_num <- 3
experiment4$scenario_num <- 4

# Machine advice for Experiments
experiment1$advice1 <- 1
experiment1$advice2 <- 9
experiment1$advice3 <- 50
experiment1$advice4 <- 28
experiment1$advice5 <- 2

experiment2$advice1 <- 79.5
experiment2$advice2 <- 20.5

experiment3$advice1 <- 25.6
experiment3$advice2 <- 25.7
experiment3$advice3 <- 34.0
experiment3$advice4 <- 14.7

experiment4$advice1 <- 11.6
experiment4$advice2 <- 60.7
experiment4$advice3 <- 13.4
experiment4$advice4 <- 14.3

### FIRST - Calculate Distance to Advice: abs(advice - userguess)
# Exp 1
experiment1$avgHuman.DISTANCE <- (abs(experiment1$advice1 - experiment1$Average.Human_1) +
                                    abs(experiment1$advice2 - experiment1$Average.Human_2) +
                                    abs(experiment1$advice3 - experiment1$Average.Human_3) +
                                    abs(experiment1$advice4 - experiment1$Average.Human_4) +
                                    abs(experiment1$advice5 - experiment1$Average.Human_5))/5

experiment1$randHuman.DISTANCE <- (abs(experiment1$advice1 - experiment1$Random.Human_1) +
                                     abs(experiment1$advice2 - experiment1$Random.Human_2) +
                                     abs(experiment1$advice3 - experiment1$Random.Human_3) +
                                     abs(experiment1$advice4 - experiment1$Random.Human_4) +
                                     abs(experiment1$advice5 - experiment1$Random.Human_5))/5

experiment1$algHuman.DISTANCE <- (abs(experiment1$advice1 - experiment1$Averaging.Algorithm_1) +
                                    abs(experiment1$advice2 - experiment1$Averaging.Algorithm_2) +
                                    abs(experiment1$advice3 - experiment1$Averaging.Algorithm_3) +
                                    abs(experiment1$advice4 - experiment1$Averaging.Algorithm_4) +
                                    abs(experiment1$advice5 - experiment1$Averaging.Algorithm_5))/5

experiment1$lerner.DISTANCE <- (abs(experiment1$advice1 - experiment1$Learning.Algorithm_1) +
                                  abs(experiment1$advice2 - experiment1$Learning.Algorithm_2) +
                                  abs(experiment1$advice3 - experiment1$Learning.Algorithm_3) +
                                  abs(experiment1$advice4 - experiment1$Learning.Algorithm_4) +
                                  abs(experiment1$advice5 - experiment1$Learning.Algorithm_5))/5

experiment1$distance <- rowMeans(cbind(experiment1$avgHuman.DISTANCE,experiment1$randHuman.DISTANCE,
                                    experiment1$algHuman.DISTANCE,experiment1$lerner.DISTANCE),na.rm=TRUE)

# Exp 2
experiment2$avgHuman.DISTANCE <- (abs(experiment2$advice1 - experiment2$Average.Human_1) +
                                abs(experiment2$advice2 - experiment2$Average.Human_2))/2

experiment2$randHuman.DISTANCE <- (abs(experiment2$advice1 - experiment2$Random.Human_1) +
                                     abs(experiment2$advice2 - experiment2$Random.Human_2))/2

experiment2$algHuman.DISTANCE <- (abs(experiment2$advice1 - experiment2$Averaging.Algorithm_1) +
                                    abs(experiment2$advice2 - experiment2$Averaging.Algorithm_2))/2

experiment2$lerner.DISTANCE <- (abs(experiment2$advice1 - experiment2$Learning.Algorithm_1) +
                                  abs(experiment2$advice2 - experiment2$Learning.Algorithm_2))/2

experiment2$avgHuman.DISTANCE2 <- (abs(experiment2$advice1 - experiment2$Anch.Avg.Human_1) +
                                     abs(experiment2$advice2 - experiment2$Anch.Avg.Human_2))/2

experiment2$randHuman.DISTANCE2 <- (abs(experiment2$advice1 - experiment2$Anch.Rand.Human_1) +
                                      abs(experiment2$advice2 - experiment2$Anch.Rand.Human_2))/2

experiment2$algHuman.DISTANCE2 <- (abs(experiment2$advice1 - experiment2$Anch.alg.Combine_1) +
                                     abs(experiment2$advice2 - experiment2$Anch.alg.Combine_2))/2

experiment2$lerner.DISTANCE2 <- (abs(experiment2$advice1 - experiment2$Anch.Full.Alg_1) +
                                   abs(experiment2$advice2 - experiment2$Anch.Full.Alg_2))/2

experiment2$distance <- rowMeans(cbind(experiment2$avgHuman.DISTANCE,experiment2$randHuman.DISTANCE,
                                    experiment2$algHuman.DISTANCE,experiment2$lerner.DISTANCE,
                                    experiment2$avgHuman.DISTANCE2,experiment2$randHuman.DISTANCE2,
                                    experiment2$algHuman.DISTANCE2,experiment2$lerner.DISTANCE2),na.rm=TRUE)

# Exp 3
experiment3$avgHuman.DISTANCE <- (abs(experiment3$advice1 - experiment3$Average.Human_1) +
                                    abs(experiment3$advice2 - experiment3$Average.Human_2) +
                                    abs(experiment3$advice3 - experiment3$Average.Human_3) +
                                    abs(experiment3$advice4 - experiment3$Average.Human_4))/4

experiment3$randHuman.DISTANCE <- (abs(experiment3$advice1 - experiment3$Random.Human_1) +
                                     abs(experiment3$advice2 - experiment3$Random.Human_2) +
                                     abs(experiment3$advice3 - experiment3$Random.Human_3) +
                                     abs(experiment3$advice4 - experiment3$Random.Human_4))/4

experiment3$algHuman.DISTANCE <- (abs(experiment3$advice1 - experiment3$Averaging.Algorithm_1) +
                                    abs(experiment3$advice2 - experiment3$Averaging.Algorithm_2) +
                                    abs(experiment3$advice3 - experiment3$Averaging.Algorithm_3) +
                                    abs(experiment3$advice4 - experiment3$Averaging.Algorithm_4))/4

experiment3$lerner.DISTANCE <- (abs(experiment3$advice1 - experiment3$Learning.Algorithm_1) +
                                  abs(experiment3$advice2 - experiment3$Learning.Algorithm_2) +
                                  abs(experiment3$advice3 - experiment3$Learning.Algorithm_3) +
                                  abs(experiment3$advice4 - experiment3$Learning.Algorithm_4))/4

experiment3$avgHuman.DISTANCE2 <- (abs(experiment3$advice1 - experiment3$Anch.Avg.Human_1) +
                                     abs(experiment3$advice2 - experiment3$Anch.Avg.Human_2) +
                                     abs(experiment3$advice3 - experiment3$Anch.Avg.Human_3) +
                                     abs(experiment3$advice4 - experiment3$Anch.Avg.Human_4))/4

experiment3$randHuman.DISTANCE2 <- (abs(experiment3$advice1 - experiment3$Anch.Rand.Human_1) +
                                      abs(experiment3$advice2 - experiment3$Anch.Rand.Human_2) +
                                      abs(experiment3$advice3 - experiment3$Anch.Rand.Human_3) +
                                      abs(experiment3$advice4 - experiment3$Anch.Rand.Human_4))/4

experiment3$algHuman.DISTANCE2 <- (abs(experiment3$advice1 - experiment3$Anch.alg.Combine_1) +
                                     abs(experiment3$advice2 - experiment3$Anch.alg.Combine_2) +
                                     abs(experiment3$advice3 - experiment3$Anch.alg.Combine_3) +
                                     abs(experiment3$advice4 - experiment3$Anch.alg.Combine_4))/4

experiment3$lerner.DISTANCE2 <- (abs(experiment3$advice1 - experiment3$Anch.Full.Alg_1) +
                                   abs(experiment3$advice2 - experiment3$Anch.Full.Alg_2) +
                                   abs(experiment3$advice3 - experiment3$Anch.Full.Alg_3) +
                                   abs(experiment3$advice4 - experiment3$Anch.Full.Alg_4))/4

experiment3$distance <- rowMeans(cbind(experiment3$avgHuman.DISTANCE,experiment3$randHuman.DISTANCE,
                                    experiment3$algHuman.DISTANCE,experiment3$lerner.DISTANCE,
                                    experiment3$avgHuman.DISTANCE2,experiment3$randHuman.DISTANCE2,
                                    experiment3$algHuman.DISTANCE2,experiment3$lerner.DISTANCE2),na.rm=TRUE)


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
# Exp 1
experiment1$avgHumanWeight <- (abs(experiment1$Initial.Forecast_1 - experiment1$Average.Human_1) / abs(experiment1$advice1 - experiment1$Initial.Forecast_1) +
                                 abs(experiment1$Initial.Forecast_2 - experiment1$Average.Human_2) / abs(experiment1$advice2 - experiment1$Initial.Forecast_2) +
                                 abs(experiment1$Initial.Forecast_3 - experiment1$Average.Human_3) / abs(experiment1$advice3 - experiment1$Initial.Forecast_3) +
                                 abs(experiment1$Initial.Forecast_4 - experiment1$Average.Human_4) / abs(experiment1$advice4 - experiment1$Initial.Forecast_4) +
                                 abs(experiment1$Initial.Forecast_5 - experiment1$Average.Human_5) / abs(experiment1$advice5 - experiment1$Initial.Forecast_5))/5

experiment1$avgHumanWeight <- (abs(experiment1$Initial.Forecast_1 - experiment1$Random.Human_1) / abs(experiment1$advice1 - experiment1$Initial.Forecast_1) +
                                 abs(experiment1$Initial.Forecast_2 - experiment1$Random.Human_2) / abs(experiment1$advice2 - experiment1$Initial.Forecast_2) +
                                 abs(experiment1$Initial.Forecast_3 - experiment1$Random.Human_3) / abs(experiment1$advice3 - experiment1$Initial.Forecast_3) +
                                 abs(experiment1$Initial.Forecast_4 - experiment1$Random.Human_4) / abs(experiment1$advice4 - experiment1$Initial.Forecast_4) +
                                 abs(experiment1$Initial.Forecast_5 - experiment1$Random.Human_5) / abs(experiment1$advice5 - experiment1$Initial.Forecast_5))/5

experiment1$avgHumanWeight <- (abs(experiment1$Initial.Forecast_1 - experiment1$Averaging.Algorithm_1) / abs(experiment1$advice1 - experiment1$Initial.Forecast_1) +
                                 abs(experiment1$Initial.Forecast_2 - experiment1$Averaging.Algorithm_2) / abs(experiment1$advice2 - experiment1$Initial.Forecast_2) +
                                 abs(experiment1$Initial.Forecast_3 - experiment1$Averaging.Algorithm_3) / abs(experiment1$advice3 - experiment1$Initial.Forecast_3) +
                                 abs(experiment1$Initial.Forecast_4 - experiment1$Averaging.Algorithm_4) / abs(experiment1$advice4 - experiment1$Initial.Forecast_4) +
                                 abs(experiment1$Initial.Forecast_5 - experiment1$Averaging.Algorithm_5) / abs(experiment1$advice5 - experiment1$Initial.Forecast_5))/5

experiment1$avgHumanWeight <- (abs(experiment1$Initial.Forecast_1 - experiment1$Learning.Algorithm_1) / abs(experiment1$advice1 - experiment1$Initial.Forecast_1) +
                                 abs(experiment1$Initial.Forecast_2 - experiment1$Learning.Algorithm_2) / abs(experiment1$advice2 - experiment1$Initial.Forecast_2) +
                                 abs(experiment1$Initial.Forecast_3 - experiment1$Learning.Algorithm_3) / abs(experiment1$advice3 - experiment1$Initial.Forecast_3) +
                                 abs(experiment1$Initial.Forecast_4 - experiment1$Learning.Algorithm_4) / abs(experiment1$advice4 - experiment1$Initial.Forecast_4) +
                                 abs(experiment1$Initial.Forecast_5 - experiment1$Learning.Algorithm_5) / abs(experiment1$advice5 - experiment1$Initial.Forecast_5))/5

experiment1$adviceWt <- rowMeans(cbind(experiment1$avgHumanWeight,experiment1$randHumanWeight,
                                       experiment1$algHumanWeight,experiment1$lernerWeight),na.rm=TRUE)

# Exp 2
experiment2$avgHumanWeight <- (abs(experiment2$Initial.Forecast_1 - experiment2$Average.Human_1) / abs(experiment2$advice1 - experiment2$Initial.Forecast_1) +
                                 abs(experiment2$Initial.Forecast_2 - experiment2$Average.Human_2) / abs(experiment2$advice2 - experiment2$Initial.Forecast_2))/2

experiment2$randHumanWeight <- (abs(experiment2$Initial.Forecast_1 - experiment2$Random.Human_1) / abs(experiment2$advice1 - experiment2$Initial.Forecast_1) +
                                 abs(experiment2$Initial.Forecast_2 - experiment2$Random.Human_2) / abs(experiment2$advice2 - experiment2$Initial.Forecast_2))/2

experiment2$algHumanWeight <- (abs(experiment2$Initial.Forecast_1 - experiment2$Averaging.Algorithm_1) / abs(experiment2$advice1 - experiment2$Initial.Forecast_1) +
                                  abs(experiment2$Initial.Forecast_2 - experiment2$Averaging.Algorithm_2) / abs(experiment2$advice2 - experiment2$Initial.Forecast_2))/2

experiment2$lernerWeight <- (abs(experiment2$Initial.Forecast_1 - experiment2$Learning.Algorithm_1) / abs(experiment2$advice1 - experiment2$Initial.Forecast_1) +
                                 abs(experiment2$Initial.Forecast_2 - experiment2$Learning.Algorithm_1) / abs(experiment2$advice2 - experiment2$Initial.Forecast_2))/2

experiment2$adviceWt <- rowMeans(cbind(experiment2$avgHumanWeight,experiment2$randHumanWeight,
                                       experiment2$algHumanWeight,experiment2$lernerWeight),na.rm=TRUE)


# Exp 3
experiment3$avgHumanWeight <- (abs(experiment3$Initial.Forecast_1 - experiment3$Average.Human_1) / abs(experiment3$advice1 - experiment3$Initial.Forecast_1) +
                                 abs(experiment3$Initial.Forecast_2 - experiment3$Average.Human_2) / abs(experiment3$advice2 - experiment3$Initial.Forecast_2) +
                                 abs(experiment3$Initial.Forecast_3 - experiment3$Average.Human_3) / abs(experiment3$advice3 - experiment3$Initial.Forecast_3) +
                                 abs(experiment3$Initial.Forecast_4 - experiment3$Average.Human_4) / abs(experiment3$advice4 - experiment3$Initial.Forecast_4))/4

experiment3$randHumanWeight <- (abs(experiment3$Initial.Forecast_1 - experiment3$Random.Human_1) / abs(experiment3$advice1 - experiment3$Initial.Forecast_1) +
                                 abs(experiment3$Initial.Forecast_2 - experiment3$Random.Human_2) / abs(experiment3$advice2 - experiment3$Initial.Forecast_2) +
                                 abs(experiment3$Initial.Forecast_3 - experiment3$Random.Human_3) / abs(experiment3$advice3 - experiment3$Initial.Forecast_3) +
                                 abs(experiment3$Initial.Forecast_4 - experiment3$Random.Human_4) / abs(experiment3$advice4 - experiment3$Initial.Forecast_4))/4

experiment3$algHumanWeight <- (abs(experiment3$Initial.Forecast_1 - experiment3$Averaging.Algorithm_1) / abs(experiment3$advice1 - experiment3$Initial.Forecast_1) +
                                  abs(experiment3$Initial.Forecast_2 - experiment3$Averaging.Algorithm_2) / abs(experiment3$advice2 - experiment3$Initial.Forecast_2) +
                                  abs(experiment3$Initial.Forecast_3 - experiment3$Averaging.Algorithm_3) / abs(experiment3$advice3 - experiment3$Initial.Forecast_3) +
                                  abs(experiment3$Initial.Forecast_4 - experiment3$Averaging.Algorithm_4) / abs(experiment3$advice4 - experiment3$Initial.Forecast_4))/4

experiment3$lernerWeight <- (abs(experiment3$Initial.Forecast_1 - experiment3$Learning.Algorithm_1) / abs(experiment3$advice1 - experiment3$Initial.Forecast_1) +
                                 abs(experiment3$Initial.Forecast_2 - experiment3$Learning.Algorithm_2) / abs(experiment3$advice2 - experiment3$Initial.Forecast_2) +
                                 abs(experiment3$Initial.Forecast_3 - experiment3$Learning.Algorithm_3) / abs(experiment3$advice3 - experiment3$Initial.Forecast_3) +
                                 abs(experiment3$Initial.Forecast_4 - experiment3$Learning.Algorithm_4) / abs(experiment3$advice4 - experiment3$Initial.Forecast_4))/4

experiment3$adviceWt <- rowMeans(cbind(experiment3$avgHumanWeight,experiment3$randHumanWeight,
                                       experiment3$algHumanWeight,experiment3$lernerWeight),na.rm=TRUE)

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
# Exp 1
experiment1$avgHumanBrier <- ((0.01*experiment1$Average.Human_1 - 0.01*experiment1$advice1)^2 +
                                (0.01*experiment1$Average.Human_2 - 0.01*experiment1$advice2)^2 +
                                (0.01*experiment1$Average.Human_3 - 0.01*experiment1$advice3)^2 +
                                (0.01*experiment1$Average.Human_4 - 0.01*experiment1$advice4)^2 +
                                (0.01*experiment1$Average.Human_5 - 0.01*experiment1$advice5)^2)/5

experiment1$randHumanBrier <- ((0.01*experiment1$Random.Human_1 - 0.01*experiment1$advice1)^2 +
                                (0.01*experiment1$Random.Human_2 - 0.01*experiment1$advice2)^2 +
                                (0.01*experiment1$Random.Human_3 - 0.01*experiment1$advice3)^2 +
                                (0.01*experiment1$Random.Human_4 - 0.01*experiment1$advice4)^2 +
                                (0.01*experiment1$Random.Human_5 - 0.01*experiment1$advice5)^2)/5

experiment1$algHumanBrier <- ((0.01*experiment1$Averaging.Algorithm_1 - 0.01*experiment1$advice1)^2 +
                               (0.01*experiment1$Averaging.Algorithm_2 - 0.01*experiment1$advice2)^2 +
                               (0.01*experiment1$Averaging.Algorithm_3 - 0.01*experiment1$advice3)^2 +
                               (0.01*experiment1$Averaging.Algorithm_4 - 0.01*experiment1$advice4)^2 +
                               (0.01*experiment1$Averaging.Algorithm_5 - 0.01*experiment1$advice5)^2)/5

experiment1$lernerBrier <- ((0.01*experiment1$Learning.Algorithm_1 - 0.01*experiment1$advice1)^2 +
                              (0.01*experiment1$Learning.Algorithm_2 - 0.01*experiment1$advice2)^2 +
                              (0.01*experiment1$Learning.Algorithm_3 - 0.01*experiment1$advice3)^2 +
                              (0.01*experiment1$Learning.Algorithm_4 - 0.01*experiment1$advice4)^2 +
                              (0.01*experiment1$Learning.Algorithm_5 - 0.01*experiment1$advice5)^2)/5

experiment1$brier <- rowMeans(cbind(experiment1$avgHumanBrier,experiment1$randHumanBrier,
                                    experiment1$algHumanBrier,experiment1$lernerBrier),na.rm=TRUE)

# Exp 2
experiment2$avgHumanBrier <- ((0.01*experiment2$Average.Human_1 - 0.01*experiment2$advice1)^2 +
                                (0.01*experiment2$Average.Human_2 - 0.01*experiment2$advice2)^2)/2

experiment2$randHumanBrier <- ((0.01*experiment2$Random.Human_1 - 0.01*experiment2$advice1)^2 +
                                 (0.01*experiment2$Random.Human_2 - 0.01*experiment2$advice2)^2)/2

experiment2$algHumanBrier <- ((0.01*experiment2$Averaging.Algorithm_1 - 0.01*experiment2$advice1)^2 +
                                (0.01*experiment2$Averaging.Algorithm_2 - 0.01*experiment2$advice2)^2)/2

experiment2$lernerBrier <- ((0.01*experiment2$Learning.Algorithm_1 - 0.01*experiment2$advice1)^2 +
                              (0.01*experiment2$Learning.Algorithm_2 - 0.01*experiment2$advice2)^2)/2

experiment2$avgHumanBrier2 <- ((0.01*experiment2$Anch.Avg.Human_1 - 0.01*experiment2$advice1)^2 +
                                (0.01*experiment2$Anch.Avg.Human_2 - 0.01*experiment2$advice2)^2)/2

experiment2$randHumanBrier2 <- ((0.01*experiment2$Anch.Rand.Human_1 - 0.01*experiment2$advice1)^2 +
                                 (0.01*experiment2$Anch.Rand.Human_2 - 0.01*experiment2$advice2)^2)/2

experiment2$algHumanBrier2 <- ((0.01*experiment2$Anch.alg.Combine_1 - 0.01*experiment2$advice1)^2 +
                                (0.01*experiment2$Anch.alg.Combine_2 - 0.01*experiment2$advice2)^2)/2

experiment2$lernerBrier2 <- ((0.01*experiment2$Anch.Full.Alg_1 - 0.01*experiment2$advice1)^2 +
                              (0.01*experiment2$Anch.Full.Alg_2 - 0.01*experiment2$advice2)^2)/2

experiment2$brier <- rowMeans(cbind(experiment2$avgHumanBrier,experiment2$randHumanBrier,
                                    experiment2$algHumanBrier,experiment2$lernerBrier,
                                    experiment2$avgHumanBrier2,experiment2$randHumanBrier2,
                                    experiment2$algHumanBrier2,experiment2$lernerBrier2),na.rm=TRUE)

# Exp 3
experiment3$avgHumanBrier <- ((0.01*experiment3$Average.Human_1 - 0.01*experiment3$advice1)^2 +
                                (0.01*experiment3$Average.Human_2 - 0.01*experiment3$advice2)^2 +
                                (0.01*experiment3$Average.Human_3 - 0.01*experiment3$advice3)^2 +
                                (0.01*experiment3$Average.Human_4 - 0.01*experiment3$advice4)^2)/4

experiment3$randHumanBrier <- ((0.01*experiment3$Random.Human_1 - 0.01*experiment3$advice1)^2 +
                                 (0.01*experiment3$Random.Human_2 - 0.01*experiment3$advice2)^2 +
                                 (0.01*experiment3$Random.Human_3 - 0.01*experiment3$advice3)^2 +
                                 (0.01*experiment3$Random.Human_4 - 0.01*experiment3$advice4)^2)/4

experiment3$algHumanBrier <- ((0.01*experiment3$Averaging.Algorithm_1 - 0.01*experiment3$advice1)^2 +
                                (0.01*experiment3$Averaging.Algorithm_2 - 0.01*experiment3$advice2)^2 +
                                (0.01*experiment3$Averaging.Algorithm_3 - 0.01*experiment3$advice3)^2 +
                                (0.01*experiment3$Averaging.Algorithm_4 - 0.01*experiment3$advice4)^2)/4

experiment3$lernerBrier <- ((0.01*experiment3$Learning.Algorithm_1 - 0.01*experiment3$advice1)^2 +
                              (0.01*experiment3$Learning.Algorithm_2 - 0.01*experiment3$advice2)^2 +
                              (0.01*experiment3$Learning.Algorithm_3 - 0.01*experiment3$advice3)^2 +
                              (0.01*experiment3$Learning.Algorithm_4 - 0.01*experiment3$advice4)^2)/4

experiment3$avgHumanBrier2 <- ((0.01*experiment3$Anch.Avg.Human_1 - 0.01*experiment3$advice1)^2 +
                                 (0.01*experiment3$Anch.Avg.Human_2 - 0.01*experiment3$advice2)^2 +
                                 (0.01*experiment3$Anch.Avg.Human_3 - 0.01*experiment3$advice3)^2 +
                                 (0.01*experiment3$Anch.Avg.Human_4 - 0.01*experiment3$advice4)^2)/4

experiment3$randHumanBrier2 <- ((0.01*experiment3$Anch.Rand.Human_1 - 0.01*experiment3$advice1)^2 +
                                  (0.01*experiment3$Anch.Rand.Human_2 - 0.01*experiment3$advice2)^2 +
                                  (0.01*experiment3$Anch.Rand.Human_3 - 0.01*experiment3$advice3)^2 +
                                  (0.01*experiment3$Anch.Rand.Human_4 - 0.01*experiment3$advice4)^2)/4

experiment3$algHumanBrier2 <- ((0.01*experiment3$Anch.alg.Combine_1 - 0.01*experiment3$advice1)^2 +
                                 (0.01*experiment3$Anch.alg.Combine_2 - 0.01*experiment3$advice2)^2 +
                                 (0.01*experiment3$Anch.alg.Combine_3 - 0.01*experiment3$advice3)^2 +
                                 (0.01*experiment3$Anch.alg.Combine_4 - 0.01*experiment3$advice4)^2)/4

experiment3$lernerBrier2 <- ((0.01*experiment3$Anch.Full.Alg_1 - 0.01*experiment3$advice1)^2 +
                               (0.01*experiment3$Anch.Full.Alg_2 - 0.01*experiment3$advice2)^2 +
                               (0.01*experiment3$Anch.Full.Alg_3 - 0.01*experiment3$advice3)^2 +
                               (0.01*experiment3$Anch.Full.Alg_4 - 0.01*experiment3$advice4)^2)/4

experiment3$brier <- rowMeans(cbind(experiment3$avgHumanBrier,experiment3$randHumanBrier,
                                    experiment3$algHumanBrier,experiment3$lernerBrier,
                                    experiment3$avgHumanBrier2,experiment3$randHumanBrier2,
                                    experiment3$algHumanBrier2,experiment3$lernerBrier2),na.rm=TRUE)

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
experiments <- rbind.fill(experiment1, experiment2, experiment3, experiment4)


# Add in treatment indicators
experiments$AvgHumanTreat <- ifelse(!is.na(experiments$avgHuman.DISTANCE) | 
                                      !is.na(experiments$avgHuman.DISTANCE2) |
                                      !is.na(experiments$avgHumanWeight), 1, 0) 
                                     # !is.na(experiments$avgHumanBrier) |
                                    #  !is.na(experiments$avgHumanBrier2), 1, 0)

experiments$RandHumanTreat <- ifelse(!is.na(experiments$randHuman.DISTANCE) | 
                                       !is.na(experiments$randHuman.DISTANCE2) |
                                       !is.na(experiments$randHumanWeight), 1, 0) 
                                      # !is.na(experiments$randHumanBrier) |
                                      # !is.na(experiments$randHumanBrier2), 1, 0)

experiments$AlgHumanTreat <- ifelse(!is.na(experiments$algHuman.DISTANCE) | 
                                      !is.na(experiments$algHuman.DISTANCE2) |
                                      !is.na(experiments$algHumanWeight), 1, 0) 
                                     # !is.na(experiments$algHumanBrier) |
                                    #  !is.na(experiments$algHumanBrier2), 1, 0)

experiments$LernerTreat <- ifelse(!is.na(experiments$lerner.DISTANCE) | 
                                    !is.na(experiments$lerner.DISTANCE2) |
                                    !is.na(experiments$lernerWeight), 1, 0) 
                                    #!is.na(experiments$lernerBrier) |
                                    #!is.na(experiments$lernerBrier2), 1, 0)

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


experiments$extroverted = (experiments$TIPI_1 + experiments$TIPI_6)/2
experiments$agreeableness = (experiments$TIPI_2 + experiments$TIPI_7)/2
experiments$openness = (experiments$TIPI_5 + experiments$TIPI_10)/2
experiments$conscientiousness = (experiments$TIPI_3 + experiments$TIPI_8)/2
experiments$stability = (experiments$TIPI_4 + experiments$TIPI_9)/2


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

experiments$needcognition = (experiments$needcog2 + experiments$needcog4)/2




#### MODELS

# Conditions: AvgHumanTreat, RandHumanTreat, AlgHumanTreat, LernerTreat, Anchoring; BUT "RandHumanTreat" dropped as baseline

model1 <- lmer(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num),  # rfx
               data = experiments); summary(model1)

model2 <- lmer(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + # conditions: EXCLUDING ANCHORING CONDITION
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num),  # rfx
               data = experiments); summary(model2)

model3 <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions
                 tia + age + ed + female + partisanship + 
                 (1|scenario_num),  # rfx
               data = experiments); summary(model3)

# algorithm * tia INXN
model4 <- lmer(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                 I(LernerTreat*tia) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model4)

model5 <- lmer(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + # conditions: EXCLUDING ANCHORING
                 I(LernerTreat*tia) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model5)

model6 <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                 I(LernerTreat*tia) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model6)

# algorithm * age INXN
model7 <- lmer(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                 I(LernerTreat*age) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model7)

model8 <- lmer(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + # conditions: EXCLUDING ANCHORING
                 I(LernerTreat*age) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model8)

model9 <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                 I(LernerTreat*age) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model9)

# algorithm * ed INXN
model10 <- lmer(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                 I(LernerTreat*ed) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model10)

model11 <- lmer(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + # conditions: EXCLUDING ANCHORING
                 I(LernerTreat*ed) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model11)

model12 <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                 I(LernerTreat*ed) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model12)

# algorithm * female INXN
model13 <- lmer(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                  I(LernerTreat*female) + # algorithm INXN
                  tia + age + ed + female + partisanship + # controls
                  (1|scenario_num), # rfx
                data = experiments); summary(model13)

model14 <- lmer(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + # conditions: EXCLUDING ANCHORING
                  I(LernerTreat*female) + # algorithm INXN
                  tia + age + ed + female + partisanship + # controls
                  (1|scenario_num), # rfx
                data = experiments); summary(model14)

model15 <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                  I(LernerTreat*female) + # algorithm INXN
                  tia + age + ed + female + partisanship + # controls
                  (1|scenario_num), # rfx
                data = experiments); summary(model15)

## EXCLUDE 16-18, as there is no "encouragement" in these waves


#### PLOTS
## Distance plot
mod1sum <- summary(model1)
mod1sim <- sim(model1)
mod1simcoef <- as.tibble(data.frame(coef(mod1sim)$fixef))
mod1simcoef$simulation <- rownames(mod1simcoef)
mod1simcoeflong <- gather(mod1simcoef, condition, coefficient, X.Intercept.:partisanship)
mod1simcoeflong$condition[mod1simcoeflong$condition == "Anchoring"] <- "Anchoring"
mod1simcoeflong$condition[mod1simcoeflong$condition == "LernerTreat"] <- "Computer Algorithm"
mod1simcoeflong$condition[mod1simcoeflong$condition == "AlgHumanTreat"] <- "Algorithm Aggregation"
mod1simcoeflong$condition[mod1simcoeflong$condition == "AvgHumanTreat"] <- "Average of Humans"
mod1simcoeflong$condition[mod1simcoeflong$condition == "tia"] <- "Trust in Automation"
mod1simcoeflong$condition[mod1simcoeflong$condition == "age"] <- "Age"
mod1simcoeflong$condition[mod1simcoeflong$condition == "female"] <- "Female"
mod1simcoeflong$condition[mod1simcoeflong$condition == "partisanship"] <- "Partisanship"
mod1simcoeflong$condition[mod1simcoeflong$condition == "ed"] <- "Education"

p1 = ggplot(mod1simcoeflong[mod1simcoeflong$condition != "X.Intercept.",]) + 
  geom_boxplot(aes(condition, coefficient)) + ggtitle("Distance to Advice") +
  xlab("Condition") + ylab("Coefficient (95% CI)") + geom_hline(aes(yintercept=0)) +
  theme_bw() + coord_flip()
p1


## Advice weight plot 
mod2sum <- summary(model2)
mod2sim <- sim(model2)
mod2simcoef <- as.tibble(data.frame(coef(mod2sim)$fixef))
mod2simcoef$simulation <- rownames(mod2simcoef)
mod2simcoeflong <- gather(mod2simcoef, condition, coefficient, X.Intercept.:partisanship)
mod2simcoeflong$condition[mod2simcoeflong$condition == "LernerTreat"] <- "Computer Algorithm"
mod2simcoeflong$condition[mod2simcoeflong$condition == "AlgHumanTreat"] <- "Algorithm Aggregation"
mod2simcoeflong$condition[mod2simcoeflong$condition == "AvgHumanTreat"] <- "Average of Humans"
mod2simcoeflong$condition[mod2simcoeflong$condition == "tia"] <- "Trust in Automation"
mod2simcoeflong$condition[mod2simcoeflong$condition == "age"] <- "Age"
mod2simcoeflong$condition[mod2simcoeflong$condition == "female"] <- "Female"
mod2simcoeflong$condition[mod2simcoeflong$condition == "partisanship"] <- "Partisanship"
mod2simcoeflong$condition[mod2simcoeflong$condition == "ed"] <- "Education"

p2 = ggplot(mod2simcoeflong[mod2simcoeflong$condition != "X.Intercept.",]) + 
  geom_boxplot(aes(condition, coefficient)) + ggtitle("Weight of Advice") +
  xlab("Condition") + ylab("Coefficient (95% CI)") + geom_hline(aes(yintercept=0)) +
  theme_bw() + coord_flip()
p2


###### INXN PLOTS:
# Model 4 Interaction Plot: TIA x Algorithm (distance)
mod4sum <- summary(model4)
tiaX <- seq(1,7)
maineffect <- fixef(model4)[4] + fixef(model4)[6] * tiaX
maineffect <- as.tibble(cbind(tiaX,maineffect))
mod4sim <- sim(model4)
mod4simcoef <- as.tibble(data.frame(coef(mod4sim)$fixef))
mod4simcoef$simulation <- rownames(mod4simcoef)
mod4simcoeflong <- mod4simcoef %>%
  mutate(simeffect1 = LernerTreat + I.LernerTreat...tia. * 1,
         simeffect2 = LernerTreat + I.LernerTreat...tia. * 2,
         simeffect3 = LernerTreat + I.LernerTreat...tia. * 3,
         simeffect4 = LernerTreat + I.LernerTreat...tia. * 4,
         simeffect5 = LernerTreat + I.LernerTreat...tia. * 5,
         simeffect6 = LernerTreat + I.LernerTreat...tia. * 6,
         simeffect7 = LernerTreat + I.LernerTreat...tia. * 7) %>%
  dplyr::select(simulation:simeffect7) %>%
  gather(tialevel, estimate, simeffect1:simeffect7) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel), nchar(tialevel))))

p3 = ggplot() + geom_line(data = mod4simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "gray") +
  geom_line(data = maineffect, aes(x=tiaX, y=maineffect), size = 1) +
  xlab("Trust in Automation") + ylab("Estimated Impact of Algorithm") + ggtitle("Distance to Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()
p3

# Model 5 Interaction Plot: TIA x Algorithm (weight)
mod5sum <- summary(model5)
tiaX <- seq(1,7)
maineffect5 <- fixef(model5)[4] + fixef(model5)[5] * tiaX
maineffect5 <- as.tibble(cbind(tiaX,maineffect5))
mod5sim <- sim(model5)
mod5simcoef <- as.tibble(data.frame(coef(mod5sim)$fixef))
mod5simcoef$simulation <- rownames(mod5simcoef)
mod5simcoeflong <- mod5simcoef %>%
  mutate(simeffect1 = LernerTreat + I.LernerTreat...tia. * 1,
         simeffect2 = LernerTreat + I.LernerTreat...tia. * 2,
         simeffect3 = LernerTreat + I.LernerTreat...tia. * 3,
         simeffect4 = LernerTreat + I.LernerTreat...tia. * 4,
         simeffect5 = LernerTreat + I.LernerTreat...tia. * 5,
         simeffect6 = LernerTreat + I.LernerTreat...tia. * 6,
         simeffect7 = LernerTreat + I.LernerTreat...tia. * 7) %>%
  dplyr::select(simulation:simeffect7) %>%
  gather(tialevel, estimate, simeffect1:simeffect7) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel), nchar(tialevel))))

p4 = ggplot() + geom_line(data = mod5simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect5, aes(x=tiaX, y=maineffect5), size = 1) +
  xlab("Trust in Automation") + ylab("Estimated Impact of Algorithm") + ggtitle("Weight of Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()
p4

# Model 7 Interaction Plot: Age x Algorithm (distance)
mod7sum <- summary(model7)
tiaX <- seq(19,74, by = 5)
maineffect7 <- fixef(model7)[4] + fixef(model7)[6] * tiaX
maineffect7 <- as.tibble(cbind(tiaX,maineffect7))
mod7sim <- sim(model7)
mod7simcoef <- as.tibble(data.frame(coef(mod7sim)$fixef))
mod7simcoef$simulation <- rownames(mod7simcoef)
mod7simcoeflong <- mod7simcoef %>%
  mutate(simeffect19 = LernerTreat + I.LernerTreat...age. * 19,
         simeffect24 = LernerTreat + I.LernerTreat...age. * 24,
         simeffect29 = LernerTreat + I.LernerTreat...age. * 29,
         simeffect34 = LernerTreat + I.LernerTreat...age. * 34,
         simeffect39 = LernerTreat + I.LernerTreat...age. * 39,
         simeffect44 = LernerTreat + I.LernerTreat...age. * 44,
         simeffect49 = LernerTreat + I.LernerTreat...age. * 49,
         simeffect54 = LernerTreat + I.LernerTreat...age. * 54,
         simeffect59 = LernerTreat + I.LernerTreat...age. * 59,
         simeffect64 = LernerTreat + I.LernerTreat...age. * 64,
         simeffect69 = LernerTreat + I.LernerTreat...age. * 69,
         simeffect74 = LernerTreat + I.LernerTreat...age. * 74) %>%
  dplyr::select(simulation:simeffect74) %>%
  gather(tialevel, estimate, simeffect19:simeffect74) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel)-1, nchar(tialevel))))

p5 = ggplot() + geom_line(data = mod7simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect7, aes(x=tiaX, y=maineffect7), size = 1) +
  xlab("Age") + ylab("Estimated Impact of Algorithm") + ggtitle("Distance to Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()
p5

# Model 8 Interaction Plot: Age x Algorithm (weight)
mod8sum <- summary(model8)
tiaX <- seq(19,74, by = 5)
maineffect8 <- fixef(model8)[4] + fixef(model8)[5] * tiaX
maineffect8 <- as.tibble(cbind(tiaX,maineffect8))
mod8sim <- sim(model8)
mod8simcoef <- as.tibble(data.frame(coef(mod8sim)$fixef))
mod8simcoef$simulation <- rownames(mod8simcoef)
mod8simcoeflong <- mod8simcoef %>%
  mutate(simeffect19 = LernerTreat + I.LernerTreat...age. * 19,
         simeffect24 = LernerTreat + I.LernerTreat...age. * 24,
         simeffect29 = LernerTreat + I.LernerTreat...age. * 29,
         simeffect34 = LernerTreat + I.LernerTreat...age. * 34,
         simeffect39 = LernerTreat + I.LernerTreat...age. * 39,
         simeffect44 = LernerTreat + I.LernerTreat...age. * 44,
         simeffect49 = LernerTreat + I.LernerTreat...age. * 49,
         simeffect54 = LernerTreat + I.LernerTreat...age. * 54,
         simeffect59 = LernerTreat + I.LernerTreat...age. * 59,
         simeffect64 = LernerTreat + I.LernerTreat...age. * 64,
         simeffect69 = LernerTreat + I.LernerTreat...age. * 69,
         simeffect74 = LernerTreat + I.LernerTreat...age. * 74) %>%
  dplyr::select(simulation:simeffect74) %>%
  gather(tialevel, estimate, simeffect19:simeffect74) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel)-1, nchar(tialevel))))

p6 = ggplot() + geom_line(data = mod8simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect8, aes(x=tiaX, y=maineffect8), size = 1) +
  xlab("Age") + ylab("Estimated Impact of Algorithm") + ggtitle("Weight of Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()
p6

# Interaction graph for model 10: Education x Algorithm (distance)
mod10sum <- summary(model10)
tiaX <- seq(2,4)
maineffect10 <- fixef(model10)[4] + fixef(model10)[6] * tiaX
maineffect10 <- as.tibble(cbind(tiaX,maineffect10))
mod10sim <- sim(model10)
mod10simcoef <- as.tibble(data.frame(coef(mod10sim)$fixef))
mod10simcoef$simulation <- rownames(mod10simcoef)
mod10simcoeflong <- mod10simcoef %>%
  mutate(simeffect2 = LernerTreat + I.LernerTreat...ed. * 2,
         simeffect3 = LernerTreat + I.LernerTreat...ed. * 3,
         simeffect4 = LernerTreat + I.LernerTreat...ed. * 4) %>%
  dplyr::select(simulation:simeffect4) %>%
  gather(tialevel, estimate, simeffect2:simeffect4) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel), nchar(tialevel))))

p7 = ggplot() + geom_line(data = mod10simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect10, aes(x=tiaX, y=maineffect10), size = 1) +
  xlab("Education") + ylab("Estimated Impact of Algorithm") + ggtitle("Distance to Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()
p7

# Interaction graph for model 11: Education x Algorithm (weight)
mod11sum <- summary(model11)
tiaX <- seq(2,4)
maineffect11 <- fixef(model11)[4] + fixef(model11)[5] * tiaX
maineffect11 <- as.tibble(cbind(tiaX,maineffect11))
mod11sim <- sim(model11)
mod11simcoef <- as.tibble(data.frame(coef(mod11sim)$fixef))
mod11simcoef$simulation <- rownames(mod11simcoef)
mod11simcoeflong <- mod11simcoef %>%
  mutate(simeffect2 = LernerTreat + I.LernerTreat...ed. * 2,
         simeffect3 = LernerTreat + I.LernerTreat...ed. * 3,
         simeffect4 = LernerTreat + I.LernerTreat...ed. * 4) %>%
  dplyr::select(simulation:simeffect4) %>%
  gather(tialevel, estimate, simeffect2:simeffect4) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel), nchar(tialevel))))

p8 = ggplot() + geom_line(data = mod11simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect11, aes(x=tiaX, y=maineffect11), size = 1) +
  xlab("Education") + ylab("Estimated Impact of Algorithm") + ggtitle("Weight of Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()
p8

# combine graphs for direct model coefficients
grid.arrange(p1, p2, ncol=2)

grid.arrange(p3,p4,p5,p6,p7,p8,ncol =2)



##### TABLES
# Create table for multilevel results
# Note the hack for putting random effects into Stargazer from:
# http://svmiller.com/blog/2015/02/quasi-automating-the-inclusion-of-random-effects-in-rs-stargazer-package/
# Also note that if using the auto-updating code in Latex, you must remove everything above \begin{tabular} and after \end{tabular}

# Function for adding rows to a data frame (will be used later)
insertrow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# Create a baseline table
Tables <- stargazer(model1, model2, model4, model5, model7, model8, model10, model11, style="ajps", 
                    title="Models of Trust in Automation", 
                    dep.var.labels.include = FALSE#, 
                    #covariate.labels=c( "Ideology", "Authoritarianism", "Power Rating", "Ideology Rating","Power Rating : Ideology","Ideology Rating : Ideology")
)

# Convert Tables to data frame and coerce the one vector (also called Tables) to character
Tables <- as.data.frame(Tables)
Tables$Tables <- as.character(Tables$Tables)

# Find where you want to put in the random effect. In our case, this is right after the last fixed effect. Line: 37 (after \hline after "constant")
r <- 37

# Create some standard label lines
randomeffect <- "{\\bf Random Effect} & & \\\\"
hline <- "\\hline"
newline <- "\\\\"

# Now insert those label lines where they are needed
Tables <- insertrow(Tables, hline, r)
Tables <- insertrow(Tables,randomeffect,r+1)
Tables <- insertrow(Tables,hline,r+2)
# Get number of unique values in each grouping
num.participants <- sapply(ranef(model1),nrow)[1]
num.scenarios <- sapply(ranef(model1),nrow)[2]

# Get standard deviation of the random effect
#stddev.model1.participants <- attributes(VarCorr(model1)$ResponseId)$stddev
stddev.model1.scenarios <- attributes(VarCorr(model1)$scenario)$stddev
#stddev.model2.participants <- attributes(VarCorr(model2)$ResponseId)$stddev
stddev.model2.scenarios <- attributes(VarCorr(model2)$scenario)$stddev
#stddev.model3.participants <- attributes(VarCorr(model4)$ResponseId)$stddev
stddev.model3.scenarios <- attributes(VarCorr(model4)$scenario)$stddev
#stddev.model4.participants <- attributes(VarCorr(model5)$ResponseId)$stddev
stddev.model4.scenarios <- attributes(VarCorr(model5)$scenario)$stddev
#stddev.model5.participants <- attributes(VarCorr(model7)$ResponseId)$stddev
stddev.model5.scenarios <- attributes(VarCorr(model7)$scenario)$stddev
#stddev.model6.participants <- attributes(VarCorr(model8)$ResponseId)$stddev
stddev.model6.scenarios <- attributes(VarCorr(model8)$scenario)$stddev
#stddev.model7.participants <- attributes(VarCorr(model10)$ResponseId)$stddev
stddev.model7.scenarios <- attributes(VarCorr(model10)$scenario)$stddev
#stddev.model8.participants <- attributes(VarCorr(model11)$ResponseId)$stddev
stddev.model8.scenarios <- attributes(VarCorr(model11)$scenario)$stddev

# Create a LaTex character row for the random effect
number.of.participants <- paste("\\# of Participants & ", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "\\\\")
#stddev.participants <- paste("Participant Standard Deviation & ", round(stddev.model1.participants, 3), "&", round(stddev.model2.participants, 3), "&", round(stddev.model3.participants, 3), "&", round(stddev.model4.participants, 3), "&", round(stddev.model5.participants, 3), "&", round(stddev.model6.participants, 3), "&", round(stddev.model7.participants, 3), "&", round(stddev.model8.participants, 3), "\\\\")
number.of.scenarios <- paste("\\# of Scenarios & ", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "\\\\")
stddev.scenarios <- paste("Scenario Standard Deviation & ", round(stddev.model1.scenarios, 3), "&", round(stddev.model2.scenarios, 3), "&", round(stddev.model3.scenarios, 3), "&", round(stddev.model4.scenarios, 3), "&", round(stddev.model5.scenarios, 3), "&", round(stddev.model6.scenarios, 3), "&", round(stddev.model7.scenarios, 3), "&", round(stddev.model8.scenarios, 3), "\\\\")

# Add these lines to the table
Tables <- insertrow(Tables,number.of.participants,r+3)
#Tables <- insertrow(Tables,stddev.participants,r+4)
Tables <- insertrow(Tables,newline,r+5)
Tables <- insertrow(Tables,number.of.scenarios,r+6)
Tables <- insertrow(Tables,stddev.scenarios,r+7)
Tables <- insertrow(Tables, hline, r+8)

# Write the table to a file that can be inserted into the document
write.table(Tables,file=("REtablepolitical.tex"),
            sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)



##### Secondary Brier analysis - just table for the SI -- issue with "ResponseId" as rfx -- "Error: grouping factors must have > 1 sampled level" -- Thus, excluded from models and Table 
model1.b <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num),  # rfx
               data = experiments); summary(model1.b)

model4.b <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                 I(LernerTreat*tia) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model4.b)

model7.b <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                 I(LernerTreat*age) + # algorithm INXN
                 tia + age + ed + female + partisanship + # controls
                 (1|scenario_num), # rfx
               data = experiments); summary(model7.b)

model10.b <- lmer(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring + # conditions 
                  I(LernerTreat*ed) + # algorithm INXN
                  tia + age + ed + female + partisanship + # controls
                  (1|scenario_num), # rfx
                data = experiments); summary(model10.b)

# Create a baseline table
TablesB <- stargazer(model1.b, model4.b, model7.b, model10.b, style="ajps", 
                    title="Models of Trust in Automation: Brier Scores", 
                    dep.var.labels.include = FALSE#, 
                    #covariate.labels=c( "Ideology", "Authoritarianism", "Power Rating", "Ideology Rating","Power Rating : Ideology","Ideology Rating : Ideology")
)

# Convert Tables to data frame and coerce the one vector (also called TablesB) to character
TablesB <- as.data.frame(TablesB)
TablesB$TablesB <- as.character(TablesB$TablesB)

# Find where you want to put in the random effect -- Line: 37 (after \hline after "constant")
r <- 37

# Create some standard label lines
randomeffect <- "{\\bf Random Effect} & & \\\\"
hline <- "\\hline"
newline <- "\\\\"

# Now insert those label lines where they are needed
TablesB <- insertrow(TablesB, hline, r)
TablesB <- insertrow(TablesB,randomeffect,r+1)
TablesB <- insertrow(TablesB,hline,r+2)

# Get number of unique values in each grouping
num.participants <- sapply(ranef(model1),nrow)[1]
num.scenarios <- sapply(ranef(model1),nrow)[2]

# Get standard deviation of the random effect
stddev.model1.b.scenarios <- attributes(VarCorr(model1.b)$scenario)$stddev
stddev.model4.b.scenarios <- attributes(VarCorr(model4.b)$scenario)$stddev
stddev.model7.b.scenarios <- attributes(VarCorr(model7.b)$scenario)$stddev
stddev.model10.b.scenarios <- attributes(VarCorr(model10.b)$scenario)$stddev

# Create a LaTex character row for the random effect
number.of.participants <- paste("\\# of Participants & ", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "\\\\")
#stddev.participants <- paste("Participant Standard Deviation & ", round(stddev.model1.participants, 3), "&", round(stddev.model2.participants, 3), "&", round(stddev.model3.participants, 3), "&", round(stddev.model4.participants, 3), "&", round(stddev.model5.participants, 3), "&", round(stddev.model6.participants, 3), "&", round(stddev.model7.participants, 3), "&", round(stddev.model8.participants, 3), "\\\\")
number.of.scenarios <- paste("\\# of Scenarios & ", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "\\\\")
stddev.scenarios <- paste("Scenario Standard Deviation & ", round(stddev.model1.b.scenarios, 3), "&", round(stddev.model4.b.scenarios, 3), "&", round(stddev.model7.b.scenarios, 3), "&", round(stddev.model10.b.scenarios, 3), "\\\\")

# Add these lines to the table
TablesB <- insertrow(TablesB,number.of.participants,r+3)
#TablesB <- insertrow(TablesB,stddev.participants,r+4)
TablesB <- insertrow(TablesB,newline,r+5)
TablesB <- insertrow(TablesB,number.of.scenarios,r+6)
TablesB <- insertrow(TablesB,stddev.scenarios,r+7)
TablesB <- insertrow(TablesB, hline, r+8)

# Write the table to a file that can be inserted into the document
write.table(TablesB,file=("REtablepoliticalbrier.tex"),
            sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

# Finally, for Appendix, create histograms for all variables used in both stages of analysis

### POLITICAL HISTOGRAMS
# Dependent Variables: distance, adviceWt, brier
p.dist <- qplot(experiments$distance, geom = 'blank',
                 main = "Distance to Advice",
                 xlab = "Range of Distance to Advice",
                 ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.75) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.dist

p.weight <- qplot(experiments$adviceWt, geom = 'blank',
                main = "Weight of Advice",
                xlab = "Range of Weight of Advice",
                ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.weight

p.brier <- qplot(experiments$brier, geom = 'blank',
                  main = "Brier Scores",
                  xlab = "Range of Brier Scores",
                  ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.01) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.brier

### DV Histograms
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# r,c
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(p.dist, vp = vplayout(1, 1))
print(p.weight, vp = vplayout(1, 2))
print(p.brier, vp = vplayout(2, 1))


# Conditions: AvgHumanTreat, AlgHumanTreat, RandHumanTreat, LernerTreat
p.AvgHumanTreat <- qplot(experiments$AvgHumanTreat, geom = 'blank',
                 main = "Average Human Treatment Condition",
                 xlab = "Average Human Treatment Condition (0/1)",
                 ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.AvgHumanTreat

p.AlgHumanTreat <- qplot(experiments$AlgHumanTreat, geom = 'blank',
                         main = "Algorithmic + Human Treatment Condition",
                         xlab = "Algorithmic + Human Treatment Condition (0/1)",
                         ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.AlgHumanTreat

p.RandHumanTreat <- qplot(experiments$RandHumanTreat, geom = 'blank',
                         main = "Random Human Treatment Condition",
                         xlab = "Random Human Treatment Condition (0/1)",
                         ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.RandHumanTreat

p.LernerTreat <- qplot(experiments$LernerTreat, geom = 'blank',
                          main = "Computer Algorithm Treatment Condition",
                          xlab = "Computer Algorithm Treatment Condition (0/1)",
                          ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.LernerTreat

### Conditions Histograms
# r,c
grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 2)))
print(p.AvgHumanTreat, vp = vplayout(1, 1))
print(p.AlgHumanTreat, vp = vplayout(1, 2))
print(p.RandHumanTreat, vp = vplayout(2, 1))
print(p.LernerTreat, vp = vplayout(2, 2))



# Control Variables: tia, age, ed, female, partisanship
p.tia <- qplot(experiments$tia, geom = 'blank',
                       main = "Trust in Algorithms",
                       xlab = "Range of Trust in Algorithms",
                       ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.4) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.tia

p.age <- qplot(experiments$age, geom = 'blank',
               main = "Age",
               xlab = "Range of Age",
               ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 1.2) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.age

p.ed <- qplot(experiments$ed, geom = 'blank',
               main = "Education",
               xlab = "Range of Education",
               ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.ed

p.female <- qplot(experiments$female, geom = 'blank',
              main = "Female",
              xlab = "Female (0/1)",
              ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.female

p.partisanship <- qplot(experiments$partisanship, geom = 'blank',
                  main = "Partisanship",
                  xlab = "Range of Partisanship",
                  ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.partisanship

### Controls Histograms
# r,c
grid.newpage()
pushViewport(viewport(layout = grid.layout(3, 2)))
print(p.tia, vp = vplayout(1, 1))
print(p.age, vp = vplayout(1, 2))
print(p.ed, vp = vplayout(2, 1))
print(p.female, vp = vplayout(2, 2))
print(p.partisanship, vp = vplayout(3, 1))

### CJ HISTOGRAMS
### FIRST LOAD IN CJ RESULTS (Data: cj1long[cj1long$adviceWt <= 1,])
# Dependent Variables: distance, adviceWt
p.dist.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$distance, geom = 'blank',
                main = "Distance to Advice",
                xlab = "Range of Distance to Advice",
                ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 1.2) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.dist.cj

p.weight.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$adviceWt, geom = 'blank',
                  main = "Weight of Advice",
                  xlab = "Range of Weight of Advice",
                  ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .1) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.weight.cj

### CJ DV Histograms
# r,c
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(p.dist.cj, vp = vplayout(1, 1))
print(p.weight.cj, vp = vplayout(1, 2))

# Conditions: algorithm, judge
p.algorithm.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$algorithm, geom = 'blank',
                         main = "Algorithm Treatment Condition",
                         xlab = "Algorithm Treatment Condition (0/1)",
                         ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.algorithm.cj

p.judge.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$judge, geom = 'blank',
                         main = "Judge Treatment Condition",
                         xlab = "Judge Treatment Condition (0/1)",
                         ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.judge.cj

### CJ Conditions Histograms
# r,c
grid.newpage()
pushViewport(viewport(layout = grid.layout(1, 2)))
print(p.algorithm.cj, vp = vplayout(1, 1))
print(p.judge.cj, vp = vplayout(1, 2))

# Controls: tia, age, ed, female, partisanship, needcog, needjudge, encouragement
p.tia.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$tia, geom = 'blank',
               main = "Trust in Algorithms",
               xlab = "Range of Trust in Algorithms",
               ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 0.4) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.tia.cj

p.age.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$age, geom = 'blank',
               main = "Age",
               xlab = "Range of Age",
               ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = 1.2) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.age.cj

p.ed.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$ed, geom = 'blank',
              main = "Education",
              xlab = "Range of Education",
              ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.ed.cj

p.female.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$female, geom = 'blank',
                  main = "Female",
                  xlab = "Female (0/1)",
                  ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.female.cj

p.partisanship.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$partisanship, geom = 'blank',
                        main = "Partisanship",
                        xlab = "Range of Partisanship",
                        ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.partisanship.cj

p.needcog.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$needcog, geom = 'blank',
                           main = "Need for Cognition",
                           xlab = "Range of Need for Cognition",
                           ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.needcog.cj

p.needjudge.cj <- qplot(cj1long[cj1long$adviceWt <= 1,]$needjudge, geom = 'blank',
                      main = "Need for Judgement",
                      xlab = "Range of Need for Judgement",
                      ylab = "Density") +
  geom_histogram(aes(y = ..density..), alpha = 0.4, binwidth = .5) + 
  theme_bw() + theme(plot.title = element_text(hjust = 0.5), legend.title = element_blank())
p.needjudge.cj

### Controls Histograms
# r,c
grid.newpage()
pushViewport(viewport(layout = grid.layout(4, 2)))
print(p.tia.cj, vp = vplayout(1, 1))
print(p.age.cj, vp = vplayout(1, 2))
print(p.ed.cj, vp = vplayout(2, 1))
print(p.female.cj, vp = vplayout(2, 2))
print(p.partisanship.cj, vp = vplayout(3, 1))
print(p.needcog.cj, vp = vplayout(3, 2))
print(p.needjudge.cj, vp = vplayout(4, 1))


#------- END MAIN POLITICAL ANALYSIS (SEE CJ SCRIPT IN DB FOR SECOND STAGE ANALYSIS) ------- 


# FOR FUTURE WORK ON PERSONALITIES...
## Create folded personality measures for 5 traits, based on TIPI responses
### CREATING EXTRAVERSION FOLDED MEASURE (single indicator based on TIPI 1 and 6) == High on extraversion = 7, low on extraversion = 1
#### TIPI 1
experiments$extraversion[experiments$TIPI_1 == ""] <- 0
experiments$extraversion[experiments$TIPI_1 == "Disagree strongly"] <- 1
experiments$extraversion[experiments$TIPI_1 == "Disagree moderately"] <- 2
experiments$extraversion[experiments$TIPI_1 == "Disagree a little"] <- 3
experiments$extraversion[experiments$TIPI_1 == "Neither agree nor diagree"] <- 4
experiments$extraversion[experiments$TIPI_1 == "Agree a little"] <- 5
experiments$extraversion[experiments$TIPI_1 == "Agree moderately"] <- 6
experiments$extraversion[experiments$TIPI_1 == "Agree strongly"] <- 7
#### TIPI 6 (reversed; see Gosling et al for more on coding)
experiments$extraversion[experiments$TIPI_6 == ""] <- 0
experiments$extraversion[experiments$TIPI_6 == "Disagree strongly"] <- 7
experiments$extraversion[experiments$TIPI_6 == "Disagree moderately"] <- 6
experiments$extraversion[experiments$TIPI_6 == "Disagree a little"] <- 5
experiments$extraversion[experiments$TIPI_6 == "Neither agree nor diagree"] <- 4
experiments$extraversion[experiments$TIPI_6 == "Agree a little"] <- 3
experiments$extraversion[experiments$TIPI_6 == "Agree moderately"] <- 2
experiments$extraversion[experiments$TIPI_6 == "Agree strongly"] <- 1

table(experiments$extraversion)

### CREATING AGREEABLENESS FOLDED MEASURE (single indicator based on TIPI 2 and 7) == High on agreeableness = 7, low on agreeableness = 1
#### TIPI 7
experiments$agreeableness[experiments$TIPI_7 == ""] <- 0
experiments$agreeableness[experiments$TIPI_7 == "Disagree strongly"] <- 1
experiments$agreeableness[experiments$TIPI_7 == "Disagree moderately"] <- 2
experiments$agreeableness[experiments$TIPI_7 == "Disagree a little"] <- 3
experiments$agreeableness[experiments$TIPI_7 == "Neither agree nor diagree"] <- 4
experiments$agreeableness[experiments$TIPI_7 == "Agree a little"] <- 5
experiments$agreeableness[experiments$TIPI_7 == "Agree moderately"] <- 6
experiments$agreeableness[experiments$TIPI_7 == "Agree strongly"] <- 7
#### TIPI 2 (reversed; see Gosling et al for more on coding)
experiments$agreeableness[experiments$TIPI_2 == ""] <- 0
experiments$agreeableness[experiments$TIPI_2 == "Disagree strongly"] <- 7
experiments$agreeableness[experiments$TIPI_2 == "Disagree moderately"] <- 6
experiments$agreeableness[experiments$TIPI_2 == "Disagree a little"] <- 5
experiments$agreeableness[experiments$TIPI_2 == "Neither agree nor diagree"] <- 4
experiments$agreeableness[experiments$TIPI_2 == "Agree a little"] <- 3
experiments$agreeableness[experiments$TIPI_2 == "Agree moderately"] <- 2
experiments$agreeableness[experiments$TIPI_2 == "Agree strongly"] <- 1

table(experiments$agreeableness)

### CREATING CONSCIENTIOUSNESS FOLDED MEASURE (single indicator based on TIPI 3 and 8) == High on conscientiousness = 7, low on conscientiousness = 1
#### TIPI 3
experiments$conscientiousness[experiments$TIPI_3 == ""] <- 0
experiments$conscientiousness[experiments$TIPI_3 == "Disagree strongly"] <- 1
experiments$conscientiousness[experiments$TIPI_3 == "Disagree moderately"] <- 2
experiments$conscientiousness[experiments$TIPI_3 == "Disagree a little"] <- 3
experiments$conscientiousness[experiments$TIPI_3 == "Neither agree nor diagree"] <- 4
experiments$conscientiousness[experiments$TIPI_3 == "Agree a little"] <- 5
experiments$conscientiousness[experiments$TIPI_3 == "Agree moderately"] <- 6
experiments$conscientiousness[experiments$TIPI_3 == "Agree strongly"] <- 7
#### TIPI 8 (reversed; see Gosling et al for more on coding)
experiments$conscientiousness[experiments$TIPI_8 == ""] <- 0
experiments$conscientiousness[experiments$TIPI_8 == "Disagree strongly"] <- 7
experiments$conscientiousness[experiments$TIPI_8 == "Disagree moderately"] <- 6
experiments$conscientiousness[experiments$TIPI_8 == "Disagree a little"] <- 5
experiments$conscientiousness[experiments$TIPI_8 == "Neither agree nor diagree"] <- 4
experiments$conscientiousness[experiments$TIPI_8 == "Agree a little"] <- 3
experiments$conscientiousness[experiments$TIPI_8 == "Agree moderately"] <- 2
experiments$conscientiousness[experiments$TIPI_8 == "Agree strongly"] <- 1

table(experiments$conscientiousness)

### CREATING EMOTIONAL STABILITY FOLDED MEASURE (single indicator based on TIPI 4 and 9) == High on emotional.stability = 7, low on emotional.stability = 1
#### TIPI 9
experiments$emotional.stability[experiments$TIPI_9 == ""] <- 0
experiments$emotional.stability[experiments$TIPI_9 == "Disagree strongly"] <- 1
experiments$emotional.stability[experiments$TIPI_9 == "Disagree moderately"] <- 2
experiments$emotional.stability[experiments$TIPI_9 == "Disagree a little"] <- 3
experiments$emotional.stability[experiments$TIPI_9 == "Neither agree nor diagree"] <- 4
experiments$emotional.stability[experiments$TIPI_9 == "Agree a little"] <- 5
experiments$emotional.stability[experiments$TIPI_9 == "Agree moderately"] <- 6
experiments$emotional.stability[experiments$TIPI_9 == "Agree strongly"] <- 7
#### TIPI 4 (reversed; see Gosling et al for more on coding)
experiments$emotional.stability[experiments$TIPI_4 == ""] <- 0
experiments$emotional.stability[experiments$TIPI_4 == "Disagree strongly"] <- 7
experiments$emotional.stability[experiments$TIPI_4 == "Disagree moderately"] <- 6
experiments$emotional.stability[experiments$TIPI_4 == "Disagree a little"] <- 5
experiments$emotional.stability[experiments$TIPI_4 == "Neither agree nor diagree"] <- 4
experiments$emotional.stability[experiments$TIPI_4 == "Agree a little"] <- 3
experiments$emotional.stability[experiments$TIPI_4 == "Agree moderately"] <- 2
experiments$emotional.stability[experiments$TIPI_4 == "Agree strongly"] <- 1

table(experiments$emotional.stability)

### CREATING OPENNESS TO EXPERIENCES FOLDED MEASURE (single indicator based on TIPI 5 and 10) == High on openness = 7, low on openness = 1
#### TIPI 5
experiments$openness[experiments$TIPI_5 == ""] <- 0
experiments$openness[experiments$TIPI_5 == "Disagree strongly"] <- 1
experiments$openness[experiments$TIPI_5 == "Disagree moderately"] <- 2
experiments$openness[experiments$TIPI_5 == "Disagree a little"] <- 3
experiments$openness[experiments$TIPI_5 == "Neither agree nor diagree"] <- 4
experiments$openness[experiments$TIPI_5 == "Agree a little"] <- 5
experiments$openness[experiments$TIPI_5 == "Agree moderately"] <- 6
experiments$openness[experiments$TIPI_5 == "Agree strongly"] <- 7
#### TIPI 10 (reversed; see Gosling et al for more on coding)
experiments$openness[experiments$TIPI_10 == ""] <- 0
experiments$openness[experiments$TIPI_10 == "Disagree strongly"] <- 7
experiments$openness[experiments$TIPI_10 == "Disagree moderately"] <- 6
experiments$openness[experiments$TIPI_10 == "Disagree a little"] <- 5
experiments$openness[experiments$TIPI_10 == "Neither agree nor diagree"] <- 4
experiments$openness[experiments$TIPI_10 == "Agree a little"] <- 3
experiments$openness[experiments$TIPI_10 == "Agree moderately"] <- 2
experiments$openness[experiments$TIPI_10 == "Agree strongly"] <- 1

table(experiments$openness)


### Dichotomous personality indicators based on responses
experiments$extravert <- ifelse(experiments$extraversion==5 | 
                                  experiments$extraversion==6 | 
                                  experiments$extraversion==7, 1, 0)

experiments$agreeable <- ifelse(experiments$agreeableness==5 | 
                                  experiments$agreeableness==6 | 
                                  experiments$agreeableness==7, 1, 0)

experiments$conscientious <- ifelse(experiments$conscientiousness==5 | 
                                      experiments$conscientiousness==6 | 
                                      experiments$conscientiousness==7, 1, 0)

experiments$stable <- ifelse(experiments$emotional.stability==5 | 
                               experiments$emotional.stability==6 | 
                               experiments$emotional.stability==7, 1, 0)

experiments$open <- ifelse(experiments$openness==5 | 
                             experiments$openness==6 | 
                             experiments$openness==7, 1, 0)
