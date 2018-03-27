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

### OLS Models for SI, by experiment wave (scenario numbers 1, 2, 3, and 4)
## DISTANCE models for each wave:
m1.w1.distance <- lm(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat, # RandHumanTreat as baseline
                     data = experiments[experiments$scenario_num == 1,]); summary(m1.w1.distance)

m1.w2.distance <- lm(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring, # RandHumanTreat as baseline
                     data = experiments[experiments$scenario_num == 2,]); summary(m1.w2.distance)

m1.w3.distance <- lm(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring, # RandHumanTreat as baseline
                     data = experiments[experiments$scenario_num == 3,]); summary(m1.w3.distance)

m1.w4.distance <- lm(distance ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring, # RandHumanTreat as baseline
                     data = experiments[experiments$scenario_num == 4,]); summary(m1.w4.distance)

#stargazer(m1.w1.distance, m1.w2.distance, m1.w3.distance, m1.w4.distance, out=c("OLS.distance.html"))

## WEIGHT models for each wave:
# Subsetting first to allow for restriction, wave1[wave1$adviceWt <= 1,]
wave1 <- subset(experiments, scenario_num == 1)
wave2 <- subset(experiments, scenario_num == 2)
wave3 <- subset(experiments, scenario_num == 3)
wave4 <- subset(experiments, scenario_num == 4)

m1.w1.weight <- lm(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat, # RandHumanTreat as baseline
                     data = wave1[wave1$adviceWt <= 1,]); summary(m1.w1.weight)

m1.w2.weight <- lm(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat, # RandHumanTreat as baseline
                     data = wave2[wave2$adviceWt <= 1,]); summary(m1.w2.weight)

m1.w3.weight <- lm(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat, # RandHumanTreat as baseline
                     data = wave3[wave3$adviceWt <= 1,]); summary(m1.w3.weight)

m1.w4.weight <- lm(adviceWt ~ AvgHumanTreat + AlgHumanTreat + LernerTreat, # RandHumanTreat as baseline
                     data = wave4[wave4$adviceWt <= 1,]); summary(m1.w4.weight)

#stargazer(m1.w1.weight, m1.w2.weight, m1.w3.weight, m1.w4.weight, out=c("OLS.weight.html"))

## BRIER models for each wave:
m1.w1.brier <- lm(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat, # RandHumanTreat as baseline
                     data = experiments[experiments$scenario_num == 1,]); summary(m1.w1.brier)

m1.w2.brier <- lm(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring, # RandHumanTreat as baseline
                     data = experiments[experiments$scenario_num == 2,]); summary(m1.w2.brier)

m1.w3.brier <- lm(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring, # RandHumanTreat as baseline
                     data = experiments[experiments$scenario_num == 3,]); summary(m1.w3.brier)

m1.w4.brier <- lm(brier ~ AvgHumanTreat + AlgHumanTreat + LernerTreat + Anchoring, # RandHumanTreat as baseline
                     data = experiments[experiments$scenario_num == 4,]); summary(m1.w4.brier)

#stargazer(m1.w1.brier, m1.w2.brier, m1.w3.brier, m1.w4.brier, out=c("OLS.brier.html"))
