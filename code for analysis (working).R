# Load a few packages
library(plyr)
library(arm)
library(ggplot2)
library(tidyr)

# Load the data (from Ryan's DB); see README markdown for this

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

# Calculate Brier Distance to Advice
# Experiment 1
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

# Experiment 2
experiment2$avgHumanBrier <- ((0.01*experiment2$Average.Human_1 - 0.01*experiment2$advice1)^2 +
                                (0.01*experiment2$Average.Human_2 - 0.01*experiment2$advice2)^2)/5

experiment2$randHumanBrier <- ((0.01*experiment2$Random.Human_1 - 0.01*experiment2$advice1)^2 +
                                 (0.01*experiment2$Random.Human_2 - 0.01*experiment2$advice2)^2)/5

experiment2$algHumanBrier <- ((0.01*experiment2$Averaging.Algorithm_1 - 0.01*experiment2$advice1)^2 +
                                (0.01*experiment2$Averaging.Algorithm_2 - 0.01*experiment2$advice2)^2)/5

experiment2$lernerBrier <- ((0.01*experiment2$Learning.Algorithm_1 - 0.01*experiment2$advice1)^2 +
                              (0.01*experiment2$Learning.Algorithm_2 - 0.01*experiment2$advice2)^2)/5

experiment2$avgHumanBrier2 <- ((0.01*experiment2$Anch.Avg.Human_1 - 0.01*experiment2$advice1)^2 +
                                (0.01*experiment2$Anch.Avg.Human_2 - 0.01*experiment2$advice2)^2)/5

experiment2$randHumanBrier2 <- ((0.01*experiment2$Anch.Rand.Human_1 - 0.01*experiment2$advice1)^2 +
                                 (0.01*experiment2$Anch.Rand.Human_2 - 0.01*experiment2$advice2)^2)/5

experiment2$algHumanBrier2 <- ((0.01*experiment2$Anch.alg.Combine_1 - 0.01*experiment2$advice1)^2 +
                                (0.01*experiment2$Anch.alg.Combine_2 - 0.01*experiment2$advice2)^2)/5

experiment2$lernerBrier2 <- ((0.01*experiment2$Anch.Full.Alg_1 - 0.01*experiment2$advice1)^2 +
                              (0.01*experiment2$Anch.Full.Alg_2 - 0.01*experiment2$advice2)^2)/5

experiment2$brier <- rowMeans(cbind(experiment2$avgHumanBrier,experiment2$randHumanBrier,
                                    experiment2$algHumanBrier,experiment2$lernerBrier,
                                    experiment2$avgHumanBrier2,experiment2$randHumanBrier2,
                                    experiment2$algHumanBrier2,experiment2$lernerBrier2),na.rm=TRUE)

# Experiment 3
experiment3$avgHumanBrier <- ((0.01*experiment3$Average.Human_1 - 0.01*experiment3$advice1)^2 +
                                (0.01*experiment3$Average.Human_2 - 0.01*experiment3$advice2)^2 +
                                (0.01*experiment3$Average.Human_3 - 0.01*experiment3$advice3)^2 +
                                (0.01*experiment3$Average.Human_4 - 0.01*experiment3$advice4)^2)/5

experiment3$randHumanBrier <- ((0.01*experiment3$Random.Human_1 - 0.01*experiment3$advice1)^2 +
                                 (0.01*experiment3$Random.Human_2 - 0.01*experiment3$advice2)^2 +
                                 (0.01*experiment3$Random.Human_3 - 0.01*experiment3$advice3)^2 +
                                 (0.01*experiment3$Random.Human_4 - 0.01*experiment3$advice4)^2)/5

experiment3$algHumanBrier <- ((0.01*experiment3$Averaging.Algorithm_1 - 0.01*experiment3$advice1)^2 +
                                (0.01*experiment3$Averaging.Algorithm_2 - 0.01*experiment3$advice2)^2 +
                                (0.01*experiment3$Averaging.Algorithm_3 - 0.01*experiment3$advice3)^2 +
                                (0.01*experiment3$Averaging.Algorithm_4 - 0.01*experiment3$advice4)^2)/5

experiment3$lernerBrier <- ((0.01*experiment3$Learning.Algorithm_1 - 0.01*experiment3$advice1)^2 +
                              (0.01*experiment3$Learning.Algorithm_2 - 0.01*experiment3$advice2)^2 +
                              (0.01*experiment3$Learning.Algorithm_3 - 0.01*experiment3$advice3)^2 +
                              (0.01*experiment3$Learning.Algorithm_4 - 0.01*experiment3$advice4)^2)/5

experiment3$avgHumanBrier2 <- ((0.01*experiment3$Anch.Avg.Human_1 - 0.01*experiment3$advice1)^2 +
                                 (0.01*experiment3$Anch.Avg.Human_2 - 0.01*experiment3$advice2)^2 +
                                 (0.01*experiment3$Anch.Avg.Human_3 - 0.01*experiment3$advice3)^2 +
                                 (0.01*experiment3$Anch.Avg.Human_4 - 0.01*experiment3$advice4)^2)/5

experiment3$randHumanBrier2 <- ((0.01*experiment3$Anch.Rand.Human_1 - 0.01*experiment3$advice1)^2 +
                                  (0.01*experiment3$Anch.Rand.Human_2 - 0.01*experiment3$advice2)^2 +
                                  (0.01*experiment3$Anch.Rand.Human_3 - 0.01*experiment3$advice3)^2 +
                                  (0.01*experiment3$Anch.Rand.Human_4 - 0.01*experiment3$advice4)^2)/5

experiment3$algHumanBrier2 <- ((0.01*experiment3$Anch.alg.Combine_1 - 0.01*experiment3$advice1)^2 +
                                 (0.01*experiment3$Anch.alg.Combine_2 - 0.01*experiment3$advice2)^2 +
                                 (0.01*experiment3$Anch.alg.Combine_3 - 0.01*experiment3$advice3)^2 +
                                 (0.01*experiment3$Anch.alg.Combine_4 - 0.01*experiment3$advice4)^2)/5

experiment3$lernerBrier2 <- ((0.01*experiment3$Anch.Full.Alg_1 - 0.01*experiment3$advice1)^2 +
                               (0.01*experiment3$Anch.Full.Alg_2 - 0.01*experiment3$advice2)^2 +
                               (0.01*experiment3$Anch.Full.Alg_3 - 0.01*experiment3$advice3)^2 +
                               (0.01*experiment3$Anch.Full.Alg_4 - 0.01*experiment3$advice4)^2)/5

experiment3$brier <- rowMeans(cbind(experiment3$avgHumanBrier,experiment3$randHumanBrier,
                                    experiment3$algHumanBrier,experiment3$lernerBrier,
                                    experiment3$avgHumanBrier2,experiment3$randHumanBrier2,
                                    experiment3$algHumanBrier2,experiment3$lernerBrier2),na.rm=TRUE)

# Experiment 4
experiment4$avgHumanBrier <- ((0.01*experiment4$Average.Human_1 - 0.01*experiment4$advice1)^2 +
                                (0.01*experiment4$Average.Human_2 - 0.01*experiment4$advice2)^2 +
                                (0.01*experiment4$Average.Human_3 - 0.01*experiment4$advice3)^2 +
                                (0.01*experiment4$Average.Human_4 - 0.01*experiment4$advice4)^2)/5

experiment4$randHumanBrier <- ((0.01*experiment4$Random.Human_1 - 0.01*experiment4$advice1)^2 +
                                 (0.01*experiment4$Random.Human_2 - 0.01*experiment4$advice2)^2 +
                                 (0.01*experiment4$Random.Human_3 - 0.01*experiment4$advice3)^2 +
                                 (0.01*experiment4$Random.Human_4 - 0.01*experiment4$advice4)^2)/5

experiment4$algHumanBrier <- ((0.01*experiment4$Averaging.Algorithm_1 - 0.01*experiment4$advice1)^2 +
                                (0.01*experiment4$Averaging.Algorithm_2 - 0.01*experiment4$advice2)^2 +
                                (0.01*experiment4$Averaging.Algorithm_3 - 0.01*experiment4$advice3)^2 +
                                (0.01*experiment4$Averaging.Algorithm_4 - 0.01*experiment4$advice4)^2)/5

experiment4$lernerBrier <- ((0.01*experiment4$Learning.Algorithm_1 - 0.01*experiment4$advice1)^2 +
                              (0.01*experiment4$Learning.Algorithm_2 - 0.01*experiment4$advice2)^2 +
                              (0.01*experiment4$Learning.Algorithm_3 - 0.01*experiment4$advice3)^2 +
                              (0.01*experiment4$Learning.Algorithm_4 - 0.01*experiment4$advice4)^2)/5

experiment4$avgHumanBrier2 <- ((0.01*experiment4$Anch.Avg.Human_1 - 0.01*experiment4$advice1)^2 +
                                 (0.01*experiment4$Anch.Avg.Human_2 - 0.01*experiment4$advice2)^2 +
                                 (0.01*experiment4$Anch.Avg.Human_3 - 0.01*experiment4$advice3)^2 +
                                 (0.01*experiment4$Anch.Avg.Human_4 - 0.01*experiment4$advice4)^2)/5

experiment4$randHumanBrier2 <- ((0.01*experiment4$Anch.Rand.Human_1 - 0.01*experiment4$advice1)^2 +
                                  (0.01*experiment4$Anch.Rand.Human_2 - 0.01*experiment4$advice2)^2 +
                                  (0.01*experiment4$Anch.Rand.Human_3 - 0.01*experiment4$advice3)^2 +
                                  (0.01*experiment4$Anch.Rand.Human_4 - 0.01*experiment4$advice4)^2)/5

experiment4$algHumanBrier2 <- ((0.01*experiment4$Anch.alg.Combine_1 - 0.01*experiment4$advice1)^2 +
                                 (0.01*experiment4$Anch.alg.Combine_2 - 0.01*experiment4$advice2)^2 +
                                 (0.01*experiment4$Anch.alg.Combine_3 - 0.01*experiment4$advice3)^2 +
                                 (0.01*experiment4$Anch.alg.Combine_4 - 0.01*experiment4$advice4)^2)/5

experiment4$lernerBrier2 <- ((0.01*experiment4$Anch.Full.Alg_1 - 0.01*experiment4$advice1)^2 +
                               (0.01*experiment4$Anch.Full.Alg_2 - 0.01*experiment4$advice2)^2 +
                               (0.01*experiment4$Anch.Full.Alg_3 - 0.01*experiment4$advice3)^2 +
                               (0.01*experiment4$Anch.Full.Alg_4 - 0.01*experiment4$advice4)^2)/5

experiment4$brier <- rowMeans(cbind(experiment4$avgHumanBrier,experiment4$randHumanBrier,
                                    experiment4$algHumanBrier,experiment4$lernerBrier,
                                    experiment4$avgHumanBrier2,experiment4$randHumanBrier2,
                                    experiment4$algHumanBrier2,experiment4$lernerBrier2),na.rm=TRUE)

# Combine the data across all waves
experiments <- rbind.fill(experiment1, experiment2, experiment3, experiment4)

# Add in treatment indicators
experiments$avgHumanTreat <- ifelse(!is.na(experiments$avgHumanBrier) | 
                                      !is.na(experiments$avgHumanBrier2), 1, 0)
experiments$randHumanTreat <- ifelse(!is.na(experiments$randHumanBrier) | 
                                      !is.na(experiments$randHumanBrier2), 1, 0)
experiments$algHumanTreat <- ifelse(!is.na(experiments$algHumanBrier) | 
                                      !is.na(experiments$algHumanBrier2), 1, 0)
experiments$lernerTreat <- ifelse(!is.na(experiments$lernerBrier) | 
                                      !is.na(experiments$lernerBrier2), 1, 0)
experiments$anchoring <- ifelse(!is.na(experiments$avgHumanBrier2) |
                                  !is.na(experiments$randHumanBrier2) |
                                  !is.na(experiments$algHumanBrier2) |
                                  !is.na(experiments$lernerBrier2), 1, 0)

# Run (exploratory) separate models for each experiment
exp1mod <- lm(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring, data=experiments[experiments$scenario_num == 1,]); summary(exp1mod)

exp2mod <- lm(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring, data=experiments[experiments$scenario_num == 2,]); summary(exp2mod)

exp3mod <- lm(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring, data=experiments[experiments$scenario_num == 3,]); summary(exp3mod)

exp4mod <- lm(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring, data=experiments[experiments$scenario_num == 4,]); summary(exp4mod)

# RE model of treatments - lower Brier score is better - thus negative impacts are good
mod1 <- lmer(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring + (1|scenario_num),
             data = experiments)
mod1sum <- summary(mod1)
mod1sim <- sim(mod1)
mod1simcoef <- data.frame(coef(mod1sim)$fixef)
mod1simcoef$simulation <- rownames(mod1simcoef)
mod1simcoeflong <- gather(mod1simcoef, condition, coefficient, X.Intercept.:anchoring)
mod1simcoeflong$condition[mod1simcoeflong$condition == "anchoring"] <- "Anchoring"
mod1simcoeflong$condition[mod1simcoeflong$condition == "lernerTreat"] <- "Computer Algorithm"
mod1simcoeflong$condition[mod1simcoeflong$condition == "algHumanTreat"] <- "Algorithm Aggregation"
mod1simcoeflong$condition[mod1simcoeflong$condition == "avgHumanTreat"] <- "Average of Humans"

# Coef plot for each condition (wave 1)
ggplot(mod1simcoeflong[mod1simcoeflong$condition != "X.Intercept.",]) + 
  geom_boxplot(aes(condition, coefficient)) +
  xlab("Condition") + ylab("Coefficient (95% CI)") + geom_hline(aes(yintercept=0)) +
  theme_bw() + coord_flip()

mod2 <- lmer(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring +
             avgHumanTreat:anchoring + algHumanTreat:anchoring + lernerTreat:anchoring
             + (1|scenario_num), data = experiments); summary(mod2)

# Create additional control variables
experiments$numed[experiments$education == "Some college, or an associate degree"] <- 1
experiments$numed[experiments$education == "Bachelor's degree (for example, BA, AB, BS)"] <- 2
experiments$numed[experiments$education == "High school graduate/GED" ] <- 1
experiments$numed[experiments$education == "Doctoral degree (for example, PhD, EdD)" ] <- 3
experiments$numed[experiments$education == "Master's degree (for example, MA, MSW, MBA)" ] <- 3
experiments$numed[experiments$education == "Trade or vocational certification" ] <- 1
experiments$numed[experiments$education == "Some graduate school" ] <- 2
experiments$numed[experiments$education == "Professional degree (for example, MD, JD, DDS)" ] <- 3
experiments$numed[experiments$education == ""] <- 0

experiments$female[experiments$Gender == "Male"] <- 0
experiments$female[experiments$Gender == "Female"] <- 1


mod3 <- lmer(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring 
             + Age + female
             + (1|scenario_num), data = experiments); summary(mod3)
  
mod4 <- lmer(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring 
             + Age + Age:lernerTreat + female
             + (1|scenario_num), data = experiments); summary(mod4)

####
####
#### Phil's Work Below
####
####

# party: 0 = NAs/don't know; 1 = Democrat; 2 = Republican; 3 = independent/third party
experiments$party[experiments$partyID == ""] <- 0
experiments$party[experiments$partyID == "Don’t know or decline to state"] <- 0
experiments$party[experiments$partyID == "Democrat"] <- 1
experiments$party[experiments$partyID == "Republican"] <- 2
experiments$party[experiments$partyID == "Independent / No party"] <- 3
experiments$party[experiments$partyID == "Member of a third party (enter its name)"] <- 3

# controls: numed, female, party, Age

## FULL MODEL (no covariates/controls)
full.mod <- lmer(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring 
             + (1|scenario_num), data = experiments); summary(full.mod)

# use SJPLOT for quick/rough visualization of ffx, rfx, and various relationships
library(sjPlot)
library(sjmisc)

# set sjPlot theme if so desired
sjp.setTheme(geom.outline.color = "antiquewhite4", 
             geom.outline.size = 1, 
             geom.label.size = 4,
             geom.label.alpha = 2,
             geom.label.color = "grey50",
             title.color = "black", 
             title.size = 1.25, 
             axis.angle.x = 0, 
             axis.textcolor = "black", 
             base = theme_bw(),
             title.align = "center")

# basic RFX by group across "anchoring"
sjp.lmer(full.mod, type = "ri.slope")

# plot fixed effects depending on group levels
# emphasize waves with anchoring (emph.grp)
# only for "algorithm" condition (vars)
sjp.lmer(full.mod, 
         type = "ri.slope", 
         #emph.grp = c("2", "3", "4"), # waves with anchoring
         facet.grid = FALSE,
         vars = "lernerTreat")

## Full specification w/ controls
full.mod1 <- lmer(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring 
                 + Age + female + party + numed
                 + (1|scenario_num), data = experiments); summary(full.mod1)


# basic ffx point plot
sjp.lmer(full.mod1, type = "fe" 
         #axis.lim = c(-5, 15)
)

# plot effects for "Age" given significance
sjp.lmer(full.mod1, type = "pred", vars = "Age", show.ci = TRUE)

## STUFF TO LOOK AT LATER - in Wave 4 only (with personality, need for cognition, etc.)

## TIPI scoring (in relation to the Big 5 -- ‘‘R’’ denotes reverse-scored items): 
## See footnote in Appendix A, from Original TIPI article: Gosling, Samuel D., Peter J. Rentfrow, and William B. Swann Jr. "A very brief measure of the Big-Five personality domains." Journal of Research in personality 37.6 (2003): 504-528.
# Extraversion: 1, 6R; Agreeableness: 2R, 7; Conscientiousness; 3, 8R; Emotional Stability: 4R, 9; Openness to Experiences: 5, 10R.

# Extraversion (1, 6R): 
experiments$TIPI_1
experiments$TIPI_6 # needs to be R

# Agreeableness (2R, 7):
experiments$TIPI_2 # needs to be R
experiments$TIPI_7

# Conscientiousness (3, 8R):
experiments$TIPI_3
experiments$TIPI_8 # needs to be R

# Emotional Stability (4R, 9):
experiments$TIPI_4 # needs to be R
experiments$TIPI_9

# Openness to Experiences (5, 10R):
experiments$TIPI_5
experiments$TIPI_10 # needs to be R

### LOOK AT THIS LATER FOR REVERSE CODING:
original <- matrix(sample(6,50,replace=TRUE),10,5)
keys <- c(1,1,-1,-1,1)  #reverse the 3rd and 4th items
new <- reverse.code(keys,original,mini=rep(1,5),maxi=rep(6,5))
original[1:3,]
new[1:3,]


