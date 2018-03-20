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

# MLM model of treatments - lower Brier score is better - thus negative impacts are good
mod1 <- lmer(brier ~ avgHumanTreat + algHumanTreat + lernerTreat + anchoring + 
                    (1|scenario_num), # rfx for exp wave
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
