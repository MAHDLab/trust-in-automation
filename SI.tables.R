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

## TABLES
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

# Find where you want to put in the random effect. In our case, this is right after the last fixed effect. Line: 36 (after \hline after "constant")
r <- 36

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
