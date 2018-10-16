##
##
## MODELS START ON LINE 360
##
##

### CJ ORGANIZATION

# Analysis of criminal justice experiment

library(plyr)
library(arm)
library(ggplot2)
library(tidyr)
library(tibble)
library(gridExtra)
library(tidyverse)
library(stargazer)
library(grid)
library(sjPlot)
library(sjlabelled)
library(sjmisc)

# Read in file
cj1 <- read.csv("/Users/bpwaggo/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Criminal Justice Experiments/Criminal Justice Experiment 1/results03162018.csv")

# Add in the information on advice and control variables
cj1 <- cj1 %>%
  mutate(adviceSC2 = 80, adviceSC4 = 68, adviceSC7 = 52, adviceSC11 = 32,
         adviceSC12 = 56, adviceSC13 = 36, adviceSC18 = 37, adviceSC19 = 87,
         advicebSC2 = 100 - 80, advicebSC4 = 100 - 68, advicebSC7 = 100 - 52, advicebSC11 = 100 - 32,
         advicebSC12 = 100 - 56, advicebSC13 = 100 - 36, advicebSC18 = 100 - 37, advicebSC19 = 100 - 87,
         encouragement = ifelse(!is.na(treatment_explain), 1, 0),
         female = ifelse(gender == "Female", 1, 0),
         edvalue = case_when(education == "Elementary of some high school" ~ 1,
                             education == "High school graduate/GED" ~ 2,
                             education == "Trade or vocational certification" ~ 2,
                             education == "Some college, or an associate degree" ~ 3,
                             education == "Bachelor's degree (for example, BA, AB, BS)" ~ 3,
                             education == "Some graduate school" ~ 4,
                             education == "Master's degree (for example, MA, MSW, MBA)" ~ 4,
                             education == "Professional degree (for example, MD, JD, DDS)" ~ 4,
                             education == "Doctoral degree (for example, PhD, EdD)" ~ 4),
         toa1 = case_when(trustinautomation_1 == "Strongly agree" ~ 7,
                          trustinautomation_1 == "Agree" ~ 6,
                          trustinautomation_1 == "Somewhat agree" ~ 5,
                          trustinautomation_1 == "Neither agree nor disagree" ~ 4,
                          trustinautomation_1 == "Somewhat disagree" ~ 3,
                          trustinautomation_1 == "Disagree" ~ 2,
                          trustinautomation_1 == "Strongly disagree" ~ 1),
         toa2 = case_when(trustinautomation_2 == "Strongly agree" ~ 7,
                          trustinautomation_2 == "Agree" ~ 6,
                          trustinautomation_2 == "Somewhat agree" ~ 5,
                          trustinautomation_2 == "Neither agree nor disagree" ~ 4,
                          trustinautomation_2 == "Somewhat disagree" ~ 3,
                          trustinautomation_2 == "Disagree" ~ 2,
                          trustinautomation_2 == "Strongly disagree" ~ 1),
         toa3 = case_when(trustinautomation_3 == "Strongly agree" ~ 7,
                          trustinautomation_3 == "Agree" ~ 6,
                          trustinautomation_3 == "Somewhat agree" ~ 5,
                          trustinautomation_3 == "Neither agree nor disagree" ~ 4,
                          trustinautomation_3 == "Somewhat disagree" ~ 3,
                          trustinautomation_3 == "Disagree" ~ 2,
                          trustinautomation_3 == "Strongly disagree" ~ 1),
         toa4 = case_when(trustinautomation_4 == "Strongly agree" ~ 1,
                          trustinautomation_4 == "Agree" ~ 2,
                          trustinautomation_4 == "Somewhat agree" ~ 3,
                          trustinautomation_4 == "Neither agree nor disagree" ~ 4,
                          trustinautomation_4 == "Somewhat disagree" ~ 5,
                          trustinautomation_4 == "Disagree" ~ 6,
                          trustinautomation_4 == "Strongly disagree" ~ 7),
         toa5 = case_when(trustinautomation_5 == "Strongly agree" ~ 1,
                          trustinautomation_5 == "Agree" ~ 2,
                          trustinautomation_5 == "Somewhat agree" ~ 3,
                          trustinautomation_5 == "Neither agree nor disagree" ~ 4,
                          trustinautomation_5 == "Somewhat disagree" ~ 5,
                          trustinautomation_5 == "Disagree" ~ 6,
                          trustinautomation_5 == "Strongly disagree" ~ 7),
         toa6 = case_when(trustinautomation_6 == "Strongly agree" ~ 7,
                          trustinautomation_6 == "Agree" ~ 6,
                          trustinautomation_6 == "Somewhat agree" ~ 5,
                          trustinautomation_6 == "Neither agree nor disagree" ~ 4,
                          trustinautomation_6 == "Somewhat disagree" ~ 3,
                          trustinautomation_6 == "Disagree" ~ 2,
                          trustinautomation_6 == "Strongly disagree" ~ 1),
         toa7 = case_when(trustinautomation_7 == "Strongly agree" ~ 7,
                          trustinautomation_7 == "Agree" ~ 6,
                          trustinautomation_7 == "Somewhat agree" ~ 5,
                          trustinautomation_7 == "Neither agree nor disagree" ~ 4,
                          trustinautomation_7 == "Somewhat disagree" ~ 3,
                          trustinautomation_7 == "Disagree" ~ 2,
                          trustinautomation_7 == "Strongly disagree" ~ 1),
         toa8 = case_when(trustinautomation_8 == "Strongly agree" ~ 1,
                          trustinautomation_8 == "Agree" ~ 2,
                          trustinautomation_8 == "Somewhat agree" ~ 3,
                          trustinautomation_8 == "Neither agree nor disagree" ~ 4,
                          trustinautomation_8 == "Somewhat disagree" ~ 5,
                          trustinautomation_8 == "Disagree" ~ 6,
                          trustinautomation_8 == "Strongly disagree" ~ 7),
         trustAutomation = (toa1 + toa2 + toa3 + toa4 + toa5 + toa6 + toa7 + toa8)/8,
         partisanship = case_when(partylean == "I lean neither way" ~ 4,
                                  partylean == "Lean Democrat" ~ 3,
                                  partylean == "Lean Republican" ~ 5,
                                  party == "Republican" & partystrong == "No, NOT strongly" ~ 6,
                                  party == "Democrat" & partystrong == "No, NOT strongly" ~ 2,
                                  party == "Republican" & partystrong == "Yes, strongly" ~ 7,
                                  party == "Democrat" & partystrong == "Yes, strongly" ~ 1),
         tipi1 = case_when(big10_1 == "Agree strongly" ~ 7,
                           big10_1 == "Agree moderately" ~ 6,
                           big10_1 == "Agree a little" ~ 5,
                           big10_1 == "Neither agree nor disagree" ~ 4,
                           big10_1 == "Disagree a little" ~ 3,
                           big10_1 == "Disagree moderately" ~ 2,
                           big10_1 == "Disagree strongly" ~ 1),
         tipi2 = case_when(big10_2 == "Agree strongly" ~ 1,
                           big10_2 == "Agree moderately" ~ 2,
                           big10_2 == "Agree a little" ~ 3,
                           big10_2 == "Neither agree nor disagree" ~ 4,
                           big10_2 == "Disagree a little" ~ 5,
                           big10_2 == "Disagree moderately" ~ 6,
                           big10_2 == "Disagree strongly" ~ 7),
         tipi3 = case_when(big10_3 == "Agree strongly" ~ 7,
                           big10_3 == "Agree moderately" ~ 6,
                           big10_3 == "Agree a little" ~ 5,
                           big10_3 == "Neither agree nor disagree" ~ 4,
                           big10_3 == "Disagree a little" ~ 3,
                           big10_3 == "Disagree moderately" ~ 2,
                           big10_3 == "Disagree strongly" ~ 1),
         tipi4 = case_when(big10_4 == "Agree strongly" ~ 1,
                           big10_4 == "Agree moderately" ~ 2,
                           big10_4 == "Agree a little" ~ 3,
                           big10_4 == "Neither agree nor disagree" ~ 4,
                           big10_4 == "Disagree a little" ~ 5,
                           big10_4 == "Disagree moderately" ~ 6,
                           big10_4 == "Disagree strongly" ~ 7),
         tipi5 = case_when(big10_5 == "Agree strongly" ~ 7,
                           big10_5 == "Agree moderately" ~ 6,
                           big10_5 == "Agree a little" ~ 5,
                           big10_5 == "Neither agree nor disagree" ~ 4,
                           big10_5 == "Disagree a little" ~ 3,
                           big10_5 == "Disagree moderately" ~ 2,
                           big10_5 == "Disagree strongly" ~ 1),
         tipi6 = case_when(big10_6 == "Agree strongly" ~ 1,
                           big10_6 == "Agree moderately" ~ 2,
                           big10_6 == "Agree a little" ~ 3,
                           big10_6 == "Neither agree nor disagree" ~ 4,
                           big10_6 == "Disagree a little" ~ 5,
                           big10_6 == "Disagree moderately" ~ 6,
                           big10_6 == "Disagree strongly" ~ 7),
         tipi7 = case_when(big10_7 == "Agree strongly" ~ 7,
                           big10_7 == "Agree moderately" ~ 6,
                           big10_7 == "Agree a little" ~ 5,
                           big10_7 == "Neither agree nor disagree" ~ 4,
                           big10_7 == "Disagree a little" ~ 3,
                           big10_7 == "Disagree moderately" ~ 2,
                           big10_7 == "Disagree strongly" ~ 1),
         tipi8 = case_when(big10_8 == "Agree strongly" ~ 1,
                           big10_8 == "Agree moderately" ~ 2,
                           big10_8 == "Agree a little" ~ 3,
                           big10_8 == "Neither agree nor disagree" ~ 4,
                           big10_8 == "Disagree a little" ~ 5,
                           big10_8 == "Disagree moderately" ~ 6,
                           big10_8 == "Disagree strongly" ~ 7),
         tipi9 = case_when(big10_9 == "Agree strongly" ~ 7,
                           big10_9 == "Agree moderately" ~ 6,
                           big10_9 == "Agree a little" ~ 5,
                           big10_9 == "Neither agree nor disagree" ~ 4,
                           big10_9 == "Disagree a little" ~ 3,
                           big10_9 == "Disagree moderately" ~ 2,
                           big10_9 == "Disagree strongly" ~ 1),
         tipi10 = case_when(big10_10 == "Agree strongly" ~ 1,
                            big10_10 == "Agree moderately" ~ 2,
                            big10_10 == "Agree a little" ~ 3,
                            big10_10 == "Neither agree nor disagree" ~ 4,
                            big10_10 == "Disagree a little" ~ 5,
                            big10_10 == "Disagree moderately" ~ 6,
                            big10_10 == "Disagree strongly" ~ 7),
         extroverted = (tipi1 + tipi6)/2,
         agreeableness = (tipi2 + tipi7)/2,
         openness = (tipi5 + tipi10)/2,
         conscientiousness = (tipi3 + tipi8)/2,
         stability = (tipi4 + tipi9)/2,
         needcog1 = case_when(needforcognition_1 == "Extremely characteristic" ~ 5,
                              needforcognition_1 == "Somewhat characteristic" ~ 4,
                              needforcognition_1 == "Uncertain" ~ 3,
                              needforcognition_1 == "Somewhat uncharacteristic" ~ 2,
                              needforcognition_1 == "Extremely uncharacteristic" ~ 1),
         needcog2 = case_when(needforcognition_2 == "Extremely characteristic" ~ 5,
                              needforcognition_2 == "Somewhat characteristic" ~ 4,
                              needforcognition_2 == "Uncertain" ~ 3,
                              needforcognition_2 == "Somewhat uncharacteristic" ~ 2,
                              needforcognition_2 == "Extremely uncharacteristic" ~ 1),
         needcog3 = case_when(needforcognition_3 == "Extremely characteristic" ~ 5,
                              needforcognition_3 == "Somewhat characteristic" ~ 4,
                              needforcognition_3 == "Uncertain" ~ 3,
                              needforcognition_3 == "Somewhat uncharacteristic" ~ 2,
                              needforcognition_3 == "Extremely uncharacteristic" ~ 1),
         needcog4 = case_when(needforcognition_4 == "Extremely characteristic" ~ 1,
                              needforcognition_4 == "Somewhat characteristic" ~ 2,
                              needforcognition_4 == "Uncertain" ~ 3,
                              needforcognition_4 == "Somewhat uncharacteristic" ~ 4,
                              needforcognition_4 == "Extremely uncharacteristic" ~ 5),
         needjudge = (needcog1 + needcog3)/2,
         needcognition = (needcog2 + needcog4)/2)

# Generate difference and recommendation weight results and conditions
cj1 <- cj1 %>%
  mutate(adTypeSC2 = ifelse(!is.na(sc2a_1) | !is.na(s2aoa_1), "algorithm", 
                            ifelse(!is.na(sc2j_1) | !is.na(s2joa_1), "judge",
                                   ifelse(!is.na(sc2mt_1) | !is.na(s2moa_1), "mt", NA))),
         adTypeSC4 = ifelse(!is.na(sc4a_1) | !is.na(s4aoa_1), "algorithm", 
                            ifelse(!is.na(sc4j_1) | !is.na(s4joa_1), "judge",
                                   ifelse(!is.na(sc4mt_1) | !is.na(s4moa_1), "mt", NA))),
         adTypeSC7 = ifelse(!is.na(sc7a_1) | !is.na(s7aoa_1), "algorithm", 
                            ifelse(!is.na(sc7j_1) | !is.na(s7joa_1), "judge",
                                   ifelse(!is.na(sc7mt_1) | !is.na(s7moa_1), "mt", NA))),
         adTypeSC11 = ifelse(!is.na(sc11a_1) | !is.na(s11aoa_1), "algorithm", 
                             ifelse(!is.na(sc11j_1) | !is.na(s11joa_1), "judge",
                                    ifelse(!is.na(sc11mt_1) | !is.na(s11moa_1), "mt", NA))),
         adTypeSC12 = ifelse(!is.na(sc12a_1) | !is.na(s12aoa_1), "algorithm", 
                             ifelse(!is.na(sc12j_1) | !is.na(s12joa_1), "judge",
                                    ifelse(!is.na(sc12mt_1) | !is.na(s12moa_1), "mt", NA))),
         adTypeSC13 = ifelse(!is.na(sc13a_1) | !is.na(s13aoa_1), "algorithm", 
                             ifelse(!is.na(sc13j_1) | !is.na(s13joa_1), "judge",
                                    ifelse(!is.na(sc13mt_1) | !is.na(s13moa_1), "mt", NA))),
         adTypeSC18 = ifelse(!is.na(sc18a_1) | !is.na(s18aoa_1), "algorithm", 
                             ifelse(!is.na(sc18j_1) | !is.na(s18joa_1), "judge",
                                    ifelse(!is.na(sc18mt_1) | !is.na(s18moa_1), "mt", NA))),
         adTypeSC19 = ifelse(!is.na(sc19a_1) | !is.na(s19aoa_1), "algorithm", 
                             ifelse(!is.na(sc19j_1) | !is.na(s19joa_1), "judge",
                                    ifelse(!is.na(sc19mt_1) | !is.na(s19moa_1), "mt", NA))))

cj1 <- cj1 %>%
  mutate(predSC2 = rowMeans(cbind(sc2a_1, s2aoa_1, sc2j_1, s2joa_1, sc2mt_1, s2moa_1), na.rm = TRUE),
         predSC4 = rowMeans(cbind(sc4a_1, s4aoa_1, sc4j_1, s4joa_1, sc4mt_1, s4moa_1), na.rm = TRUE),
         predSC7 = rowMeans(cbind(sc7a_1, s7aoa_1, sc7j_1, s7joa_1, sc7mt_1, s7moa_1), na.rm = TRUE),
         predSC11 = rowMeans(cbind(sc11a_1, s11aoa_1, sc11j_1, s11joa_1, sc11mt_1, s11moa_1), na.rm = TRUE),
         predSC12 = rowMeans(cbind(sc12a_1, s12aoa_1, sc12j_1, s12joa_1, sc12mt_1, s12moa_1), na.rm = TRUE),
         predSC13 = rowMeans(cbind(sc13a_1, s13aoa_1, sc13j_1, s13joa_1, sc13mt_1, s13moa_1), na.rm = TRUE),
         predSC18 = rowMeans(cbind(sc18a_1, s18aoa_1, sc18j_1, s18joa_1, sc18mt_1, s18moa_1), na.rm = TRUE),
         predSC19 = rowMeans(cbind(sc19a_1, s19aoa_1, sc19j_1, s19joa_1, sc19mt_1, s19moa_1), na.rm = TRUE),
         predbSC2 = rowMeans(cbind(sc2a_2, s2aoa_2, sc2j_2, s2joa_2, sc2mt_2, s2moa_2), na.rm = TRUE),
         predbSC4 = rowMeans(cbind(sc4a_2, s4aoa_2, sc4j_2, s4joa_2, sc4mt_2, s4moa_2), na.rm = TRUE),
         predbSC7 = rowMeans(cbind(sc7a_2, s7aoa_2, sc7j_2, s7joa_2, sc7mt_2, s7moa_2), na.rm = TRUE),
         predbSC11 = rowMeans(cbind(sc11a_2, s11aoa_2, sc11j_2, s11joa_2, sc11mt_2, s11moa_2), na.rm = TRUE),
         predbSC12 = rowMeans(cbind(sc12a_2, s12aoa_2, sc12j_2, s12joa_2, sc12mt_2, s12moa_2), na.rm = TRUE),
         predbSC13 = rowMeans(cbind(sc13a_2, s13aoa_2, sc13j_2, s13joa_2, sc13mt_2, s13moa_2), na.rm = TRUE),
         predbSC18 = rowMeans(cbind(sc18a_2, s18aoa_2, sc18j_2, s18joa_2, sc18mt_2, s18moa_2), na.rm = TRUE),
         predbSC19 = rowMeans(cbind(sc19a_2, s19aoa_2, sc19j_2, s19joa_2, sc19mt_2, s19moa_2), na.rm = TRUE))

cj1 <- cj1 %>%
  mutate(initSC2 = rowMeans(cbind(sc2_1, s2ao_1, sc2_1, s2jo_1, sc2_1, s2mo_1), na.rm = TRUE),
         initSC4 = rowMeans(cbind(sc4_1, s4ao_1, sc4_1, s4jo_1, sc4_1, s4mo_1), na.rm = TRUE),
         initSC7 = rowMeans(cbind(sc7_1, s7ao_1, sc7_1, s7jo_1, sc7_1, s7mo_1), na.rm = TRUE),
         initSC11 = rowMeans(cbind(sc11_1, s11ao_1, sc11_1, s11jo_1, sc11_1, s11mo_1), na.rm = TRUE),
         initSC12 = rowMeans(cbind(sc12_1, s12ao_1, sc12_1, s12jo_1, sc12_1, s12mo_1), na.rm = TRUE),
         initSC13 = rowMeans(cbind(sc13_1, s13ao_1, sc13_1, s13jo_1, sc13_1, s13mo_1), na.rm = TRUE),
         initSC18 = rowMeans(cbind(sc18_1, s18ao_1, sc18_1, s18jo_1, sc18_1, s18mo_1), na.rm = TRUE),
         initSC19 = rowMeans(cbind(sc19_1, s19ao_1, sc19_1, s19jo_1, sc19_1, s19mo_1), na.rm = TRUE),
         initbSC2 = rowMeans(cbind(sc2_2, s2ao_2, sc2_2, s2jo_2, sc2_2, s2mo_2), na.rm = TRUE),
         initbSC4 = rowMeans(cbind(sc4_2, s4ao_2, sc4_2, s4jo_2, sc4_2, s4mo_2), na.rm = TRUE),
         initbSC7 = rowMeans(cbind(sc7_2, s7ao_2, sc7_2, s7jo_2, sc7_2, s7mo_2), na.rm = TRUE),
         initbSC11 = rowMeans(cbind(sc11_2, s11ao_2, sc11_2, s11jo_2, sc11_2, s11mo_2), na.rm = TRUE),
         initbSC12 = rowMeans(cbind(sc12_2, s12ao_2, sc12_2, s12jo_2, sc12_2, s12mo_2), na.rm = TRUE),
         initbSC13 = rowMeans(cbind(sc13_2, s13ao_2, sc13_2, s13jo_2, sc13_2, s13mo_2), na.rm = TRUE),
         initbSC18 = rowMeans(cbind(sc18_2, s18ao_2, sc18_2, s18jo_2, sc18_2, s18mo_2), na.rm = TRUE),
         initbSC19 = rowMeans(cbind(sc19_2, s19ao_2, sc19_2, s19jo_2, sc19_2, s19mo_2), na.rm = TRUE))

sc2 <- cj1 %>% 
  select(ResponseId, trustAutomation, age, encouragement, female, partisanship,
         extroverted, agreeableness, openness, conscientiousness,
         stability, needcognition, needjudge, edvalue, ends_with("SC2")) %>% 
  mutate(scenario = 2)
names(sc2) <- c("ResponseId", "tia", "age", "encouragement", "female",
                "partisanship", "extrovert", "agreeable", "open", 
                "conscientious", "stable", "needcog", "needjudge", "ed",
                "advice", "adviceb", "adType", "pred", "predb", "init", 
                "initb","scenario")
sc4 <- cj1 %>% 
  select(ResponseId, trustAutomation, age, encouragement, female, partisanship,
         extroverted, agreeableness, openness, conscientiousness,
         stability, needcognition, needjudge, edvalue, ends_with("SC4")) %>%
  mutate(scenario = 4)
names(sc4) <- c("ResponseId", "tia", "age", "encouragement", "female",
                "partisanship", "extrovert", "agreeable", "open", 
                "conscientious", "stable", "needcog", "needjudge", "ed",
                "advice", "adviceb", "adType", "pred", "predb", "init", 
                "initb","scenario")
sc7 <- cj1 %>% 
  select(ResponseId, trustAutomation, age, encouragement, female, partisanship,
         extroverted, agreeableness, openness, conscientiousness,
         stability, needcognition, needjudge, edvalue, ends_with("SC7")) %>% 
  mutate(scenario = 7)
names(sc7) <- c("ResponseId", "tia", "age", "encouragement", "female",
                "partisanship", "extrovert", "agreeable", "open", 
                "conscientious", "stable", "needcog", "needjudge", "ed",
                "advice", "adviceb", "adType", "pred", "predb", "init", 
                "initb","scenario")
sc11 <- cj1 %>% 
  select(ResponseId, trustAutomation, age, encouragement, female, partisanship,
         extroverted, agreeableness, openness, conscientiousness,
         stability, needcognition, needjudge, edvalue, ends_with("SC11")) %>% 
  mutate(scenario = 11)
names(sc11) <- c("ResponseId", "tia", "age", "encouragement", "female",
                 "partisanship", "extrovert", "agreeable", "open", 
                 "conscientious", "stable", "needcog", "needjudge", "ed",
                 "advice", "adviceb", "adType", "pred", "predb", "init", 
                 "initb","scenario")
sc12 <- cj1 %>% 
  select(ResponseId, trustAutomation, age, encouragement, female, partisanship,
         extroverted, agreeableness, openness, conscientiousness,
         stability, needcognition, needjudge, edvalue, ends_with("SC12")) %>% 
  mutate(scenario = 12)
names(sc12) <- c("ResponseId", "tia", "age", "encouragement", "female",
                 "partisanship", "extrovert", "agreeable", "open", 
                 "conscientious", "stable", "needcog", "needjudge", "ed",
                 "advice", "adviceb", "adType", "pred", "predb", "init", 
                 "initb","scenario")
sc13 <- cj1 %>% 
  select(ResponseId, trustAutomation, age, encouragement, female, partisanship,
         extroverted, agreeableness, openness, conscientiousness,
         stability, needcognition, needjudge, edvalue, ends_with("SC13")) %>% 
  mutate(scenario = 13)
names(sc13) <- c("ResponseId", "tia", "age", "encouragement", "female",
                 "partisanship", "extrovert", "agreeable", "open", 
                 "conscientious", "stable", "needcog", "needjudge", "ed",
                 "advice", "adviceb", "adType", "pred", "predb", "init", 
                 "initb","scenario")
sc18 <- cj1 %>% 
  select(ResponseId, trustAutomation, age, encouragement, female, partisanship,
         extroverted, agreeableness, openness, conscientiousness,
         stability, needcognition, needjudge, edvalue, ends_with("SC18")) %>% 
  mutate(scenario = 18)
names(sc18) <- c("ResponseId", "tia", "age", "encouragement", "female",
                 "partisanship", "extrovert", "agreeable", "open", 
                 "conscientious", "stable", "needcog", "needjudge", "ed",
                 "advice", "adviceb", "adType", "pred", "predb", "init", 
                 "initb","scenario")
sc19 <- cj1 %>% 
  select(ResponseId, trustAutomation, age, encouragement, female, partisanship,
         extroverted, agreeableness, openness, conscientiousness,
         stability, needcognition, needjudge, edvalue, ends_with("SC19")) %>% 
  mutate(scenario = 19)
names(sc19) <- c("ResponseId", "tia", "age", "encouragement", "female",
                 "partisanship", "extrovert", "agreeable", "open", 
                 "conscientious", "stable", "needcog", "needjudge", "ed",
                 "advice", "adviceb", "adType", "pred", "predb", "init", 
                 "initb","scenario")

cj1long <- rbind(sc2, sc4, sc7, sc11, sc12, sc13, sc18, sc19)

cj1long <- cj1long %>%
  mutate(distance = (abs(pred-advice) + abs(predb-adviceb))/2,
         adviceWt = ((abs(pred-init)/abs(advice-init)) + (abs(predb-initb)/abs(adviceb-initb)))/2,
         brier = ((advice-pred)^2 + (adviceb-predb)^2)/2,
         algorithm = ifelse(adType == "algorithm", 1, 0),
         judge = ifelse(adType == "judge", 1, 0))


###
###
### MODELS
###
###


#### Simple Bivariate effects to motivate
m1 <- lm(adviceWt ~ algorithm + judge,
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m1)

m2 <- lm(distance ~ algorithm + judge,
         data = cj1long[cj1long$adviceWt <= 1,]); summary(m2)

stargazer(m1, m2, single.row = TRUE, out="simple.motivation.html")



#### TRUST - NC and NE
m4 <- lmer(tia ~ needcog + needjudge +
           age + ed + female + partisanship +
           algorithm + judge +
         (1|ResponseId) + (1|scenario),
         data = cj1long[cj1long$adviceWt <= 1,]); summary(m4)


#### Impacting Behavior? - NC and NE
m5 <- lmer(adviceWt ~ needcog + needjudge +
             age + ed + female + partisanship +
             algorithm + judge +
             I(algorithm*needcog) + I(algorithm*needjudge) +I(judge*needcog) + I(judge*needjudge) +
             (1|ResponseId) + (1|scenario),
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m5)

m6 <- lmer(distance ~ needcog + needjudge +
             age + ed + female + partisanship +
             algorithm + judge +
             I(algorithm*needcog) + I(algorithm*needjudge) +I(judge*needcog) + I(judge*needjudge) +
             (1|ResponseId) + (1|scenario),
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m6)

stargazer(m4, single.row = TRUE, out="cj.nfc.nfe.TIA.html")

stargazer(m5, m6, single.row = TRUE, out="cj.nfc.nfe.BEHAVIOR.html")



## TRUST - Big 5
m7 <- lmer(tia ~ agreeable + open + 
             age + ed + female + partisanship +
             algorithm + judge +
             (1|ResponseId) + (1|scenario),
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m7)

## Impacting Behavior? - Big 5
m8 <- lmer(adviceWt ~ agreeable + open + 
             age + ed + female + partisanship +
             algorithm + judge +
             I(algorithm*agreeable) + I(algorithm*open)+
             I(judge*agreeable) + I(judge*open) +
             (1|ResponseId) + (1|scenario),
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m8)

m9 <- lmer(distance ~ agreeable + open + 
             age + ed + female + partisanship +
             algorithm + judge +
             I(algorithm*agreeable) + I(algorithm*open)+
             I(judge*agreeable) + I(judge*open) +
             (1|ResponseId) + (1|scenario),
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m9)



stargazer(m7, single.row = TRUE, out="cj.big5.TIA.html")

stargazer(m8, m9, single.row = TRUE, out="cj.big5.BEHAVIOR.html")


## ALL MODELS:
stargazer(m4, m7, single.row = TRUE, out="trust.html")

stargazer(m5, m6, m8, m9, single.row = TRUE, out="behavior.html")


###
###
### PLOTS
###
###


## CJ - NfC/NfE
cj1long$NfC <- cj1long$needcog
cj1long$NfE <- cj1long$needjudge
cj1long$Algorithm <- cj1long$algorithm
cj1long$Judge <- cj1long$judge

# Re-run for Sjplot labels
m4 <- lmer(tia ~ NfC + NfE +
             age + ed + female + partisanship +
             algorithm + judge +
             (1|ResponseId) + (1|scenario),
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m4)


#plot_model(m4, vline.color = "dark gray", colors = "black", title = "Impact of NfC & NfE on Trust in Automation",
#           order.terms = c(1, 2),
#           rm.terms = c("age", "ed", "female", "partisanship", "algorithm", "judge"),
#           auto.label = FALSE)

#plot_model(m5, vline.color = "dark gray", colors = "black", title = "Impact of NfC & NfE on Weighting of Advice",
#           order.terms = c(9, 10, 11, 12),
#           rm.terms = c("needcog", "needjudge", "age", "ed", "female", "partisanship", "algorithm", "judge"),
#           auto.label = FALSE)

#plot_model(m6, vline.color = "dark gray", colors = "black", title = "Impact of NfC & NfE on Distance to Advice",
#           order.terms = c(9, 10, 11, 12),
#           rm.terms = c("needcog", "needjudge", "age", "ed", "female", "partisanship", "algorithm", "judge"),
#           auto.label = FALSE)


# TIA Conditional Plots
plot_model(m4, type = "pred", terms = c("needcog", "algorithm"), 
           title = "Conditional Effect of NfC & Algorithm Condition on Trust in Automation") + ylim(3,5.5)

plot_model(m4, type = "pred", terms = c("needcog", "judge"), 
           title = "Conditional Effect of NfC & Judge Condition on Trust in Automation") + ylim(3,5.5)

# Weight Conditional Plots
plot_model(m5, type = "pred", terms = c("needcog", "algorithm"), 
           title = "Conditional Effect of NfC & Algorithm Condition on Weight of Advice") + ylim(0,0.6)

plot_model(m5, type = "pred", terms = c("needcog", "judge"), 
           title = "Conditional Effect of NfC & Judge Condition on Weight of Advice") + ylim(0,0.6)



#



































# OLD FROM OTHER PAPER ******** Analysis of political science experiment

# From P's DB for quick access
experiment4 <- read.csv("/Users/bpwaggo/Dropbox/Hybrid Forecasting Houston/Trust in Automation Experiments/Experiment 4/results03052018.csv", as.is = TRUE)


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


# Personality: TIPI
### EXTRAVERSION
#### TIPI 1
experiments$TIPI_1[experiments$TIPI_1 == ""] <- 0
experiments$TIPI_1[experiments$TIPI_1 == "Disagree strongly"] <- 1
experiments$TIPI_1[experiments$TIPI_1 == "Disagree moderately"] <- 2
experiments$TIPI_1[experiments$TIPI_1 == "Disagree a little"] <- 3
experiments$TIPI_1[experiments$TIPI_1 == "Neither agree nor diagree"] <- 4
experiments$TIPI_1[experiments$TIPI_1 == "Agree a little"] <- 5
experiments$TIPI_1[experiments$TIPI_1 == "Agree moderately"] <- 6
experiments$TIPI_1[experiments$TIPI_1 == "Agree strongly"] <- 7
#### TIPI 6 (reversed; see Gosling et al for more on coding)
experiments$TIPI_6[experiments$TIPI_6 == ""] <- 0
experiments$TIPI_6[experiments$TIPI_6 == "Disagree strongly"] <- 7
experiments$TIPI_6[experiments$TIPI_6 == "Disagree moderately"] <- 6
experiments$TIPI_6[experiments$TIPI_6 == "Disagree a little"] <- 5
experiments$TIPI_6[experiments$TIPI_6 == "Neither agree nor diagree"] <- 4
experiments$TIPI_6[experiments$TIPI_6 == "Agree a little"] <- 3
experiments$TIPI_6[experiments$TIPI_6 == "Agree moderately"] <- 2
experiments$TIPI_6[experiments$TIPI_6 == "Agree strongly"] <- 1


### AGREEABLENESS
#### TIPI 7
experiments$TIPI_7[experiments$TIPI_7 == ""] <- 0
experiments$TIPI_7[experiments$TIPI_7 == "Disagree strongly"] <- 1
experiments$TIPI_7[experiments$TIPI_7 == "Disagree moderately"] <- 2
experiments$TIPI_7[experiments$TIPI_7 == "Disagree a little"] <- 3
experiments$TIPI_7[experiments$TIPI_7 == "Neither agree nor diagree"] <- 4
experiments$TIPI_7[experiments$TIPI_7 == "Agree a little"] <- 5
experiments$TIPI_7[experiments$TIPI_7 == "Agree moderately"] <- 6
experiments$TIPI_7[experiments$TIPI_7 == "Agree strongly"] <- 7
#### TIPI 2 (reversed; see Gosling et al for more on coding)
experiments$TIPI_2[experiments$TIPI_2 == ""] <- 0
experiments$TIPI_2[experiments$TIPI_2 == "Disagree strongly"] <- 7
experiments$TIPI_2[experiments$TIPI_2 == "Disagree moderately"] <- 6
experiments$TIPI_2[experiments$TIPI_2 == "Disagree a little"] <- 5
experiments$TIPI_2[experiments$TIPI_2 == "Neither agree nor diagree"] <- 4
experiments$TIPI_2[experiments$TIPI_2 == "Agree a little"] <- 3
experiments$TIPI_2[experiments$TIPI_2 == "Agree moderately"] <- 2
experiments$TIPI_2[experiments$TIPI_2 == "Agree strongly"] <- 1


### CONSCIENTIOUSNESS
#### TIPI 3
experiments$TIPI_3[experiments$TIPI_3 == ""] <- 0
experiments$TIPI_3[experiments$TIPI_3 == "Disagree strongly"] <- 1
experiments$TIPI_3[experiments$TIPI_3 == "Disagree moderately"] <- 2
experiments$TIPI_3[experiments$TIPI_3 == "Disagree a little"] <- 3
experiments$TIPI_3[experiments$TIPI_3 == "Neither agree nor diagree"] <- 4
experiments$TIPI_3[experiments$TIPI_3 == "Agree a little"] <- 5
experiments$TIPI_3[experiments$TIPI_3 == "Agree moderately"] <- 6
experiments$TIPI_3[experiments$TIPI_3 == "Agree strongly"] <- 7
#### TIPI 8 (reversed; see Gosling et al for more on coding)
experiments$TIPI_8[experiments$TIPI_8 == ""] <- 0
experiments$TIPI_8[experiments$TIPI_8 == "Disagree strongly"] <- 7
experiments$TIPI_8[experiments$TIPI_8 == "Disagree moderately"] <- 6
experiments$TIPI_8[experiments$TIPI_8 == "Disagree a little"] <- 5
experiments$TIPI_8[experiments$TIPI_8 == "Neither agree nor diagree"] <- 4
experiments$TIPI_8[experiments$TIPI_8 == "Agree a little"] <- 3
experiments$TIPI_8[experiments$TIPI_8 == "Agree moderately"] <- 2
experiments$TIPI_8[experiments$TIPI_8 == "Agree strongly"] <- 1


### EMOTIONAL STABILITY
#### TIPI 9
experiments$TIPI_9[experiments$TIPI_9 == ""] <- 0
experiments$TIPI_9[experiments$TIPI_9 == "Disagree strongly"] <- 1
experiments$TIPI_9[experiments$TIPI_9 == "Disagree moderately"] <- 2
experiments$TIPI_9[experiments$TIPI_9 == "Disagree a little"] <- 3
experiments$TIPI_9[experiments$TIPI_9 == "Neither agree nor diagree"] <- 4
experiments$TIPI_9[experiments$TIPI_9 == "Agree a little"] <- 5
experiments$TIPI_9[experiments$TIPI_9 == "Agree moderately"] <- 6
experiments$TIPI_9[experiments$TIPI_9 == "Agree strongly"] <- 7
#### TIPI 4 (reversed; see Gosling et al for more on coding)
experiments$TIPI_4[experiments$TIPI_4 == ""] <- 0
experiments$TIPI_4[experiments$TIPI_4 == "Disagree strongly"] <- 7
experiments$TIPI_4[experiments$TIPI_4 == "Disagree moderately"] <- 6
experiments$TIPI_4[experiments$TIPI_4 == "Disagree a little"] <- 5
experiments$TIPI_4[experiments$TIPI_4 == "Neither agree nor diagree"] <- 4
experiments$TIPI_4[experiments$TIPI_4 == "Agree a little"] <- 3
experiments$TIPI_4[experiments$TIPI_4 == "Agree moderately"] <- 2
experiments$TIPI_4[experiments$TIPI_4 == "Agree strongly"] <- 1


### OPENNESS TO EXPERIENCES
#### TIPI 5
experiments$TIPI_5[experiments$TIPI_5 == ""] <- 0
experiments$TIPI_5[experiments$TIPI_5 == "Disagree strongly"] <- 1
experiments$TIPI_5[experiments$TIPI_5 == "Disagree moderately"] <- 2
experiments$TIPI_5[experiments$TIPI_5 == "Disagree a little"] <- 3
experiments$TIPI_5[experiments$TIPI_5 == "Neither agree nor diagree"] <- 4
experiments$TIPI_5[experiments$TIPI_5 == "Agree a little"] <- 5
experiments$TIPI_5[experiments$TIPI_5 == "Agree moderately"] <- 6
experiments$TIPI_5[experiments$TIPI_5 == "Agree strongly"] <- 7
#### TIPI 10 (reversed; see Gosling et al for more on coding)
experiments$TIPI_10[experiments$TIPI_10 == ""] <- 0
experiments$TIPI_10[experiments$TIPI_10 == "Disagree strongly"] <- 7
experiments$TIPI_10[experiments$TIPI_10 == "Disagree moderately"] <- 6
experiments$TIPI_10[experiments$TIPI_10 == "Disagree a little"] <- 5
experiments$TIPI_10[experiments$TIPI_10 == "Neither agree nor diagree"] <- 4
experiments$TIPI_10[experiments$TIPI_10 == "Agree a little"] <- 3
experiments$TIPI_10[experiments$TIPI_10 == "Agree moderately"] <- 2
experiments$TIPI_10[experiments$TIPI_10 == "Agree strongly"] <- 1

# First make numeric
experiments$TIPI_1 <- as.numeric(experiments$TIPI_1)
experiments$TIPI_2 <- as.numeric(experiments$TIPI_2)
experiments$TIPI_3 <- as.numeric(experiments$TIPI_3)
experiments$TIPI_4 <- as.numeric(experiments$TIPI_4)
experiments$TIPI_5 <- as.numeric(experiments$TIPI_5)
experiments$TIPI_6 <- as.numeric(experiments$TIPI_6)
experiments$TIPI_7 <- as.numeric(experiments$TIPI_7)
experiments$TIPI_8 <- as.numeric(experiments$TIPI_8)
experiments$TIPI_9 <- as.numeric(experiments$TIPI_9)
experiments$TIPI_10 <- as.numeric(experiments$TIPI_10)

experiments$extroverted = ((experiments$TIPI_1 + experiments$TIPI_6)/2)
experiments$agreeableness = (experiments$TIPI_2 + experiments$TIPI_7)/2
experiments$openness = (experiments$TIPI_5 + experiments$TIPI_10)/2
experiments$conscientiousness = (experiments$TIPI_3 + experiments$TIPI_8)/2
experiments$stability = (experiments$TIPI_4 + experiments$TIPI_9)/2


# Personality: Cognition and Judgement - ONLY IN 4 - exclude from full analysis
experiments$needcog1[experiments$CogEval_1 == " "] <- 0
experiments$needcog1[experiments$CogEval_1 == "Extremely characteristic"] <- 5
experiments$needcog1[experiments$CogEval_1 == "Somewhat characteristic"] <- 4
experiments$needcog1[experiments$CogEval_1 == "Uncertain"] <- 3
experiments$needcog1[experiments$CogEval_1 == "Somewhat uncharacteristic"] <- 2
experiments$needcog1[experiments$CogEval_1 == "Extremely uncharacteristic"] <- 1

experiments$needcog2[experiments$CogEval_2 == " "] <- 0
experiments$needcog2[experiments$CogEval_2 == "Extremely characteristic"] <- 5
experiments$needcog2[experiments$CogEval_2 == "Somewhat characteristic"] <- 4
experiments$needcog2[experiments$CogEval_2 == "Uncertain"] <- 3
experiments$needcog2[experiments$CogEval_2 == "Somewhat uncharacteristic"] <- 2
experiments$needcog2[experiments$CogEval_2 == "Extremely uncharacteristic"] <- 1

experiments$needcog3[experiments$CogEval_3 == " "] <- 0
experiments$needcog3[experiments$CogEval_3 == "Extremely characteristic"] <- 5
experiments$needcog3[experiments$CogEval_3 == "Somewhat characteristic"] <- 4
experiments$needcog3[experiments$CogEval_3 == "Uncertain"] <- 3
experiments$needcog3[experiments$CogEval_3 == "Somewhat uncharacteristic"] <- 2
experiments$needcog3[experiments$CogEval_3 == "Extremely uncharacteristic"] <- 1

experiments$needcog4[experiments$CogEval_4 == " "] <- 0
experiments$needcog4[experiments$CogEval_4 == "Extremely characteristic"] <- 1
experiments$needcog4[experiments$CogEval_4 == "Somewhat characteristic"] <- 2
experiments$needcog4[experiments$CogEval_4 == "Uncertain"] <- 3
experiments$needcog4[experiments$CogEval_4 == "Somewhat uncharacteristic"] <- 4
experiments$needcog4[experiments$CogEval_4 == "Extremely uncharacteristic"] <- 5

# Now make numeric for indicator calculation
experiments$needcog1 <- as.numeric(experiments$needcog1)
experiments$needcog2 <- as.numeric(experiments$needcog2)
experiments$needcog3 <- as.numeric(experiments$needcog3)
experiments$needcog4 <- as.numeric(experiments$needcog4)


experiments$needjudge = (experiments$needcog1 + experiments$needcog3)/2

experiments$needcognition = (experiments$needcog2 + experiments$needcog4)/2

#








# OLD*********************** for Appendix, create histograms for all variables used in both stages of analysis

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

## NOW COMPILE
# Function for adding rows to a data frame (will be used later)
insertrow <- function(existingDF, newrow, r) {
  existingDF[seq(r+1,nrow(existingDF)+1),] <- existingDF[seq(r,nrow(existingDF)),]
  existingDF[r,] <- newrow
  existingDF
}

# Create a baseline table - DISTANCE
Tables.ols.dist <- stargazer(m1.w1.distance, m1.w2.distance, m1.w3.distance, m1.w4.distance, 
                             style="ajps", 
                             title="Models of Trust in Automation: OLS Distance Models", 
                             dep.var.labels.include = FALSE
)

# Convert Tables to data frame and coerce the one vector (also called Tables) to character
Tables.ols.dist <- as.data.frame(Tables.ols.dist)
Tables.ols.dist$Tables.ols.dist <- as.character(Tables.ols.dist$Tables.ols.dist)

# Write the table to a file that can be inserted into the document
write.table(Tables.ols.dist,file=("OLStablepoliticalDIST.tex"),
            sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)



# Create a baseline table - WEIGHT
Tables.ols.weight <- stargazer(m1.w1.weight, m1.w2.weight, m1.w3.weight, m1.w4.weight,
                               style="ajps", 
                               title="Models of Trust in Automation: OLS Weight Models", 
                               dep.var.labels.include = FALSE
)

# Convert Tables to data frame and coerce the one vector (also called Tables) to character
Tables.ols.weight <- as.data.frame(Tables.ols.weight)
Tables.ols.weight$Tables.ols.weight <- as.character(Tables.ols.weight$Tables.ols.weight)

# Write the table to a file that can be inserted into the document
write.table(Tables.ols.weight,file=("OLStablepoliticalWEIGHT.tex"),
            sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)



# Create a baseline table - BRIER
Tables.ols.brier <- stargazer(m1.w1.brier, m1.w2.brier, m1.w3.brier, m1.w4.brier, 
                              style="ajps", 
                              title="Models of Trust in Automation: OLS Brier Models", 
                              dep.var.labels.include = FALSE
)

# Convert Tables to data frame and coerce the one vector (also called Tables) to character
Tables.ols.brier <- as.data.frame(Tables.ols.brier)
Tables.ols.brier$Tables.ols.brier <- as.character(Tables.ols.brier$Tables.ols.brier)

# Write the table to a file that can be inserted into the document
write.table(Tables.ols.brier,file=("OLStablepoliticalBRIER.tex"),
            sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)
