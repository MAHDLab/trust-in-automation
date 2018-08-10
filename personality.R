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

# Read in file (from P DB)
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
### MODELS
###

#### Simple Bivariate effects to motivate
m1 <- lm(adviceWt ~ algorithm + judge,
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m1)

m2 <- lm(distance ~ algorithm + judge,
         data = cj1long[cj1long$adviceWt <= 1,]); summary(m2)

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

#### TRUST - Big 5
m7 <- lmer(tia ~ agreeable + open + 
             age + ed + female + partisanship +
             algorithm + judge +
             (1|ResponseId) + (1|scenario),
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m7)

#### Impacting Behavior? - Big 5
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

### Basic SJPlot Stuff as a start
## NfC/NfE
cj1long$NfC <- cj1long$needcog
cj1long$NfE <- cj1long$needjudge
cj1long$Algorithm <- cj1long$algorithm
cj1long$Judge <- cj1long$judge

m4 <- lmer(tia ~ NfC + NfE +
             age + ed + female + partisanship +
             algorithm + judge +
             (1|ResponseId) + (1|scenario),
           data = cj1long[cj1long$adviceWt <= 1,]); summary(m4)

library(sjPlot)
library(sjlabelled)
library(sjmisc)

plot_model(m4, vline.color = "dark gray", colors = "black", title = "Impact of NfC & NfE on Trust in Automation",
           order.terms = c(1, 2),
           rm.terms = c("age", "ed", "female", "partisanship", "algorithm", "judge"),
           auto.label = FALSE)

plot_model(m5, vline.color = "dark gray", colors = "black", title = "Impact of NfC & NfE on Weighting of Advice",
           order.terms = c(9, 10, 11, 12),
           rm.terms = c("needcog", "needjudge", "age", "ed", "female", "partisanship", "algorithm", "judge"),
           auto.label = FALSE)

plot_model(m6, vline.color = "dark gray", colors = "black", title = "Impact of NfC & NfE on Distance to Advice",
           order.terms = c(9, 10, 11, 12),
           rm.terms = c("needcog", "needjudge", "age", "ed", "female", "partisanship", "algorithm", "judge"),
           auto.label = FALSE)

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
