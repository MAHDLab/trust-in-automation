# Analysis of criminal justice experiment

library(arm)
library(tidyverse)
library(here)
library(stargazer)

# Read in file
cj1 <- read_csv(here("results03162018.csv"))

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

model1 <- lmer(distance ~ algorithm + judge + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), data = cj1long[cj1long$adviceWt <= 1,])
summary(model1)
model2 <- lmer(adviceWt ~ algorithm + judge + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model2)
model3 <- lmer(brier ~ algorithm + judge + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model3)
model4 <- lmer(distance ~ algorithm + judge + I(algorithm*tia) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), data = cj1long[cj1long$adviceWt <= 1,])
summary(model4)
model5 <- lmer(adviceWt ~ algorithm + judge + I(algorithm*tia) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model5)
model6 <- lmer(brier ~ algorithm + judge + I(algorithm*tia) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model6)
model7 <- lmer(distance ~ algorithm + judge + I(algorithm*age) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), data = cj1long[cj1long$adviceWt <= 1,])
summary(model7)
model8 <- lmer(adviceWt ~ algorithm + judge + I(algorithm*age) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model8)
model9 <- lmer(brier ~ algorithm + judge + I(algorithm*age) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model9)
model10 <- lmer(distance ~ algorithm + judge + I(algorithm*ed) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), data = cj1long[cj1long$adviceWt <= 1,])
summary(model10)
model11 <- lmer(adviceWt ~ algorithm + judge + I(algorithm*ed) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model11)
model12 <- lmer(brier ~ algorithm + judge + I(algorithm*ed) + tia + age + ed + encouragement + 
                  female + partisanship + needcog + needjudge +
                  (1|ResponseId) + (1|scenario), 
                data = cj1long[cj1long$adviceWt <= 1,])
summary(model12)
model13 <- lmer(distance ~ algorithm + judge + I(algorithm*female) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), data = cj1long[cj1long$adviceWt <= 1,])
summary(model13)
model14 <- lmer(adviceWt ~ algorithm + judge + I(algorithm*female) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model14)
model15 <- lmer(brier ~ algorithm + judge + I(algorithm*female) + tia + age + ed + encouragement + 
                  female + partisanship + needcog + needjudge +
                  (1|ResponseId) + (1|scenario), 
                data = cj1long[cj1long$adviceWt <= 1,])
summary(model15)
model16 <- lmer(distance ~ algorithm + judge + I(algorithm*encouragement) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), data = cj1long[cj1long$adviceWt <= 1,])
summary(model16)
model17 <- lmer(adviceWt ~ algorithm + judge + I(algorithm*encouragement) + tia + age + ed + encouragement + 
                  female + partisanship + needcog + needjudge +
                  (1|ResponseId) + (1|scenario), 
                data = cj1long[cj1long$adviceWt <= 1,])
summary(model17)
model18 <- lmer(adviceWt ~ algorithm + judge + I(algorithm*encouragement) + tia + age + ed + encouragement + 
                  female + partisanship + needcog + needjudge +
                  (1|ResponseId) + (1|scenario), 
                data = cj1long[cj1long$adviceWt <= 1,])
summary(model18)


# Table of multilevel results
# Create table
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
# Find where you want to put in the random effect. In our case, this is right after the last fixed effect. Line: 39 in this case.
r <- 39
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
stddev.model1.participants <- attributes(VarCorr(model1)$ResponseId)$stddev
stddev.model1.scenarios <- attributes(VarCorr(model1)$scenario)$stddev
stddev.model2.participants <- attributes(VarCorr(model2)$ResponseId)$stddev
stddev.model2.scenarios <- attributes(VarCorr(model2)$scenario)$stddev
stddev.model3.participants <- attributes(VarCorr(model4)$ResponseId)$stddev
stddev.model3.scenarios <- attributes(VarCorr(model4)$scenario)$stddev
stddev.model4.participants <- attributes(VarCorr(model5)$ResponseId)$stddev
stddev.model4.scenarios <- attributes(VarCorr(model5)$scenario)$stddev
stddev.model5.participants <- attributes(VarCorr(model7)$ResponseId)$stddev
stddev.model5.scenarios <- attributes(VarCorr(model7)$scenario)$stddev
stddev.model6.participants <- attributes(VarCorr(model8)$ResponseId)$stddev
stddev.model6.scenarios <- attributes(VarCorr(model8)$scenario)$stddev
stddev.model7.participants <- attributes(VarCorr(model10)$ResponseId)$stddev
stddev.model7.scenarios <- attributes(VarCorr(model10)$scenario)$stddev
stddev.model8.participants <- attributes(VarCorr(model11)$ResponseId)$stddev
stddev.model8.scenarios <- attributes(VarCorr(model11)$scenario)$stddev
# Create a LaTex character row for the random effect
number.of.participants <- paste("\\# of Participants & ", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "\\\\")
stddev.participants <- paste("Participant Standard Deviation & ", round(stddev.model1.participants, 3), "&", round(stddev.model2.participants, 3), "&", round(stddev.model3.participants, 3), "&", round(stddev.model4.participants, 3), "&", round(stddev.model5.participants, 3), "&", round(stddev.model6.participants, 3), "&", round(stddev.model7.participants, 3), "&", round(stddev.model8.participants, 3), "\\\\")
number.of.scenarios <- paste("\\# of Scenarios & ", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "\\\\")
stddev.scenarios <- paste("Scenario Standard Deviation & ", round(stddev.model1.scenarios, 3), "&", round(stddev.model2.scenarios, 3), "&", round(stddev.model3.scenarios, 3), "&", round(stddev.model4.scenarios, 3), "&", round(stddev.model5.scenarios, 3), "&", round(stddev.model6.scenarios, 3), "&", round(stddev.model7.scenarios, 3), "&", round(stddev.model8.scenarios, 3), "\\\\")
# Add these lines to the table
Tables <- insertrow(Tables,number.of.participants,r+3)
Tables <- insertrow(Tables,stddev.participants,r+4)
Tables <- insertrow(Tables,newline,r+5)
Tables <- insertrow(Tables,number.of.scenarios,r+6)
Tables <- insertrow(Tables,stddev.scenarios,r+7)
Tables <- insertrow(Tables, hline, r+8)
# Write the table to a file that can be inserted into the document
write.table(Tables,file=here("REtable.tex"),
            sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)

# Create plots
# Model 1
mod1sum <- summary(model1)
mod1sim <- sim(model1)
mod1simcoef <- as.tibble(data.frame(coef(mod1sim)$fixef))
mod1simcoef$simulation <- rownames(mod1simcoef)
mod1simcoeflong <- gather(mod1simcoef, condition, coefficient, X.Intercept.:needjudge)
mod1simcoeflong$condition[mod1simcoeflong$condition == "algorithm"] <- "Computer Algorithm"
mod1simcoeflong$condition[mod1simcoeflong$condition == "judge"] <- "Judge"
mod1simcoeflong$condition[mod1simcoeflong$condition == "tia"] <- "Trust in Automation"
mod1simcoeflong$condition[mod1simcoeflong$condition == "age"] <- "Age"
mod1simcoeflong$condition[mod1simcoeflong$condition == "ed"] <- "Education"
mod1simcoeflong$condition[mod1simcoeflong$condition == "encoragement"] <- "Encouragement Treatment"
mod1simcoeflong$condition[mod1simcoeflong$condition == "female"] <- "Female"
mod1simcoeflong$condition[mod1simcoeflong$condition == "partisanship"] <- "Partisanship"
mod1simcoeflong$condition[mod1simcoeflong$condition == "needcog"] <- "Need for Cognition"
mod1simcoeflong$condition[mod1simcoeflong$condition == "needjudge"] <- "Need for Judgement"

p1 = ggplot(mod1simcoeflong[mod1simcoeflong$condition != "X.Intercept.",]) + 
  geom_boxplot(aes(condition, coefficient)) + ggtitle("Distance to Advice") +
  xlab("Condition") + ylab("Coefficient (95% CI)") + geom_hline(aes(yintercept=0)) +
  theme_bw() + coord_flip()

# Model 2
mod2sum <- summary(model2)
mod2sim <- sim(model2)
mod2simcoef <- as.tibble(data.frame(coef(mod2sim)$fixef))
mod2simcoef$simulation <- rownames(mod2simcoef)
mod2simcoeflong <- gather(mod2simcoef, condition, coefficient, X.Intercept.:needjudge)
mod2simcoeflong$condition[mod2simcoeflong$condition == "algorithm"] <- "Computer Algorithm"
mod2simcoeflong$condition[mod2simcoeflong$condition == "judge"] <- "Judge"
mod2simcoeflong$condition[mod2simcoeflong$condition == "tia"] <- "Trust in Automation"
mod2simcoeflong$condition[mod2simcoeflong$condition == "age"] <- "Age"
mod2simcoeflong$condition[mod2simcoeflong$condition == "ed"] <- "Education"
mod2simcoeflong$condition[mod2simcoeflong$condition == "encoragement"] <- "Encouragement Treatment"
mod2simcoeflong$condition[mod2simcoeflong$condition == "female"] <- "Female"
mod2simcoeflong$condition[mod2simcoeflong$condition == "partisanship"] <- "Partisanship"
mod2simcoeflong$condition[mod2simcoeflong$condition == "needcog"] <- "Need for Cognition"
mod2simcoeflong$condition[mod2simcoeflong$condition == "needjudge"] <- "Need for Judgement"

p2 = ggplot(mod2simcoeflong[mod2simcoeflong$condition != "X.Intercept.",]) + 
  geom_boxplot(aes(condition, coefficient)) + ggtitle("Weight of Advice") +
  xlab("Condition") + ylab("Coefficient (95% CI)") + geom_hline(aes(yintercept=0)) +
  theme_bw() + coord_flip()

# Model 4 Interaction Plot
mod4sum <- summary(model4)
tiaX <- seq(1,7)
maineffect <- fixef(model4)[2] + fixef(model4)[4] * tiaX
maineffect <- as.tibble(cbind(tiaX,maineffect))
mod4sim <- sim(model4)
mod4simcoef <- as.tibble(data.frame(coef(mod4sim)$fixef))
mod4simcoef$simulation <- rownames(mod4simcoef)
mod4simcoeflong <- mod4simcoef %>%
  mutate(simeffect1 = algorithm + I.algorithm...tia. * 1,
         simeffect2 = algorithm + I.algorithm...tia. * 2,
         simeffect3 = algorithm + I.algorithm...tia. * 3,
         simeffect4 = algorithm + I.algorithm...tia. * 4,
         simeffect5 = algorithm + I.algorithm...tia. * 5,
         simeffect6 = algorithm + I.algorithm...tia. * 6,
         simeffect7 = algorithm + I.algorithm...tia. * 7) %>%
  dplyr::select(simulation:simeffect7) %>%
  gather(tialevel, estimate, simeffect1:simeffect7) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel), nchar(tialevel))))

p3 = ggplot() + geom_line(data = mod4simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "gray") +
  geom_line(data = maineffect, aes(x=tiaX, y=maineffect), size = 1) +
  xlab("Trust in Automation") + ylab("Estimated Impact of Algorithm") + ggtitle("Distance to Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()

# Model 5 Interaction Plot
mod5sum <- summary(model5)
tiaX <- seq(1,7)
maineffect5 <- fixef(model5)[2] + fixef(model5)[4] * tiaX
maineffect5 <- as.tibble(cbind(tiaX,maineffect5))
mod5sim <- sim(model5)
mod5simcoef <- as.tibble(data.frame(coef(mod5sim)$fixef))
mod5simcoef$simulation <- rownames(mod5simcoef)
mod5simcoeflong <- mod5simcoef %>%
  mutate(simeffect1 = algorithm + I.algorithm...tia. * 1,
         simeffect2 = algorithm + I.algorithm...tia. * 2,
         simeffect3 = algorithm + I.algorithm...tia. * 3,
         simeffect4 = algorithm + I.algorithm...tia. * 4,
         simeffect5 = algorithm + I.algorithm...tia. * 5,
         simeffect6 = algorithm + I.algorithm...tia. * 6,
         simeffect7 = algorithm + I.algorithm...tia. * 7) %>%
  dplyr::select(simulation:simeffect7) %>%
  gather(tialevel, estimate, simeffect1:simeffect7) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel), nchar(tialevel))))

p4 = ggplot() + geom_line(data = mod5simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect5, aes(x=tiaX, y=maineffect5), size = 1) +
  xlab("Trust in Automation") + ylab("Estimated Impact of Algorithm") + ggtitle("Weight of Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()

# Model 7 Interaction Plot
mod7sum <- summary(model7)
tiaX <- seq(19,74, by = 5)
maineffect7 <- fixef(model7)[2] + fixef(model7)[4] * tiaX
maineffect7 <- as.tibble(cbind(tiaX,maineffect7))
mod7sim <- sim(model7)
mod7simcoef <- as.tibble(data.frame(coef(mod7sim)$fixef))
mod7simcoef$simulation <- rownames(mod7simcoef)
mod7simcoeflong <- mod7simcoef %>%
  mutate(simeffect19 = algorithm + I.algorithm...age. * 19,
         simeffect24 = algorithm + I.algorithm...age. * 24,
         simeffect29 = algorithm + I.algorithm...age. * 29,
         simeffect34 = algorithm + I.algorithm...age. * 34,
         simeffect39 = algorithm + I.algorithm...age. * 39,
         simeffect44 = algorithm + I.algorithm...age. * 44,
         simeffect49 = algorithm + I.algorithm...age. * 49,
         simeffect54 = algorithm + I.algorithm...age. * 54,
         simeffect59 = algorithm + I.algorithm...age. * 59,
         simeffect64 = algorithm + I.algorithm...age. * 64,
         simeffect69 = algorithm + I.algorithm...age. * 69,
         simeffect74 = algorithm + I.algorithm...age. * 74) %>%
  dplyr::select(simulation:simeffect74) %>%
  gather(tialevel, estimate, simeffect19:simeffect74) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel)-1, nchar(tialevel))))

p5 = ggplot() + geom_line(data = mod7simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect7, aes(x=tiaX, y=maineffect7), size = 1) +
  xlab("Age") + ylab("Estimated Impact of Algorithm") + ggtitle("Distance to Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()

# Model 8 Interaction Plot
mod8sum <- summary(model8)
tiaX <- seq(19,74, by = 5)
maineffect8 <- fixef(model8)[2] + fixef(model8)[4] * tiaX
maineffect8 <- as.tibble(cbind(tiaX,maineffect8))
mod8sim <- sim(model8)
mod8simcoef <- as.tibble(data.frame(coef(mod8sim)$fixef))
mod8simcoef$simulation <- rownames(mod8simcoef)
mod8simcoeflong <- mod8simcoef %>%
  mutate(simeffect19 = algorithm + I.algorithm...age. * 19,
         simeffect24 = algorithm + I.algorithm...age. * 24,
         simeffect29 = algorithm + I.algorithm...age. * 29,
         simeffect34 = algorithm + I.algorithm...age. * 34,
         simeffect39 = algorithm + I.algorithm...age. * 39,
         simeffect44 = algorithm + I.algorithm...age. * 44,
         simeffect49 = algorithm + I.algorithm...age. * 49,
         simeffect54 = algorithm + I.algorithm...age. * 54,
         simeffect59 = algorithm + I.algorithm...age. * 59,
         simeffect64 = algorithm + I.algorithm...age. * 64,
         simeffect69 = algorithm + I.algorithm...age. * 69,
         simeffect74 = algorithm + I.algorithm...age. * 74) %>%
  dplyr::select(simulation:simeffect74) %>%
  gather(tialevel, estimate, simeffect19:simeffect74) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel)-1, nchar(tialevel))))

p6 = ggplot() + geom_line(data = mod8simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect8, aes(x=tiaX, y=maineffect8), size = 1) +
  xlab("Age") + ylab("Estimated Impact of Algorithm") + ggtitle("Weight of Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()

# Interaction graph for model 10
mod10sum <- summary(model10)
tiaX <- seq(2,4)
maineffect10 <- fixef(model10)[2] + fixef(model10)[4] * tiaX
maineffect10 <- as.tibble(cbind(tiaX,maineffect10))
mod10sim <- sim(model10)
mod10simcoef <- as.tibble(data.frame(coef(mod10sim)$fixef))
mod10simcoef$simulation <- rownames(mod10simcoef)
mod10simcoeflong <- mod10simcoef %>%
  mutate(simeffect2 = algorithm + I.algorithm...ed. * 2,
         simeffect3 = algorithm + I.algorithm...ed. * 3,
         simeffect4 = algorithm + I.algorithm...ed. * 4) %>%
  dplyr::select(simulation:simeffect4) %>%
  gather(tialevel, estimate, simeffect2:simeffect4) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel), nchar(tialevel))))

p7 = ggplot() + geom_line(data = mod10simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect10, aes(x=tiaX, y=maineffect10), size = 1) +
  xlab("Education") + ylab("Estimated Impact of Algorithm") + ggtitle("Distance to Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()

# Interaction graph for model 11
mod11sum <- summary(model11)
tiaX <- seq(2,4)
maineffect11 <- fixef(model11)[2] + fixef(model11)[4] * tiaX
maineffect11 <- as.tibble(cbind(tiaX,maineffect11))
mod11sim <- sim(model11)
mod11simcoef <- as.tibble(data.frame(coef(mod11sim)$fixef))
mod11simcoef$simulation <- rownames(mod11simcoef)
mod11simcoeflong <- mod11simcoef %>%
  mutate(simeffect2 = algorithm + I.algorithm...ed. * 2,
         simeffect3 = algorithm + I.algorithm...ed. * 3,
         simeffect4 = algorithm + I.algorithm...ed. * 4) %>%
  dplyr::select(simulation:simeffect4) %>%
  gather(tialevel, estimate, simeffect2:simeffect4) %>%
  mutate(tia = as.numeric(substring(tialevel, nchar(tialevel), nchar(tialevel))))

p8 = ggplot() + geom_line(data = mod11simcoeflong,aes(x=tia,y=estimate, group=simulation), color = "grey") +
  geom_line(data = maineffect11, aes(x=tiaX, y=maineffect11), size = 1) +
  xlab("Education") + ylab("Estimated Impact of Algorithm") + ggtitle("Weight of Advice") +
  geom_hline(aes(yintercept = 0)) + theme_bw()


# combine graphs for direct model coefficients
grid.arrange(p1, p2, ncol=2)

grid.arrange(p3,p4,p5,p6,p7,p8,ncol =2)

# Create table for Brier scores
Tables <- stargazer(model3, model6, model9, model12, style="ajps", 
                    title="Models of Trust in Automation", 
                    dep.var.labels.include = FALSE#, 
                    #covariate.labels=c( "Ideology", "Authoritarianism", "Power Rating", "Ideology Rating","Power Rating : Ideology","Ideology Rating : Ideology")
)
# Convert Tables to data frame and coerce the one vector (also called Tables) to character
Tables <- as.data.frame(Tables)
Tables$Tables <- as.character(Tables$Tables)
# Find where you want to put in the random effect. In our case, this is right after the last fixed effect. Line: 23.
r <- 39
# Create some standard label lines
randomeffect <- "{\\bf Random Effect} & & \\\\"
hline <- "\\hline"
newline <- "\\\\"
# Now insert those label lines where they are needed
Tables <- insertrow(Tables, hline, r)
Tables <- insertrow(Tables,randomeffect,r+1)
Tables <- insertrow(Tables,hline,r+2)
# Get number of unique values in each grouping
num.participants <- sapply(ranef(model3),nrow)[1]
num.scenarios <- sapply(ranef(model3),nrow)[2]
# Get standard deviation of the random effect
stddev.model3.participants <- attributes(VarCorr(model3)$ResponseId)$stddev
stddev.model3.scenarios <- attributes(VarCorr(model13)$scenario)$stddev
stddev.model6.participants <- attributes(VarCorr(model6)$ResponseId)$stddev
stddev.model6.scenarios <- attributes(VarCorr(model6)$scenario)$stddev
stddev.model9.participants <- attributes(VarCorr(model9)$ResponseId)$stddev
stddev.model9.scenarios <- attributes(VarCorr(model9)$scenario)$stddev
stddev.model12.participants <- attributes(VarCorr(model12)$ResponseId)$stddev
stddev.model12.scenarios <- attributes(VarCorr(model12)$scenario)$stddev
# Create a LaTex character row for the random effect
number.of.participants <- paste("\\# of Participants & ", num.participants, "&", num.participants, "&", num.participants, "&", num.participants, "\\\\")
stddev.participants <- paste("Participant Standard Deviation & ", round(stddev.model3.participants, 3), "&", round(stddev.model6.participants, 3), "&", round(stddev.model9.participants, 3), "&", round(stddev.model12.participants, 3), "\\\\")
number.of.scenarios <- paste("\\# of Scenarios & ", num.scenarios, "&", num.scenarios, "&", num.scenarios, "&", num.scenarios, "\\\\")
stddev.scenarios <- paste("Scenario Standard Deviation & ", round(stddev.model3.scenarios, 3), "&", round(stddev.model6.scenarios, 3), "&", round(stddev.model9.scenarios, 3), "&", round(stddev.model12.scenarios, 3), "\\\\")
# Add these lines to the table
Tables <- insertrow(Tables,number.of.participants,r+3)
Tables <- insertrow(Tables,stddev.participants,r+4)
Tables <- insertrow(Tables,newline,r+5)
Tables <- insertrow(Tables,number.of.scenarios,r+6)
Tables <- insertrow(Tables,stddev.scenarios,r+7)
Tables <- insertrow(Tables, hline, r+8)
# Write the table to a file that can be inserted into the document
write.table(Tables,file=here("REtableBrierCJ.tex"),
            sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)



## OLS Models for SI
cjm1.w1.distance <- lm(distance ~ algorithm + judge, # RandHumanTreat as baseline
                     data = cj1long[cj1long$adviceWt <= 1,]); summary(cjm1.w1.distance)

## WEIGHT models for each wave:
cjm1.w1.weight <- lm(adviceWt ~ algorithm + judge, # RandHumanTreat as baseline
                   data = cj1long[cj1long$adviceWt <= 1,]); summary(cjm1.w1.weight)

## BRIER models for each wave:
cjm1.w1.brier <- lm(brier ~ algorithm + judge, # RandHumanTreat as baseline
                  data = cj1long[cj1long$adviceWt <= 1,]); summary(cjm1.w1.brier)

## Now Compile
# Create a baseline table - DISTANCE
Tables.ols.CJ <- stargazer(cjm1.w1.distance, cjm1.w1.weight, cjm1.w1.brier, 
                             style="ajps", 
                             title="Models of Trust in Automation: Criminal Justice OLS Models", 
                             dep.var.labels.include = FALSE
)

# Convert Tables to data frame and coerce the one vector (also called Tables) to character
Tables.ols.CJ <- as.data.frame(Tables.ols.CJ)
Tables.ols.CJ$Tables.ols.CJ <- as.character(Tables.ols.CJ$Tables.ols.CJ)

# Write the table to a file that can be inserted into the document
write.table(Tables.ols.CJ,file=("OLStableCJ.tex"),
            sep="",row.names= FALSE,na="", quote = FALSE, col.names = FALSE)



# Test if TIA is generalized trust

model13 <- lmer(distance ~ algorithm + judge + I(judge*tia) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), data = cj1long[cj1long$adviceWt <= 1,])
summary(model13)
model14 <- lmer(adviceWt ~ algorithm + judge + I(judge*tia) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model14)
model15 <- lmer(brier ~ algorithm + judge + I(judge*tia) + tia + age + ed + encouragement + 
                 female + partisanship + needcog + needjudge +
                 (1|ResponseId) + (1|scenario), 
               data = cj1long[cj1long$adviceWt <= 1,])
summary(model15)
