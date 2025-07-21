##############################################################
# ACPS Career Self-Efficacy Analysis - Data Wrangling Script            
# Authors: Asha Muralidharan     
# GitHub: asha-ec                              
# Last revised: 2025-03-07  
# Summary: A document that cleans and combines the data from the Career Self-Efficacy 
#          Surveys implemented on Starr Hill Pathways students and ACPS students
#          in 7th and 8th grade over two collection periods in 2023 and 2024
##############################################################

##############################################################
# Library Intros                               
##############################################################

library(tidyverse)
library(readxl)
library(writexl)
library(plotrix)

library(MatchIt)
library(WeightIt)
library(cobalt)
library(marginaleffects)

##############################################################
# Pull in Self-Efficacy Data                            
##############################################################

# Call in data as sent to me by ACPS
cse <- read_excel("../raw_data/EQCT_MSCSE_Alldata_03_06_2025.xlsx")

##############################################################
# Deal with duplicate students (8th graders)                          
##############################################################

cse <- cse[order(cse$survey_administration, decreasing = TRUE),]

cse <- cse %>% distinct(unique_id, .keep_all = TRUE)

##############################################################
# Change column names (for survey questions)                          
##############################################################

#rename question columns 

colnames(cse) <- c("unique_id","survey_administration","class_year","gender","race_ethnicity",                                                                                                             
                  "econ_dis", "starr_hill", "avid","sped", "EL", "simplegpa_gr6", "attendance_gr6",                                                                                                             
                  "reading_score_gr6", "math_score_gr6","SE1","SE2","SE3","SE4","SE5","SE6","SE7",
                  "SE8","SE9","SE10","SE11","OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9",
                  "OE10","occupation_1","occupation_2","occupation_3","date_completed")

##############################################################
# Build subscores                          
##############################################################

# Identify self-efficacy questions
survey_seqs <- c("SE1","SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11")
# Identify outcome expectations questions
survey_oeqs <- c("OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10")
survey_all_qs <- c("SE1","SE2","SE3","SE4","SE5","SE6","SE7","SE8","SE9","SE10","SE11",
                   "OE1","OE2","OE3","OE4","OE5","OE6","OE7","OE8","OE9","OE10")

# Attach scoring to numerical values and convert to numeric variable
cse <- cse %>% mutate_if(is.character, function(x) ifelse(x=="Strongly Agree","5",x))
cse <- cse %>% mutate_if(is.character,function(x) ifelse(x=="Agree","4",x))
cse <- cse %>% mutate_if(is.character,function(x) ifelse(x=="Not sure","3",x))
cse <- cse %>% mutate_if(is.character,function(x) ifelse(x=="Not Sure","3",x))
cse <- cse %>% mutate_if(is.character,function(x) ifelse(x=="Disagree","2",x))
cse <- cse %>% mutate_if(is.character,function(x) ifelse(x=="Strongly Disagree","1",x))

cse <- cse %>% mutate_at(survey_all_qs, as.numeric)

#Create means for score and subscores
cse <- cse %>%
  add_column(SE_subscore=rowMeans(cse[,survey_seqs], na.rm=FALSE))

cse <- cse %>%
  add_column(OE_subscore=rowMeans(cse[,survey_oeqs], na.rm=FALSE))

cse <- cse %>%
  add_column(CSE_composite=rowMeans(cse[,(survey_all_qs)], na.rm=FALSE))

cse$SE_subscore <- cse$SE_subscore %>% round(digits=2)
cse$OE_subscore <- cse$OE_subscore %>% round(digits=2)
cse$CSE_composite <- cse$CSE_composite %>% round(digits=2)

# Drop students without CSE_score

 cse <- cse %>% subset(!is.na(CSE_composite))
##############################################################
# Make necessary variables factors                         
##############################################################

cse <- cse %>% mutate_at(c("gender","race_ethnicity","class_year","econ_dis",
                           "avid","sped","EL","starr_hill", "survey_administration"), as.factor)
# Relevel variables to reassign references
cse <- within(cse, gender <- relevel(gender, ref = "M"))
cse <- within(cse, race_ethnicity <- relevel(race_ethnicity, ref = "WH"))
cse <- within(cse, class_year <- relevel(class_year, ref = "2030"))
cse <- within(cse, econ_dis <- relevel(econ_dis, ref = "N"))
cse <- within(cse, avid <- relevel(avid, ref = "N"))
cse <- within(cse, sped <- relevel(sped, ref = "N"))
cse <- within(cse, EL <- relevel(EL, ref = "N"))
cse <- within(cse, starr_hill <- relevel(starr_hill, ref = "N"))
cse <- cse %>% mutate(starr_hill=ifelse(starr_hill=="Y",1,0)) 
cse <- within(cse, survey_administration <- relevel(survey_administration, ref = "2023"))

##############################################################
# Coarsen Numerical Variables by Factorizing                        
##############################################################

# Create GPA Bins
cse <- cse %>% add_column(gpa_quantiles="")
cse <- cse %>% mutate(gpa_quantiles = ntile(simplegpa_gr6, 4))
cse <- cse %>% mutate(gpa_quantiles= ifelse(is.na(gpa_quantiles), "Missing", gpa_quantiles))
cse$gpa_quantiles <- as.factor(cse$gpa_quantiles)
cse <- within(cse, gpa_quantiles <- relevel(gpa_quantiles, ref = "4"))

# Create Attendance Bins
cse <- cse %>% add_column(chronic_abs="")
cse <- cse %>% mutate(chronic_abs= ifelse(attendance_gr6<=90, "Chronically Absent", chronic_abs))
cse <- cse %>% mutate(chronic_abs= ifelse(attendance_gr6>90, "Not Chronically Absent", chronic_abs))
cse <- cse %>% mutate(chronic_abs= ifelse(is.na(chronic_abs), "Missing", chronic_abs))
cse$chronic_abs <- as.factor(cse$chronic_abs)
cse <- within(cse, chronic_abs <- relevel(chronic_abs, ref = "Not Chronically Absent"))

# Create Reading SOL Bins
cse <- cse %>% add_column(readsol_pf="")
cse <- cse %>% mutate(readsol_pf= ifelse(math_score_gr6<400, "Fail", readsol_pf))
cse <- cse %>% mutate(readsol_pf= ifelse(math_score_gr6>=400, "Pass", readsol_pf))
cse <- cse %>% mutate(readsol_pf= ifelse(is.na(math_score_gr6), "Missing", readsol_pf))
cse$readsol_pf <- factor(cse$readsol_pf, ordered=TRUE, levels= c("Missing", "Fail","Pass"))

# Create Math SOL Bins

cse <- cse %>% add_column(mathsol_pf="")
cse <- cse %>% mutate(mathsol_pf= ifelse(math_score_gr6<400, "Fail", mathsol_pf))
cse <- cse %>% mutate(mathsol_pf= ifelse(math_score_gr6>=400, "Pass", mathsol_pf))
cse <- cse %>% mutate(mathsol_pf= ifelse(is.na(math_score_gr6), "Missing", mathsol_pf))
cse$mathsol_pf <- factor(cse$mathsol_pf, ordered=TRUE, levels= c("Missing", "Fail","Pass"))

cse %>% write.csv("../data/cse_prematch_clean_combinedcollection.csv")

##############################################################
# See Match Balance for SHP and Control for Item Analysis                       
##############################################################

nomatch <- matchit(starr_hill ~
                      SE1+
                      SE2+
                      SE3+
                      SE4+
                      SE5+
                      SE6+
                      SE7+
                      SE8+
                      SE9+
                      SE10+
                      SE11+
                      OE1+
                      OE2+
                      OE3+
                      OE4+
                      OE5+
                      OE6+
                      OE7+
                      OE8+
                      OE9+
                      OE10,
                      data =cse, method = NULL)
                      
summary(nomatch)


##############################################################
# Matching                      
##############################################################

SHP_match <- matchit(starr_hill ~ 
                      gender + 
                      race_ethnicity +
                      avid +
                      sped + 
                      EL +
                      class_year + 
                      gpa_quantiles + 
                      chronic_abs +
                      mathsol_pf + 
                      readsol_pf +
                      econ_dis +
                      survey_administration, 
                      data =cse, method = "cem", estimand = "ATT", distance = "glm")

summary(SHP_match)

chosen_match <- match.data(SHP_match)

chosen_match %>% write_csv("../data/cse_matched_combinedcollection.csv")





 




