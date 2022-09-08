#Prepare Data
#install.packages(tidyverse)
#install.packages("readxl")
#install.packages("dplyr")
#install.packages("matrixStats")
#install.packages('survey')
#install.packages('base')
library(tidyverse)
library(readxl)
library(dplyr)
library(matrixStats)
library(survey)
library(base)

CCHS201516 <- read.csv("C:/Users/tselo/Desktop/HFASt Lab/CDPoRT Coding/CDPoRT Part 1/CCHS201516.csv")
#View(CCHS20131516)

# Creating smaller data frame for CCHS data, contains only variables used in Part 1
attach(CCHS201516)
CCHSdata <- data.frame(CASEID, geodghr4, geo_prv, wts_m, dhhgage, dhh_sex, alc_010, alc_015, alc_020, alwdvwky, smkdvsty, fvcdvfru, fvcdvora, fvcdvjui, fvcdvgrn, fvcdvveg, sdcdgcgt, incdvrpr, incdvrca, ehg2dvr3, dhhgms, ccc_015, ccc_065, gendvhdi, gen_020, ccc_130, ccc_030, ccc_085, ccc_090, ccc_095, hwtdgwtk, hwtdghtm)
detach(CCHS201516)
CDPORT = CCHSdata
CDPORT$sample_wt = CDPORT$wts_m
View(CDPORT)

#*create continuous age grouping
CDPORT$cchs_age_grp <-
  ifelse(CDPORT$dhhgage==1, "12-14",
  ifelse(CDPORT$dhhgage==2,"15-17",
  ifelse(CDPORT$dhhgage==3, "18-19",
  ifelse(CDPORT$dhhgage==4, "20-24",
  ifelse(CDPORT$dhhgage==5, "25-29",
  ifelse(CDPORT$dhhgage==6, "30-34",
  ifelse(CDPORT$dhhgage==7, "35-39",
  ifelse(CDPORT$dhhgage==8, "40-44",
  ifelse(CDPORT$dhhgage==9, "45-49",
  ifelse(CDPORT$dhhgage==10, "50-54",
  ifelse(CDPORT$dhhgage==11, "55-59",
  ifelse(CDPORT$dhhgage==12, "60-64",
  ifelse(CDPORT$dhhgage==13, "65-69",
  ifelse(CDPORT$dhhgage==14, "70-74",
  ifelse(CDPORT$dhhgage==15, "75-79",
  ifelse(CDPORT$dhhgage==16, "80-100", 'Unknown'))))))))))))))))

# Notes:
#    - this is generated from the cchs_age_grp with a discrete uniform distribution for each group
#    - need to add plus one to the final value (b) of UNIFORM distribution because UNIFORM will only do [a,b);

#inserted set seed function so that the random ages generated will be consistent
#not sure if the seed will be consistent be consistent between R and SAS
set.seed(1379) #do I have to set seed before each runif function? I guess test by looking at the numbers generated in the CDPORT table?
#also test that the seed gives the same numbers in SAS

CDPORT$cchs_age_cts <-
  ifelse(CDPORT$cchs_age_grp=='12-14',floor(runif(1,min=12,max=14+1)), 
  ifelse(CDPORT$cchs_age_grp=='15-17',floor(runif(1,min=15,max=17+1)),
  ifelse(CDPORT$cchs_age_grp=='18-19',floor(runif(1,min=18,max=19+1)),
  ifelse(CDPORT$cchs_age_grp=='20-24',floor(runif(1,min=20,max=24+1)),
  ifelse(CDPORT$cchs_age_grp=='20-24',floor(runif(1,min=20,max=24+1)),
  ifelse(CDPORT$cchs_age_grp=='25-29',floor(runif(1,min=25,max=29+1)),
  ifelse(CDPORT$cchs_age_grp=='30-34',floor(runif(1,min=30,max=34+1)),
  ifelse(CDPORT$cchs_age_grp=='35-39',floor(runif(1,min=35,max=39+1)),
  ifelse(CDPORT$cchs_age_grp=='40-44',floor(runif(1,min=40,max=44+1)),
  ifelse(CDPORT$cchs_age_grp=='45-49',floor(runif(1,min=45,max=49+1)),
  ifelse(CDPORT$cchs_age_grp=='50-54',floor(runif(1,min=50,max=54+1)),
  ifelse(CDPORT$cchs_age_grp=='55-59',floor(runif(1,min=55,max=59+1)),
  ifelse(CDPORT$cchs_age_grp=='60-64',floor(runif(1,min=60,max=64+1)),
  ifelse(CDPORT$cchs_age_grp=='65-69',floor(runif(1,min=65,max=69+1)),
  ifelse(CDPORT$cchs_age_grp=='70-74',floor(runif(1,min=70,max=74+1)),
  ifelse(CDPORT$cchs_age_grp=='75-79',floor(runif(1,min=75,max=79+1)),
  ifelse(CDPORT$cchs_age_grp=='80-100',floor(runif(1,min=80,max=100+1)), NA))))))))))))))))) 

#*Age centred at 45
CDPORT$age_c <- CDPORT$cchs_age_cts - 45
  #** Age groups
CDPORT$age_cat <-
  ifelse(  CDPORT$cchs_age_cts < 20,'Lt 20 yrs',
  ifelse(20<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 34,'20 to 34 yrs',
  ifelse(35<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 44,'35 to 40 yrs',
  ifelse(45<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 54,'45 to 54 yrs',
  ifelse(55<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 64,'55 to 64 yrs',
  ifelse(65<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 74,'65 to 74 yrs',
  ifelse(75<= CDPORT$cchs_age_cts & CDPORT$cchs_age_cts <= 84,'75 to 84 yrs',
  ifelse(CDPORT$cchs_age_cts >= 85,'85+ yrs','Unknown'))))))))
  

#*Sex
CDPORT$female <- 
  ifelse(CDPORT$dhh_sex==1,0,
  ifelse(CDPORT$dhh_sex==2,1, NA))
CDPORT$male <- 
  ifelse(CDPORT$dhh_sex==1,1,
  ifelse(CDPORT$dhh_sex==2,0, NA))

#*ALCOHOL CONSUMPTION
#original code (used do function from SAS)
#CDPORT$alc_a <-
#  ifelse(CDPORT$alc_010==2 | CDPORT$alc_015 %in% c(1, 2, 3),'Never Drinker',
#         ifelse(CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7),do(
#           if((CDPORT$dhh_sex==1 & 0<=CDPORT$alwdvwky & CDPORT$alwdvwky<= 3)|(CDPORT$dhh_sex==2 & 0<=CDPORT$alwdvwky & CDPORT$alwdvwky<= 2)){
#             'LightDrinker'
#           } else if((CDPORT$dhh_sex==1 & 4<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 21)|(CDPORT$dhh_sex==2 & 3<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 14)){
#             'ModerateDrinker'
#           } else if((CDPORT$dhh_sex==1 & 21<CDPORT$alwdvwky & CDPORT$alwdvwky<996)|(CDPORT$dhh_sex==2 & 14<CDPORT$alwdvwky & CDPORT$alwdvwky<996)|(5<=CDPORT$alc_020 & CDPORT$alc_020<=6)){
#             'HeavyDrinker'
#           })), 'Unknown')

#only alc_a that works is below, 'Never Drinker' and total is accurate, need to fix other categories 
#thinking there is an issue with the brackets that are used to separate the different parts of the conditions

############### EP EDITS ################################################
CDPORT$alc_a <-
  ifelse(CDPORT$alc_010==2 | CDPORT$alc_015 %in% c(1, 2, 3),'Never Drinker',
               ifelse((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) &
                     ((CDPORT$dhh_sex==1 & 21<CDPORT$alwdvwky & CDPORT$alwdvwky <996)|
                     (CDPORT$dhh_sex==2 & 14<CDPORT$alwdvwky & CDPORT$alwdvwky <996)|
                     (5<=CDPORT$alc_020 & CDPORT$alc_020 <=6)), 'HeavyDrinker',
               ifelse((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) &
                      ((CDPORT$dhh_sex==1 & 4<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 21)|
                      (CDPORT$dhh_sex==2 & 3<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 14)), 'ModerateDrinker',
                ifelse((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) &
                      ((CDPORT$dhh_sex==1 & 0<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 3)|
                      (CDPORT$dhh_sex==2 & 0<= CDPORT$alwdvwky & CDPORT$alwdvwky <= 2)), 'LightDrinker',
                                     'Unknown'))))
#####################################################################################


#ifelse((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) &
#        ((CDPORT$dhh_sex==1 & 0<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 3)|
#          (CDPORT$dhh_sex==2 & 0<= CDPORT$alwdvwky & CDPORT$alwdvwky <= 2)), 'LightDrinker',
#           ifelse((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) &
#                   ((CDPORT$dhh_sex==1 & 4<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 21)|
#                     (CDPORT$dhh_sex==2 & 3<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 14)), 'ModerateDrinker',

#testing a function
#alc_afx <-function(row){
#  alc_010 <-row[['alc_010']]
#  alc_015 <-row[['alc_015']]
#  alc_020 <-row[['alc_020']]
#  dhh_sex <-row[['dhh_sex']]
#  alwdvwky <-row[['alwdvwky']]
  
#  if(CDPORT$alc_010==2 | CDPORT$alc_015 %in% c(1, 2, 3)){
#    alc_a <-'Never Drinker'
#  }
#  else if((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) & ((CDPORT$dhh_sex==1 & 0<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 3)|(CDPORT$dhh_sex==2 & 0<= CDPORT$alwdvwky & CDPORT$alwdvwky <= 2))){
#    alc_a <-'LightDrinker'
#  }
#  else if((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) & ((CDPORT$dhh_sex==1 & 4<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 21)|(CDPORT$dhh_sex==2 & 3<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 14))){
#    alc_a <-'ModerateDrinker'
#  }
#  else if((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) & (CDPORT$dhh_sex==1 & 21<CDPORT$alwdvwky & CDPORT$alwdvwky <996)|(CDPORT$dhh_sex==2 & 14<CDPORT$alwdvwky & CDPORT$alwdvwky <996)|(5<=CDPORT$alc_020 & CDPORT$alc_020 <=6)){
#    alc_a <-'HeavyDrinker'
#    }
#  return(alc_a)
#}
#CDPORT$alc_a <-apply(CDPORT, 1, alc_afx)

#testing another modification
#CDPORT$alc_a <-'Unknown'
#  if(CDPORT$alc_010==2 | CDPORT$alc_015 %in% c(1, 2, 3)){'Never Drinker'
#    }else if((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) & ((CDPORT$dhh_sex==1 & 0<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 3)|(CDPORT$dhh_sex==2 & 0<= CDPORT$alwdvwky & CDPORT$alwdvwky <= 2))){'LightDrinker'
#      }else if((CDPORT$alc_010==1 & CDPORT$alc_015 %in% c(4, 5, 6, 7)) & ((CDPORT$dhh_sex==1 & 4<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 21)|(CDPORT$dhh_sex==2 & 3<= CDPORT$alwdvwky & CDPORT$alwdvwky<= 14))){'ModerateDrinker'
#        }else if((CDPORT$dhh_sex==1 & 21<CDPORT$alwdvwky & CDPORT$alwdvwky <996)|(CDPORT$dhh_sex==2 & 14<CDPORT$alwdvwky & CDPORT$alwdvwky <996)|(5<=CDPORT$alc_020 & CDPORT$alc_020 <=6)){'HeavyDrinker'}

#*SMOKING
CDPORT$smoking <- 
  ifelse(CDPORT$smkdvsty==1,'Daily',
  ifelse(CDPORT$smkdvsty==2,'Occasional smoker',
  ifelse(CDPORT$smkdvsty==3, 'Always occassional',
  ifelse(CDPORT$smkdvsty==4,'Former daily',
  ifelse(CDPORT$smkdvsty==5,'Former occassional',
  ifelse(CDPORT$smkdvsty==6,'Never','Unknown'))))))

#*DAILY FRUIT AND VEG CONSUMPTION
#**Fruit: based on the CCHS derived variable FVCDFRU
#**Carrot: based on the CCHS derived variable FVCDCAR
#**Potato: based on the CCHS derived variable FVCDPOT
#**Juice: based on the CCHS derived variable FVCDJUI
#**Salad: based on the CCHS derived variable FVCDSAL
#**Other_Veg: based on the CCHS derived variable FVCDVEG 
#**If the values for fruit, carrot, potato, juice, salad and other_veg are missing, just ignore
CDPORT$fruit = ifelse(CDPORT$fvcdvfru==999.9,NA, CDPORT$fvcdvfru) 
CDPORT$orange = CDPORT$fvcdvora
CDPORT$orange = ifelse(CDPORT$fvcdvora==999.9,NA, CDPORT$fvcdvora)
CDPORT$juice = CDPORT$fvcdvjui
CDPORT$juice = ifelse(CDPORT$fvcdvjui==999.9,NA, CDPORT$fvcdvjui)
CDPORT$juice2 <- 
  ifelse(is.na(CDPORT$juice), NA,
  ifelse(CDPORT$fvcdvjui<1,CDPORT$juice,1))#new variable created to be able to add juice to fruit_veg
CDPORT$green = CDPORT$fvcdvgrn
CDPORT$green = ifelse(CDPORT$fvcdvgrn==999.9,NA, CDPORT$fvcdvgrn)
CDPORT$other_veg = CDPORT$fvcdvveg 
CDPORT$other_veg = ifelse(CDPORT$fvcdvveg==999.9,NA, CDPORT$fvcdvveg)
CDPORT$juice <-NULL
CDPORT$fruit_veg = 
  ifelse((is.na(CDPORT$fruit) | is.na(CDPORT$orange) | is.na(CDPORT$juice2) |
           is.na(CDPORT$green) | is.na(CDPORT$other_veg)), NA,
         rowSums(CDPORT[ , c('fruit', 'orange', 'juice2', 'green', 'other_veg')]))

#**If missing value for Fruit_Veg, then delete.
#all feeder variables above have accurate frequencies (when compared to SAS) but fruit_veg_a categories show different frequencies?
CDPORT$fruit_veg_a <- 
  ifelse(0<=CDPORT$fruit_veg & CDPORT$fruit_veg<3,0,
  ifelse(3<=CDPORT$fruit_veg & CDPORT$fruit_veg<6,1,
  ifelse(CDPORT$fruit_veg>=6,2,
         NA)))

#*VISIBLE MINORITY
CDPORT$ethnicity_a <-
  ifelse(CDPORT$sdcdgcgt==1,'White',
  ifelse(CDPORT$sdcdgcgt==2,'Non-White', 'Unknown'))

#*HOUSEHOLD INCOME (provincial-level)
CDPORT$income_pr_5 <- 
  ifelse(CDPORT$incdvrpr %in% c(1, 2),'Q1',
  ifelse(CDPORT$incdvrpr %in% c(3, 4),'Q2',
  ifelse(CDPORT$incdvrpr %in% c(5, 6),'Q3',
  ifelse(CDPORT$incdvrpr %in% c(7, 8),'Q4',
  ifelse(CDPORT$incdvrpr %in% c(9, 10),'Q5', 'Unknown')))))
CDPORT$income_ca_5 <-
  ifelse(CDPORT$incdvrca %in% c(1, 2),'Q1',
  ifelse(CDPORT$incdvrca %in% c(3, 4),'Q2',
  ifelse(CDPORT$incdvrca %in% c(5, 6),'Q3',
  ifelse(CDPORT$incdvrca %in% c(7, 8),'Q4',
  ifelse(CDPORT$incdvrca %in% c(9, 10),'Q5', 'Unknown')))))

#*EDUCATION
CDPORT$education_cat <- 
  ifelse(CDPORT$ehg2dvr3==1,'LessThanSecondary',
  ifelse(CDPORT$ehg2dvr3==2,'SecondaryGraduate',
  ifelse(CDPORT$ehg2dvr3==3,'MoreThanSecondary', 'Unknown')))

    #**Education - 5 cat
CDPORT$education_d <- 
  ifelse(CDPORT$ehg2dvr3==1,'LessThanSecondary',
  ifelse(CDPORT$ehg2dvr3 %in% c(2, 3),'SecondaryGraduate', 'Unknown'))

#*Marital Status
CDPORT$marital_a <- 
  ifelse(CDPORT$dhhgms %in% c(1, 2),'Married or CommonLaw',
  ifelse(CDPORT$dhhgms %in% c(3),'Widowed, Separated, or Divorced',
  ifelse(CDPORT$dhhgms %in% c(4),'Single', 'Unknown')))

#*Asthma
CDPORT$asthma_flag <- 
  ifelse(CDPORT$ccc_015==1,'Yes',
  ifelse(CDPORT$ccc_015==2,'No', 'Unknown'))

#*Corrected BMI
######################## EP EDITS ########################################################
#(moved NA from the end of the argument to the middle and changed < to >=)
CDPORT$bmi <- 
  ifelse(CDPORT$hwtdgwtk>=999.96 & CDPORT$hwtdghtm>=9.996, NA, (CDPORT$hwtdgwtk / (CDPORT$hwtdghtm^2)))
###########################################################################################
CDPORT$bmi_corr_cat= NA
CDPORT$bmi_corr <-
  ifelse(CDPORT$female==0,(-0.29227 + 1.03239*CDPORT$bmi),
  ifelse(CDPORT$female==1,(0.10927 + 1.02584*CDPORT$bmi), CDPORT$bmi))

    #**BMI categories
#bmi categories 1, 5, and 9 have discrepancies in their frequencies when compared to SAS but the rest are accurate
CDPORT$bmi_a <-
  ifelse(is.na(CDPORT$bmi_corr), 9,
  ifelse(0<=CDPORT$bmi_corr & CDPORT$bmi_corr<18.5,1, 
  ifelse(18.5<=CDPORT$bmi_corr & CDPORT$bmi_corr<25,0,
  ifelse(25 <=CDPORT$bmi_corr & CDPORT$bmi_corr<30,2,
  ifelse(30 <=CDPORT$bmi_corr & CDPORT$bmi_corr<35,3,
  ifelse(35 <=CDPORT$bmi_corr & CDPORT$bmi_corr<40,4,
  ifelse(40 <=CDPORT$bmi_corr & CDPORT$bmi_corr<999,5,9)))))))
    
#*High blood pressure
CDPORT$hbp_flag <-
  ifelse(CDPORT$ccc_065==1,'Yes',
  ifelse(CDPORT$ccc_065==2,'No', 'Unknown'))

#*Self-Rated general health
CDPORT$health_a <-
  ifelse(CDPORT$gendvhdi %in% c(0,1),'Poor or Fair', 
  ifelse(CDPORT$gendvhdi==2,'Good',
  ifelse(CDPORT$gendvhdi %in% c(3,4),'Excellent or Very Good', 'Unknown')))

#*Life stress
CDPORT$stress_a <-
  ifelse(CDPORT$gen_020 %in% c(1),'Not at all stressful',
  ifelse(CDPORT$gen_020 %in% c(2),'Not very stressful',
  ifelse(CDPORT$gen_020 %in% c(3),'A bit stressful',
  ifelse(CDPORT$gen_020 %in% c(4,5),'Quite a bit or extremely stressful', 'Unknown'))))

#*Cancer
CDPORT$cancer <- 
  ifelse(CDPORT$ccc_130==1,'Yes',
  ifelse(CDPORT$ccc_130==2,'No', 'Unknown'))

#*COPD
CDPORT$copd <-
  ifelse(CDPORT$ccc_030==1,'Yes',
  ifelse(CDPORT$ccc_030==2,'No', 'Unknown'))

#*Heart Disease
CDPORT$hrd <-
  ifelse(CDPORT$ccc_085==1,'Yes',
  ifelse(CDPORT$ccc_085==2,'No', 'Unknown'))
      
#*Stroke
CDPORT$stroke <-
  ifelse(CDPORT$ccc_090==1,'Yes',
  ifelse(CDPORT$ccc_090==2,'No', 'Unknown'))
      
#*Diabetes
CDPORT$diab <-
  ifelse(CDPORT$ccc_095==1,'Yes',
  ifelse(CDPORT$ccc_095==2,'No', 'Unknown'))

#*Flag for missing on any predictor variable (except BMI)
CDPORT$missing_female <-
  ifelse(CDPORT$alc_a=='Unknown'|CDPORT$smoking=='Unknown'|
           CDPORT$fruit_veg_a=='Unknown'| CDPORT$ethnicity_a == "Unknown" |
           CDPORT$education_d == "Unknown"| CDPORT$marital_a == "Unknown" | 
           CDPORT$asthma_flag == "Unknown" | CDPORT$hbp_flag == "Unknown" | 
           CDPORT$health_a == "Unknown" | CDPORT$stress_a == "Unknown", 1, 0)
CDPORT$missing_male <-
  ifelse(CDPORT$alc_a == "Unknown" | CDPORT$smoking  == "Unknown" |CDPORT$fruit_veg_a=='Unknown'|
           CDPORT$ethnicity_a  == "Unknown" | CDPORT$income_ca_5 == "Unknown" |
           CDPORT$asthma_flag == "Unknown" | CDPORT$hbp_flag == "Unknown" |
           CDPORT$health_a == "Unknown" | CDPORT$stress_a == "Unknown", 1, 0)

CDPORT$missing_f <-
  ifelse(CDPORT$female == 1 & CDPORT$missing_female == 1, 1, 0)
CDPORT$missing_m <-
  ifelse(CDPORT$female == 0 & CDPORT$missing_male == 1, 1, 0)

#check that all variables are in the CDPORT data frame (from the CCHS dataset)
#names(CDPORT)

#restricting sample to province of Ontario
CDPORT <- filter(CDPORT, CDPORT$geo_prv == 35) 

#Apply Exclusions
ind <- with(CDPORT, (CDPORT$cchs_age_cts<20) | (CDPORT$cancer=='Yes')|
                                           (CDPORT$copd=='Yes')|
                                           (CDPORT$hrd=='Yes')|
                                           (CDPORT$stroke=='Yes')|
                                           (CDPORT$diab=='Yes')|
                                           (CDPORT$missing_f==1)|
                                           (CDPORT$missing_m==1))
CDPORT <- CDPORT[!ind, ]
#*Checking variable frequencies
summary(CDPORT)
table(CDPORT$alc_a)
table(CDPORT$age_cat)
table(CDPORT$female)
table(CDPORT$smoking)
table(CDPORT$fruit_veg)
table(CDPORT$fruit_veg_a)
table(CDPORT$fruit)
table(CDPORT$orange)
table(CDPORT$juice2)
table(CDPORT$green)
table(CDPORT$other_veg)
table(CDPORT$ethnicity_a)
table(CDPORT$income_pr_5)
table(CDPORT$income_ca_5)
table(CDPORT$education_cat)
table(CDPORT$education_d)
table(CDPORT$marital_a)
table(CDPORT$asthma_flag)
table(CDPORT$bmi_a)
table(CDPORT$hbp_flag)
table(CDPORT$health_a)
table(CDPORT$stress_a)
table(CDPORT$male)
CDPORT$bmi_corrs = summary(CDPORT$bmi_corr)
class(CDPORT$bmi_corr)

#*proc univariate portion; checks distribution of continuous age with weights
#svydesign(ids=CDPORT$CASEID, variables=CDPORT$cchs_age_cts, weights=CDPORT$sample_wt)
weighted.mean(CDPORT$cchs_age_cts, CDPORT$sample_wt)