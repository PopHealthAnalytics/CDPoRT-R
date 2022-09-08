#install.packages('data.table')
#install.packages('questionr')
#install.packages('rcompanion')
#install.packages('purrr')
#install.packages('xlsx')
library(data.table)
library(plyr)
library(questionr)
library(rcompanion)
library(purrr)
library(xlsx)

#MERGE CDPoRT linear predictor variables into the CCHS dataset

#CDPORT_lp combines female_lp and male_lp datasets
CDPORT_lp <- female_lp
CDPORT_lp<-arrange(CDPORT_lp, CASEID)
View(CDPORT_lp)
attach(male_lp)
CDPORT_lp$age_rcsf <-
  ifelse(male_lp$female==0, male_lp$age_rcsf, CDPORT_lp$age_rcsf)

CDPORT_lp$age_c1 <-
  ifelse(male_lp$female==0, male_lp$age_c1,CDPORT_lp$age_c1)

CDPORT_lp$age_c2 <-
  ifelse(male_lp$female==0, male_lp$age_c2,CDPORT_lp$age_c2)

CDPORT_lp$age_c3 <-
  ifelse(male_lp$female==0, male_lp$age_c3, NA)

CDPORT_lp$full_lp <-
  ifelse(male_lp$female==0, male_lp$full_lp,CDPORT_lp$full_lp)

CDPORT_lp$full_pred_surv <-
  ifelse(male_lp$female==0, male_lp$full_pred_surv,CDPORT_lp$full_pred_surv)

CDPORT_lp$full_pred_risk <-
  ifelse(male_lp$female==0, male_lp$full_pred_risk,CDPORT_lp$full_pred_risk)

CDPORT_lp$full_aft_lp <-
  ifelse(male_lp$female==0, male_lp$full_aft_lp,CDPORT_lp$full_aft_lp)

CDPORT_lp$full_aft_surv <-
  ifelse(male_lp$female==0, male_lp$full_aft_surv,CDPORT_lp$full_aft_surv)

CDPORT_lp$full_aft_risk <-
  ifelse(male_lp$female==0, male_lp$full_aft_risk,CDPORT_lp$full_aft_risk)


#keep linear predictor variables from CDPORT_lp and add to CDPORT dataset
#create new variables in CDPORT that are equivalent to CDPORT_lp variables
attach(CDPORT)
CDPORT$age_rcsf <- CDPORT_lp$age_rcsf
CDPORT$age_c1 <- CDPORT_lp$age_c1
CDPORT$age_c2 <- CDPORT_lp$age_c2
CDPORT$age_c3 <- CDPORT_lp$age_c3
CDPORT$full_lp <- CDPORT_lp$full_lp
CDPORT$full_pred_surv <- CDPORT_lp$full_pred_surv
CDPORT$full_pred_risk <- CDPORT_lp$full_pred_risk
CDPORT$full_aft_lp <- CDPORT_lp$full_aft_lp
CDPORT$full_aft_surv <- CDPORT_lp$full_aft_surv
CDPORT$full_aft_risk <- CDPORT_lp$full_aft_risk

#Create a variable for the number of cases
CDPORT$ncd_full = CDPORT$full_pred_risk*CDPORT$sample_wt

###############################################################################
#*Troubleshooting CDPORT_lp discrepancies in R vs. SAS
attach(CDPORT)
CDPORT_lptest <-data.frame(CASEID, female, cchs_age_grp, cchs_age_cts, age_c, age_c1, age_c2, age_c3, full_lp, ncd_full)
write.csv(CDPORT_lptest,"C:/Users/tselo/Desktop/HFASt Lab/CDPoRT Coding/CDPoRT Part 3/Export-CDPORT_lp-R.csv", row.names = FALSE)
###############################################################################

#ESTIMATE CDPORT RISK
#*using calculated linear predictor from the full model

#distribution of full_pred_risk
ggplot(CDPORT, aes(x=full_pred_risk)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

#distribution of ncd_full
ggplot(CDPORT, aes(x=ncd_full)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666")

###############################################################################
#*Overall, Ontario

#average 10-year predicted risk of chronic disease for Ontario
risk_geoprv <- 
  CDPORT %>%
  group_by(geo_prv) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))#ci=groupwiseMean(full_pred_risk ~ geo_prv, data = CDPORT)) OR'Confidence Interval' = x+/-tn-1, 1-α/2*(s/√n)) OR  CI=(CI(CDPORT$full_pred_risk, ci=0.95))?
risk_geoprv
#mean full_pred_risk is always 1 for each category, trying to figure out how to get more decimals -is this needed?
#cat(formatC(full_pred_risk, format='f', digits=10)) 

#sum of new chronic disease cases for Ontario
ncd_geoprv <- 
  CDPORT %>%
  group_by(geo_prv) %>% 
  summarise('ncd_full' = sum(ncd_full))
ncd_geoprv

################################################################################
#By Ontario health region

#create new variable for Ontario health regions using geodghr4
risk_region <- 
  CDPORT %>%
  group_by(geodghr4) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))#ci=groupwiseMean(full_pred_risk ~ geo_prv, data = CDPORT)) OR'Confidence Interval' = x+/-tn-1, 1-α/2*(s/√n)) OR  CI=(CI(CDPORT$full_pred_risk, ci=0.95))?
risk_region

#sum of new chronic disease cases for Ontario
ncd_region <- 
  CDPORT %>%
  group_by(geodghr4) %>% 
  summarise('ncd_full' = sum(ncd_full))
ncd_region


################################################################################

#*By Age

#average 10-year predicted risk of chronic disease by age group
risk_agecat <- 
  CDPORT %>%
  group_by(age_cat) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))
risk_agecat


#sum of new chronic disease cases for each age category
ncd_agecat <- 
  CDPORT %>%
  group_by(age_cat) %>% 
  dplyr::summarise('ncd_full' = sum(ncd_full))
ncd_agecat                                 

################################################################################

#*By Sex

#average 10-year predicted risk of chronic disease by sex
risk_sex <- 
  CDPORT %>%
  group_by(female) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))
risk_sex


#sum of new chronic disease cases for each sex
ncd_sex <- 
  CDPORT %>%
  group_by(female) %>% 
  dplyr::summarise('ncd_full' = sum(ncd_full))
ncd_sex                                 

################################################################################

#By Household Income

#average 10-year predicted risk of chronic disease by income
risk_inc <- 
  CDPORT %>%
  group_by(income_ca_5) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))
risk_inc


#sum of new chronic disease cases by income
ncd_inc <- 
  CDPORT %>%
  group_by(income_ca_5) %>% 
  dplyr::summarise('ncd_full' = sum(ncd_full))
ncd_inc                                 

################################################################################

#By BMI

#average 10-year predicted risk of chronic disease for each bmi category
risk_bmi <- 
  CDPORT %>%
  group_by(bmi_a) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))
risk_bmi


#sum of new chronic disease cases for each bmi category
ncd_bmi <- 
  CDPORT %>%
  group_by(bmi_a) %>% 
  dplyr::summarise('ncd_full' = sum(ncd_full))
ncd_bmi                                 


################################################################################

#By High Blood Pressure

#average 10-year predicted risk of chronic disease based on blood pressure
risk_hbp <- 
  CDPORT %>%
  group_by(hbp_flag) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))
risk_hbp


#sum of new chronic disease cases based on blood pressure
ncd_hbp <- 
  CDPORT %>%
  group_by(hbp_flag) %>% 
  dplyr::summarise('ncd_full' = sum(ncd_full))
ncd_hbp                                 

################################################################################

#By Smoking

#average 10-year predicted risk of chronic disease based on smoking
risk_smk <- 
  CDPORT %>%
  group_by(smoking) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))
risk_smk


#sum of new chronic disease cases based on smoking
ncd_smk <- 
  CDPORT %>%
  group_by(smoking) %>% 
  dplyr::summarise('ncd_full' = sum(ncd_full))
ncd_smk                                 


################################################################################

#By Alcohol Consumption

#average 10-year predicted risk of chronic disease based on alcohol consumption
risk_alc <- 
  CDPORT %>%
  group_by(alc_a) %>% 
  dplyr::summarise(n = n(), mean = mean(CDPORT$full_pred_risk), 'std error of mean' = sd(full_pred_risk)/sqrt(n)) %>%
  mutate(percent=n/sum(n))
risk_alc


#sum of new chronic disease cases based on alcohol consumption
ncd_alc <- 
  CDPORT %>%
  group_by(alc_a) %>% 
  dplyr::summarise('ncd_full' = sum(ncd_full))
ncd_alc                                 


################################################################################
#CSV Export
attach(CDPORT_lp)
#export_overall <-CDPORT_lp[, c("geodghr4", "risk_region", "ncd_region")]
#write.csv(overall,"C:/Users/tselo/Desktop/HFASt Lab/CDPoRT Coding/CDPoRT Part 3/Export-overall.csv", row.names = FALSE)
export_overall <-data.frame(geodghr4, risk_region, ncd_region)
write.csv(export_overall,"C:/Users/tselo/Desktop/HFASt Lab/CDPoRT Coding/CDPoRT Part 3/Export-overall.csv", row.names = FALSE)

export_age <-data.frame(age_cat, risk_agecat, ncd_agecat)
write.csv(export_age,"C:/Users/tselo/Desktop/HFASt Lab/CDPoRT Coding/CDPoRT Part 3/Export-age.csv", row.names = FALSE)

export_sex <-data.frame(female, risk_sex, ncd_sex)
write.csv(export_sex,"C:/Users/tselo/Desktop/HFASt Lab/CDPoRT Coding/CDPoRT Part 3/Export-sex.csv", row.names = FALSE)

################################################################################
#Troubleshooting
write.xlsx(CDPORT_lp, "C:/Users/tselo/Desktop/HFASt Lab/CDPoRT Coding/CDPoRT Part 3/CDPORT_lpR.xlsx")