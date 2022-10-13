#Calculate the linear predictors for each person
#*************************************************************************/
#install.packages('Hmisc')
install.packages('expss')
library(Hmisc)
library(expss)

# macro removed, terms inputted manually

#/* ------ */
 # /* Female */
  #/* ------ */
female_lp <- keep(CDPORT, CASEID, geodghr4, female, cchs_age_grp, 
                      cchs_age_cts, alc_a, smoking, fruit_veg_a, age_c, 
                      ethnicity_a, education_d, marital_a, asthma_flag, bmi_a, 
                      hbp_flag, health_a, stress_a, sample_wt)
View(female_lp)

# 4 knots @(-22, -8, 8, 32)
rcspline.eval(female_lp$age_c, nk=4, knots=c(-22,-8,8,32), inclx=FALSE, knots.only=TRUE)
female_lp$age_rcsf <- rcspline.eval(female_lp$age_c, nk=4, knots=c(-22,-8,8,32))
female_lp$age_c1 <- female_lp$age_rcsf[1:19814,1]#number after colon represents the number of entries within female_lp dataset
female_lp$age_c2 <- female_lp$age_rcsf[1:19814,2]

#/* Full model */  

female_lp$lp_alcohol <-
  ifelse(female_lp$alc_a=='HeavyDrinker',0.2395,
  ifelse(female_lp$alc_a=='ModerateDrinker',0.1035,
  ifelse(female_lp$alc_a=='NeverDrinker',0.3163, 0))) 
                                            
female_lp$lp_smoking <-
  ifelse(female_lp$smoking=='Always occassional',0.2722,
  ifelse(female_lp$smoking=='Occassional smoker',0.6087,
  ifelse(female_lp$smoking=='Daily',1.050,
  ifelse(female_lp$smoking=='Former occassional',-0.1565,
  ifelse(female_lp$smoking=='Former daily',0.2008, 0)))))
    
female_lp$lp_fvc <-
  ifelse(female_lp$fruit_veg_a==1,-0.0614,
  ifelse(female_lp$fruit_veg_a==2,-0.1168, 0))

female_lp$lp_age = 0.1285*female_lp$age_c #age continuous (spline term 1)
female_lp$lp_age1 = -0.2481*female_lp$age_c1 #age spline term 2 - how do I get c1 and c2 from the spline function
female_lp$lp_age2 = 0.5088*female_lp$age_c2 #age spline term 3
    
female_lp$lp_ethnicity <-
  ifelse(female_lp$ethnicity_a=='Non-White',0.3402, 0)

female_lp$lp_education <-
  ifelse(female_lp$education_d=='SecondaryGraduate',-0.1046, 0)

female_lp$lp_marital <-
  ifelse(female_lp$marital_a == 'Single', 0.0726,
  ifelse(female_lp$marital_a == 'Widowed, Separated or Divorced',0.0849, 0))

female_lp$lp_asthma <-
  ifelse(female_lp$asthma_flag=='Yes',0.3730, 0)

female_lp$lp_bmi <-
  ifelse(female_lp$bmi_a==1,-0.1823,
  ifelse(female_lp$bmi_a==2,0.3696,
  ifelse(female_lp$bmi_a==3,0.6141,
  ifelse(female_lp$bmi_a==4,1.0320,
  ifelse(female_lp$bmi_a==5,1.1634,
  ifelse(female_lp$bmi_a==9,0.4308, 0))))))

female_lp$lp_hbp <-
  ifelse(female_lp$hbp_flag=='Yes',0.3258, 0)

female_lp$lp_health <-
  ifelse(female_lp$health_a=='Poor or Fair',0.1939,
  ifelse(female_lp$health_a=='Excellent or Very Good',-0.1850, 0))

female_lp$lp_stress <-
  ifelse(female_lp$stress_a=='A bit stressful',-0.0008,
  ifelse(female_lp$stress_a=='Not very stressful',-0.1031,
  ifelse(female_lp$stress_a=='Quite a bit or extremely stressful',0.0152, 0)))

female_lp$full_lp = 
 rowSums(female_lp[ , c('lp_alcohol','lp_smoking','lp_fvc','lp_age','lp_age1','lp_age2',
                   'lp_ethnicity','lp_education','lp_marital','lp_asthma','lp_bmi','lp_hbp',
                  'lp_health','lp_stress'), -4.3449])
       
female_lp$full_pred_surv = exp(-1*exp(female_lp$full_lp)*((10)**1.1275))
female_lp$full_pred_risk = 1-female_lp$full_pred_surv
    
female_lp$full_aft_lp = -1*female_lp$full_lp/1.1275
female_lp$full_aft_surv = 1*exp(-1*exp((log(10)-female_lp$full_aft_lp)/0.887))
female_lp$full_aft_risk = 1-female_lp$full_aft_surv

#Troubleshooting discrepancy in R vs SAS
#creates a new data frame with the lp variables
#female_lp_test <- keep(female_lp, CASEID, perl("^lp")) #replicate this code in SAS for comparison
#View(female_lp_test)

female_lp$minus_agelp = 
  rowSums(female_lp[ , c('lp_alcohol','lp_smoking','lp_fvc',
                         'lp_ethnicity','lp_education','lp_marital','lp_asthma','lp_bmi','lp_hbp',
                         'lp_health','lp_stress'), -4.3449])

female_lp_test <-data.frame(CASEID, minus_agelp, full_lp)

View(female_lp_test)

write.csv(female_lp_test,"C://Users/tselot/Desktop/CDPoRT Dev/female_lp_test.csv", row.names = FALSE)


columns_to_remove <- grep("lp_", names(female_lp))
female_lp<-female_lp[,-columns_to_remove]

#/* ------ */
# /* Male */
#/* ------ */
male_lp <- keep(CDPORT, CASEID, geodghr4, female, cchs_age_grp, cchs_age_cts, alc_a, smoking, fruit_veg_a, age_c, ethnicity_a,
                      income_ca_5, asthma_flag, bmi_a, hbp_flag, health_a,
                      stress_a, sample_wt)
View(male_lp)
#/*Full model*/

# 5 knots @(-23, -11, -2, 9, 26)
rcspline.eval(male_lp$age_c, nk = 5, knots=c(-23,-11,-2,9,26), inclx=FALSE, knots.only=TRUE)
male_lp$age_rcsf <- rcspline.eval(male_lp$age_c, nk=5, knots=c(-23,-11,-2,9,26))
male_lp$age_c1 <- male_lp$age_rcsf[1:19814,1]
male_lp$age_c2 <- male_lp$age_rcsf[1:19814,2]
male_lp$age_c3 <- male_lp$age_rcsf[1:19814,3]


male_lp$lp_alcohol <-
  ifelse(male_lp$alc_a=='HeavyDrinker',-0.1188,
  ifelse(male_lp$alc_a=='ModerateDrinker',-0.0091,
  ifelse(male_lp$alc_a=='NeverDrinker',0.1806, 0)))

male_lp$lp_smoking <-
  ifelse(male_lp$smoking=='Always occassional',0.0501,
  ifelse(male_lp$smoking=='Occassional smoker',0.3514,
  ifelse(male_lp$smoking=='Daily',0.8324,
  ifelse(male_lp$smoking=='Former occassional',0.0121,
  ifelse(male_lp$smoking=='Former daily',0.1619, 0)))))

male_lp$lp_fvc <-
  ifelse(male_lp$fruit_veg_a==1,-0.0760,
  ifelse(male_lp$fruit_veg_a==2,-0.1626, 0))

male_lp$lp_age = 0.1900*male_lp$age_c #age continuous (spline term 1)
male_lp$lp_age1 = -0.4187*male_lp$age_c1 #age spline term 2
male_lp$lp_age2 = 0.9329*male_lp$age_c2 #age spline term 3
male_lp$lp_age3 = -0.4848*male_lp$age_c3 #age spline term 4

male_lp$lp_ethnicity <-
  ifelse(male_lp$ethnicity_a == 'Non-White',0.2690, 0) #visible minority

male_lp$lp_income <-
  ifelse(male_lp$income_ca_5 == 'Q1',0.1139, #low income
  ifelse(male_lp$income_ca_5 == 'Unknown',0.1345, 0)) #unknown income

male_lp$lp_asthma <-
  ifelse(male_lp$asthma_flag == 'Yes',0.2749, 0) #asthma

male_lp$lp_bmi <-
  ifelse(male_lp$bmi_a == 1,0.4222, #underweight
  ifelse(male_lp$bmi_a == 2,0.1428, #overweight
  ifelse(male_lp$bmi_a == 3,0.5687, #obese class 1
  ifelse(male_lp$bmi_a == 4,0.9830, #obese class 2
  ifelse(male_lp$bmi_a == 5,1.1879, #obese class 3
  ifelse(male_lp$bmi_a == 9,0.3607, 0)))))) #unknown BMI

male_lp$lp_hbp <-
  ifelse(male_lp$hbp_flag == 'Yes', 0.3599, 0) #HBP

male_lp$lp_health <-
  ifelse(male_lp$health_a == 'Poor or Fair', 0.1138, #poor or fair health
  ifelse(male_lp$health_a == 'Excellent or Very Good',-0.2896, 0)) #very good or excellent health

male_lp$lp_stress <-
  ifelse(male_lp$stress_a == 'A bit stressful',-0.0318,
  ifelse(male_lp$stress_a == 'Not very stressful',-0.0821,
  ifelse(male_lp$stress_a == 'Quite a bit or extremely stressful',-0.1284, 0)))

male_lp$full_lp = 
  rowSums(male_lp[ , c('lp_alcohol','lp_smoking','lp_fvc',
                    'lp_age','lp_age1','lp_age2',
                    'lp_age3','lp_ethnicity','lp_income',
                    'lp_asthma','lp_bmi','lp_hbp',
                    'lp_health','lp_stress'), -3.1830])

male_lp$full_pred_surv = exp(-1*exp(male_lp$full_lp)*((10)**1.1770))
male_lp$full_pred_risk = 1-male_lp$full_pred_surv

male_lp$full_aft_lp = -1*male_lp$full_lp/1.1770
male_lp$full_aft_surv = 1*exp(-1*exp((log(10)-male_lp$full_aft_lp)/0.8496))
male_lp$full_aft_risk = 1-male_lp$full_aft_surv

columns_to_remove <- grep("lp_", names(male_lp))
male_lp<-male_lp[,-columns_to_remove]

#male_lp$full_lp_m <-
#  ifelse(male_lp$female==0, male_lp$full_lp, NA)
#male_lp <-male_lp[male_lp$female!=1]

#/************************
#Run the macro
#*************************

#removed

#/***********************
#Check Female
#************************
#*Check RCS
attach(female_lp)
female_lp %>%
  select(age_c, age_c1, age_c2) %>%
  head(., 10)

#*Check predicted probabilities - FEMALES
female_lp %>%
  select(full_lp, full_pred_surv, full_pred_risk, full_aft_lp, full_aft_surv, full_aft_risk) %>%
  head(., 10)
detach(female_lp)

#/***********************
#Check Male
#************************
#*Check RCS
attach(male_lp)
male_lp %>%
  select(age_c, age_c1, age_c2, age_c3) %>%
  head(., 10)

#*Check predicted probabilities - MALES
male_lp %>%
  select(full_lp, full_pred_surv, full_pred_risk, full_aft_lp, full_aft_surv, full_aft_risk) %>%
  head(., 10)
detach(male_lp)

