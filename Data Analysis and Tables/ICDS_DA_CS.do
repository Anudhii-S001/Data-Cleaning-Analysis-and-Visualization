
***** Stata Version 18.0

***** Title: Regression Analysis 

***** Date Created: Jan 20, 2024
***** Date Updated: Feb 20, 2024

***** This Script has the following sections: 
** Section 1: Regression Analysis before Statistical Matching
*          1.1 Average Treatment Effects
*          1.2 Heterogeneous Treatment Effects
            
***** Brief Project Description: 
** The project aims to examine the impact of an early childhood development program - 'Integrated Child Development Services' -
** in India on pre-adolescent learning outcomes using data from the Indian Human Development Survey. The focus is particularly on
** the heterogeneity in treatment effects to target the intervention to subgroups that benefit the most from it. 

***** Brief description of ICDS: 
** In 1975, the Government of India initiated the Integrated Child Development Services (ICDS) scheme. 
** It offers nutritional support, health assessments, referral services, immunizations, and non-formal preschool education to children aged 0 to 6 years. 

***** Brief description of the Indian Human Development Survey: 
** IHDS is a nation-wide survey encompassing both rural and urban regions, comprising over 1,500 villages, 970 urban blocks and
** over 41,000 households. The survey includes a diverse range of questions on topics such as healthcare, education, employment,
** marriage, and gender relations. IHDS encompasses two waves: the first wave of survey was held in 2004–05 and the second wave
** was conducted in 2011–12. In the second wave, about 85 per cent of the households from the first wave were re-interviewed. 

***** Data used for analysis: 
** For the purpose of this study, three datasets from both the waves of IHDS were combined using a common variable `household ID'
** to obtain a panel data set consisting of data from both the waves. The sample is restricted to include only those children
** whose households were part of the first wave of survey and then re-interviewed in the second wave. The information about
** whether a child received any ICDS intervention is obtained from the eligible woman in the household in the first wave and is
** matched with the learning outcomes of children in the second wave. The final dataset is a panel, comprising relevant variables
** from both the waves of the survey. However, the study is cross-sectional since the learning outcomes of children are only
** recorded in the second wave.

***** Brief variable description: 
** Outcomes: Standardized Reading and Math Scores of children
** Treatment: Whether the child received any ICDS intervention

***** In all regressions, standard errors are clustered at the household level and State fixed effects are included. 

***** Please note: This file not reproducible because data cannot be made publicly available.The analysis for this project is still underway.  

 
clear


********************************************************************************
*******  SECTION 1: REGRESSION ANALYSIS BEFORE STATISTICAL MATCHING      *******
********************************************************************************	

************************* 1.1 AVERAGE TREATMENT EFFECTS ************************

*** Importing survey data 
use "/Users/Desktop/DATA ANALYSIS AND TABLES/survey_data_cleaned.dta"

ssc install estout
eststo clear

*** Table 1: Average Effect of ICDS on Pre-adolescent Learning Outcomes 

* Reading Score 
eststo: reg std_reading_score i.icds_treatment i.child_sex child_age_wave2 i.child_family_type ///
mother_age mother_educ i.mother_health mother_surviving_children father_educ i.school_type ///
i.teacher_good i.teacher_regular i.hh_caste i.hh_urban hh_meals_perday i.hh_toilet_avail ///
i.hh_electricity_avail  i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend ///
i.midday_meal wtew i.stateid, vce(cluster hhid)

* Math Score
eststo: reg std_math_score i.icds_treatment i.child_sex child_age_wave2 i.child_family_type ///
mother_age mother_educ i.mother_health mother_surviving_children father_educ i.school_type ///
i.teacher_good i.teacher_regular i.hh_caste i.hh_urban hh_meals_perday i.hh_toilet_avail ///
i.hh_electricity_avail  i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend ///
i.midday_meal wtew i.stateid, vce(cluster hhid)

esttab using table1.rtf, keep(1.icds_treatment) compress label r2 ar2 se ///
mtitles("Reading Score" "Math Score") ///
title(Table 1: Average Effect of ICDS on Pre-adolescent Learning Outcomes) ///
addnotes(Note: Outcome variables in the study are standardized reading and standardized math scores. The treatment variable is receipt of any ICDS intervention. The following covariates are controlled for: child's age, child's sex, child's family type, mother's education, mother's age, mother's general health condition, the number of surviving children the mother has, father's education, child's school type, child's teacher is regular,child's teacher is good, child receives mid-day meals, household receives water, household owns a toilet, household receives electricity, number of meals per day in household, household's residence, household is below poverty line, log of household's annual consumption expenditure, household's religion, and household's caste. State fixed effects are included in all regressions. Robust standard errors in parentheses are adjusted to account for household-level correlation) starlevels(* 0.10 ** 0.05 *** 0.01) replace

eststo clear

********************** 1.2 HETEROGENEOUS TREATMENT EFFECTS *********************


*** Table 2: Impact of ICDS on Pre-adolescent Learning Outcomes - Heterogeneous Treatment Effects by Child-specific characteristics

** READING SCORE 

* By child's sex: Female = 1 Male = 0
sort child_sex
by child_sex: eststo: quietly reg std_reading_score icds_treatment child_age_wave2 ///
i.child_family_type mother_age mother_educ i.mother_health mother_surviving_children ///
father_educ i.school_type i.teacher_good i.teacher_regular i.hh_caste i.hh_urban ///
hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail  i.hh_water_avail i.hh_poor ///
i.hh_religion log_hh_expend i.midday_meal wtew i.stateid, vce(cluster hhid)

* By child's school type: public, private or other
sort school_type
by school_type: eststo: quietly reg std_reading_score icds_treatment i.child_sex child_age_wave2 ///
i.child_family_type mother_age mother_educ i.mother_health ///
mother_surviving_children father_educ i.teacher_good i.teacher_regular ///
i.hh_caste i.hh_urban hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail ///
i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend i.midday_meal wtew ///
i.stateid, vce(cluster hhid)

esttab, keep(icds_treatment) compress label nodepvar

esttab using table2.rtf, keep(icds_treatment) compress label nodepvar r2 ar2 se ///
title(Table 2: Heterogeneous Treatment Effects by Child-specific characteristics) ///
starlevels(* 0.10 ** 0.05 *** 0.01) replace

eststo clear

** MATH SCORE 

* By child's sex: Female = 1 Male = 0
sort child_sex
by child_sex: eststo: quietly reg std_math_score icds_treatment child_age_wave2 ///
i.child_family_type mother_age mother_educ i.mother_health mother_surviving_children ///
father_educ i.school_type i.teacher_good i.teacher_regular i.hh_caste i.hh_urban ///
hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail  i.hh_water_avail i.hh_poor ///
i.hh_religion log_hh_expend i.midday_meal wtew i.stateid, vce(cluster hhid)

* By child's school type: public, private or other
sort school_type
by school_type: eststo: quietly reg std_math_score icds_treatment i.child_sex child_age_wave2 ///
i.child_family_type mother_age mother_educ i.mother_health ///
mother_surviving_children father_educ i.teacher_good i.teacher_regular ///
i.hh_caste i.hh_urban hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail ///
i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend i.midday_meal wtew ///
i.stateid, vce(cluster hhid)

esttab, keep(icds_treatment) compress label nodepvar

esttab using table2.rtf, keep(icds_treatment) compress label nodepvar r2 ar2 se ///
addnotes(Note: Outcome variables in the study are standardized reading and standardized math scores. The treatment variable is receipt of any ICDS intervention. The following covariates are controlled for: child's age, child's sex, child's family type, mother's education, mother's age, mother's general health condition, the number of surviving children the mother has, father's education, child's school type, child's teacher is regular,child's teacher is good, child receives mid-day meals, household receives water, household owns a toilet, household receives electricity, number of meals per day in household, household's residence, household is below poverty line, log of household's annual consumption expenditure, household's religion, and household's caste. State fixed effects are included in all regressions. Robust standard errors in parentheses are adjusted to account for household-level correlation) starlevels(* 0.10 ** 0.05 *** 0.01) append

eststo clear


*** Table 3: Impact of ICDS on Pre-adolescent Learning Outcomes - Heterogeneous Treatment Effects by Household-specific characteristics

** READING SCORE 

* By household's residence: urban or rural 
sort(hh_urban)
by hh_urban: eststo: quietly reg std_reading_score icds_treatment i.child_sex child_age_wave2 ///
i.child_family_type mother_age mother_educ i.mother_health ///
mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular ///
i.hh_caste hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail ///
i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend i.midday_meal wtew ///
i.stateid, vce(cluster hhid)

* By household's social caste: backward, forward caste or other
sort(hh_caste)
by hh_caste: eststo: quietly reg std_reading_score icds_treatment i.child_sex child_age_wave2 ///
i.child_family_type mother_age mother_educ i.mother_health ///
mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular ///
i.hh_urban hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail ///
i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend i.midday_meal wtew ///
i.stateid, vce(cluster hhid)

* By household's economic status: low or high economic status 

* Creating a binary variable for household's economic status
egen quintile_hh_expend=cut(log_hh_expend), group(5)
recode quintile_hh_expend (3/4 = 1 "High Economic Status") (0/2 = 0 "Low Economic Status"), gen(hh_economic_status)

sort(hh_economic_status)
by hh_economic_status: eststo: quietly reg std_reading_score icds_treatment ///
i.child_sex  child_age_wave2 i.child_family_type mother_age mother_educ i.mother_health ///
mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular ///
i.hh_caste i.hh_urban hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail ///
i.hh_water_avail i.hh_poor i.hh_religion i.midday_meal wtew ///
i.stateid, vce(cluster hhid)

esttab, keep(icds_treatment) compress label nodepvar

esttab using table3.rtf, keep(icds_treatment) compress label nodepvar r2 ar2 se ///
title(Table 3: Heterogeneous Treatment Effects by Household-specific characteristics) ///
modelwidth(5) starlevels(* 0.10 ** 0.05 *** 0.01) replace

eststo clear

** MATH SCORE 

* By household's residence: urban or rural 

sort(hh_urban)
by hh_urban: eststo: quietly reg std_math_score icds_treatment i.child_sex child_age_wave2 ///
i.child_family_type mother_age mother_educ i.mother_health ///
mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular ///
i.hh_caste hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail ///
i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend i.midday_meal wtew ///
i.stateid, vce(cluster hhid)

* By household's social caste: backward, forward caste or other

sort(hh_caste)
by hh_caste: eststo: quietly reg std_math_score icds_treatment i.child_sex child_age_wave2 ///
i.child_family_type mother_age mother_educ i.mother_health ///
mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular ///
i.hh_urban hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail ///
i.hh_water_avail i.hh_poor i.hh_religion log_hh_expend i.midday_meal wtew ///
i.stateid, vce(cluster hhid)

* By household's economic status: low or high economic status 

sort(hh_economic_status)
by hh_economic_status: eststo: quietly reg std_math_score icds_treatment ///
i.child_sex  child_age_wave2 i.child_family_type mother_age mother_educ i.mother_health ///
mother_surviving_children father_educ i.school_type i.teacher_good i.teacher_regular ///
i.hh_caste i.hh_urban hh_meals_perday i.hh_toilet_avail i.hh_electricity_avail ///
i.hh_water_avail i.hh_poor i.hh_religion i.midday_meal wtew ///
i.stateid, vce(cluster hhid)

esttab, keep(icds_treatment) compress label nodepvar

esttab using table3.rtf, keep(icds_treatment) compress label nodepvar r2 ar2 se ///
addnotes(Note: Outcome variables in the study are standardized reading and standardized math scores. The treatment variable is receipt of any ICDS intervention. The following covariates are controlled for: child's age, child's sex, child's family type, mother's education, mother's age, mother's general health condition, the number of surviving children the mother has, father's education, child's school type, child's teacher is regular,child's teacher is good, child receives mid-day meals, household receives water, household owns a toilet, household receives electricity, number of meals per day in household, household's residence, household is below poverty line, log of household's annual consumption expenditure, household's religion, and household's caste. State fixed effects are included in all regressions. Robust standard errors in parentheses are adjusted to account for household-level correlation) modelwidth(5) starlevels(* 0.10 ** 0.05 *** 0.01) append

eststo clear
	

	
********************************************************************************
************			        END OF SCRIPT				        ************
********************************************************************************	

  