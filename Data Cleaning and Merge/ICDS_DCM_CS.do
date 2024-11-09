***** THESIS CODE FILE CLEANING REPLICATION IN STATA 

***** Title: Data Cleaning and Merge Code

***** Date Created: Jan 10, 2024
***** Date Updated: Feb 15, 2024

***** This Script has the following sections: 
** Section 1: Merging Data 
** Section 2: Data Cleaning

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

***** Please note: This file not reproducible because data cannot be made publicly available.The analysis for this project is still underway.  


********************************************************************************
************			     SECTION 1: MERGING DATA				************
********************************************************************************	


******************************** FIRST MERGE ***********************************

clear

*** Importing household data set 
use "/Users/Desktop/DATA CLEANING AND MERGE/hhwide.dta"

*** Merging household and eligible women data   
merge m:1 HHBASE using "/Users/Desktop/DATA CLEANING AND MERGE/ewwide.dta"

*** Keep matched data and drop variable for merge
keep if _merge==3
drop _merge

*** Saving merged data set 
save "/Users/Desktop /DATA CLEANING AND MERGE/merged_hh_ew_data.dta"


******************************* SECOND MERGE ***********************************

clear 

*** Importing individual data set 
use "/Users/Desktop /DATA CLEANING AND MERGE/indwide.dta"

*** Keep only children in the age group of 8 to 11 years 
keep if RO5>=8 & RO5<=11

*** Merging individual data with merged household and eligible women data 
merge m:m HHBASE using "/Users/Desktop/DATA CLEANING AND MERGE/merged_hh_ew_data.dta"

*** Keep matched data and drop variable for merge
keep if _merge==3
drop _merge

*** Saving merged data set 
save "/Users/Desktop /DATA CLEANING AND MERGE/merged_survey_data.dta"


********************************************************************************
************			     SECTION 2: DATA CLEANING				************
********************************************************************************	

clear

*** Importing merged survey data 
use "/Users/Desktop /DATA CLEANING AND MERGE/merged_survey_data.dta"

*** Keeping only children currently enrolled in school
keep if ED5==1

*** Dropping redundant variables 
drop PSUID HHSPLITID IDPERSON IDHH MB14 AD6 AD7 URBAN EW6 XRO4 CH2 ID11 HHBASE ///
GROUPS6 PBASE XBMI WT ED5 XLB48

* Generating a variable for receiving any ICDS intervention and adding label - Treatment 
gen icds_treatment = .
replace  icds_treatment = 1 if (XLB49A1 == 1) | (XLB49B1 == 1) | (XLB49C1 == 1) | ///
(XLB49D1 == 1) | (XLB49E1 == 1)
replace  icds_treatment = 0 if (XLB49A1 == 0) & (XLB49B1 == 0) & (XLB49C1 == 0) & ///
(XLB49D1 == 0) & (XLB49E1 == 0)
label variable icds_treatment "Received an ICDS intervention"
label define treatment 1 "Yes" 0 "No"
label values icds_treatment treatment

* Dropping observations if treatment is missing in the survey data
drop if icds_treatment == .
		  
*** Generating a variable for standardized reading score and adding label - Outcome 
egen std_reading_score = std(TA8B)
label variable std_reading_score "Standardized values of Raw Reading Scores"

*** Generating a variable for standardized math score and and adding label - Outcome
egen std_math_score = std(TA9B)
label variable std_math_score "Standardized values of Raw Math Scores"

* Dropping observations if outcome data is missing 
drop if std_reading_score == . 
drop if std_math_score == . 

*** Generating a variable for log transformation of household expenditure and adding label
gen log_hh_expend = log(XCOTOTAL)
label variable log_hh_expend "Log of Household Expenditure"

	  
*** Creating dummy variables 

* Child's sex
recode XRO3 (2 = 1 "Female") (1 = 0 "Male"), gen(child_sex)

* Child's family type 
recode XFAMCAT (3 = 1 "Nuclear") (4/5 = 0 "Joint"), gen(child_family_type)

* Child's mother's health 
recode XEW10 (1/3 = 1 "Good") (4/5 = 0 "Poor") (missing = .), gen(mother_health)

* Child's teacher is regular 
recode CH6 (3 = 1 "Regular") (1/2 = 0 "Irregular") (missing = .), gen(teacher_regular)

* Child's teacher is good 
recode CH10 (1/2 = 1 "Yes") (3/4 = 0 "No") (missing = .), gen(teacher_good)

* Availability of toilet in child's household 
recode XSA4 (2/4 = 1 "Yes") (1 = 0 "No") (missing = .), gen(hh_toilet_avail)

*** Creating categorical variables 

* Child's school type 
recode CS4 (1/3 = 1 "Public") (4 = 2 "Private") (5/7 = 3 "Other") (missing = .), gen(school_type)

* Household's religion 
recode XID11 (1 = 1 "Hindu") (2 = 2 "Muslim") (3 = 3 "Christian") (4/9 = 4 "Other"), ///
gen(hh_religion)

* Household's caste
recode XGROUPS6 (2 = 1 "Forward caste") (3/4 = 2 "Backward caste") (5/7 = 3 "Other"), ///
gen(hh_caste) 

* State ID 
recode STATEID (1 = 1 "Jammu & Kashmir") (2 = 2 "Himachal Pradesh") (3 = 3 "Punjab") ///
(5 = 4 "Uttrakhand") (6 = 5 "Haryana") (8 = 6 "Rajasthan") (9 = 7 "Uttar Pradesh") ///
(10 = 8 "Bihar") (11/18 = 9 "Northeast") (19 = 10 "West Bengal") (20 = 11 "Jharkhand") ///
(21 = 12 "Orissa") (22 = 13 "Chhattisgarh") (23 = 14 "Madhya Pradesh") (24 = 15 "Gujarat") ///
(27 = 16 "Maharashtra") (28 = 17 "Andhra Pradesh") (29 = 18 "Karnataka") (30 = 19 "Goa") ///
(32 = 20 "Kerala") (33 = 21 "Tamil Nadu") (4 = 22 "Union Territories") (7 = 22 "Union Territories") (25 = 22 "Union Territories") (26 = 22 "Union Territories") (31 = 22 "Union Territories") (34 = 22 "Union Territories") (35 = 22 "Union Territories"), gen(stateid)

order stateid

*** Defining value label for whether household is below poverty line 
label define poor 1 "Below poverty line" 0 "Above poverty line"
label values XPOOR poor

*** Dropping old variables 
drop XLB49A1 XLB49B1 XLB49C1 XLB49D1 XLB49E1 TA8B TA9B XCOTOTAL XRO3 XFAMCAT XEW10 CH6 CH10 XSA4 CS4 XID11 XGROUPS6 STATEID 

*** Renaming remanining variables 
rename (XWA5A  XFU1 XFU2 XURBAN XEW6 XEW8 XEW9 XSPED6 XRO5 XPOOR RO5 CS14Y)	(hh_water_avail hh_electricity_avail hh_meals_perday hh_urban mother_age mother_educ mother_surviving_children father_educ child_age_wave1 hh_poor child_age_wave2 midday_meal)	  
		  
*** Converting variables into lowercase  		  
rename *, lower	

*** Saving cleaned data set 
save "/Users/Desktop /DATA CLEANING AND MERGE/survey_data_cleaned.dta"	


********************************************************************************
************			        END OF SCRIPT				        ************
********************************************************************************	

  
  
