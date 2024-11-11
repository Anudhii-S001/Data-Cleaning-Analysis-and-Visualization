***** Stata Version 18.0

***** Title: Summary Statistics and T-test table in Latex

***** Date Created: Jan 25, 2024
***** Date Updated: Feb 20, 2024

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


clear 

*** Importing survey data 
* use "/Users/Desktop/DATA ANALYSIS AND TABLES/survey_data_cleaned.dta"
use "/Users/Desktop/survey_data_cleaned.dta"

*** Defining the list of control variables 
local vars "child_age_wave1 child_age_wave2 child_family_type child_sex mother_age mother_educ mother_health mother_surviving_children father_educ school_type teacher_good teacher_regular midday_meal hh_caste hh_religion hh_meals_perday hh_poor hh_toilet_avail hh_electricity_avail hh_water_avail hh_urban log_hh_expend"

*** Looping over each variable
foreach var in `vars' {

    ** Performing a t-test on the variable of interest by the grouping variable
    ttest `var', by(icds_treatment)
    
    ** Extracting values for the treatment group (icds_treatment == 1)
    local mean_treatment = r(mu_1)         // Mean 
    local sd_treatment = r(sd_1)           // Standard deviation 
    local obs_treatment = r(N_1)           // Number of observations 

    ** Extracting values for the treatment group (icds_treatment == 0)
    local mean_control = r(mu_2)            // Mean 
    local sd_control = r(sd_2)              // Standard deviation 
    local obs_control = r(N_2)              // Number of observations 

    ** Extracting the p-value for the t-test
    local p_value = r(p)                    // p-value for the difference in means
    
    ** Creating new variables to hold the extracted values
            gen `var'_mean_t = `mean_treatment'
            gen `var'_sd_t = `sd_treatment'
            gen `var'_obs_t = `obs_treatment'
	        gen `var'_mean_c = `mean_control'
            gen `var'_sd_c = `sd_control'
            gen `var'_obs_c = `obs_control'
	        gen `var'_pvalue = `p_value'

}

** Creating a table in latex 

*file open table using "/Users/anudhiisundaram/Desktop/sum_stat_table.tex", write replace
file open table using "/Users/Tia/Desktop/Anudhii/sum_stat_table.tex", write replace

file write table "\documentclass[12pt]{article}" _n
file write table "\usepackage{graphicx}" _n
file write table "\usepackage{caption}" _n
file write table "\usepackage{booktabs}" _n
file write table "\usepackage{multirow}" _n
file write table "\usepackage{makecell}" _n
* This will change the height of the table
file write table "\renewcommand{\arraystretch}{1.5}" _n   
file write table "\setlength{\arrayrulewidth}{0.5mm}" _n


file write table "\begin{document}" _n

* Start the table
file write table "\begin{table}[ht]" _n
file write table " \hspace*{-1in}" _n
file write table " \begin{minipage}{\textwidth} " _n

file write table "    \centering" _n
file write table "    \caption{Summary Statistics and T-test}" _n
file write table "    \begin{tabular}{lccc ccc c}" _n
file write table "        \toprule" _n
file write table "        & \multicolumn{3}{c}{\textbf{Treatment}} & \multicolumn{3}{c}{\textbf{Control}} & \\" _n
file write table "        \cmidrule(lr){2-4} \cmidrule(lr){5-7}" _n
file write table "        \textbf{Variable} & \textbf{Mean} & \textbf{Std. Dev.} & \textbf{N} & \textbf{Mean} & \textbf{Std. Dev.} & \textbf{N} & \textbf{P-value} \\" _n
file write table "        \midrule" _n

* Write data rows (you need to define the variables in Stata for these)
file write table "        Child's Age Wave 1 & `=string(round(child_age_wave1_mean_t, 0.001))' & `=string(round(child_age_wave1_sd_t, 0.001))' & `=string(round(child_age_wave1_obs_t, 0.001))' & `=string(round(child_age_wave1_mean_c, 0.001))' & `=string(round(child_age_wave1_sd_c, 0.001))' & `=string(round(child_age_wave1_obs_c, 0.001))' & `=string(round(child_age_wave1_pvalue, 0.001))' \\" _n

file write table "        Child's Age Wave 2 & `=string(round(child_age_wave2_mean_t, 0.001))' & `=string(round(child_age_wave2_sd_t, 0.001))' & `=string(round(child_age_wave2_obs_t, 0.001))' & `=string(round(child_age_wave2_mean_c, 0.001))' & `=string(round(child_age_wave2_sd_c, 0.001))' & `=string(round(child_age_wave2_obs_c, 0.001))' & `=string(round(child_age_wave2_pvalue, 0.001))' \\" _n

file write table "        Child's Family Type & `=string(round(child_family_type_mean_t, 0.001))' & `=string(round(child_family_type_sd_t, 0.001))' & `=string(round(child_family_type_obs_t, 0.001))' & `=string(round(child_family_type_mean_c, 0.001))' & `=string(round(child_family_type_sd_c, 0.001))' & `=string(round(child_family_type_obs_c, 0.001))' & `=string(round(child_family_type_pvalue, 0.001))' \\" _n

file write table "        Child's Sex & `=string(round(child_sex_mean_t, 0.001))' & `=string(round(child_sex_sd_t, 0.001))' & `=string(round(child_sex_obs_t, 0.001))' & `=string(round(child_sex_mean_c, 0.001))' & `=string(round(child_sex_sd_c, 0.001))' & `=string(round(child_sex_obs_c, 0.001))' & `=string(round(child_sex_pvalue, 0.001))' \\" _n

file write table "        Mother's age & `=string(round(mother_age_mean_t, 0.001))' & `=string(round(mother_age_sd_t, 0.001))' & `=string(round(mother_age_obs_t, 0.001))' & `=string(round(mother_educ_mean_c, 0.001))' & `=string(round(mother_age_sd_c, 0.001))' & `=string(round(mother_age_obs_c, 0.001))' & `=string(round(mother_age_pvalue, 0.001))' \\" _n

file write table "        Mother's education & `=string(round(mother_educ_mean_t, 0.001))' & `=string(round(mother_educ_sd_t, 0.001))' & `=string(round(mother_educ_obs_t, 0.001))' & `=string(round(mother_educ_mean_c, 0.001))' & `=string(round(mother_educ_sd_c, 0.001))' & `=string(round(mother_educ_obs_c, 0.001))' & `=string(round(mother_educ_pvalue, 0.001))' \\" _n

file write table "        Mother's health & `=string(round(mother_health_mean_t, 0.001))' & `=string(round(mother_health_sd_t, 0.001))' & `=string(round(mother_health_obs_t, 0.001))' & `=string(round(mother_health_mean_c, 0.001))' & `=string(round(mother_health_sd_c, 0.001))' & `=string(round(mother_health_obs_c, 0.001))' & `=string(round(mother_health_pvalue, 0.001))' \\" _n

file write table "        Number of surviving children & `=string(round(mother_surviving_children_mean_t, 0.001))' & `=string(round(mother_surviving_children_sd_t, 0.001))' & `=string(round(mother_surviving_children_obs_t, 0.001))' & `=string(round(mother_surviving_children_mean_c, 0.001))' & `=string(round(mother_surviving_children_sd_c, 0.001))' & `=string(round(mother_surviving_children_obs_c, 0.001))' & `=string(round(mother_surviving_children_pvalue, 0.001))' \\" _n

file write table "        Father's education & `=string(round(father_educ_mean_t, 0.001))' & `=string(round(father_educ_sd_t, 0.001))' & `=string(round(father_educ_obs_t, 0.001))' & `=string(round(father_educ_mean_c, 0.001))' & `=string(round(father_educ_sd_c, 0.001))' & `=string(round(father_educ_obs_c, 0.001))' & `=string(round(father_educ_pvalue, 0.001))' \\" _n

file write table "        Child's school type & `=string(round(school_type_mean_t, 0.001))' & `=string(round(school_type_sd_t, 0.001))' & `=string(round(school_type_obs_t, 0.001))' & `=string(round(school_type_mean_c, 0.001))' & `=string(round(school_type_sd_c, 0.001))' & `=string(round(school_type_obs_c, 0.001))' & `=string(round(school_type_pvalue, 0.001))' \\" _n

file write table "        Child's teacher is good & `=string(round(teacher_good_mean_t, 0.001))' & `=string(round(teacher_good_sd_t, 0.001))' & `=string(round(teacher_good_obs_t, 0.001))' & `=string(round(teacher_good_mean_c, 0.001))' & `=string(round(teacher_good_sd_c, 0.001))' & `=string(round(teacher_good_obs_c, 0.001))' & `=string(round(teacher_good_pvalue, 0.001))' \\" _n

file write table "        Child's teacher is regular & `=string(round(teacher_regular_mean_t, 0.001))' & `=string(round(teacher_regular_sd_t, 0.001))' & `=string(round(teacher_regular_obs_t, 0.001))' & `=string(round(teacher_regular_mean_c, 0.001))' & `=string(round(teacher_regular_sd_c, 0.001))' & `=string(round(teacher_regular_obs_c, 0.001))' & `=string(round(teacher_regular_pvalue, 0.001))' \\" _n

file write table "        Receives midday meals & `=string(round(midday_meal_mean_t, 0.001))' & `=string(round(midday_meal_sd_t, 0.001))' & `=string(round(midday_meal_obs_t, 0.001))' & `=string(round(midday_meal_mean_c, 0.001))' & `=string(round(midday_meal_sd_c, 0.001))' & `=string(round(midday_meal_obs_c, 0.001))' & `=string(round(midday_meal_pvalue, 0.001))' \\" _n

file write table "        HH Caste & `=string(round(hh_caste_mean_t, 0.001))' & `=string(round(hh_caste_sd_t, 0.001))' & `=string(round(hh_caste_obs_t, 0.001))' & `=string(round(hh_caste_mean_c, 0.001))' & `=string(round(hh_caste_sd_c, 0.001))' & `=string(round(hh_caste_obs_c, 0.001))' & `=string(round(hh_caste_pvalue, 0.001))' \\" _n

file write table "        HH Religion & `=string(round(hh_religion_mean_t, 0.001))' & `=string(round(hh_religion_sd_t, 0.001))' & `=string(round(hh_religion_obs_t, 0.001))' & `=string(round(hh_religion_mean_c, 0.001))' & `=string(round(hh_religion_sd_c, 0.001))' & `=string(round(hh_religion_obs_c, 0.001))' & `=string(round(hh_religion_pvalue, 0.001))' \\" _n

file write table "       HH Number of meals/day & `=string(round(hh_meals_perday_mean_t, 0.001))' & `=string(round(hh_meals_perday_sd_t, 0.001))' & `=string(round(hh_meals_perday_obs_t, 0.001))' & `=string(round(hh_meals_perday_mean_c, 0.001))' & `=string(round(hh_meals_perday_sd_c, 0.001))' & `=string(round(hh_meals_perday_obs_c, 0.001))' & `=string(round(hh_meals_perday_pvalue, 0.001))' \\" _n

file write table "        HH Poor & `=string(round(hh_poor_mean_t, 0.001))' & `=string(round(hh_poor_sd_t, 0.001))' & `=string(round(hh_poor_obs_t, 0.001))' & `=string(round(hh_poor_mean_c, 0.001))' & `=string(round(hh_poor_sd_c, 0.001))' & `=string(round(hh_poor_obs_c, 0.001))' & `=string(round(hh_poor_pvalue, 0.001))' \\" _n

file write table "        HH Toilet available & `=string(round(hh_toilet_avail_mean_t, 0.001))' & `=string(round(hh_toilet_avail_sd_t, 0.001))' & `=string(round(hh_toilet_avail_obs_t, 0.001))' & `=string(round(hh_toilet_avail_mean_c, 0.001))' & `=string(round(hh_toilet_avail_sd_c, 0.001))' & `=string(round(hh_toilet_avail_obs_c, 0.001))' & `=string(round(hh_toilet_avail_pvalue, 0.001))' \\" _n

file write table "        HH Electricity available & `=string(round(hh_electricity_avail_mean_t, 0.001))' & `=string(round(hh_electricity_avail_sd_t, 0.001))' & `=string(round(hh_electricity_avail_obs_t, 0.001))' & `=string(round(hh_electricity_avail_mean_c, 0.001))' & `=string(round(hh_electricity_avail_sd_c, 0.001))' & `=string(round(hh_electricity_avail_obs_c, 0.001))' & `=string(round(hh_electricity_avail_pvalue, 0.001))' \\" _n

file write table "        HH Water available & `=string(round(hh_water_avail_mean_t, 0.001))' & `=string(round(hh_water_avail_sd_t, 0.001))' & `=string(round(hh_electricity_avail_obs_t, 0.001))' & `=string(round(hh_water_avail_mean_c, 0.001))' & `=string(round(hh_water_avail_sd_c, 0.001))' & `=string(round(hh_water_avail_obs_c, 0.001))' & `=string(round(hh_water_avail_pvalue, 0.001))' \\" _n

file write table "        HH Residence & `=string(round(hh_urban_mean_t, 0.001))' & `=string(round(hh_urban_sd_t, 0.001))' & `=string(round(hh_urban_obs_t, 0.001))' & `=string(round(hh_urban_mean_c, 0.001))' & `=string(round(hh_urban_sd_c, 0.001))' & `=string(round(hh_urban_obs_c, 0.001))' & `=string(round(hh_urban_pvalue, 0.001))' \\" _n

file write table "        Log HH expenditure & `=string(round(log_hh_expend_mean_t, 0.001))' & `=string(round(log_hh_expend_sd_t, 0.001))' & `=string(round(hh_electricity_avail_obs_t, 0.001))' & `=string(round(log_hh_expend_mean_c, 0.001))' & `=string(round(log_hh_expend_sd_c, 0.001))' & `=string(round(log_hh_expend_obs_c, 0.001))' & `=string(round(log_hh_expend_pvalue, 0.001))' \\" _n

file write table "        \bottomrule" _n

* Close the table
file write table "    \end{tabular}" _n
file write table "    \end{minipage}" _n
file write table "\end{table}" _n
file write table "\end{document}" _n

* Close the file
file close table


********************************************************************************
************			        END OF SCRIPT				        ************
********************************************************************************	

  