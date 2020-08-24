*The below code is to process, analyze and gain insights from Health Information National Trend Survey data collected about the use of cancer and health-related information from the American public.

*The goal of the analysis is to identify the key parameters invovled in predicting if a user is likely to use healthcare wearable device or not.
**The exploratory analysis conducted on the data is in the end of the file.

log using "D:\Projects\Ranga\HINTS5 Cycle3 March2020\STATA study\LogSession5.smcl"


*******************Importing the HINTS Cycle3 data******************************
use "D:\Projects\Ranga\HINTS5 Cycle3 March2020\hints5_cycle3_public.dta",clear
*describe

*Preparing the merged weights for the survey analysis
gen merged_nwgt0=tg_all_finwt0
forvalues n2=1/50 {
local x2=`n2'+50
gen merged_nwgt`n2'=tg_all_finwt0
gen merged_nwgt`x2'=tg_all_finwt`n2'
}
save h5c3.dta, replace

*Setting Survey design and its parameters to reflect appropriate weights for Survey Regression
*We use Jack kniving that creates 100 samples and run models on these subsamples.
svyset [pw=merged_nwgt0], jkrw(merged_nwgt1-merged_nwgt100, multiplier(0.98)) vce(jack) dof(98) mse
save h5c3.dta, replace

*****************************SCREENING VARIABLES********************************
*We screen the data and remove the rows that are not in our interest. Like the users that do not have a phone or users that never used Internet.

tabulate havedevice_cat, nolab
drop if havedevice_cat == 3 | havedevice_cat == 4

*Removing the "Missing data" and "Partially Filled data"
tabulate wearabledevtrackhealth,nolab
drop if wearabledevtrackhealth<0

save h5c3.dta, replace

**************************** FILTERING DATA ************************************

*Keeping only the variables required for the analysis
*keep merged_nwgt0 merged_nwgt1-merged_nwgt100 wearabledevtrackhealth freqweardevtrackhealth havedevice_cat electronic_selfhealthinfo electronic_buymedicine electronic_talkdoctor electronic_trackedhealthcosts electronic_testresults electronic_madeappts electronic_ecigharms tablet_achievegoal tablet_makedecision tablet_discussionshcp willingsharedata_hcp willingsharedata_fam freqgoprovider generalhealth ownabilitytakecarehealth medconditions_diabetes medconditions_highbp medconditions_heartcondition medconditions_lungdisease medconditions_depression weightperception weightintention phq4  weeklyminutesmoderateexercise timesstrengthtraining enjoyexercise regexercise_pressure regexercise_appearance regexercise_guilt regexercise_enjoyment physact_helpsleep physact_reduceanxiety physact_reducepain averagesleepquality agegrpa agegrpb maritalstatus educa educb bmi raceethn raceethn5 selfgender hhinc
*Dropped variables: havedevice_cat qualitycare freqgourgentcare qualitycareurgentcare calorieinfo_fewercalories calorieinfo_feweritems calorieinfo_smallersizes

*===============================================================================
*RE-CODING the variables to reflect appropriate values and labels
*===============================================================================
recode wearabledevtrackhealth electronic_selfhealthinfo electronic_buymedicine electronic_talkdoctor electronic_trackedhealthcosts electronic_testresults electronic_madeappts electronic_ecigharms tablet_achievegoal tablet_makedecision tablet_discussionshcp willingsharedata_hcp willingsharedata_fam medconditions_diabetes medconditions_highbp medconditions_heartcondition medconditions_lungdisease medconditions_depression (2 = 0)
lab define newlabel 0 "No" 1 "Yes", modify
lab val wearabledevtrackhealth electronic_selfhealthinfo electronic_buymedicine electronic_talkdoctor electronic_trackedhealthcosts electronic_testresults electronic_madeappts electronic_ecigharms tablet_achievegoal tablet_makedecision tablet_discussionshcp willingsharedata_hcp willingsharedata_fam medconditions_diabetes medconditions_highbp medconditions_heartcondition medconditions_lungdisease medconditions_depression newlabel

gen wearabledevtrackhealth_r = wearabledevtrackhealth
lab val wearabledevtrackhealth_r newlabel

gen devtrackhealth_r = freqweardevtrackhealth
recode devtrackhealth_r (-1=0) (1=5) (2=4) (4=2) (5=1)
replace devtrackhealth_r = . if devtrackhealth_r <0

gen electronic_selfhealthinfo_r = electronic_selfhealthinfo
gen electronic_talkdoctor_r = electronic_talkdoctor
gen electronic_buymedicine_r = electronic_buymedicine
gen electronic_trackedhealthcosts_r = electronic_trackedhealthcosts 
gen electronic_testresults_r = electronic_testresults
gen electronic_madeappts_r = electronic_madeappts

replace electronic_buymedicine_r = . if electronic_buymedicine_r < 0
replace electronic_selfhealthinfo_r = . if electronic_selfhealthinfo_r < 0
replace electronic_talkdoctor_r = . if electronic_talkdoctor_r < 0
replace electronic_trackedhealthcosts_r = . if electronic_trackedhealthcosts_r < 0
replace electronic_testresults_r = . if electronic_testresults_r < 0
replace electronic_madeappts_r = . if electronic_madeappts_r < 0

generate tech_comfort_r = electronic_buymedicine_r + electronic_selfhealthinfo_r +electronic_talkdoctor_r +electronic_trackedhealthcosts_r +electronic_testresults_r+ electronic_madeappts_r

gen tablet_achievegoal_r = tablet_achievegoal
recode tablet_achievegoal_r (-1=0)
replace tablet_achievegoal_r = . if tablet_achievegoal_r < 0

gen tablet_makedecision_r = tablet_makedecision
recode tablet_makedecision_r (-1=0)
replace tablet_makedecision_r = . if tablet_makedecision_r < 0

gen tablet_discussionshcp_r = tablet_discussionshcp
recode tablet_discussionshcp_r (-1=0)
replace tablet_discussionshcp_r = . if tablet_discussionshcp_r < 0

gen willingsharedata_hcp_r = willingsharedata_hcp
recode willingsharedata_hcp_r (-1=0)
replace willingsharedata_hcp_r = . if willingsharedata_hcp_r < 0
 
gen willingsharedata_fam_r = willingsharedata_fam
recode willingsharedata_fam_r (-1=0)
replace willingsharedata_fam_r = . if willingsharedata_fam_r < 0

lab val tablet_achievegoal_r tablet_makedecision_r tablet_makedecision_r tablet_discussionshcp_r willingsharedata_hcp_r willingsharedata_fam_r newlabel

gen generalhealth_r = generalhealth
recode generalhealth_r (1=5) (2=4) (4=2) (5=1)
replace generalhealth_r = . if generalhealth_r < 0
lab define poor_to_excellent 1 "Poor" 2 "Fair" 3 "Good" 4 "Very Good" 5 "Excellent", modify
lab val generalhealth_r poor_to_excellent

gen ownabilitytakecarehealth_r = ownabilitytakecarehealth 
recode ownabilitytakecarehealth_r (1=5) (2=4) (4=2) (5=1)
replace ownabilitytakecarehealth_r = . if ownabilitytakecarehealth_r < 0
lab define notconfindent_to_compconfident 1 "Not confident at all" 2 "A little confident" 3 "Somewhat confident" 4 "Very confident" 5 "Completely confident", modify
lab val ownabilitytakecarehealth_r notconfindent_to_compconfident

*Generating medconditions_chronic and medconditions_mental from 5 variables in for loop below:
foreach i in medconditions_diabetes medconditions_highbp medconditions_heartcondition medconditions_lungdisease medconditions_depression {
	*drop `i'_r
	gen `i'_r = `i'
	replace `i'_r = . if `i'_r <0
}
egen medconditions_chronic_r = anycount(medconditions_diabetes_r medconditions_highbp_r medconditions_heartcondition_r medconditions_lungdisease_r ), value(1)
gen medconditions_mental_r = medconditions_depression_r

*Generating BINARY Chronic variable:
gen medconditions_chronic_r1 = medconditions_chronic_r
recode medconditions_chronic_r1 (2=1) (3=1) (4=1)
lab define chronic_or_not 0 "Not Chronic" 1 "Chronic" , modify
lab val medconditions_chronic_r1 chronic_or_not


gen weightperception_r = weightperception  
recode weightperception_r (3=1) (4=2) (5=3) (2=4) (1=5)
replace weightperception_r = . if weightperception_r < 0
lab define underweight_to_overweight 1 "Underweight" 2 "Slightly underweight" 3 "Just about the right weight for you?" 4 "Slightly overweight" 5 "Overweight", modify
lab val weightperception_r underweight_to_overweight

gen weightperception_r1 = weightperception_r  
recode weightperception_r1 (2=1) (3=1) (4=2) (5=3)
lab define weightperception_r1_label 1 "Underweight or Slightly underweight or right weight" 2 "Slightly overweight" 3 "Overweight", modify
lab val weightperception_r1 weightperception_r1_label

gen weightperception_r2 = weightperception_r  
recode weightperception_r2 (3=1) (1=2) (2=3)
lab define weightperception_r2_label 1 "Just about the right weight for you?" 2 "Underweight" 3 "Slightly underweight" 4 "Slightly overweight" 5 "Overweight", modify
lab val weightperception_r2 weightperception_r2_label


*Below LoseWeight is a binary variable of weightintention:
gen weightintention_r = weightintention
replace weightintention_r = . if weightintention_r < 0

gen lose_weight = weightintention_r
recode  lose_weight (2=0) (3=0) (4=0)

gen freqgoprovider_r = freqgoprovider
replace freqgoprovider_r = . if freqgoprovider_r < 0

gen freqgoprovider_r1 = freqgoprovider_r
recode freqgoprovider_r1 (2=1) (3=2) (4=2) (5=2) (6=2)
lab define freqgoprovider_r1_label 0 "Not at all" 1 "1-2 times" 2 "3 or more times", modify
lab val freqgoprovider_r1 freqgoprovider_r1_label


*tostring weightintention_r, replace  

*replace weightintention_r = "LoseWeight" if weightintention_r == "1"
*replace weightintention_r = "MaintainWeight" if weightintention_r == "2"
*replace weightintention_r = "GainWeight" if weightintention_r == "3"
*replace weightintention_r = "NoAttention" if weightintention_r == "4"
*replace weightintention_r = . if weightintention_r == "."

gen phq4_r = phq4
replace phq4_r = . if phq4_r < 0

gen weeklyminutesmoderateexercise_r = weeklyminutesmoderateexercise
replace weeklyminutesmoderateexercise_r = . if weeklyminutesmoderateexercise_r < 0

gen timesstrengthtraining_r = timesstrengthtraining
replace timesstrengthtraining_r = . if timesstrengthtraining_r < 0

gen enjoyexercise_r = enjoyexercise  
recode enjoyexercise_r (4=1) (3=2) (2=3) (1=4)
replace enjoyexercise_r = . if enjoyexercise_r < 0
lab define notatall_to_lot 1 "Not at all" 2 "A Little" 3 "Some" 4 "A Lot", modify
lab val enjoyexercise_r notatall_to_lot


foreach i in regexercise_pressure regexercise_appearance regexercise_guilt regexercise_enjoyment {
	*drop `i'_r
	gen `i'_r = `i'
	recode `i'_r (4=1) (3=2) (2=3) (1=4)
	replace `i'_r = . if `i'_r <0
	lab val `i'_r notatall_to_lot
}

*Making "No" and "Don't Know" to "No" as we are just interested in Yes/No
foreach i in physact_helpsleep physact_reduceanxiety physact_reducepain {
	*drop `i'_r
	gen `i'_r = `i'
	recode `i'_r (2=0) (3=0)
	replace `i'_r = . if `i'_r <0
	lab val `i'_r newlabel  
}

gen averagesleepquality_r = averagesleepquality  
recode averagesleepquality_r (4=1) (3=2) (2=3) (1=4)
replace averagesleepquality_r = . if averagesleepquality_r < 0
lab define bad_to_good 1 "Very bad" 2 "Fairly bad" 3 "Fairly Good" 4 "Very Good", modify
lab val averagesleepquality_r bad_to_good
save h5c3.dta, replace

*******************DEMOGRAPHIC VARIABLES****************************************
*AGE
ssc install copydesc
gen agegrpa_r = agegrpa
replace agegrpa_r = . if agegrpa_r < 0
copydesc  agegrpa  agegrpa_r

gen agegrpb_r = agegrpb
replace agegrpb_r = . if agegrpb_r < 0
copydesc  agegrpb  agegrpb_r

gen agegrpc_r = agegrpb
replace agegrpc_r = . if agegrpc_r < 0
recode agegrpc_r (2=1) (3=2)(4=2)(5=2)
lab define agegrpc_label 1 "18-49" 2 "50-75+", modify
lab val agegrpc_r agegrpc_label


*MARITAL STATUS
gen maritalstatus_r = maritalstatus  
recode maritalstatus_r (2=1) (3=1) (4=1) (5=1) (6=0)
replace maritalstatus_r = . if maritalstatus_r < 0
lab define married_or_not 1 "Married" 2 "Single", modify
lab val maritalstatus_r married_or_not

*EDUCATION STATUS
gen educa_r = educa
replace educa_r = . if educa_r < 0
copydesc  educa  educa_r

*BMI
gen bmi_r = bmi
replace bmi_r = . if bmi_r < 0

*RACE
gen raceethn5_r = raceethn5
replace raceethn5_r = . if raceethn5_r < 0
recode raceethn5_r (1=4) (4=1)
*copydesc  raceethn5  raceethn5_r
lab define race 1 "Non-Hispanic Asian" 2 "Non-Hispanic Black or African American" 3 "Hispanic" 4 "Non-Hispanic White" 5 "Non-Hispanic Other ", modify
lab val raceethn5_r race


*GENDER
gen selfgender_r = selfgender  
recode selfgender_r (2=0)
replace selfgender_r = . if selfgender_r < 0
lab define male_or_female 1 "Male" 0 "Female", modify
lab val selfgender_r male_or_female

gen genderc_r = genderc  
recode genderc_r (2=0)
replace genderc_r = . if genderc_r < 0
lab define male_or_female 1 "Male" 0 "Female", modify
lab val genderc_r male_or_female


*INCOME
gen hhinc_r = hhinc
replace hhinc_r = . if hhinc_r < 0
copydesc  hhinc  hhinc_r
save h5c3.dta, replace


*********************Saving transformed dataset*********************************
keep merged_nwgt0 merged_nwgt1-merged_nwgt100 wearabledevtrackhealth_r freqgoprovider_r devtrackhealth_r tech_comfort_r tablet_achievegoal_r tablet_makedecision_r tablet_discussionshcp_r willingsharedata_hcp_r willingsharedata_fam_r generalhealth_r ownabilitytakecarehealth_r medconditions_chronic_r medconditions_mental_r weightperception_r weightintention_r phq4_r weeklyminutesmoderateexercise_r timesstrengthtraining_r enjoyexercise_r regexercise_pressure_r regexercise_appearance_r regexercise_guilt_r regexercise_enjoyment_r physact_helpsleep_r physact_reduceanxiety_r physact_reducepain_r averagesleepquality_r agegrpa_r agegrpb_r maritalstatus_r educa_r raceethn5_r selfgender_r bmi_r hhinc_r lose_weight medconditions_diabetes_r medconditions_highbp_r medconditions_heartcondition_r medconditions_lungdisease_r

save h5c3_clean.dta, replace

*===============================================================================
pwcorr devtrackhealth_r tech_comfort_r generalhealth_r  phq4_r weeklyminutesmoderateexercise_r timesstrengthtraining_r enjoyexercise_r regexercise_pressure_r regexercise_appearance_r regexercise_guilt_r regexercise_enjoyment_r averagesleepquality_r weightperception_r, sig


i.agegrpa_r i.maritalstatus_r i.educa_r i.raceethn5_r i.selfgender_r i.hhinc_r weeklyminutesmoderateexercise_r timesstrengthtraining_r enjoyexercise_r regexercise_pressure_r regexercise_appearance_r regexercise_guilt_r regexercise_enjoyment_r physact_helpsleep_r physact_reduceanxiety_r physact_reducepain_r averagesleepquality_r generalhealth_r ownabilitytakecarehealth_r i.medconditions_chronic_r i.freqgoprovider_r weightperception_r lose_weight tech_comfort_r tablet_achievegoal_r tablet_makedecision_r tablet_discussionshcp_r bmi_r

i.agegrpc_r i.maritalstatus_r i.educa_r i.raceethn5_r bmi_r i.selfgender_r i.hhinc_r   i.physact_reduceanxiety_r physact_reducepain_r averagesleepquality_r i.generalhealth_r  i.freqgoprovider_r1 i.weightperception_r2 i.lose_weight tech_comfort_r tablet_achievegoal_r i.medconditions_diabetes_r i.medconditions_highbp_r i.medconditions_heartcondition_r i.medconditions_lungdisease_r  i.enjoyexercise_r 

*Senior Citizens variables:
 i.maritalstatus_r i.educa_r i.raceethn5_r bmi_r i.selfgender_r i.hhinc_r   i.physact_reduceanxiety_r physact_reducepain_r averagesleepquality_r i.generalhealth_r  i.freqgoprovider_r1 i.weightperception_r2 i.lose_weight tech_comfort_r tablet_achievegoal_r i.medconditions_diabetes_r i.medconditions_highbp_r i.medconditions_heartcondition_r i.medconditions_lungdisease_r  i.enjoyexercise_r 
 
*Prof:
logistic wearabledevtrackhealth_r i.maritalstatus_r i.educa_r i.raceethn5_r bmi_r i.selfgender_r i.hhinc_r i.physact_reduceanxiety_r physact_reducepain_r averagesleepquality_r generalhealth_r  freqgoprovider_r1 i.weightperception_r2 i.lose_weight tech_comfort_r tablet_achievegoal_r i.medconditions_chronic_r1  i.enjoyexercise_r

	*Frequency of using wearabledevtrackhealth
logistic devtrackhealth_r i.maritalstatus_r i.educa_r i.raceethn5_r bmi_r i.selfgender_r i.hhinc_r i.physact_reduceanxiety_r physact_reducepain_r averagesleepquality_r generalhealth_r  freqgoprovider_r1 i.weightperception_r2 i.lose_weight tech_comfort_r tablet_achievegoal_r i.medconditions_chronic_r1 i.enjoyexercise_r

	* willingness to share data
logistic willingsharedata_hcp_r i.maritalstatus_r i.educa_r i.raceethn5_r bmi_r i.selfgender_r i.hhinc_r i.physact_reduceanxiety_r physact_reducepain_r averagesleepquality_r generalhealth_r  freqgoprovider_r1 i.weightperception_r2 i.lose_weight tech_comfort_r tablet_achievegoal_r i.medconditions_chronic_r1  i.enjoyexercise_r	
	
*Survey logistic
svy: logistic wearabledevtrackhealth_r i.agegrpb_r i.maritalstatus_r i.educa_r i.raceethn5_r bmi_r i.selfgender_r i.hhinc_r i.physact_reduceanxiety_r physact_reducepain_r averagesleepquality_r generalhealth_r  freqgoprovider_r1 i.weightperception_r2 i.lose_weight tech_comfort_r tablet_achievegoal_r  i.enjoyexercise_r



*=================================================================================================
****************** Generating Exploratory analysis table *****************************************
svy: tabulate  agegrpb_r wearabledevtrackhealth_r, col
svy: tabulate  raceethn5_r wearabledevtrackhealth_r, col 
svy: tabulate   selfgender_r wearabledevtrackhealth_r, col
svy: tabulate   maritalstatus_r  wearabledevtrackhealth_r, col
svy: tabulate    educa_r  wearabledevtrackhealth_r, col
svy: tabulate   hhinc_r  wearabledevtrackhealth_r, col
svy: tabulate    generalhealth_r  wearabledevtrackhealth_r, col

tabulate  agegrpb_r wearabledevtrackhealth_r, col
tabulate  raceethn5_r wearabledevtrackhealth_r, col 
tabulate   selfgender_r wearabledevtrackhealth_r, col
tabulate   maritalstatus_r  wearabledevtrackhealth_r, col
tabulate    educa_r  wearabledevtrackhealth_r, col
tabulate   hhinc_r  wearabledevtrackhealth_r, col
tabulate    generalhealth_r  wearabledevtrackhealth_r, col

ttest devtrackhealth_r, by(wearabledevtrackhealth_r)
ttest tech_comfort_r, by(wearabledevtrackhealth_r)
ttest generalhealth_r, by(wearabledevtrackhealth_r)
ttest ownabilitytakecarehealth_r, by(wearabledevtrackhealth_r)
ttest phq4_r, by(wearabledevtrackhealth_r)
ttest weeklyminutesmoderateexercise_r, by(wearabledevtrackhealth_r)
ttest timesstrengthtraining_r, by(wearabledevtrackhealth_r)
     

*===============================================================================
**************************Chi-Squared Test**************************************

tabulate wearabledevtrackhealth_r tablet_achievegoal_r, chi2
tabulate wearabledevtrackhealth_r tablet_makedecision_r, chi2
tabulate wearabledevtrackhealth_r tablet_discussionshcp_r, chi2
tabulate wearabledevtrackhealth_r willingsharedata_hcp_r, chi2
tabulate wearabledevtrackhealth_r willingsharedata_fam_r, chi2
tabulate wearabledevtrackhealth_r medconditions_chronic_r, chi2
tabulate wearabledevtrackhealth_r medconditions_mental_r, chi2
tabulate wearabledevtrackhealth_r weightintention_r, chi2

tabulate wearabledevtrackhealth_r physact_helpsleep_r, chi2
tabulate wearabledevtrackhealth_r physact_reduceanxiety_r, chi2
tabulate wearabledevtrackhealth_r physact_reducepain_r, chi2

tabulate wearabledevtrackhealth_r agegrpa_r, chi2
tabulate wearabledevtrackhealth_r maritalstatus_r, chi2
tabulate wearabledevtrackhealth_r educa_r, chi2
tabulate wearabledevtrackhealth_r raceethn5_r, chi2
tabulate wearabledevtrackhealth_r selfgender_r, chi2
tabulate wearabledevtrackhealth_r hhinc_r, chi2



*===============================================================================
******************** Missing Value analysis in key variables *******************

*Checking missing values in variables:
ssc install mdesc
mdesc
bysort  wearabledevtrackhealth_r: mdesc

*Generating a count variable to see the count of missing in each observation
egen count = rowmiss(wearabledevtrackhealth_r - hhinc_r)
tabulate count

*gen count = 0
*foreach var of varlist wearabledevtrackhealth_r - hhinc_r {
*replace count = count + 1 if `var' == .
*}
*summarize count
bysort wearabledevtrackhealth_r: tabulate count
list * if wearabledevtrackhealth_r == 1 & count==1,nolab

*Counting the negative entries in observations by excluding "qualitycareurgentcare" variable from the varlist
drop count_exc
local varlist
foreach var of varlist electronic_selfhealthinfo - weeklyminutesmoderateexercise {
    if inlist("`var'", "qualitycareurgentcare") continue
    local varlist `varlist' `var'
}
gen count_exc = 0
foreach var of varlist `varlist' {
replace count_exc = count_exc + 1 if `var' == .
}
bysort wearabledevtrackhealth: tabulate count_exc

*===============================================================================
*        ============ wearabledevtrackhealth = "YES" ================
*===============================================================================
*Analysing the variables with 1 missing value (qualitycareurgentcare)
list * if wearabledevtrackhealth == 1 & count_exc==1,nolab
	*102 records with below variable as -1 (Inapplicable) -- Should be FIXED in re-coding
tabulate  qualitycareurgentcare if wearabledevtrackhealth == 1 & count_exc==1,nolab

*Analysing the calorie variables resulting in 3 missing values in observations
list * if wearabledevtrackhealth == 1 & count_exc==3,nolab
	*Atleast 292 records with below 3 variables as -1 (Inapplicable)  -- Should be FIXED in re-coding
tabulate calorieinfo_fewercalories  if wearabledevtrackhealth == 1 & count_exc==3,nolab
tabulate  calorieinfo_feweritems if wearabledevtrackhealth == 1 & count_exc==3,nolab
tabulate  calorieinfo_smallersizes if wearabledevtrackhealth == 1 & count_exc==3,nolab

*Analysing the  variables resulting in 4 missing values in observations
list * if wearabledevtrackhealth == 1 & count_exc==4,nolab
	*29 records with below hhinc variables as -1 (inapplicable)  -- Should be FIXED in re-coding
tabulate  qualitycare if wearabledevtrackhealth == 1 & count_exc==4,nolab
	*13 records with below hhinc variables as -9 (error)
tabulate  hhinc if wearabledevtrackhealth == 1 & count_exc==4,nolab
	*Atleast 72 records with below 3 variables as -1 (Inapplicable)  -- Should be FIXED in re-coding
tabulate calorieinfo_fewercalories  if wearabledevtrackhealth == 1 & count_exc==4,nolab
tabulate calorieinfo_feweritems if wearabledevtrackhealth == 1 & count_exc==4,nolab
tabulate calorieinfo_smallersizes if wearabledevtrackhealth == 1 & count_exc==4,nolab
	*10 records with below weightintention variables as -5 (Multiple response selected error)
tabulate  weightintention if wearabledevtrackhealth == 1 & count_exc==4,nolab


*=======================================================================
*         ============ wearabledevtrackhealth = "NO" ================= 
*======================================================================= 
list * if wearabledevtrackhealth == 0 & count_exc==3,nolab
	*All of the 3 variables below have the values as negative as they do not have a health tracking device --Should be FIXED
tabulate freqweardevtrackhealth if wearabledevtrackhealth == 0 & count_exc==3,nolab
tabulate willingsharedata_hcp if wearabledevtrackhealth == 0 & count_exc==3,nolab
tabulate willingsharedata_fam if wearabledevtrackhealth == 0 & count_exc==3,nolab

tabulate hhinc if wearabledevtrackhealth == 0 & count_exc==4,nolab

tabulate qualitycare if wearabledevtrackhealth == 0 & count_exc==4,nolab

tabulate weightintention if wearabledevtrackhealth == 0 & count_exc==4,nolab

tabulate selfgender if wearabledevtrackhealth == 0 & count_exc==4,nolab

	*Atleast 964 records have all three of the below variables as -1 (Inapplicable) -- Should be FIXED in re-coding
tabulate calorieinfo_fewercalories  if wearabledevtrackhealth == 0 & count_exc==6,nolab
tabulate calorieinfo_feweritems if wearabledevtrackhealth == 0 & count_exc==6,nolab
tabulate calorieinfo_smallersizes if wearabledevtrackhealth == 0 & count_exc==6,nolab


tabulate enjoyexercise if wearabledevtrackhealth == 0 & count_exc==7,nolab
tabulate hhinc if wearabledevtrackhealth == 0 & count_exc==7,nolab
tabulate weightperception if wearabledevtrackhealth == 0 & count_exc==7,nolab
tabulate regexercise_pressure if wearabledevtrackhealth == 0 & count_exc==7,nolab
tabulate selfgender if wearabledevtrackhealth == 0 & count_exc==7,nolab
tabulate qualitycare if wearabledevtrackhealth == 0 & count_exc==7,nolab


*Based on observation from above analyses, Re-Counting negative values by ignoring more variables
drop count_exc1
local varlist
foreach var of varlist electronic_selfhealthinfo - weeklyminutesmoderateexercise {
    if inlist("`var'", "qualitycareurgentcare","calorieinfo_fewercalories","calorieinfo_feweritems","calorieinfo_smallersizes","freqweardevtrackhealth", "willingsharedata_hcp","willingsharedata_fam","qualitycare") continue
    local varlist `varlist' `var'
}
gen count_exc1 = 0
foreach var of varlist `varlist' {
replace count_exc1 = count_exc1 + 1 if `var' < 0
}
bysort wearabledevtrackhealth: tabulate count_exc1


*Identifying the percentage of records with negative values
drop neg_or_not
egen neg_or_not =rowmin(_all)
replace neg_or_not = -1 if neg_or_not<0
replace neg_or_not = 1 if neg_or_not>=0

tabulate neg_or_not

*pca electronic_selfhealthinfo electronic_buymedicine electronic_talkdoctor electronic_trackedhealthcosts electronic_testresults electronic_madeappts electronic_ecigharms
