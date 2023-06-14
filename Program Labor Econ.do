clear all
set more off
set scrollbufsize 2000000


global AVZ 	"D:\Labor_Econ_Term_Paper\SOEP\work_data"
global MY_IN_PATH "D:\Labor_Econ_Term_Paper\SOEP\work_data_2\6.8"
global MY_DO_FILES "D:\Labor_Econ_Term_Paper\SOEP\program\do"
global MY_LOG_OUT "D:\Labor_Econ_Term_Paper\SOEP\program\log"
global MY_OUT_DATA "D:\Labor_Econ_Term_Paper\SOEP\program\output"
global MY_OUT_TEMP "D:\Labor_Econ_Term_Paper\SOEP\program\temp"

use $MY_IN_PATH.dta ,clear 


*-------------------------------------------------------------------------------
*Rename and clean data
*-------------------------------------------------------------------------------
quiet foreach  year in 10 11 12 13 14 15 16 {
		rename emplst`year' employ_s`year'
		rename e11105`year' occupation`year'
		rename ijob1`year' wage_1_job`year'
		rename ijob2`year' wage_2_job`year'
		rename e11101`year' hrs_w`year'
		}

quiet rename gebjahr brth_year

foreach  year in 10 11 12 13 14 15 16 {
		gen t_inc`year' =  wage_1_job`year' + wage_2_job`year'
		gen hourly_inc`year' = t_inc`year' / hrs_w`year'
		}

foreach  year in 10 11 12 13 14 15 16 {
		 drop if employ_s`year' ==6  
		 drop if employ_s`year' < 0
		 recode employ_s`year' (1/4 = 1 "Employed") (5 = 0 "Unemployed"), generate(employ_`year')
		 }

foreach  year in 10 11 12 13 14 15 16 {
		 gen occ_change_`year' = 0
		 replace occ_change_`year' = 1 if jobch`year' == 4
		 }		

drop if employ_14 == 0
drop if t_inc14 == 0
drop if hourly_inc14 == 0
drop if missing( hourly_inc14 )

keep pid brth_year sex hourly_inc10 hourly_inc11 hourly_inc12 hourly_inc13 hourly_inc14 hourly_inc15 hourly_inc16 t_inc11 t_inc12 t_inc13 t_inc14 t_inc15 t_inc16 employ_10 employ_11 employ_12 employ_13 employ_14 employ_15 employ_16 hrs_w14 hrs_w15 hrs_w15 occ_change_10 occ_change_11 occ_change_12 occ_change_13 occ_change_14 occ_change_15 occ_change_16
		
save "$MY_OUT_DATA\cleaned", replace
