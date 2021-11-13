clear all

if c(username)=="victo"{
	global raw_dta "C:\Users\victo\OneDrive\Documents\Econometrics_2\Raw_data"
	global analytical_dta "C:\Users\victo\OneDrive\Documents\Econometrics_2\Analytical_data"
	global do_file "C:\Users\victo\OneDrive\Documents\Econometrics_2\Do_file"
	global logs "C:\Users\victo\OneDrive\Documents\Econometrics_2\Log_file"
	global tables "C:\Users\victo\OneDrive\Documents\Econometrics_2\Tables"
}
****************************************
*Project: Problem Set 2
*Done by: Victor Kilanko
*****************************************
cd "${raw_dta}"
	use "offenses_known_yearly_1960_2018.dta", clear // using my saved data in raw data folder
	
*****Question 5: Replicate Table 2********************************************************************
gen Homicide = actual_murder + actual_manslaughter
collapse(sum) Homicide actual_robbery_total actual_robbery_with_a_gun actual_burg_total actual_theft_total actual_mtr_veh_theft_total actual_assault_aggravated, by(year state)

destring state, replace
encode state, gen (statefip)

drop if year<2000

save fbi_ucr.dta, replace

****Merge the FBI UCR data with ACS data
use "usa_00004.dta", clear //clean the data first
gen Black_male_15_24= 1 if (racblk==2 & sex==1 & (age>=15 & age <=24))
gen Black_male_25_44= 1 if (racblk==2 & sex==1 & (age>=25 & age <=44))
gen White_male_15_24= 1 if (racwht==2 & sex==1 & (age>=15 & age <=24))
gen White_male_25_44= 1 if (racwht==2 & sex==1 & (age>=25 & age <=44))
gen unemployment =1 if occ2010==9920
collapse(sum) Black_male_15_24 Black_male_25_44 White_male_15_24 White_male_25_44 unemployment (median) hhincome, by (year statefip)
merge m:m state year using "fbi_ucr.dta"
keep if _merge==3
drop _merge
replace state = proper(state) 
save ucr_acsp2.dta, replace

*****Merge the Castle_Doctrine data with this
merge m:m state year using "Castle_Doctrine.dta"
keep if _merge==3
drop _merge
save castle_paper.dta, replace
foreach x of varlist Black_male_15_24 Black_male_25_44 White_male_15_24 White_male_25_44 unemployment {
	gen `x'_percent = `x'/population*100000
	label var `x'_percent "`x'_percent"
}

drop Black_male_15_24 Black_male_25_44 White_male_15_24 White_male_25_44 unemployment

****Convert the crime data to per 100,000
foreach x of varlist Homicide actual_robbery_total actual_robbery_with_a_gun actual_burg_total actual_theft_total actual_mtr_veh_theft_total actual_assault_aggravated {
	gen `x'_000 = `x'/population*100000
	label var `x'_000 "`x'in_100000"
}

gen Proportion_of_robberies_with_gun = actual_robbery_with_a_gun_000/100
drop Homicide actual_robbery_total actual_robbery_with_a_gun actual_burg_total actual_theft_total actual_mtr_veh_theft_total actual_assault_aggravated actual_robbery_with_a_gun_000

****Rename variables to fit the paper's names
rename unemployment_percent Unemployment_rate
rename poverty Poverty_rate
rename exp_subsidy Government_subsidy_per_capita
rename exp_pubwelfare Government_welfare_per_capita 
rename prisoner Prisoners_per_100000_residents
rename police Police_per_100000_residents
rename actual_robbery_total_000 Robberies_per_100000_population
rename Homicide_000 Homicides_per_100000_population
*rename actual_robbery_with_a_gun_000 Proportion_of_robberies_with_gun
rename actual_burg_total_000 Burglary_per_100000_population
rename actual_theft_total_000 Larceny_per_100000_population
rename actual_mtr_veh_theft_total_000 Motr_theft_per_100000_population
rename actual_assault_aggravated_000 Aggr_aslt_per_100000_population
rename hhincome Median_household_income
save castle_paper.dta, replace

bysort sid: egen pop_wt=mean(population) // population weight	
summarize 
outreg2 using "Table2a.tex", sum(log) replace eqkeep (mean) title (Descriptive Statistics) cti(Mean-Unweighted)

*****************Reproduce Figure 1 & 2***********************************************************************************Done
cd "${raw_dta}"
use "castle_paper.dta", clear

gen lhom = log(Homicides_per_100000_population)
gen lpop=log(population)
gen lpolice=log(Police_per_100000_residents)
gen lhhincome=log(Median_household_income)
gen lprisoner=log(Prisoners_per_100000_residents)
gen lsubsidy=log(Government_subsidy_per_capita)
gen lwelfare=log(Government_welfare_per_capita)
gen lmotor=log(Motr_theft_per_100000_population)
gen llarceny = log(Larceny_per_100000_population)

*egen id= group(state)
xtset sid year

****Figure 1a- Homicide graph for Florida
gen homflo = 1 if statefip ==12
replace homflo = 0 if statefip !=12
gen Florida_state = lhom if homflo==1

egen bottomline = mean(lhom), by (year cdl)
gen statecontrol=bottomline if cdl==0
egen state_control = mean(statecontrol), by (year)
*gen control_2 = state_control if cdl==0
twoway (tsline Florida_state) (tsline state_control), title(2005-Florida)
*collapse(mean) Homicides_per_100000_population, by (cdl year)
graph save FLO, replace

****Figure 1b- Homicide graph for Alabama, Alaska, Arizona, Georgia, Indiana, Kansas, Kentucky, Louisiana, Michigan, Mississippi, Oklahoma, South Carolina, South Dakota
gen hommore = 1 if statefip == 1 | statefip ==2 | statefip== 4 | statefip == 13  | statefip ==18 | statefip ==20 | statefip ==21 | statefip ==22 | statefip ==26 | statefip ==28 | statefip ==40 | statefip ==45 | statefip ==46
*replace hommore = 0 if statefip != 1 | 2 | 4 | 13  | 18 | 20 | 21 | 22 | 26 | 28 | 40 | 45 | 46
gen Alabama_1b = lhom if hommore==1
egen Alabama_1bm = mean(Alabama_1b), by (year)
twoway (tsline Alabama_1bm) (tsline state_control), title(2006 States)
graph save ALA_1b, replace

****Figure 1c- Homicide graph for Missouri, North Dakota, Tennessee, Texas
gen hommntt = 1 if statefip ==29 | statefip ==38 | statefip == 47 | statefip== 48
*replace hommntt = 0 if statefip !=29 & statefip !=38 & statefip != 47 & statefip != 48
gen Missouri_NDTNTX = lhom if hommntt==1
egen Missouri_NDTNTXm  = mean(Missouri_NDTNTX), by (year)
twoway (tsline Missouri_NDTNTXm) (tsline state_control), title(2007 States)
graph save MISS1c, replace

****Figure 1d- Homicide graph for Ohio, West Virginia
gen homowv = 1 if statefip ==39 | statefip== 54
*replace homowv = 0 if statefip !=39 & statefip!= 54
gen Ohio_WV = lhom if homowv==1
egen Ohio_WVm  = mean(Ohio_WV), by (year)
twoway (tsline Ohio_WVm) (tsline state_control), title(2008 States)
graph save OH1d, replace

****Figure 1e- Homicide graph for Montana
gen hommon = 1 if statefip ==30
replace hommon = 0 if statefip !=30
gen Montana_state = lhom if hommon==1
twoway (tsline Montana_state) (tsline state_control), title(2009-Montana)
graph save MON1e, replace

************Figure 2- Regression Plot
xtset sid year

*My dependent variables
local lpolice lprisoner lsubsidy lwelfare lhhincome Poverty_rate Unemployment_rate Black_male_15_24_percent White_male_15_24_percent Black_male_25_44_percent White_male_25_44_percent

gen CDL = 0
replace CDL = 1 if cdl >0
gen ST_YR = state*year
gen CDL_year = CDL*year

*Creating leads/lags for my regression plot. I will be showing how homicide rate varies over time
gen postyear = year-effyear 
replace postyear = 0 if postyear == .
gen post = 1 if postyear >=1  // this is the post variable
replace post = 0 if postyear < 1

gen lag1=0
replace lag1=1 if postyear ==-4 | postyear ==-3

gen lag2=0
replace lag2=1 if postyear ==-2 | postyear == -1

gen treat_year = 1 if postyear == 0

gen dd=0
replace dd = 1 if postyear ==0 

gen dd1=0
replace dd1=1 if postyear == 1 | postyear ==2

gen dd2 = 0
replace dd2 = 1 if postyear >= 3

*****Absorbing how regions can affect my homicide rate through the region by year fe
forvalues i=2000/2010{
	gen year`i'=(year==`i')
	gen r`i'1=year`i'*northeast
	gen r`i'2=year`i'*midwest
	gen r`i'3=year`i'*south
	gen r`i'4=year`i'*west
	drop year`i'
}		 

*Creating my control for region and year fe
global region r20001-r20104

*Other variables needed
global crime llarceny lmotor 

***Regression plot for the unweighted OLS
xi: xtreg lhom i.year $region `local' lag1 lag2 dd dd1 dd2,cluster(sid) fe
coefplot, keep(lag1 lag2 dd dd1 dd2) connect(1) vertical title(Unweighted OLS) noci
graph save U-OLS, replace

****Regression plot for the weighted OLS 
bysort sid: egen pop_wt=mean(population) // population weight
xi: xtreg lhom i.year $region `local' lag1 lag2 dd dd1 dd2  [aweight=pop_wt],cluster(sid) fe
coefplot, keep(lag1 lag2 dd1 dd2) connect(1) vertical title(Weighted OLS) noci
graph save W-OLS, replace

****Regression plot for the negative binomial 
xi: nbreg Homicides_per_100000_population i.year $region `local' lag1 lag2 dd dd1 dd2, exposure(population) cluster(sid)
coefplot, keep(lag1 lag2 dd dd1 dd2) connect(1) vertical title(Negative Binomial) noci
graph save NE_BI, replace

************Table 5 **********************
gen treat_post = post*CDL

*My dependent variables
local lpolice lprisoner lsubsidy lwelfare lhhincome Poverty_rate Unemployment_rate Black_male_15_24_percent White_male_15_24_percent Black_male_25_44_percent White_male_25_44_percent

*******Unweighted OLS
xi: xtreg lhom CDL treat_post i.year, cluster(sid) fe 
outreg2 using "Table5a.tex", replace keep (CDL) title(OLS-Unweighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region, cluster(sid) fe 
outreg2 using "Table5a.tex", append keep (CDL) title(OLS-Unweighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region `local', cluster(sid) fe  
outreg2 using "Table5a.tex", append keep (CDL) title(OLS-Unweighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region `local' pre2_cdl, cluster(sid) fe  
outreg2 using "Table5a.tex", append keep (CDL pre2_cdl) title(OLS-Unweighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region `local' $crime, cluster(sid) fe 
outreg2 using "Table5a.tex", append keep (CDL) title(OLS-Unweighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region `local' i.sid, cluster(sid) fe  
outreg2 using "Table5a.tex", append keep (CDL) title(OLS-Unweighted) eqkeep (mean N)


*******Weighted OLS
xi: xtreg lhom CDL treat_post i.year [aweight=pop_wt], cluster(sid) fe 
outreg2 using "Table5b.tex", replace keep (CDL) title(OLS-Weighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region [aweight=pop_wt], cluster(sid) fe 
outreg2 using "Table5b.tex", append keep (CDL) title(OLS-Weighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region `local' [aweight=pop_wt], cluster(sid) fe  
outreg2 using "Table5b.tex", append keep (CDL) title(OLS-Weighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region `local' pre2_cdl [aweight=pop_wt], cluster(sid) fe  
outreg2 using "Table5b.tex", append keep (CDL pre2_cdl) title(OLS-Weighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region `local' $crime [aweight=pop_wt], cluster(sid) fe 
outreg2 using "Table5b.tex", append keep (CDL) title(OLS-Weighted) eqkeep (mean N)
xi: xtreg lhom CDL treat_post i.year $region `local' i.sid [aweight=pop_wt], cluster(sid) fe  
outreg2 using "Table5b.tex", append keep (CDL) title(OLS-Weighted) eqkeep (mean N)

