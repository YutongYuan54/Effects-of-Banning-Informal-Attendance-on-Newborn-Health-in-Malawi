
** Do-file for "Does Does a ban on informal health providers save lives? Evidence from Malawi"

set more off


cd "/Users/Jane/Desktop/Study/Undergradute/ECO403/Paper/Data files for submission"
use "birth attendants.dta", clear

ssc install outreg2

***********************
** LHS variables 
***********************

* Birth attended by a TBA
ge tba=m3g if m3g<9

* Birth attended by a formal sector provider
ge sba=doctor if doctor<9
replace sba=1 if nurse==1 
replace sba=1 if m3d==1 & (placeofbirth>=21 & placeofbirth <=33)

* Birth attended by a relative
ge relative=m3h
replace relative=. if m3h>1

* Birth attended by nobody
ge noone=m3n
replace noone=. if m3n>1

* Neonatal mortality
ge neomort=b6<=130

* Early neonatal mortality
ge earlymort=b6<=106

******************* 
** Other variables
******************* 

* Multiple birth

ge twin=.
replace twin=1 if b0>0 & b0<=3
replace twin=0 if b0==0

* First child
ge first=bord==1

* Young mother
ge young=ageatbirth<=18

* Religion:
gen _christian = (religion1)
replace _christian = 1 if religion2 ==1
replace _christian = 1 if religion3 ==1
replace _christian = 1 if religion4 ==1
replace _christian = 1 if religion5 ==1
gen _muslim = (religion6)
gen _othrel = (religion7)
replace _othrel = 1 if religion8 ==1 
replace _othrel = 1 if religion9 ==1 

* Maternal age at birth
recode ageatbirth (12/18=1 "<18") (19/24=2 "19-24") (25/29=3 "25-29") (30/34=4 "30-34") ///
 (35/39=5 "35-39") (40/44=6 "40-44") (45/49=7 "45-49"), gen(agegp)

tab agegp, g(agegp)

replace anteno=. if m14==99
replace m2a=. if m2a==9
replace m2b=. if m2b==9

* Spouse education
tab v701 if v701<8, g(educ2_)

* Ethnicity
ge tribe=v131
replace tribe=9 if tribe>9 & tribe<.
tab tribe, g(tribe)

* Historical TBA use

ge predata=1 if birthyr<=2007

global predata if predata==1

egen preclust=group(dhsclust predata)
bys preclust: egen tbause=mean(tba)
replace tbause=. if preclust==.

qui sum tbause, det
ge a=1 if tbause>=r(p75) &tbause<.
replace a=0 if tbause<r(p75)

bys dhsclust: egen high=mean(a)
bys dhsclust: egen tbahist=mean(tbause)

drop a 

label var tbahist "Historical prevalence of informal birth attendant use"

*  Year x month indicator

gen x = string(birthmonth,"%02.0f")
egen y=concat(x birthyr), p(-)
gen time = monthly(y, "MY")
format time %tmMonYY
drop x y

* Define Post variable

gen post=time>=tm(2008m1)

* Add marternal mortality rate
gen mmr = 0

replace mmr = 610 if birthyr == 2005
replace mmr = 566 if birthyr == 2006
replace mmr = 526 if birthyr == 2007
replace mmr = 493 if birthyr == 2008
replace mmr = 466 if birthyr == 2009
replace mmr = 444 if birthyr == 2010


**************
** FIGURES 
**************

* Distribution of clusters by historical TBA use 

qui sum tbause, det
local x=r(p50)
local y=r(p75)
local z=r(p90)

histogram tbahist, width(.05) disc frac /*addl*/ ///
 xtitle("Proportion of births") ///
 ytitle("Fraction") xline(`x', lpattern(dash) lstyle(foreground)) ///
 xline(`y', lpattern(dash) lstyle(foreground)) xline(`z', lpattern(dash) ///
 lstyle(foreground)) ///
 title("Informal birth attendant prevalence at baseline")  subtitle(" ")


** GRAPHS BY EXPOSURE

bys birthyr high: egen sba50 =mean(sba)
bys birthyr high: egen tba50 =mean(tba)

* Figures: Effect of the ban on provider use

twoway (connected tba50 birthyr if high==0 ,  ysc(r(0 .9)) ylabel(0(.2).9) ymtick(0(.2).9)) ///
 (connected tba50 birthyr if high==1), legend(order(1 "Low exposure" 2 "High exposure") ///
 rows(1))  xtitle("") ytitle("") xline(2007, lpattern(dot)) title("Use of informal birth attendants")  subtitle(" ")

twoway (connected sba50 birthyr if high==0 , ysc(r(0 1)) ylabel(0(.2)1) ymtick(0(.2)1)) ///
 (connected sba50 birthyr if high==1), legend(order(1 "Low exposure" 2 "High exposure") ///
 rows(1)) xtitle("") ytitle("") xline(2007, lpattern(dot)) title("Use of formal sector providers")  subtitle(" ")


** Distribution of facility quality

qui sum qindexmin, det
local x=r(p50)
local y=r(p75)

histogram qindexmin, disc frac xline(`x', lpattern(dash) lstyle(foreground)) ///
 xline(`y', lpattern(dash) lstyle(foreground)) ///
 xtitle("Quality score") ytitle("Fraction") title("Distribution of births by quality of nearest health facility")  subtitle(" ")


** Change in TBA use by baseline prevalence

recode tbahist (.01/.19999999=1 "1-20%") (.2/.3999999=2 "20-40%") (.4/.5999999=3 "40-60%") ///
(.6/.7999999=4 "60-80%") (.8/.9999999=5 "80-99%") (else=.), ge(prevalence)

graph bar (mean) tba, over(post, relabel(1 "Pre" 2 "Post")) over(prevalence) bargap(0) ytitle("Proportion") ///
 title("Change in the use of informal birth attendants by baseline distribution", size(medium)) subtitle(" ")
graph save Graph "Change in TBA use by baseline prevalence", replace



*****************
** Descriptives
*****************

* Table 1: Summary Statistics - all women in our regressions

preserve
duplicates drop dhsclust v002 v003, force
tabstat age1-age7 partner _christian _muslim _othrel educ1_1-educ1_4 tribe1-tribe9 educ2_1-educ2_4 toilet1 electricity2 ///
 urban2 mindist region1-region3 [aw=wgt], s(mean sd count) columns(stat)
restore


* Table 1b: Summary Statistics - all births in our regressions
tabstat ageatbirth bord twin male young first mindist anteno sba tba [aw=wgt], by(post) s(mean sd count) long nototal columns(stat)

 
 preserve
 
/* Drop births that took place in the month of or month preceding month of interview 
since infant may not have been exposed to the full neonatal period */

drop if birthyr==2010 & birthmonth>v006-2

tabstat neomort earlymort [aw=wgt], by(post) s(mean sd count) long nototal columns(stat)

restore


****************************************************************************
** Are high-exposure villages different from low-exposure villages?
****************************************************************************

ge unemploy=v731==0
replace unempl=. if v731==.
ge antetime=m13 if m13<=9

replace v151=v151-1
label define gender 0 "Male" 1 "Female"
label values v151 gender

tabstat ageatbirth twin male anteno if post==0 [aw=wgt], by(high) s(mean sd) columns(stat)
tabstat v201 v212 v511 _christian educ1_1 educ2_1 unemploy v151 v136 bicycle2 electricity2 radio2 wealth1 urban2 region1-region3 mindist qindexmin [aw=wgt], by(high) s(mean sd) columns(stat)

qui reg ageatbirth high if post==0, cl(sdistrict)
qui test high
outreg2 using high_vs_low.xls, replace  adds(Prob > F, `r(p)') 

foreach x of varlist twin male anteno  {
qui reg `x' high if post==0, cl(sdistrict)
qui test high
outreg2 using high_vs_low.xls, append adds(Prob > F, `r(p)') 
}
foreach x of varlist v201 v212 v511 _christian educ1_1 educ2_1 unemploy v151 v136 bicycle2 electricity2 radio2 wealth1 urban2 region1-region3 mindist qindexmin {
qui reg `x' high, cl(sdistrict)
qui test high
outreg2 using high_vs_low.xls, append adds(Prob > F, `r(p)') 
}



*******************
** Validity checks
*******************


* Parallel Trends

xi:areg tba I.high*time if post==0, abs(sdistrict) cl(sdistrict)
outreg2 using parallel_trends.xls, replace

xi:areg sba I.high*time if post==0, abs(sdistrict) cl(sdistrict)
outreg2 using parallel_trends.xls, append 


* Is treatment variable related to observable characteristics? 

global main I.high*post

qui xi:reg twin $main I.sdistrict*time, cl(sdistrict)
outreg2 using validity_check.xls, replace keep(post _Ihigh_1 _IhigXpost_1)

foreach var of varlist male first young v201 v212 v511 _christian educ1_1 educ2_1 unemploy v151 v136 bicycle2 electricity2 radio2 wealth1 urban2 mindist {
	qui xi:reg `var' $main  I.sdistrict*time, cl(sdistrict)
	outreg2 using validity_check.xls, append keep(post _Ihigh_1 _IhigXpost_1)
}



*********************************************
** Effect of the ban on birth attendant use
*********************************************

global control male bord twin educ1_2-educ1_4 partner i.tribe agegp2-agegp7 _christian _muslim _othrel educ2_2-educ2_4 mindist wealth2-wealth5 urban2 
global controlxpost male##post c.bord##post twin##post educ1_2##post educ1_3##post educ1_4##post partner##post tribe##post agegp##post _christian##post _muslim##post _othrel##post educ2_2##post educ2_3##post educ2_4##post post##c.mindist v190##post urban2##post 
global if if tbahist>0 & tbahist<1

foreach var of varlist tba sba relative noone  {
qui xi:areg `var' $main I.sdistrict, abs(time) cl(sdistrict)
outreg2 using use_of_`var'.xls, replace
}
	
foreach var of varlist tba sba relative noone  {
	
	qui xi:areg `var' $main $control I.sdistrict, abs(time) cl(sdistrict)
	outreg2 using use_of_`var'.xls, append
	
	qui xi:areg `var' $main  $controlxpost I.sdistrict, abs(time) cl(sdistrict)
	outreg2 using use_of_`var'.xls, append

	qui xi:reg `var' $main $controlxpost I.sdistrict*time, cl(sdistrict)
	outreg2 using use_of_`var'.xls, append

	qui xi:reg `var' $main $controlxpost I.sdistrict*time $if, cl(sdistrict)
	outreg2 using use_of_`var'.xls, append
	
	qui xi:areg `var' $main $controlxpost I.time, abs(dhsclust) cl(sdistrict)
	outreg2 using use_of_`var'.xls, append
}
**************************************************************************************
foreach var of varlist tba sba relative noone  {
qui xi:areg `var' $main I.sdistrict, abs(time) cl(sdistrict)
outreg2 using use_of_`var'.xls, replace
}
	
foreach var of varlist tba sba relative noone  {
	
	qui xi:areg `var' $main $control I.sdistrict, abs(time) cl(sdistrict)
	outreg2 using use_of_`var'.xls, append
	
	qui xi:areg `var' $main  $controlxpost I.sdistrict, abs(time) cl(sdistrict)
	outreg2 using use_of_`var'.xls, append

	qui xi:reg `var' $main $controlxpost I.sdistrict*time, cl(sdistrict)
	outreg2 using use_of_`var'.xls, append

	qui xi:reg `var' $main $controlxpost I.sdistrict*time $if, cl(sdistrict)
	outreg2 using use_of_`var'.xls, append
	
	qui xi:areg `var' $main $controlxpost I.time, abs(dhsclust) cl(sdistrict)
	outreg2 using use_of_`var'.xls, append
}
**************
** Robustness
**************

* Use baseline prevalence instead of binary exposure variable

qui xi:reg tba i.post*tbahist $controlxpost  I.sdistrict*time $if, cl(sdistrict)
outreg2 using treatment_is_continuous.xls, replace

foreach var of varlist sba relative noone  {
qui xi:reg `var' i.post*tbahist $controlxpost I.sdistrict*time $if, cl(sdistrict)
outreg2 using treatment_is_continuous.xls, append
}


* Did the ban change patterns of prenatal care?

qui xi:areg anteno $main, abs(sdistrict) cl(sdistrict)
outreg2 using prenatal_care.xls, replace keep(post _Ihigh_1 _IhigXpost_1)

foreach var of varlist anteno m2a m2b {
	
	qui xi:areg `var' $main $controlxpost I.sdistrict, abs(time) cl(sdistrict)
	outreg2 using prenatal_care.xls, append keep(post _Ihigh_1 _IhigXpost_1)
	
	qui xi:reg `var' $main $controlxpost I.sdistrict*time, cl(sdistrict)
	outreg2 using prenatal_care.xls, append keep(post _Ihigh_1 _IhigXpost_1)
	
	qui xi:reg `var' $main $controlxpost I.sdistrict*time $if, cl(sdistrict)
	outreg2 using prenatal_care.xls, append keep(post _Ihigh_1 _IhigXpost_1)
}



******************************************
** Effect of the ban on newborn mortality
******************************************

use "mortality.dta", clear

* Year x month dummies
ge year=birthyr
gen x = string(birthmonth,"%02.0f")
egen y=concat(x birthyr), p(-)
gen time = monthly(y, "MY")
format time %tmMonYY
drop x y


/* Drop births that took place in the month of or month preceding month of interview 
since infant may not have been exposed to the full neonatal period */

drop if time>tm(2010m4) & v006==6
drop if time>tm(2010m5) & v006==7
drop if time>tm(2010m6) & v006==8
drop if time>tm(2010m7) & v006==9
drop if time>tm(2010m8) & v006==10

** Mortality Graphs 
****************************************************************************************

***********************
** LHS variables 
***********************

* Birth attended by a TBA
ge tba=m3g if m3g<9

* Birth attended by a formal sector provider
ge sba=doctor if doctor<9
replace sba=1 if nurse==1 
replace sba=1 if m3d==1 & (placeofbirth>=21 & placeofbirth <=33)

* Birth attended by a relative
ge relative=m3h
replace relative=. if m3h>1

* Birth attended by nobody
ge noone=m3n
replace noone=. if m3n>1

* Neonatal mortality
ge neomort=b6<=130

* Early neonatal mortality
ge earlymort=b6<=106

******************* 
** Other variables
******************* 

* Multiple birth

ge twin=.
replace twin=1 if b0>0 & b0<=3
replace twin=0 if b0==0

* First child
ge first=bord==1

* Young mother
ge young=ageatbirth<=18

* Religion:
gen _christian = (religion1)
replace _christian = 1 if religion2 ==1
replace _christian = 1 if religion3 ==1
replace _christian = 1 if religion4 ==1
replace _christian = 1 if religion5 ==1
gen _muslim = (religion6)
gen _othrel = (religion7)
replace _othrel = 1 if religion8 ==1 
replace _othrel = 1 if religion9 ==1 

* Maternal age at birth
recode ageatbirth (12/18=1 "<18") (19/24=2 "19-24") (25/29=3 "25-29") (30/34=4 "30-34") (35/39=5 "35-39") (40/44=6 "40-44") (45/49=7 "45-49"), gen(agegp)

tab agegp, g(agegp)

replace anteno=. if m14==99
replace m2a=. if m2a==9
replace m2b=. if m2b==9

* Spouse education
tab v701 if v701<8, g(educ2_)

* Ethnicity
ge tribe=v131
replace tribe=9 if tribe>9 & tribe<.
tab tribe, g(tribe)

* Historical TBA use

ge predata=1 if birthyr<=2007

global predata if predata==1

egen preclust=group(dhsclust predata)
bys preclust: egen tbause=mean(tba)
replace tbause=. if preclust==.

qui sum tbause, det
ge a=1 if tbause>=r(p75) &tbause<.
replace a=0 if tbause<r(p75)

bys dhsclust: egen high=mean(a)
bys dhsclust: egen tbahist=mean(tbause)

drop a 

label var tbahist "Historical prevalence of informal birth attendant use"

* Define Post variable

gen post=time>=tm(2008m1)

* Add marternal mortality rate
gen mmr = 0
replace mmr = 610 if birthyr == 2005
replace mmr = 566 if birthyr == 2006
replace mmr = 526 if birthyr == 2007
replace mmr = 493 if birthyr == 2008
replace mmr = 466 if birthyr == 2009
replace mmr = 444 if birthyr == 2010
****************************************************************************************

bys year high: egen neoy=mean(neomort)
bys year high: egen earlyy =mean(earlymort)


twoway (connected earlyy year if high==0, ysc(r(0 .05)) ylabel(0(.01).05) ymtick(0(.01).05)) (connected earlyy year if high==1), legend(order(1 "Low exposure" 2 "High exposure") rows(1)) xlabel(2001 (1) 2010) xtitle("") ytitle("") xline(2007, lpattern(dot)) legend(off) title("A. Within the first week") subtitle(" ")

twoway (connected neoy year if high==0, ysc(r(0 .06)) ylabel(0(.01).06) ymtick(0(.01).06)) (connected neoy year if high==1), legend(order(1 "Low exposure" 2 "High exposure") rows(1)) xlabel(2001 (1) 2010)  xtitle("") ytitle("") xline(2007, lpattern(dot)) title("B. Within the first month")  subtitle(" ")

//gr combine "Early neonatal mortality" "Neonatal mortality" , col(1) hole(3) graphregion(margin(l=10 r=10)) iscale(.6) ysize(6) xsize(5) title("Probability of a newborn death", size(medium))


* Parallel Trends (Mortality)

xi:areg neomort I.high*time if post==0 , abs(sdistrict) cl(sdistrict)
outreg2 using parallel_trends.xls, append
xi:areg earlymort I.high*time if post==0, abs(sdistrict) cl(sdistrict)
outreg2 using parallel_trends.xls, append
*******
*******
xi:areg neomort I.high*time if post==0 , abs(sdistrict) cl(sdistrict)
outreg2 using parallel_trends.xls, append
xi:areg earlymort I.high*time if post==0, abs(sdistrict) cl(sdistrict)
outreg2 using parallel_trends.xls, append

*******
*******
* Mortality results

global main high##post
global control2 male first young educ1_2-educ1_4 partner i.tribe _christian _muslim _othrel educ2_2-educ2_4 mindist wealth2-wealth5 urban2 
global control2xpost male##post first##post young##post educ1_2##post educ1_3##post educ1_4##post partner##post tribe##post _christian##post _muslim##post _othrel##post educ2_2##post educ2_3##post educ2_4##post c.mindist##post v190##post urban2##post 

foreach var of varlist earlymort neomort {
qui xi:areg `var' $main I.sdistrict, abs(time) cl(sdistrict)
outreg2 using table_`var'.xls, replace 
}

foreach var of varlist earlymort  {

	qui xi:areg `var' $main $control2 I.sdistrict, abs(time) cl(sdistrict)
	outreg2 using table_`var'.xls, append 
	
	qui xi:areg `var' $main  $control2xpost I.sdistrict, abs(time) cl(sdistrict)
	outreg2 using table_`var'.xls, append 
	
	qui xi:reg `var' $main $control2xpost I.sdistrict*time, cl(sdistrict)
	outreg2 using table_`var'.xls, append 
	
	qui xi:reg `var' $main $control2xpost I.sdistrict*time if tbahist>0 & tbahist<1, cl(sdistrict)
	outreg2 using table_`var'.xls, append 
	
	qui xi:areg `var' $main $control2xpost I.time, abs(dhsclust) cl(sdistrict)
	outreg2 using table_`var'.xls, append 
	
	qui xi:reg `var' i.post*tbahist $control2xpost I.sdistrict*time if tbahist>0 & tbahist<1, cl(sdistrict)
	outreg2 using treatment_is_continuous.xls, append

}

* Robustness

* 5- year window 

foreach var of varlist earlymort neomort {
qui xi:areg `var' $main  $control2xpost I.sdistrict if year>=2005, abs(time) cl(sdistrict)
outreg2 using robustness_5yr_recall_`var'.xls, replace 
}

foreach var of varlist earlymort neomort {

	qui xi:reg `var' $main $control2xpost I.sdistrict*time if year>=2005 & tbahist>0 & tbahist<1, cl(sdistrict)
	outreg2 using robustness_5yr_recall_`var'.xls, append 
}


* Offsetting mortality effect of relative-attended births?

qui xi:reg earlymort $main $control2xpost I.sdistrict*time if tbahist>0 & tbahist<1 & v467d==1, cl(sdistrict)
outreg2 using distance_is_a_problem.xls, append keep(1.high#1.post)
qui xi:reg earlymort $main $control2xpost I.sdistrict*time if tbahist>0 & tbahist<1 & v467d==2, cl(sdistrict)
outreg2 using distance_is_a_problem.xls, append keep(1.high#1.post)

qui xi:reg neomort $main $control2xpost I.sdistrict*time if tbahist>0 & tbahist<1 & v467d==1, cl(sdistrict)
outreg2 using distance_is_a_problem.xls, append keep(1.high#1.post)
qui xi:reg neomort $main $control2xpost I.sdistrict*time if tbahist>0 & tbahist<1 & v467d==2, cl(sdistrict)
outreg2 using distance_is_a_problem.xls, append keep(1.high#1.post)


* Does mortality depend on facility quality?

ge highq=0 if qindexmin<.
qui sum qindexmin, det
replace highq=1 if qindexmin>=r(p75) & qindexmin<.

foreach x of numlist 0/1 {
foreach var of varlist earlymort neomort {
qui xi:areg `var' $main I.sdistrict if highq==`x', abs(time) cl(sdistrict)
outreg2 using quality_`var'`x'.xls, replace 
}
}

foreach x of numlist 0/1 {
foreach var of varlist earlymort neomort {

	qui xi:areg `var' $main $control2 I.sdistrict if highq==`x', abs(time) cl(sdistrict)
	outreg2 using quality_`var'`x'.xls, append 
	
	qui xi:areg `var' $main  $control2xpost I.sdistrict if highq==`x', abs(time) cl(sdistrict)
	outreg2 using quality_`var'`x'.xls, append 
	
	qui xi:reg `var' $main $control2xpost I.sdistrict*time if highq==`x', cl(sdistrict)
	outreg2 using quality_`var'`x'.xls, append 
	
	qui xi:reg `var' $main $control2xpost I.sdistrict*time if highq==`x' & tbahist>0 & tbahist<1 , cl(sdistrict)
	outreg2 using quality_`var'`x'.xls, append 
	
	qui xi:areg `var' $main $control2xpost I.time if highq==`x', abs(dhsclust) cl(sdistrict)
	outreg2 using quality_`var'`x'.xls, append
}
}


*********************************
** Triple difference regressions 
*********************************

use "Mortality_triple difference.dta", clear

* Child death indicator

ge mort=earlymort==1
replace mort=1 if mort2yr==1

set matsize 800

global ddd high##post##treated
global control2 male first young educ1_2-educ1_4 partner i.tribe agegp2-agegp7 _christian _muslim _othrel educ2_2-educ2_4 mindist wealth2-wealth5 urban2 
global control2xpost male##post first##post young##post educ1_2##post educ1_3##post educ1_4##post partner##post tribe##post _christian##post _muslim##post _othrel##post educ2_2##post educ2_3##post educ2_4##post c.mindist##post v190##post urban2##post 
global control2xtreated male##treated first##treated young##treated educ1_2##treated educ1_3##treated educ1_4##treated partner##treated tribe##treated _christian##treated _muslim##treated _othrel##treated educ2_2##treated educ2_3##treated educ2_4##post c.mindist##post v190##post urban2##post 

foreach var of varlist mort {
qui xi:areg `var' $ddd $control2 I.sdistrict, abs(time) cl(sdistrict)
outreg2 using triplediff_`var'.xls, replace 
}

foreach var of varlist mort {

	qui xi:areg `var' $ddd  $control2xpost $control2xtreated I.sdistrict, abs(time) cl(sdistrict)
	outreg2 using triplediff_`var'.xls, append 
	
	qui xi:reg `var' $ddd $control2xpost $control2xtreated I.sdistrict*time, cl(sdistrict)
	outreg2 using triplediff_`var'.xls, append 
	
	qui xi:reg `var' $ddd $control2xpost $control2xtreated I.sdistrict*time if tbahist>0 & tbahist<1, cl(sdistrict)
	outreg2 using triplediff_`var'.xls, append 

}


** Quadruple difference

foreach x of numlist 0/1 {
foreach var of varlist mort {
	qui xi:areg `var' $ddd $control2 I.sdistrict if highq==`x', abs(time) cl(sdistrict)
	outreg2 using quadruplediff_`var'`x'.xls, replace 
}
}

foreach x of numlist 0/1 {
foreach var of varlist mort {
	
	qui xi:areg `var' $ddd  $control2xpost $control2xtreated I.sdistrict if highq==`x', abs(time) cl(sdistrict)
	outreg2 using quadruplediff_`var'`x'.xls, append 
	
	qui xi:reg `var' $ddd $control2xpost $control2xtreated I.sdistrict*time if highq==`x', cl(sdistrict)
	outreg2 using quadruplediff_`var'`x'.xls, append 
	
	qui xi:reg `var' $ddd $control2xpost $control2xtreated I.sdistrict*time if tbahist>0 & tbahist<1 & highq==`x', cl(sdistrict)
	outreg2 using quadruplediff_`var'`x'.xls, append 

}
}




