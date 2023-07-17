********************************************************************************
* SP6015 
* Do-file Demo Mgg#5
* - Multinomial Logit Regression
* - Ordered Logit Regression
* - Poisson Regression
* By Adenantera Dwicaksono, PhD.
********************************************************************************

********** SETUP **********
clear all
set more off
version 12

********************************************************************************
* A. Multinomial Logit Regression
********************************************************************************

*****************************  
* 1.DATA DESCRIPTION
*****************************  

* Data Set comes from :
* J. A. Herriges and C. L. Kling, 
* "Nonlinear Income Effects in Random Utility Models", 
* Review of Economics and Statistics, 81(1999): 62-72

* The data are given as a combined observation with data on all 4 choices.
* This will work for multinomial logit program.
* For conditional logit will need to make a new data set which has
* four separate entries for each observation as there are four alternatives. 

* Filename: NLDATA.ASC
* Format: Ascii
* Number of Observations: 1182
* Each observations appears over 3 lines with 4 variables per line 
* so 4 x 1182 = 4728 observations 
* Variable Number and Description
* 1	Recreation mode choice. = 1 if beach, = 2 if pier; = 3 if private boat; = 4 if charter
* 2	Price for chosen alternative
* 3	Catch rate for chosen alternative
* 4	= 1 if beach mode chosen; = 0 otherwise
* 5	= 1 if pier mode chosen; = 0 otherwise
* 6	= 1 if private boat mode chosen; = 0 otherwise
* 7	= 1 if charter boat mode chosen; = 0 otherwise
* 8	= price for beach mode
* 9	= price for pier mode
* 10	= price for private boat mode
* 11	= price for charter boat mode
* 12	= catch rate for beach mode
* 13	= catch rate for pier mode
* 14	= catch rate for private boat mode
* 15	= catch rate for charter boat mode
* 16	= monthly income

*****************************  
* 2.DATA EXPLORATION
*****************************  

* Method to read in depends on model used

/* Data are on fishing mode: 1 beach, 2 pier, 3 private boat, 4 charter
   Data come as one observation having data for all 4 modes.
   Both alternative specific and alternative invariant regresssors.
*/

// Import data from raw format using infile
infile mode price crate dbeach dpier dprivate dcharter pbeach ppier /*
   */ pprivate pcharter qbeach qpier qprivate qcharter income /*
   */ using nldata.asc

// Crate income variable per $1000
gen ydiv1000 = income/1000

// Assign value label 
label define modetype 1 "beach" 2 "pier" 3 "private" 4 "charter"
label values mode modetype

//Summarize the data
summarize
sort mode
by mode: summarize

summarize ydiv100 pbeach ppier pprivate pcharter qbeach qpier /* 
    */ qprivate qcharter dbeach dpier dprivate dcharter
sort mode
by mode: summarize ydiv100 pbeach ppier pprivate pcharter qbeach qpier /* 
    */ qprivate qcharter dbeach dpier dprivate dcharter

*****************************  
* 3.MULTINOMIAL LOGIT: ALTERNATIVE-INVARIANT REGRESSOR/CASE SPESIFIC REGRESSORS
*****************************  	
	
*** (3A) Estimate the model

* Data are already in form for mlogit

* The following gives MNL column of
mlogit mode ydiv1000, baseoutcome(1)  nolog

mlogit mode ydiv1000, rr baseoutcome(1)  nolog

*** (3B) Calculate the marginal effects

quietly mlogit mode ydiv1000, baseoutcome(1)
* Predict by default gives the probabilities
* As check compare predicted to actual probabilities

predict p1_beach p2_pier p3_private p4_charter
summarize p1_beach p2_pier p3_private p4_charter

* Compute marginal effects using margin command

margins, predict(pr outcome(2)) dydx(ydiv1000)
margins, predict(pr outcome(3)) dydx(ydiv1000)
margins, predict(pr outcome(4)) dydx(ydiv1000)

* Alternative to compute marginal effects:
* Better is to evaluate marginal effect for each observation and average
* The following calculates marginal effects using noncalculus methods 
* by comparing the predicted probability before and after change in x
* Here consider small change of 0.0001 - then multiply by 1000
* So should be similar to using calculus methods.

replace ydiv1000 = ydiv1000 + 0.0001
predict p1new p2new p3new p4new
gen dp1dy = 10000*(p1new - p1_beach)
gen dp2dy = 10000*(p2new - p2_pier)
gen dp3dy = 10000*(p3new - p3_private)
gen dp4dy = 10000*(p4new - p4_charter)

* The computed marginal effects follow. 
sum dp1dy dp2dy dp3dy dp4dy

/*
Note on interpreatation:

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
       dp1dy |      1,182    .0001549    .0015919  -.0042468   .0027567
       dp2dy |      1,182   -.0207849    .0046004  -.0278652  -.0067055
       dp3dy |      1,182    .0318045    .0014852   .0280142   .0336766
       dp4dy |      1,182   -.0111929    .0041308  -.0190735  -.0026822

An increase of $1000 in monthly income is associated with changes of 0.0001,
-0.021, 0.033, -0.011 in the probability of fishing from a beach, a pier, 
a private boat, and a charter boat.
	   
*/

*****************************  
* 4.MULTINOMIAL LOGIT: ALTERNATIVE-SPECIFIC REGRESSOR
*****************************  	

*** (4A) Estimate the model

// Import data from raw format using infile
clear all

infile mode price crate dbeach dpier dprivate dcharter pbeach ppier /*
   */ pprivate pcharter qbeach qpier qprivate qcharter income /*
   */ using nldata.asc

// Create income variable per $1000
gen ydiv1000 = income/1000

* Data are one entry per individual
* Need to reshape to 4 observations per individual - one for each alternative
* Use reshape to do this which also creates variable (see below)
*   alternatv = 1 if beach, = 2 if pier; = 3 if private boat; = 4 if charter
gen id = _n
gen d1 = dbeach
gen p1 = pbeach
gen q1 = qbeach
gen d2 = dpier
gen p2 = ppier
gen q2 = qpier
gen d3 = dprivate
gen p3 = pprivate
gen q3 = qprivate
gen d4 = dcharter
gen p4 = pcharter
gen q4 = qcharter
describe
summarize

reshape long d p q, i(id) j(alterntv)
* This automatically creates alterntv = 1 (beach), ... 4 (charter)
describe
summarize

label var d "Mode of fishing"
label var p "Price"
label var q "Catch rate"

* Run Alternative specific regression
asclogit d p q, case(id) alternatives(alterntv) casevars(ydiv1000) ///
basealternative(1) nolog

*** (4B) Calculate the marginal effects

//Compute predicted probabilities Pr(y|p) from the models
predict pinitial

* Now compute marginal effects
* Consider in turn a change in each price and catch rate 
* Change catch rate by 0.001 and then multiply by 1000

* Change p1: price beach
replace p = p + 1 if alterntv==1 // Chang x (price) for 1 unit 
predict pnewp1 //computed predicted probabilities Pr(Y|p')
gen mep1 = 100*(pnewp1 - pinitial) // computed differences in Y given the change in p
replace p = p - 1 if alterntv==1 //change back to its originial values

* Change p2: price pier
replace p = p + 1 if alterntv==2
predict pnewp2 
gen mep2 = 100*(pnewp2 - pinitial)
replace p = p - 1 if alterntv==2

* Change p3: price private boat
replace p = p + 1 if alterntv==3
predict pnewp3 
gen mep3 = 100*(pnewp3 - pinitial)
replace p = p - 1 if alterntv==3

* Change p4: price charter boat
replace p = p + 1 if alterntv==4
predict pnewp4 
gen mep4 = 100*(pnewp4 - pinitial)
replace p = p - 1 if alterntv==4

* Change q1: catch rate beach
replace q = q + 0.001 if alterntv==1
predict pnewq1 
gen meq1 = 1000*(pnewq1 - pinitial)
replace q = q - 0.001 if alterntv==1

* Change q2: catch rate pier
replace q = q + 0.001 if alterntv==2
predict pnewq2 
gen meq2 = 1000*(pnewq2 - pinitial)
replace q = q - 0.001 if alterntv==2

* Change q1: catch rate private boat
replace q = q + 0.001 if alterntv==3
predict pnewq3 
gen meq3 = 1000*(pnewq3 - pinitial)
replace q = q - 0.001 if alterntv==3

* Change q1: catch rate charter boat
replace q = q + 0.001 if alterntv==4
predict pnewq4 
gen meq4 = 1000*(pnewq4 - pinitial)
replace q = q + 0.001 if alterntv==4

sort alterntv
by alterntv: sum pinitial mep1 mep2 mep3 mep4 meq1 meq2 meq3 meq4 

*Note for interpretation:

/*
-> alterntv = 1

    Variable |        Obs        Mean    Std. Dev.       Min        Max
-------------+---------------------------------------------------------
    pinitial |      1,182    .1133672    .1285041   3.79e-10    .616194
        mep1 |      1,182   -.2095802    .1946469  -.6278992  -9.40e-10
        mep2 |      1,182    .0905259    .1548809          0    .623852
        mep3 |      1,182    .0552965    .0475004   6.93e-10   .2934963
        mep4 |      1,182    .0644468    .0503129   2.61e-10   .1730099
-------------+---------------------------------------------------------
        meq1 |      1,182    .0300619     .027827   1.36e-10   .0894666
        meq2 |      1,182   -.0129272    .0220644  -.0888407          0
        meq3 |      1,182   -.0079174    .0068033  -.0418425          0
        meq4 |      1,182   -.0092158    .0072033  -.0247359          0

Interpreatation:
An increase of $100 in the price of beach fishing leads to a decrease of 0.209 
in the probability of fishing and an increase of 0.09, 0.055, and 0.064, 
respectively, in the probability of fishing from a beach, a pier, a private boat, 
and a charter boat.

An increase of 1-unit change in the catch rate of beach fishing leads to an 
increase of 0.03 in the probability of fishing and a decrease of 0.012, 0.007, 
and 0.009 respectively, in the probability of fishing from a beach, a pier, 
a private boat, and a charter boat.
		
*/

********************************************************************************
* B. Ordered Logit Regressions
********************************************************************************

********** DATA DESCRIPTION **********
* Hipothetical data from https://stats.idre.ucla.edu/stat/data/ologit.dta
* Variables:
* - apply (coded 0, 1, 2), that we will use as our outcome variable
*	(0 = unlikely; 1: somewhat likely; 2: very likely
* - pared, which is a 0/1 variable indicating whether at least one parent 
*   has a graduate degree
* - public, which is a 0/1 variable where 1 indicates that the undergraduate 
*	institution is public and 0 private
* - gpa: which is the student’s grade point average

//Open dataset
use college.dta, clear

// summarize the data
tab apply

tab apply pared

tab apply public

summarize gpa

table apply, cont(mean gpa sd gpa)

// Run ordered logit legression

ologit apply pared public gpa, nolog

// Run ordered logit legression and report regressions as odd-ratio

ologit apply pared public gpa, or nolog

/*


Ordered logistic regression                     Number of obs     =        400
                                                LR chi2(3)        =      24.18
                                                Prob > chi2       =     0.0000
Log likelihood = -358.51244                     Pseudo R2         =     0.0326

------------------------------------------------------------------------------
       apply | Odds Ratio   Std. Err.      z    P>|z|     [95% Conf. Interval]
-------------+----------------------------------------------------------------
       pared |   2.850982   .7577601     3.94   0.000      1.69338    4.799927
      public |   .9430059   .2808826    -0.20   0.844     .5259888    1.690645
         gpa |   1.851037   .4824377     2.36   0.018      1.11062    3.085067
-------------+----------------------------------------------------------------
       /cut1 |   2.203323   .7795353                      .6754621    3.731184
       /cut2 |   4.298767   .8043147                       2.72234    5.875195
------------------------------------------------------------------------------

Interpretation:
- The odd of a person whose one of the parents had graduate degree to choose 
"somewhat likely" over "unlikely" was 2.8 time
- The odd of a person with a 1 point increase in GPA to choose "somewhat likely" 
over "unlikely" was 1.8 time

*/

// We need to test the proportional odds assumption.
/* 
There are two tests that can be used to do so.  
First, we need to download a user-written command called omodel (type search omodel).
That performs a likelihood ratio test, with a null hypothesis that there is no 
difference in the coefficients between models (we “hope” to get a non-significant 
result). second, The brant command performs a Brant test. 
As the note at the bottom of the output indicates, we also “hope” that these 
tests are non-significant.  
The brant command, is part of the spost add-on and can be obtained 
by typing search spost.

*/

//Run omodel
omodel logit apply pared public gpa

// We can perform Brant test by running ologit regression first followed by 
// brant command
ologit apply pared public gpa, nolog

brant, detail

// As mentioned in the lecture notes, we can choose to use mlogit intead if the
// parallel regression assumption is violated
mlogit apply pared public gpa, nolog

//Or we can choose to generalized ologit
gologit2 apply pared public gpa


********************************************************************************
* C. Poisson Regression
********************************************************************************


********** DATA DESCRIPTION **********

* Data Set comes from :
* 2003 U.S. Medical Expenditure Panel Survey (MEPS)
* Sample of Medicare population aged 65 and higher 
* Filename: mus17data.dta
* Number of Observations: 3,677
* Variables:
*	- docvis = annual number of doctor visits
*	- offer = 1 if employer offers insurance
* 	- ssiratio = ratio of SSI income to toatl income
* 	- age = Age
* 	- educyr = Years of education
* 	- physician = #Visits to doctor
* 	- nonphysician = #Visits to health professional, but not doctor
* 	- medicaid = 1 if has Medicaid public insurance
* 	- private = 1 if has private supplementary insurance
* 	- female = 1 if female
* 	- phylim = 1 if physical limitation
* 	- actlim  = 1 if activity limitation
* 	- income  =  Income
* 	- totchr  = # chronic conditions
* 	- insured  = 1 if has private supplementary insurance
* 	- age2 = Age-squared
* 	- linc = log(income)
* 	- bh = 1 if black or Hispanic

// Open the dataset
use mus17data.dta, clear

// summarize the data
summarize docvis private medicaid age age2 educyr actlim totchr

// run basic poisson regression
poisson docvis private medicaid age age2 educyr actlim totchr

// poisson regression with correction for overdispersion
poisson docvis i.private i.medicaid c.age c.age2 c.educyr i.actlim c.totchr, vce(robust) nolog

// compute marginal effects
margins, dydx(*) //AME

margins, dydx(*) atmean //MEM

//test for overdispersion
poisson docvis i.private i.medicaid c.age c.age2 c.educyr i.actlim c.totchr, nolog
predict muhat, n
generate ystar = ((docvis-muhat)^2 - docvis)/muhat
regress ystar muhat, noconstant noheader

