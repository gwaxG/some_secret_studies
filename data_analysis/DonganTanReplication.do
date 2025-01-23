********************************** Main Results ********************************
use DonganTanRobustneReplication, clear
/* drop if year==1949 & treat==1: Countries sanctioned in the first year of the dataset are excluded to avoid spurious results*/

encode country, gen(state)
xtset state year

* Table 1: summary statistics
global controls polity EcGI lgdp interwar intrawar lcinc efindex 
xtreg mm i.treat i.year $controls, fe vce(cluster state)
sum treat mm $controls if e(sample)

* Table 2: Effects of economic sanctions on mass mobilization
/* Model (1) */
xtreg mm i.treat i.year, fe vce(cluster state)

/* Model (2) */
xtreg mm i.treat i.year $controls, fe vce(cluster state)

/* Model (3) */
reg mm i.state i.year if treat == 0, nocons
predict adj, residuals
reg adj i.treat, vce(bootstrap)

/* Model (4) */
reg adj i.treat $controls, vce(bootstrap)

/* Model (5) */
drop adj
reg mm i.state i.year if ever_sanctioned == 0, nocons
predict adj, residuals
reg adj i.treat, vce(bootstrap) 

/* Model (6) */
reg adj i.treat $controls, vce(bootstrap)


* Table 3: Conditional effects of economic sanctions on mass mobilization
gen yearXdem = year*dem
gen stateXdem = state*dem
gen yearXEcGI = year*lowEcGI
gen stateXEcGI = state*lowEcGI

/* Model (1) */
drop adj
reg mm i.state i.year i.yearXdem i.stateXdem if treat == 0, nocons
predict adj, residuals
reg adj i.treat##i.dem EcGI lgdp interwar intrawar lcinc efindex, vce(bootstrap)
margins, dydx(treat) at(dem=(0(1)1))
marginsplot, ytitle(Marginal effect of sanctions imposition, size(medium small)) sub(A: Unsanctioned) xlabel(0 "Autocracy" 1 "Democracy", labsize(medium small) nogrid) title("") xtitle("") plotregion(margin(large)) saving(dem1)

/* Model (2) */
drop adj
reg mm i.state i.year i.yearXdem i.stateXdem if ever_sanctioned == 0, nocons
predict adj, residuals
reg adj i.treat##i.dem EcGI lgdp interwar intrawar lcinc efindex, vce(bootstrap)
margins, dydx(treat) at(dem=(0(1)1))
marginsplot, ytitle(Marginal effect of sanctions imposition, size(medium small)) sub(B: Not-yet-sanctioned) xlabel(0 "Autocracy" 1 "Democracy", labsize(medium small) nogrid) title("") xtitle("") plotregion(margin(large)) saving(dem2)

/* Model (3) */
drop adj
reg mm i.state i.year i.yearXEcGI i.stateXEcGI if treat == 0, nocons
predict adj, residuals
reg adj i.treat##i.lowEcGI polity lgdp interwar intrawar lcinc efindex, vce(bootstrap)
margins, dydx(treat) at(lowEcGI=(0(1)1))
marginsplot, ytitle(Marginal effect of sanctions imposition, size(medium small)) sub(A: Unsanctioned) xlabel(0 "High-globalized" 1 "Low-globalized", labsize(medium small) nogrid) title("") xtitle("") plotregion(margin(large)) saving(EcGl1)

/* Model (4) */
drop adj
reg mm i.state i.year i.yearXEcGI i.stateXEcGI if ever_sanctioned == 0, nocons
predict adj, residuals
reg adj i.treat##i.lowEcGI polity lgdp interwar intrawar lcinc efindex, vce(bootstrap)
margins, dydx(treat) at(lowEcGI=(0(1)1))
marginsplot, ytitle(Marginal effect of sanctions imposition, size(medium small)) sub(B: Not-yet-sanctioned) xlabel(0 "High-globalized" 1 "Low-globalized", labsize(medium small) nogrid) title("") xtitle("") plotregion(margin(large)) saving(EcGl2)

/* Figure 2: Marginal effect of sanctions imposition conditional on democracy */
gr combine dem1.gph dem2.gph

/* Figure 3: Marginal effect of sanctions imposition conditional on economic globalization */
gr combine EcGl1.gph EcGl2.gph

* Figure 4: Dynamic effects of economic sanctions on mass mobilization
gen rel_time =  year - first_sanction

// leads
	cap drop F_*
	forval x = 1/10 {
		gen F_`x' = rel_time == -`x'
	}
//lags
	cap drop L_*
	forval x = 0/10 {
		gen L_`x' = rel_time ==  `x'
	}

did2s mm, first_stage(state year) second_stage(F_* L_*) treatment(treat) cluster(state)
event_plot, default_look graph_opt(xtitle("Years since economic sanctions were imposed") ytitle("Average effect") sub(A: Without covariates) xlabel(-10(1)10)) stub_lag(L_#) stub_lead(F_#) together
graph save trend1

did2s mm, first_stage(state year) second_stage(F_* L_* $controls) treatment(treat) cluster(state)
event_plot, default_look graph_opt(xtitle("Years since economic sanctions were imposed") ytitle("Average effect") sub(B: With covariates) xlabel(-10(1)10)) stub_lag(L_#) stub_lead(F_#) together
graph save trend2

gr combine trend1.gph trend2.gph, rows(2)


*********************************** Robustness check  **************************
use DonganTanRobustnessReplication, clear
/* drop if year==1989 & treat==1: Countries sanctioned in the first year of the dataset are excluded to avoid spurious results*/

encode country, gen(state)
xtset state year

* Table 4: Effect of economic sanctions on mass mobilization using EUSANCT dataset
global controls polity EcGI lgdp interwar intrawar lcinc efindex demsanc coup regtype
/* Model (1) */
xtreg mm i.treat i.year, fe vce(cluster state)

/* Model (2) */
xtreg mm i.treat i.year $controls, fe vce(cluster state)

/* Model (3) */
reg mm i.state i.year if treat == 0, nocons
predict adj, residuals
reg adj i.treat, vce(bootstrap)

/* Model (4) */
reg adj i.treat $controls, vce(bootstrap)

/* Model (5) */
drop adj
reg mm i.state i.year if ever_sanctioned == 0, nocons
predict adj, residuals
reg adj i.treat, vce(bootstrap) 

/* Model (6) */
reg adj i.treat $controls, vce(bootstrap)

* Figure 5 & 6: Marginal effect of sanctions imposition.
gen yearXdem = year*dem
gen stateXdem = state*dem
gen yearXEcGI = year*lowEcGI
gen stateXEcGI = state*lowEcGI

/* Figure 5 */
drop adj
reg mm i.state i.year i.yearXdem i.stateXdem if treat == 0, nocons
predict adj, residuals
reg adj i.treat##i.dem EcGI lgdp interwar intrawar lcinc efindex demsanc coup regtype, vce(bootstrap)
margins, dydx(treat) at(dem=(0(1)1))
marginsplot, ytitle(Marginal effect of sanctions imposition, size(medium small)) sub(A: Unsanctioned) xlabel(0 "Autocracy" 1 "Democracy", labsize(medium small) nogrid) title("") xtitle("") plotregion(margin(large)) saving(dem1)

drop adj
reg mm i.state i.year i.yearXdem i.stateXdem if ever_sanctioned == 0, nocons
predict adj, residuals
reg adj i.treat##i.dem EcGI lgdp interwar intrawar lcinc efindex demsanc coup regtype, vce(bootstrap)
margins, dydx(treat) at(dem=(0(1)1))
marginsplot, ytitle(Marginal effect of sanctions imposition, size(medium small)) sub(B: Not-yet-sanctioned) xlabel(0 "Autocracy" 1 "Democracy", labsize(medium small) nogrid) title("") xtitle("") plotregion(margin(large)) saving(dem2)

gr combine dem1.gph dem2.gph

/* Figure 6 */
drop adj
reg mm i.state i.year i.yearXEcGI i.stateXEcGI if treat == 0, nocons
predict adj, residuals
reg adj i.treat##i.lowEcGI polity lgdp interwar intrawar lcinc efindex demsanc coup regtype, vce(bootstrap)
margins, dydx(treat) at(lowEcGI=(0(1)1))
marginsplot, ytitle(Marginal effect of sanctions imposition, size(medium small)) sub(A: Unsanctioned) xlabel(0 "High-globalized" 1 "Low-globalized", labsize(medium small) nogrid) title("") xtitle("") plotregion(margin(large)) saving(EcGl1)

drop adj
reg mm i.state i.year i.yearXEcGI i.stateXEcGI if ever_sanctioned == 0, nocons
predict adj, residuals
reg adj i.treat##i.lowEcGI polity lgdp interwar intrawar lcinc efindex demsanc coup regtype, vce(bootstrap)
margins, dydx(treat) at(lowEcGI=(0(1)1))
marginsplot, ytitle(Marginal effect of sanctions imposition, size(medium small)) sub(B: Not-yet-sanctioned) xlabel(0 "High-globalized" 1 "Low-globalized", labsize(medium small) nogrid) title("") xtitle("") plotregion(margin(large)) saving(EcGl2)

gr combine EcGl1.gph EcGl2.gph

* Table 5: Effects of economic sanctions on terror incidents and political stability index
use DonganTanRobustnessReplication2, clear

encode country, gen(state)
xtset state year

/* Model (1) */
global controls polity EcGI lgdp interwar intrawar lcinc efindex
keep if year>=1970 & year<=2020 /* Global Terrorism Database: 1970-2020 */
drop if year==1970 & treat==1 /*Countries sanctioned in the first year of the dataset are excluded to avoid spurious results*/

egen first_sanction = min(year / (treat == 1)), by(state)
gen ever_sanctioned = 1 if year >= first_sanction
replace ever_sanctioned=0 if ever_sanctioned==.

reg terror i.state i.year if ever_sanctioned == 0, nocons
predict adj, residuals
reg adj i.treat, vce(bootstrap)

/* Model (2) */
reg adj i.treat $controls, vce(bootstrap)

/* Model (3) */
use DonganTanRobustnessReplication2, clear

global controls polity EcGI lgdp interwar intrawar lcinc efindex

encode country, gen(state)
xtset state year

keep if year>=1996 & year<=2022 /* Political Stability Index: 1996-2022 */
drop if year==1996 & treat==1 /*Countries sanctioned in the first year of the dataset are excluded to avoid spurious results*/

egen first_sanction = min(year / (treat == 1)), by(state)
gen ever_sanctioned = 1 if year >= first_sanction
replace ever_sanctioned=0 if ever_sanctioned==.

reg pve i.state i.year if ever_sanctioned == 0, nocons
predict adj, residuals
reg adj i.treat, vce(bootstrap)

/* Model (4) */
reg adj i.treat $controls, vce(bootstrap)


*************************** Appendix 2: Parallel trends ************************
use DonganTanReplication, clear

encode country, gen(state)
xtset state year

global controls polity EcGI lgdp interwar intrawar lcinc efindex

* Figure A1: Trend of mass mobilization in TWFE estimator
gen rel_time =  year - first_sanction

// leads
	cap drop F_*
	forval x = 1/10 {  
		gen F_`x' = rel_time == -`x'
	}
//lags
	cap drop L_*
	forval x = 0/10 {
		gen L_`x' = rel_time ==  `x'
	}

xtreg mm F_* L_* i.year $controls, fe vce(cluster state)
event_plot, default_look graph_opt(xtitle("Years since economic sanctions were imposed") ytitle("Effects on mass mobilization") xlabel(-10(1)10)) stub_lag(L_#) stub_lead(F_#) together

* Figure A2: Trend of mass mobilization in 2sDiD estimator (democracies vs. autocracies)
did2s mm if dem==1, first_stage(state year) second_stage(F_* L_* $controls) treatment(treat) cluster(state)
event_plot, default_look graph_opt(xtitle("Years since economic sanctions were imposed") ytitle("Effects on mass mobilization") sub(A: Democracies) xlabel(-10(1)10)) stub_lag(L_#) stub_lead(F_#) together
graph save appdem1

did2s mm if dem==0, first_stage(state year) second_stage(F_* L_* $controls) treatment(treat) cluster(state)
event_plot, default_look graph_opt(xtitle("Years since economic sanctions were imposed") ytitle("Effects on mass mobilization") sub(B: Autocracies) xlabel(-10(1)10)) stub_lag(L_#) stub_lead(F_#) together
graph save appdem2

gr combine appdem1.gph appdem2.gph, saving(appdem) rows(2)

* Figure A3: Trend of mass mobilization in 2sDiD estimator (high-globalized vs. low-globalized)
did2s mm if lowEcGI==1, first_stage(state year) second_stage(F_* L_* $controls) treatment(treat) cluster(state)
event_plot, default_look graph_opt(xtitle("Years since economic sanctions were imposed") ytitle("Effects on mass mobilization") sub(A: Low-globalized) xlabel(-10(1)10)) stub_lag(L_#) stub_lead(F_#) together
graph save appEcGl1

did2s mm if lowEcGI==0, first_stage(state year) second_stage(F_* L_* $controls) treatment(treat) cluster(state)
event_plot, default_look graph_opt(xtitle("Years since economic sanctions were imposed") ytitle("Effects on mass mobilization") sub(B: High-globalized) xlabel(-10(1)10)) stub_lag(L_#) stub_lead(F_#) together
graph save appEcGl2

gr combine appEcGl1.gph appEcGl2.gph, saving(appEcGl) rows(2)


************************** Appendix 3: Correlation test ************************
use DonganTanReplication, clear
pwcorr mm treat polity EcGI lgdp interwar intrawar lcinc efindex, sig star(.05)

use DonganTanRobustnessReplication2, clear
keep if year>=1970 & year<=2020 /* Global Terrorism Database: 1970-2020 */
pwcorr terror treat polity EcGI lgdp interwar intrawar lcinc efindex, sig star(.05)

use DonganTanRobustnessReplication2, clear
keep if year>=1996 & year<=2022 /* Political Stability Index: 1996-2022 */
pwcorr pve treat polity EcGI lgdp interwar intrawar lcinc efindex, sig star(.05)

