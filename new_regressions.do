

*****************************************************************************************************************
* REGRESSIONS
*****************************************************************************************************************
cd  "C:\Users\wb519128\Dropbox\Work\Insper\PMRJ"

 use data_SIM_2019-01.dta, clear

*Table 1

//sum  violent_death_sim vehicle_robbery  street_robbery lag_dist_target_vd  lag_dist_target_vr lag_dist_target_sr police_killing  other_robberies cargo_robbery burglary store_robbery   vehicle_theft street_theft  body_found   drug_seizure gun_seizure arrest2  max_prize population  policemen_aisp policemen_upp  exp_similar  exper_cmt_total on_target_vd on_target_vr on_target_sr on_target if cycle~=1 & sem_year>100

*Table 2


foreach y of varlist  violent_death_sim  vehicle_robbery  street_robbery homicide dpolice_killing {
xi: xtreg `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year if sem_year>100,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)
	outreg2 using Results\tab2.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
xi: xtreg  `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt if sem_year>100,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)	
	outreg2 using Results\tab2.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
xi: xtivreg `y' ( on_target =lag12_dist_target_vr lag12_dist_target_sr lag12_dist_target_vd  ) policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)	
	eret2 scalar F_test=e(F_f)
	outreg2 using Results\tab2.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}

*Table 3 - gaming

foreach y of varlist vehicle_theft street_theft  dbody_found  {
xi: xtreg `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year if sem_year>100,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)
	outreg2 using Results\tab3.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
xi: xtreg  `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt if sem_year>100,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)
	outreg2 using Results\tab3.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
xi: xtivreg `y' ( on_target =lag12_dist_target_vr lag12_dist_target_sr lag12_dist_target_vd  ) policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)	
	eret2 scalar F_test=e(F_f)
	outreg2 using Results\tab3.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}


*Table 4 - Spillovers

foreach y of varlist other_robberies cargo_robbery burglary store_robbery  {
xi: xtreg `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year if sem_year>100,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)
	outreg2 using Results\tab4.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
xi: xtreg  `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt if sem_year>100,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)
	outreg2 using Results\tab4.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
xi: xtivreg `y' ( on_target =lag12_dist_target_vr lag12_dist_target_sr lag12_dist_target_vd  ) policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)	
	eret2 scalar F_test=e(F_f)
	outreg2 using Results\tab4.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}



* Table 5 - poisson
foreach y of varlist violent_death_sim  vehicle_robbery  street_robbery homicide dpolice_killing cargo_robbery  vehicle_theft street_theft  dbody_found drug_seizure gun_seizure arrest2{

xtpoisson  `y' on_target i.month i.year policemen_aisp policemen_upp n_precinct if cycle~=1 & sem_year>100, i(aisp) exposure(population) fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar effect= exp(_b[on_target])-1
	outreg2 using Results\tab5.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y effect)
xtpoisson  `y' on_target i.month i.year policemen_aisp policemen_upp n_precinct i.id_cmt if cycle~=1 & sem_year>100, i(aisp) exposure(population) fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar effect= exp(_b[on_target])-1
	outreg2 using Results\tab5.xls, keep(on_target) dec(3) nocons  aster(se)   e(mean_y effect)
}

*Figure 2 - timing

foreach y of varlist violent_death_sim  vehicle_robbery  street_robbery dpolice_killing vehicle_theft street_theft  {
foreach i of numlist 2 (1) 6 {
xi: xtreg `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt if cycle==`i',  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)	
	eret2 scalar F_test=e(F_f)
	outreg2 using Results\tab6.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}
}


*Table 7 - effort

foreach y of varlist arrest2 drug_seizure gun_seizure {
xi: xtreg `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year if sem_year>100,  fe 
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)
	outreg2 using Results\tab7.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
xi: xtreg  `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt  if sem_year>100,  fe 
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)
	outreg2 using Results\tab7.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )
xi: xtivreg `y' ( on_target =lag12_dist_target_vr lag12_dist_target_sr lag12_dist_target_vd  ) policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt,  fe 
	sum `y' if e(sample)==1
	eret2 scalar mean_y=r(mean)
	eret2 scalar adj_R2=e(r2_a)	
	eret2 scalar F_test=e(F_f)
	outreg2 using Results\tab7.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}


*Reduced-form
foreach y of varlist  violent_death_sim  vehicle_robbery  street_robbery homicide dpolice_killing {
xi: xtreg `y'  lag12_dist_target_vd policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt,  fe 
	outreg2 using Results\tab8.xls, keep(lag12_dist_target_vr lag12_dist_target_sr lag12_dist_target_vd ) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
xi: xtreg `y'  lag12_dist_target_vr lag12_dist_target_sr  lag12_dist_target_vd policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt,  fe 
	outreg2 using Results\tab8.xls, keep(lag12_dist_target_vr lag12_dist_target_sr lag12_dist_target_vd ) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}

*Table 8 - teste de autocorrela‹o
foreach y of varlist  violent_death_sim  vehicle_robbery  street_robbery homicide dpolice_killing {

  xi: xtreg  `y'  lag1_on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt if  cycle==1,  fe 
	outreg2 using Results\tab9.xls, keep(lag1_on_target ) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}


*****************************************************************************************************************
* GRAPHS
*****************************************************************************************************************
/*

egen mean_on_target=mean(on_target), by(cycle )
twoway ( line mean_on_target cycle, sort  ytitle("Share of police units that are on target") xtitle("") )

egen mean_hit_target=mean(hit_target), by(cycle )
twoway ( line mean_hit_target cycle, sort  ytitle("Share of police units that hit the semester target") xtitle("") )

foreach x of varlist hit_target_vd  hit_target_vr hit_target_sr{
egen mean_`x'=mean(`x'), by(cycle )
}
twoway ( line mean_hit_target_vd cycle, sort  legend(label(1 "violent death")) lpattern(dash) ) ( line mean_hit_target_vr cycle , sort legend(label(2 "vehicle robbery")) ) ( line mean_hit_target_sr cycle , sort legend(label(3 "street robbery")) xtitle("") ytitle("Share of police units that hit the semester target") )

egen mean_hit_target2=mean(hit_target), by(cycle year)
twoway ( line mean_hit_target2 cycle if year==2010, sort legend(label(1 "2010")) lpattern(dash))  ( line mean_hit_target2 cycle if year==2011, sort legend(label(2 "2011")) )  ( line mean_hit_target2 cycle if year==2012, sort legend(label(3 "2012")) )  ( line mean_hit_target2 cycle if year==2013, sort legend(label(4 "2013")))  ( line mean_hit_target2 cycle if year==2014, sort legend(label(5 "2014")) lpattern(dash_dot))  ( line mean_hit_target2 cycle if year==2015, sort legend(label(6 "2015"))  ytitle("Share of police units that hit the target") xtitle("month") )

foreach x of varlist hit_target_vr {
egen mean_`x'2=mean(`x'), by(cycle year)
twoway ( line mean_`x'2 cycle if year==2010, sort legend(label(1 "2010")) lpattern(dash))  ( line mean_`x'2 cycle if year==2011, sort legend(label(2 "2011")) )  ( line mean_`x'2 cycle if year==2012, sort legend(label(3 "2012")) )  ( line mean_`x'2 cycle if year==2013, sort legend(label(4 "2013")))  ( line mean_`x'2 cycle if year==2014, sort legend(label(5 "2014")) lpattern(dash_dot))  ( line mean_`x'2 cycle if year==2015, sort legend(label(6 "2015"))  ytitle("Share of police units that hit the target") xtitle("month") )
}

foreach x of varlist  violent_death_sim  vehicle_robbery  street_robbery {
bysort aisp (year): gen hist_`x'= (`x'[_n-12]+`x'[_n-24]+`x'[_n-36])/3/population*100000
bysort aisp (year): gen dt_`x'= `x'- (`x'[_n-12]+`x'[_n-24]+`x'[_n-36])/3

}

 graph box lag_dist_target_vr, ytitle(Vehicle Robbery)
 graph box lag_dist_target_sr, noout ytitle(Street Robbery)
 graph box lag_dist_target_vd, noout ytitle(Violent Death)

 
 graph box dt_vehicle_robbery, over(year) noout
 graph box dt_street_robbery, over(year) noout

 graph box lag_dist_target_vr, noout

*Problema na base
 br aisp year month vehicle_robbery  vehicle_robbery_cum target_vr_sem dist_target_vr if dist_target_vr>1000 & dist_target_vr!=. 
 
 
 */
