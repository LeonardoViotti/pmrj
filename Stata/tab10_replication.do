
 use data_SIM_2019-07.dta, clear

*Table 1

/*
sum  violent_death_sim vehicle_robbery  street_robbery lag_dist_target_vd  lag_dist_target_vr lag_dist_target_sr police_killing  other_robberies cargo_robbery burglary store_robbery   vehicle_theft street_theft  body_found   drug_seizure gun_seizure arrest2  max_prize population  policemen_aisp policemen_upp  exp_similar  exper_cmt_total on_target_vd on_target_vr on_target_sr on_target if cycle~=1 & sem_year>100 
*/

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

*Table 10 - Dif-Dif
gen last_month=(month==6 | month==12) 

foreach x of varlist violent_death_sim target_vd street_robbery target_sr vehicle_robbery target_vr {
egen `x'6=sum(`x'), by(aisp sem_year)
}
gen award_violent_death=(violent_death_sim6<=target_vd6) 
gen award_street_robbery=(street_robbery6<= target_sr6)
gen award_vehicle_robbery=(vehicle_robbery6 <= target_vr)

drop awarded // Not sure if the right thing to do, just make sure that it runs
gen awarded= (award_violent_death==1 | award_street_robbery==1 | award_vehicle_robbery==1)

gen hit_target = (on_target==1  )
bysort  aisp (month_year):replace hit_target = (awarded[_n-1]==1 ) if cycle==1 

keep if month==1 | month==6 | month==7 | month==12

gen last_month_hit= hit_target*last_month


foreach y of varlist  violent_death_sim  vehicle_robbery  street_robbery other_robberies cargo_robbery burglary store_robbery vehicle_theft street_theft  dbody_found {
xi: xtreg `y' last_month_hit hit_target last_month  policemen_aisp policemen_upp n_precinct max_prize population i.month i.year  if sem_year>100,  fe 
	outreg2 using Results\tab10.xls, keep(last_month_hit hit_target last_month  ) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
xi: xtreg `y' last_month_hit hit_target last_month  policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt if sem_year>100,  fe 
	outreg2 using Results\tab10.xls, keep(last_month_hit hit_target last_month  ) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}
