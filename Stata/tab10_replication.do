cd  "C:\Users\wb519128\Dropbox\Work\Insper\PMRJ\data"
use data_SIM_2019-07.dta, clear


*Table 10 - Dif-Dif


gen last_month=(month==6 | month==12) 


// Semester sums of variables
foreach x of varlist violent_death_sim target_vd street_robbery target_sr vehicle_robbery target_vr {
egen `x'6=sum(`x'), by(aisp sem_year)
}

// Target specific award, if AIPS was on target for each variable in the semester
gen award_violent_death=(violent_death_sim6<=target_vd6) 
gen award_street_robbery=(street_robbery6<= target_sr6)
gen award_vehicle_robbery=(vehicle_robbery6 <= target_vr) // Potential error in the code


// If AISP was awarded in any of the target variables
drop awarded // Not sure if the right thing to do, just make sure that it runs
gen awarded= (award_violent_death==1 | award_street_robbery==1 | award_vehicle_robbery==1)


// Hit target is 1 when on_target is 1 
gen hit_target = (on_target==1  )

// Hit target is also 1 when first month of semester (cycle is 1) and aisp was awarded in the past semester in any variable
bysort  aisp (month_year):replace hit_target = (awarded[_n-1]==1 ) if cycle==1 


// Keep only first and last months of each semester
keep if month==1 | month==6 | month==7 | month==12

// Interaction variable
gen last_month_hit= hit_target*last_month


// Regression
foreach y of varlist  violent_death_sim  vehicle_robbery  street_robbery other_robberies cargo_robbery burglary store_robbery vehicle_theft street_theft  dbody_found {
xi: xtreg `y' last_month_hit hit_target last_month  policemen_aisp policemen_upp n_precinct max_prize population i.month i.year  if sem_year>100,  fe 
	outreg2 using Results\tab10.xls, keep(last_month_hit hit_target last_month  ) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
xi: xtreg `y' last_month_hit hit_target last_month  policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt if sem_year>100,  fe 
	outreg2 using Results\tab10.xls, keep(last_month_hit hit_target last_month  ) dec(3) nocons  aster(se) e(mean_y adj_R2 F_test)
}
