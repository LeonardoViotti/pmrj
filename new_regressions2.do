*****************************************************************************************************************
* New regressions and placebos
*****************************************************************************************************************
global dir C:\Users\wb519128\Dropbox\Work\Insper\PMRJ

 
use $dir/data_SIM_2019-01.dta, clear

//import delimited $dir/data_SIM_2019.csv, clear
 
 
* Merge com a base de meta placedo

preserve
	tempfile
	import delimited  "$dir/placebo_targets_gis.csv", clear 
		
	save placebo_targets, replace
restore
 
 drop if missing(aisp) | missing(year) | missing(month)
 merge 1:1 aisp year month using placebo_targets



*****************************************************************************************************************
* REGRESSIONS
*****************************************************************************************************************

/*

foreach y of varlist  violent_death_sim /*  vehicle_robbery  street_robbery homicide dpolice_killing */  {
	xi: xtreg `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year if sem_year>100,  fe 
		sum `y' if e(sample)==1
		eret2 scalar mean_y=r(mean)
		eret2 scalar adj_R2=e(r2_a)
		*outreg2 using Results\tab2.xls, keep(on_target) dec(3) nocons  aster(se) e(mean_y adj_R2 )

		
	xi: reg `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.aisp i.month i.year if sem_year>100
	
	*xi: ols_spatial_HAC  `y'  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year if sem_year>100,  fe 	
	
	} 
	*/
	

	xtreg violent_death_sim  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year if sem_year>100,  fe 
	reg violent_death_sim  on_target policemen_aisp policemen_upp n_precinct max_prize population i.aisp i.month i.year if sem_year>100
	
	ols_spatial_HAC violent_death_sim  on_target policemen_aisp policemen_upp n_precinct max_prize population i.aisp i.month i.year if sem_year>100, lat(latitude) lon(longitude) t(year) p(aisp) dist(500) lag(5)
