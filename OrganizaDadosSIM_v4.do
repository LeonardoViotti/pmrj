

cd  "/Users/joanacmm/Dropbox/CrimeData/

*****************************************************************************************************************
* ORGANIZE PCERJ INFORMATION
*****************************************************************************************************************
insheet using "PCERJ\Base_DP_consolidado.csv", delimiter(";") clear
destring _all, replace
*Add population figures
merge 1:1 cisp vano mes using ArquivosAuxiliares\painel_dp_populacao_MES.dta
drop if _merge 	==2
drop _merge
gen furto_rua=  furto_transeunte + furto_coletivo + furto_celular

gen n_precinct=1


rename cisp id_precinct
rename vano year
rename mes month

*Add favela area 
merge m:1 id_precinct using Geo\favela_area.dta
drop _merge
sort id_precinct

*Add precinct area
merge m:1 id_precinct using Geo\precinct_area.dta
drop _merge
sort id_precinct

merge 1:1 id_precinct year month using PCERJ\gun_seizure.dta
drop _merge

* Merge DD data
merge 1:1 id_precinct year month  using Denuncias/gunfight_reports.dta
drop _merge

recode gunfight_event gunfight_report (.=0) if year >=2007  & year <=2015

rename  year ano
rename  month mes

* Collapse data from precinct level do AISP level
destring apf, replace
destring aaapai, replace
destring roubo_cx_eletronico, replace



collapse (sum) n_precinct indicador_letalidade hom_doloso encontro_cadaver apreensao_drogas total_gun rifle pistol machine_gun apf aaapai hom_por_interv_policial total_roubos ///
 total_furtos roubo_comercio roubo_residencia indicador_roubo_rua roubo_veiculo furto_veiculo furto_rua roubo_carga roubo_cx_eletronico roubo_conducao_saque  gunfight_report  gunfight_event pop_cisp area_favela area_precinct (max) mcirc  risp, by(aisp mes ano)


*Add targets
merge 1:1 aisp ano mes using "SIM/meta_lv.dta"
sort aisp mes ano
drop _merge
merge 1:1 aisp ano mes using "SIM/meta_rr.dta"
sort aisp mes ano
drop _merge
merge 1:1 aisp ano mes using "SIM/meta_rv.dta"
sort aisp mes ano
drop _merge


*Add number of policemen. Dataset starts in 2008 and ends in 2015/6
gen mes_ano=ym(ano, mes)
merge 1:1 aisp mes_ano using ArquivosAuxiliares\Efetivo\efetivo_bpm_upp\efetivo_bpm_upp_mod.dta
drop _merge


*Add information on commanders
sort aisp mes_ano
merge 1:1 aisp mes_ano using ArquivosAuxiliares\Efetivo\Base_cmt_aisp.dta
drop _merge
gen exp_similar= d_clus_det1 *exper_1 + d_clus_det2 *exper_2  + d_clus_det3 *exper_3 +  d_clus_det4 *exper_4 +   d_clus_det5 *exper_5 + d_clus_det6 *exper_6


gen semestre=1 if mes>=1 & mes<=6
replace semestre=2 if mes>=7 & mes<=12

gen max_prize=1.5 if ano==2009 & semestre==2 | ano==2010 & semestre==1
replace max_prize=3 if  ano==2010 & semestre==2
replace max_prize=6 if  ano==2011 & semestre==1
replace max_prize=9 if  ano==2011 & semestre==2 | ano==2012
replace max_prize=13.5 if  ano>=2013 

gen min_prize=.5 if ano==2009 & semestre==2 | ano==2010 & semestre==1
replace min_prize=1 if  ano==2010 & semestre==2
replace min_prize=2 if  ano==2011 & semestre==1
replace min_prize=3 if  ano==2011 & semestre==2 | ano==2012
replace min_prize=1.5 if  ano>=2013 


rename indicador_letalidade violent_death
rename hom_doloso homicide
rename apreensao_drogas drug_seizure
rename total_gun gun_seizure
rename apf arrest
rename aaapai juvenile_arrest
rename hom_por_interv_policial police_killing
rename total_roubos robbery
rename total_furtos theft
rename indicador_roubo_rua street_robbery
rename roubo_veiculo vehicle_robbery 
rename roubo_residencia burglary
rename roubo_comercio store_robbery
rename furto_rua street_theft
rename mcirc id_municipality
rename pop_cisp population
rename roubo_carga cargo_robbery
rename furto_veiculos vehicle_theft
rename mes month
rename ano year
rename meta_lv target_vd
rename meta_rr target_sr
rename meta_rv target_vr
rename semestre semester
rename encontro_cadaver body_found
rename area_precinct area_aisp
rename  efetivo_bpm policemen_aisp
rename efetivo_upp policemen_upp
rename mes_ano month_year

gen violent_death_sim=homicide if year<=2010
replace violent_death_sim=violent_death if year>2010
gen dpolice_killing=(police_killing>0)
gen dbody_found=(body_found>0)

rename roubo_conducao_saque withdraw_robbery
rename roubo_cx_eletronico atm_robbery
gen other_robberies= robbery - street_robbery
gen arrest2=arrest+juvenile_arrest

foreach x of varlist  juvenile_arrest arrest withdraw_robbery  atm_robbery street_robbery burglary store_robbery theft robbery gun_seizure drug_seizure vehicle_robbery vehicle_theft cargo_robbery other_robberies {
label variable `x' "registers of `x'"
}
foreach x of varlist  violent_death homicide body_found police_killing {
label variable `x' "victims of `x'"
}

label var population "predicted population at the precinct"
label var year "year"
label var month "month"
label var id_municipality "municipality identifier"
label var min_prize "monetary_value of the prize for AISP that hit the target (R$ 1000)"
label var max_prize "monetary_value of the prize for AISP in the 1st place (R$ 1000)"
label var n_precinct "number of precincts inside the aisp"
label var exp_similar "number of months AISP chief has in similar AISP"
label var gunfight_event "number of gunfight events"
label var gunfight_report "number of gunfight reports"


gen sem_year=yh(year, semester)
format sem_year %th
format month_year %tm

gen cycle=1 if month==1 | month==7
replace cycle=2 if month==2 | month==8
replace cycle=3 if  month==3 | month==9
replace cycle=4 if month==4 | month==10
replace cycle=5 if month==5 | month==11
replace cycle=6 if month==6 | month==12



*****************************************************************************************************************
* CREATE TREATMENT VARIABLES 
*****************************************************************************************************************

*There are no information on targets for AISP 1 and AISP 13
 drop if aisp==1 | aisp==13
*Meta mensal acumulada até o mes anterior

foreach x of varlist violent_death_sim target_vd street_robbery target_sr vehicle_robbery target_vr {

bysort aisp (sem_year month): gen `x'_cum= `x'[_n-1] if month==2 | month==8
bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==3 | month==9
bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==4 | month==10
bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==5 | month==11
bysort  aisp (sem_year): replace `x'_cum=  `x'[_n-1] +  `x'_cum[_n-1] if month==6 | month==12
}

foreach x of varlist violent_death_sim  street_robbery  vehicle_robbery  {
bysort aisp (sem_year month): gen `x'_cum2= `x' if month==1 | month==7
bysort  aisp (sem_year): replace `x'_cum2=  `x'+  `x'_cum2[_n-1] if month==2 | month==8
bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==3 | month==9
bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==4 | month==10
bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==5 | month==11
bysort  aisp (sem_year): replace `x'_cum2=  `x' +  `x'_cum2[_n-1] if month==6 | month==12
}


foreach x of varlist target_vd  target_sr  target_vr {
egen `x'_sem=sum(`x') , by(aisp sem_year) 
}
foreach x of varlist target_vd_sem  target_sr_sem  target_vr_sem {
replace `x'=. if year<2009 | year==2009 & semester==1 | year>2015
} 

gen on_target_vd=(violent_death_sim_cum<=target_vd_cum) if year<=2012
replace on_target_vd=(violent_death_sim_cum<=target_vd_cum*1.1) if year>=2013
replace on_target_vd=. if cycle==1
gen on_target_sr=(street_robbery_cum<=target_sr_cum)
replace on_target_sr=(street_robbery_cum<=target_sr_cum*1.1) if year>=2013
replace on_target_sr=. if cycle==1
gen on_target_vr=(vehicle_robbery_cum<=target_vr_cum)
replace on_target_vr=(vehicle_robbery_cum<=target_vr_cum*1.1) if year>=2013
replace on_target_vr=. if cycle==1

foreach x of varlist on_target_vr  on_target_vd  on_target_sr {
replace `x'=. if year<2009 | year==2009 & semester==1 | year>2015
} 

gen on_target= (on_target_vd==1 & on_target_sr==1 & on_target_vr==1)
replace on_target=. if cycle==1

sort aisp month_year 
gen lag1_on_target=on_target[_n-1]


gen dist_target_vd=violent_death_sim_cum /target_vd_sem -1 
bysort aisp (month_year): gen lag12_dist_target_vd=dist_target_vd[_n-12]

gen dist_target_vr=(vehicle_robbery_cum)/(target_vr_sem ) -1 
bysort aisp (month_year): gen lag12_dist_target_vr=dist_target_vr[_n-12]

gen dist_target_sr=street_robbery_cum /target_sr_sem -1 
bysort aisp (month_year): gen lag12_dist_target_sr=dist_target_sr[_n-12]



foreach x of varlist target_vd  {
label var `x' "violent death target (month)"
label var `x'_sem "violent death target (semester)"
label var `x'_cum "violent death target (cumulative until t-1)"
label var on_`x' "indicator for on target until t-1)"
label var dist_`x' "distance to the target (=0 on target, >0 above target)"

}


label var cycle "Indicator for the month"


order aisp year month semester month_year sem_year cycle violent_death homicide violent_death_sim  police_killing body_found robbery theft street_robbery vehicle_robbery vehicle_theft cargo_robbery other_robberies drug_seizure gun_seizure arrest juvenile_arrest target_vd target_sr target_vr 

gen sample=(year==2009 & semester==2 | year>=2010 & year<=2014 | year==2015 & semester==1)

 
 keep if year>2005
 
xtset aisp month_year

egen tag=tag(aisp id_cmt)
egen n_comandos=sum(tag), by(id_cmt)
* media=2.2, median = 2
egen group=group(id_cmt aisp)
gen n=1 
egen tempo_comando=sum(n), by(group)
*median=mean=14 meses

drop area_favela


cd  "/Users/joanacmm/Dropbox/AvaliacaoSIM
*cd C:\Users\Presidencia\Dropbox\AvaliacaoSIM\


save data_SIM_2019-01.dta, replace

exit


*****************************************************************************************************************
* REGRESSIONS
*****************************************************************************************************************
cd  "/Users/joanacmm/Dropbox/AvaliacaoSIM

 use data_SIM_2019-01.dta, clear

*Table 1

sum  violent_death_sim vehicle_robbery  street_robbery lag_dist_target_vd  lag_dist_target_vr lag_dist_target_sr police_killing  other_robberies cargo_robbery burglary store_robbery   vehicle_theft street_theft  body_found   drug_seizure gun_seizure arrest2  max_prize population  policemen_aisp policemen_upp  exp_similar  exper_cmt_total on_target_vd on_target_vr on_target_sr on_target if cycle~=1 & sem_year>100

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
