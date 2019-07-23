
drop if aisp == 40 | aisp == 41

xi: xtreg violent_death_sim  on_target policemen_aisp policemen_upp n_precinct max_prize population i.month i.year if sem_year>100,  fe 
xi: xtivreg violent_death_sim ( on_target =lag12_dist_target_vr lag12_dist_target_sr lag12_dist_target_vd  ) policemen_aisp policemen_upp n_precinct max_prize population i.month i.year i.id_cmt,  fe 
di `e(rs_a)'
