
library(rdrobust)

# 100thousand rates

sr$vd_ppop <- (sr$violent_death_sim/sr$population)*100000
sr$vr_ppop <- (sr$vehicle_robbery/sr$population)*100000
sr$sr_ppop <- (sr$street_robbery/sr$population)*100000



# Simplest RD plots

# Residuo da rv_real.tx vs lv_dist
(rdplot(sr$vd_ppop, 
        sr$dist_target_vd, 
        # y.lim = c(-8, 13), 
        # x.lim = c(-1,2), 
        # y.label = "rv_real.tx (res)", 
        # x.label = "lv_dist", 
        binselect = "qsmvpr",
        p = 4
))

(rdplot(sr$vr_ppop, 
        sr$dist_target_vr, 
        # y.lim = c(-8, 13), 
         x.lim = c(-1,2), 
        # y.label = "rv_real.tx (res)", 
        # x.label = "lv_dist", 
        binselect = "qsmvpr",
        p = 4
))


(rdplot(sr$sr_ppop, 
        sr$dist_target_sr, 
        # y.lim = c(-8, 13), 
        #x.lim = c(-1,3), 
        # y.label = "rv_real.tx (res)", 
        # x.label = "lv_dist", 
        binselect = "qsmvpr",
        p = 4
))

# Residual RD plots

fit.vd    <- lm(vd_ppop ~ as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)
fit.vr    <- lm(vr_ppop ~ as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)
fit.sr    <- lm(sr_ppop ~ as.factor(aisp) + as.factor(month) + as.factor(year), data = sr)

sr$res_vd    <- fit.vd$resid
sr$res_vr    <- fit.vr$resid
sr$res_sr    <- fit.sr$resid


(rdplot(sr$res_vd, 
        sr$dist_target_vd, 
        # y.lim = c(-8, 13), 
        # x.lim = c(-1,2), 
        # y.label = "rv_real.tx (res)", 
        # x.label = "lv_dist", 
        binselect = "qsmvpr",
        p = 4
))

(rdplot(sr$res_vr, 
        sr$dist_target_vr, 
        # y.lim = c(-8, 13), 
        x.lim = c(-1,2), 
        # y.label = "rv_real.tx (res)", 
        # x.label = "lv_dist", 
        binselect = "qsmvpr",
        p = 4
))

(rdplot(sr$res_sr, 
        sr$dist_target_sr, 
        # y.lim = c(-8, 13), 
        # x.lim = c(-1,2), 
        # y.label = "rv_real.tx (res)", 
        # x.label = "lv_dist", 
        binselect = "qsmvpr",
        p = 4
))



# foo <- sr %>% 
#        select("aisp",
#           "year",
#           "semester",
#           "month",
#           "violent_death",
#           "violent_death_sim",
#           "violent_death_sim_cum",
#           "target_vd",
#           "target_vd_sem",
#           "target_vd_cum",
#           "on_target_vd",
#           "dist_target_vd")
# 
# #idm <- \


# foo$fvd <- ((sr$target_vd_sem - sr$violent_death_sim_cum)/sr$target_vd_sem) -1
# foo$comp_target <- sr$target_vd_sem*3 + sr$target_vr*2 + sr$target_sr