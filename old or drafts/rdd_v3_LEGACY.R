################################################################################
##################################### RDD ######################################
##################################### V3 #######################################
################################################################################

require(rdrobust)

rm(list = ls())

#setwd("C:\\Users\\Francimara\\Downloads\\Leonardo\\PMRJ")
#setwd("D:\\Usu�rio\\Downloads")
setwd("C:\\Users\\LeonardoTV.INSPER\\Dropbox\\Trab\\Insper\\Metricis\\PM Rio\\Cod")

source("Sim_v3.R")

######## RDD: Distancia de cada crime                                           ####

# Ja arrumando para depois de 2012, quando AISPs podiam atingir ate 110% da meta
# e ainda ganhar o premio.

# Letalidade violenta
sim$lv_dist <- ifelse(sim$ano <= 2012, 
                      (sim$lv_cum2 -  sim$lv_meta.sem)/sim$lv_meta.sem,
                      (sim$lv_cum2 - (sim$lv_meta.sem*1.1))/(sim$lv_meta.sem*1.1)
)

#Para usar o IV, tirar a correcao de lv_dist (porque o firpo pediu)
#sim$lv_dist <- (sim$lv_cum2 - sim$lv_meta.sem)/sim$lv_meta.sem


# Roubo de veiculos
sim$rv_dist <- ifelse(sim$ano <= 2012, 
                      (sim$rv_cum2 -  sim$rv_meta.sem)/sim$rv_meta.sem,
                      (sim$rv_cum2 - (sim$rv_meta.sem*1.1))/(sim$rv_meta.sem*1.1)
)

# Roubo de rua
sim$rr_dist <- ifelse(sim$ano <= 2012, 
                      (sim$rr_cum2 -  sim$rr_meta.sem)/sim$rr_meta.sem,
                      (sim$rr_cum2 - (sim$rr_meta.sem*1.1))/(sim$rr_meta.sem*1.1)
)

# Distancia da meta composta
sim$dist <- ifelse(sim$ano <= 2012, 
                   (sim$rea.cum - sim$meta.s)/sim$meta.s,
                   (sim$rea.cum - (sim$meta.s*1.1))/(sim$meta.s*1.1)
)



######## RDD: Subamostras                                                       ####

### Come�o e fim ###

#sim.ini <- subset(sim, (sim$mes.s == 1 | sim$mes.s == 2 | sim$mes.s == 3))
#sim.fim <- subset(sim, (sim$mes.s == 4 | sim$mes.s == 5 | sim$mes.s == 6))

sim.ini <- subset(sim, (sim$mes.s == 1 | sim$mes.s == 2))
sim.fim <- subset(sim, (sim$mes.s == 3 | sim$mes.s == 4 | sim$mes.s == 5 | sim$mes.s == 6))

### Quartis ###

sim.sq <- subset(sim, sim$qar != 1)
sim.cq <- subset(sim, sim$qar == 1)

sim.ini.sq <- subset(sim.ini, sim$qar != 1)
sim.fim.cq <- subset(sim.fim, sim$qar == 1)

######## RDD: Graficos                                                          ####

setwd("C:\\Users\\LeonardoTV.INSPER\\Dropbox\\Trab\\Insper\\Metricis\\PM Rio\\RDD")


### Comecar PDF ###
#pdf( file = paste(Sys.Date(), ".pdf", sep=""))

### histogramas ###

# Variavel dist
hist(sim$dist, breaks= 65, xlab = "dist", main = "N�mero de AISPs/m�s vs dist")

hist(sim$dist, breaks= 65, xlab = "dist", main = "N�mero de AISPs/m�s vs dist (Ampliado)", xlim = c(-0.1, 0.1))

# Variaveis lv_dist, rv_dist e rr_dist
hist(sim$lv_dist, breaks= 65, xlab = "lv_dist", main = "N�mero de AISPs/m�s vs lv_dist")

hist(sim$lv_dist, breaks= 65, xlab = "lv_dist", main = "N�mero de AISPs/m�s vs lv_dist (Ampliado)", xlim = c(-0.3, 0.3))

hist(sim$rv_dist, breaks= 65, xlab = "rv_dist", main = "N�mero de AISPs/m�s vs rv_dist")

hist(sim$rv_dist, breaks= 65, xlab = "rv_dist", main = "N�mero de AISPs/m�s vs rv_dist (Ampliado)", xlim = c(-0.3, 0.3))

hist(sim$rr_dist, breaks= 65, xlab = "rr_dist", main = "N�mero de AISPs/m�s vs rr_dist")

hist(sim$rr_dist, breaks= 65, xlab = "rr_dist", main = "N�mero de AISPs/m�s vs rr_dist (Ampliado)", xlim = c(-0.3, 0.3))

### Terminar PDF ###
#dev.off()

######## RDD: Outliers                                                          ####

# identificar os outliers (top 1% e bottom 1% )
oh_lv = rownames(head(sim[order(-sim$lv_real.tx),], n = (0.01*length(sim$lv_real.tx))))
ol_lv = rownames(tail(sim[order(-sim$lv_real.tx),], n = (0.01*length(sim$lv_real.tx))))

oh_rv = rownames(head(sim[order(-sim$rv_real.tx),], n = (0.01*length(sim$rv_real.tx))))
ol_rv = rownames(tail(sim[order(-sim$rv_real.tx),], n = (0.01*length(sim$rv_real.tx))))

oh_rr = rownames(head(sim[order(-sim$rr_real.tx),], n = (0.01*length(sim$rr_real.tx))))
ol_rr = rownames(tail(sim[order(-sim$rr_real.tx),], n = (0.01*length(sim$rr_real.tx))))


# subsetar para apenas as observacoes que nao estao nos 1%s
sim <- subset(sim, 
              !(rownames(sim) %in% c(oh_lv, ol_lv, oh_rv, ol_rv, oh_rr, ol_rr)) 
)
# subsetar para lv_dist (-1, 1)
sim <- subset(sim, 
              sim$lv_dist <= 1
)

######## RDD: Regredir ocorr�ncias nos controles                                ####

# explicar o que eu estou fazendo aqui !!!!!

fit.lv    <- lm(lv_real.tx ~ as.factor(aisp) + as.factor(mes) + as.factor(ano), data = sim)
fit.rv    <- lm(rv_real.tx ~ as.factor(aisp) + as.factor(mes) + as.factor(ano), data = sim)
fit.rr    <- lm(rr_real.tx ~ as.factor(aisp) + as.factor(mes) + as.factor(ano), data = sim)
fit.dist  <- lm(dist 	   ~ as.factor(aisp) + as.factor(mes) + as.factor(ano), data = sim)

# Como pode haver missing na populacao (dependentes em tx) a coluna do residuo 
# pode nao ter o mesmo comprimento que a base. 

sel.lv   <- which(!is.na(sim$lv_real.tx))
sel.rv   <- which(!is.na(sim$rv_real.tx))
sel.rr   <- which(!is.na(sim$rr_real.tx))
sel.dist <- which(!is.na(sim$dist))

# Colocar uma coluna na base com os valores do residuo
sim$res.lv <- NA
sim$res.lv[sel.lv]     <- fit.lv$resid

sim$res.rv <- NA
sim$res.rv[sel.rv]     <- fit.rv$resid

sim$res.rr <- NA
sim$res.rr[sel.rr]     <- fit.rr$resid

sim$res.dist <- NA
sim$res.dist[sel.dist] <- fit.dist$resid 


######## RDD: Subamostras                                                       ####

### Come�o e fim ###
sim.ini <- subset(sim, (sim$mes.s == 1 | sim$mes.s == 2 | sim$mes.s == 3))
sim.fim <- subset(sim, (sim$mes.s == 4 | sim$mes.s == 5 | sim$mes.s == 6))

### Quartis ###

sim.sq <- subset(sim, sim$qar != 1)
sim.cq <- subset(sim, sim$qar == 1)

sim.ini.cq <- subset(sim.cq, (sim.cq$mes.s == 1 | sim.cq$mes.s == 2 | sim.cq$mes.s == 3))
sim.fim.cq <- subset(sim.cq, (sim.cq$mes.s == 4 | sim.cq$mes.s == 5 | sim.cq$mes.s == 6))



######## RDD: Regressao                                                         ####

setwd("C:\\Users\\LeonardoTV.INSPER\\Dropbox\\Trab\\Insper\\Metricis\\PM Rio\\RDD")

#comecar pdf
pdf( file = paste(Sys.Date(), "teste.pdf", sep=""))

# Residuo da rv_real.tx vs lv_dist
(rdplot(sim.cq$res.rv, sim.cq$lv_dist, 
        y.lim = c(-8, 13), 
        x.lim = c(-1,2), 
        y.label = "rv_real.tx (res)", 
        x.label = "lv_dist", 
        binselect = "qsmvpr",
        p = 3,
        title = "Taxa de RV por 100mil hab. vs lv_dist com controles \n (ap�s o sistema de quartis)"
))

#finalizar pdf
dev.off()

# FOO SEM O SISTEMA DE QUARTIS TODOS OS MESES

#sharp lv_real.tx com controles	

# 1	
rdrobust(
  y = sim$res.lv, 
  x = sim$lv_dist 
)

# 2
rdrobust(
  y = sim$lv_real.tx, 
  x = sim$dist,
  fuzzy = sim$lv_dist,
  covs = cbind(
    as.factor(sim$aisp),
    as.factor(sim$ano),
    as.factor(sim$mes)
  ) 
)

#fuzzy rv_real.tx com controles	
rdrobust(
  y = sim$res.rv, 
  x = sim$dist, 
  fuzzy = sim$lv_dist,
)


RDestimate(lv_real.tx ~ lv_dist, data = sim)
