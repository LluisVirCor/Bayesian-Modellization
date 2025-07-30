
library(R2WinBUGS)


#################  HOSPITALS  ###################

y <- c(0, 18, 8, 46, 8, 13, 9, 31, 14, 8, 29, 24)
n <- c(47, 148, 119, 810, 211, 196, 148, 215, 207, 97, 256, 360)

p.obs <- y/n

t <- length(y) # grandaria mostra


Iter <- 2000
Burn <- 800
Thin <- 4
Chain <- 2
### en llença "Burn", i despres se'n queda "Iter" que les tria cada "Thins", es a dir en simmula Burn+Iter*Thin per 
### a cada cadena, nombre de cadenes es "Chain"




#### MODEL HospitalA   ######################

dades <- list(t=t, y = y, n = n)
inicials <- list(list(p=0.9),list(p=0.1))
parametres <- c("p")

 HospitalA <- bugs(dades, inicials, parameters.to.save=parametres, 
			model="PATH/HospitalA.bug",
			 bugs.directory="C:/Archivos de programa/WinBUGS14/",
			 n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, debug=T,
			 working.directory = "W:\\")



attach.bugs(HospitalA)

par(mfrow=c(1,1))

plot(1:t, p.obs, ylab="p", xlab="hospital", main="Model A", ty="n", ylim=c(0,0.2))
for ( i in 1:t) {
 boxplot(p, outline = F, col = "green", add = T, at = i)
}

detach.bugs()


#### MODEL HospitalB   ######################

dades <- list(t=t, y = y, n = n)
inicials <- list(list(p=rep(0.9,t)),list(p=rep(0.1,t)))
parametres <- c("p")

 HospitalB <- bugs(dades, inicials, parameters.to.save=parametres, 
			model="PATH/HospitalB.bug",
			 bugs.directory="C:/Archivos de programa/WinBUGS14/",
			 n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, debug=F,
			 working.directory = "W:\\")



attach.bugs(HospitalB)

par(mfrow=c(1,1))
plot(1:t, p.obs, ylab="p", xlab="hospital", main="Model B", ty="n", ylim=c(0,0.25))
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
}

detach.bugs()



#### MODEL HospitalC   ######################

dades <- list(t=t, y = y, n = n)
inicials <- list(list(p=rep(0.9,t),a=1,b=1),list(p=rep(0.1,t),a=1,b=1))
parametres <- c("p","a","b")

 HospitalC <- bugs(dades, inicials, parameters.to.save=parametres, 
			model="PATH/HospitalC.bug",
			 bugs.directory="C:/Archivos de programa/WinBUGS14/",
			 n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, debug=T,
			 working.directory = "W:\\")



attach.bugs(HospitalC)

par(mfrow=c(1,1))
plot(1:t, p.obs, ylab="p", xlab="hospital", main="Model C", ty="n", ylim=c(0,0.25))
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
}
detach.bugs()



####  REPRESENTEM ELS MODELS A B I C

par(mfrow=c(1,3))

attach.bugs(HospitalA)
plot(1:t, p.obs, xlab="hospital", ylab="p", main="Model A", ty="n", ylim=c(0,0.25))
for ( i in 1:t) {
 boxplot(p, outline = F, col = "green", add = T, at = i)
}
detach.bugs()

attach.bugs(HospitalB)
plot(1:t, p.obs, xlab="hospital", ylab="p", main="Model B", ty="n", ylim=c(0,0.25))
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
}
detach.bugs()

attach.bugs(HospitalC)
plot(1:t, p.obs, xlab="hospital", ylab="p", main="Model C", ty="n", ylim=c(0,0.25))
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
}
detach.bugs()




######################################################################


#### apartat d) 

####  VALIDACIÓ amb la predictiva a posteriori per al MODEL A i C  ####


attach.bugs(HospitalA)
y.pred.A <- NULL
 for (j in 1:(Iter*Chain)) {
  y.pred.aux <- rep(0, t)
  for (i in 1:t) {
   y.pred.aux[i] <- rbinom(1, n[i], p[j])/n[i]
  }
 y.pred.A <- rbind(y.pred.A, y.pred.aux)
}
detach.bugs()

attach.bugs(HospitalC)
y.pred.C <- NULL
 for (j in 1:(Iter*Chain)) {
  y.pred.aux <- rep(0, t)
  for (i in 1:t) {
   y.pred.aux[i] <- rbinom(1, n[i], p[j,i])/n[i]
  }
 y.pred.C <- rbind(y.pred.C, y.pred.aux)
}
detach.bugs()


par(mfrow=c(1,2))

plot(1:t, p.obs, ylab="y", xlab="type", main="Model A", pch=19, ylim=c(0,0.2))
for ( i in 1:t) {
boxplot(y.pred.A[,i], outline = F, col = "green", add = T, at = i)
}
points(1:t, p.obs, pch=19,cex=2)
#points(1:t, apply(y.pred.A,2,mean), pch=19,cex=2, col="blue")

plot(1:t, p.obs, ylab="y", xlab="type", main="Model C", pch=19, ylim=c(0,0.2))
for ( i in 1:t) {
boxplot(y.pred.C[,i], outline = F, col = "green", add = T, at = i)
}
points(1:t, p.obs, pch=19,cex=2)




######################################################################



####### f)  PARAMETRITZACIÓ ALTERNATIVA  ######################


#### MODEL HospitalC2   ######################



dades <- list(t=t, y = y, n = n)
inicials <- list(list(tau=1),list(tau=1))
parametres <- c("p","mu","tau")

 HospitalC2 <- bugs(dades, inicials, parameters.to.save=parametres, 
			model="PATH/HospitalC2.bug",
#			model="PATH/HospitalC2.bug",
#			model="PATH/HospitalC2.bug",
			 bugs.directory="C:/Archivos de programa/WinBUGS14/",
			 n.iter=(Iter*Thin+Burn),n.burnin=Burn, n.thin=Thin, n.chains=Chain, debug=F)
			# working.directory = "W:\\")


par(mfrow=c(1,2))

attach.bugs(HospitalC)
plot(1:t, p.obs, ylab="p", xlab="type", main="Model C", pch=19, ylim=c(0,0.2), ty="n")
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
}
detach.bugs()


attach.bugs(HospitalC2)
plot(1:t, p.obs, ylab="p", xlab="type", main="Model C2", pch=19, ylim=c(0,0.2), ty="n")
for ( i in 1:t) {
 boxplot(p[,i], outline = F, col = "green", add = T, at = i)
}
detach.bugs()



