

#	DEFINICIO DELS PARMAETRES DE LA DISTRIBUCIO A PRIORI

prior.C <-c(alpha = 42, beta = 28)
prior.E <-c(alpha = 64, beta = 16)



#	DIBUIX DE LA DISTRIBU DISTRIBUCIO A PRIORI

suport <- seq(from = 0, to = 1, length = 100)
dist.prior.C <- dbeta(suport, prior.C[1], prior.C[2])
dist.prior.E <- dbeta(suport, prior.E[1], prior.E[2])

par(mfrow=c(1,1))

plot(suport,dist.prior.C, ylab="", xlab = "theta", ty="l", ylim=c(0, max(dist.prior.C,dist.prior.E)))
 lines(suport, dist.prior.E, lty=2)
 title("Distribucions a priori")
 legend("topleft",c("Tr. Convencional","Tr. Experimental"), lty=c(1,2))


#	DADES

N <- 40
y.C <- 24
y.E <- 30

#####	DISTRIBUCIO A POSTERIORI

posterior.C <- c(alpha = prior.C[1] + y.C, beta = prior.C[2] + (N-y.C))
posterior.E <- c(alpha = prior.E[1] + y.E, beta = prior.E[2] + (N-y.E))




#####	DIBUIX DE LA DISTRIBU DISTRIBUCIO A PRIORI, A POSTERIORI I LA VERSEMBLANÇA

dist.posterior.C <- dbeta(suport,posterior.C[1],posterior.C[2])
dist.posterior.E <- dbeta(suport,posterior.E[1],posterior.E[2])

####### VERSEMBLANÇA reescalada   ###########################################

vers.C <- dbinom(y.C,N,suport)/integrate(function(x)dbinom(y.C,N,x), lower = 0, upper = 1, subdivisions=1000)$value
vers.E <- dbinom(y.E,N,suport)/integrate(function(x)dbinom(y.E,N,x), lower = 0, upper = 1, subdivisions=1000)$value


par(mfrow=c(1,2))

plot(suport,dist.prior.C, ylab="", xlab=expression(theta),type="l", ylim=c(0, max(dist.posterior.C,dist.posterior.E)), lty=2)
 lines(suport,dist.posterior.C)
 lines(suport,vers.C, lty=3)
 legend("topleft", c("a priori","a posteriori","versemblança"), lty = c(2,1,3))
 title("Tractament Convencional")


plot(suport,dist.prior.E, ylab="", xlab=expression(theta),type="l", ylim=c(0, max(dist.posterior.C,dist.posterior.E)), lty=2)
 lines(suport,dist.posterior.E)
 lines(suport,vers.E, lty=3)
 legend("topleft", c("a priori","a posteriori","versemblança"), lty = c(2,1,3))
 title("Tractament Experimental")




#########  SORTIDA de resultats

sortida.C <- matrix(nrow = 7, ncol = 2)

colnames(sortida.C) <- c('priori', 'posteriori')
rownames(sortida.C) <- c('alpha', 'beta', 'mitjana', 'variança', '2,5%', 'mediana', '97.5%')

sortida.C[1:2, 1] <- prior.C
sortida.C[3, 1] <- prior.C[1]/(prior.C[1] + prior.C[2])
sortida.C[4, 1] <- (prior.C[1]*prior.C[2])/(((prior.C[1]+prior.C[2])^2)*(prior.C[1]+prior.C[2]+1))
sortida.C[5, 1] <- qbeta(0.025, prior.C[1], prior.C[2])
sortida.C[6, 1] <- qbeta(0.5, prior.C[1], prior.C[2])
sortida.C[7, 1] <- qbeta(0.975, prior.C[1], prior.C[2])

sortida.C[1:2, 2] <- posterior.C
sortida.C[3, 2] <- posterior.C[1]/(posterior.C[1] + posterior.C[2])
sortida.C[4, 2] <- (posterior.C[1]*posterior.C[2])/(((posterior.C[1]+posterior.C[2])^2)*(posterior.C[1]+posterior.C[2]+1))
sortida.C[5, 2] <- qbeta(0.025, posterior.C[1], posterior.C[2])
sortida.C[6, 2] <- qbeta(0.5, posterior.C[1], posterior.C[2])
sortida.C[7, 2] <- qbeta(0.975, posterior.C[1], posterior.C[2])

round(sortida.C, 3)



sortida.E <- matrix(nrow = 7, ncol = 2)

colnames(sortida.E) <- c('priori', 'posteriori')
rownames(sortida.E) <- c('alpha', 'beta', 'mitjana', 'variança', '2,5%', 'mediana', '97.5%')

sortida.E[1:2, 1] <- prior.E
sortida.E[3, 1] <- prior.E[1]/(prior.E[1] + prior.E[2])
sortida.E[4, 1] <- (prior.E[1]*prior.E[2])/(((prior.E[1]+prior.E[2])^2)*(prior.E[1]+prior.E[2]+1))
sortida.E[5, 1] <- qbeta(0.025, prior.E[1], prior.E[2])
sortida.E[6, 1] <- qbeta(0.5, prior.E[1], prior.E[2])
sortida.E[7, 1] <- qbeta(0.975, prior.E[1], prior.E[2])

sortida.E[1:2, 2] <- posterior.E
sortida.E[3, 2] <- posterior.E[1]/(posterior.E[1] + posterior.E[2])
sortida.E[4, 2] <- (posterior.E[1]*posterior.E[2])/(((posterior.E[1]+posterior.E[2])^2)*(posterior.E[1]+posterior.E[2]+1))
sortida.E[5, 2] <- qbeta(0.025, posterior.E[1], posterior.E[2])
sortida.E[6, 2] <- qbeta(0.5, posterior.E[1], posterior.E[2])
sortida.E[7, 2] <- qbeta(0.975, posterior.E[1], posterior.E[2])

round(sortida.E, 3)



#  DIBUIX DE LA PREDICTIVA A PRIORI I A POSTERIORI  #####


n.sim <- 10000000

z.prior.C <- rbeta(n.sim, prior.C[1], prior.C[2])
pre.prior.C <- rbinom(n.sim, 100, z.prior.C)

z.posterior.C <- rbeta(n.sim, posterior.C[1], posterior.C[2])
pre.posterior.C <- rbinom(n.sim, 100, z.posterior.C)


z.prior.E <- rbeta(n.sim, prior.E[1], prior.E[2])
pre.prior.E <- rbinom(n.sim, 100, z.prior.E)

z.posterior.E <- rbeta(n.sim, posterior.E[1], posterior.E[2])
pre.posterior.E <- rbinom(n.sim, 100, z.posterior.E)


par(mfrow=c(2, 2))

plot(table(pre.prior.C), xlim=c(0, 100), main="pre. priori Tr. Convencional (n=100)")
plot(table(pre.prior.E), xlim=c(0, 100), main="pre. priori Tr. Experimental (n=100)")
plot(table(pre.posterior.C), xlim=c(0, 100), main="pre. posteriori Tr. Convencional (n=100)")
plot(table(pre.posterior.E), type="h", xlim=c(0, 100), main="pre. posteriori Tr. Convencional (n=100)")


#    DIFERENCIA DE PROPORCIONS 

#Resoldrem el problema mitjançant simulacio pq ja diferencia de Betes no es Beta

par(mfrow=c(1,1))
dif<-z.posterior.E-z.posterior.C
plot(density(dif),main="Dist.Diferències",xlab="")

#La probabilitat que la diferencia sigui mes gran que 0.1
#Com no disposem de funció, hem d'usar simulacions

prob<-sum(dif>0.1)/n.sim

#Es evident que un tractament és molt millor
	

# ODDS RATIO

#OR vol dir que els tractaments són iguals



OR<-(z.posterior.E/(1-z.posterior.E))/(z.posterior.C/(1-z.posterior.C))
plot(density(OR),xlab="")
abline(v=1,col="red")
abline(v=c(quantile(OR,0.025),quantile(OR,0.975)),col="blue")













