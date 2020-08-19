##############################################################################
# Title: Quadratic Risk Premium Simulations
#        for Discussion at MFA 2020
#        "Loss Uncertainty, Gain Uncertainty, and Expected Stock Returns"
# Author: James Yae
# Date:  Aug 6, 2020
# Note: Produce figures and regression
##############################################################################

require(MASS)
## why QRPg QRPl predict returns?
muy_set =  seq(0.03,0.3,by=0.0005)/12

QRPg = rep(NA, length(muy_set))
QRPl = rep(NA, length(muy_set))

j=1
sig = 0.3/sqrt(12)
for (muy in muy_set  ) {
  
  mu = muy
  z=mu/sig
  pl = pnorm(-z) 
  pg = 1-pl
  x2 = mu*sig*dnorm(z)
  x1 =(mu^2+sig^2)
  
  El2_P = x1*pl-x2
  Eg2_P = x1*pg+x2
  
  mu = 0.02/12
  z=mu/sig
  pl = pnorm(-z) 
  pg = 1-pl
  x2 = mu*sig*dnorm(z)
  x1 =(mu^2+sig^2)
  
  El2_Q = x1*pl-x2
  Eg2_Q = x1*pg+x2
  
  QRPg[j] = El2_Q - El2_P
  QRPl[j] = Eg2_P - Eg2_Q
  
  j=j+1
}

 
plot(muy_set, QRPl, type="l", lwd =3, ylab="",main="Quadratic Risk Premium (QRP)" ,xlab = "Monthly expected return")
lines(muy_set, QRPg, lty=1)
QRP = QRPl-QRPg
lines(muy_set, QRP, lty=2)
 
 
SS = matrix(c(1,-0.25,-0.61,-0.2,1,0.87,-0.25,0.87,1),3,3)
sda = matrix(c(0.15,0.25,0.3),ncol=1)
scl = 0.0001
eall = mvrnorm(n = length(QRP), mu=rep(0,3), Sigma=scl*(sda%*%t(sda))*SS, empirical = TRUE)
mQRPg = QRPg+eall[,1]
mQRPl = QRPl+eall[,2]
mQRP = mQRPl-mQRPg

cor(mQRPg,mQRPl)
c(sd(mQRPg),sd(mQRPl),sd(mQRP))
cor(cbind(mQRPg,mQRPl,mQRP))

plot(mQRPl,mQRPg)
plot(muy_set, mQRPg)
plot(muy_set, mQRPl)
plot(muy_set, mQRP)

mQRPg_all = rep(mQRPg,12*20)
mQRPl_all = rep(mQRPl,12*20)
mQRP_all = mQRPl_all-mQRPg_all

rt = rnorm(length(muy_set)*12*20,muy_set,sig)
summary(lm(rt~mQRPg_all+mQRPl_all))
summary(lm(rt~mQRP_all))
length(muy_set)

r_set = seq(-0.1,0.1,by=0.001)
plot(r_set[r_set>=0],r_set[r_set>=0]^2,xlim=c(-0.1,0.1),
     ylim=c(-1,1)/100,type="l",main="",xlab="Stock returns", ylab="My return")
lines(r_set[r_set<0],-r_set[r_set<0]^2)

