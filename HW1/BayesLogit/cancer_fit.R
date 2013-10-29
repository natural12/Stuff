#Set up the specifications:

response <- as.matrix(data[,11])
y=matrix(rep(0,569),nrow=569,ncol=1)
for (i in 1:569){
  if (response[i,]=="M"){
    y[i,]=1
  }
  else {y[i,]=0}
}

x <- as.matrix(data[,1:10])
beta.0 <- c(rep(0,11))
p <- length(beta.0)
mu.0=t(c(rep(0,11)))
Sigma.0.inv <- diag(rep(1000,p))
beta.initial <- c(rep(1,11)) # initial values of sampling algorithm

result <- matrix(rep(0,11000*p),nrow=(11000),ncol=p)

glm.ca=glm(y~x,family=binomial())

library(MASS)


logalpha=function(beta_star,beta_t){
  logstar= t(y)%*%(x%*%beta_star) -t(m)%*%log(1+exp(x%*%beta_star)) -0.5*t(beta_star)%*%Sigma.0.inv%*%beta_star
  logt= t(y)%*%(x%*%beta_t) -t(m)%*%log(1+exp(x%*%beta_t)) -0.5*t(beta_t)%*%Sigma.0.inv%*%beta_t
  
  a=min(0,logstar-logt)
  a  
}
