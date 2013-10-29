
#Set up the specifications:

response = as.matrix(data[,11])
y=matrix(rep(0,569),nrow=569,ncol=1)
for (i in 1:569){
  if (response[i,]=="M"){
    y[i,]=1
  }
  else {y[i,]=0}
}

x = as.matrix(cbind(rep(1,569),data[,1:10]))

beta.0 = c(rep(0,11))
p = length(beta.0)
m=c(rep(1,569))
mu.0=t(c(rep(0,11)))
Sigma.0.inv = diag(rep(1/1000,p))

result = matrix(rep(0,11000*p),nrow=(11000),ncol=p)




glm.ca=glm(y~-1+x,family=binomial())

beta.initial <- glm.ca$coefficient # initial values of sampling algorithm



v=vcov(glm.ca)

library(MASS)


logalpha=function(beta_star,beta_t){
  logstar= t(y)%*%(x%*%beta_star) -sum(log(1+exp(x%*%beta_star))) -0.5*t(beta_star)%*%Sigma.0.inv%*%beta_star
  logt= t(y)%*%(x%*%beta_t) -sum(log(1+exp(x%*%beta_t))) -0.5*t(beta_t)%*%Sigma.0.inv%*%beta_t
  
  a=min(0,logstar-logt)
  a  
}

## Function start##

"bayes.logreg" <- function(m,y,x,beta.0,Sigma.0.inv,niter=10000,burnin=1000,
                           print.every=1000,retune=100,verbose=TRUE)
{
  sample=matrix(nrow=(niter+burnin),ncol=p)
  beta_t=beta.initial
  w=0.5
  count=0
  count.ite=0   
  
  for(i in 1:(burnin+niter)){
    beta_star=c(rep(0,11))
    
    beta_star= mvrnorm(n=1,mu=beta_t,Sigma=w*v)
    
    lalpha=logalpha(beta_star,beta_t)
    
    U=runif(1,min=0,max=1)
    
    if (log(U)<lalpha){
      beta_t=beta_star
      
      sample[i,]=beta_star
      
      count=count+1
      
      if(i>burnin){count.ite=count.ite+1}
    }
    if (log(U)>lalpha){sample[i,]=beta_t}
    
    
    if (i%% retune==0 && i<=burnin){
      accept.rate=count/retune
      
      if(accept.rate<0.3) w=w/sqrt(5)
      
      if(accept.rate>0.6) w=w*sqrt(5)
      
      cat("Acceptance Rate after", i, "iterations during burnin period", accept.rate, "\n")
      cat("Tuning Parameters after", i, "iterations during burnin period: ", w, "\n")
      
      count <- 0
    }
    if(i%%print.every==0){cat(i," iterations of MH have been completed. \n")}
    
  } #for loop end
  
  
  cat("Acceptance Rate after Burnin is ", count.ite/niter, "\n")
  cat("Tuning Parameter After Burnin is: ", w, "\n") 
  
  
  sample[(burnin+1:niter),]
  
  
} ## function end




result <- bayes.logreg(m,y,x,beta.0,Sigma.0.inv,niter=10000,burnin=1000,print.every=1000,retune=100,verbose=FALSE)


# see convergence ###
par(mfrow=c(3,4))
for(i in 1:p){
  plot(result[,i],type='l')
}


## b) autocorrelation ##
g=matrix(nrow=11,ncol=1)
for (i in 1:11){
 g[i]=acf(result[,i],plot=FALSE)$acf[2]
}


## c) check 95% credible intervals of beta, which seems to related to response##
g=matrix(nrow=11,ncol=2)
for(j in 1:p){
  k<- sort(result[,j])
  q1=k[250]
  q3=k[9750]
  g[j,]=c(q1,q3)
}

par(mfrow=c(4,3))
for(i in 1:p){
  hist(result[,i],main=paste("Histogram of Beta_",(i-1)),xlab=paste("Beta_",(i-1)))
  abline(v=g[i,],col="red")
}



## d) predictive prosterior check##
h=1:10000
g=sample(h,1000)
s=matrix(nrow=1000,ncol=1)

for (i in 1:1000){
  s[i,]=mean(rbinom(569,1,prob=((exp(x%*% result[g[i],]))/(1+exp(x%*% result[g[i],])))))
}


truemean=mean(y)

par(mfrow=c(1,1))
hist(s,main="Histogram of 1000 means from 1000 samples",xlab="mean")
abline(v=truemean,col="red")

