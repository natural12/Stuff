library(MASS)
library(truncnorm)

data=read.table("~/data_02.txt", header=T, quote="\"")
x=as.matrix(data[,2:9])
y=as.matrix(data[,1])


#
mini_data <- read.table("~/mini_data.txt", header=T, quote="\"")
x=as.matrix(mini_data[,2:9])
y=as.matrix(mini_data[,1])

beta_0=matrix(0,8,1)
p <- length(beta_0)
sigma_0=diag(rep(0,p))
sigma_0_inv =diag(rep(0,p))


probit_mcmc_cpu <- function(y,x,beta_0,sigma_0_inv, niter,burnin) {
  
  sample=matrix(nrow=(niter+burnin),ncol=p)
  sigma_inv=sigma_0_inv +(t(x) %*% x)
  var=solve(sigma_inv)
  beta_ite=c(rep(0,8))
  n=nrow(y)
  z=matrix(0,n,1)
  a=matrix(0,n,1)  #lower bound
  b=matrix(0,n,1)  #upper bound
  v1=which(y==1)
  b[v1]=Inf
  v2=which(y==0)
  a[v2]=-Inf
  
  for(i in 1:(burnin+niter)) {
      
      mean= x %*% beta_ite   ## mean for z    
      
      z =rtruncnorm(n,a=a,b=b,mean,sd=1)
      
      mean=var %*% (sigma_0_inv %*% beta_0 + t(x) %*% z)  ## mean for posterior beta
      beta_ite=mvrnorm(n=1,mu=mean,Sigma=var)
      sample[i,]=beta_ite
      
  }
  
sample[(burnin+1:niter),]  
  
}

cpu_time <- system.time({
  result=probit_mcmc_cpu(y,x,beta_0,sigma_0_inv, niter=2000,burnin=500)
})

save(result,file="d_5_CPU.rdata")

save(d_2,file="~/d_2_mcmc_CPU.dat")
load("gpu_cpu_d4.rdata")
d4=result


pars02=read.table("~/pars_02.txt", header=T, quote="\"")

par(mfrow=c(3,3))
for (i in 1:8)
{
  plot(d2[,i])
  lines(1:2500,rep(pars02[i,],2500),col=2)
  abline(h=mean(d2[,i]),col="blue")
}

k=matrix(0,8,1)
for(i in 1:8){
  k[i,]=mean(d4[,i])  
}

