library(mvtnorm)
library(coda)

########################################################################################
########################################################################################
## Handle batch job arguments:

# 1-indexed version is used now.
args <- commandArgs(TRUE)

cat(paste0("Command-line arguments:\n"))
print(args)

####
# sim_start ==> Lowest simulation number to be analyzed by this particular batch job
###

#######################
sim_start <- 1000
length.datasets <- 200
#######################

if (length(args)==0){
  sinkit <- FALSE
  sim_num <- sim_start + 1
  set.seed(1330931)
} else {
  # Sink output to file?
  sinkit <- TRUE
  # Decide on the job number, usually start at 1000:
  sim_num <- sim_start + as.numeric(args[1])
  # Set a different random seed for every job number!!!
  set.seed(762*sim_num + 1330931)
}

# Simulation datasets numbered 1001-1200
fileno <- sim_num

data <- read.table(file=paste("/home/natural12/STA250/Stuff/HW1/BayesLogit/data/blr_data_",fileno,".csv",sep=""), header=TRUE, sep=",")

########################################################################################
########################################################################################
#Set up the specifications:

y <- data[,1]
m <- data[,2]
x <- as.matrix(data[,3:4])
beta.0 <- c(0,0)
p <- length(beta.0)
mu.0=t(c(0,0))
Sigma.0.inv <- diag(rep(1,p))
beta.initial <- c(1,1) # initial values of sampling algorithm
result <- matrix(rep(0,11000*p),nrow=(11000),ncol=p)


library(MASS)


logalpha=function(beta_star,beta_t){
  logstar= t(y)%*%(x%*%beta_star) -t(m)%*%log(1+exp(x%*%beta_star)) -0.5*t(beta_star)%*%Sigma.0.inv%*%beta_star
  logt= t(y)%*%(x%*%beta_t) -t(m)%*%log(1+exp(x%*%beta_t)) -0.5*t(beta_t)%*%Sigma.0.inv%*%beta_t
  
  a=min(0,logstar-logt)
  a  
}


## Funciton start##

"bayes.logreg" <- function(m,y,x,beta.0,Sigma.0.inv,niter=10000,burnin=1000,
                           print.every=1000,retune=100,verbose=TRUE)
{
  sample=matrix(nrow=(niter+burnin),ncol=p)
  beta_t=beta.initial
  w=1
  sigma.w=diag(rep(1,p))
  count=0
  count.ite=0
  
  for(i in 1:(burnin+niter)){
    beta_star=c(0,0)
    beta_star= mvrnorm(n=1,mu=beta_t,Sigma=w*sigma.w)
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
      if(accept.rate<0.3) w=w/sqrt(10)
      if(accept.rate>0.6) w=w*sqrt(10)
      cat("Acceptance Rate after", i, "iterations during burnin period", accept.rate, "\n")
      cat("Tuning Parameters after", i, "iterations during burnin period: ", w, "\n")
      count <- 0
    }
    if(i%%print.every==0){cat(i," iterations of MH in Gibbs have been completed. \n")}
 
  } #for loop end
  
  
  cat("Acceptance Rate after Burnin is ", count.ite/niter, "\n")
  cat("Tuning Parameter After Burnin is: ", w, "\n") 
  

sample[(burnin+1:niter),]
  
  
} ## function end




result <- bayes.logreg(m,y,x,beta.0,Sigma.0.inv,niter=10000,burnin=1000,print.every=1000,retune=100,verbose=FALSE)


Beta.percentile <- matrix(rep(0,99*p),nrow=99,ncol=2)
for(j in 1:p){
  Rj <- sort(result[,j])
  for(i in 1:99){Beta.percentile[i,j] <- Rj[100*i]}
}


write.table(BetaPercentile, file=paste("/home/natural12/STA250/Stuff/HW1/BayesLogit/results/blr_res_",fileno,".csv",sep=""),
            row.names=FALSE,col.names=FALSE,sep=",")




#################################################

# Read data corresponding to appropriate sim_num:

# Extract X and y:

# Fit the Bayesian model:

# Extract posterior quantiles...

# Write results to a (99 x p) csv file...

# Go celebrate.

cat("done. :)\n")
