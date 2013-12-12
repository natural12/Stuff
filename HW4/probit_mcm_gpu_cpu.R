library(RCUDA)
cat("Loading module...\n")
m = loadModule("rtruncnorm_expo_tail.ptx")
cat("done. Extracting kernel...\n")
k = m$truncnorm_kernel
cat("done. Setting up miscellaneous stuff...\n")

data=read.table("~/data_04.txt", header=T, quote="\"")
x=as.matrix(data[,2:9])
y=as.matrix(data[,1])



library(MASS)
library(truncnorm)



##mini_data <- read.table("~/mini_data.txt", header=T, quote="\"")
#x=as.matrix(mini_data[,2:9])
#y=as.matrix(mini_data[,1])
##


beta_0=matrix(0,8,1)
p <- length(beta_0)
sigma_0=diag(rep(0,p))
sigma_0_inv =diag(rep(0,p))



rng_a=10L
rng_b=2L


n=as.integer(nrow(data))

"compute_grid" <- function(N,sqrt_threads_per_block=16L,grid_nd=1)
{
  # if...
  # N = 1,000,000
  # => 1954 blocks of 512 threads will suffice
  # => (62 x 32) grid, (512 x 1 x 1) blocks
  # Fix block dims:
  block_dims <- c(as.integer(sqrt_threads_per_block), as.integer(sqrt_threads_per_block), 1L)
  threads_per_block <- prod(block_dims)
  if (grid_nd==1){
    grid_d1 <- as.integer(max(1L,ceiling(N/threads_per_block)))
    grid_d2 <- 1L
  } else {
    grid_d1 <- as.integer(max(1L, floor(sqrt(N/threads_per_block))))
    grid_d2 <- as.integer(ceiling(N/(grid_d1*threads_per_block)))
  }
  grid_dims <- c(grid_d1, grid_d2, 1L)
  return(list("grid_dims"=grid_dims,"block_dims"=block_dims))
}

g=compute_grid(n,16L,1)
grid_dims=g$grid_dims

block_dims =g$block_dims


cat("Grid size:\n")
print(grid_dims)
cat("Block size:\n")
print(block_dims)

nthreads <- prod(grid_dims)*prod(block_dims)
cat("Total number of threads to launch = ",nthreads,"\n")
if (nthreads < n){
  stop("Grid is not large enough...!")
}

cat("Running CUDA kernel...\n")



probit_mcmc_cpu <- function(y,x,beta_0,sigma_0_inv, niter,burnin,print.every=100) {
  
  sample=matrix(nrow=(niter+burnin),ncol=p)
  sigma_inv=sigma_0_inv +(t(x) %*% x)
  var=solve(sigma_inv)
  beta_ite=c(rep(0,8))
  z=matrix(0,n,1)
  s=matrix(1,n,1)
  a=matrix(0,n,1)
  b=matrix(0,n,1)
  v1=which(y==1)
  b[v1]=Inf
  
  v2=which(y==0)
  a[v2]=-Inf
  
  for(i in 1:(burnin+niter)) {
      
      mean= x %*% beta_ite
      mu=matrix(as.integer(mean))
     
      
      vals=matrix(0,n,1)
      
      z =.cuda(k,"vals"=vals,n,"mu"=mu,"sigma"=s, "a"=a, "b"=b,rng_a,rng_b,gridDim = grid_dims, blockDim = block_dims,outputs="vals")
          
     
    mean=var %*% (sigma_0_inv %*% beta_0 + t(x) %*% z)
    beta_ite=mvrnorm(n=1,mu=mean,Sigma=var)
    sample[i,]=beta_ite
    if(i%%print.every==0){cat(i," iterations have been completed. \n")}
  }
  
  sample[(burnin+1:niter),]  
  
}


gpu_cpu_time <- system.time({
result=probit_mcmc_cpu(y,x,beta_0,sigma_0_inv, niter=2000,burnin=500,print.every=100)
})



save(result,file="gpu_cpu_d4.rdata")
