library(RCUDA)

cat("Setting cuGetContext(TRUE)...\n")
cuGetContext(TRUE)
cat("done. Profiling CUDA code...\n")

cat("Loading module...\n")
m = loadModule("rtruncnorm_tail_twosided.ptx")
cat("done. Extracting kernel...\n")
k = m$truncnorm_kernel
cat("done. Setting up miscellaneous stuff...\n")
n = 1E4L

vals=matrix(0,n,1)
cat("done. Setting mu and sigma...\n")
mu = matrix(5,n,1)
sigma = matrix(1,n,1)
a=matrix(-10E6,n,1)
b=matrix(0,n,1)

rng_a=10L
rng_b=5L

# if...
# N = 1,000,000
# => 1954 blocks of 512 threads will suffice
# => (62 x 32) grid, (512 x 1 x 1) blocks

# Fix block dims:
threads_per_block <- 512L
block_dims <- c(threads_per_block, 1L, 1L)
grid_d1 <- floor(sqrt(n/threads_per_block))
grid_d2 <- ceiling(n/(grid_d1*threads_per_block))
grid_dims <- c(grid_d1, grid_d2, 1L)

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


vals=matrix(0,n,1)

cu_time <- system.time({
  cat("Copying random N(0,1)'s to device...\n")
  mem = copyToDevice(vals)
  .cuda(k, mem,n,"mu"=mu,"sigma"=sigma, "a"=a, "b"=b,rng_a,rng_b,gridDim = grid_dims, blockDim = block_dims)
  cat("Copying result back from device...\n")
  cu_ret = copyFromDevice(obj=mem,nels=mem@nels,type="float")
})



library(truncnorm)
r_time <- system.time({
  r_ret <- rtruncnorm(n,a=-10E6,b=0,mean=5,sd=1)
})

cat("done. Finished profile run! :)\n")

# Not the best comparison but a rough real-world comparison:
cat("CUDA time:\n")
print(cu_time)

cat("R time:\n")
print(r_time)

# Differences due to floating point vs. double...
tdiff <- sum(abs(cu_ret - r_ret))
cat("Difference in RCUDA vs R results = ",tdiff,"\n")

cat("Differences in first few values...\n")
print(abs(diff(head(cu_ret)-head(r_ret))))

cat("Differences in last few values...\n")
print(abs(diff(tail(cu_ret)-tail(r_ret))))
