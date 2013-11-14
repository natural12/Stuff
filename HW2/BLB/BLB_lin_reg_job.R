
args <- commandArgs(TRUE)

cat("Command-line arguments:\n")
print(args)

####
# sim_start ==> Lowest possible dataset number
###

###################
sim_start <-0
###################

if (length(args)==0){
  sim_num <- sim_start + 1
  set.seed(121231)
} else {
  # SLURM can use either 0- or 1-indexing...
  # Lets use 1-indexing here...
  sim_num <- sim_start + as.numeric(args[1])
  sim_seed <- (762*(sim_num-1) + 121231)
}

cat(paste("\nAnalyzing dataset number ",sim_num,"...\n\n",sep=""))


library(BH)
library(bigmemory.sri)
library(bigmemory)
library(biganalytics)





# Attach big.matrix :

fulldata <- attach.big.matrix("/home/pdbaines/data/blb_lin_reg_data.desc", header=F)


# Remaining BLB specs:
gamma=0.7

n=nrow(fulldata)

b=as.integer(n^gamma)

s=5


## get s_index and r_index by extracting job number
s_index=ceiling(sim_num/50)
r_index=sim_num%%50

if (r_index==0){
  r_index=50
}



# Extract the subset:
set.seed(s_index)
index=sample(n,size=b,replace=FALSE)
subset=fulldata[index,]




# Bootstrap dataset:
rm(list=".Random.seed", envir=globalenv()) 
bootstrap=rmultinom(1,n,prob=rep(1/b,b))


# Fit lm:
column=ncol(subset)
x = as.matrix(subset[,1:(column-1)])
y=as.matrix(subset[,(column)])

fit=lm(y~-1+x,weights=bootstrap/n)


# Output file:

outfile = paste0("output/","coef_",sprintf("%02d",s_index),"_",sprintf("%02d",r_index),".txt")


write.table(fit$coefficients, file=outfile,sep="")


