source("~/Documents/Pomona College/SURP/bootstrap/BLBfunction.R")

library(foreach)
library(doParallel)

#parameters
n <- 10000
b <- n^.5
s <- 100
r <- 10000

output.matrix <- matrix(,nrow=100,ncol=11)

for (i in 1:100) {
  data <- rnorm(n,0,1)
  BLB.output <- BLB(data,n,b,s,r)
  output.matrix[i,1:7] <- BLB.output
}

interval.length <- output.matrix[,6]-output.matrix[,2]
caught.mu <- ifelse(((output.matrix[,2] <= 0) & (output.matrix[,6] >= 0)),1,0)
below.mu <- ifelse(output.matrix[,6] < 0,1,0)
above.mu <- ifelse(output.matrix[,2] > 0,1,0)

output.matrix[,8:11] <- cbind(interval.length, caught.mu, below.mu, above.mu)
colnames(output.matrix) <- c('0.005','0.025','0.05','0.5','0.95','0.975','0.995',
                             'Length','Caught mu','below mu','above mu')

write.table(output.matrix,file='BLB_n10000_b100_s100_r10000',row.names=F,col.names=T,sep=",")
