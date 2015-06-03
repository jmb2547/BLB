BLB <- function(data,n,b,s,r) {
  
  num.cluster1 = 25
  num.runs1 = 4*num.cluster1
    
  run1 <- list()
  length(run1) <- s #need this val
    
  c1 <- makeCluster(num.cluster1)
  registerDoParallel(c1)
  
  subsamples <- matrix(,nrow=s,ncol=trunc(b))
  quality.estimates <- matrix(,nrow=s,ncol=7)
  stats.matrix <- matrix(,nrow=s,ncol=r)
  
  perm.results.sim <- foreach(i=1:s, .combine='rbind') %dopar%{
    subsamples[i,] <- sample(data,b,replace=F)
    stats <- c()
    for (j in 1:r) {
      resample.vector <- rmultinom(1,n,rep(1/b,b))
      stats[j] <- sum(resample.vector*subsamples[i,])/n
    }
    stats.matrix[i,] <- stats
    quality.estimates[i,] <- quantile(stats,c(0.005,.025,0.05,.5,.95,.975,.995))
  }
  final.quality.estimate = apply(perm.results.sim, 2, mean)
  return(final.quality.estimate)
}
