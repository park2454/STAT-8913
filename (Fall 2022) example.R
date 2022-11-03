cusum = function(x, abs=TRUE){
  T = length(x)
  if(abs==FALSE){
    res = rep(NA,T-1)
    for (t in 1:T-1){
      res[t] = sqrt(t*(T-t)/T)*abs(mean(x[1:t]) - mean(x[-(1:t)]))
    }
  } else {
    res = rep(NA,T-1)
    for(t in 1:T-1){
      res[t] = sqrt(t*(T-t)/T)*(mean(x[-(1:t)]) - mean(x[1:t]))
    }
  }
  return(res)
}

set.seed(1)
Y = matrix(rnorm(1000*100),1000,100)
Y[1:500,1] = Y[1:500,1] + 1
out = apply(Y,2,function(x){return(cusum(x,abs=FALSE))})
cusum_max = apply(out,1,max)
cusum_avg = apply(out,1,mean)
pdf("example1.pdf", 8, 6)
par(mfrow=c(2,2))
plot(Y[,1],type="l", main="Change present")
abline(v=500, lty=2)
plot(Y[,100],type="l", main="Change not present")
plot(cusum_max, type="l", main="CUSUM Max")
abline(v=500, lty=2)
abline(v=which.max(cusum_max), lty=2, col="red")
plot(cusum_avg, type="l", main="CUSUM Average")
abline(v=500, lty=2)
abline(v=which.max(cusum_avg), lty=2, col="red")
dev.off()

set.seed(1)
Y = matrix(rnorm(1000*100),1000,100)
Y[1:500,1:50] = Y[1:500,1:50] + 0.1
out = apply(Y,2,function(x){return(cusum(x,abs=FALSE))})
cusum_max = apply(out,1,max)
cusum_avg = apply(out,1,mean)
pdf("example2.pdf",8,6)
par(mfrow=c(2,2))
plot(Y[,1],type="l", main="Change present")
abline(v=500, lty=2)
plot(Y[,100],type="l", main="Change not present")
plot(cusum_max, type="l", main="CUSUM Max")
abline(v=500, lty=2)
abline(v=which.max(cusum_max), lty=2, col="red")
plot(cusum_avg, type="l", main="CUSUM Average")
abline(v=500, lty=2)
abline(v=which.max(cusum_avg), lty=2, col="red")
dev.off()

set.seed(1)
Y = matrix(rnorm(1000*100),1000,100)
Y[1:500,1:5] = Y[1:500,1:5] + 0.2
out = apply(Y,2,cusum)
cusum_max = apply(out,1,max)
cusum_avg = apply(out,1,mean)
SVD = svd(out, nu=1, nv=1)
a = SVD$v
cusum_proj = cusum(Y %*% a, abs=FALSE)

pdf("example3.pdf",12.6)
par(mfrow=c(2,3))
plot(cusum_max,type="l", main="CUSUM Max")
abline(v=500, lty=2)
abline(v=which.max(cusum_max), lty=2, col="red")
plot(cusum_avg,type="l", main="CUSUM Average")
abline(v=500, lty=2)
abline(v=which.max(cusum_avg), lty=2, col="red")
plot(cusum_proj,type="l", main="CUSUM Projection")
abline(v=500, lty=2)
abline(v=which.max(cusum_proj), lty=2, col="red")

for(thres in 1:3){
  sum_thr = function(x){
    sum(x*(x > thres))
  }
  cusum_thr = apply(out,1,sum_thr)
  plot(cusum_thr,type="l", main=paste0("CUSUM Threshold=",thres))
  abline(v=500, lty=2)
  abline(v=which.max(cusum_thr), lty=2, col="red")
}
dev.off()