source("honestRpart.R")

pred <- function(X) {
  return(X[, 1] + 3*X[, 2] + X[, 3]^4 + X[, 4]*X[, 5] + 1*(X[, 6]<0.5) + (X[, 7] < 0.5)*(X[, 8]))
}

#### Trial 1 ####

sim <- function(dummy, xtest, d, n, subsample, leaf.size, ntree) {
  ans <- c()
  for (iter in 1:50) {
    cat("Iter:", iter, "\n")
    X <- matrix(runif(d*n), nrow=n)
    Y <- pred(X) + runif(n, -1, 1)
    blv <- boulevard(X, Y, ntree=ntree, subsample=subsample, 
                     leaf.size=leaf.size, method="random") 
    ans <- cbind(ans, predict.boulevard(blv, xtest))
  }
}

#### Trial 2 ####
d <- 8
n <- 10000
subsample <- 0.5
leaf.size <- 30
ntree <- 500

ntest <- 10
xtest <- matrix(runif(d*ntest), nrow=ntest)
ans2 <- c()
for (iter in 1:50) {
  cat("Iter:", iter, "\n")
  X <- matrix(runif(d*n), nrow=n)
  Y <- pred(X) + runif(n, -1, 1)
  blv <- boulevard(X, Y, ntree=ntree, subsample=subsample, 
                   leaf.size=leaf.size, method="random") 
  ans2 <- cbind(ans2, predict.boulevard(blv, xtest))
}


#### Trial 3 ####
d <- 8
n <- 10000
subsample=0.5
leaf.size <- 30
ntree <- 500

ntest <- 10
xtest <- matrix(runif(d*ntest), nrow=ntest)
ans3 <- c()
for (iter in 1:50) {
  cat("Iter:", iter, "\n")
  X <- matrix(runif(d*n), nrow=n)
  Y <- pred(X) + rnorm(n)
  blv <- boulevard(X, Y, ntree=ntree, subsample=subsample, 
                   leaf.size=leaf.size, method="random") 
  ans3 <- cbind(ans3, predict.boulevard(blv, xtest))
}


#### Analyzing ####
vmat <- matrix(0, ncol=5, nrow=4)
for (iter in 1:4) {
  mat <- c()
  for (i in 1:1000) {
    mat <- rbind(mat, limiting[[iter]][[i]])
  }
  boxplot(mat)
  for (i in 1:5) {
    vmat[iter, i] = var(mat[,i])
    print(ks.test(scale(mat[, i]), pnorm))
  }
  plot(density(mat[,1], bw=0.1), col=1, xlim=c(0.4,1.8), ylim=c(0,4), main="")
  lines(density(mat[,2], bw=0.1))
  lines(density(mat[,3], bw=0.1))
  lines(density(mat[,4], bw=0.1))
  lines(density(mat[,5], bw=0.1))
}

print(apply(vmat, c(1), mean))

#### Analyzing (New) ####
color <- c(1,2,3,4,5,1,2,3,4,5)
load("/Users/yichenzhou/Box Sync/Boulevard/Paper/Boulevard/Simulation/result_c_180415220056.RData")
setEPS()
postscript("ld_1.eps", width=6, height=4.5)
plot(NA, xlab="N=1000, Bandwidth=0.03", ylab="Density",
     cex.axis=1.5, cex.lab=1.5,
     xlim=c(0.7, 3.8), ylim=c(0, 5))
for (i in 1:10) {
    lines(density(result[, i], bw=0.03), col=color[i], lwd=1.5)
}
dev.off()

load("/Users/yichenzhou/Box Sync/Boulevard/Paper/Boulevard/Simulation/result_c_180415231822.RData")
setEPS()
postscript("ld_2.eps", width=6, height=4.5)
plot(NA, xlab="N=1000, Bandwidth=0.03", ylab="Density",
     cex.axis=1.5, cex.lab=1.5,
     xlim=c(0.7, 3.8), ylim=c(0, 5))
for (i in 1:10) {
    lines(density(result[, i], bw=0.03), col=color[i], lwd=1.5)
}
dev.off()

load("/Users/yichenzhou/Box Sync/Boulevard/Paper/Boulevard/Simulation/result_c_180416003419.RData")
setEPS()
postscript("ld_3.eps", width=6, height=4.5)
plot(NA, xlab="N=1000, Bandwidth=0.03", ylab="Density",
     cex.axis=1.5, cex.lab=1.5,
     xlim=c(0.7, 3.8), ylim=c(0, 5))
for (i in 1:10) {
    lines(density(result[, i], bw=0.03), col=color[i], lwd=1.5)
}
dev.off()

load("/Users/yichenzhou/Box Sync/Boulevard/Paper/Boulevard/Simulation/result_c_180416012821.RData")
setEPS()
postscript("ld_4.eps", width=6, height=4.5)
plot(NA, xlab="N=1000, Bandwidth=0.03", ylab="Density",
     cex.axis=1.5, cex.lab=1.5,
     xlim=c(0.7, 3.8), ylim=c(0, 5))
for (i in 1:10) {
    lines(density(result[, i], bw=0.03), col=color[i], lwd=1.5)
}
dev.off()

