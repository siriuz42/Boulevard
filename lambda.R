## Check the impact of lambda on Boulevard

source("honestRpart.R")
library("randomForest")
library("gbm")
library("MASS")

#### Start with some simulated data ####

trainBLV <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, leaf.size=10, subsample=0.8, method="random", lambda = 0.8) {
    model <- boulevard(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method=method, lambda = lambda)
    return(list(mse=model$mse, testmse=model$testmse))
}


#### Plot: Simulation 3 ####
pred <- function(X) {
    return(X[, 1] + 3*X[, 2] + (1-X[, 3])^2 + X[, 4]*X[, 5] + (1-X[, 6])^6 + X[, 7])
}

d <- 7
n <- 5000
subsample <- 0.3
leaf.size <- 20
ntree <- 300
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + runif(n, -1, 1)
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelBLV_2 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard", lambda = 0.2)
modelRBLV_2 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random", lambda = 0.2)
modelBLV_5 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard", lambda = 0.5)
modelRBLV_5 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random", lambda = 0.5)
modelBLV_8 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard", lambda = 0.8)
modelRBLV_8 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random", lambda = 0.8)

mat <- c()
mat <- cbind(modelBLV_2$mse, modelBLV_2$testmse,
             modelBLV_5$mse, modelBLV_5$testmse,
             modelBLV_8$mse, modelBLV_8$testmse,
             modelRBLV_5$mse, modelRBLV_5$testmse,
             modelRBLV_2$mse, modelRBLV_2$testmse,
             modelRBLV_8$mse, modelRBLV_8$testmse)
setEPS()
postscript("lambda_1_part1.eps", width=2.5, height=4)
matplot(x=1:50, y=mat[1:50, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5,6,6), lty=c(1,1,2,2,3,3,4,4,5,6,6), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(1, 50, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10, 12)], type="p", col=c(1,2,3,4,5,6), pch=16, add=TRUE)
# legend("topright", legend=c(expression(paste(lambda, "=0.2 BLV")),
#                             expression(paste(lambda, "=0.5 BLV")),
#                             expression(paste(lambda, "=0.8 BLV")),
#                             expression(paste(lambda, "=0.2 rBLV")),
#                             expression(paste(lambda, "=0.5 rBLV")),
#                             expression(paste(lambda, "=0.8 rBLV"))
# ),
# col=c(1:6),
# lty=c(1:6),
# lwd=1, cex=0.8, ncol=6)
dev.off()
setEPS()
postscript("lambda_1_part2.eps", width=4, height=4)
matplot(x=51:300, y=mat[51:300, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5,6,6), lty=c(1,1,2,2,3,3,4,4,5,6,6), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(51, 300, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10, 12)], type="p", col=c(1,2,3,4,5,6), pch=16, add=TRUE)
dev.off()

#### Plot: Simulation 4 ####
pred <- function(X) {
    return(X[, 1] + 3*X[, 2] + (1-X[, 3])^2 + X[, 4]*X[, 5] + (1-X[, 6])^6 + X[, 7])
}

d <- 7
n <- 5000
subsample <- 0.3
leaf.size <- 20
ntree <- 300
X <- matrix(runif(d*n), nrow=n)
Y <- pred(X) + sign(runif(n, -1, 1))*0.5
xtest <- matrix(runif(d*100), ncol=d)
ytest <- pred(xtest)
modelBLV_2 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard", lambda = 0.2)
modelRBLV_2 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random", lambda = 0.2)
modelBLV_5 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard", lambda = 0.5)
modelRBLV_5 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random", lambda = 0.5)
modelBLV_8 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard", lambda = 0.8)
modelRBLV_8 <- trainBLV(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random", lambda = 0.8)


mat <- c()
mat <- cbind(modelBLV_2$mse, modelBLV_2$testmse,
             modelBLV_5$mse, modelBLV_5$testmse,
             modelBLV_8$mse, modelBLV_8$testmse,
             modelRBLV_5$mse, modelRBLV_5$testmse,
             modelRBLV_2$mse, modelRBLV_2$testmse,
             modelRBLV_8$mse, modelRBLV_8$testmse)
setEPS()
postscript("lambda_2_part1.eps", width=2.5, height=4)
matplot(x=1:50, y=mat[1:50, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5,6,6), lty=c(1,1,2,2,3,3,4,4,5,6,6), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(1, 50, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10, 12)], type="p", col=c(1,2,3,4,5,6), pch=16, add=TRUE)
# legend("topright", legend=c(expression(paste(lambda, "=0.2 BLV")),
#                             expression(paste(lambda, "=0.5 BLV")),
#                             expression(paste(lambda, "=0.8 BLV")),
#                             expression(paste(lambda, "=0.2 rBLV")),
#                             expression(paste(lambda, "=0.5 rBLV")),
#                             expression(paste(lambda, "=0.8 rBLV"))
# ),
# col=c(1:6),
# lty=c(1:6),
# lwd=1, cex=0.8, ncol=6)
dev.off()
setEPS()
postscript("lambda_2_part2.eps", width=4, height=4)
matplot(x=51:300, y=mat[51:300, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5,6,6), lty=c(1,1,2,2,3,3,4,4,5,6,6), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(51, 300, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10, 12)], type="p", col=c(1,2,3,4,5,6), pch=16, add=TRUE)
dev.off()
