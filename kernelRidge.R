source("honestRpart.R")

pred <- function(X) {
    return(X[, 1] + 3*X[, 2] + X[, 3]^2 + 2*X[, 4]*X[, 5])
}

error <- function(n, d = 1) {
    return(runif(n, -d, d))
}

####
set.seed(42)

d <- 5
n <- 200
ntree <- 100
xtest <- t(matrix(c(0.1, 0.1, 0.1, 0.1, 0.1,
                    0.6, 0.9, 0.8, 0.9, 0.7,
                    0.1, 0.1, 0.9, 0.9, 0.9,
                    0.9, 0.1, 0.1, 0.1, 0.9), nrow=d))
ytest <- pred(xtest)
lambda <- 0.8

#### Generate convergence path
xtrain <- matrix(runif(d*n), nrow=n)
ytrain <- pred(xtrain) + error(n, d = 1)
blv <- boulevard(xtrain, ytrain, ntree=ntree, subsample=0.8, lambda = lambda,
                 leaf.size = 5, method="random") 
structMat <- predict.boulevard.structure.vec(blv, newdata = xtrain)
structVec <- predict.boulevard.structure.vec(blv, newdata = xtest)
blv.pred <- predict.boulevard(blv, xtest, fullpath = TRUE)
kr.pred <- (1+lambda)/lambda * structVec%*%solve(diag(n)/lambda + structMat)%*%ytrain

setEPS()
drawx <- c(seq(1, 10, by=1), seq(12, 60, by = 4), seq(68, 100, by=8))
postscript("blv_conv.eps", width=5, height=5)
matplot(x=drawx, y = t(blv.pred[, drawx]), type="p", pch=1:4, col=1:4,
        xlab="Ensemble Size",
        ylab="Prediction",
        main="Boulevard Convergence", 
        cex = 1)
abline(h = kr.pred, col=1:5, lty=1:4, lwd = 2)
legend("topright", legend = c("Pt 1", "Pt 2", "Pt 3", "Pt 4"), 
       lty = 1:4,
       pch = 1:4,
       col = 1:4, cex=1)
dev.off()

blv.pred.vec <- c()
kr.pred.vec <- c()
col.vec <- c()

for (i in 1:20) {
    xtrain <- matrix(runif(d*n), nrow=n)
    ytrain <- pred(xtrain) + error(n, d = 1)
    blv <- boulevard(xtrain, ytrain, ntree=ntree, subsample=0.8, lambda = lambda,
                     leaf.size = 5, method="random") 
    structMat <- predict.boulevard.structure.vec(blv, newdata = xtrain)
    structVec <- predict.boulevard.structure.vec(blv, newdata = xtest)
    blv.pred <- predict.boulevard(blv, xtest)
    kr.pred <- (1+lambda)/lambda * structVec%*%solve(diag(n)/lambda + structMat)%*%ytrain
    blv.pred.vec <- c(blv.pred.vec, blv.pred)
    kr.pred.vec <- c(kr.pred.vec, kr.pred)
    col.vec <- c(col.vec, 1:4)
}

for (i in 1:4) {
    print(c(
        mean((blv.pred.vec[col.vec==i] - ytest[i])^2),
        mean((kr.pred.vec[col.vec==i] - ytest[i])^2)
    ))
}

setEPS()
postscript("blv_kr.eps", width=5, height=5)
par(mfrow=c(2,2))
bd=list(c(0,3), c(3,6), c(1,4), c(0,3))
for (i in 1:4) {
    plot(blv.pred.vec[col.vec==i], kr.pred.vec[col.vec==i], 
         col=i, 
         pch=i,
         ylim=bd[[i]], xlim=bd[[i]],
         ylab="Kernel Ridge",
         xlab="Boulevard",
         main=paste("Pt", i))
    if (i==1) {
        text(2.3, 1.2, "MSE\nBLV: 0.910\nKRR: 0.917", cex=0.8)
    } else if (i==2) {
        text(3.7, 3.7, "MSE\nBLV: 0.912\nKRR: 0.917", cex=0.8)
    } else if (i==3) {
        text(1.7, 2.0, "MSE\nBLV: 0.042\nKRR: 0.040", cex=0.8)
    } else if (i==4) {
        text(1.0, 2.0, "MSE\nBLV: 0.556\nKRR: 0.556", cex=0.8)
    }
    abline(h=ytest[i], col=i, lty=i)
    abline(v=ytest[i], col=i, lty=i)
}
dev.off()
