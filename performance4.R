source("honestRpart.R")
library("randomForest")
library("gbm")
library("MASS")

#### Start with some simulated data ####


trainRF <- function (X, Y, ntree=1000, xtest=NULL, ytest=NULL, subsample = 0.8, leaf.size=10) {
    colnames(X) <- paste("x", 1:ncol(X), sep="")
    model <- randomForest(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, nodesize=leaf.size, sampsize = nrow(X)*subsample, keep.forest=TRUE)
    mse <- model$test$mse
    return(list(mse=model$mse, testmse=mse))
}

trainGBT <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, leaf.size=10) {
    colnames(X) <- paste("x", 1:ncol(X), sep="")
    data <- data.frame(Y = Y, X = X)
    model <- gbm(Y ~ ., data = data, distribution="gaussian", 
                 n.trees = ntree, shrinkage = 0.05,
                 bag.fraction = 1, train.fraction = 1, n.minobsinnode = leaf.size)
    colnames(xtest) <- paste("x", 1:ncol(X), sep="")
    testData <- data.frame(X = xtest)
    tmpPred <- predict.gbm(model, newdata=testData, n.trees=c(1:ntree))
    modelPred <- predict.gbm(model, newdata=data, n.trees=c(1:ntree))
    mse <- c()
    modelmse <- c()
    for (b in 1:ntree) {
        mse <- c(mse, mean((tmpPred[, b]-ytest)^2))
        modelmse <- c(modelmse, mean((modelPred[, b]-Y)^2))
    }
    return(list(mse=modelmse, testmse=mse))
}

trainSGBT <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, subsample=0.8, leaf.size=10) {
    colnames(X) <- paste("x", 1:ncol(X), sep="")
    data <- data.frame(Y = Y, X = X)
    model <- gbm(Y ~ ., data = data, distribution="gaussian", 
                 n.trees = ntree, shrinkage = 0.05,
                 bag.fraction = subsample, train.fraction = 1, n.minobsinnode = leaf.size)
    colnames(xtest) <- paste("x", 1:ncol(X), sep="")
    testData <- data.frame(X = xtest)
    tmpPred <- predict.gbm(model, newdata=testData, n.trees=c(1:ntree))
    modelPred <- predict.gbm(model, newdata=data, n.trees=c(1:ntree))
    mse <- c()
    modelmse <- c()
    for (b in 1:ntree) {
        mse <- c(mse, mean((tmpPred[, b]-ytest)^2))
        modelmse <- c(modelmse, mean((modelPred[, b]-Y)^2))
    }
    return(list(mse=modelmse, testmse=mse))
}

trainBLV <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, leaf.size=10, subsample=0.8, method="random") {
    model <- boulevard(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method=method)
    return(list(mse=model$mse, testmse=model$testmse))
}


#### Plot: CASP ####
raw.data <- read.csv("CASP/CASP.csv", header=TRUE)
y <- as.vector(raw.data[["RMSD"]])[1:20000]
x <- raw.data[1:20000, 2:10]
n <- nrow(x)
d <- ncol(x)
ntree <- 1000
subsample <- 0.5
leaf.size <- 50


kfold <- 5
fold <- sample(cut(1:nrow(x), breaks=kfold, label=FALSE))
mat <- matrix(0, nrow=ntree, ncol=10)

for (foldno in 1:kfold) {
    ss <- which(fold==foldno)
    xtrain <- x[-ss, ]
    xtest <- x[ss, ]
    ytrain <- y[-ss]
    ytest <- y[ss]
    
    modelGBT <- trainGBT(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size/subsample)
    modelSGBT <- trainSGBT(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
    modelRF <- trainRF(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample)
    modelBLV <- trainBLV(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
    modelRBLV <- trainBLV(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")
    mat <- mat + cbind(modelGBT$mse, modelGBT$testmse, 
                       modelSGBT$mse, modelSGBT$testmse,
                       modelRF$mse, modelRF$testmse,
                       modelBLV$mse, modelBLV$testmse,
                       modelRBLV$mse, modelRBLV$testmse)
}

mat <- mat / kfold
save(mat, file="casp2.RData")

setEPS()
postscript("casp2_400.eps", width=6, height=4.5)
load("casp2.RData")
matplot(x=2:ntree, y=mat[2:ntree, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        main="CASP",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(2, ntree, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10)], type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
# legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
#                             "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
#        col=c(1:5, 1:5), 
#        lty=c(rep(2,5), rep(1,5)),
#        lwd=1, cex=0.8, ncol=2)
dev.off()

#### Plot: Boston ####
raw.data <- Boston
y <- as.vector(raw.data[[14]])
x <- raw.data[, 1:13]

kfold <- 5

d <- ncol(x)
ntree <- 1000
subsample <- 0.8
leaf.size <- 5
fold <- sample(cut(1:nrow(x), breaks=kfold, label=FALSE))
mat <- matrix(0, nrow=ntree, ncol=10)

for (foldno in 1:kfold) {
    ss <- which(fold==foldno)
    xtrain <- x[-ss, ]
    xtest <- x[ss, ]
    ytrain <- y[-ss]
    ytest <- y[ss]

    modelGBT <- trainGBT(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size/subsample)
    modelSGBT <- trainSGBT(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
    modelRF <- trainRF(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample)
    modelBLV <- trainBLV(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
    modelRBLV <- trainBLV(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")
    mat <- mat + cbind(modelGBT$mse, modelGBT$testmse, 
                       modelSGBT$mse, modelSGBT$testmse,
                       modelRF$mse, modelRF$testmse,
                       modelBLV$mse, modelBLV$testmse,
                       modelRBLV$mse, modelRBLV$testmse)
    print(mat)
}

mat <- mat / kfold
save(mat, file="Boston2.RData")

setEPS()
postscript("boston2_400.eps", width=6, height=4.5)
load("Boston2.RData")
matplot(x=2:ntree, y=mat[2:ntree, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        main="Boston Housing",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(2, ntree, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10)], type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
# legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
#                             "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
#        col=c(1:5, 1:5), 
#        lty=c(rep(2,5), rep(1,5)),
#        lwd=1, cex=0.8, ncol=2)
dev.off()

#### Plot: airfoil ####
raw.data <- read.table("airfoil_self_noise.dat", header=FALSE)
y <- as.vector(raw.data[, 6])
x <- as.matrix(raw.data[, 1:5])
d <- ncol(x)
ntree <- 1000
subsample <- 0.8
leaf.size <- 20

kfold <- 5
fold <- sample(cut(1:nrow(x), breaks=kfold, label=FALSE))
mat <- matrix(0, nrow=ntree, ncol=10)

for (foldno in 1:kfold) {
    ss <- which(fold==foldno)
    xtrain <- x[-ss, ]
    xtest <- x[ss, ]
    ytrain <- y[-ss]
    ytest <- y[ss]
    
    modelGBT <- trainGBT(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size/subsample)
    modelSGBT <- trainSGBT(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
    modelRF <- trainRF(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample)
    modelBLV <- trainBLV(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
    modelRBLV <- trainBLV(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")
    mat <- mat + cbind(modelGBT$mse, modelGBT$testmse, 
                       modelSGBT$mse, modelSGBT$testmse,
                       modelRF$mse, modelRF$testmse,
                       modelBLV$mse, modelBLV$testmse,
                       modelRBLV$mse, modelRBLV$testmse)
}

mat <- mat / kfold
save(mat, file="airfoil2.RData")

setEPS()
postscript("airfoil2_400.eps", width=6, height=4.5)
load("airfoil2.RData")
matplot(x=2:ntree, y=mat[2:ntree, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        main="Airfoil",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(2, ntree, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10)], type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
# legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
#                             "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
#        col=c(1:5, 1:5), 
#        lty=c(rep(2,5), rep(1,5)),
#        lwd=1, cex=0.8, ncol=2)
dev.off()
#### Plot: CCPP ####
raw.data <- read.csv("CCPP/ccpp.csv", header=TRUE)
y <- as.vector(raw.data$PE)
x <- as.matrix(raw.data[, 1:4])

n <- nrow(x)
d <- ncol(x)
ntree <- 1000
subsample <- 0.5
leaf.size <- 50

kfold <- 5
fold <- sample(cut(1:nrow(x), breaks=kfold, label=FALSE))
mat <- matrix(0, nrow=ntree, ncol=10)

for (foldno in 1:kfold) {
    ss <- which(fold==foldno)
    xtrain <- x[-ss, ]
    xtest <- x[ss, ]
    ytrain <- y[-ss]
    ytest <- y[ss]
    
    modelGBT <- trainGBT(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size/subsample)
    modelSGBT <- trainSGBT(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, subsample=subsample, leaf.size=leaf.size)
    modelRF <- trainRF(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample)
    modelBLV <- trainBLV(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="standard")
    modelRBLV <- trainBLV(xtrain, ytrain, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method="random")
    mat <- mat + cbind(modelGBT$mse, modelGBT$testmse, 
                       modelSGBT$mse, modelSGBT$testmse,
                       modelRF$mse, modelRF$testmse,
                       modelBLV$mse, modelBLV$testmse,
                       modelRBLV$mse, modelRBLV$testmse)
}

mat <- mat / kfold
save(mat, file="ccpp2.RData")

setEPS()
postscript("ccpp2_4001.eps", width=6, height=4.5)
load(file="ccpp2.RData")
matplot(x=5:ntree, y=(mat[5:ntree, ]), type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        main="CCPP",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(5, ntree, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10)], type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
# legend("topright", legend=c("GBT Train", "SGBT Train", "RF Train", "BLV Train", "rBLV Train",
#                             "GBT Test", "SGBT Test", "RF Test", "BLV Test", "rBLV Test"),
#        col=c(1:5, 1:5), 
#        lty=c(rep(2,5), rep(1,5)),
#        lwd=1, cex=0.8, ncol=2)
dev.off()





#### NEW PLOTTING ####
ntree <- 300

setEPS()
postscript("casp_part1.eps", width=2.5, height=4)
load("casp2.RData")
matplot(x=2:50, y= (mat[2:50, ]), type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        main="CASP",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(2, 50, length.out=7))
matplot(subx, y=(mat[subx, c(2,4,6,8,10)]), type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
dev.off()
setEPS()
postscript("casp_part2.eps", width=4, height=4)
load("casp2.RData")
matplot(x=51:300, y= (mat[51:300, ]), type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(51, 300, length.out=10))
matplot(subx, y=(mat[subx, c(2,4,6,8,10)]), type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
dev.off()


setEPS()
postscript("boston_part1.eps", width=2.5, height=4)
load("Boston2.RData")
matplot(x=2:50, y=(mat[2:50, ]), type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        main="Boston Housing",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(2, 50, length.out=7))
matplot(subx, y=(mat[subx, c(2,4,6,8,10)]), type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
dev.off()
setEPS()
postscript("boston_part2.eps", width=4, height=4)
load("Boston2.RData")
matplot(x=51:300, y=(mat[51:300, ]), type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(51, 300, length.out=10))
matplot(subx, y=(mat[subx, c(2,4,6,8,10)]), type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
dev.off()


setEPS()
postscript("airfoil_part1.eps", width=2.5, height=4)
load("airfoil2.RData")
matplot(x=2:50, y=(mat[2:50, ]), type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        main="Airfoil",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(2, 50, length.out=7))
matplot(subx, y=(mat[subx, c(2,4,6,8,10)]), type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
dev.off()
setEPS()
postscript("airfoil_part2.eps", width=4, height=4)
load("airfoil2.RData")
matplot(x=51:300, y=(mat[51:300, ]), type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(51, 300, length.out=10))
matplot(subx, y=(mat[subx, c(2,4,6,8,10)]), type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
dev.off()


setEPS()
postscript("ccpp_part1.eps", width=2.5, height=4)
load(file="ccpp2.RData")
matplot(x=2:50, y=mat[2:50, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        main="CCPP",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(2, 50, length.out=7))
matplot(subx, y=mat[subx, c(2,4,6,8,10)], type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
dev.off()
setEPS()
postscript("ccpp_part2.eps", width=4, height=4)
load(file="ccpp2.RData")
matplot(x=51:300, y=mat[51:300, ], type="l", col=c(1,1,2,2,3,3,4,4,5,5), lty=c(1,1,2,2,3,3,4,4,5,5), 
        lwd=1.5, 
        ylab="MSE", "xlab"="Ensemble Size",
        cex.lab=1.5, cex.axis=1.5)
subx <- round(seq(51, 300, length.out=10))
matplot(subx, y=mat[subx, c(2,4,6,8,10)], type="p", col=c(1,2,3,4,5), pch=16, add=TRUE)
dev.off()

