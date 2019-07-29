# Readme: Manual

This repo holds the supplementary code for the arXiv submission *Boulevard: Regularized Stochastic Gradient Boosted Trees and Their Limiting Distribution*

https://arxiv.org/abs/1806.09762

# List of files in this repo:

+ honestRpart.R : Boulevard main lib. The implementation relies on 
```r
library(rpart)
library(treeClust)
library(plotrix)
```

+ limiting.R : Code showing the limiting distribution of Boulevard predictions

+ lambda.R : Code showing how different lambda values impact the performance of Boulevard

+ kernelRidge.R : Code showing the comparison between Boulevard convergence point and its empirically estimated KRR form 

+ performance.R : Code showing the performance, mainly accuracy, of Boulevard on several real world data sets

+ performance4.R : Another version of `performance.R`

# How to train Boulevard boosting models

We take performance4.R as an example, which also shows how training a Boulevard model compares to training other tree ensembles.

```r
trainBLV <- function(X, Y, ntree=1000, xtest=NULL, ytest=NULL, leaf.size=10, subsample=0.8, method="random") {
    model <- boulevard(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method=method)
    return(list(mse=model$mse, testmse=model$testmse))
}
```

Run `boulevard` to get a boulevard model. It's defined as
```r
boulevard <- function(X, Y, ntree=1000, lambda = 0.8, subsample=0.8, xtest=NULL, ytest=NULL, leaf.size=10, method="random")
```
The arguments here are
+ `X`: training covariates
+ `Y`: training labels
+ `ntree`: number of trees in the ensemble
+ `lambda`: the `lambda` value mentioned in the algorithm. As a shrinkage parameter, it is preferable to have a `lambda` close to 1.
+ `subsample`: subsampling rate, if needed.
+ `xtest`: testing covariates, if needed.
+ `ytest`: testing labels, if needed.
+ `leaf.size`: the minimal leaf size used in the trees to allow a split.
+ `method`: how a Boulevard tree is built. Default is `random`, a randomized decision tree built by assigning random `y` values to the training covariates then running CART. It can also be `standard` which is adaptive.

Calling `boulevard` returns a trained model 
```r
model <- boulevard(X, Y, ntree=ntree, xtest=xtest, ytest=ytest, leaf.size=leaf.size, subsample=subsample, method=method)
```
The output model, which can be later used to make predictions on new covariates is a list consisting of the following entries.
+ `model$trees:` the decision trees built by Boulevard
+ `model$mse`: MSE curve against the number of trees on the training data
+ `model$testmse`: if provided, MSE curve against the number of trees on the testing data
+ `model$lambda`: `lambda` value used in Boulevard boosting
+ `model$sigma2`: MSE of the entire ensemble, used as the estimator of the error variance

# How to make Boulevard predictions
We take `kernelRigde.R` as an example. The code 
```r
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
```
repeats the study 20 times where for each time a Boulevard model is trained by
```r
blv <- boulevard(xtrain, ytrain, ntree=ntree, subsample=0.8, lambda = lambda, leaf.size = 5, method="random") 
```
There are three types of predictions made here. 
+ Predicting labels using `predict.boulevard`:
```r
blv.pred <- predict.boulevard(blv, xtest)
```
`predict.boulevard` is defined as
```r
predict.boulevard <- function(blv, newdata, fullpath = FALSE)
```
It takes three arguments: the trained Boulevard boosting model, the covariates of the new data, and `fullpath` which decides whether the return value is a vector showing the ensemble-wise results, or a matrix also including the interim results along  the boosting.
+ Predicting structural vector (matrix):
```r
predict.boulevard.structure.vec <- function(blv, newdata)
```
+ Predicting the half length of the reproduction interval
```r
predict.boulevard.variance <- function(blv, newdata, narrow = FALSE)
```
The square root of the return value can be used as the half length. The third argument `narrow` decides which eigenvalue we use for the inverse matrix in the KKR form. It can be set to `TRUE` to give more aggresive results.
