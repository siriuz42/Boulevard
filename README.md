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
+ `subsample`: sampling rate, if needed.
+ `xtest`: testing covariates, if needed.
+ `ytest`: testing labels, if needed.
+ `leaf.size`: the minimal leaf size used in the trees to allow a split.
+ `method`: how a Boulevard tree is built. Default is `random`, a randomized decision tree built by assigning random `y` values to the training covariates then running CART. It can also be `standard` which is adaptive.



