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
