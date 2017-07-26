
# SIMULE
This is an R implementation of the [SIMULE](https://arxiv.org/abs/1605.03468) algorithm proposed in the following paper:

"A constrained L1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models",
accepted by Machine Learning @ [URL](https://link.springer.com/article/10.1007/s10994-017-5635-7)

Please run demo(simule) to learn the basic functions provided by this package. For further details, please read the original paper @ [URL](http://link.springer.com/article/10.1007/s10994-017-5635-7) or read the R-package Manual: @ [URL](https://cran.r-project.org/web/packages/simule/simule.pdf)

## Dependency
It depends on the following existing packages. To use them, simply
```r
library('pcaPP')
library('lpSolve')
library('parallel')
```
If you don't have these packages, simply use
```r
install.packages('packageNameFromAbove')
```

## Usage

0. install the R "simule" package through R console:
```r
install.packages('simule')
```

1. then load the library simule in R console, by running:
```r
library(simule)
```

2. Then, simply run the function  ```simule``` on your favorite datasets
For example,
```r
simule(CovarianceMatrixList, lambda = 0.05, epsilon = 0.5, parallel = TRUE)
```

This function will returns a ```list``` (a data structure in R) of graphs estimated by the SIMULE package.

## Three possible types of inputs for the Argument ``` CovarianceMatrixList ```

1. The argument ``` CovarianceMatrixList ``` can represent a ```list``` of data matrices directly:
The i-th item ``` CovarianceMatrixList[[i]]``` represents the i-th matrix  in a ```list``` of data matrices ```CovarianceMatrixList```.
** Please make sure the order of the feature variables are the same among all the data matrices in ```CovarianceMatrixList```.**


2. If the input ``` CovarianceMatrixList ``` is Symmetric, the package automatically assumes that the data inputs belong to the following two types:

- The argument ``` CovarianceMatrixList ``` can a ```list``` of covariance matrices.
Assuming ``` X ``` represents a list of data matrices, whose i-th item ``` X[[i]]``` represents the data matrix of the i-th task.

We can use the following function to calculate the covariance matrices:
```r
CovarianceMatrixList[[i]] = cov(X[[i]])
```

- The argument ``` CovarianceMatrixList ``` can represent a ```list``` of kendall's tau correlation matrices.
The kendall's tau correlation matrices can be calculated by using the following command:
```r
cor.fk(X[[i]])  
```
(by the ``` 'pcaPP' ``` package.)

The kendall's tau correlation matrices can also be calculated through the following R functions:
```r
cor(X[[i]], method = 'kendall')
```
However the above way of calculating kendall's tau correlation matrix is very slow in R.


## Other Arguments

- ``` lambda ```

The parameter for the sparsity level of the estimated graphs. The larger ```lambda``` you choose, the ***sparser*** graphs you will estimate from the inputs.

- ``` epsilon ```

The parameter reflects the differences of sparsity level between the shared subgraph versus the context-specific subgraphs. The larger ```epsilon``` you choose, the ***denser*** the shared subgraph is (while the context-specific subgraphs are ***sparser***) and vice versa.

- ``` covType  ```
This parameter controls SIMULE estimates the sparse Gaussian Graphical models (sGGM) or the sparse Nonparanormal Graphical Models from the input data.  This parameter  matters only when the input argument ``` CovarianceMatrixList ```  represents a ```list``` of data matrices directly:
When ``` covType = "cov" ``` the package estimates sGGMs from the input.
When ``` covType == "kendall"```, the package estimates sNGMs from the input.

- ``` parallel ```

Logic parameter for parallel implementation or not. If you have a multi-core machine, let ```parallel = TRUE```.
