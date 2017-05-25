# Paper16-SIMULE

"A constrained L1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models", 
accepted by Journal of Machine Learning 

# SIMULE
This is an R implementation of the [SIMULE](https://arxiv.org/abs/1605.03468).

See Manual: https://cran.r-project.org/web/packages/simule/simule.pdf
## Dependency
It depends on the following existing packages. To use them, simply
```r
library('pcaPP')
library('lpSolve')
library('parallel')
```
If you don't have these packages, simply use
```r
install.packages('packageName')
```
## Usage
To use the **SIMULE** code, simply use the function named ```SIMULE``` in *SIMULE.R*.

For example,
```r
SIMULE(Cors, lambda = 0.05, epsilon = 0.5, parallel = TRUE)
```
The function will returns a ```list``` (a data structure in R) of graphs estimated from SIMULE.

## Arguments
- ``` Cors ```

It is a ```list``` of correlation/covariance matrices. For fast estimation of kendall's tau correlation matrices, use the
```r
cor.fk(X[[i]])  
```
in the ``` 'pcaPP' ``` package. Here ``` X[[i]]``` is one instance in a ```list``` of data matrices ```X```.

**Make sure the order of the feature variables are the same among the data matrices.**
You can also use the default function in R.
For covariance matrices,
```r
cov(X[[i]])
```
For kendall's tau correlation matrices,
```r
cor(X[[i]], method = 'kendall')
```
Note that the default function of kendall's tau correlation matrix is very slow in R.

- ``` lambda ```

The parameter for the sparisty level of the estimated graphs. The larger ```lambda``` you choose, the ***sparser*** graphs you will obtain.

- ``` epsilon ```

The parameter reflects the differences of sparsity in the shared subgraph versus the context-specific subgraphs. The larger ```epsilon``` you choose, the shared subgraph is ***denser*** while the context-specific subgraphs is ***sparser*** and vice versa.

- ``` parallel ```

Logic parameter for parallel implementation or not. If you have a multi-core machine, let ```parallel = TRUE```.

