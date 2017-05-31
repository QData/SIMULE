\name{simule}
\alias{simule}
\title{A constrained l1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models}
\usage{
simule(X, lambda, epsilon, covType = "cov", parallel = FALSE)
}
\arguments{
\item{X}{A List of matrices. They can be both data matrices or covariance/correlation matrices. If every matrix in the X is a sysmetric matrix, the matrices are assumed to be covariance/correlation matrices.}
\item{lambda}{A postive scalar. The hyperparameter controlling the sparsity level of the matrices.}
\item{epsilon}{A postive scalar. The hyperparameter controlling the differences between the shared pattern among graphs and the individual part of each graph. If epsilon becomes larger, generated graphs will be more similar to each other.}
\item{covType}{A parameter to decide which type of matrices when the input is a list of data matrices. "cov" means the sample covariance matrices and "cor" means the kendall's tau correlation matrices.  }
\item{parallel}{A boolean. It is the hyperparameter that decides to use multithreading architecture or not.}
}
\description{
The R implementation of the SIMULE method, which is introduced in the paper "A constrained l1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models"
}
\value{
\item{Graphs}{A list of the estimated inverse covariance matrices.}
}
\author{
Beilun Wang
}
\references{
Beilun Wang, Ritambhara Singh, Yanjun Qi (2017).  A constrained L1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models. https://arxiv.org/abs/1605.03468
}
\examples{
\dontrun{
data(exampleData)
simule(X = exampleData , 0.05, 1, covType = "cov", TRUE)
}
}
