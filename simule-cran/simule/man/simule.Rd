\name{simule}
\alias{simule}
\title{A constrained l1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models}
\usage{
simule(X, lambda, epsilon, covType = "cov", parallel = FALSE)
}
\arguments{
\item{X}{A List of matrices. They can be both data matrices or covariance/correlation matrices. If each matrix in the X is sysmetric matrix, we assume they are covariance/correlation matrices you give.}
\item{lambda}{A postive scalar. It is a hyperparameter controling the sparsity level of the matrices.}
\item{epsilon}{A postive scalar. It is a hyperparameter controling the differences between the shared pattern among graphs and the individual part of each graph. If you choose larger epsilon, all the graphs will be more similar to the others}
\item{covType}{A parameter to decide which type of matrices you use when the input is a list of data matrices. There are two options. "cov" means the sample covariance matrices and "cor" means the kendall's tau correlation matrices.  }
\item{parallel}{A boolean. It is a hyperparameter that decides the function is run by using multithreading architecture or not.}
}
\description{
An R implementation of simule method, which is introduced in A constrained l1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models
}
\examples{
\dontrun{
data(exampleData)
simule(X = exampleData , 0.05, 1, covType = "cov", TRUE)
}
}
