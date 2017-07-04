\name{simule}
\alias{simule}
\title{A constrained l1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models}
\usage{
simule(X, lambda, epsilon = 1, covType = "cov", parallel = FALSE)
}
\arguments{
\item{X}{A List of input matrices. They can be data matrices or covariance/correlation matrices. If every matrix in the X is a symmetric matrix, the matrices are assumed to be covariance/correlation matrices.}
\item{lambda}{A positive number. The hyperparameter controls the sparsity level of the matrices. The \eqn{\lambda} in the details.}
\item{epsilon}{A positive number. The hyperparameter controls the differences between the shared pattern among graphs and the individual part of each graph. The \eqn{\epsilon} in the details. If epsilon becomes larger, the generated graphs will be more similar to each other. The default value is 1, which means that we set the same weights to the shared pattern among graphs and the individual part of each graph.}
\item{covType}{A parameter to decide which Graphical model we choose. If covType = "cov", it means that we estimate multiple Gaussian Graphical models. In another word, we assume that the \eqn{\Sigma^{(i)}} in the details are estimated by the sample covariance matrices. If covType = "cor", it means that we estimate multiple nonparanormal Graphical models. In another word, we assume that the \eqn{\Sigma^{(i)}} in the details are estimated by the kendall's tau correlation matrices.}
\item{parallel}{A boolean. It is the parameter to decide that the package uses the multithreading architecture or not.}
}
\description{
Estimate multiple, related graphical models across multiple related datasets using the SIMULE algorithm.
}
\value{
\item{Graphs}{A list of the estimated inverse covariance matrices.}
}
\details{
  The SIMULE algorithm is a constrained l1 minimization method to detect shared and individual parts of multiple graphs explicitly. It solves the following equation:
\deqn{
\hat{\Omega}^{(1)}_I, \hat{\Omega}^{(2)}_I, \dots, \hat{\Omega}^{(K)}_I, \hat{\Omega}_S = \min\limits_{\Omega^{(i)}_I,\Omega_S}\sum\limits_i ||\Omega^{(i)}_I||_1+ \epsilon K||\Omega_S||_1
}
Subject to :
\deqn{
  ||\Sigma^{(i)}(\Omega^{(i)}_I + \Omega_S) - I||_{\infty} \le \lambda_{n},  i =
    1,\dots,K \nonumber
}
Please also see the equation (7) in our paper. The \eqn{\lambda_n} is the hyperparameter controlling the sparsity level of the matrices and it is the \code{lambda} in our function. The \eqn{\epsilon} is the hyperparameter controlling the differences between the shared pattern among graphs and the individual part of each graph. It is the \code{epsilon} parameter in our function and the default value is 1. For further details, please see our paper: <http://link.springer.com/article/10.1007/s10994-017-5635-7>.
}
\author{
Beilun Wang
}
\references{
Beilun Wang, Ritambhara Singh, Yanjun Qi (2017).  A constrained L1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models. http://link.springer.com/article/10.1007/s10994-017-5635-7
}
\examples{
\dontrun{
data(exampleData)
simule(X = exampleData , 0.05, 1, covType = "cov", TRUE)
plot.simule(results)
plot.simule(results, type="share")
plot.simule(results, type="sub", subID=1)
plot.simule(results, type="neighbor", index=50)
}
}
