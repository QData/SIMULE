\name{net.neighbors}
\alias{net.neighbors}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get network neighbors of a node
}
\description{
For each class, returns the names of the nodes connected to a given node.
}
\usage{
net.neighbors(theta, index)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each an estimated sparse inverse covariance matrix.  (For example, the result of FGL or GGL.)
}
  \item{index}{
The row number of the node to be investigated.
}
}

\value{
neighbors, a list of length K, each element of which is a vector of the row names of the nodes neighboring the node of interest.
}
\references{
Beilun Wang, Ritambhara Singh and Yanjun Qi (2017).  A Constrained L1 Minimization Approach for Estimating Multiple Sparse Gaussian or Nonparanormal Graphical Models. <arXiv:1605.03468>
}
\author{
Beilun Wang}

\examples{
\dontrun{
## load an example dataset with K=two tasks, p=100 features, and n=200 samples per task:
data(exampleData)
##run simule
result = simule(X = exampleData , 0.05, 1, covType = "cov", FALSE)
## get neighbors of node 50:
net.neighbors(result$Graphs,index=50)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
