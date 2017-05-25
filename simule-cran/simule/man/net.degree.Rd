\name{net.degree}
\alias{net.degree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the degree of every node in all classes.
}
\description{
For each class, lists the degree of every node.
}
\usage{
net.degree(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each an estimated sparse inverse covariance matrix.  (For example, the result of simule.)
}
}

\value{
degree, a list of p-length vectors, each giving the degree of all p nodes in the network for the corresponding class.}
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
## get degree list:
net.degree(result$Graphs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
