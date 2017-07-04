\name{net.hubs}
\alias{net.hubs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get degrees of most connected nodes.
}
\description{
List the degrees of the hub nodes in each class.
}
\usage{
net.hubs(theta, nhubs = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each element represents an estimated sparse inverse covariance matrix.  (For example, the result of SIMULE.)
}
  \item{nhubs}{
The number of hubs to be identified.
}
}

\value{
hubs, a length K list, each element is a vector of Length n hubs giving the degree of the most connected nodes of the corresponding class.}
\references{
Beilun Wang, Ritambhara Singh and Yanjun Qi (2017).  A Constrained L1 Minimization Approach for Estimating Multiple Sparse Gaussian or Nonparanormal Graphical Models. <http://link.springer.com/article/10.1007/s10994-017-5635-7>
}
\author{
Beilun Wang}
\examples{
\dontrun{
## load an example dataset with K=2 tasks, p=100 features, and n=200 samples per task:
data(exampleData)
##run simule
result = simule(X = exampleData , 0.05, 1, covType = "cov", FALSE)
## get hubs list:
net.hubs(result$Graphs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
