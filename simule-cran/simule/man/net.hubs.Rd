\name{net.hubs}
\alias{net.hubs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get degrees of the most connected nodes of each graph in the input list of multiple graphs.
}
\description{
List the degrees of the hub nodes of each graph in the input list of multiple graphs.
}
\usage{
net.hubs(theta, nhubs = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
  An input list of multiple graphs. Each graph is represented as a pXp matrix. (For example, the result of the SIMULE algorithm: a list of pXp matrices in which each matrix represents an estimated sparse inverse covariance matrix.)
}
  \item{nhubs}{
The number of hubs to be identified of each graph in the input list of multiple graphs.
}
}

\value{
hubs, a length K list. Each element in this list is a vector of length nhubs whose entries give the degree of the most connected nodes of each graph in the input list of multiple graphs.}
\references{
Beilun Wang, Ritambhara Singh and Yanjun Qi (2017).  A Constrained L1 Minimization Approach for Estimating Multiple Sparse Gaussian or Nonparanormal Graphical Models. <http://link.springer.com/article/10.1007/s10994-017-5635-7>
}
\author{
Beilun Wang}
\examples{
\dontrun{
## load an example multi-task dataset with K=2 tasks, p=100 features, and n=200 samples per task:
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
