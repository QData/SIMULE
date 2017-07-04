\name{net.edges}
\alias{net.edges}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the edges in a network
}
\description{
For each class, list every estimated edge in the form of pair of connected nodes.
}
\usage{
net.edges(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, Each matrix in the list represents an estimated sparse inverse covariance matrix.
}
}

\value{
edges, a length K list, each element of the list represents an igraph.es object which is the detail of all pairs of connected nodes in the class.
}
\references{
Beilun Wang, Ritambhara Singh, Yanjun Qi (2017).  A constrained L1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models. <http://link.springer.com/article/10.1007/s10994-017-5635-7>
}
\author{
Beilun Wang}

\examples{
\dontrun{
## load an example dataset with K=2 tasks, p=100 features, and n=200 samples per task:
data(exampleData)
##run simule
result = simule(X = exampleData , 0.05, 1, covType = "cov", FALSE)
## get edges list:
net.edges(result$Graphs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
