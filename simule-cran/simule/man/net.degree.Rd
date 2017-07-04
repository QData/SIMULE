\name{net.degree}
\alias{net.degree}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
List the degree of every node in all classes.
}
\description{
Lists the degree of every node for each class.
}
\usage{
net.degree(theta)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices in which each matrix represents an estimated sparse inverse covariance matrix.  (For example, the result of the SIMULE algorithm.)
}
}

\value{
Degrees, in the format of a list of length p vectors represents the degree of all p nodes in the network for a class.}
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
## get degree list:
net.degree(result$Graphs)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
