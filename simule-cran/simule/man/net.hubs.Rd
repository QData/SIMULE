\name{net.hubs}
\alias{net.hubs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get degrees of most connected nodes.
}
\description{
List the degrees of the most connected nodes in each class.
}
\usage{
net.hubs(theta, nhubs = 10)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{theta}{
A list of pXp matrices, each an estimated sparse inverse covariance matrix.  (For example, the result of FGL or GGL.)
}
  \item{nhubs}{
The number of hubs to be identified.  net.hubs() will list the degree of the nhubs most connected nodes in each class.
}
}

\value{
hubs, a list of length K, each element of which is a vector giving the degree of the most connected nodes in the corresponding class.}
\references{
Patrick Danaher, Pei Wang and Daniela Witten (2011).  The joint graphical lasso for inverse covariance estimation across multiple classes.  http://arxiv.org/abs/1111.0324
}
\author{
Patrick Danaher
}
\examples{
## load an example dataset with K=two tasks, p=100 features, and n=200 samples per task:
data(exampleData)
##run simule
result = simule(X = exampleData , 0.05, 1, covType = "cov", FALSE)
## get hubs list:
net.hubs(result$Graphs)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
