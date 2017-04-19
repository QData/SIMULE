\name{exampleData}
\alias{exampleData}
\docType{data}
\title{
Toy two task simulated dataset.
}
\description{
A dataset with 100 features and 2 tasks of data, each with 200 observations.  The two tasks' data matrices are stored in a list. The dataset is generated from two random graphs.
}
\usage{data(exampleData)}
\format{
  The format is:
List of 2
 $ : num [1:200, 1:100] -0.0982 -0.2417 -1.704 0.4 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : NULL
 $ : num [1:200, 1:100] -0.161 0.41 0.17 0. ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : NULL
  .. ..$ : NULL
}

\examples{
data(exampleData)
}
\keyword{datasets}
