\name{plot.simule}
\alias{plot.simule}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
  Plotting function for simule objects
}
\description{
  Plotting function for simule objects. This function implements plotting for either the original multiple graphs, the shared graph,
  the subject-specific networks, or the neighborhood networks for certain node.
}
\usage{
  \method{plot}{simule}(x, type="graph", subID=NULL, index=NULL, ...)
}
\arguments{
  \item{x}{
    simule object
  }
  \item{type}{
    Plotting type. This argument defines which type of network(s) is chosen by the user. There are four options:
    "graph": plot the original networks, "share": plot the shared graph, "sub": plot subject-specific network, and "neighbor": plot the neighborhood networks for a given node.
  }
  \item{subID}{
    If \code{type="sub"}, \code{subID} indicates which subject-specific network should be plotted.
  }
  \item{index}{
    If \code{type="neighbor"}, \code{index} indicates which node to choose for plotting the neighborhood networks.
  }
  \item{...}{
    Additional arguments to pass to plot function
  }
}
\details{
  Plotting function for simule objects. It can be used to plot simulated networks or results obtained from running simule algorithm.
}
\references{
Beilun Wang, Ritambhara Singh, Yanjun Qi (2017).  A constrained L1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models. http://link.springer.com/article/10.1007/s10994-017-5635-7
}
\author{
  Beilun Wang and Yanjun Qi
}
\seealso{
  \code{\link{simule}}
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
\keyword{ simule }
