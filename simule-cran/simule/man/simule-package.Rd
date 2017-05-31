\name{simule-package}
\alias{simule-package}
\docType{package}
\title{
Shared and Individual parts of MULtiple graphs Explicitly}
\description{
Run the SIMULE algorithm for the network estimation and the sparse inverse covariance estimation across multiple related datasets.
}
\details{
\tabular{ll}{
Package: \tab simule\cr
Type: \tab Package\cr
Version: \tab 1.1\cr
Date: \tab 2017-05-08\cr
License: \tab GPL (>= 2)\cr
}
Identifying context-specific entity networks from aggregated data is an important task, arising often in bioinformatics and neuroimaging. Computationally, this task can be formulated as jointly estimating multiple different, but related, sparse Undirected Graphical Models (UGM) from aggregated samples across several contexts. Previous joint-UGM studies have mostly focused on sparse Gaussian Graphical Models (sGGMs) and can't identify context-specific edge patterns directly. We, therefore, propose a novel approach, SIMULE (detecting Shared and Individual parts of MULtiple graphs Explicitly) to learn multi-UGM via a constrained L1 minimization. SIMULE automatically infers both specific edge patterns that are unique to each context and shared interactions preserved among all the contexts. Through the L1 constrained formulation, this problem is cast as multiple independent subtasks of linear programming that can be solved efficiently in parallel. In addition to Gaussian data, SIMULE can also handle multivariate nonparanormal data that greatly relaxes the normality assumption that many real-world applications do not follow. We provide a novel theoretical proof showing that SIMULE achieves a consistent result at the rate O(log(Kp)/n_{tot}). On multiple synthetic datasets and two biomedical datasets, SIMULE shows significant improvement over state-of-the-art multi-sGGM and single-UGM baselines.
}
\author{
Beilun Wang

Maintainer: Beilun Wang - bw4mw at virginia dot edu
}
\references{
Beilun Wang, Ritambhara Singh, Yanjun Qi (2017).  A constrained L1 minimization approach for estimating multiple Sparse Gaussian or Nonparanormal Graphical Models. https://arxiv.org/abs/1605.03468
}
\keyword{ package }
\examples{
\dontrun{
data(exampleData)
simule(X = exampleData , 0.05, 1, covType = "cov", TRUE)
}
}
