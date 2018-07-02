#A simplex solver for linear programming problem in (N)SIMULE
.wlinprogSPar <- function(i, Sigma, W, lambda){
  # num of p * N
  # pTimesN = nrow(Sigma)
  # num of p * (N + 1)
  # Get parameters
  q = ncol(Sigma)
  p = ncol(Sigma) - nrow(Sigma)
  N = nrow(Sigma) / p
  # Generate e_j
  e = rep(0, p * N)
  for(j in 1:N){
    e[i + (j - 1) * p] = 1
  }
  # linear programming solution
  f.obj = rep(W[i, ], 2 * (N+1))
  con1 = cbind(-Sigma, +Sigma)
  b1 = lambda - e
  b2 =  lambda + e
  f.con = rbind(-diag(2 * q), con1, -con1)
  f.rhs = c(rep(0, 2 * q), b1, b2)
  f.dir = rep("<=", length(f.rhs))
  lp.out = lp("min", f.obj, f.con, f.dir, f.rhs)
  beta = lp.out$solution[1:q] - lp.out$solution[(q + 1):(2 * q)]
  if (lp.out$status == 2) warning("No feasible solution!  Try a larger tuning parameter!")
  return(beta)
}

#' A constrained and weighted l1 minimization approach for estimating multiple
#' Sparse Gaussian or Nonparanormal Graphical Models
#'
#' Estimate multiple, related sparse Gaussian or Nonparanormal graphical models
#' from multiple related datasets using the SIMULE algorithm. Please run
#' demo(wsimule) to learn the basic functions provided by this package.
#' For further details, please read the original paper: Beilun Wang, Ritambhara
#' Singh, Yanjun Qi (2017) <DOI:10.1007/s10994-017-5635-7>.
#'
#' The SIMULE algorithm is a constrained l1 minimization method that can detect
#' both the shared and the task-specific parts of multiple graphs explicitly
#' from data (through jointly estimating multiple sparse Gaussian graphical
#' models or Nonparanormal graphical models). It solves the following equation:
#' \deqn{ \hat{\Omega}^{(1)}_I, \hat{\Omega}^{(2)}_I, \dots,
#' \hat{\Omega}^{(K)}_I, \hat{\Omega}_S =
#' \min\limits_{\Omega^{(i)}_I,\Omega_S}\sum\limits_i ||W \cdot
#' \Omega^{(i)}_I||_1+ \epsilon K||W \cdot \Omega_S||_1 } Subject to : \deqn{
#' ||\Sigma^{(i)}(\Omega^{(i)}_I + \Omega_S) - I||_{\infty} \le \lambda_{n}, i
#' = 1,\dots,K \nonumber } Please also see the equation (7) in our paper. The
#' \eqn{\lambda_n} is the hyperparameter controlling the sparsity level of the
#' matrices and it is the \code{lambda} in our function. The \eqn{\epsilon} is
#' the hyperparameter controlling the differences between the shared pattern
#' among graphs and the individual part of each graph. It is the \code{epsilon}
#' parameter in our function and the default value is 1. For further details,
#' please see our paper:
#' <http://link.springer.com/article/10.1007/s10994-017-5635-7>.
#'
#' @param X A List of input matrices. They can be data matrices or
#' covariance/correlation matrices. If every matrix in the X is a symmetric
#' matrix, the matrices are assumed to be covariance/correlation matrices. More
#' details at <https://github.com/QData/SIMULE>
#' @param lambda A positive number. The hyperparameter controls the sparsity
#' level of the matrices. The \eqn{\lambda_n} in the following section:
#' Details.
#' @param epsilon A positive number. The hyperparameter controls the
#' differences between the shared pattern among graphs and the individual part
#' of each graph. The \eqn{\epsilon} in the following section: Details. If
#' epsilon becomes larger, the generated graphs will be more similar to each
#' other. The default value is 1, which means that we set the same weights to
#' the shared pattern among graphs and the individual part of each graph.
#' @param W A weight matrix. This matrix uses the prior knowledge of the
#' graphs. For example, if we use wsimule to infer multiple human brain
#' connectome graphs, the \eqn{W} can be the anatomical distance matrix of
#' human brain. The default value is a matrix, whose entries all equals to 1.
#' This means that we do not have any prior knowledge.
#' @param covType A parameter to decide which Graphical model we choose to
#' estimate from the input data.
#'
#' If covType = "cov", it means that we estimate multiple sparse Gaussian
#' Graphical models. This option assumes that we calculate (when input X
#' represents data directly) or use (when X elements are symmetric representing
#' covariance matrices) the sample covariance matrices as input to the simule
#' algorithm.
#'
#' If covType = "kendall", it means that we estimate multiple nonparanormal
#' Graphical models. This option assumes that we calculate (when input X
#' represents data directly) or use (when X elements are symmetric representing
#' correlation matrices) the kendall's tau correlation matrices as input to the
#' simule algorithm.
#' @param parallel A boolean. This parameter decides if the package will use
#' the multithreading architecture or not.
#' @return \item{Graphs}{A list of the estimated inverse covariance/correlation
#' matrices.} \item{share}{The share graph among multiple tasks.}
#' @author Beilun Wang
#' @references Beilun Wang, Ritambhara Singh, Yanjun Qi (2017).  A constrained
#' L1 minimization approach for estimating multiple Sparse Gaussian or
#' Nonparanormal Graphical Models.
#' http://link.springer.com/article/10.1007/s10994-017-5635-7
#' @examples
#' \dontrun{
#' data(exampleData)
#' result = wsimule(X = exampleData , lambda = 0.1, epsilon = 0.45,
#' W = matrix(1,20,20), covType = "cov", FALSE)
#' plot.simule(result)
#' }
#' @export
#' @import lpSolve
#' @import parallel
#' @import pcaPP
#' @importFrom stats cov
wsimule <- function(X, lambda, epsilon = 1, W, covType = "cov",parallel = FALSE ){

  if (is.data.frame(X[[1]])){
    for (i in 1:(length(X))){
      X[[i]] = as.matrix(X[[i]])
    }
  }

  #get number of tasks
  N = length(X)
  #get the cov/cor matrices
  if (isSymmetric(X[[1]]) == FALSE){
    try(if (covType %in% c("cov","kendall") == FALSE) stop("The cov/cor type you specifies is not include in this package. Please use your own function to obtain the list of cov/cor and use them as the input of simule()"))
    if (covType == "cov")
    {
      for (i in 1:N){
        X[[i]] = cov(X[[i]])
      }
    }
    if (covType == "kendall"){
      for(i in 1:N){
        X[[i]] = cor.fk(X[[i]])
      }
    }
  }
  # initialize the parameters
  Graphs = list()
  p = ncol(X[[1]])
  if (missing(W)){
    W = matrix(1, p, p)
  }
  xt = matrix(0, (N + 1) * p, p)
  I = diag(1, p, p)
  Z = matrix(0, p, p)
  # generate the condition matrix A
  A = X[[1]]
  for(i in 2:N){
    A = cbind(A,Z)
  }
  A = cbind(A,(1/(epsilon * N))*X[[1]])
  for(i in 2:N){
    temp = Z
    for(j in 2:N){
      if (j == i){
        temp = cbind(temp,X[[i]])
      }
      else{
        temp = cbind(temp,Z)
      }
    }
    temp = cbind(temp, 1/(epsilon * N) * X[[i]])
    A = rbind(A, temp)
  }
  # define the function f for parallelization
  f = function(x) .wlinprogSPar(x, A, W, lambda)

  if(parallel == TRUE){ # parallel version
    # number of cores to collect,
    # default number is number cores in your machine - 1,
    # you can set your own number by changing this line.
    no_cores = detectCores() - 1
    cl = makeCluster(no_cores)
    # declare variable and function names to the cluster
    clusterExport(cl, list("f", "A", "W", "lambda", ".linprogSPar", "lp"), envir = environment())
    result = parLapply(cl, 1:p, f)
    #print('Done!')
    for (i in 1:p){
      xt[,i] = result[[i]]
    }
    stopCluster(cl)
  }else{ # single machine code
    for (i in 1 : p){
      xt[,i] = f(i)
      if (i %% 10 == 0){
        cat("=")
        if(i %% 100 == 0){
          cat("+")
        }
      }
    }
    print("Done!")
  }

  for(i in 1:N){
    # combine the results from each column. (\hat{\Omega}_{tot}^1)
    Graphs[[i]] = xt[(1 + (i-1) * p):(i * p),] + 1/(epsilon * N) * xt[(1 + N * p):((N + 1) * p),]
    # make it be symmetric
    for(j in 1:p){
      for(k in j:p){
        if (abs(Graphs[[i]][j,k]) < abs(Graphs[[i]][k,j])){
          Graphs[[i]][j,k] = Graphs[[i]][j,k]
          Graphs[[i]][k,j] = Graphs[[i]][j,k]
        }
        else{
          Graphs[[i]][j,k] = Graphs[[i]][k,j]
          Graphs[[i]][k,j] = Graphs[[i]][k,j]
        }
      }
    }
  }
  share = 1/(epsilon * N) * xt[(1 + N * p):((N + 1) * p),]
  for(j in 1:p){
    for(k in j:p){
      if (abs(share[j,k]) < abs(share[k,j])){
        share[j,k] = share[j,k]
        share[k,j] = share[j,k]
      }
      else{
        share[j,k] = share[k,j]
        share[k,j] = share[k,j]
      }
    }
  }
  out = list(Graphs = Graphs, share = share)
  class(out) = "simule"
  return(out)
}


