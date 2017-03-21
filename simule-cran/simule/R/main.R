##############
#author: Beilun Wang
#input: 
#X       -- a list of data matrices
#method  -- string vector to decide which method to use, 
#           options include "SIMULE", "NSIMULE", "SIMULE-I", "NSIMULE-I", "SIMULE-P"
#lambda  -- sparse parameter
#epsilon -- group parameter
#parallel-- boolean scalar decide if use parallel implementation or not
#output:
#graphs  -- a list of inverse of covariance matrices
##############
main <- function(X, method = "SIMULE", lambda = 0.1, epsilon = 1, parallel = FALSE){
  source("SIMULE.R")
  result = list()
  if(method == "SIMULE"){
    N = length(X)
    covs = list()
    for(i in 1:N){
      covs[[i]] = cov(X[[i]])
    }
    result = SIMULE(covs, lambda, epsilon, parallel)
  }
  if(method == "NSIMULE"){
    library('pcaPP')
    N = length(X)
    cors = list()
    for(i in 1:N){
      cors[[i]] = cor.fk(X[[i]])
    }
    result = SIMULE(cors, lambda, epsilon, parallel)
  }
  if(method == "NSIMULE-I"){
    source("intertwined.R")
    cors = InterwinedLasso(X, method = "kendall")
    result = SIMULE(cors, lambda, epsilon, parallel)
  }
  if(method == "SIMULE-I"){
    source("intertwined.R")
    cors = InterwinedLasso(X, method = "pearson")
    result = SIMULE(cors, lambda, epsilon, parallel)
  }
  if(method == "SIMULE"){
    library(POET)
    N = length(X)
    covs = list()
    for(i in 1:N){
      covs[[i]] = POET(X[[i]])
    }
    result = SIMULE(covs, lambda, epsilon, parallel)
  }
  result
}