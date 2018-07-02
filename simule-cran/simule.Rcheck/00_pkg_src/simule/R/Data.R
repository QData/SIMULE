#' A simulated toy dataset that includes 2 data matrices (from 2 related
#' tasks).
#'
#' A simulated toy dataset that includes 2 data matrices (from 2 related
#' tasks). Each data matrix is about 100 features observed in 200 samples. The
#' two data matrices are about exactly the same set of 100 features. This
#' multi-task dataset is generated from two related random graphs. Please run
#' demo(simule) to learn the basic functions provided by this package.  For
#' further details, please read the original paper:
#' <http://link.springer.com/article/10.1007/s10994-017-5635-7>.
#'
#'
#' @name exampleData
#' @docType data
#' @format The format is: List of 2 matrices $ : num [1:200, 1:100] -0.0982
#' -0.2417 -1.704 0.4 ...  ..- attr(*, "dimnames")=List of 2 .. ..$ : NULL ..
#' ..$ : NULL $ : num [1:200, 1:100] -0.161 0.41 0.17 0. ...  ..- attr(*,
#' "dimnames")=List of 2 .. ..$ : NULL .. ..$ : NULL
#' @keywords datasets
#' @usage data(exampleData)
NULL


#' A simulated toy dataset that includes 3 igraph objects
#'
#' (first one being the shared graph and second and third being task specific 1 and 2 graphs)
#' The graphs are generated from two related random graphs and the underlaying high dimensional gaussian distribution
#' generates the exampleData dataset. exampleDataGraph serves as a groundtruth to compare in demo(synthetic).
#'
#'
#' @name exampleDataGraph
#' @docType data
#' @format A list of 3 igraph objects
#' @keywords datasets
#' @usage data(exampleDataGraph)
NULL

#' Microarray data set for breast cancer
#'
#' % This gene expression data set is freely available, coming from the Hess
#' \emph{et al}'s paper. It concerns one hundred thirty-three patients with
#' stage I--III breast cancer.  Patients were treated with chemotherapy prior
#' to surgery. Patient response to the treatment can be classified as either a
#' pathologic complete response (pCR) or residual disease (not-pCR). Hess
#' \emph{et al} developed and tested a reliable multigene predictor for
#' treatment response on this data set, composed by a set of 26 genes having a
#' high predictive value.
#'
#' The dataset splits into 2 parts (pCR and not pCR), on which network
#' inference algorithms should be applied independently or in the multitask
#' framework: only individuals from the same classes should be consider as
#' independent and identically distributed.
#'
#'
#' @name cancer
#' @docType data
#'
#' @format a list of two objects: dataframe with 133 observations of 26 features and
#' factors indicating whether each sample (out of 133) is of type "not" or type "pcr"
#'
#'
#' @references % K.R. Hess, K. Anderson, W.F. Symmans, V. Valero, N. Ibrahim,
#' J.A. Mejia, D. Booser, R.L. Theriault, U.  Buzdar, P.J. Dempsey, R. Rouzier,
#' N. Sneige, J.S. Ross, T. Vidaurre, H.L. Gomez, G.N. Hortobagyi, and L.
#' Pustzai (2006). Pharmacogenomic predictor of sensitivity to preoperative
#' chemotherapy with Paclitaxel and Fluorouracil, Doxorubicin, and
#' Cyclophosphamide in breast cancer, \emph{Journal of Clinical Oncology}, vol.
#' 24(26), pp. 4236--4244.
#' @keywords datasets
#' @usage data(cancer)
NULL

#' NIPS word count dataset
#'
#' This NIPS Conference Papers 1987-2015 Data set is avaiable at UCI Machine Learning Repository.
#' The original dataset is in the form of a 11463 x 5812 matrix of word counts (11463 words and 5812 conference papers)
#' Due to the size of the original dataset, it is preprocessed and reduced to a list of two matrices (2900 x 37 and 2911 x 37)
#' The dataset consists of two tasks (early (up to 2006) and recent (after 2006) NIPS conference papers) with 37 words
#'
#'
#' @name nip_37_data
#' @docType data
#' @references 'Poisson Random Fields for Dynamic Feature Models'. Perrone V., Jenkins P. A., Spano D., Teh Y. W. (2016)
#' @format a list of two nonnegative integer matrices (1:2900, 1:37) and (1:2911,1:37)
#' Columns are named with year_paperid and rows are names with word name
#' @keywords datasets
#' @usage data(nip_37_data)
NULL
