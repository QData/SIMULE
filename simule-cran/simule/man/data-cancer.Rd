\name{cancer}
\alias{cancer}

\docType{data}

\title{Microarray data set for breast cancer}

\usage{data(cancer)}

\description{%
  This gene expression data set is freely available, coming from the
  Hess \emph{et al}'s paper. It concerns one hundred thirty-three
  patients with stage I--III breast cancer.  Patients were treated with
  chemotherapy prior to surgery. Patient response to the treatment can
  be classified as either a pathologic complete response (pCR) or
  residual disease (not-pCR). Hess \emph{et al} developed and tested a
  reliable multigene predictor for treatment response on this data set,
  composed by a set of 26 genes having a high predictive value.

  The dataset splits into 2 parts (pCR and not pCR), on which network
  inference algorithms should be applied independently or in the
  multitask framework: only individuals from the same classes should be
  consider as independent and identically distributed.
}

\format{%
  A list named \code{cancer} comprising two objects:
  \describe{
    \item{\code{expr}}{a \code{data.frame} with 26 columns and 133
      rows. The \eqn{n}{n}th row gives the expression levels of the 26
      identified genes for the \eqn{n}{n}th patient. The columns are
      named according to the genes.
    }
    \item{\code{status}}{a factor of size 133 with 2 levels
      (\code{"pcr"} and \code{"not"}), describing the status of the patient.
    }
  }
}

\references{%
  K.R. Hess, K. Anderson, W.F. Symmans, V. Valero, N. Ibrahim,
  J.A. Mejia, D. Booser, R.L. Theriault, U.  Buzdar, P.J. Dempsey,
  R. Rouzier, N. Sneige, J.S. Ross, T. Vidaurre, H.L. Gomez,
  G.N. Hortobagyi, and L. Pustzai (2006). Pharmacogenomic predictor of
  sensitivity to preoperative chemotherapy with Paclitaxel and
  Fluorouracil, Doxorubicin, and Cyclophosphamide in breast cancer,
  \emph{Journal of Clinical Oncology}, vol. 24(26), pp. 4236--4244.
}

\examples{
## load the breast cancer data set
data(cancer)
attach(cancer)
}

\keyword{datasets}
