\name{seq_funbarRF_BOLD}
\alias{seq_funbarRF_BOLD}

\title{
Conversion of barcode sequences into numeric feature vectors based on gap pair compositions, only for the sequences collected from BOLD database using the function \code{\link{seq_BOLD}}.
}
\description{
This function provides similar output as that of \code{\link{seq_funbarRF}} and \code{\link{seq_funbarRF_manual}} functions. The only difference is in input sequences. To execute this function, the user has to collect the barcode sequences from the BOLD databse using the function \code{\link{seq_BOLD}} and the same has to be supplied as input to this function.
}

\usage{seq_funbarRF_BOLD (object3)}

\arguments{
  \item{object3}{
An object generated using the function \code{\link{seq_BOLD}}.
}
}
\details{
This function is a supplemnt to the \code{\link{seq_funbarRF}} and \code{\link{seq_funbarRF_manual}} functions for mapping the barcode sequences from BOLD database into numeric vectors based on gap-pair compositional features.
}
\value{

 \item{ref_label }{Species labels of barcode sequences as factor.}
 \item{ref_gpc }{A matrix of dimension \emph{N}*96, where \emph{N} is the number of sequences and 96 columns represent the gap pair composition features for 0, 1, 2, 3, 4 and 5 gaps together.}

}
\references{
\enumerate{
\item Yu C.S., Chen Y.C., Lu C.H., and Hwang J.K. (2006). Prediction of protein subcellular localization. \emph{Proteins}, 64(3), 643-651.

\item Meher P.K., Sahu T.K., Gahoi S., and Rao A.R. (2018). ir-HSP: Improved recognition of heat shock proteins, their families and sub-types based on g-spaced di-peptide features and support vector machine. \emph{Front. Genet}., 8, 235.

\item  Li H. (2016). BioSeqClass: Classification for biological Sequences. R package version 1.32.0.
}
}
\author{
Prabina Kumar Meher, Division of Statistical Genetics,Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA

}

\seealso{
\code{\link{seq_funbarRF}},  \code{\link{seq_funbarRF_manual}},  \code{\link{featureGapPairComposition}}
}
\examples{

funbar_p3 <- seq_BOLD (taxon="Campylospora")
res_p3 <- seq_funbarRF_BOLD (object3=funbar_p3)
####################################
\donttest{
funbar_p1 <- seq_BOLD (taxon="Erysiphales")
res_p1 <- seq_funbarRF_BOLD (object3=funbar_p1)
##########################################
}

}
\keyword{ BOLD }
\keyword{ CBOL }
\keyword{ Gap-pair feature }