\name{seq_funbarRF_manual}
\alias{seq_funbarRF_manual}

\title{
Conversion of barcode sequences manually collected from BOLD database into numeric features based on gap pair compositions.
}
\description{
This function resulted in similar output as that of \code{\link{seq_funbarRF}} function. The only difference is in input sequences.
To execute this function, the user has to collect the barcode sequences manually from the BOLD database and the same has to be supplied as input to this function.
}
\usage{seq_funbarRF_manual (manual_seq)}

\arguments{
  \item{manual_seq}{ Barcode sequences manually collected from the BOLD database. 

}
}
\details{ This function is a supplemnt to the \code{\link{seq_funbarRF}} function for mapping the manually collected barcode sequences from BOLD database into numeric feature vectors based on gap-pair compositional features.

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
\code{\link{seq_funbarRF}},  \code{\link{data_barcode}},  \code{\link{featureGapPairComposition}}
}
\examples{

data (data_barcode)
tr_ss <- seq_funbarRF_manual (manual_seq=data_barcode$Fish$train[1:2])
print (tr_ss$ref_label)
head (tr_ss$ref_gpc)
######################################

ts_ss <- seq_funbarRF_manual (manual_seq=data_barcode$Inga$test[1:2])
print (tr_ss$ref_label)
head (tr_ss$ref_gpc)


}

\keyword{ BOLD }
\keyword{ CBOL }
\keyword{ Gap-pair feature }




