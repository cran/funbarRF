\name{encGPC}
\alias{encGPC}

\title{
Encoding barcode sequences using gap-pair compositional features.
}
\description{
This function can be used for encoding the barcode sequences with gap-pair compositional features. This is an alternative function to \code{\link{seq_funbarRF}} and \code{\link{seq_funbarRF_manual}} functions.
}
\usage{
encGPC (bar_seq)
}
\arguments{
  \item{bar_seq}{
Barcode sequences of class DNAStringSet.
}
}
\details{
The user has to supply the barcode sequences in FASTA format, the class of which must be of DNAString type. It can also be an object generated using \code{\link{read_seq_txt}} function.

}
\value{
\item{test }{A dataframe of \emph{N} rows and 96 columns, where \emph{N} is the number of input sequences supplied by the user.}

}
\references{
\enumerate{

\item  Li H. (2016). BioSeqClass: Classification for biological Sequences. R package version 1.32.0.
}
}
\author{
Prabina Kumar Meher, Division of Statistical Genetics,Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA

}

\seealso{
\code{\link{seq_funbarRF}},  \code{\link{seq_funbarRF_manual}}
}
\examples{
data (fun_dat)
ms <- read_seq_txt (fun_dat$seq)[1:2]
res <- encGPC (ms)
head (res)

}
\keyword{ Random Forest }
\keyword{ Prediction }
\keyword{Gap-pair composition }

