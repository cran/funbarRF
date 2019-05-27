\name{seq_funbarRF}
\alias{seq_funbarRF}

\title{
Conversion of barcode sequences into numeric vectors based on gap pair compositions, with user supplied barcode sequences and species labels.
}
\description{
This function can be used to map the sequence dataset onto numeric feature vectors, based on gap pair composition features. This function requires the barcode sequences in DNAString format and the species label of each sequence as factor. The resultant output can be directly used as input to train the random forest based prediction model. 
}
\usage{seq_funbarRF (reference_seq, seq_id)}

\arguments{
  \item{reference_seq}{Barcode sequences of class DNAStringSet. It can also be an object generated using the function \code{\link{read_seq_txt}}.}
  \item{seq_id}{A vctor of species labels as factor. The length of the vector must be equal to the number of sequences in reference_seq.}
}
\details{
For the argument seq_id, user has to supply the species label for each sequence in the specified format. For example, the species label \emph{Absidia caerulea} should written as \emph{Absidia_caerulea}. The class of the \code{seq_id} must be of factor type.
}
\value{

 \item{ref_label }{Species labels of barcode sequences as factor. }
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
\code{\link{seq_funbarRF_manual}},  \code{\link{featureGapPairComposition}}

}
\examples{
data(fun_dat) 
kk <- read_seq_txt (fun_dat$seq)[1:2]
zz <- as.factor (as.character(fun_dat$seq_name[1:2]))
res <- seq_funbarRF (reference_seq=kk, seq_id=zz)
print (res$ref_label)
head (res$ref_gpc)
}

\keyword{ Fungal barcode }
\keyword{ DNAStringSet}
\keyword{ Gap-pair compositions}









