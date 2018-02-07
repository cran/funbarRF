\name{read_seq_txt}
\alias{read_seq_txt}

\title{
Conversion of DNA sequences of character types to DNAStringSet types.
}
\description{
This function can be used for conversion of sequence dataset of character types to DNAString format.
}
\usage{read_seq_txt (seq.file)}

\arguments{
  \item{seq.file}{A character vector of barcode sequences.}
}

\value{The function returns input sequences in DNAString format.}

\references{
\enumerate{
 \item Pages H., Aboyoun P., Gentleman R., and DebRoy S. (2016). Biostrings: String objects representing biological sequences, and matching algorithms. R package version 2.42.1.
}
}
\author{
Prabina Kumar Meher, Division of Statistical Genetics,Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}

\seealso{
\code{\link{DNAStringSet}},  \code{\link{readDNAStringSet}},  \code{\link{XStringSet}},  \code{\link{writeXStringSet}}
}
\examples{
data (fun_dat)
bs <- c("AATG","ATTGCCGTA","TTGAACGAAT","TGGCATTG")
read_seq_txt (bs)
kk <- read_seq_txt (fun_dat$seq)

}

\keyword{ DNA barcode }
\keyword{ FASTA format }
\keyword{ DNAString }
