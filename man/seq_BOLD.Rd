\name{seq_BOLD}
\alias{seq_BOLD}

\title{Retrieveing barcode sequences from BOLD \url{ http://www.boldsystems.org/ } database.}

\description{This function can be used to retrieve fungal barcode sequences from BOLD database, according to user supplied taxon.}

\usage{
seq_BOLD (taxon)
}

\arguments{
  \item{taxon}{A character including taxa maching phylum, class, order, family and genera.}
}

\value{

\item{bold.seq }{Barcode sequences of fungi for the user supplied taxa.}
 \item{seq.name }{The sequence identifiers for the corresponding barcode sequences.}

}
\references{
\enumerate{
\item Chamberlain S. (2017). bold: Interface to Bold Systems API. R package version 0.5.0. \url{ https://CRAN.R-project.org/package=bold }.
}
}
\author{
Prabina Kumar Meher, Division of Statistical Genetics,Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA
}
\note{
The function \code{\link{seq_BOLD}} can be used as an alternative to the manual extraction of DNA barcode sequences from BOLD systems. Besides funagl species, this function can also be used to retrieve barcode sequences for other species.
}


\seealso{
\code{\link{seq_funbarRF}},  \code{\link{seq_funbarRF_manual}},  \code{\link{seq_funbarRF_BOLD}} 
}
\examples{

#Extracting sequence with genera rank
funbar_p5 <- seq_BOLD (taxon="Campylospora")

\donttest{
#Extracting sequence with order rank
funbar_p3 <- seq_BOLD (taxon="Erysiphales")
}

}

\keyword{ Fungal Barcode }
\keyword{ BOLD }
