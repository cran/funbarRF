\name{data_barcode}
\alias{data_barcode}
\docType{data}
\title{
Barcode sequences for five differet taxonomical entities i.e., Fish, Bat, Inga, Drosophila and Cypraeidae
}
\description{
This dataset consists of barcode sequences for both reference and query sets pertaining to the above mentioned five taxonomical entities.
}
\usage{data (data_barcode)}

\details{
These datasets have been used in earlier studies for the species identificatin using DNA barcode sequences.
}
\source{
These datasets can be retrieved from \url{ http://dmb.iasi.cnr.it/blog.php }.
Also, can be obtained from \url{ http://cabgrid.res.in:8080/spidbar/Dataset/ED-I/ }.
}
\references{
\enumerate{
\item Weitschek E., Fiscon G., and Felici G. (2014). Supervised DNA barcodes species classification: analysis, comparisons and results. \emph{BioData Mining}, 7, 4.

\item Van Velzen R., Weitschek E., Felici G., Bakker F.T. (2012). DNA barcoding of recently diverged species: relative performance of matching methods. \emph{PLoS One}, 7 (1), e30490.

\item Meher P.K., Sahu T.K., and Rao A.R. (2016). Identification of species based on DNA barcode using k-mer feature vector and Random forest classifier. \emph{Gene}, 592(2), 316-324.
}
}
\examples{

data (data_barcode)
fish_reference <- data_barcode$Fish$train
fish_query <- data_barcode$Fish$test

#######################
\donttest{bat_reference <- data_barcode$Bat$train
bat_query <- data_barcode$Bat$test
#############################
inga_reference <- data_barcode$Inga$train
inga_query <- data_barcode$Inga$test
####################################
drosophila_reference <- data_barcode$Drosophila$train
drosophila_query <- data_barcode$Drosophila$test
####################################
cypraeidae_reference <- data_barcode$Cypraeidae$train
cypraeidae_query <- data_barcode$Cypraeidae$test
}
}
\keyword{datasets}
\keyword{SPIDBAR}
\keyword{BLOG2.0}
