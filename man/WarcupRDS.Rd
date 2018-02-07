\name{WarcupRDS}
\alias{WarcupRDS}
\docType{data}
\title{
Warcup training dataset which is trained with funbarRF.
}
\description{
The RDP Warcup ITS training set was retrieved from \url{https://sourceforge.net/projects/rdp-classifier/ }. The collected dataset comprises 17878 sequences belonging to 8551 species. After removing the 2262 singletons, a final dataset comprising 15616 sequences belonging to 6289 species was prepared.

}
\usage{data (WarcupRDS)}

\details{
This dataset can be used to train the Random Forest prediction model in a local server after installing the \strong{funbarRF} package, which can be subsequently used for prediction of the species labels for unknown specimen. For predicting the species labels of unknown specimen, see examples section.
}
\source{
RDP classifier Warcup ITS training dataset (\url{ https://sourceforge.net/projects/rdp-classifier/ })
}
\references{
\enumerate{
\item Deshpande V., Wang Q., Greenfield P., Charleston M., Porras-Alfaro A., Kuske C.R., Cole J.R., Midgley D.J., and Tran-Dinh N. (2016) .Fungal identification using a Bayesian classifier and the Warcup training set of internal transcribed spacer sequences. \emph{Mycologia}. 108(1): 1-5.
}
}
\examples{
\donttest{
#Prepararing the trained model.
data (WarcupRDS) # Loading Warcup ITS training dataset.
trs <- WarcupRDS # Reading Warcup dataset into R.
tr<- trs[1:100]
en_tr <- encGPC (tr) # Encoding of Warcup dataset with gap-pair compositional features.
y1 <- as.factor (rownames(en_tr)) # preparing response vector.
x1 <- en_tr # Preparing predictors.
library(randomForest) # Install the "randomForest" package from CRAN.
ff <- randomForest (y=y1, x=x1, mtry=10, ntree=500) 
# Training with random forest technique. User has to use sufficient number of ntree.
#Preparing the test set.
data (fun_dat)
ms <- read_seq_txt (fun_dat$seq)[1:2] #test/query sequences.
res_enc <- encGPC (ms) #encoding of the query sequences with gap-pair compositionsl features.
#Prediction of species labels for the test set.
test_res <- predict (ff, res_enc, type="response") #prediction of labels for the query sequences.
print (test_res) #priniting the predicted labels.
}
}

\keyword{Warcup ITS}
\keyword{Random Forest}



