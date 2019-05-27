\name{Unite}
\alias{Unite}
\docType{data}
\title{
UNITE training dataset of 143723 sequences belonging to 9001 species.
}
\description{
The UNITE training dataset used in the RDP classifier was obtained from \url{https://rdp.cme.msu.edu/classifier/classifier.jsp } In the collected dataset, 145019 ITS sequences belonging to 10297 species were present.
After removing 1296 species with single sequences, a dataset comprising of 143723 sequences belonging to 9001 species was prepared. This dataset can be used to train the prediction model using the proposed approach i.e., gap-pair compositional features and Random Forest method.
We couldnot able to train the model with UNITE training dataset due to lack of super computing facility.
However, the user can use this dataset to train the model in local server, details of which is provided in details section.
}
\usage{data (Unite)}

\details{
By using the 143723 sequences of 9001 species, the random Forest model can be trained with gap-pair compositional features. The developed trained model can be subsequently used for predicting the species labels of query barcode sequences.
For step-by-step procedure, see the examples section.
}

\references{
\enumerate{
\item Deshpande V., Wang Q., Greenfield P., Charleston M., Porras-Alfaro A., Kuske C.R., Cole J.R., Midgley D.J., and Tran-Dinh N. (2016). Fungal identification using a Bayesian classifier and the Warcup training set of internal transcribed spacer sequences. \emph{Mycologia}, 108(1), 1-5.

\item Koljalg U., Nilsson R. H., Abarenkov K. et al. (2013). Towards a unified paradigm for sequence-based identification of fungi. \emph{Mol. Ecol.}, 22, 5271-5277.
}
}
\examples{
\donttest{
#Prepararing the trained model.
data (Unite) # Loading UNITE dataset.
trs <- Unite # Reading UNITE dataset into R.
tr<- trs[1:100]
en_tr <- encGPC (tr) # Encoding of UNITE dataset with gap-pair compositional features.
y1 <- as.factor (rownames(en_tr)) # preparing response vector.
x1 <- en_tr # Preparing predictors.
library(randomForest) # Install the "randomForest" package from CRAN.
ff <- randomForest (y=y1, x=x1, mtry=10, ntree=500) # Training with random forest technique.

#Preparing the test set.
data (fun_dat)
ms <- read_seq_txt (fun_dat$seq)[1:2] #test/query sequences.
res_enc <- encGPC (ms) #encoding of the query sequences with gap-pair compositionsl features.

#Prediction of species labels for the test set.
test_res <- predict (ff, res_enc, type="response") #prediction of labels for the query sequences.
print (test_res) #priniting the predicted labels.
}
}
\keyword{datasets}
\keyword{UNITE}
