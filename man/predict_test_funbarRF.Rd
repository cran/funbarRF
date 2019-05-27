\name{predict_test_funbarRF}
\alias{predict_test_funbarRF}

\title{
Prediction of species label for the query fungal barcode sequences.
}
\description{
This function can be used for predicting the species labels for the fungal barcode sequences of the query set, using the model trained with reference barcode sequences.
}
\usage{predict_test_funbarRF (object1, object2, m_try = 10, n_tree = 500)}

\arguments{
  \item{object1}{
An object created by the function \code{\link{seq_funbarRF}} or \code{\link{seq_funbarRF_manual}} , with \strong{reference dataset} as input.
}
  \item{object2}{
An object created by the function \code{\link{seq_funbarRF}} or \code{\link{seq_funbarRF_manual}} , with \strong{query dataset} as input.
}
  \item{m_try}{
This parameter is required for \code{\link{randomForest}}. It represents the number (must be an integer) of variables to be randomly sampled at each split. Default value is 10.

}
  \item{n_tree}{
This is also a parameter for \code{\link{randomForest}}. It denotes the number (must be an integer) of tree-based classifiers to be built. This should not be set to too small, to ensure that every instance gets predicted at least a few times. Default is 500.

}
}

\value{
A dataframe consisting of predicted species label for each sequence of the query dataset.
}
\references{
\enumerate{
\item Liaw A., and Wiener M. (2002). Classification and Regression by randomForest. \emph{R News}, 2(3), 18-22.

\item Meher P.K., Sahu T.K., and Rao A.R. (2016). Identification of species based on DNA barcode using k-mer feature vector and Random forest classifier. \emph{Gene}, 592(2), 316-324.
}
}
\author{
Prabina Kumar Meher, Division of Statistical Genetics,Indian Agricultural Statistics Research Institute, New Delhi-110012, INDIA

}


\seealso{
\code{\link{randomForest}},  \code{\link{predict_train_funbarRF}},  \code{\link{predict}}
}
\examples{

data (data_barcode)
train1 <- seq_funbarRF_manual (manual_seq=data_barcode$Fish$train[1:30])
test1 <- seq_funbarRF_manual (manual_seq=data_barcode$Fish$test[1:3])
res1 <- predict_test_funbarRF (object1=train1, object2=test1, m_try = 10, n_tree = 5)
# kindly use large number of n_tree
print(res1)


##################################
\donttest{
data (data_barcode)
train2 <- seq_funbarRF_manual (manual_seq=data_barcode$Inga$train[1:30])
test2 <- seq_funbarRF_manual (manual_seq=data_barcode$Inga$test[1:3])
res2 <- predict_test_funbarRF (object1=train2, object2=test2, m_try = 10, n_tree = 20) 
# kindly use large number of n_tree
print(res2)
}
}
\keyword{ Random Forest }
\keyword{ Prediction }
\keyword{Species label }

