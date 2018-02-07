\name{predict_train_funbarRF}
\alias{predict_train_funbarRF}
\title{Prediction of species labels for the out-of-bag (OOB) reference barcode sequence using Random Forest.}

\description{
Generally, training or reference dataset is used to train the model and not for prediction purpose. However, since  Random Forest method is used here, prediction for the OOB instances is made. The OOB instances are the observations that are not participated in constructing tree-based classifiers. 
}
\usage{predict_train_funbarRF (object, m_try = 10, n_tree = 500)}

\arguments{
  \item{object}{
An object created by the function \code{\link{seq_funbarRF}} or \code{\link{seq_funbarRF_manual}} or \code{\link{seq_funbarRF_BOLD}}. 
}
  \item{m_try}{
This parameter is required for \code{\link{randomForest}}. It represents the number of variables to be randomly sampled at each split. Default value is 10.
}
  \item{n_tree}{
This is also a parameter for \code{\link{randomForest}}. It denotes the number of tree-based classifiers to be built. This should not be set to too small a number, to ensure that every instance gets predicted at least a few times. Default is 500.
}
}
\details{
The user has to supply the reference sequence dataset to assess the accuracy of the developed prediction approach. Here, the prediction for the species label is made for the OOB instances and are then aggregated over all the classifiers for final prediction based on majority voting strategy. 
}
\value{

 \item{result_train }{A dataframe consisting of species labels, number of species labels observed and correctly predicted. }
 
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
\code{\link{randomForest}},  \code{\link{predict_test_funbarRF}}
}
\examples{
#######################
data (fun_dat) 
kk <- read_seq_txt (fun_dat$seq)[1:5]
zz <- as.factor(as.character (fun_dat$seq_name)[1:5])
train <- seq_funbarRF (reference_seq=kk, seq_id=zz)
res <- predict_train_funbarRF (object=train, m_try=10, n_tree=20) 
# kindly use large number of n_tree
print(res)

######################
\donttest{
data (data_barcode)
tr_ss <- seq_funbarRF_manual (manual_seq=data_barcode$Fish$train[1:100])
prd1 <- predict_train_funbarRF (object=tr_ss, m_try=10, n_tree=500)
# kindly use large number of n_tree
print(prd1)

############################
tr_bold <- seq_BOLD (taxon="Campylospora")
tr_seq <- seq_funbarRF_BOLD (object3=tr_bold)
prd <- predict_train_funbarRF (object=tr_seq, m_try=10, n_tree=20) 
# kindly use large number of n_tree
print(prd)
}
}

\keyword{ Random Forest }
\keyword{ Machine learning}
\keyword{ Predictors}
\keyword{ Observed label}
\keyword{ Predicted label}
