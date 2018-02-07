predict_train_funbarRF <-
function(object, m_try=10, n_tree=500){

y <- as.factor(as.character(object$ref_label))
x <- object$ref_gpc
mt <- m_try
nt <- n_tree
suppressWarnings(ff <- randomForest(y ~.,x, mtry=mt, ntree=nt))
zz <- ff$confusion
dz <- diag(zz)
up <- table(y)
result_train <- data.frame(up, dz)
colnames(result_train) <- c("Species", "#Observed","#Correctly predicted")
rownames(result_train) <- NULL
return(result_train)
}
