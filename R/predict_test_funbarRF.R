predict_test_funbarRF <-
function(object1, object2, m_try=10, n_tree=500){

y <- as.factor(as.character(object1$ref_label))
x <- object1$ref_gpc
mt <- m_try
nt <- n_tree
suppressWarnings(ff <- randomForest(y~.,x, mtry=mt, ntree=nt))
test <- object2$ref_gpc
predicted_class <- predict(ff, test, type="response")
test.res <- data.frame(predicted_class)
rownames(test.res) <- NULL
return(test.res)

}
