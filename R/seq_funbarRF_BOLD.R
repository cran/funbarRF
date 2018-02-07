seq_funbarRF_BOLD <-
function(object3){

reference_seq <- object3$bold.seq
seq_id <- as.character(object3$seq_name)
g <- c(0,1,2,3,4,5)
mk <- as.character(as.character(reference_seq))
mz_xdn <- chartr("T","U", mk)
cz.tr <- sapply (mz_xdn,  function(s) sapply (g, function(w)featureGapPairComposition(s,w,class=elements("dnaBase"))[,1:16]))
colnames(cz.tr)<-NULL
kk <- list(ref_label=as.factor(seq_id), ref_gpc=as.matrix(t(cz.tr)))
return(kk)
}
