seq_funbarRF <-
function(reference_seq, seq_id){

if(class(reference_seq)!="DNAStringSet")
{ stop("The reference_seq object must be of class DNAStringSet")
}else{

g <- c(0,1,2,3,4,5)
mk <- as.character(as.character(reference_seq))
mz_xdn <- chartr("T","U", mk)
cz.tr <- sapply (mz_xdn,  function(s) sapply (g, function(w)featureGapPairComposition(s,w,class=elements("dnaBase"))[,1:16]))
colnames(cz.tr)<- NULL
pp <- as.character(seq_id)
kk <- list(ref_label=as.factor(pp), ref_gpc=as.matrix(t(cz.tr)))

}
return(kk)

}
