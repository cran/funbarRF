encGPC <-
function(bar_seq){
g <- c(0,1,2,3,4,5)
ss <- bar_seq
mk <- as.character(as.character(bar_seq))
mz_xdn <- chartr("T","U", mk)
cz.ts <- t(sapply (mz_xdn,  function(s) sapply (g, function(w)featureGapPairComposition(s,w,class=elements("dnaBase"))[,1:16])))
colnames(cz.ts)<-NULL
rownames(cz.ts)<-as.character(names(ss))
test <- cz.ts
return(test)
}
