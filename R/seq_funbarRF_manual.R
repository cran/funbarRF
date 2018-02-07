seq_funbarRF_manual <-
function(manual_seq){

xdn <- manual_seq
z <- sapply(names(xdn),function(s)strsplit(s, split="|", fixed=T))
mm <- lapply(z, unlist)
mk <- sapply(mm, function(s){z <- s[2]; z})
write.table(mk, file="names.txt")
k1 <- read.csv(file="names.txt", header=F, sep="")
mk <- as.character(k1[,2])
mms <- function(s)unlist(strsplit(s, split=" ", fixed=T))
zz <- sapply(mk,mms)
colp <- function(s){paste(s, collapse="_")}
ss <- unlist(lapply(zz,colp))
y.tr <- as.factor(paste(ss))
y <- as.factor(paste(y.tr[-1])) 
reference_seq <- xdn
seq_id <- as.character(y)
g <- c(0,1,2,3,4,5)
mk <- as.character(as.character(reference_seq))
mz_xdn <- chartr("T","U", mk)
cz.tr <- sapply (mz_xdn,  function(s) sapply (g, function(w)featureGapPairComposition(s,w,class=elements("dnaBase"))[,1:16]))
colnames(cz.tr)<-NULL
kk <- list(ref_label=as.factor(seq_id), ref_gpc=as.matrix(t(cz.tr)))
return(kk)
}
