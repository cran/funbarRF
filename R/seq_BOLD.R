seq_BOLD <-
function(taxon){
res <- bold_seq(taxon)
zseq <- DNAStringSet(as.character(lapply(res, function(s) subseq(s$sequence, start=1, end=nchar(s$sequence)-1))))
znam <- as.character(lapply(res, function(s) s$name))
zz <- sapply(znam,function(s)unlist(strsplit(s, split=" ", fixed=T)))
nms <- as.character(lapply(zz, function(s) paste(s, collapse="_")))
names(zseq)<- nms
seqid <- list(bold.seq=zseq, seq_name=as.factor(nms))
return(seqid)
}
