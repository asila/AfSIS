library(readr)
# Read pxrf
pxrf <- na.omit(read_csv("~/Dropbox/AfSIS_reporting_data/Seperated_datasets/AfSIS_pxrf_with_llq.csv"))

# function to replace <llq with NA values
llx <- function(x){
	gsub("<llq","NA",x)
}
# replace <llq with NAs
fre <- apply(pxrf,2,llx)

write.table(fre, file = "~/Dropbox/AfSISDB - Sample Datasets/AfSIS_pxrf_NA.csv",row.names=FALSE,sep=",")
fre <- read_csv("~/Dropbox/AfSISDB - Sample Datasets/AfSIS_pxrf_NA.csv")

# function to omit na then compute min
llx<-function(x){
	min(na.omit(x))
}
#List of minimum values
mins <- apply(fre,2,llx)
lld <- as.numeric(mins)*0.05 #downscale the minmums
names(lld)<-colnames(pxrf)

#Replace <llq with lld values
pxrf.f <- matrix(0, nrow=nrow(pxrf),ncol=ncol(pxrf))
for (i in 1:ncol(pxrf)){
	for ( k in 1:nrow(pxrf)){
	pxrf.f[k,i]	<- ifelse(pxrf[k,i]== "<llq",lld[i],pxrf[k,i])[[1]]
	}
} 
colnames(pxrf.f) <- colnames(pxrf)
write.table(pxrf.f, file = "~/Dropbox/AfSIS_reporting_data/Seperated_datasets/AfSIS_baseline_reference_data/AfSIS_pxrf.csv",row.names=FALSE,sep=",")
