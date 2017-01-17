
#' Set-up with AfSIS1 data:
#' C,N and Mehlich-3 extractable P,K,S,Ca & Mg, from 60 sentinel sites
#' Adapted from M. Walsh, December 2015

# install.packages(c("downloader","baseline","readr","ggplot2","dtw","reshape","soil.spec","plyr","plotly","DT), dependencies=T)
suppressPackageStartupMessages({
require(downloader)
require(baseline)
require(readr)
require(dplyr)
require(ggplot2)
require(dtw)
require(reshape)
require(soil.spec)
require(plyr)
require(plotly)
require(DT)
})

# Data setup --------------------------------------------------------------
# Create a data folder in your current working directory
dir.create("NB60_data", showWarnings=F)
setwd("./NB60_data")

# Download
download("https://www.dropbox.com/s/k9pti8a4fvaxjlm/Nutbal60.zip?dl=1", "Nutbal60.zip", mode="wb")
unzip("Nutbal60.zip", overwrite=T)
prof <- read.table("Profiles.csv", header=T, sep=",") ## profile locations and site names
samp <- read.table("Samples.csv", header=T, sep=",") ## sample data
geos <- read.table("nb60_GS.csv", header=T, sep=",") ## GeoSurvey data
dat <- merge(prof, samp, by="PID")

# This file is missing in this link!
download("https://www.dropbox.com/s/bivkvxrjno8fo67/nb60_micro.csv?dl=1", "nb60_micro.csv", mode="wb")
# mic <- read.table("nb60_micro.csv", header=T, sep=","); #commented to prevent from running 
# dat <- merge(dat, mic, by="SSN"); #commented to prevent from running 


# Reference data setup ----------------------------------------
download("https://www.dropbox.com/s/iwxkuq934p1osnt/AfSIS_reference_data.zip?dl=1", "AfSIS_reference_data.zip", mode="wb")
unzip("AfSIS_reference_data.zip")
wet <- read_csv("AfSIS_wet.csv")
# Exclude "m3.EC S", "ESR", "ESP" variables
wet.rem <- which(colnames(wet)%in% c("m3.EC S", "ESR", "ESP"))
wet <- wet[, -wet.rem]
pxrf <- read_csv("AfSIS_pxrf.csv")
cn <- read_csv("AfSIS_CN.csv")
psa <- read_csv("AfSIS_LDPSA.csv")
# Use fully calgon dispersed for PSA
psa <- psa[, c("SSN","psa.c4clay","psa.c4silt","psa.c4sand")]
colnames(psa) <- c("SSN", "Clay", "Silt", "Sand")

#Remove P, K, S, Ca, Mg from the dat table; they conflict PXRF variables
dat.rem <- which(colnames(dat)%in% c("P", "K", "S","Ca", "Mg"))
dat <- dat[, -dat.rem]

#Remove Site and Cluster from the pxrf table; they conflict dat variables
pxrf.rem <- which(colnames(pxrf)%in% c("Site", "Cluster","Country", "Depth"))
pxrf <- pxrf[, -pxrf.rem]

#Merge all the reference to dat table
datr <- merge(dat, wet, by ="SSN", all.x = TRUE)
datr <- merge(datr, cn, by ="SSN", all.x = TRUE) # add cn
datr <- merge(datr, psa, by ="SSN", all.x = TRUE) # add psa
datr <- merge(datr, pxrf, by ="SSN", all.x = TRUE) # add pxrf

# Other datasets downloaded are for:
# 1. Mineralogical data using XRD;
# 2. Soil moisture; and 
# 3. Atterberg limits

write.csv(datr, "Afsis_reference.csv", row.names=F)

# Remove extraneous objects from memory -----------------------------------
rm(list=c("dat", "wet","cn","psa","pxrf"))

# Load and merge HSTXT MIR spectra ----------------------------------------
download("https://www.dropbox.com/s/6fvipxqlmg704g3/hstxt_MIR.csv.zip?dl=1", "hstxt_MIR.csv.zip", mode="wb")
unzip("hstxt_MIR.csv.zip", overwrite=T)
mir <- read.table("hstxt_MIR.csv", header=F, sep=",", stringsAsFactors=F)
mir <- as.data.frame(mir)
names(mir) <- c("SSN", paste("m", signif(seq(7497.964, 599.76, length.out=3578), 6), sep=""))


# Do baseline correction for the mir table using the default method (irls)
spectra <- as.matrix(mir[,-1])
bl <- baseline(spectra,method="modpolyfit") # or use alternative ilrs
colnames(bl@corrected) <- colnames(spectra)

wavenumbers <- as.numeric(substr(colnames(bl@corrected),2,19))

par(mfrow=c(1,1))
plot(wavenumbers,bl@corrected[1,],type="l",ylim = range(bl@corrected),xlim = c(4000,600))
for(i in 2:nrow(spectra)){
	lines(wavenumbers,bl@corrected[i,],col = "brown")
}
abline(h = 0,lty = 2,col = "grey")
abline(v=c(2371,2350),col = "blue", lty = 2)
text(2350,1.8,col = "blue", labels = "CO2 bands")

#Exclude co2 regions in the MIR ranges (2370 to 2351 cm^-1)
q <- which(wavenumbers > 2350 & wavenumbers < 2371)

png(file =  "Baseline corrected raw spectra.png",height = 400, width = 800)
plot(wavenumbers[-q],bl@corrected[1,-q],type="l",ylim = range(bl@corrected),xlim = c(4000,600),xlab = expression("Wavenumbers cm"^-1),ylab="Absorbance", main = "Baseline corrected raw spectra")
for(i in 2:nrow(spectra)){
	lines(wavenumbers[-q],bl@corrected[i,-q],col = "brown")
}
dev.off()

# Do PCA with the baseline corrected
spectra <- bl@corrected[,-q]
colnames(spectra) <-  wavenumbers[-q]
# Select MIR range (4000 to 600)
mir.r<-which(as.numeric(colnames(spectra))>600 & as.numeric(colnames(spectra))<4001)
spectra<-spectra[,mir.r]

pc <- prcomp(spectra)

# Variance
imp<-round(summary(pc)$importance[2,1:10]*100,1)
imp.pc <- which(imp>1)
imp <- imp[imp.pc]
# Important PCS
k <- length(imp)


# Loadings
loadings <- t(pc$rotation[,1:k])
lpc <- cbind(paste0("PC",c(1:k)," (",imp,"%)"),loadings)
colnames(lpc) <- c("PC",colnames(loadings))
#Save the loadings plot
write.table(lpc, file = "Loadings.csv", sep=",", row.names=FALSE)

# read a file with PCA loadings from the AfSIS MIR baseline corrected spectra
spec <- read.csv("Loadings.csv")
wave <- round(as.numeric(substr(colnames(spec[,-1]),2,19)),1)
colnames(spec) <- c("loadings", wave)
spec.melt <- melt(spec,id="loadings")

# Open up in a browser
datatable(lpc,selection = "multiple")

#By spectra
p<-ggplot(data=spec.melt, aes(x=as.numeric(as.vector(variable)),y=value,group = loadings, color=loadings))+
  geom_line(size=0.34)+
  ggtitle("PCA loadings")+
  xlim(c(4000,600))+
  ylim(range(spec.melt$value))+ 
  xlab(expression("Wavenumbers cm"^-1))+
  ylab("Loadings")+
  theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
  )

ggplotly(p)

# Show each loadings seperately

p<-ggplot(data=spec.melt, aes(x=as.numeric(as.vector(variable)),y=value,group = loadings, color=loadings))+
  geom_line(size=0.44)+
  ggtitle("PCA loadings")+
  xlim(c(4000,600))+
  ylim(range(spec.melt$value))+ 
  xlab(expression("Wavenumbers cm"^-1))+
  ylab("Loadings")+
  theme_bw() +
  theme(plot.background = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid.minor = element_blank()
  )
p1 <- p+theme(legend.position = "none")
p1 <- p1+facet_grid(loadings~. ,scales="free_y",switch="y")
p1 <- p1 + theme(strip.text.y = element_text(colour = "blue", size = 6,hjust = 0.5, vjust = 0.5))
#
ggplotly(p1)

# Get scores for the important PCS
par(mfrow=c(1,1))
pairs(pc$x[,1:k])

# Add SSN to the PCS
pc.ssn <- as.data.frame(cbind(as.vector(mir[,1]),round(pc$x[,1:k],3)))
colnames(pc.ssn) <- c("SSN",paste0("PC",c(1:k)))

