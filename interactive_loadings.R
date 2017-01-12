suppressMessages(library(dtw))
suppressMessages(library(readr))
suppressMessages(library(ggplot2))
suppressMessages(library(dplyr))
suppressMessages(library(reshape))
suppressMessages(library(soil.spec))
suppressMessages(library(plyr))
suppressMessages(library(plotly))
suppressMessages(library(DT))

# read a file with PCA loadings from the AfSIS MIR baseline corrected spectra
spec <- read.csv("~/studies/AfSIS_data/Loadings.csv")
wave <- round(as.numeric(substr(colnames(spec[,-1]),2,19)),1)
colnames(spec) <- c("loadings", wave)
spec.melt <- melt(spec,id="loadings")

datatable(spec,selection = "multiple")
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

# Show each loadings seprately

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
