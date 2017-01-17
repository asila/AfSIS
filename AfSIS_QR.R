# 'This is a program for creating random alpha numeric strings
# 'which are then used to generate quick response (QR) codes. 
# 'Both the alpha numeric strings and QR codes can be downloaded to 
# 'user's computer. Begin by install the two required packages: shiny
# ' and qrcode

list.of.packages <- c("shiny", "qrcode")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

library(shiny)
library(qrcode)

# Define the UI ------------------------
# Global variables first

n = sample(1:100,1) # At least one alpha numeric should be requested for this code

study <- 'VTS' # Default_prefix

ui <- fluidPage(
			titlePanel("Generating alpha-numeric strings for QR codes"),
		sidebarLayout(
		sidebarPanel(
			numericInput('n', 'Number of QR codes required:', n),
				verticalLayout(
				helpText("Type the number of codes required. It is faster!",style = "color:blue"),
				helpText("Provide study name e.g. AFSIS",style = "color:grey"),
				 tags$hr(),
				textInput('study', "Prefix for the study:", study)),
				  tags$hr(),
				 		radioButtons("filetype", "file type:",choices = c("csv","tsv")),
				 			downloadButton('downloadData', 'Download'),
				 			helpText("Download list by clicking Download button above",style = "color:grey"),
				 			helpText("Find downloaded list in your Downloader folder",style = "color:blue")

				 						 		),
				  mainPanel(
					h3("Created list of a random alpha-numeric strings",style = "color:blue"),
					tableOutput("qrcodes")
					)
				)
			)

# Define server code ------------------------

server <- function (input, output){
	output$qrcodes <- renderTable({
		if(n>0){
			qrcodes <- c(1:input$n)
			for (i in 1:input$n){
				qrcodes[i] <- paste0(c(tolower(substr(input$study,1,3)),sample(letters,3), sample(c(0:9),2),sample(letters,2),sample(c(1:9),2)),collapse="",sep="")
				
				# 'Uncheck to obtain the QR images ------------------------
				#fnam <- paste(qrcodes[i],".png",sep="")
				#png(file=fnam)
				#qrcode_gen(qrcodes[i],dataOutput=FALSE,plotQRcode=TRUE)
				#dev.off()
				#Or use image
				#png(file=fnam)
				#test <- qrcode_gen(qrcodes[i],dataOutput=TRUE,plotQRcode=TRUE)
				#image(test,col=c("white","grey"),useRaster =TRUE, axes = FALSE)
				#dev.off()
				}
				}
			qrcodes<-cbind(1:length(qrcodes),matrix(qrcodes,ncol=1))
			colnames(qrcodes)<-c("QR_No.","QR_String")
			qrcodes <- as.data.frame(qrcodes)
			
			output$downloadData<-downloadHandler(filename = function (){
				paste(c(study,"_Randomly alpha numeric strings", input$filetype, sep=","))},
				#paste0(c(study,".csv"))},

				content = function (file){
					sep <- switch(input$filetype ,"tsv" = "\t","csv"=",")
					#write to a file
					write.table(qrcodes, file , sep=sep, row.names = FALSE)
					}
					)
				qrcodes
				}
				)
	}

shinyApp(ui = ui, server = server)	

