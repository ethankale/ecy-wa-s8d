
###----------------------Variables which can be changed ----------------------------------------
### Set up the directory names to read data, script files and send output.  

#dataDirectory <- "X:\\Hobbs_Will\\Final 2007-2012 Stormwater Data\\"
#scriptDirectory <- paste(dataDirectory, "R\\", sep="")
#outputDirectory <- paste(scriptDirectory, "Output\\", sep="")

dataDirectory   <- getwd()
scriptDirectory <- paste(dataDirectory, "scripts", sep="/")
outputDirectory <- paste(dataDirectory, "output", sep="/")

### set up name of the input csv file.
csvFile <- paste(dataDirectory, "FinalMasterFile_4-28-2014.csv", sep="/")

### set a flag to decide whether to generate a pdf file.  If this flag is false, then create emf files instead.
output_types <- c("Screen", "PDF", "EMF")  ### recognized output types, don't change these please
output_selected <- output_types[2]         ### to change the output type, change this number between 1-3

#####
###----------------------Commands to do the plots-------------------------------------------------
### Load USGS Non-detects package.  Requires that the package be installed first.
require(NADA)
require(devEMF)
options(width=200)

### Read the data and summarize - (this will execute the commands in a separate script file).
source( paste(scriptDirectory, "Plot_byParam_sub1_ReadData.r", sep="/"))

### Assign parameters to cases A, B, C and calculate K-M stats, MLE and PetoPrentice
source( paste(scriptDirectory, "Plot_byParam_sub2_AssignCase.r", sep="/"))

#####
### Either open the console window or the pdf file if option selected:

windowHeight <-  7.25
windowWidth  <- 10

if (output_selected == "Screen") {
  windows(height=windowHeight, width=windowWidth, rescale = "fixed")
  par(mgp=c(2.8,0.5,0))
  layout(matrix(c(1:6), 2, 3, byrow=TRUE))
} else if (output_selected == "PDF") {
  pdf(paste(outputDirectory, "Parameter_Plots.pdf", sep="/"), height=windowHeight, width=windowWidth)
  par(mgp=c(2.8,0.5,0))
  layout(matrix(c(1:6), 2, 3, byrow=TRUE))
}

### Set up information for the lab-color-key:
Storm$LabAbbv <- as.vector(abbreviate(Storm$Result_Lab_Name,minlength=4))
Storm$LabAbbv[which(Storm$LabAbbv=="")]                <- "Unk"
Storm$LabAbbv[which(Storm$LabAbbv=="ARI(SW")]          <-"ARI-SW"
Storm$LabAbbv[which(Storm$LabAbbv=="AS&CTW(1sldfci")]  <-"AS&CTW"
Storm$LabAbbv[which(Storm$LabAbbv=="CAS(KW(3")]        <-"CAS-KW"

Storm$LabColor<-as.integer(Storm$Result_Lab_Name)

temp <- unique(Storm[,c("LabAbbv","LabColor")])

Lab.Abbv <- c(1:15)
Lab.Abbv[temp$LabColor] <- as.vector(temp$LabAbbv)
Lab.Abbv <- c("Unk", "Lab-1", "Lab-2", "Lab-3", "Lab-4", "Lab-5", "Lab-6", "Lab-7", "Lab-8","Lab-9","Lab-10","Lab-11","Lab-12","Lab-13","Lab-14","Lab-15")


### Create a list of each parameter to make the plots:
ParamList <- as.vector(sort(unique(Storm$Parameter.string)))

### Create a matrix to store Detection counts for export.
Det_Store <- matrix(data=NA, nrow=length(ParamList), ncol=12)
rownames(Det_Store) <- ParamList
colnames(Det_Store) <- c("#Det_Ind", "#Det_Com", "#Det_HRes", "#Det_LowRes", 
                         "#ND_Ind",  "#ND_Com",  "#ND_HRes",  "#ND_LowRes",  
                          "PctDet_Ind", "PctDet_Com", "PctDet_HRes", "PctDet_LowRes")

##i <- 56  ## dissolved copper Case A example
##i <- 50  ## Chlorpyrifos water Case C example
##i <- 69  ## %gravel
##i <- 5   ## 2-Nitrophenol sediment  (ug/Kg)
for (i in 1:length(ParamList)) {
  # VERY IMPORTANT - the value of "i" refers to the parameter being plotted throughout
  #  this loop.  DO NOT set the value of "i" within any of the subscripts.
  
  if (output_selected == "EMF") {
    emf(paste(outputDirectory, "EMF\\", i, ".emf", sep=""), height=windowHeight, width=windowWidth)
  }

  ParamData <- Storm[which(Storm$Parameter.string == ParamList[i]), ]
  ParamData$new_Result_Value[which(ParamData$new_Result_Value==0)]<- 0.01
  ylimits <-  c(min(ParamData$new_Result_Value)/2, max(ParamData$new_Result_Value)*2)

  # Plot data, with the type of plotting depending on the quality of the data
  if(Case.list$case.code[i] %in% c("A", "B")) {
    
    # Detect & non-detect by land use type plot
    source( paste(scriptDirectory, "Plot_byParam_sub3AB_jitterPlot.r", sep="/"))
    
    # Chance of exceedance plot
    if(Case.list$num.Detects[i] >= 5) {
      source( paste(scriptDirectory, "Plot_byParam_sub4_QQPlot.r", sep="/"))
    } else {
      plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
      text(x=5,y=5,"Not Plotted\n(Fewer than 5 detections)")
    }
    
    # Non-detection limits plot
    source( paste(scriptDirectory, "Plot_byParam_sub5_CensorLevels.r", sep="/"))
    
    # Emperical distribution plots
    if(Case.list$num.Detects[i] >= 5) {
      source( paste(scriptDirectory, "Plot_byParam_sub6AB_EDF.r", sep="/"))
    } else {
      plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
      text(x=5,y=5,"Not Plotted\n(Fewer than 5 detections)")
    }
    
    # Boxplot by land use, with non-detect ranges marked
    if(Case.list$num.Detects[i] >= 5) {
      source( paste(scriptDirectory, "Plot_byParam_sub7_BoxPlot_LandUse.r", sep="/"))
    } else {
      plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
      text(x=5,y=5,"Not Plotted\n(Less than 5 detections)")
    }

    # Boxplot by season, with non-detect ranges marked.
    if(Case.list$num.Detects[i] >= 5) {
      source( paste(scriptDirectory, "Plot_byParam_sub8_BoxPlot_Season.r", sep="/"))
    } else {
      plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
      text(x=5,y=5,"Not Plotted\n(Less than 5 detections)")
    }

  } else {
    source( paste(scriptDirectory, "Plot_byParam_sub3C_jitterPlot.r", sep="/"))
    #plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    #text(x=5,y=5,"Under Construction")
    plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    text(x=5,y=5,"Not Plotted\n(Case C)")
    source( paste(scriptDirectory, "Plot_byParam_sub5_CensorLevels.r", sep="/"))
    if(Case.list$num.Detects[i] >= 5) {
      source( paste(scriptDirectory, "Plot_byParam_sub6C_EDF.r", sep="/"))
    } else {
      plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
      text(x=5,y=5,"Not Plotted\n(Less than 5 detections)")
    }

    plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    text(x=5,y=5,"Not Plotted\n(Case C)")
    plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    text(x=5,y=5,"Not Plotted\n(Case C)")

  }
  
    if (output_selected == "EMF") {
      dev.off()
    }

}  #end for i


### Close the pdf file if that is the output type:
if (output_selected == "PDF") {
  dev.off()
} 

write.csv(Case.list, paste(outputDirectory, "Param_Summary.csv", sep="/"))
# write.csv(Case.list, paste(outputDirectory, "Param_Summary_landuse.csv", sep=""))
# write.csv(Case.list, paste(outputDirectory, "Param_Summary_season.csv", sep=""))


