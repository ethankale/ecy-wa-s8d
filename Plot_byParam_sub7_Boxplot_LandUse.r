

##### Box Plots by land use: #############################################################################
xlimits <- c(0.5,4.5)
BoxData <- ParamData
BoxData$Type<-factor(BoxData$Type,levels=c("IND","COM","HDR","LDR"))

DetCount_Indust <- sum(BoxData[which(BoxData$Type == "IND"),]$nonDetect_Flag == FALSE)
DetCount_Commer <- sum(BoxData[which(BoxData$Type == "COM"),]$nonDetect_Flag == FALSE)
DetCount_HighRes <- sum(BoxData[which(BoxData$Type == "HDR"),]$nonDetect_Flag == FALSE)
DetCount_LowRes <- sum(BoxData[which(BoxData$Type == "LDR"),]$nonDetect_Flag == FALSE)

Data_Indust <- BoxData[which(BoxData$Type == "IND"),] 
Data_Commer <- BoxData[which(BoxData$Type == "COM"),]
Data_HighRes <- BoxData[which(BoxData$Type == "HDR"),]
Data_LowRes <- BoxData[which(BoxData$Type == "LDR"),]

ND_Indust <- Data_Indust[which(Data_Indust$nonDetect_Flag == TRUE),]
ND_Commer <- Data_Commer[which(Data_Commer$nonDetect_Flag == TRUE),]
ND_HighRes <- Data_HighRes[which(Data_HighRes$nonDetect_Flag == TRUE),]
ND_LowRes <- Data_LowRes[which(Data_LowRes$nonDetect_Flag == TRUE),]


if (DetCount_Indust < 5 && any(BoxData$Type == "IND")) {
    BoxData <- BoxData[-which(BoxData$Type == "IND"),]
}
if (DetCount_Commer < 5 && any(BoxData$Type == "COM")) {
    BoxData <- BoxData[-which(BoxData$Type == "COM"),]
}
if (DetCount_HighRes < 5 && any(BoxData$Type == "HDR")) {
    BoxData <- BoxData[-which(BoxData$Type == "HDR"),]
}
if (DetCount_LowRes < 5 && any(BoxData$Type == "LDR")) {
    BoxData <- BoxData[-which(BoxData$Type == "LDR"),]
}


nonDetects <- BoxData[which(BoxData$nonDetect_Flag == TRUE),]
colorList <- c("blue", "blue", "blue", "blue")

if (nrow(BoxData) == 0 ) {
  plot(x=c(1:10), y=c(1:10), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  text(x=5,y=5,"Not Plotted\n(No land use has 5 or more detections)")

} else if (nrow(nonDetects) > 0 ) {
  boxplot( new_Result_Value ~ Type, data= BoxData, xaxt="n", border=colorList, 
           log="y", xlim=xlimits, ylim=ylimits, las=2, cex.axis=0.8, xaxs="i", yaxs="i", main="")

  if (nrow(ND_Indust) > 0 ) {
    max_nonDetect <- max(ND_Indust$new_Result_Value)
    min_nonDetect <- min(ND_Indust$new_Result_Value)
    rect(xleft=0.5, xright=1.5, ybottom=ylimits[1], ytop=max_nonDetect, col="gray93", border="white")
  }
  if (nrow(ND_Commer) > 0 ) {
    max_nonDetect <- max(ND_Commer$new_Result_Value)
    min_nonDetect <- min(ND_Commer$new_Result_Value)
    rect(xleft=1.5, xright=2.5, ybottom=ylimits[1], ytop=max_nonDetect, col="gray93", border="white")
  }
  if (nrow(ND_HighRes) > 0 ) {
    max_nonDetect <- max(ND_HighRes$new_Result_Value)
    min_nonDetect <- min(ND_HighRes$new_Result_Value)
    rect(xleft=2.5, xright=3.5, ybottom=ylimits[1], ytop=max_nonDetect, col="gray93", border="white")
  }
  if (nrow(ND_LowRes) > 0 ) {
    max_nonDetect <- max(ND_LowRes$new_Result_Value)
    min_nonDetect <- min(ND_LowRes$new_Result_Value)
    rect(xleft=3.5, xright=4.5, ybottom=ylimits[1], ytop=max_nonDetect, col="gray93", border="white")
  }

  par(new=TRUE)
  boxplot( new_Result_Value ~ Type, data= BoxData, xaxt="n", border=colorList, 
      lty="dashed", log="y", xlim=xlimits, ylim=ylimits, ylab=ParamList[i], las=2, cex.axis=0.8, xaxs="i", yaxs="i", 
      main="\nBoxplot\nby Land Use")

  if (nrow(ND_Indust) > 0 ) {
    max_nonDetect <- max(ND_Indust$new_Result_Value)
    min_nonDetect <- min(ND_Indust$new_Result_Value)
    lines(x=c(0.5,1.5), y=c(min_nonDetect, min_nonDetect), lwd=1, col="red", lty="solid")
    lines(x=c(0.5,1.5), y=c(max_nonDetect, max_nonDetect), lwd=1, col="red", lty="dashed")
  }
  if (nrow(ND_Commer) > 0 ) {
    max_nonDetect <- max(ND_Commer$new_Result_Value)
    min_nonDetect <- min(ND_Commer$new_Result_Value)
    lines(x=c(1.5,2.5), y=c(min_nonDetect, min_nonDetect), lwd=1, col="red", lty="solid")
    lines(x=c(1.5,2.5), y=c(max_nonDetect, max_nonDetect), lwd=1, col="red", lty="dashed")
  }
  if (nrow(ND_HighRes) > 0 ) {
    max_nonDetect <- max(ND_HighRes$new_Result_Value)
    min_nonDetect <- min(ND_HighRes$new_Result_Value)
    lines(x=c(2.5,3.5), y=c(min_nonDetect, min_nonDetect), lwd=1, col="red", lty="solid")
    lines(x=c(2.5,3.5), y=c(max_nonDetect, max_nonDetect), lwd=1, col="red", lty="dashed")
  }
  if (nrow(ND_LowRes) > 0 ) {
    max_nonDetect <- max(ND_LowRes$new_Result_Value)
    min_nonDetect <- min(ND_LowRes$new_Result_Value)
    lines(x=c(3.5,4.5), y=c(min_nonDetect, min_nonDetect), lwd=1, col="red", lty="solid")
    lines(x=c(3.5,4.5), y=c(max_nonDetect, max_nonDetect), lwd=1, col="red", lty="dashed")
  }


} else {
  boxplot( new_Result_Value ~ Type, data= BoxData, xaxt="n", border=colorList, 
       log="y", xlim=xlimits, ylim=ylimits, ylab=ParamList[i], las=2, cex.axis=0.8, xaxs="i", yaxs="i", 
       main="\nBoxplot\nby Land Use", cex.axis=0.8)
}



### Axis labels and legend:
if (nrow(BoxData) > 0 ) {
  percentCensor <- c(nrow(ND_Indust), nrow(ND_Commer), nrow(ND_HighRes), nrow(ND_LowRes)) 
  percentCensor <- percentCensor / ( 
             c(nrow(ND_Indust), nrow(ND_Commer), nrow(ND_HighRes), nrow(ND_LowRes)) + 
             c(DetCount_Indust, DetCount_Commer, DetCount_HighRes, DetCount_LowRes) )
  percentCensor <- round(100*percentCensor, 1)

  axislabels <- paste(c("Ind", "Com", "HRes", "LRes"), 
                      rep("\nDet=",4), 
                      c(DetCount_Indust, DetCount_Commer, DetCount_HighRes, DetCount_LowRes),
                      rep("\n ND=",4), 
                      c(nrow(ND_Indust), nrow(ND_Commer), nrow(ND_HighRes), nrow(ND_LowRes)), 
                      rep("\n",4),
                      percentCensor,
                      rep("%",4),
                      sep="") 
  axis(side=1, at=c(1:4), labels=rep("",4))
  mtext(side=1, line=3.5, at=c(1:4), text=axislabels, adj=0.5, padj=0, cex=0.6)

  ymax <- par("usr")[4]
  ymin <- par("usr")[3]
  ymin_legend <- 10^(ymax + 0.01*(ymax-ymin))
  ymax_legend <- 10^(ymax + 0.18*(ymax-ymin))

  legend(x=c(3.5,5), y=c(ymin_legend, ymax_legend), legend=c("max-ND", "min-ND", "ND Region"),
            lty=c("dashed", "solid", NA), border=rep(NA,3),
            col=c("red", "red", NA), fill=c(NA, NA, NA), xpd=NA, cex=0.8, bty="o", bg="white")
  rect(xleft=3.7, xright=4.05, ybottom=10^(ymax + 0.02*(ymax-ymin)), ytop=10^(ymax + 0.07*(ymax-ymin)), col="gray93", border=NA, xpd=NA)
}

