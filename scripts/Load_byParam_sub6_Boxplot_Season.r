

##### Box Plots by season: 
xlimits <- c(0.5,4.5)

BoxData <- ParamData

DetCount_Winter <- sum(BoxData[which(BoxData$Season == 1),]$nonDetect_Flag == FALSE)
DetCount_Spring <- sum(BoxData[which(BoxData$Season == 2),]$nonDetect_Flag == FALSE)
DetCount_Summer <- sum(BoxData[which(BoxData$Season == 3),]$nonDetect_Flag == FALSE)
DetCount_Fall <- sum(BoxData[which(BoxData$Season == 4),]$nonDetect_Flag == FALSE)

Data_Winter <- BoxData[which(BoxData$Season == 1),] 
Data_Spring <- BoxData[which(BoxData$Season == 2),]
Data_Summer <- BoxData[which(BoxData$Season == 3),]
Data_Fall <- BoxData[which(BoxData$Season == 4),]

ND_Winter <- Data_Winter[which(Data_Winter$nonDetect_Flag == TRUE),]
ND_Spring <- Data_Spring[which(Data_Spring$nonDetect_Flag == TRUE),]
ND_Summer <- Data_Summer[which(Data_Summer$nonDetect_Flag == TRUE),]
ND_Fall <- Data_Fall[which(Data_Fall$nonDetect_Flag == TRUE),]


if (DetCount_Winter < 5 && any(BoxData$Season == 1)) {
    BoxData <- BoxData[-which(BoxData$Season == 1),]
}
if (DetCount_Spring < 5 && any(BoxData$Season == 2)) {
    BoxData <- BoxData[-which(BoxData$Season == 2),]
}
if (DetCount_Summer < 5 && any(BoxData$Season == 3)) {
    BoxData <- BoxData[-which(BoxData$Season == 3),]
}
if (DetCount_Fall < 5 && any(BoxData$Season == 4)) {
    BoxData <- BoxData[-which(BoxData$Season == 4),]
}

nonDetects <- BoxData[which(BoxData$nonDetect_Flag == TRUE),]
colorList <- c("blue", "blue", "blue", "blue")

if (nrow(BoxData) == 0 ) {
  plot(x=c(1:10), y=c(1:10), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  text(x=5,y=5,"Not Plotted\n(No season has 5 or more detections)")

} else if (nrow(nonDetects) > 0 ) {
  boxplot( new_Result_Value ~ Season, data= BoxData, xaxt="n", border=colorList, 
           log="y", xlim=xlimits, ylim=ylimits, las=2, cex.axis=0.8, xaxs="i", yaxs="i", main="")

  if (nrow(ND_Winter) > 0 ) {
    max_nonDetect <- max(ND_Winter$new_Result_Value)
    min_nonDetect <- min(ND_Winter$new_Result_Value)
    rect(xleft=0.5, xright=1.5, ybottom=ylimits[1], ytop=max_nonDetect, col="gray93", border="white")
  }
  if (nrow(ND_Spring) > 0 ) {
    max_nonDetect <- max(ND_Spring$new_Result_Value)
    min_nonDetect <- min(ND_Spring$new_Result_Value)
    rect(xleft=1.5, xright=2.5, ybottom=ylimits[1], ytop=max_nonDetect, col="gray93", border="white")
  }
  if (nrow(ND_Summer) > 0 ) {
    max_nonDetect <- max(ND_Summer$new_Result_Value)
    min_nonDetect <- min(ND_Summer$new_Result_Value)
    rect(xleft=2.5, xright=3.5, ybottom=ylimits[1], ytop=max_nonDetect, col="gray93", border="white")
  }
  if (nrow(ND_Fall) > 0 ) {
    max_nonDetect <- max(ND_Fall$new_Result_Value)
    min_nonDetect <- min(ND_Fall$new_Result_Value)
    rect(xleft=3.5, xright=4.5, ybottom=ylimits[1], ytop=max_nonDetect, col="gray93", border="white")
  }

  par(new=TRUE)
  boxplot( new_Result_Value ~ Season, data= BoxData, xaxt="n", border=colorList, 
      lty="dashed", log="y", xlim=xlimits, ylim=ylimits, ylab=ParamList[i], las=2, cex.axis=0.8, xaxs="i", yaxs="i", 
      main="\nBoxplot\nby Season")

  if (nrow(ND_Winter) > 0 ) {
    max_nonDetect <- max(ND_Winter$new_Result_Value)
    min_nonDetect <- min(ND_Winter$new_Result_Value)
    lines(x=c(0.5,1.5), y=c(min_nonDetect, min_nonDetect), lwd=1, col="red", lty="solid")
    lines(x=c(0.5,1.5), y=c(max_nonDetect, max_nonDetect), lwd=1, col="red", lty="dashed")
  }
  if (nrow(ND_Spring) > 0 ) {
    max_nonDetect <- max(ND_Spring$new_Result_Value)
    min_nonDetect <- min(ND_Spring$new_Result_Value)
    lines(x=c(1.5,2.5), y=c(min_nonDetect, min_nonDetect), lwd=1, col="red", lty="solid")
    lines(x=c(1.5,2.5), y=c(max_nonDetect, max_nonDetect), lwd=1, col="red", lty="dashed")
  }
  if (nrow(ND_Summer) > 0 ) {
    max_nonDetect <- max(ND_Summer$new_Result_Value)
    min_nonDetect <- min(ND_Summer$new_Result_Value)
    lines(x=c(2.5,3.5), y=c(min_nonDetect, min_nonDetect), lwd=1, col="red", lty="solid")
    lines(x=c(2.5,3.5), y=c(max_nonDetect, max_nonDetect), lwd=1, col="red", lty="dashed")
  }
  if (nrow(ND_Fall) > 0 ) {
    max_nonDetect <- max(ND_Fall$new_Result_Value)
    min_nonDetect <- min(ND_Fall$new_Result_Value)
    lines(x=c(3.5,4.5), y=c(min_nonDetect, min_nonDetect), lwd=1, col="red", lty="solid")
    lines(x=c(3.5,4.5), y=c(max_nonDetect, max_nonDetect), lwd=1, col="red", lty="dashed")
  }


} else {
  boxplot( new_Result_Value ~ Season, data= BoxData, xaxt="n", border=colorList, 
       log="y", xlim=xlimits, ylim=ylimits, ylab=ParamList[i], las=2, cex.axis=0.8, xaxs="i", yaxs="i", 
       main="\nBoxplot\nby Season", cex.axis=0.8)
}

### Axis labels and legend:
if (nrow(BoxData) > 0 ) {

  percentCensor <- c(nrow(ND_Winter), nrow(ND_Spring), nrow(ND_Summer), nrow(ND_Fall)) 
  percentCensor <- percentCensor / ( 
             c(nrow(ND_Winter), nrow(ND_Spring), nrow(ND_Summer), nrow(ND_Fall)) + 
             c(DetCount_Winter, DetCount_Spring, DetCount_Summer, DetCount_Fall) )
  percentCensor <- round(100*percentCensor, 1)


  axislabels <- paste(c("Winter", "Spring", "Summer", "Fall"), 
                      rep("\nDet=",4), 
                      c(DetCount_Winter, DetCount_Spring, DetCount_Summer, DetCount_Fall),
                      rep("\n ND=",4), 
                      c(nrow(ND_Winter), nrow(ND_Spring), nrow(ND_Summer), nrow(ND_Fall)), 
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