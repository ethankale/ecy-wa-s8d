
##### Box Plots by land use: #############################################################################

xlimits <- c(0.5,4.5)
ylimits <-  c(min(ParamData$storm_area_loads)/2, max(ParamData$storm_area_loads)*2)
BoxData <- ParamData
lu      <- c("IND","COM","HDR","LDR")
BoxData$Type <- factor(BoxData$Type, levels = lu)

ndMin   <- c()
ndMax   <- c()
numRows <- c()
detCounts <- c()

j = 1

# Clean the data before plotting
for (use in lu) {
  data     <- BoxData[which(BoxData$Type == use),] 
  detcount <- sum(data$nonDetect_Flag == FALSE)
  nd       <- data[which(data$nonDetect_Flag == TRUE),]
  
  ndMax[j]    <- ifelse(length(nd$sample_area_loads) > 0, max(nd$sample_area_loads), 0)
  ndMin[j]    <- ifelse(length(nd$sample_area_loads) > 0, min(nd$sample_area_loads), -1)
  numRows[j]  <- nrow(nd)
  detCounts[j]<- detcount
  
  # Remove land uses with few detections from plotting
  if (detcount < 5 && any(BoxData$Type == use)) {
    BoxData <- BoxData[-which(BoxData$Type == use),]
  }
  
  j <- j+1
}

nonDetects <- BoxData[which(BoxData$nonDetect_Flag == TRUE),]
colorList <- c("blue", "blue", "blue", "blue")

# Insufficient data plot
if (nrow(BoxData) == 0 ) {
  plot(x=c(1:10), y=c(1:10), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  text(x=5,y=5,"Not Plotted\n(No land use has 5 or more detections)")

# Full boxplot
} else if (nrow(nonDetects) > 0 ) {
  boxplot( sample_area_loads ~ Type, 
           data   = BoxData, 
           xaxt   = "n", 
           border = colorList, 
           log    = "y", 
           xlim   = xlimits, 
           ylim   = ylimits, 
           las    = 2,
           cex.axis=0.8,
           xaxs   = "i", 
           yaxs   = "i", 
           main   = ""
  )
  
  # Plot the maximum non-detect for each land use (gray rectangles)
  for (k in 1:length(ndMax)) {
    if (ndMax[k] >= 0) {
      rect(xleft   = -0.5+k, 
           xright  = 0.5+k, 
           ybottom = ylimits[1], 
           ytop    = ndMax[k], 
           col     = "gray93", 
           border  = "white"
      )
    }
  }

  par(new=TRUE)
  
  # Now plot over the previous boxplot.  Not sure why.
  boxplot(sample_area_loads ~ Type, 
           data  = BoxData, 
           xaxt  = "n", 
           border= colorList, 
           lty   = "dashed", 
           log   = "y", 
           xlim  = xlimits, 
           ylim  = ylimits, 
           ylab  = ParamList[i], 
           las   = 2, 
           cex.axis=0.8, 
           xaxs  = "i", 
           yaxs  = "i", 
           main  = "\nBoxplot of Area Loads\nby Land Use (kg/ha)"
  )

  # Plot the non-detect limits
  for (k in 1:length(ndMin)) {
    if (ndMin[k] <= -1) {
      
      # Minimum limit
      lines(x   = c(-0.5+k, 0.5+k), 
            y   = c(ndMin[k], ndMin[k]), 
            lwd = 1, 
            col = "red", 
            lty = "solid"
      )
      
      # Maximum limit
      lines(x   = c(-0.5+k, 0.5+k), 
            y   = c(ndMax[k], ndMax[k]), 
            lwd = 1, 
            col = "red", 
            lty = "dashed"
      )
    }
  }
  
} else {
  boxplot( sample_area_loads ~ Type, 
           data = BoxData, 
           xaxt = "n", 
           border=colorList, 
           log  = "y", 
           xlim = xlimits, 
           ylim = ylimits, 
           ylab = ParamList[i], 
           las  = 2, 
           cex.axis=0.8, 
           xaxs = "i", 
           yaxs = "i", 
           main = "\nBoxplot\nby Land Use", 
           cex.axis=0.8)
}

### Axis labels and legend:
if (nrow(BoxData) > 0 ) {
  percentCensor <- numRows 
  percentCensor <- percentCensor / ( numRows + detCounts )
  percentCensor <- round(100*percentCensor, 1)

  axislabels <- paste(c("Ind", "Com", "HRes", "LRes"), 
                      rep("\nDet=",4), detCounts,
                      rep("\n ND=",4), numRows, 
                      rep("\n",4),     percentCensor,
                      rep("%",4),
                      sep="") 
  axis(side=1, at=c(1:4), labels=rep("",4))
  mtext(side=1, line=3.5, at=c(1:4), text=axislabels, adj=0.5, padj=0, cex=0.6)

  ymax <- par("usr")[4]
  ymin <- par("usr")[3]
  ymin_legend <- 10^(ymax + 0.01*(ymax-ymin))
  ymax_legend <- 10^(ymax + 0.18*(ymax-ymin))

  legend(x   = c(3.5,5), 
         y   = c(ymin_legend, ymax_legend), legend=c("max-ND", "min-ND", "ND Region"),
         lty = c("dashed", "solid", NA),
         border = rep(NA,3),
         col = c("red", "red", NA), 
         fill= c(NA, NA, NA), 
         xpd = NA, 
         cex = 0.8, 
         bty = "o", 
         bg = "white"
  )
  rect(xleft=3.7, xright=4.05, ybottom=10^(ymax + 0.02*(ymax-ymin)), ytop=10^(ymax + 0.07*(ymax-ymin)), col="gray93", border=NA, xpd=NA)
}
