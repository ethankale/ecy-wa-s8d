### Jitter plots for Case C - not separated by land use.

##### Jitter Plots: ######################################################################################################
  xlimits <- c(0, 4)
  plot(x=c(1:10), y=c(1:10), xlim=xlimits, ylim=ylimits, ylab=ParamList[i], type="n", xaxt="n", xlab="", log="y", xaxs="i", 
      yaxs="i", main="\nJitter Plot", las=2, cex.axis=0.8)

  palette(c("violet", "purple", "gray80", "gray60"))

  if (nrow(ParamData) > 0) {
    if (nrow(ParamData) > 2) {
       jitterX <- sample(seq(from=0.05, to=3.95, length.out=nrow(ParamData)))
    } else {
       if (nrow(ParamData) == 2) {
         jitterX <- c(1.5, 2.5)
       } else {
         jitterX <- c(2)
       }
    }

    nonDetects <- ParamData[which(ParamData$nonDetect_Flag == 1),]
    if (nrow(nonDetects) > 0 ) {
      jitterX_ND <- jitterX[1:nrow(nonDetects)]
      segments(y0=rep(min(ylimits),nrow(nonDetects)), x0=jitterX_ND, y1=nonDetects$new_Result_Value, 
                x1=jitterX_ND, col=3+nonDetects$WetSeason)
    }

    Detects <- ParamData[which(ParamData$nonDetect_Flag == 0),]
    if (nrow(Detects) >0 ) {
      jitterX_Det <- jitterX[nrow(nonDetects)+1:nrow(Detects)]
      points(y=Detects$new_Result_Value, x=jitterX_Det, pch=16, cex=1.2, col=1+Detects$WetSeason)
    }
  }  ## end if nrow(ParamData)
   
  box()

  percentCensor <- 100*nrow(nonDetects)/nrow(ParamData)
  percentCensor <- round(percentCensor, 1)
  axislabels <- paste(c("Randomized X"), 
                      c("\nDet="), 
                      c(nrow(ParamData)-nrow(nonDetects)),
                      c("\n ND="), 
                      c(nrow(nonDetects)), 
                      c("\n"),
                      percentCensor,
                      c("%"),
                      sep="") 
  axis(side=1, at=c(2), labels=c(""))
  mtext(side=1, line=3.5, at=2, text=axislabels, adj=0.5, padj=0, cex=0.6)



  ymax <- par("usr")[4]
  ymin <- par("usr")[3]
  ymin_legend <- 10^(ymax + 0.01*(ymax-ymin))
  ymax_legend <- 10^(ymax + 0.22*(ymax-ymin))

  legend(x=c(2.6,4.3), y=c(ymin_legend, ymax_legend), 
            legend=c("Detect-DrySeas", "Detect-WetSeas", "NonDetect-DrySeas", "NonDetect-WetSeas"),
            lty=c("blank", "blank", "solid", "solid"), pch=c(16,16,NA, NA), pt.cex=c(1.2,1.2,1.2,1.2), 
            col=c("violet", "purple", "gray80", "gray60"), xpd=NA, cex=0.8, bty="o", bg="white")



