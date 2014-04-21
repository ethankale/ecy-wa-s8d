

##### Jitter Plots: ######################################################################################################
  xlimits <- c(0, 4)
  plot(x=c(1:10), y=c(1:10), xlim=xlimits, ylim=ylimits, ylab=ParamList[i], type="n", xaxt="n", xlab="", log="y", xaxs="i", 
      yaxs="i", main="\nJitter Plot", las=2, cex.axis=0.8)
  abline(v=c(1:4))
  axis(side=1, at=c(0.5, 1.5, 2.5, 3.5), labels=rep("",4))

  palette(c("violet", "purple", "gray80", "gray60"))
  
  Data_Indust <- ParamData[which(ParamData$useCode == "IND"),] 
  Data_Commer <- ParamData[which(ParamData$useCode == "COM"),]
  Data_HighRes <- ParamData[which(ParamData$useCode == "HDR"),]
  Data_LowRes <- ParamData[which(ParamData$useCode == "LDR"),]

  if (nrow(Data_Indust) > 0) {
    if (nrow(Data_Indust) > 2) {
       jitterX <- sample(seq(from=0.05, to=0.95, length.out=nrow(Data_Indust)))
    } else {
       if (nrow(Data_Indust) == 2) {
         jitterX <- c(0.3, 0.7)
       } else {
         jitterX <- c(0.5)
       }
    }

    nonDetects <- Data_Indust[which(Data_Indust$nonDetect_Flag == 1),]
    if (nrow(nonDetects) > 0 ) {
      jitterX_ND <- jitterX[1:nrow(nonDetects)]
      segments(y0=rep(min(ylimits),nrow(nonDetects)), x0=jitterX_ND, y1=nonDetects$new_Result_Value, 
                x1=jitterX_ND, col=3+nonDetects$WetSeason)
    }

    Detects <- Data_Indust[which(Data_Indust$nonDetect_Flag == 0),]
    if (nrow(Detects) >0 ) {
      jitterX_Det <- jitterX[nrow(nonDetects)+1:nrow(Detects)]
      points(y=Detects$new_Result_Value, x=jitterX_Det, pch=16, cex=1.2, col=1+Detects$WetSeason)
  } }
  
  if (nrow(Data_Commer) > 0) {
    if (nrow(Data_Commer) > 2) {
       jitterX <- sample(seq(from=1.05, to=1.95, length.out=nrow(Data_Commer)))
    } else {
       if (nrow(Data_Commer) == 2) {
         jitterX <- c(1.3, 1.7)
       } else {
         jitterX <- c(1.5)
       }
    }

    nonDetects <- Data_Commer[which(Data_Commer$nonDetect_Flag == 1),]
    if (nrow(nonDetects) > 0 ) {
      jitterX_ND <- jitterX[1:nrow(nonDetects)]
      segments(y0=rep(min(ylimits),nrow(nonDetects)), x0=jitterX_ND, y1=nonDetects$new_Result_Value, 
               x1=jitterX_ND, col=3+nonDetects$WetSeason)
    }

    Detects <- Data_Commer[which(Data_Commer$nonDetect_Flag == 0),]
    if (nrow(Detects) >0 ) {
      jitterX_Det <- jitterX[nrow(nonDetects)+1:nrow(Detects)]
      points(y=Detects$new_Result_Value, x=jitterX_Det, pch=16, cex=1.2, col=1+Detects$WetSeason)
  } }

  if (nrow(Data_HighRes) > 0) {
    if (nrow(Data_HighRes) > 2) {
       jitterX <- sample(seq(from=2.05, to=2.95, length.out=nrow(Data_HighRes)))
    } else {
       if (nrow(Data_HighRes) == 2) {
         jitterX <- c(2.3, 2.7)
       } else {
         jitterX <- c(2.5)
       }
    }

    nonDetects <- Data_HighRes[which(Data_HighRes$nonDetect_Flag == 1),]
    if (nrow(nonDetects) > 0 ) {
      jitterX_ND <- jitterX[1:nrow(nonDetects)]
      segments(y0=rep(min(ylimits),nrow(nonDetects)), x0=jitterX_ND, y1=nonDetects$new_Result_Value, 
                 x1=jitterX_ND, col=3+nonDetects$WetSeason)
    }

    Detects <- Data_HighRes[which(Data_HighRes$nonDetect_Flag == 0),]
    if (nrow(Detects) >0 ) {
      jitterX_Det <- jitterX[nrow(nonDetects)+1:nrow(Detects)]
      points(y=Detects$new_Result_Value, x=jitterX_Det, pch=16, cex=1.2, col=1+Detects$WetSeason)
  } }

  if (nrow(Data_LowRes) > 0) {
    if (nrow(Data_LowRes) > 2) {
       jitterX <- sample(seq(from=3.05, to=3.95, length.out=nrow(Data_LowRes)))
    } else {
       if (nrow(Data_LowRes) == 2) {
         jitterX <- c(3.3, 3.7)
       } else {
         jitterX <- c(3.5)
       }
    }

    nonDetects <- Data_LowRes[which(Data_LowRes$nonDetect_Flag == 1),]
    if (nrow(nonDetects) > 0 ) {
      jitterX_ND <- jitterX[1:nrow(nonDetects)]
      segments(y0=rep(min(ylimits),nrow(nonDetects)), x0=jitterX_ND, y1=nonDetects$new_Result_Value, 
               x1=jitterX_ND, col=3+nonDetects$WetSeason)
    }

    Detects <- Data_LowRes[which(Data_LowRes$nonDetect_Flag == 0),]
    if (nrow(Detects) >0 ) {
      jitterX_Det <- jitterX[nrow(nonDetects)+1:nrow(Detects)]
      points(y=Detects$new_Result_Value, x=jitterX_Det, pch=16, cex=1.2, col=1+Detects$WetSeason)
  } } 

  box()


  DetCount_Indust <- sum(ParamData[which(ParamData$useCode == "IND"),]$nonDetect_Flag == FALSE)
  DetCount_Commer <- sum(ParamData[which(ParamData$useCode == "COM"),]$nonDetect_Flag == FALSE)
  DetCount_HighRes <- sum(ParamData[which(ParamData$useCode == "HDR"),]$nonDetect_Flag == FALSE)
  DetCount_LowRes <- sum(ParamData[which(ParamData$useCode == "LDR"),]$nonDetect_Flag == FALSE)

  ND_Indust <- Data_Indust[which(Data_Indust$nonDetect_Flag == TRUE),]
  ND_Commer <- Data_Commer[which(Data_Commer$nonDetect_Flag == TRUE),]
  ND_HighRes <- Data_HighRes[which(Data_HighRes$nonDetect_Flag == TRUE),]
  ND_LowRes <- Data_LowRes[which(Data_LowRes$nonDetect_Flag == TRUE),]

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
  mtext(side=1, line=3.5, at=-0.5+c(1:4), text=axislabels, adj=0.5, padj=0, cex=0.6)


  ymax <- par("usr")[4]
  ymin <- par("usr")[3]
  ymin_legend <- 10^(ymax + 0.01*(ymax-ymin))
  ymax_legend <- 10^(ymax + 0.22*(ymax-ymin))

  legend(x=c(2.6,4.3), y=c(ymin_legend, ymax_legend), 
            legend=c("Detect-DrySeas", "Detect-WetSeas", "NonDetect-DrySeas", "NonDetect-WetSeas"),
            lty=c("blank", "blank", "solid", "solid"), pch=c(16,16,NA, NA), pt.cex=c(1.2,1.2,1.2,1.2), 
            col=c("violet", "purple", "gray80", "gray60"), xpd=NA, cex=0.8, bty="o", bg="white")



