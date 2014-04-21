####EDITS MADE: Brandi deleted following text from title of plot n(axes switched)#############

require(NADA)

##### Plot K-M Curves: ###############################################################################################
K_M <- cenfit(obs=ParamData$new_Result_Value, censored=ParamData$nonDetect_Flag, groups=ParamData$Type)

  KM_data <- summary(K_M)

  for (iUse in c(1:length(KM_data))) {
     if (iUse == 1) {
       LandUse <- strsplit(names(KM_data[1]),"=")[[1]][2]
       plot_data_old <- cbind( KM_data[[1]], LandUse)
     } else {
       LandUse <- strsplit(names(KM_data[iUse]),"=")[[1]][2]
       plot_data_new <- cbind( KM_data[[iUse]], LandUse)
       plot_data <- rbind(plot_data_old, plot_data_new)
       plot_data_old <- plot_data
  }}


if (ncol(plot_data) > 3) {
  plot(x=1:10, y=1:10, xlim=c(0,1), ylim=ylimits, type="n", xlab="", ylab=ParamList[i], log="y", 
       xaxs="i", yaxs="i", main="\nEmpirical Distribution \nFunction (EDF)", las=2, cex.axis=0.8, xaxt="n")
  axis(side=1, at=c(0, 0.2, 0.4, 0.6, 0.8, 1), labels=c(0, 20, 40, 60, 80, 100))
  mtext(text="Percent Below", side=1, at=0.5, line=2, cex=0.8)

  iComm <- which(plot_data$LandUse == "COM")
  iIndus <- which(plot_data$LandUse == "IND")
  iHigh <- which(plot_data$LandUse == "HDR")
  iLow <- which(plot_data$LandUse == "LDR")

  ##abline(v=0.5, col="lightgreen")
  lines(x=plot_data[iIndus, "prob"], y=plot_data[iIndus, "obs"], type="s", lty="dashed", col="orange", lwd=1)  
  lines(x=plot_data[iComm, "prob"], y=plot_data[iComm, "obs"], type="s", lty="solid", col="black", lwd=1)  
  lines(x=plot_data[iHigh, "prob"], y=plot_data[iHigh, "obs"], type="s", lty="dotdash", col="blue", lwd=1)  
  lines(x=plot_data[iLow, "prob"], y=plot_data[iLow, "obs"], type="s", lty="dotted", col="deeppink", lwd=1)  

  ymax <- par("usr")[4]
  ymin <- par("usr")[3]
  ymin_legend <- 10^(ymax + 0.01*(ymax-ymin))
  ymax_legend <- 10^(ymax + 0.225*(ymax-ymin))

  legend(x=c(0.8,1.1), y=c(ymin_legend, ymax_legend), legend=c("Ind", "Com", "HRes", "LRes"),
            lty=c("dashed", "solid", "dotdash", "dotted"), 
            col=c("orange", "black", "blue", "deeppink"), xpd=NA, cex=0.8, bty="o", bg="white")
} else {
  plot(x=c(1:10), y=c(1:10), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  text(x=5,y=5,"Not Plotted\n(Too few detects)")

}

