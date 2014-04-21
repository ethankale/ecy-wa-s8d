####EDITS MADE: Brandi deleted following text from title of plot \n(axes switched)#############

require(NADA)

##### Plot K-M Curves: ###############################################################################################
K_M <- cenfit(obs=ParamData$new_Result_Value, censored=ParamData$nonDetect_Flag)

KM_data <- summary(K_M)

if (ncol(KM_data) > 3) {
  plot(x=1:10, y=1:10, xlim=c(0,1), ylim=ylimits, type="n", xlab="", ylab=ParamList[i], log="y", 
       xaxs="i", yaxs="i", main="\nEmpirical Distribution \nFunction (EDF)", las=2, cex.axis=0.8, xaxt="n")
  axis(side=1, at=c(0, 0.2, 0.4, 0.6, 0.8, 1), labels=c(0, 20, 40, 60, 80, 100))
  mtext(text="Percent Below", side=1, at=0.5, line=2, cex=0.8)

  ##abline(v=0.5, col="lightgreen")
  lines(x=KM_data[, "prob"], y=KM_data[, "obs"], type="s", lty="solid", col="black", lwd=1)  

  ymax <- par("usr")[4]
  ymin <- par("usr")[3]
  ymin_legend <- 10^(ymax + 0.01*(ymax-ymin))
  ymax_legend <- 10^(ymax + 0.12*(ymax-ymin))

  legend(x=c(0.8,1.1), y=c(ymin_legend, ymax_legend), legend=c("All Data"),
            lty=c("solid"), 
            col=c("black"), xpd=NA, cex=0.8, bty="o", bg="white")
} else {
  plot(x=c(1:10), y=c(1:10), type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
  text(x=5,y=5,"Not Plotted\n(Too few detects)")

}

