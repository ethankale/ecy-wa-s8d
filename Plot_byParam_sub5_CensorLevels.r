

##### Non-Detect Levels by Permittee ########################################################################

#color_list <- c("#999999", "#56B4E9", "brown", "#009E73", "#F0E442", "#D55E00", "#CC79A7", "#000000", "#0072B2","red", "dark green", )
color_list<-c("#000000","#00FF7F", "#7FFFD4","#458B00","#CD661D","#6495ED","#00FFFF","#00008B","#FFB90F","#CAFF70","#8A2BE2","#BF3EFF","#BEBEBE","#1E90FF","#FF0000")
#color_list<-rainbow(15)
#color_list<-c(24,7,8,16,20,26,30,31,33,81,54,204,244,47,56)

Param_nonDetects <- ParamData[which(ParamData$nonDetect_Flag == TRUE), ]

if(nrow(Param_nonDetects) > 0) {
  plot(x=c(1:10), y=c(1:10), xlim=c(0,5), ylim=ylimits, ylab=ParamList[i], type="n", xaxt="n", xlab="", log="y", xaxs="i", 
      yaxs="i", main="\nNon-Detect Levels", las=2, cex.axis=0.8)
  ### abline(v=c(1:4))
  ### axis(side=1, at=c(0.5, 1.5, 2.5, 3.5, 4.5), labels=c("CLK", "CoS", "PoS", "SNO", "TAC"))

  if (nrow(Param_nonDetects) > 2 ) {
    xValues <- seq(from=0.2, to=4.8, length.out=nrow(Param_nonDetects)) 
  } else if (nrow(Param_nonDetects) == 2) {
    xValues <- c(2, 4)  
  } else {
    xValues <- c(2.5)
  }
   
  plot_order <- order(Param_nonDetects$new_Result_Value)
  segments(x0=xValues, x1=xValues, y0=rep(min(ylimits), nrow(Param_nonDetects)), 
           y1=Param_nonDetects$new_Result_Value[plot_order],
       col=color_list[Param_nonDetects$LabColor[plot_order]])

  ### split_nonDetects <- split(Param_nonDetects, f=Param_nonDetects$Permittee)
  ### for (iPermittee in c(1:length(split_nonDetects))) {
  ###   if (nrow(split_nonDetects[[iPermittee]]) > 0) {
  ###     Permittee_Data <- split_nonDetects[[iPermittee]]
  ###     if (nrow(Permittee_Data) > 2) {
  ###       xValues <- (iPermittee-1) + seq(from=0.05, to=0.95, length.out=nrow(Permittee_Data))
  ###     } else {
  ###      if (nrow(Permittee_Data) == 2) {
  ###        xValues <- (iPermittee-1) + c(0.3, 0.7)
  ###      } else {
  ###        xValues <- (iPermittee-1) + c(0.5)
  ###      }
  ###     }
  ###     plot_order <- order(Permittee_Data$new_Result_Reported_Value)
  ###     segments(x0=xValues, x1=xValues, y0=rep(min(ylimits),nrow(Permittee_Data)), 
  ### y1=Permittee_Dat$new_Result_Reported_Value[plot_order],
  ###         col=color_list[Permittee_Data$LabColor[plot_order]])


  ###   } #endif

  ### } #end for


  box()
  par(new=TRUE)

  plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")

  ### Add legend to identify lab colors:
  xLeft_val <- seq(1.5,9,by=0.5)
  xRight_val <- xLeft_val + 0.5
  ybot <- rep(-1.7,length(xLeft_val))
  ytop <- ybot + 0.5

  rect(xleft=xLeft_val, xright=xRight_val, ybottom=ybot, ytop=ytop, col=color_list, border=NA, xpd=NA)
  text(x=xLeft_val, y=ytop+0.2, labels=Lab.Abbv, xpd=NA, srt=45, cex=0.6, pos=4)

} else {
    plot(1:10, type="n", xaxt="n", yaxt="n", xlab="", ylab="", bty="n")
    text(x=5,y=5,"Not Plotted\n(No Non-Detects)")
} #endif

