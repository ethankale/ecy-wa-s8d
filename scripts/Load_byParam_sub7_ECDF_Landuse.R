####Peto-Prentice EDF curves from Plot_byParam_subAB_EDF.r
###cumulative distribution functions for each land use to visualize significant difference in loads

##### Plot K-M Curves: ###############################################################################################
storm_load <- storm_load[-which(is.na(storm_load$sample_area_loads) | is.na(storm_load$storm_area_loads)), ]          
ParamList <- as.vector(sort(unique(storm_load$Parameter_string)))

#i <- 2

for (i in 1:length(ParamList)) {
  
  ParamData  <- storm_load[which(storm_load$Parameter_string == ParamList[i]), ]
  ParamData<-ParamData[!is.na(ParamData$sample_area_loads),]
  ylimits <-  c(min(ParamData$sample_area_loads)/2, max(ParamData$sample_area_loads)*2)
  
  K_M <- cenfit(obs=ParamData$sample_area_loads, censored=ParamData$nonDetect_Flag, groups=ParamData$Type)
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
  plot(x=1:10, y=1:10, 
       xlim = c(0,1), 
       ylim = ylimits, 
       type = "n", 
       xlab = "", 
       ylab = ParamList[i], 
       log  = "y", 
       xaxs = "i", 
       yaxs = "i", 
       main = "\nEmpirical Distribution \nFunction (EDF)", 
       las  = 2, 
       cex.axis=0.8, 
       xaxt = "n"
  )
  
  axis(side=1, 
       at=c(0, 0.2, 0.4, 0.6, 0.8, 1), 
       labels=c(0, 20, 40, 60, 80, 100)
  )
  mtext(text="Percent Below", 
        side=1, 
        at=0.5, 
        line=2, 
        cex=0.8
  )
  
  # set up parameters for the lines representing each land use
  landuseLines <- data.frame(
    lu  = c("IND", "COM", "HDR", "LDR"),
    col = c("orange", "black", "blue", "deeppink"),
    lty = c("dashed", "solid", "dotdash", "dotted")
  )
  
  # Plot each land use line individually
  for (j in 1:nrow(landuseLines)) {
    
    data <- which(as.character(plot_data$LandUse) == as.character(landuseLines[j,1]))
    lines(x = plot_data[data, "prob"],
          y = plot_data[data, "obs"],
          type = "s",
          lty  = as.character(landuseLines$lty[j]),
          col  = as.character(landuseLines$col[j]),
          lwd  = 1
    )
  }
  
  # Labels & legend
  ymax <- par("usr")[4]
  ymin <- par("usr")[3]
  ymin_legend <- 10^(ymax + 0.01*(ymax-ymin))
  ymax_legend <- 10^(ymax + 0.225*(ymax-ymin))
  
  legend(x = c(0.8,1.1), 
         y = c(ymin_legend, ymax_legend), 
         legend = c("Ind", "Com", "HRes", "LRes"),
         lty = c("dashed", "solid", "dotdash", "dotted"), 
         col = c("orange", "black", "blue", "deeppink"), 
         xpd = NA, 
         cex = 0.8, 
         bty = "o", 
         bg  = "white"
  )
} else {
  plot(x = c(1:10), 
       y = c(1:10), 
       type = "n", 
       xaxt = "n", 
       yaxt = "n", 
       xlab = "", 
       ylab = "", 
       bty  = "n"
  )
  
  text(x = 5, y=5, "Not Plotted\n(Too few detects)")
  
  }
}  #end for i

dev.off()

######Seasonal boxplots#######
storm_load$WetSeason[which(storm_load$WetSeason=="TRUE")]<-"wet"
storm_load$WetSeason[which(storm_load$WetSeason=="FALSE")]<-"dry"
ParamList <- as.vector(sort(unique(storm_load$Parameter_string)))

pdf(paste(outputDirectory,"Load seasonal boxplots.pdf",sep="/"))
#i<-61
for (i in 1:length(ParamList)) {
  
  ParamData  <- storm_load[which(storm_load$Parameter_string == ParamList[i]), ]
  ParamData<-ParamData[!is.na(ParamData$sample_area_loads),]
  
cenboxplot(ParamData$sample_area_loads, ParamData$nonDetect_Flag,ParamData$WetSeason,boxwex=0.5,ylab=ParamList[i])

}
dev.off()