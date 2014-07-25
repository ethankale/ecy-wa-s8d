###THIS IS A COPY OF THE SCRIPT FOR CALCULATING DATA SUMMARIES FROM CONCENTRATIONS-------------------------
###All data qualifiers from the concentrations are carried forward to the load value###
###First remove the entries which don't have a storm or sample event associated with them (mainly grab samples)

###SHOULD BE RUN AFTER LINES 10-32 IN Load_byParam_sub2_AssignCase.r######


#storm_load <- storm_load[-which(is.na(storm_load$sample_loads) | is.na(storm_load$storm_loads)), ]          
GroupList <- unique(storm_load[,c("Parameter_string", "paramGroup", "Parameter", "new_Fraction_Analyzed", "sample_loads")])
ParamList <- as.vector(sort(unique(storm_load$Parameter_string)))

sink(paste(outputDirectory, "massLoadSummary.txt", sep="/"))

### Store Detection counts in a matrix for export.
Case.list.mass <- data.frame(Parameter=character(),
                        CaseCode=integer(), 
                        pctCensor=numeric(), 
                        nSamples=integer(),
                        num.Detects=integer(),
                        num.nonDetects=integer(),
                        min.Detect=numeric(),
                        max.Detect=numeric(),
                        min.nonDetect=numeric(),
                        max.nonDetect=numeric(),
                        KM.mean=numeric(),
                        KM.mean.SE=numeric(),
                        KM.mean.95LCL=numeric(),
                        KM.mean.95UCL=numeric(),
                        KM.25ile=numeric(),
                        KM.median=numeric(),
                        KM.75ile=numeric(),
                        KM.90ile=numeric(),
                        KM.SD=numeric(),
                        ROS.mean=numeric(),
                        ROS.25ile=numeric(),
                        ROS.median=numeric(),
                        ROS.75ile=numeric(),
                        ROS.90ile=numeric(),
                        ROS.SD=numeric(),
                        ROS_correlation=numeric(),
                        PetoPrentice.pvalue=numeric(),
                        PetoPrentice.chisq=numeric(),
                        PetoPrentice.df=numeric(),
                        wilcoxon_pvalue=numeric())


for (i in c(1:length(ParamList))) {
  ###for (i in c(120:133)) {
  ParamData  <- storm_load[which(storm_load$Parameter_string == ParamList[i]), ]
  Detects    <- ParamData$sample_loads[which(ParamData$nonDetect_Flag == FALSE) ]
  nonDetects <- ParamData$sample_loads[which(ParamData$nonDetect_Flag == TRUE) ]
  
  if (length(nonDetects) == 0) {
    num.nonDetects <- 0
    min.nonDetects <- NA
    max.nonDetects <- NA
  } else {
    num.nonDetects <- length(nonDetects)
    min.nonDetects <- min(nonDetects)
    max.nonDetects <- max(nonDetects)
  }
  
  if (length(Detects) == 0) {
    num.Detects <- 0
    min.Detects <- NA
    max.Detects <- NA
  } else {
    num.Detects <- length(Detects)
    min.Detects <- min(Detects)
    max.Detects <- max(Detects)
  }
  
  nSamples <- c(num.nonDetects + num.Detects)
  pctCensor <- round(100*num.nonDetects / nSamples,1)
  if (pctCensor <= 50) {
    case.code <- "A"
  } else if (pctCensor <= 80) {
    case.code <- "B"
  } else {
    case.code <- "C"  
  }
  
  cat(paste("Row:  ", i, "      Parameter=", ParamList[i], sep=""))
  cat("\n")
  
  if (case.code == "A") {
    KM.fit <- cenfit(ParamData$sample_loads, ParamData$nonDetect_Flag)
    print(KM.fit)
    print(mean(KM.fit))
    print(quantile(KM.fit,c(0.25,0.75,0.9)))
    cat("\n")
    
    KM.mean.data   <- mean(KM.fit)
    KM.quantile    <- quantile(KM.fit,c(0.25,0.75,0.9))
    KM.mean        <- as.vector(KM.mean.data[1])
    KM.mean.SE     <- as.vector(KM.mean.data[2])
    KM.mean.95LCL  <- as.vector(KM.mean.data[3])
    KM.mean.95UCL  <- as.vector(KM.mean.data[4])
    KM.25ile       <- as.vector(KM.quantile[1])
    KM.median      <- as.vector(median(KM.fit))
    KM.75ile       <- as.vector(KM.quantile[2])
    KM.90ile       <- as.vector(KM.quantile[3])
    KM.SD          <- as.vector(sd(KM.fit))
  } else {
    KM.mean       <- NA
    KM.mean.SE    <- NA
    KM.mean.95LCL <- NA
    KM.mean.95UCL <- NA
    KM.25ile      <- NA
    KM.median     <- NA
    KM.75ile      <- NA
    KM.90ile      <- NA
    KM.SD         <- NA
  }
  
  nGroups <- length(unique(ParamData$Type))
  if( case.code == "A" & num.Detects > 9 & nGroups > 2) {
    PetoPrentice <- cendiff(ParamData$sample_loads, ParamData$nonDetect_Flag, groups=ParamData$Type)
    print(PetoPrentice)
    cat("\n\n\n")
    PetoPrentice.chisq <- PetoPrentice$chisq
    PetoPrentice.df <- nGroups-1
    if (PetoPrentice.chisq < 60) {
      PetoPrentice.pvalue <- 1 - pchisq(PetoPrentice$chisq, nGroups-1)
    } else {
      PetoPrentice.pvalue <- -expm1(pchisq(PetoPrentice$chisq, df=PetoPrentice.df, log.p=TRUE))
    }
    
  } else {
    PetoPrentice.chisq <- NA
    PetoPrentice.df <- NA
    PetoPrentice.pvalue <- NA
  }
  
  if (case.code %in% c("A", "B") &  num.Detects >= 5) {
    Param_ROS <- cenros(ParamData$sample_loads, ParamData$nonDetect_Flag)
    ROS_correlation <- sqrt(summary(Param_ROS)$r.squared)
  } else {
    ROS_correlation <- NA
  }
  
  
  if (case.code == "B" & nSamples>50) {
    ROS.fit <- ros(ParamData$sample_loads, ParamData$nonDetect_Flag)
    print(ROS.fit)
    print(quantile(ROS.fit,c(0.25,0.75,0.9)))
    cat("\n")
    ROS.quantile   <- quantile(ROS.fit,c(0.25,0.75,0.9))
    ROS.mean       <- as.vector(mean(ROS.fit))
    ROS.25ile      <- as.vector(ROS.quantile[1])
    ROS.median     <- as.vector(median(ROS.fit))
    ROS.75ile      <- as.vector(ROS.quantile[2])
    ROS.90ile      <- as.vector(ROS.quantile[3])
    ROS.SD         <- as.vector(sd(ROS.fit))
    
  } else {
    ROS.mean     <- NA
    ROS.mean.SE  <- NA
    ROS.25ile    <-NA
    ROS.median   <- NA
    ROS.75ile    <- NA
    ROS.90ile    <- NA
    ROS.SD       <- NA
  }
  
  if (length(unique(ParamData$WetSeason))>1) {
    Wilcoxon_season<-cendiff(ParamData$sample_loads, ParamData$nonDetect_Flag,as.factor(ParamData$WetSeason))
    wilcoxon_pvalue<-pchisq(Wilcoxon_season$chisq,1,lower.tail=FALSE)
  } else {
    wilcoxon_pvalue<- NA
  }
  Case.list.mass <- rbind(Case.list.mass, data.frame(ParamList[i], case.code, pctCensor, nSamples, num.Detects, 
                                           num.nonDetects, min.Detects, max.Detects, min.nonDetects, max.nonDetects, 
                                           KM.mean, KM.mean.SE, KM.mean.95LCL, KM.mean.95UCL, KM.25ile,KM.median, KM.75ile,KM.90ile, 
                                           KM.SD,PetoPrentice.pvalue, PetoPrentice.chisq, PetoPrentice.df,ROS.mean,ROS.25ile,ROS.median,
                                           ROS.75ile,ROS.90ile,ROS.SD,ROS_correlation, wilcoxon_pvalue))
  
}

sink()

#write.csv(Case.list, paste(outputDirectory, "Param_Summary_Mass_Loads.csv", sep="/"))
#write.csv(Case.list, paste(outputDirectory, "Param_Summary_Mass_Loads_Landuse.csv", sep="/"))
#write.csv(Case.list, paste(outputDirectory, "Param_Summary_Mass_Loads_Season.csv", sep="/"))
