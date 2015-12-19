#####
# Initial stantistical analysis of data, including:
#  - chi-squared of each unique parameter/matrix/unit combination by land use type
#  - creation of a Case.list table with summary statistics and grading (A,B,C) of each unique combination of parameter/matrix/unit

GroupList <- unique(Storm[,c("Parameter.string", "paramGroup", "Parameter", "Sample_Matrix", "new_Fraction_Analyzed", "new_Result_Units")])
ParamList <- as.vector(sort(unique(Storm$Parameter.string)))

### Store Detection counts in a matrix for export.
Case.list <- data.frame(Parameter=character(),
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
                 PPCC_test=character(),
                 wilcoxon_pvalue=numeric())




sink(paste(outputDirectory, "PetoPrentice.txt", sep="/"))
#sink(paste(outputDirectory, "PetoPrentice_landuse.txt", sep=""))   ###use for parameter + landuse summary
#sink(paste(outputDirectory, "PetoPrentice_season.txt", sep=""))   ###use for parameter + season summary

for (i in c(1:length(ParamList))) {
###for (i in c(120:133)) {
  ParamData  <- Storm[which(Storm$Parameter.string == ParamList[i]), ]
  Detects    <- ParamData$new_Result_Value[which(ParamData$nonDetect_Flag == FALSE) ]
  nonDetects <- ParamData$new_Result_Value[which(ParamData$nonDetect_Flag == TRUE) ]

  # Count & categorize detects & non-detects
  num.nonDetects <- length(nonDetects)
  if (num.nonDetects == 0) {
    min.nonDetects <- NA
    max.nonDetects <- NA
  } else {
    min.nonDetects <- min(nonDetects)
    max.nonDetects <- max(nonDetects)
  }

  num.Detects <- length(Detects)
  if (num.Detects == 0) {
    min.Detects <- NA
    max.Detects <- NA
  } else {
    min.Detects <- min(Detects)
    max.Detects <- max(Detects)
  }

  # Calculate percent of censored samples for each group, and therefore statistical "grade" of the group (A,B, or C)
  nSamples  <- c(num.nonDetects + num.Detects)
  pctCensor <- round(100 * num.nonDetects / nSamples, 1)
  if (pctCensor <= 50) {
    case.code <- "A"
  } else if (pctCensor <= 80) {
    case.code <- "B"
  } else {
    case.code <- "C"  
  }

  # Print out each unique group, calculating statistical details if the group is of grade "A"
  cat(paste("Row:", i, "        Parameter=", ParamList[i], sep=""))
  cat("\n")

  if (case.code == "A") {
    
    KM.fit <- cenfit(ParamData$new_Result_Value, ParamData$nonDetect_Flag)
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
    PetoPrentice <- cendiff(ParamData$new_Result_Value, ParamData$nonDetect_Flag, groups=ParamData$Type)
    
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

  ROS_correlation <- NA
  PPCC_test <- NA
  
   if (case.code == "B" & nSamples>50) {
    ROS.fit <- ros(ParamData$new_Result_Value, ParamData$nonDetect_Flag)
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
  Wilcoxon_season<-cendiff(ParamData$new_Result_Value, ParamData$nonDetect_Flag,as.factor(ParamData$WetSeason))
     wilcoxon_pvalue<-pchisq(Wilcoxon_season$chisq,1,lower.tail=FALSE)
  } else {
    wilcoxon_pvalue<- NA
  }
  
  Case.list <- rbind(Case.list, data.frame(ParamList[i], case.code, pctCensor, nSamples, num.Detects, 
                      num.nonDetects, min.Detects, max.Detects, min.nonDetects, max.nonDetects, 
                      KM.mean, KM.mean.SE, KM.mean.95LCL, KM.mean.95UCL, KM.25ile,KM.median, KM.75ile,KM.90ile, 
                      KM.SD, ROS.mean,ROS.25ile,ROS.median,ROS.75ile,ROS.90ile,ROS.SD,ROS_correlation, 
                      PetoPrentice.pvalue, PetoPrentice.chisq,PetoPrentice.df, PPCC_test,wilcoxon_pvalue))

}
sink()

print(table(Case.list$case.code))

