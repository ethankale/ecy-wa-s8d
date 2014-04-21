## require(NADA)

setwd("X:\\Hobbs_Will\\Final 2007-2012 Stormwater Data\\R\\")					##tells R what folders to look in

Storm <- read.csv("FinalMasterFile_4-15-2014.csv") 
           
### Make a few adjustments to the Storm dataframe:
Storm <- Storm[-which(Storm$nonDetect_Flag=="WARNING"),]                ### Remove REJ and B data flagged as 'WARNING'
Storm <- Storm[-which(Storm$paramStatus=="Omit"),]                      ### Remove the parameters that are designated 'Omit' for analysis and plotting
Storm <- Storm[-which(substr(Storm$Parameter,1,8) == "Particle"),]

### Create a list of parameter names to group data for plotting/analysis:
Parameter.string <- paste(Storm$Parameter, " ", tolower(Storm$Sample_Matrix), " ", 
          tolower(Storm$new_Fraction_Analyzed), " (", Storm$new_Result_Units, ")", sep="")          
Storm <- cbind(Storm, Parameter.string)


## Only test those chemicals with 10 or more detections
Parameter.List <- table(Storm[,c("Parameter.string", "nonDetect_Flag")])
Parameter.List <- Parameter.List[which(Parameter.List[,1] >= 10),]
#total.samples<-Parameter.List[,1]+Parameter.List[,2]                             ##pulling out those samples with >90% detection
#Parameter.List <- Parameter.List[which(Parameter.List[,1]/total.samples >= 0.9),]
Parameter.List <- rownames(Parameter.List)

Storm_Detects <- Storm[which(Storm$Parameter.string %in% Parameter.List), ]
Storm_Detects <- droplevels(Storm_Detects[which(Storm_Detects$nonDetect_Flag == FALSE), ])
Storm_qualifiers<-table(Storm_Detects[,c("Parameter.string", "Result_Data_Qualifier")])
write.csv(Storm_qualifiers,"Parameter Data Qualifier summary.csv")

shapiro_normal_test <- unlist(tapply(X=Storm_Detects$new_Result_Value, INDEX=Storm_Detects$Parameter.string, FUN=shapiro.test))
write.csv(shapiro_normal_test, "Shapiro_Normal_Test.csv")

shapiro_Log_normal_test <- unlist(tapply(X=log(Storm_Detects$new_Result_Value), INDEX=Storm_Detects$Parameter.string, FUN=shapiro.test))
write.csv(shapiro_Log_normal_test, "Shapiro_LOG_Normal_Test.csv")

##pdf("QQplots.pdf", height=5, width=10)					##I blocked the QQplot development (don't want it to re-write any plots at this point_)
##for (iParam in Parameter.List) {
  ##layout(matrix(1:2, nrow=1, ncol=2))
  ##testdata <- Storm_Detects[which(Storm_Detects$Parameter.string == iParam),]
  ##qqnorm(testdata$new_Result_Reported_Value, main=iParam)
  ##qqline(testdata$new_Result_Reported_Value)
  ##qqnorm(log(testdata$new_Result_Reported_Value), main=paste("Log(", iParam, ")", sep=""))
  ##qqline(log(testdata$new_Result_Reported_Value))

##}
##dev.off()
