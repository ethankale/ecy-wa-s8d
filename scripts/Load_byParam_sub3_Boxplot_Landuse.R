###look-up the corresponding storm and sample event volume and attach to the 'storm' data file
##"Y:\\SHARED Files\\Brandi L\\Final 2007-2012 Stormwater Data\\R\\storm_flow_lookup_4-14-2014.r"



BoxData$Type<-factor(BoxData$Type,levels=c("IND","COM","HDR","LDR"))


Parameter.string <- paste(storm$Parameter, " ", tolower(storm$Sample_Matrix), " ", 
          tolower(storm$new_Fraction_Analyzed), " (", storm$new_Result_Units, ")", sep="")
storm <- droplevels(cbind(storm, Parameter.string))
tot_cu<-storm[which(storm$Parameter.string=="Copper water dissolved (ug/L)"),]
boxplot(sample_loads~Season, data=tot_cu, log="y")
boxplot(sample_loads~Type, data=tot_cu, log="y")