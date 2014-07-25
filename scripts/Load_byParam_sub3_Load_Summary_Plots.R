####Script for creating plot output to summarize contaminant loads by mass and area###

###IMPORTANT###
###SHOULD BE RUN AFTER LINES 10-32 IN Load_byParam_sub2_AssignCase.r######

require(NADA)

##### Here through line 34 replicated from LoadL_byParam_sub2_AssignCase.R.

storm_load <- Storm[-which(Storm$paramClass=="Measurement"),]                ### remove all the flow data and conventional parameters
storm_load <- storm_load[-which(storm_load$Sample_Matrix=="Sediment"),]      ### remove sediment samples
storm_load <- storm_load[which(storm_load$new_Result_Units=="ug/L"),]        ### ensure all remaining samples are in appropriate units

storm_load$sample.year<-as.numeric(format(storm_load$Field_Collection_End_Date,"%Y"))

### calculate sample event loads -------------------------
# Unit conversion note - sample and storm volumes are in m3.  Desired load units are Kg.
#  All samples should be in ug/L.  Multiply ug/L by 1000 to get ug/m3.  Then divide
#  the resulting load by 1e9 (1,000,000,000) to convert ug to Kg.

storm_load$sample_loads <- storm_load$sample_event_flow_volume*(storm_load$new_Result_Value*1e-06)
storm_load$storm_loads  <- storm_load$storm_event_flow_volume*(storm_load$new_Result_Value*1e-06)
storm_load$load_units   <- "Kg"

### Unit area loads -------------------------
# Convert to kg per hectare; otherwise simple multiplication.
storm_load$storm_area_loads  <- storm_load$storm_loads  / (storm_load$Acres * 2.47105)
storm_load$sample_area_loads <- storm_load$sample_loads / (storm_load$Acres * 2.47105)
storm_load$area_load_units   <- "Kg/hectare"

# Update Parameter_string to remove load units (which should now be identical for all parameters)
storm_load$Parameter_string <- sub("\\s+$", "", paste(storm_load$Parameter, tolower(storm_load$new_Fraction_Analyzed), sep=" "))

##### New stuff. -------------------------

ParamList <- as.vector(sort(unique(storm_load$Parameter_string)))

# ditch the samples with no sample loads
storm_load <- storm_load[-which(is.na(storm_load$sample_loads) | is.na(storm_load$storm_loads)), ]

# create the data summaries for the mass loads
source(paste(scriptDirectory, "Load_byParam_sub4a_Mass_Load_Summary.r", sep="/"))
Storm_mass_loads<-Case.list.mass

# create the data summaries for the aerial loads
source(paste(scriptDirectory, "Load_byParam_sub4b_Aerial_Load_Summary.r", sep="/"))
Storm_area_loads<-Case.list.area

###Produce summary plots

pdf(file = paste(outputDirectory,"Plots of Load Summaries.pdf",sep="/"),
    height = 8.5,
    width = 11)

par(mgp=c(2.8,0.5,0),
    bg = "white")

# Set up the display with three plots on top, two on bottom

# Would like to do this, but can't figure out how to do
#  multiple page PDFs using split.screen.  C'mon, Google.

#split.screen(c(2,1))
#split.screen(c(1,3), 1)
#split.screen(c(1,2), 2)

layout(matrix(c(1,2,3,4,5,5), nrow = 2, ncol = 3, byrow = TRUE))

#i<-20

for (i in 1:length(ParamList)) {
  # VERY IMPORTANT - the value of "i" refers to the parameter being plotted throughout
  #  this loop.  DO NOT set the value of "i" within any of the subscripts.
  
    ParamData <- storm_load[which(storm_load$Parameter_string == ParamList[i]), ]
      
    # Produce boxplots of the mass loads by land use
    source( paste(scriptDirectory, "Load_byParam_sub5a_Mass_Boxplot_LandUse.r", sep="/"))
    
    #Produce boxplots of the aerial loads by land use
    source( paste(scriptDirectory, "Load_byParam_sub5b_Aerial_Boxplot_LandUse.r", sep="/"))
    
    ylimits <-  c(min(ParamData$storm_area_loads)/2, max(ParamData$storm_area_loads)*2)
    
    # Produce boxplots of the aerial loads by Wet season
    
    # Produce ECDFs for aerial loads
    
    # Produce jitter plots of aerial loads vs. binned % impervious surface
    
    
}

close.screen(all = TRUE)
dev.off()
