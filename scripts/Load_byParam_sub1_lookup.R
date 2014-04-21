
### convert from text to date format:
Storm$Field_Collection_Start_Date <- as.Date(Storm$Field_Collection_Start_Date, "%m/%d/%Y")
Storm$Field_Collection_End_Date <- as.Date(Storm$Field_Collection_End_Date, "%m/%d/%Y")

table(Storm$Parameter)

####test whether the sample volume is greater than the storm
#sample_storm<-Storm[,c("Parameter","Location_ID","Field_Collection_Start_Date","new_Result_Value")]
#sample_storm<-subset(Storm, Parameter=="Storm Event Flow Volume" | Parameter=="Sampled-Event Flow Volume",select=c("Parameter","Location_ID","Field_Collection_Start_Date","new_Result_Value"))
#location.start<- paste(sample_storm$Location_ID," ",sample_storm$Field_Collection_Start_Date,sep="")
#sample_storm<- cbind(sample_storm,location.start)
#storm.test<-reshape(data=sample_storm, v.names="new_Result_Value", idvar="location.start", timevar="Parameter", direction="wide")
#colnames(storm.test)[4]<-"Sample_event_flow"
#colnames(storm.test)[5]<-"Storm_event_flow"
#storm.test<-na.omit(storm.test)   ##ditch the rows with NAs
#Big_sample<-storm.test[storm.test$Sample_event_flow > storm.test$Storm_event_flow,]


### Create a separate table of just storm flows.  
storm_event_flows <- Storm[which(Storm$Parameter == "Storm Event Flow Volume"),c("Location_ID", "Field_Collection_Start_Date", "Field_Collection_End_Date", "new_Result_Value")]
sample_event_flows <- Storm[which(Storm$Parameter == "Sampled-Event Flow Volume"),c("Location_ID", "Field_Collection_Start_Date", "Field_Collection_End_Date", "new_Result_Value")]

### Calculate storm length.  Print out and remove any storms longer than 6 days:
storm_event_flows$storm_length <- storm_event_flows$Field_Collection_End_Date - 
                                   storm_event_flows$Field_Collection_Start_Date
storm_event_flows[which(storm_event_flows$storm_length > 6),]
storm_event_flows <- storm_event_flows[-which(storm_event_flows$storm_length > 6),]

sample_event_flows$storm_length <- sample_event_flows$Field_Collection_End_Date - 
                                   sample_event_flows$Field_Collection_Start_Date
sample_event_flows[which(sample_event_flows$storm_length > 6),]
sample_event_flows <- sample_event_flows[-which(sample_event_flows$storm_length > 6),]



### Create a function to lookup the storm flow for any samples with a unique match between
### sample date and the storm start/end dates:
get_storm_flow <- function (sampLocation, sampDate, stormList) {
  location.flags <- sampLocation == stormList$Location_ID
  date.flags <- sampDate >= stormList$Field_Collection_Start_Date & 
                sampDate <= stormList$Field_Collection_End_Date
  match <- location.flags & date.flags
  if (sum(match) == 1) {
    this_flow <- stormList[match, "new_Result_Value"]    
    return(this_flow)
  }
  return(NA)
}

### Create a new column in the storm file, fill it with NAs.
Storm$storm_event_flow_volume <- NA
Storm$sample_event_flow_volume <- NA

### Loop through each sample and try to look up the storm volume for that sample.
### WARNING THIS IS REALLY SLOW.  ONLY DOING FIRST 100 RIGHT NOW FOR TESTING.
#i <- 1
#for (i in 1:100) {
for (i in 1:nrow(Storm)) {
  Storm[i,"storm_event_flow_volume"] <- get_storm_flow(sampLocation=Storm[i,"Location_ID"], 
                                      sampDate=Storm[i,"Field_Collection_Start_Date"],
                                      stormList=storm_event_flows)
}
##to pull out sample event volumes  
#for (i in 1:100) {
for (i in 1:nrow(Storm)) {
  Storm[i,"sample_event_flow_volume"] <- get_storm_flow(sampLocation=Storm[i,"Location_ID"], 
                                      sampDate=Storm[i,"Field_Collection_Start_Date"],
                                      stormList=storm_event_flows)
}


## print out a check to see if it works
Storm[1:100, "storm_event_flow_volume"]
Storm[1:100, "sample_event_flow_volume"]

### Eventually you'll want to write the results out as a csv I think?
#write.csv(...





