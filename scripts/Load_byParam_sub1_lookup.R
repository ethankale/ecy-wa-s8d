
#####
# Pulls rainfall events out of the raw data.
# Both sample events (duration of datalogger) and storm events, which should be 
#  duration from beginning to end of rainfall

require(sqldf)

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
storm_event_flows  <- Storm[which(Storm$Parameter == "Storm Event Flow Volume"),
                            c("Location_ID", 
                              "Field_Collection_Start_Date", 
                              "Field_Collection_End_Date", 
                              "new_Result_Value")
                            ]

sample_event_flows <- Storm[which(Storm$Parameter == "Sampled-Event Flow Volume"),
                            c("Location_ID", 
                              "Field_Collection_Start_Date", 
                              "Field_Collection_End_Date", 
                              "new_Result_Value")
                            ]

### Calculate storm length.  Print out and remove any storms longer than 6 days:
storm_event_flows$storm_length <- storm_event_flows$Field_Collection_End_Date - 
                                  storm_event_flows$Field_Collection_Start_Date
storm_event_flows[which(storm_event_flows$storm_length > 6),]
storm_event_flows <- storm_event_flows[-which(storm_event_flows$storm_length > 6),]

sample_event_flows$storm_length <- sample_event_flows$Field_Collection_End_Date - 
                                   sample_event_flows$Field_Collection_Start_Date
sample_event_flows[which(sample_event_flows$storm_length > 6),]
sample_event_flows <- sample_event_flows[-which(sample_event_flows$storm_length > 6),]

### Order the sample and storm tables by start date, and add an ID
storm_event_flows <- storm_event_flows[with(storm_event_flows, order(Field_Collection_Start_Date)), ]
idList <- seq(from=1, to=nrow(storm_event_flows), by = 1)
storm_event_flows$id <- idList

sample_event_flows <- sample_event_flows[with(sample_event_flows, order(Field_Collection_Start_Date)), ]
idList <- seq(from=1, to=nrow(sample_event_flows), by = 1)
sample_event_flows$id <- idList

### Plot out the storm events vs. the sample events. -------------------------
plot(x    = storm_event_flows$Field_Collection_Start_Date, 
     y    = storm_event_flows$new_Result_Value,
     main = "All Storm Events",
     sub  = "Volume of Total Storm vs. Sampled Storm Events",
     xlab = "Date",
     ylab = "Volume of Water (m3)"
     )

rect(xleft   = sample_event_flows$Field_Collection_Start_Date, 
     xright  = sample_event_flows$Field_Collection_Start_Date, 
     ytop    = sample_event_flows$new_Result_Value, 
     ybottom = 0,
     col     = "gray",
     border  = "gray"
     )

points(x    = storm_event_flows$Field_Collection_Start_Date, 
       y    = storm_event_flows$new_Result_Value,
       )

### Plot out the small events (storm vs. sample volumes, again) -------------------------
storm_event_flows_small   <- storm_event_flows[which(storm_event_flows$new_Result_Value <= 5000),]
sample_event_flows_small  <- sample_event_flows[which(sample_event_flows$new_Result_Value <= 5000),]

plot(x    = storm_event_flows_small$Field_Collection_Start_Date, 
     y    = storm_event_flows_small$new_Result_Value,
     main = "Small Storm Events",
     sub  = "Volume of Total Storm vs. Sampled Storm Events",
     xlab = "Date",
     ylab = "Volume of Water (m3)"
)

rect(xleft   = sample_event_flows_small$Field_Collection_Start_Date, 
     xright  = sample_event_flows_small$Field_Collection_Start_Date, 
     ytop    = sample_event_flows_small$new_Result_Value, 
     ybottom = 0,
     col     = "gray",
     border  = "gray"
)

points(x    = storm_event_flows_small$Field_Collection_Start_Date, 
       y    = storm_event_flows_small$new_Result_Value,
       )

### Find overlapping storm events and sample events -------------------------
overlapStorms <- sqldf("SELECT a.id, group_concat(b.id) as matching
                       FROM storm_event_flows a
                         LEFT JOIN storm_event_flows b
                           ON b.Field_Collection_Start_Date >= a.Field_Collection_Start_Date
                           AND b.Field_Collection_Start_Date <= a.Field_Collection_End_Date
                       GROUP BY a.id"
                       )
#overlapStorms$matching <- (overlapStorms$id == overlapStorms$id2)

overlapSamples <- sqldf("SELECT a.id, group_concat(b.id) as matching
                        FROM sample_event_flows a
                        LEFT JOIN sample_event_flows b
                          ON b.Field_Collection_Start_Date >= a.Field_Collection_Start_Date
                          AND b.Field_Collection_Start_Date <= a.Field_Collection_End_Date
                        GROUP BY a.id"
                        )
#overlapSamples$matching <- (overlapSamples$id == overlapSamples$id2)

### Match up flow volumes to data with SQL -------------------------
test <- sqldf(c("CREATE INDEX s1 ON Storm(Location_ID, Field_Collection_Start_Date)",
              "CREATE INDEX s2 ON storm_event_flows(Location_ID, Field_Collection_Start_Date, Field_Collection_End_Date)",
              "CREATE INDEX s3 ON sample_event_flows(Location_ID, Field_Collection_Start_Date, Field_Collection_End_Date)",
              "SELECT Storm.*, 
                  storm_event_flows.new_Result_Value AS storm_event_flow_volume,
                  sample_event_flows.new_Result_Value AS sample_event_flow_volume
                FROM Storm
                LEFT OUTER JOIN sample_event_flows 
                  ON Storm.Location_ID = sample_event_flows.Location_ID
                    AND Storm.Field_Collection_Start_Date >= sample_event_flows.Field_Collection_Start_Date
                    AND Storm.Field_Collection_Start_Date <= sample_event_flows.Field_Collection_End_Date
                LEFT OUTER JOIN storm_event_flows 
                  ON Storm.Location_ID = storm_event_flows.Location_ID
                    AND Storm.Field_Collection_Start_Date >= storm_event_flows.Field_Collection_Start_Date
                    AND Storm.Field_Collection_Start_Date <= storm_event_flows.Field_Collection_End_Date")
              )

## print out a check to see if it works
Storm[1:100, "storm_event_flow_volume"]
Storm[1:100, "sample_event_flow_volume"]

### Eventually you'll want to write the results out as a csv I think?
#write.csv(...
