
#####
# Pulls rainfall events out of the raw data.
# Both sample events (duration of datalogger) and storm events, which should be 
#  duration from beginning to end of rainfall

require(sqldf)

### convert from text to date format.  Also replace end date NAs with the start date value.
Storm$Field_Collection_Start_Date <- as.Date(Storm$Field_Collection_Start_Date, "%m/%d/%Y")
Storm$Field_Collection_End_Date   <- as.Date(Storm$Field_Collection_End_Date, "%m/%d/%Y")

Storm$Field_Collection_End_Date[is.na(Storm$Field_Collection_End_Date)] <- Storm$Field_Collection_Start_Date[is.na(Storm$Field_Collection_End_Date)]

### Dates as numbers (days since 1970/01/01), to make things simple for the SQL:
Storm$start <- as.numeric(Storm$Field_Collection_Start_Date)
Storm$end   <- as.numeric(Storm$Field_Collection_End_Date)

### Create a separate table of just storm flows. -------------------------
fields <- c("Location_ID", 
            "Permittee",
            "Field_Collection_Start_Date", 
            "Field_Collection_End_Date", 
            "start",
            "end",
            "WetSeason",
            "new_Result_Value")

storm_event_flows  <- Storm[which(Storm$Parameter == "Storm Event Flow Volume"), fields]
sample_event_flows <- Storm[which(Storm$Parameter == "Sampled-Event Flow Volume"), fields]

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
storm_event_flows <- storm_event_flows[with(storm_event_flows, order(Permittee, Location_ID, Field_Collection_Start_Date)), ]
idList <- seq(from=1, to=nrow(storm_event_flows), by = 1)
storm_event_flows$id <- idList

sample_event_flows <- sample_event_flows[with(sample_event_flows, order(Permittee, Location_ID, Field_Collection_Start_Date)), ]
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
                           ON a.start >= b.start
                           AND a.end <= b.end
                           AND a.Location_Id = b.Location_Id
                       GROUP BY a.id"
                       )
#overlapStorms$matching <- (overlapStorms$id == overlapStorms$id2)
overlapSamples <- sqldf("SELECT a.id, group_concat(b.id) as matching
                        FROM sample_event_flows a
                        LEFT JOIN sample_event_flows b
                          ON b.start >= a.start
                          AND b.end <= a.end
                          AND b.Location_Id = a.Location_Id
                        GROUP BY a.id"
                        )

### Match up flow volumes to data with SQL -------------------------
Storm <- sqldf(c("CREATE INDEX s1 ON Storm(Location_ID, start)",
              "CREATE INDEX s2 ON storm_event_flows(Location_ID, start, end)",
              "CREATE INDEX s3 ON sample_event_flows(Location_ID, start, end)",
              "SELECT Storm.*, 
                  storm_event_flows.new_Result_Value AS storm_event_flow_volume,
                  sample_event_flows.new_Result_Value AS sample_event_flow_volume
                FROM Storm
                LEFT OUTER JOIN sample_event_flows 
                  ON Storm.Location_ID = sample_event_flows.Location_ID
                    AND Storm.start >= sample_event_flows.start
                    AND Storm.end <= sample_event_flows.end
                LEFT OUTER JOIN storm_event_flows 
                  ON Storm.Location_ID = storm_event_flows.Location_ID
                    AND Storm.start >= storm_event_flows.start
                    AND Storm.end <= storm_event_flows.end")
              )

# Second function to match storm & sample loads with parameter, this time casting a 
#  wider net for those that did not match at first.  Match those storms or events
#  where EITHER the begin or end date is within the event span.
Storm$partial_match <- "N"

# Pull values without matching storms into a separate dataframe, remove from Storm
noStorm <- Storm[which(is.na(Storm$storm_event_flow_volume) | is.na(Storm$sample_event_flow_volume)), ]
Storm   <- Storm[-which(is.na(Storm$storm_event_flow_volume) | is.na(Storm$sample_event_flow_volume)), ]

# Find & append events where either the beginning or end date of the sample is within the event span
partStormMatch <- sqldf(c("CREATE INDEX s1 ON noStorm(Location_ID, start)",
                          "CREATE INDEX s2 ON storm_event_flows(Location_ID, start, end)",
                          "CREATE INDEX s3 ON sample_event_flows(Location_ID, start, end)",
                          "SELECT noStorm.*, 
                            storm_event_flows.new_Result_Value AS storm_event_flow_volume_part,
                            sample_event_flows.new_Result_Value AS sample_event_flow_volume_part
                          FROM noStorm
                          LEFT OUTER JOIN sample_event_flows 
                            ON noStorm.Location_ID = sample_event_flows.Location_ID
                            AND (
                              (noStorm.start >= sample_event_flows.start
                                AND noStorm.start <= sample_event_flows.end)
                              OR (noStorm.end >= sample_event_flows.start
                                AND noStorm.end <= sample_event_flows.end)
                            )
                          LEFT OUTER JOIN storm_event_flows 
                            ON noStorm.Location_ID = storm_event_flows.Location_ID
                              AND (
                                (noStorm.start >= storm_event_flows.start
                                  AND noStorm.start <= storm_event_flows.end)
                                OR (noStorm.end >= storm_event_flows.start
                                  AND noStorm.end <= storm_event_flows.end)
                              )"
                          )
                        )

# Make columns in partStormMatch the same as Storm
partStormMatch$storm_event_flow_volume  <- partStormMatch$storm_event_flow_volume_part
partStormMatch$sample_event_flow_volume <- partStormMatch$sample_event_flow_volume_part

partStormMatch <- partStormMatch[,!names(partStormMatch) %in% c("storm_event_flow_volume_part", "sample_event_flow_volume_part")]
partStormMatch$partial_match <- "Y"

# Add the partial (flagged) matches back to the regular matches.
Storm <- rbind(Storm, partStormMatch)

### print out a check to see if it works
#Storm[1:100, "storm_event_flow_volume"]
#Storm[1:100, "sample_event_flow_volume"]

### Eventually you'll want to write the results out as a csv I think?
#write.csv(...
