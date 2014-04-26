#####
# Determine the criteria level for each measured parameter
#  for which there exist criteria.
# Depends on Plot_byParam_ver9_Apr2014.r (or newer).
#####

criteria <- function(parameter, standard, pH, hardness) {
  # Returns Washington State freshwater criteria, in ug/L
  # By default returns acute criteria; otherwise you can specify human health (hh) or chronic
  
  if(missing(standard)) {standard = "acute"}
  
  if(missing(parameter)) {
    stop("Parameter required, at a minimum.")
  }
  
  missingpH   <- paste("For parameter", parameter, "pH is required.")
  missingHard <- paste("For parameter", parameter, "hardness is required.")
  criterion   <- ""
  
  if (standard == "acute") {
  
    if(parameter == "Ammonia water  (ug/L)") {
      if(missing(pH)) {stop(missingpH)}
      criterion <- (0.275/(1+(10^(7.204-pH))))+(39/(1+(10^(pH-7.204)))*1000)
    
    } else if (parameter == "Aresenic water dissolved (ug/L)") {
      criterion <- 360
    
    } else if (parameter == "Cadmium water  (ug/L)"){
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.944*(exp(1.128*(log(hardness))-3.828))/(1.136672 - (log(hardness)*(0.041838)))
    
    } else if (parameter == "Cadmium water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- (0.944)*(exp(1.128*(log(hardness))-3.828))
  
    } else if (parameter == "Chloride water  (ug/L)"){
      criterion <- 860
      
    } else if (parameter == "Chlorpyrifos water  (ug/L)"){
      criterion <- 0.083
  
    } else if (parameter == "Copper water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- (0.960)*(exp(0.9422(log(hardness)) - 1.464))/0.96
  
    } else if (parameter == "Copper water dissolved (ug/L)"){
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.960 * (exp(0.9422(log(hardness)) - 1.464))
      
    } else if (parameter == "Lead water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.791 * (exp(1.273 *(log(hardness)) - 1.460)) / (1.46203 - (log(hardness) * (0.145712)))
  
    } else if (parameter == "Lead water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.791 * (exp(1.273 * (log(hardness)) - 1.460)) 
  
    } else if (parameter == "Mercury water  (ug/L)") {
      criterion <- 2.1
      
    } else if (parameter == "Mercury water dissolved (ug/L)") {
      criterion <- 1.785
      
    } else if (parameter == "Pentachlorophenol water  (ug/L)") {
      if(missing(pH)) {stop(missingpH)}
      criterion <- exp((1.005*pH) - 4.830)
  
    } else if (parameter == "Total PCB water  (ug/L)") {
      criterion <- 2
  
    } else if (parameter == "Zinc water  (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.978 * (exp(0.8473 * (log(hardness)) + 0.8604)) / 0.978
      
    } else if (parameter == "Zinc water dissolved (ug/L)") {
      if(missing(hardness)) {stop(missingHard)}
      criterion <- 0.978 * (exp(0.8473 * (log(hardness)) + 0.8604))

    } else {criterion <- NA}
  
  } else if (standard == "chronic"){
    
  }
  return(criterion)
  
}