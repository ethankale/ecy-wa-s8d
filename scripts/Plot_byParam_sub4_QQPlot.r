
# Graph chance of exceedance for a parameter -----------------------------------------

require(NADA)

  Param_ROS <- cenros(ParamData$new_Result_Value, ParamData$nonDetect_Flag)
  plot(Param_ROS)

  cor_value <- sqrt(summary(Param_ROS)$r.squared)
  nDetects  <- Case.list$num.Detects[i]

###  probability plot coefficient test - See Helsel and Hirsch 2002, the following is from table B3.
###  alpha = 0.05
###  Ho is that data come from log-normal distribution

  k_PPCC <- c(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 
              21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 
             37, 38, 39, 40, 41, 42, 43, 46, 47, 50, 55, 60, 65, 70, 75, 80, 90, 100)
  r_star <- c(0.879, 0.868, 0.88, 0.888, 0.898, 0.906, 0.912, 0.918, 0.923, 0.928, 
              0.932, 0.935, 0.939, 0.941, 0.944, 0.946, 0.949, 0.951, 0.952, 0.954, 
              0.956, 0.957, 0.959, 0.96, 0.961, 0.962, 0.963, 0.964, 0.965, 0.966, 
              0.967, 0.968, 0.969, 0.969, 0.97, 0.971, 0.971, 0.972, 0.973, 0.973, 
              0.974, 0.975, 0.976, 0.977, 0.979, 0.98, 0.981, 0.983, 0.984, 0.985, 
              0.986, 0.987)
  r_star_selected <- r_star[max(which(k_PPCC <= nDetects))]

  if (cor_value < r_star_selected) {
    PPCC <- "reject Ho"
  } else {
    PPCC <- "do not reject Ho"
  }

  Case.list[i,"ROS_correlation"] <- cor_value
  Case.list[i,"PPCC_test"] <- PPCC
