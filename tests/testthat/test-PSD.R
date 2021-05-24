library(psdr)


test_that("MakeOneSidedAmplitudeSpectrum works", {
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:(L-1))*T; #time vector
  
  #Form a signal (time series) that contains two frequencies:
  #1. 10 Hz with amplitude of 1
  #2. 25 Hz with amplitude of 2
  S <- 1*sin(2*pi*10*t) + 2*sin(2*pi*25*t);
  
  actual_results <- MakeOneSidedAmplitudeSpectrum(Fs, S)
  actual_frequencies <- actual_results[[1]]
  actual_amplitudes <- actual_results[[2]]
  
  #Making the single side amp spectrum manually
  X <- S
  Y <- stats::fft(S)
  P2 <- abs(Y/L)
  P1 <- P2[c(1:((L/2)+1))]
  P1[c((2:(length(P1)-1)))] <- 2*P1[c((2:(length(P1)-1)))]  #Double amplitude because we are only looking at one side.
  f = Fs*(0:(L/2))/L
  expected_frequencies <- f
  expected_amplitudes <- P1

  expect_equal(actual_frequencies, expected_frequencies)
  
  expect_equal(actual_amplitudes, expected_amplitudes)
  
})


test_that("MakePowerSpectralDensity works", {
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:(L-1))*T; #time vector
  
  #Form a signal (time series) that contains two frequencies:
  #1. 10 Hz with amplitude of 1
  #2. 25 Hz with amplitude of 2
  S <- 1*sin(2*pi*10*t) + 2*sin(2*pi*25*t);
  
  one_side_results <- MakeOneSidedAmplitudeSpectrum(Fs, S)
  one_side_frequencies <- actual_results[[1]]
  one_side_amplitudes <- actual_results[[2]]
  
  #Use math operations to see if one sided amplitude
  #can be converted to PSD
  expected_PSD <- ((one_side_amplitudes/2)^2)/(0.5*Fs)
  
  PSD_results <- MakePowerSpectralDensity(Fs, S)
  
  actual_PSD <- PSD_results[[2]]
  
  expect_equal(actual_PSD, expected_PSD)
  
})


test_that("MakeCompositePSDForAllWindows works", {
  

  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:(L-1))*T; #time vector
  
  #First signal
  #1. 10 Hz with amplitude of 4
  #2. 25 Hz with amplitude of 4
  S1 <- 1*sin(2*pi*10*t) + 2*sin(2*pi*25*t);
  S1 <- S1 + rnorm(length(t)) #Add some noise
  S1.data.frame <- as.data.frame(cbind(t, S1))
  colnames(S1.data.frame) <- c("Time", "Signal")
  
  #Second signal
  #1. 5 Hz with amplitude of 2
  #2. 8 Hz with amplitude of 2
  S2 <- 2*sin(2*pi*5*t) + 2*sin(2*pi*8*t);
  S2 <- S2 + rnorm(length(t)) #Add some noise
  S2.data.frame <- as.data.frame(cbind(t, S2))
  colnames(S2.data.frame) <- c("Time", "Signal")
  
  #Third signal
  #1. 5 Hz with amplitude of 2
  #2. 8 Hz with amplitude of 2
  S3 <- 2*sin(2*pi*5*t) + 2*sin(2*pi*8*t);
  S3 <- S3 + rnorm(length(t)) #Add some noise
  S3.data.frame <- as.data.frame(cbind(t, S3))
  colnames(S3.data.frame) <- c("Time", "Signal")
  
  #Add all signals to a List
  list.of.windows <- list(S1.data.frame, S2.data.frame, S3.data.frame)
  
  averaged_results <- MakeCompositePSDForAllWindows(list.of.windows, "Signal", Fs, 0, 50, 0.1)
  
  
  PSD1 <- MakePowerSpectralDensity(Fs, S1)[[2]]
  PSD2 <- MakePowerSpectralDensity(Fs, S2)[[2]]
  PSD3 <- MakePowerSpectralDensity(Fs, S3)[[2]]
  combined_PSD <- rbind(PSD1, PSD2, PSD3)
  
  expected_average <- (PSD1 + PSD2 + PSD3)/3
  expected_stddev <- apply(combined_PSD,2, stats::sd)
  
  
  expect_equal(averaged_results[[2]], expected_average)
  
  expect_equal(averaged_results[[3]], expected_stddev)
  
})


test_that("MakeCompositeXYPlotForAllWindows works", {
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:(L-1))*T; #time vector
  
  #First signal
  #1. 1 Hz with amplitude of 4
  S1 <- 4*sin(2*pi*1*t)
  S1.data.frame <- as.data.frame(cbind(t, S1))
  colnames(S1.data.frame) <- c("Time", "Signal")
  
  #Second signal
  #1. 1 Hz with amplitude of -2
  #2. 2 Hz with amplitude of -2
  S2 <- (-2)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
  S2.data.frame <- as.data.frame(cbind(t, S2))
  colnames(S2.data.frame) <- c("Time", "Signal")
  
  #Third signal
  #1. 1 Hz with amplitude of 2
  #2. 2 Hz with amplitude of 2
  S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
  S3.data.frame <- as.data.frame(cbind(t, S3))
  colnames(S3.data.frame) <- c("Time", "Signal")
  
  #Add all signals to a List
  list.of.windows <- list(S1.data.frame, S2.data.frame, S3.data.frame)
  
  averaged_results <- MakeCompositeXYPlotForAllWindows(list.of.windows, "Signal", 0, 999, 1)
  
  combined_timeseries <- rbind(S1, S2, S3)
  
  expected_average <- (S1 + S2 + S3)/3
  expected_stddev <- apply(combined_timeseries,2, stats::sd)
  
  expect_equal(averaged_results[[2]], expected_average)
  
  expect_equal(averaged_results[[3]], expected_stddev)
  
  
})


#AutomatedCompositePlotting tests:


test_that("AutomatedCompositePlotting timeseries works", {
  
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:(L-1))*T; #time vector
  
  #First signal
  #1. 1 Hz with amplitude of 2
  S1 <- 2*sin(2*pi*1*t)
  level1.vals <- rep("a", length(S1))
  level2.vals <- rep("1", length(S1))
  S1.data.frame <- as.data.frame(cbind(t, S1, level1.vals, level2.vals))
  colnames(S1.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
  
  #Second signal
  #1. 1 Hz with amplitude of -4
  #2. 2 Hz with amplitude of -2
  S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S2))
  level2.vals <- rep("2", length(S2))
  S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
  colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
  
  #Third signal
  #1. 1 Hz with amplitude of 2
  #2. 2 Hz with amplitude of 2
  S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S3))
  level2.vals <- rep("3", length(S3))
  S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
  colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
  
  #Fourth signal
  #1. 1 Hz with amplitude of -2
  S4 <- -2*sin(2*pi*1*t)
  level1.vals <- rep("b", length(S4))
  level2.vals <- rep("3", length(S4))
  S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
  colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
  
  windows <- list(S1.data.frame, S2.data.frame, S3.data.frame, S4.data.frame)
  
  #Gets the composite of the first, second, and third signal. Should result in a flat signal.
  FirstComboToUse <- list( c("a"), c(1, 2, 3) )
  
  #Gets the composite of the third and fourth signal
  SecondComboToUse <- list( c("a", "b"), c(3) )
  
  
  #Timeseries-----------------------------------------------------------------
  
  #-------------------------------------------
  # Test for no envelope
  #-------------------------------------------
  
  timeseries.results <- AutomatedCompositePlotting(list.of.windows = windows,
                                                   name.of.col.containing.time.series = "Signal",
                                                   x_start = 0,
                                                   x_end = 999,
                                                   x_increment = 1,
                                                   level1.column.name = "level1.ID",
                                                   level2.column.name = "level2.ID",
                                                   level.combinations = list(FirstComboToUse, SecondComboToUse),
                                                   level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
                                                   plot.title = "Example",
                                                   plot.xlab = "Time",
                                                   plot.ylab = "Original units",
                                                   combination.index.for.envelope = NULL,
                                                   TimeSeries.PSD.LogPSD = "TimeSeries",
                                                   sampling_frequency = NULL)
  
  #Are there two objects total.
  expect_equal(length(timeseries.results), 2)
  
  #Is the first object a list of dataframes, with the length of list equal the number of combinations.
  expect_equal(length(timeseries.results[[1]]), 2)
  expect_equal(is.data.frame(timeseries.results[[1]][[1]]), TRUE)
  
  #Is the second object a ggplot.
  expect_equal(ggplot2::is.ggplot(timeseries.results[[2]]), TRUE)
  
  #Does the ggplot have two layers, one for each combo.
  expect_equal(length(timeseries.results[[2]]$layers), 2)
  
  #-------------------------------------------
  # Test for envelope
  #-------------------------------------------
  
  timeseries.results.envelope <- AutomatedCompositePlotting(list.of.windows = windows,
                                                   name.of.col.containing.time.series = "Signal",
                                                   x_start = 0,
                                                   x_end = 999,
                                                   x_increment = 1,
                                                   level1.column.name = "level1.ID",
                                                   level2.column.name = "level2.ID",
                                                   level.combinations = list(FirstComboToUse, SecondComboToUse),
                                                   level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
                                                   plot.title = "Example",
                                                   plot.xlab = "Time",
                                                   plot.ylab = "Original units",
                                                   combination.index.for.envelope = 1,
                                                   TimeSeries.PSD.LogPSD = "TimeSeries",
                                                   sampling_frequency = NULL)
  
  #Are there two objects total.
  expect_equal(length(timeseries.results.envelope), 2)
  
  #Is the first object a list of dataframes, with the length of list equal the number of combinations.
  expect_equal(length(timeseries.results.envelope[[1]]), 2)
  expect_equal(is.data.frame(timeseries.results.envelope[[1]][[1]]), TRUE)
  
  #Is the second object a ggplot.
  expect_equal(ggplot2::is.ggplot(timeseries.results.envelope[[2]]), TRUE)
  
  #Does the ggplot have two layers, one for each combo, and one for envelope
  expect_equal(length(timeseries.results.envelope[[2]]$layers), 3)
  
  
})


test_that("AutomatedCompositePlotting PSD works", {
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:(L-1))*T; #time vector
  
  #First signal
  #1. 1 Hz with amplitude of 2
  S1 <- 2*sin(2*pi*1*t)
  level1.vals <- rep("a", length(S1))
  level2.vals <- rep("1", length(S1))
  S1.data.frame <- as.data.frame(cbind(t, S1, level1.vals, level2.vals))
  colnames(S1.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
  
  #Second signal
  #1. 1 Hz with amplitude of -4
  #2. 2 Hz with amplitude of -2
  S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S2))
  level2.vals <- rep("2", length(S2))
  S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
  colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
  
  #Third signal
  #1. 1 Hz with amplitude of 2
  #2. 2 Hz with amplitude of 2
  S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S3))
  level2.vals <- rep("3", length(S3))
  S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
  colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
  
  #Fourth signal
  #1. 1 Hz with amplitude of -2
  S4 <- -2*sin(2*pi*1*t)
  level1.vals <- rep("b", length(S4))
  level2.vals <- rep("3", length(S4))
  S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
  colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
  
  windows <- list(S1.data.frame, S2.data.frame, S3.data.frame, S4.data.frame)
  
  #Gets the composite of the first, second, and third signal. Should result in a flat signal.
  FirstComboToUse <- list( c("a"), c(1, 2, 3) )
  
  #Gets the composite of the third and fourth signal
  SecondComboToUse <- list( c("a", "b"), c(3) )
  
  #-------------------------------------------
  # Test for no envelope
  #-------------------------------------------
  
  #Expect warning about tied ranks from the statistical testing
  expect_warning(
    PSD.results <- AutomatedCompositePlotting(list.of.windows = windows,
                                                       name.of.col.containing.time.series = "Signal",
                                                       x_start = 0,
                                                       x_end = 50,
                                                       x_increment = 0.01,
                                                       level1.column.name = "level1.ID",
                                                       level2.column.name = "level2.ID",
                                                       level.combinations = list(FirstComboToUse, SecondComboToUse),
                                                       level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
                                                       plot.title = "Example",
                                                       plot.xlab = "Hz",
                                                       plot.ylab = "(Original units)^2/Hz",
                                                       combination.index.for.envelope = NULL,
                                                       TimeSeries.PSD.LogPSD = "PSD",
                                                       sampling_frequency = 100)
  )
  
  #Are there three objects total.
  expect_equal(length(PSD.results), 3)
  
  #Is the first object a list of dataframes, with the length of list equal the number of combinations.
  expect_equal(length(PSD.results[[1]]), 2)
  expect_equal(is.data.frame(PSD.results[[1]][[1]]), TRUE)
  
  #Is the second object a ggplot.
  expect_equal(ggplot2::is.ggplot(PSD.results[[2]]), TRUE)
  
  #Does the ggplot have two layers, one for each combo, and one for envelope.
  expect_equal(length(PSD.results[[2]]$layers), 2)
  
  #Is the third object a list with three objects for statistical testing. 
  expect_equal(length(PSD.results[[3]]), 3)
  
  #Is the first object for statistical testing a dataframe with number of rows = number of total windows to make composites.
  expect_equal(is.data.frame(PSD.results[[3]][[1]]), TRUE)
  number_of_windows_used <- length(FirstComboToUse[[1]]) * length(FirstComboToUse[[2]]) + length(SecondComboToUse[[1]]) * length(SecondComboToUse[[2]])
  expect_equal(dim(PSD.results[[3]][[1]])[1], number_of_windows_used)
  
  #Is the second object for statistical testing using kruskal wallis chi-squared.
  expect_equal(names(PSD.results[[3]][[2]]$statistic), "Kruskal-Wallis chi-squared")
  
  #Is the third object for statistical testing using wilcoxon rank sum test.
  expect_equal(PSD.results[[3]][[3]]$method, "Wilcoxon rank sum test with continuity correction")
  
  
  #-------------------------------------------
  # Test for envelope
  #-------------------------------------------
  
  #Expect warning about tied ranks from the statistical testing
  expect_warning(
  PSD.results.envelope <- AutomatedCompositePlotting(list.of.windows = windows,
                                            name.of.col.containing.time.series = "Signal",
                                            x_start = 0,
                                            x_end = 50,
                                            x_increment = 0.01,
                                            level1.column.name = "level1.ID",
                                            level2.column.name = "level2.ID",
                                            level.combinations = list(FirstComboToUse, SecondComboToUse),
                                            level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
                                            plot.title = "Example",
                                            plot.xlab = "Hz",
                                            plot.ylab = "(Original units)^2/Hz",
                                            combination.index.for.envelope = 2,
                                            TimeSeries.PSD.LogPSD = "PSD",
                                            sampling_frequency = 100)
  )
  
  #Are there three objects total.
  expect_equal(length(PSD.results.envelope), 3)
  
  #Is the first object a list of dataframes, with the length of list equal the number of combinations.
  expect_equal(length(PSD.results.envelope[[1]]), 2)
  expect_equal(is.data.frame(PSD.results.envelope[[1]][[1]]), TRUE)
  
  #Is the second object a ggplot.
  expect_equal(ggplot2::is.ggplot(PSD.results.envelope[[2]]), TRUE)
  
  #Does the ggplot have two layers, one for each combo, and one for envelope.
  expect_equal(length(PSD.results.envelope[[2]]$layers), 3)
  
  #Is the third object a list with three objects for statistical testing. 
  expect_equal(length(PSD.results.envelope[[3]]), 3)
  
  #Is the first object for statistical testing a dataframe with number of rows = number of total windows to make composites.
  expect_equal(is.data.frame(PSD.results.envelope[[3]][[1]]), TRUE)
  number_of_windows_used <- length(FirstComboToUse[[1]]) * length(FirstComboToUse[[2]]) + length(SecondComboToUse[[1]]) * length(SecondComboToUse[[2]])
  expect_equal(dim(PSD.results.envelope[[3]][[1]])[1], number_of_windows_used)
  
  #Is the second object for statistical testing using kruskal wallis chi-squared.
  expect_equal(names(PSD.results.envelope[[3]][[2]]$statistic), "Kruskal-Wallis chi-squared")
  
  #Is the third object for statistical testing using wilcoxon rank sum test.
  expect_equal(PSD.results.envelope[[3]][[3]]$method, "Wilcoxon rank sum test with continuity correction")
  
  
})



test_that("AutomatedCompositePlotting LogPSD works", {
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:(L-1))*T; #time vector
  
  #First signal
  #1. 1 Hz with amplitude of 2
  S1 <- 2*sin(2*pi*1*t)
  level1.vals <- rep("a", length(S1))
  level2.vals <- rep("1", length(S1))
  S1.data.frame <- as.data.frame(cbind(t, S1, level1.vals, level2.vals))
  colnames(S1.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
  
  #Second signal
  #1. 1 Hz with amplitude of -4
  #2. 2 Hz with amplitude of -2
  S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S2))
  level2.vals <- rep("2", length(S2))
  S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
  colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
  
  #Third signal
  #1. 1 Hz with amplitude of 2
  #2. 2 Hz with amplitude of 2
  S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S3))
  level2.vals <- rep("3", length(S3))
  S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
  colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
  
  #Fourth signal
  #1. 1 Hz with amplitude of -2
  S4 <- -2*sin(2*pi*1*t)
  level1.vals <- rep("b", length(S4))
  level2.vals <- rep("3", length(S4))
  S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
  colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
  
  windows <- list(S1.data.frame, S2.data.frame, S3.data.frame, S4.data.frame)
  
  #Gets the composite of the first, second, and third signal. Should result in a flat signal.
  FirstComboToUse <- list( c("a"), c(1, 2, 3) )
  
  #Gets the composite of the third and fourth signal
  SecondComboToUse <- list( c("a", "b"), c(3) )
  
  #-------------------------------------------
  # Test for no envelope
  #-------------------------------------------

    LogPSD.results <- AutomatedCompositePlotting(list.of.windows = windows,
                                              name.of.col.containing.time.series = "Signal",
                                              x_start = 0,
                                              x_end = 50,
                                              x_increment = 0.01,
                                              level1.column.name = "level1.ID",
                                              level2.column.name = "level2.ID",
                                              level.combinations = list(FirstComboToUse, SecondComboToUse),
                                              level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
                                              plot.title = "Example",
                                              plot.xlab = "Hz",
                                              plot.ylab = "(Original units)^2/Hz",
                                              combination.index.for.envelope = NULL,
                                              TimeSeries.PSD.LogPSD = "LogPSD",
                                              sampling_frequency = 100)
  
  #Are there two objects total.
  expect_equal(length(LogPSD.results), 2)
  
  #Is the first object a list of dataframes, with the length of list equal the number of combinations.
  expect_equal(length(LogPSD.results[[1]]), 2)
  expect_equal(is.data.frame(LogPSD.results[[1]][[1]]), TRUE)
  
  #Is the second object a ggplot.
  expect_equal(ggplot2::is.ggplot(LogPSD.results[[2]]), TRUE)
  
  #Does the ggplot have two layers, one for each combo, and one for envelope.
  expect_equal(length(LogPSD.results[[2]]$layers), 2)
  
  
  #-------------------------------------------
  # Test for envelope
  #-------------------------------------------
  
  LogPSD.results.envelope <- AutomatedCompositePlotting(list.of.windows = windows,
                                               name.of.col.containing.time.series = "Signal",
                                               x_start = 0,
                                               x_end = 50,
                                               x_increment = 0.01,
                                               level1.column.name = "level1.ID",
                                               level2.column.name = "level2.ID",
                                               level.combinations = list(FirstComboToUse, SecondComboToUse),
                                               level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
                                               plot.title = "Example",
                                               plot.xlab = "Hz",
                                               plot.ylab = "(Original units)^2/Hz",
                                               combination.index.for.envelope = 2,
                                               TimeSeries.PSD.LogPSD = "LogPSD",
                                               sampling_frequency = 100)
  
  #Are there two objects total.
  expect_equal(length(LogPSD.results.envelope), 2)
  
  #Is the first object a list of dataframes, with the length of list equal the number of combinations.
  expect_equal(length(LogPSD.results.envelope[[1]]), 2)
  expect_equal(is.data.frame(LogPSD.results.envelope[[1]][[1]]), TRUE)
  
  #Is the second object a ggplot.
  expect_equal(ggplot2::is.ggplot(LogPSD.results.envelope[[2]]), TRUE)
  
  #Does the ggplot have two layers, one for each combo, and one for envelope.
  expect_equal(length(LogPSD.results.envelope[[2]]$layers), 3)
  
  
})


test_that("PSDIntegrationPerFreqBin works", {
  
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:L-1)*T; #time vector
  
  #Form a signal (time series) that contains two frequencies:
  #1. 10 Hz with amplitude of 1
  #2. 25 Hz with amplitude of 2
  S <- 1*sin(2*pi*10*t) + 2*sin(2*pi*25*t);
  
  results <- MakePowerSpectralDensity(Fs, S)
  
  frequencies <- results[[1]]
  
  PSD <- results[[2]]
  
  bins <- list(
    c(9, 11),
    c(24,26),
    c(9,26),
    c(30,40)
  )
  
  integration.results <- PSDIntegrationPerFreqBin(Fs, S, bins)
  
  #bin 3 should be sum of bin 1 and 2
  expect_equal(integration.results[[1]][[2]] + integration.results[[2]][[2]], integration.results[[3]][[2]], tolerance = 0.01)
  
  #bin 4 should be zero
  expect_equal(integration.results[[4]][[2]], 0, tolerance = 0.01)
  

})



test_that("PSDIdentifyDominantFrequency works", {
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:(L-1))*T; #time vector

  #First signal
  #1. 1 Hz with amplitude of 2
  S1 <- 2*sin(2*pi*1*t)
  level1.vals <- rep("a", length(S1))
  level2.vals <- rep("1", length(S1))
  S1.data.frame <- as.data.frame(cbind(t, S1, level1.vals, level2.vals))
  colnames(S1.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
  
  results <- PSDIdentifyDominantFrequency(Fs, S1.data.frame[,"Signal"], 0, 10, 0.01)
  
  expect_equal(results[[1]], 1)
  
})



test_that("SingleBinPSDIntegrationOrDominantFreqComparison integration comparison works", {
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:L-1)*T; #time vector
  
  #First signal
  #1. 1 Hz with amplitude of 2
  S1 <- 2*sin(2*pi*1*t)
  level1.vals <- rep("a", length(S1))
  level2.vals <- rep("1", length(S1))
  S1.data.frame <- as.data.frame(cbind(t, S1, level1.vals, level2.vals))
  colnames(S1.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
  
  #Second signal
  #1. 1 Hz with amplitude of -4
  #2. 2 Hz with amplitude of -2
  S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S2))
  level2.vals <- rep("2", length(S2))
  S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
  colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
  
  #Third signal
  #1. 1 Hz with amplitude of 2
  #2. 2 Hz with amplitude of 2
  S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S3))
  level2.vals <- rep("3", length(S3))
  S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
  colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
  
  #Fourth signal
  #1. 1 Hz with amplitude of -2
  S4 <- -2*sin(2*pi*1*t)
  level1.vals <- rep("b", length(S4))
  level2.vals <- rep("3", length(S4))
  S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
  colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
  
  #Fifth signal
  #1. 5 Hz with amplitude of -2
  S5 <- -2*sin(2*pi*5*t)
  level1.vals <- rep("c", length(S5))
  level2.vals <- rep("1", length(S5))
  S5.data.frame <- as.data.frame(cbind(t, S5, level1.vals, level2.vals))
  colnames(S5.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S5.data.frame[,"Signal"] <- as.numeric(S5.data.frame[,"Signal"])
  
  #Extra representation of S2 dataframe to show an example that has enough samples
  #to have significance for Kruskal-Wallis test
  windows <- list(S1.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame,
                  S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S3.data.frame,
                  S4.data.frame,
                  S5.data.frame, S5.data.frame, S5.data.frame, S5.data.frame, S5.data.frame)
  
  #Gets the composite of the first, second, and third signal. Should result in a flat signal.
  FirstComboToUse <- list( c("a"), c(1, 2, 3) )
  
  #Gets the composite of the third and fourth signal
  SecondComboToUse <- list( c("a", "b"), c(3) )
  
  #Gets the composite of fifth signal
  ThirdComboToUse <- list( c("c"), c(1) )
  
  #-------------------------------------------
  # Test integration to see if non-significant difference is indicated as non-sig
  #-------------------------------------------
  
  #expect warning for tied ranks.
  expect_warning(
    integration.compare.res.no.sig <- SingleBinPSDIntegrationOrDominantFreqComparison(
    list.of.windows = windows,
    name.of.col.containing.time.series = "Signal",
    level1.column.name = "level1.ID",
    level2.column.name = "level2.ID",
    level.combinations = list(FirstComboToUse, SecondComboToUse),
    level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
    sampling_frequency = 100,
    single.bin.boundary = c(1.5, 2.5),
    integration.or.dominant.freq = "integration")
  )
  
  #Does p-value indicate non-significant
  expect_equal(integration.compare.res.no.sig[[2]]$p.value == 1, TRUE)
  
  #Are there 13 windows in total. "Signal 1 + 2 + 3" should have 11. "Signal 3 + 4" should have 2.
  #Note that a single window can be found in multiple categories. 
  expect_equal(dim(integration.compare.res.no.sig[[1]])[1], 13)
  
  
  #-------------------------------------------
  # Test integration to see if significant difference is indicated as sig
  #-------------------------------------------
  
  #expect warning for tied ranks.
  expect_warning(
  integration.compare.res.sig <- SingleBinPSDIntegrationOrDominantFreqComparison(
    list.of.windows = windows,
    name.of.col.containing.time.series = "Signal",
    level1.column.name = "level1.ID",
    level2.column.name = "level2.ID",
    level.combinations = list(FirstComboToUse, ThirdComboToUse),
    level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 5"),
    sampling_frequency = 100,
    single.bin.boundary = c(1.5, 2.5),
    integration.or.dominant.freq = "integration")
  )

  #Does p-value indicate non signficant
  expect_equal(integration.compare.res.sig[[2]]$p.value < 0.05, TRUE)
  
  #Are there 16 windows in total. "Signal 1 + 2 + 3" should have 11. "Signal 5" should have 5.
  expect_equal(dim(integration.compare.res.sig[[1]])[1], 16)
  
})



test_that("SingleBinPSDIntegrationOrDominantFreqComparison dominant frequency comparison works", {
  
  #Create a vector of time that represent times where data are sampled.
  Fs = 100; #sampling frequency in Hz
  T = 1/Fs; #sampling period
  L = 1000; #length of time vector
  t = (0:L-1)*T; #time vector
  
  #First signal
  #1. 1 Hz with amplitude of 2
  S1 <- 2*sin(2*pi*1*t)
  level1.vals <- rep("a", length(S1))
  level2.vals <- rep("1", length(S1))
  S1.data.frame <- as.data.frame(cbind(t, S1, level1.vals, level2.vals))
  colnames(S1.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
  
  #Second signal
  #1. 1 Hz with amplitude of -4
  #2. 2 Hz with amplitude of -2
  S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S2))
  level2.vals <- rep("2", length(S2))
  S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
  colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
  
  #Third signal
  #1. 1 Hz with amplitude of 2
  #2. 2 Hz with amplitude of 2
  S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
  level1.vals <- rep("a", length(S3))
  level2.vals <- rep("3", length(S3))
  S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
  colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
  
  #Fourth signal
  #1. 1 Hz with amplitude of -2
  S4 <- -2*sin(2*pi*1*t)
  level1.vals <- rep("b", length(S4))
  level2.vals <- rep("3", length(S4))
  S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
  colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
  
  #Fifth signal
  #1. 5 Hz with amplitude of -2
  S5 <- -2*sin(2*pi*5*t)
  level1.vals <- rep("c", length(S5))
  level2.vals <- rep("1", length(S5))
  S5.data.frame <- as.data.frame(cbind(t, S5, level1.vals, level2.vals))
  colnames(S5.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
  S5.data.frame[,"Signal"] <- as.numeric(S5.data.frame[,"Signal"])
  
  #Extra representation of S2 dataframe to show an example that has enough samples
  #to have significance for Kruskal-Wallis test
  windows <- list(S1.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame,
                  S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S3.data.frame,
                  S4.data.frame,
                  S5.data.frame, S5.data.frame, S5.data.frame, S5.data.frame, S5.data.frame)
  
  #Gets the composite of the first, second, and third signal. Should result in a flat signal.
  FirstComboToUse <- list( c("a"), c(1, 2, 3) )
  
  #Gets the composite of the third and fourth signal
  SecondComboToUse <- list( c("a", "b"), c(3) )
  
  #Gets the composite of fifth signal
  ThirdComboToUse <- list( c("c"), c(1) )
  
  #-------------------------------------------
  # Test dominant frequency to see if non-significant difference is indicated as non-sig
  #-------------------------------------------
  
  #expect warning for tied ranks.
  expect_warning(
    integration.compare.res.no.sig <- SingleBinPSDIntegrationOrDominantFreqComparison(
      list.of.windows = windows,
      name.of.col.containing.time.series = "Signal",
      level1.column.name = "level1.ID",
      level2.column.name = "level2.ID",
      level.combinations = list(FirstComboToUse, SecondComboToUse),
      level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
      sampling_frequency = 100,
      single.bin.boundary = NULL,
      x_start = 0,
      x_end = 50,
      x_increment = 0.1,
      integration.or.dominant.freq = "dominant.freq")
  )
  
  #Does p-value indicate non-significant (NA since all values are the same)
  expect_equal(is.na(integration.compare.res.no.sig[[2]]$p.value), TRUE)
  
  #Are there 13 windows in total. "Signal 1 + 2 + 3" should have 11. "Signal 3 + 4" should have 2.
  #Note that a single window can be found in multiple categories. 
  expect_equal(dim(integration.compare.res.no.sig[[1]])[1], 13)
  
  
  #-------------------------------------------
  # Test dominant frequency to see if significant difference is indicated as sig
  #-------------------------------------------
  
  #expect warning for tied ranks.
  expect_warning(
    integration.compare.res.sig <- SingleBinPSDIntegrationOrDominantFreqComparison(
      list.of.windows = windows,
      name.of.col.containing.time.series = "Signal",
      level1.column.name = "level1.ID",
      level2.column.name = "level2.ID",
      level.combinations = list(FirstComboToUse, ThirdComboToUse),
      level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 5"),
      sampling_frequency = 100,
      single.bin.boundary = NULL,
      x_start = 0,
      x_end = 50,
      x_increment = 0.1,
      integration.or.dominant.freq = "dominat.freq")
  )
  
  #Does p-value indicate non signficant
  expect_equal(integration.compare.res.sig[[2]]$p.value < 0.05, TRUE)
  
  #Are there 16 windows in total. "Signal 1 + 2 + 3" should have 11. "Signal 5" should have 5.
  expect_equal(dim(integration.compare.res.sig[[1]])[1], 16)
  
  
})
