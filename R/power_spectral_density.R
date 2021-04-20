

#' Create a one sided amplitude spectrum using time series data
#'
#' Code was referenced from here: https://www.mathworks.com/help/matlab/ref/fft.html
#'
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param data_vector Vector of numeric values. Timeseries vector of data.
#'
#' @return A List with two objects:
#' 1. Vector of frequencies in Hz. The maximum frequency should be half the sampling frequency. Called Nyquist Frequency.
#' 2. Vector amplitudes corresponding with each frequency. Units should be in the original units of the data vector.
#'
#' The vector of frequencies can be used as the x-axis values of a single sided spectrum amplitude plot.
#' The vector of amplitudes can be used as the y-axis values of a single sided spectrum.
#'
#' @export
#'
#' @examples
#'
#' #Create a vector of time that represent times where data are sampled.
#' Fs = 100; #sampling frequency in Hz
#' T = 1/Fs; #sampling period
#' L = 1000; #length of time vector
#' t = (0:L-1)*T; #time vector
#'
#' #Form a signal (time series) that contains two frequencies:
#' #1. 10 Hz with amplitude of 1
#' #2. 25 Hz with amplitude of 2
#' S <- 1*sin(2*pi*10*t) + 2*sin(2*pi*25*t);
#'
#' results <- MakeOneSidedAmplitudeSpectrum(Fs, S)
#'
#' frequencies <- results[[1]]
#'
#' amplitudes <- results[[2]]
#'
#' dev.new()
#' plot(frequencies, amplitudes, type = "l")
#'
#'
MakeOneSidedAmplitudeSpectrum <- function(sampling_frequency, data_vector){

  Fs <- sampling_frequency
  T <- 1/Fs
  L <- length(data_vector)
  t <- (0:(L-1)) * T

  X <- data_vector

  Y <- stats::fft(X)

  P2 <- abs(Y/L)

  P1 <- P2[c(1:((L/2)+1))]

  P1[c((2:(length(P1)-1)))] <- 2*P1[c((2:(length(P1)-1)))]  #Double amplitude because we are only looking at one side.


  f = Fs*(0:(L/2))/L

  output <- list(f, P1)

  return(output)

}


#' Create a one power spectral density (PSD) plot using time series data
#'
#' Using fft(), the PSD of a time series dataset can be calculated.
#'
#' If time series is a vector of accelerometer data, then the outputted y-axis will
#' have units of (acceleration^2)/Hz.
#'
#' References
#' https://www.mathworks.com/help/signal/ug/power-spectral-density-estimates-using-fft.html
#' https://blog.endaq.com/why-the-power-spectral-density-psd-is-the-gold-standard-of-vibration-analysis
#' https://endaq.com/pages/power-spectral-density
#'
#'
#'
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param data_vector Vector of numeric values. Timeseries vector of data.
#'
#' @return A List with two objects:
#' 1. Vector of frequencies in Hz. The maximum frequency should be half the sampling frequency. Called Nyquist Frequency.
#' 2. Vector of PSD values corresponding with each frequency. Units should be in the original units of the data vector squared and divided by frequency.
#'
#' The vector of frequencies can be used as the x-axis values of a single sided spectrum amplitude plot.
#' The vector of PSD values can be used as the y-axis values of the PSD plot.
#'
#' @export
#'
#'
#' @examples
#' #Create a vector of time that represent times where data are sampled.
#' Fs = 100; #sampling frequency in Hz
#' T = 1/Fs; #sampling period
#' L = 1000; #length of time vector
#' t = (0:L-1)*T; #time vector
#'
#' #Form a signal (time series) that contains two frequencies:
#' #1. 10 Hz with amplitude of 1
#' #2. 25 Hz with amplitude of 2
#' S <- 1*sin(2*pi*10*t) + 2*sin(2*pi*25*t);
#'
#' results <- MakePowerSpectralDensity(Fs, S)
#'
#' frequencies <- results[[1]]
#'
#' PSD <- results[[2]]
#'
#' dev.new()
#' plot(frequencies, PSD, type = "l")
#'
#'
MakePowerSpectralDensity <- function(sampling_frequency, data_vector){

  Fs <- sampling_frequency
  T <- 1/Fs
  L <- length(data_vector)
  t <- (0:(L-1)) * T

  X <- data_vector

  Y <- stats::fft(X)

  P2 <- abs(Y/L)

  P1 <- P2[c(1:((L/2)+1))]

  P1 <- (P1 * P1)/(Fs)

  P1[c((2:(length(P1)-1)))] <- 2*P1[c((2:(length(P1)-1)))]  #Double amplitude because we are only looking at one side.


  f = Fs*(0:(L/2))/L

  output <- list(f, P1)

  return(output)

}

#' Make PSD for each window in a list and then find the average of all the PSDs
#'
#' Given multiple windows of time series data, if the sampling frequency for
#' all time series is the same, then the PSD for each window can be calculated,
#' and then averaged to create a composite PSD.
#'
#' Using fft(), the PSD of a time series dataset can be calculated.This is done
#' for multiple windows of time using the MakePowerSpectralDensity() function
#' for each window. When the code executes, a counter is displayed to indicate
#' how many windows have been analyzed.
#'
#' @param list.of.windows A list of windows (dataframes).
#' @param name.of.col.containing.time.series A string that specifies the name of the column in the windows that correspond to the time series that should be used for making PSD.
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param x_start Numeric value specifying start of the new x-axis for the averaged PSD. Default is 0 Hz.
#' @param x_end Numeric value specifying end of the new x-axis for the averaged PSD. Maximum value is the sampling_frequency divided by 2.
#' @param x_increment Numeric value specifying increment of the new x-axis for the averaged PSD.
#'
#' @return A List with two objects:
#' 1. Vector of frequencies in Hz. The maximum frequency should be half the sampling frequency. Called Nyquist Frequency.
#' 2. Vector of averaged PSD values corresponding with each frequency. Units should be in the original units of the data vector squared and divided by frequency.
#' 3. Vector of standard deviation of PSD values corresponding with each frequency. This can be used to generate error envelopes or error bars to show the variation between windows.
#'
#' The vector of frequencies can be used as the x-axis values of a single sided spectrum amplitude plot.
#' The vector of PSD values can be used as the y-axis values of the PSD plot.
#'
#' @export
#'
#' @examples
#' #Create a vector of time that represent times where data are sampled.
#' Fs = 100; #sampling frequency in Hz
#' T = 1/Fs; #sampling period
#' L = 1000; #length of time vector
#' t = (0:L-1)*T; #time vector
#'
#' #First signal
#' #1. 10 Hz with amplitude of 4
#' #2. 25 Hz with amplitude of 4
#' S1 <- 1*sin(2*pi*10*t) + 2*sin(2*pi*25*t);
#' S1 <- S1 + rnorm(length(t)) #Add some noise
#' S1.data.frame <- as.data.frame(cbind(t, S1))
#' colnames(S1.data.frame) <- c("Time", "Signal")
#'
#' #Second signal
#' #1. 5 Hz with amplitude of 2
#' #2. 8 Hz with amplitude of 2
#' S2 <- 2*sin(2*pi*5*t) + 2*sin(2*pi*8*t);
#' S2 <- S2 + rnorm(length(t)) #Add some noise
#' S2.data.frame <- as.data.frame(cbind(t, S2))
#' colnames(S2.data.frame) <- c("Time", "Signal")
#'
#' #Third signal
#' #1. 5 Hz with amplitude of 2
#' #2. 8 Hz with amplitude of 2
#' S3 <- 2*sin(2*pi*5*t) + 2*sin(2*pi*8*t);
#' S3 <- S3 + rnorm(length(t)) #Add some noise
#' S3.data.frame <- as.data.frame(cbind(t, S3))
#' colnames(S3.data.frame) <- c("Time", "Signal")
#'
#' #Add all signals to a List
#' list.of.windows <- list(S1.data.frame, S2.data.frame, S3.data.frame)
#'
#' results <- MakeCompositePSDForAllWindows(list.of.windows, "Signal", Fs, 0, 30, 0.1)
#'
#' frequencies <- results[[1]]
#'
#' averaged.PSD <- results[[2]]
#'
#' stddev.PSD <- results[[3]]
#'
#' dev.new()
#' plot(frequencies, averaged.PSD, type = "l")
#'
#' dev.new()
#' plot(frequencies, averaged.PSD, type = "l")
#' #Add error bars
#' arrows(frequencies, averaged.PSD, frequencies, averaged.PSD + stddev.PSD, length=0.05, angle=90)
#' arrows(frequencies, averaged.PSD, frequencies, averaged.PSD - stddev.PSD, length=0.05, angle=90)
#'
#'
MakeCompositePSDForAllWindows <- function(list.of.windows,
                                          name.of.col.containing.time.series,
                                          sampling_frequency,
                                          x_start = 0,
                                          x_end,
                                          x_increment){

  #Each row of the matrix will be the PSD values for a single window.
  #Each column will correspond to a different frequency.
  #This will be used to calculate the standard deviation of PSD at each frequency.
  captured.PSD.values <- NULL

  #Testing conditions
  # list.of.windows <- windows.amplitude.one.speech
  # name.of.col.containing.time.series <- "PC1"
  # sampling_frequency <- 50
  # x_start <- 0
  # x_end <- 24
  # x_increment <- 0.1

  #The axes for each PSD is slightly different, so we want to
  #interpolate for a given x-axis
  new_x <- seq(x_start, x_end, by = x_increment)

  #Create vector to hold the summed amplitudes.
  summed.amplitudes <- rep(0,length(new_x))

  #Go through all windows
  for(i in 1:length(list.of.windows)){

    print(i)

    single.window <- list.of.windows[[i]]
    single.window.results <- MakePowerSpectralDensity(sampling_frequency, single.window[,name.of.col.containing.time.series])

    single.window.freq <- single.window.results[[1]]
    single.window.amplitude <- single.window.results[[2]]

    #Interpolate every curve so that they contain the same x values. 0 to 24.
    interpolation.res <- stats::approx(x = single.window.freq, y = single.window.amplitude,
                                xout = new_x, method="linear")

    summed.amplitudes <- summed.amplitudes + interpolation.res$y

    captured.PSD.values <- rbind(captured.PSD.values, interpolation.res$y)

  }

  #Take the average of the summed vector to get the average PSD value at
  #each frequency.
  averaged.amplitudes <- summed.amplitudes/length(list.of.windows)

  #Get the standard deviation of the PSD value at each frequency. Std for
  #each column.
  captured.PSD.values <- apply(captured.PSD.values, 2, as.numeric)
  stddev.amplitudes <- apply(captured.PSD.values,2, stats::sd)

  output <- list(new_x, averaged.amplitudes, stddev.amplitudes)
  return(output)

}

#' Find averaged xy plots
#'
#' If there are multiple 2D plots where the range of the x values are the same,
#' then this function can allow you to average the y-values for all of these plots.
#' The increment of the x-values can be different because this function uses interpolation
#' to ensure each window has the same x-axis when the averaging step occurs.
#'
#'
#' @param list.of.windows A list of windows (dataframes). Each window should have the same range of values in the x-axis in order for averaging to work.
#' @param name.of.col.containing.time.series A string that specifies the name of the column in the windows that correspond to the time series that should be used for making averaging.
#' @param x_start Numeric value specifying start of the new x-axis for the averaged PSD. Default is 0, so the first observation in the time series corresponds with x = 0.
#' @param x_end Numeric value specifying end of the new x-axis for the averaged PSD. Maximum value is the sampling_frequency divided by 2.
#' @param x_increment Numeric value specifying increment of the new x-axis for the averaged PSD.
#'
#' @return
#' 1. Vector of x values for plotting. The units will be number of observations. So if the time series has 100 observations and x_increment used is 1, then each tick mark on the x-axis corresponds to one observation unit.
#' 2. Vector of averaged y values after looking at all windows.
#' 3. Vector of standard deviation of y values for each x value.
#'
#'
#' @export
#'
#' @examples
#' #Create a vector of time that represent times where data are sampled.
#' Fs = 100; #sampling frequency in Hz
#' T = 1/Fs; #sampling period
#' L = 1000; #length of time vector
#' t = (0:L-1)*T; #time vector
#'
#' #First signal
#' #1. 1 Hz with amplitude of 4
#' S1 <- 4*sin(2*pi*1*t)
#' S1.data.frame <- as.data.frame(cbind(t, S1))
#' colnames(S1.data.frame) <- c("Time", "Signal")
#'
#' #Second signal
#' #1. 1 Hz with amplitude of -2
#' #2. 2 Hz with amplitude of -2
#' S2 <- (-2)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
#' S2.data.frame <- as.data.frame(cbind(t, S2))
#' colnames(S2.data.frame) <- c("Time", "Signal")
#'
#' #Third signal
#' #1. 1 Hz with amplitude of 2
#' #2. 2 Hz with amplitude of 2
#' S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
#' S3.data.frame <- as.data.frame(cbind(t, S3))
#' colnames(S3.data.frame) <- c("Time", "Signal")
#'
#' #Add all signals to a List
#' list.of.windows <- list(S1.data.frame, S2.data.frame, S3.data.frame)
#'
#' results <- MakeCompositeXYPlotForAllWindows(list.of.windows, "Signal", 0, 999, 1)
#'
#' x.values <- results[[1]]
#'
#' y.values <- results[[2]]
#'
#' stddev.y.values <- results[[3]]
#'
#' #plot each xy plot individually
#' dev.new()
#' plot(t, S1, ylim = c(-5, 5), type = "l")
#' lines(t, S2, col="blue")
#' lines(t, S3, col="green")
#'
#'
#' #plot the averaged plot
#' #The only curve remaining should be the 1Hz with amplitude of 4/3.
#' dev.new()
#' plot(x.values, y.values, type = "l")
#'
#' #plot averaged plot with error bars
#' dev.new()
#' plot(x.values, y.values, type = "l")
#' #Add error bars
#' arrows(x.values, y.values, x.values, y.values + stddev.y.values, length=0.05, angle=90)
#' arrows(x.values, y.values, x.values, y.values - stddev.y.values, length=0.05, angle=90)
#'
#'
MakeCompositeXYPlotForAllWindows <- function(list.of.windows,
                                          name.of.col.containing.time.series,
                                          x_start = 0,
                                          x_end,
                                          x_increment){

  #Each row of the matrix will be the y values for a single window.
  #Each column will correspond to a different x value.
  #This will be used to calculate the standard deviation of y values at each x value
  captured.y.values <- NULL

  #We want to ensure that the axes we are averaging across are the same
  #for all windows.
  #For PD data, 0, 150, 1. Increment is 1, but each increment represents 0.02 seconds.
  new_x <- seq(x_start, x_end, by = x_increment)

  #Create vector to hold the summed y values.
  summed.y <- rep(0,length(new_x))

  #Go through all windows
  for(i in 1:length(list.of.windows)){

    print(i)

    single.window <- list.of.windows[[i]]

    #First observation is 0.
    single.window.x <- 0:(length(single.window[,name.of.col.containing.time.series])-1)
    single.window.y <- single.window[,name.of.col.containing.time.series]

    #Interpolate every curve so that they contain the same x values. 0 to 24.
    interpolation.res <- stats::approx(x = single.window.x, y = single.window.y,
                                       xout = new_x, method="linear")

    summed.y <- summed.y + interpolation.res$y

    captured.y.values <- rbind(captured.y.values, interpolation.res$y)

  }

  #Take the average of the summed vector to get the average PSD value at
  #each frequency.
  averaged.y <- summed.y/length(list.of.windows)

  #Get the standard deviation of the PSD value at each frequency. Std for
  #each column.
  captured.y.values <- apply(captured.y.values, 2, as.numeric)
  stddev.y <- apply(captured.y.values,2, stats::sd)

  output <- list(new_x, averaged.y, stddev.y)
  return(output)

}



##This is the function to use to quickly make plots. To adjust the plots
#for specific formatting, then use the above functions to get values to plot
#and then use your own plotting code.
#Output the ggplot object.

#Outputs two objects.
#1. An automatically generated ggplot
#2. The values for each line to be plotting. The values can be used to remake ggplot. This is good if the color/axes/etc. need to be changed.



#' Title
#'
#' @param list.of.windows A list of windows (dataframes).
#' @param name.of.col.containing.time.series A string that specifies the name of the column in the windows that correspond to the time series that should be used.
#' @param x_start Numeric value specifying start of the new x-axis. Default is 0.
#' @param x_end Numeric value specifying end of the new x-axis. For PSD, maximum value is the sampling_frequency divided by 2.
#' @param x_increment Numeric value specifying increment of the new x-axis.
#' @param level1.column.name A String that specifies the column name to use for the first level. This column should only contain one unique value within each window.
#' @param level2.column.name A String that specifies the column name to use for the second level. This column should only contain one unique value within each window.
#' @param level.combinations A List containing Lists. Each list that it contains has two vectors. The first vector specifying the values for level1 and the second vector specifying the values for level2. Each list element will correspond to a new line on the plot.
#' @param level.combinations.labels A vector of strings that labels each combination. This is used for making the figure legend.
#' @param plot.title String for title of plot.
#' @param plot.xlab String for x-axis of plot.
#' @param plot.ylab String for y-axis of plot.
#' @param combination.index.for.envelope A numeric value that specifies which combination (index of level.combinations) should have a line with an error envelope. The default is no envelope.
#' @param TimeSeries.PSD.LogPSD A String with 3 possible values to specify what type of plot to create from the time series: 1. "TimeSeries", 2. "PSD", 3. "LogPSD"
#' @param sampling_frequency Numeric value used for specifying sampling frequency if PSD or LogPSD is made with this function. Default is NULL because default plot created is a time series plot.
#'
#' @return A List with two objects:
#' 1. A List of dataframes containing values for each line on the plot.
#' 2. A ggplot object that can be plotted right away.
#'
#' @export
#'
#' @examples
#'
#' #I want to create a plot that shows two curves:
#' #1. Composite of time series signals 1, 2, and 3.
#' #2. Composite of time series signals 3 and 4.
#'
#' #Create a vector of time that represent times where data are sampled.
#' Fs = 100; #sampling frequency in Hz
#' T = 1/Fs; #sampling period
#' L = 1000; #length of time vector
#' t = (0:L-1)*T; #time vector
#'
#' #First signal
#' #1. 1 Hz with amplitude of 2
#' S1 <- 2*sin(2*pi*1*t)
#' level1.vals <- rep("a", length(S1))
#' level2.vals <- rep("1", length(S1))
#' S1.data.frame <- as.data.frame(cbind(t, S1, level1.vals, level2.vals))
#' colnames(S1.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#'
#' #Second signal
#' #1. 1 Hz with amplitude of -4
#' #2. 2 Hz with amplitude of -2
#' S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S2))
#' level2.vals <- rep("2", length(S2))
#' S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
#' colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#'
#' #Third signal
#' #1. 1 Hz with amplitude of 2
#' #2. 2 Hz with amplitude of 2
#' S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S3))
#' level2.vals <- rep("3", length(S3))
#' S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
#' colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#'
#' #Fourth signal
#' #1. 1 Hz with amplitude of 2
#' S4 <- 2*sin(2*pi*1*t)
#' level1.vals <- rep("b", length(S4))
#' level2.vals <- rep("3", length(S4))
#' S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
#' colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#'
#' windows <- list(S1.data.frame, S2.data.frame, S3.data.frame, S4.data.frame)
#'
#' #Gets the composite of the first, second, and third signal. Should result in a flat signal.
#' FirstComboToUse <- list( c("a"), c(1, 2, 3) )
#'
#' #Gets the composite of the third and fourth signal
#' SecondComboToUse <- list( c("a", "b"), c(3) )
#'
#' timeseries.results <- AutomatedCompositePlotting(list.of.windows = windows,
#'                            name.of.col.containing.time.series = "Signal",
#'                            x_start = 0,
#'                            x_end = 999,
#'                            x_increment = 1,
#'                            level1.column.name = "level1.ID",
#'                            level2.column.name = "level2.ID",
#'                            level.combinations = list(FirstComboToUse, SecondComboToUse),
#'                            level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
#'                            plot.title = "Example",
#'                            plot.xlab = "Time",
#'                            plot.ylab = "Original units",
#'                            combination.index.for.envelope = NULL,
#'                            TimeSeries.PSD.LogPSD = "TimeSeries",
#'                            sampling_frequency = NULL)
#'
#' ggplot.obj <- timeseries.results[[2]]
#'
#' dev.new()
#' ggplot.obj
#'
AutomatedCompositePlotting <- function(list.of.windows,
                           name.of.col.containing.time.series,
                           x_start = 0,
                           x_end,
                           x_increment,
                           level1.column.name,
                           level2.column.name,
                           level.combinations,
                           level.combinations.labels,
                           plot.title,
                           plot.xlab,
                           plot.ylab,
                           combination.index.for.envelope = NULL,
                           TimeSeries.PSD.LogPSD = "TimeSeries",
                           sampling_frequency = NULL){


  #Each object in this list contains the x and y values for a line that should
  #appear in the plot
  list.of.values.to.plot <- list()
  list.of.dataframes.to.plot <- list()

  #------------------------------------------------------------------------------
  # Generate values to plot for each composite line specified by objects in the
  # level.combinations list
  #------------------------------------------------------------------------------

  #The combination specifies the window subsets to use for each composite line.
  #For each combination, generate the values to plot for the composite line.

  for(i in 1:length(level.combinations)){

    level1.categories.to.use <-  level.combinations[[i]][[1]]
    level2.categories.to.use <- level.combinations[[i]][[2]]

    subset.windows <- GetSubsetOfWindowsTwoLevels(list.of.windows, level1.column.name, level2.column.name,
                                level1.categories.to.use, level2.categories.to.use)


    ##SWITCH HERE FOR XY, PSD, LOG
    if(TimeSeries.PSD.LogPSD == "TimeSeries"){

      list.of.values.to.plot[[i]] <- MakeCompositeXYPlotForAllWindows(subset.windows,
                                                                      name.of.col.containing.time.series,
                                                                      x_start = x_start,
                                                                      x_end = x_end,
                                                                      x_increment = x_increment)
    } else if(TimeSeries.PSD.LogPSD == "PSD"){

      list.of.values.to.plot[[i]] <- MakeCompositePSDForAllWindows(subset.windows,
                                                                   name.of.col.containing.time.series,
                                                                   sampling_frequency,
                                                                   x_start = 0,
                                                                   x_end,
                                                                   x_increment)
    } else if(TimeSeries.PSD.LogPSD == "LogPSD"){

      list.of.values.to.plot[[i]] <- MakeCompositePSDForAllWindows(subset.windows,
                                                                   name.of.col.containing.time.series,
                                                                   sampling_frequency,
                                                                   x_start = 0,
                                                                   x_end,
                                                                   x_increment)

      list.of.values.to.plot[[i]][[2]] <- log(list.of.values.to.plot[[i]][[2]])

      list.of.values.to.plot[[i]][[3]] <- log(list.of.values.to.plot[[i]][[3]])


    } else{

      stop("Invalid input for argument: TimeSeries.PSD.LogPSD")

    }


    data.temp <- as.data.frame(cbind(list.of.values.to.plot[[i]][[1]], list.of.values.to.plot[[i]][[2]], list.of.values.to.plot[[i]][[3]]))
    colnames(data.temp) <- c("xvals", "yvals", "ystddev")
    list.of.dataframes.to.plot[[i]] <- data.temp

  }

  #------------------------------------------------------------------------------
  # Create ggplot
  #------------------------------------------------------------------------------

  ggplot.object <- ggplot(list.of.dataframes.to.plot[[1]], aes(x=xvals, y=yvals))

  #Add error envelope to plot. Envelope needs to be added before lines are added
  #otherwise the envelope will cover the lines.
  if(!is.null(combination.index.for.envelope)){

    ggplot.object <- ggplot.object +
      geom_ribbon(data = list.of.dataframes.to.plot[[combination.index.for.envelope]], aes(ymin=yvals-ystddev, ymax=yvals+ystddev), fill = "grey70")

  }

  #Add lines to plot
  for(i in 1:length(level.combinations)){

    ggplot.object <- ggplot.object +
                     geom_line(data = list.of.dataframes.to.plot[[i]], aes(x=xvals, y=yvals, color = level.combinations.labels[[i]]))


  }

  #Add legend to plot
  my.colors <- c("blue", "red", "black", "green")

  values.to.use <- NULL

  ##LEFT OFF HERE##############################################################################################
  #https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot
  #Use paste or something to get "=" into the expression.

  ggplot.object <- ggplot.object +
                   scale_colour_manual("",
                        breaks = level.combinations.labels,
                        values = values.to.use)

  #Add title and axes to plot
  ggplot.object <- ggplot.object +
                   ggtitle(plot.title) +
                   xlab(plot.xlab) +
                   ylab(plot.ylab)


  #------------------------------------------------------------------------------
  # Return values to plot for each line as well as the ggplot object
  #------------------------------------------------------------------------------

  results <- list(list.of.dataframes.to.plot, ggplot.object)

  return(results)


  #Testing: PlotTimeSeries(windows, "SummedXYZ", 0, 150, 1, "RTR_AMP", "TASK_ID", list( list(c(1, 2, 3), c(task.names)) ), "combined", "Combined amplitudes and task into 1", "Time", "Acceleration", 1)
}

