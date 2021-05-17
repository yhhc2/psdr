

#' Create a one sided amplitude spectrum using time series data
#'
#' Code was referenced from here: https://www.mathworks.com/help/matlab/ref/fft.html
#'
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param data_vector Vector of numeric values. Time series vector of data.
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
#' @param data_vector Vector of numeric values. Time series vector of data.
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

  X <- as.numeric(data_vector)

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

  if(length(list.of.windows) > 1){

    stddev.amplitudes <- apply(captured.PSD.values,2, stats::sd)

  } else{

    stddev.amplitudes <- 0

  }


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

  if(length(list.of.windows) > 1){

    stddev.y <- apply(captured.y.values,2, stats::sd)

  } else{

    stddev.y <- 0

  }

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



#' Automated plotting of time series, PSD, and log transformed PSD.
#'
#' This function uses a lot of the functions in this package (psdr) to automate the
#' plotting process for plotting composite curves and having multiple curves
#' on the same plot.
#'
#' Given a list of windows, you can specify which windows you want to average together
#' to form a curve on the plot. You can specify multiple combos and therefore multiple
#' curves can be plotted on the same plot with a legend to specify the combo
#' used to create each curve. An error envelope can also be created for a single curve
#' on the plot.
#'
#' The function automatically generates a ggplot
#' for easy plotting. However, the function also outputs dataframes for each combo.
#' Each dataframe has 3 columns:
#'
#' 1. X value: For timeseries, this will be in the original units that separates
#' each observation in the time series. For example, if there are 150 observations
#' and each observation is 0.02 seconds apart, then if 150 observations are
#' specified as the x_increment, then each observation are still 0.02 seconds.
#' The time difference between the first and last observation needs to equal the
#' time difference between the first and last observation in the original time series.
#' For PSD and LogPSd, the units will be in Hz (frequency). The frequency range
#' depends on the sampling frequency. Smallest frequency is 0 and largest frequency
#' is sampling_frequency/2.
#'
#' 2. Y value: For time series, this will be in the original units of the time series.
#' For PSD, the units will be (original units)^2/Hz, for LogPSD, the units will be
#' log((original units)^2/Hz)).
#'
#' 3. Standard deviation of Y value. This can be used to plot error bars
#' or error envelopes to see the spread of the windows used to make the composite.
#'
#' Three different plots can be created:
#' 1. Time series plot. This simply takes the time series in the windows, averages them for each combo, and then plots the composite curve for each combo.
#' 2. PSD plot. This takes the time series in the windows and given the sampling frequency, it calculates the PSD. It averages the PSD for the windows in each combo, and then plots the composite curve for each combo.
#' 3. Log transformed PSD plot. Same as PSD plot except at the end, the composite PSD curves are log transformed.
#'
#' @param list.of.windows A list of windows (dataframes). All windows should have similar lengths
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
#' @param my.colors A vector of strings that specify the color for each line. 9 default values are used.
#'
#' @return A List with two objects:
#' 1. A List of dataframes containing values for each line on the plot. The order of the dataframes correspond to the order of the combinations in level.combinations.
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
#' S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
#'
#' #Second signal
#' #1. 1 Hz with amplitude of -4
#' #2. 2 Hz with amplitude of -2
#' S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S2))
#' level2.vals <- rep("2", length(S2))
#' S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
#' colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
#'
#' #Third signal
#' #1. 1 Hz with amplitude of 2
#' #2. 2 Hz with amplitude of 2
#' S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S3))
#' level2.vals <- rep("3", length(S3))
#' S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
#' colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
#'
#' #Fourth signal
#' #1. 1 Hz with amplitude of -2
#' S4 <- -2*sin(2*pi*1*t)
#' level1.vals <- rep("b", length(S4))
#' level2.vals <- rep("3", length(S4))
#' S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
#' colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
#'
#' windows <- list(S1.data.frame, S2.data.frame, S3.data.frame, S4.data.frame)
#'
#' #Gets the composite of the first, second, and third signal. Should result in a flat signal.
#' FirstComboToUse <- list( c("a"), c(1, 2, 3) )
#'
#' #Gets the composite of the third and fourth signal
#' SecondComboToUse <- list( c("a", "b"), c(3) )
#'
#'
#' #Timeseries-----------------------------------------------------------------
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
#' ggplot.obj.timeseries <- timeseries.results[[2]]
#'
#' #Plot. Will see the 1+2+3 curve as a flat line. The 3+4 curve will only have 2 Hz.
#' dev.new()
#' ggplot.obj.timeseries
#'
#' #PSD-------------------------------------------------------------------------
#'
#' #Note that the PSDs are not generated directly from the "Signal 1 + 2 + 3" and
#' #the "Signal 3 + 4" time series. Instead, PSDs are generated individually
#' #for signals 1, 2, 3, and 4, and then then are summed together.
#'
#' PSD.results <- AutomatedCompositePlotting(list.of.windows = windows,
#'                            name.of.col.containing.time.series = "Signal",
#'                            x_start = 0,
#'                            x_end = 50,
#'                            x_increment = 0.01,
#'                            level1.column.name = "level1.ID",
#'                            level2.column.name = "level2.ID",
#'                            level.combinations = list(FirstComboToUse, SecondComboToUse),
#'                            level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
#'                            plot.title = "Example",
#'                            plot.xlab = "Hz",
#'                            plot.ylab = "(Original units)^2/Hz",
#'                            combination.index.for.envelope = 2,
#'                            TimeSeries.PSD.LogPSD = "PSD",
#'                            sampling_frequency = 100)
#'
#' ggplot.obj.PSD <- PSD.results[[2]]
#'
#' #Plot. For both plots, two peaks will be present, 1 Hz and 2 Hz. 1 Hz should be
#' #stronger in both cases because more signals have this frequency (even if amp is negative).
#' #Error envelope is specified for the second (red) curve. Envelope should only
#' #be present for 2 Hz signal.
#' dev.new()
#' ggplot.obj.PSD
#'
#' #PSD Zoomed in---------------------------------------------------------------
#'
#' PSD.results <- AutomatedCompositePlotting(list.of.windows = windows,
#'                            name.of.col.containing.time.series = "Signal",
#'                            x_start = 0,
#'                            x_end = 5,
#'                            x_increment = 0.01,
#'                            level1.column.name = "level1.ID",
#'                            level2.column.name = "level2.ID",
#'                            level.combinations = list(FirstComboToUse, SecondComboToUse),
#'                            level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
#'                            plot.title = "Example",
#'                            plot.xlab = "Hz",
#'                            plot.ylab = "(Original units)^2/Hz",
#'                            combination.index.for.envelope = 2,
#'                            TimeSeries.PSD.LogPSD = "PSD",
#'                            sampling_frequency = 100)
#'
#' ggplot.obj.PSD <- PSD.results[[2]]
#'
#' #Plot. For both plots, two peaks will be present, 1 Hz and 2 Hz. 1 Hz should be
#' #stronger in both cases because more signals have this frequency (even if amp is negative).
#' #Error envelope is specified for the second (red) curve. Envelope should only
#' #be present for 1 Hz signal.
#' dev.new()
#' ggplot.obj.PSD
#'
#' #LogPSD-------------------------------------------------------------------------
#'
#' LogPSD.results <- AutomatedCompositePlotting(list.of.windows = windows,
#'                            name.of.col.containing.time.series = "Signal",
#'                            x_start = 0,
#'                            x_end = 50,
#'                            x_increment = 0.01,
#'                            level1.column.name = "level1.ID",
#'                            level2.column.name = "level2.ID",
#'                            level.combinations = list(FirstComboToUse, SecondComboToUse),
#'                            level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
#'                            plot.title = "Example",
#'                            plot.xlab = "Hz",
#'                            plot.ylab = "log((Original units)^2/Hz)",
#'                            combination.index.for.envelope = NULL,
#'                            TimeSeries.PSD.LogPSD = "LogPSD",
#'                            sampling_frequency = 100)
#'
#' ggplot.obj.LogPSD <- LogPSD.results[[2]]
#'
#' #Plot. For both plots, two peaks will be present, 1 Hz and 2 Hz. 1 Hz should
#' #be stronger in both cases because more signals have this frequency (even if amp is negative).
#' #Error envelope is specified for the second (red) curve. Envelope should only
#' #be present for 2 Hz signal.
#' dev.new()
#' ggplot.obj.LogPSD
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
                           sampling_frequency = NULL,
                           my.colors = c("blue", "red", "black", "green", "gold", "darkorchid1", "brown", "deeppink", "gray")){

  # #Testing conditions
  # list.of.windows = windows
  # name.of.col.containing.time.series = "Signal"
  # x_start = 0
  # x_end = 999
  # x_increment = 1
  # level1.column.name = "level1.ID"
  # level2.column.name = "level2.ID"
  # level.combinations = list(FirstComboToUse, SecondComboToUse)
  # level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4")
  # plot.title = "Example"
  # plot.xlab = "Time"
  # plot.ylab = "Original units"
  # combination.index.for.envelope = NULL
  # TimeSeries.PSD.LogPSD = "TimeSeries"
  # sampling_frequency = NULL

  # #Testing conditions
  # list.of.windows = windows
  # name.of.col.containing.time.series = "Signal"
  # x_start = 0
  # x_end = 50
  # x_increment = 0.01
  # level1.column.name = "level1.ID"
  # level2.column.name = "level2.ID"
  # level.combinations = list(FirstComboToUse, SecondComboToUse)
  # level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4")
  # plot.title = "Example"
  # plot.xlab = "Hz"
  # plot.ylab = "(Original units)^2/Hz"
  # combination.index.for.envelope = NULL
  # TimeSeries.PSD.LogPSD = "PSD"
  # sampling_frequency = NULL

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
                                                                   x_start = x_start,
                                                                   x_end,
                                                                   x_increment)
    } else if(TimeSeries.PSD.LogPSD == "LogPSD"){

      list.of.values.to.plot[[i]] <- MakeCompositePSDForAllWindows(subset.windows,
                                                                   name.of.col.containing.time.series,
                                                                   sampling_frequency,
                                                                   x_start = x_start,
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

  ggplot.object <- ggplot2::ggplot(list.of.dataframes.to.plot[[1]], ggplot2::aes(x=xvals, y=yvals))

  #Remove white space from all combination labels. This is needed for the legend to work.
  for(i in 1:length(level.combinations.labels)){

    x <- level.combinations.labels[[i]]
    level.combinations.labels[[i]] <- gsub(" ", "", x, fixed = TRUE)

  }


  #Add error envelope to plot. Envelope needs to be added before lines are added
  #otherwise the envelope will cover the lines.
  if(!is.null(combination.index.for.envelope)){

    ggplot.object <- ggplot.object +
                     ggplot2::geom_ribbon(data = list.of.dataframes.to.plot[[combination.index.for.envelope]], ggplot2::aes(ymin=yvals-ystddev, ymax=yvals+ystddev), fill = "grey70")

  }

  #Add lines to plot
  #i will not be evaluated if placed inside ggplot2 aes(). aes() stores expression. LEFT HERE
  #https://stackoverflow.com/questions/32698616/ggplot2-adding-lines-in-a-loop-and-retaining-colour-mappings
  #Need to use aes_() https://stackoverflow.com/questions/39021021/force-ggplot-to-evaluate-counter-variable
  for(i in 1:length(level.combinations)){

    ggplot.object <- ggplot.object +
                     ggplot2::geom_line(data = list.of.dataframes.to.plot[[i]], ggplot2::aes_(x=quote(xvals), y=quote(yvals), color = level.combinations.labels[[i]]))



  }

  #Add legend to plot
  #my.colors <- c("blue", "red", "black", "green", "gold", "darkorchid1", "brown", "pink", "gray")

  # #https://stackoverflow.com/questions/10349206/add-legend-to-ggplot2-line-plot
  # values.to.use <- NULL
  # for(i in 1:length(level.combinations.labels)){
  #
  #   x <- paste("\"", level.combinations.labels[[i]], "\"", "=", "\"", my.colors[[i]], "\"")
  #
  #   values.to.use[[i]] <- gsub(" ", "", x, fixed = TRUE)
  #
  # }

  named.colors <- my.colors[1:length(level.combinations.labels)]
  names(named.colors) <- level.combinations.labels

  ggplot.object <- ggplot.object +
                   ggplot2::scale_colour_manual("",
                        breaks = level.combinations.labels,
                        values = named.colors)

  # ggplot.object <- ggplot.object +
  #   ggplot2::scale_colour_manual("",
  #                                values = unlist(values.to.use))

  # ggplot.object <- ggplot.object +
  #                  ggplot2::scale_colour_manual("",
  #                       breaks = level.combinations.labels,
  #                       values = c("Signal1+2+3"="blue", "Signal3+4"="red"))

  #values = c("Signal1+2+3"="blue", "Signal3+4"="red") #this expression just creates a named vector.


  #Add title and axes to plot
  ggplot.object <- ggplot.object +
                   ggplot2::ggtitle(plot.title) +
                   ggplot2::xlab(plot.xlab) +
                   ggplot2::ylab(plot.ylab)


  #------------------------------------------------------------------------------
  # Return values to plot for each line as well as the ggplot object
  #------------------------------------------------------------------------------

  results <- list(list.of.dataframes.to.plot, ggplot.object)

  return(results)


  #Testing: PlotTimeSeries(windows, "SummedXYZ", 0, 150, 1, "RTR_AMP", "TASK_ID", list( list(c(1, 2, 3), c(task.names)) ), "combined", "Combined amplitudes and task into 1", "Time", "Acceleration", 1)
}



#' Given a time series vector, generate a PSD, then calculate integration for specified bins
#'
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param data_vector Vector of numeric values. Timeseries vector of data.
#' @param frequency_bins A list of objects where each object is a vector with two elements. The first element is a numeric value specifying the start frequency of a bin. The second element is a numeric value specifying the end frequency of a bin. Each object corresponds to a new frequency bin for calculating integral. For integration, approxfun is used, so increment does not need to be specified.
#'
#' @return A list where each object is also a list. The nested list objects have the first element specifying the bin boundaries. The second element specifies the integral.
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
#' results <- MakePowerSpectralDensity(Fs, S)
#'
#' frequencies <- results[[1]]
#'
#' PSD <- results[[2]]
#'
#' dev.new()
#' plot(frequencies, PSD, type = "l")
#'
#' bins <- list(
#' c(9, 11),
#' c(24,26),
#' c(9,26),
#' c(30,40)
#' )
#'
#' integration.results <- PSDIntegrationPerFreqBin(Fs, S, bins)
#'
#' for(i in 1:length(integration.results)){
#'
#'    message <- paste("Area in bin ", integration.results[[i]][[1]], " is ",
#'                    integration.results[[i]][[2]])
#'
#'    print(message)
#'
#' }
#'
PSDIntegrationPerFreqBin <- function(sampling_frequency, data_vector, frequency_bins){


  #This will hold the final results to output. A list where each object is also
  #a list. The nested list objects have the first element specifying the bin
  #boundaries. The second element specifies the integral.
  results <- list()

  single.window.results <- MakePowerSpectralDensity(sampling_frequency, data_vector)

  single.window.freq <- single.window.results[[1]]
  single.window.amplitude <- single.window.results[[2]]

  #Interpolate every curve so that they contain the same x values. 0 to 24.
  interpolation.res.function <- stats::approxfun(x = single.window.freq, y = single.window.amplitude,
                                              method="linear")

  #Divide the frequency into bins. For each bin, calculate the integral.
  for(i in 1:length(frequency_bins)){

    bin.start <- frequency_bins[[i]][[1]]
    bin.end <- frequency_bins[[i]][[2]]

    #Integration for bin
    integration.res <- stats::integrate(interpolation.res.function, bin.start, bin.end)

    bin.label <- paste(bin.start, "-", bin.end)

    results[[i]] <- list(bin.label, integration.res$value)

  }

  return(results)

}



#' Given a x, y, plot. Find the maximum value on the plot.
#'
#' To generate a curve of points, interpolation is used
#' and the range and increment can be specified. Will output
#' a message if multiple maxima are detected.
#'
#' @param x_vector A numerical vector with x coordinates.
#' @param y_vector A numerical vector with y coordinates.
#' @param x_start Numeric value specifying start of x value to look at.
#' @param x_end Numeric value specifying end of x value to look at.
#' @param x_increment Numeric value specifying the increment of the x-values to use.
#'
#' @return A vector with two elements, The first element is the x value
#' where the max y value is found. The second element is the max y value.
#'
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
#' S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
#'
#' #Second signal
#' #1. 1 Hz with amplitude of -4
#' #2. 2 Hz with amplitude of -2
#' S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S2))
#' level2.vals <- rep("2", length(S2))
#' S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
#' colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
#'
#' #Third signal
#' #1. 1 Hz with amplitude of 2
#' #2. 2 Hz with amplitude of 2
#' S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S3))
#' level2.vals <- rep("3", length(S3))
#' S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
#' colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
#'
#' #Fourth signal
#' #1. 1 Hz with amplitude of -2
#' S4 <- -2*sin(2*pi*1*t)
#' level1.vals <- rep("b", length(S4))
#' level2.vals <- rep("3", length(S4))
#' S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
#' colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
#'
#' #Extra representation of S2 dataframe to show an example that has enough samples
#' #to have significance for Kruskal-Wallis test
#' windows <- list(S1.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame,
#' S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S3.data.frame,
#' S4.data.frame)
#'
#' #Gets the composite of the first, second, and third signal. Should result in a flat signal.
#' FirstComboToUse <- list( c("a"), c(1, 2, 3) )
#'
#' #Gets the composite of the third and fourth signal
#' SecondComboToUse <- list( c("a", "b"), c(3) )
#'
#'
#' #PSD-------------------------------------------------------------------------
#'
#' PSD.results <- AutomatedCompositePlotting(list.of.windows = windows,
#'                            name.of.col.containing.time.series = "Signal",
#'                            x_start = 0,
#'                            x_end = 10,
#'                            x_increment = 0.01,
#'                            level1.column.name = "level1.ID",
#'                            level2.column.name = "level2.ID",
#'                            level.combinations = list(FirstComboToUse, SecondComboToUse),
#'                            level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
#'                            plot.title = "Example",
#'                            plot.xlab = "Hz",
#'                            plot.ylab = "(Original units)^2/Hz",
#'                            combination.index.for.envelope = 2,
#'                            TimeSeries.PSD.LogPSD = "PSD",
#'                            sampling_frequency = 100)
#'
#' ggplot.obj.PSD <- PSD.results[[2]]
#'
#' dataframes.plotted <- PSD.results[[1]]
#'
#' first.curve <- dataframes.plotted[[1]]
#'
#' second.curve <- dataframes.plotted[[2]]
#'
#' first.curve.max <- IdentifyMaxOnXY(first.curve$xvals, first.curve$yvals, 0, 10, 0.01)
#' first.curve.max.limited <- IdentifyMaxOnXY(first.curve$xvals, first.curve$yvals, 1.25, 2.5, 0.01)
#'
#' second.curve.max <- IdentifyMaxOnXY(second.curve$xvals, second.curve$yvals, 0, 10, 0.01)
#'
#'
IdentifyMaxOnXY <- function(x_vector, y_vector, x_start = 0,
                                         x_end,
                                         x_increment){

  #The axes for each PSD is slightly different, so we want to
  #interpolate for a given x-axis
  new_x <- seq(x_start, x_end, by = x_increment)

  #Interpolate every curve so that they contain the same x values. 0 to 24.
  interpolation.res <- stats::approx(x = x_vector, y = y_vector,
                                     xout = new_x, method="linear")

  xval <- interpolation.res$x
  yval <- interpolation.res$y

  interpolation.res.dataframe <- data.frame(xval, yval)

  #Identify row with maximum amplitude
  results <- interpolation.res.dataframe[which.max(interpolation.res.dataframe$yval),]

  if(length(results) > 2){

    print("Multiple maxima detected")

  }

  return(results)

}



#' Given a time series vector, create a PSD and find the dominant frequency.
#'
#' The range to look for a dominant frequency (frequency corresponding to max
#' PSD value) should be specified for this function. This function uses the
#' MakePowerSpectralDensity() function and the IdentifyMaxOnXY() function
#' together. If multiple equal maxima are found, then IdentifyMaxOnXY()
#' will display a message.
#'
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param data_vector Vector of numeric values. Timeseries vector of data.
#' @param x_start Numeric value specifying start of x value to look at.
#' @param x_end Numeric value specifying end of x value to look at.
#' @param x_increment Numeric value specifying the increment of the x-values to use.
#'
#' @return A vector with two elements, The first element is the x value (frequency)
#' where the max y value (PSD value) is found. The second element is the max y value.
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
#' #First signal
#' #1. 1 Hz with amplitude of 2
#' S1 <- 2*sin(2*pi*1*t)
#' level1.vals <- rep("a", length(S1))
#' level2.vals <- rep("1", length(S1))
#' S1.data.frame <- as.data.frame(cbind(t, S1, level1.vals, level2.vals))
#' colnames(S1.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
#'
#'
#' results <- PSDIdentifyDominantFrequency(Fs, S1.data.frame[,"Signal"], 0, 10, 0.01)
#'
PSDIdentifyDominantFrequency <- function(sampling_frequency, data_vector, x_start = 0,
                                         x_end,
                                         x_increment){


  single.window.results <- MakePowerSpectralDensity(sampling_frequency, data_vector)

  single.window.freq <- single.window.results[[1]]
  single.window.amplitude <- single.window.results[[2]]

  results <- IdentifyMaxOnXY(single.window.freq, single.window.amplitude, x_start = x_start,
                              x_end = x_end,
                              x_increment = x_increment)

  return(results)

}


#-------------------------------------------------------------------------------
#Use the integration and dominant frequency finder functions to work on multiple
#windows

#' Calculate integral for multiple PSDs for a single frequency bin
#'
#' @param list.of.windows A list of windows (dataframes).
#' @param name.of.col.containing.time.series A string that specifies the name of the column in the windows that correspond to the time series that should be used for making PSD.
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param single.bin.boundary A numeric vector with two elements. First element is the start frequency for the bin. Second element is the end frequency of the bin.  For integration, approxfun is used, so increment does not need to be specified.
#'
#' @return A vector where each element is the integration result of each window.
#'
#' @export
#'
#' @examples
#'
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
#' S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
#'
#' #Second signal
#' #1. 1 Hz with amplitude of -4
#' #2. 2 Hz with amplitude of -2
#' S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S2))
#' level2.vals <- rep("2", length(S2))
#' S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
#' colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
#'
#' #Third signal
#' #1. 1 Hz with amplitude of 2
#' #2. 2 Hz with amplitude of 2
#' S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S3))
#' level2.vals <- rep("3", length(S3))
#' S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
#' colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
#'
#' #Fourth signal
#' #1. 1 Hz with amplitude of -2
#' S4 <- -2*sin(2*pi*1*t)
#' level1.vals <- rep("b", length(S4))
#' level2.vals <- rep("3", length(S4))
#' S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
#' colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
#'
#' windows <- list(S1.data.frame, S2.data.frame, S3.data.frame, S4.data.frame)
#'
#'
#' results <- SingleBinPSDIntegrationForMultipleWindows(windows, "Signal", Fs, c(0,2))
#'
#'
SingleBinPSDIntegrationForMultipleWindows <- function(list.of.windows,
                                                      name.of.col.containing.time.series,
                                                      sampling_frequency,
                                                      single.bin.boundary){

  #Each row of the matrix will be the PSD values for a single window.
  #Each column will correspond to a different frequency.
  #This will be used to calculate the standard deviation of PSD at each frequency.
  captured.integration.values <- NULL

  #Go through all windows
  for(i in 1:length(list.of.windows)){

    print(i)

    single.window <- list.of.windows[[i]]

    integration.results <- PSDIntegrationPerFreqBin(sampling_frequency, single.window[,name.of.col.containing.time.series],
                                                    list(single.bin.boundary))

    integration.value <- integration.results[[1]][[2]]

    captured.integration.values <- c(captured.integration.values, integration.value)

  }

  return(captured.integration.values)

}



#' Calculate dominant frequency for multiple PSDs for a single frequency range
#'
#' @param list.of.windows A list of windows (dataframes).
#' @param name.of.col.containing.time.series A string that specifies the name of the column in the windows that correspond to the time series that should be used for making PSD.
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param x_start Numeric value specifying start of x value (frequency) to look at.
#' @param x_end Numeric value specifying end of x value to look at.
#' @param x_increment Numeric value specifying the increment of the x-values to use.
#'
#'
#' @return A vector where each element is the dominant frequency of each window.
#'
#' @export
#'
#' @examples
#'
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
#' S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
#'
#' #Second signal
#' #1. 1 Hz with amplitude of -4
#' #2. 2 Hz with amplitude of -2
#' S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S2))
#' level2.vals <- rep("2", length(S2))
#' S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
#' colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
#'
#' #Third signal
#' #1. 1 Hz with amplitude of 2
#' #2. 2 Hz with amplitude of 2
#' S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S3))
#' level2.vals <- rep("3", length(S3))
#' S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
#' colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
#'
#' #Fourth signal
#' #1. 1 Hz with amplitude of -2
#' S4 <- -2*sin(2*pi*1*t)
#' level1.vals <- rep("b", length(S4))
#' level2.vals <- rep("3", length(S4))
#' S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
#' colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
#'
#' windows <- list(S1.data.frame, S2.data.frame, S3.data.frame, S4.data.frame)
#'
#'
#' results <- PSDDominantFrequencyForMultipleWindows(windows, "Signal", Fs, 0, 10, 0.01)
#'
#'
PSDDominantFrequencyForMultipleWindows <- function(list.of.windows,
                                                      name.of.col.containing.time.series,
                                                      sampling_frequency,
                                                   x_start, x_end, x_increment){

  #Each row of the matrix will be the PSD values for a single window.
  #Each column will correspond to a different frequency.
  #This will be used to calculate the standard deviation of PSD at each frequency.
  captured.dominant.freq.values <- NULL

  #Go through all windows
  for(i in 1:length(list.of.windows)){

    print(i)

    single.window <- list.of.windows[[i]]


    dominant.freq.results <- PSDIdentifyDominantFrequency(sampling_frequency,
                                                        single.window[,name.of.col.containing.time.series],
                                                        x_start = x_start,
                                                        x_end = x_end,
                                                        x_increment = x_increment)

    dominant.freq.value <- dominant.freq.results[1,1]

    captured.dominant.freq.values <- c(captured.dominant.freq.values, dominant.freq.value)

  }

  return(captured.dominant.freq.values)

}



#' Given sets of windows corresponding to different combos, see if the integration
#' or dominant frequency of a specific frequency range is significantly different between the combos
#'
#' Just for a single frequency bin: For Each combination in level.combinations,
#' generate the integral or dominant frequency for each
#' window of each combination. At this point, we should have vectors of
#' integrals or dominant frequency with each vector
#' corresponding to a different combo. Now we want to see if the integrals or dominant frequencies
#' in each combo significantly differ from the other combos. Kruskal-Wallis test is used
#' as a non-parametric ANOVA test to see if the combos have integrals or dominant frequencies that
#' are significantly different.
#'
#' Need to specify whether to compare integral or dominant frequency:
#' If integral (total power in frequency bin) is the value to compare, then
#' SingleBinPSDIntegrationForMultipleWindows() is used. If dominant frequency (
#' frequency corresponding to max PSD value in frequency bin) is the value to
#' compare, then PSDDominantFrequencyForMultipleWindows() is used.
#'
#' @param list.of.windows A list of windows (dataframes).
#' @param name.of.col.containing.time.series A string that specifies the name of the column in the windows that correspond to the time series that should be used for making PSD.
#' @param level1.column.name A String that specifies the column name to use for the first level. This column should only contain one unique value within each window.
#' @param level2.column.name A String that specifies the column name to use for the second level. This column should only contain one unique value within each window.
#' @param level.combinations A List containing Lists. Each list that it contains has two vectors. The first vector specifying the values for level1 and the second vector specifying the values for level2. Each list element will correspond to a new line on the plot.
#' @param level.combinations.labels A vector of strings that labels each combination. This is used for naming the groups in integrals.with.combo.labels
#' @param sampling_frequency Numeric value specifying sampling frequency in hertz. If data is sampled once every second, then sampling frequency is 1 Hz. If data is sampled once every 2 seconds, then sampling frequency is 0.5 Hz.
#' @param single.bin.boundary A numeric vector with two elements. First element is the start frequency for the bin. Second element is the end frequency of the bin.
#' @param x_start Numeric value specifying start of the new x-axis for the averaged PSD. Default is 0 Hz.
#' @param x_end Numeric value specifying end of the new x-axis for the averaged PSD. Maximum value is the sampling_frequency divided by 2.
#' @param x_increment Numeric value specifying increment of the new x-axis for the averaged PSD.
#' @param integration.or.dominant.freq A string with two possible values for choosing whether integral or dominant frequency should be calculated and compared: "integration" or "dominant_freq".
#'
#' @return A list with 3 objects:
#' 1. integrals.with.combo.labels: Dataframe used for statistical testing.
#' 2. kruskal.test.res: Results from Kruskal-Willis testing.
#' 3. pairwise.wilcox.rest.res: Results from pairwise Wilcoxo testing
#'
#'
#' @export
#'
#' @examples
#'
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
#' S1.data.frame[,"Signal"] <- as.numeric(S1.data.frame[,"Signal"])
#'
#' #Second signal
#' #1. 1 Hz with amplitude of -4
#' #2. 2 Hz with amplitude of -2
#' S2 <- (-4)*sin(2*pi*1*t) - 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S2))
#' level2.vals <- rep("2", length(S2))
#' S2.data.frame <- as.data.frame(cbind(t, S2, level1.vals, level2.vals))
#' colnames(S2.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S2.data.frame[,"Signal"] <- as.numeric(S2.data.frame[,"Signal"])
#'
#' #Third signal
#' #1. 1 Hz with amplitude of 2
#' #2. 2 Hz with amplitude of 2
#' S3 <- 2*sin(2*pi*1*t) + 2*sin(2*pi*2*t);
#' level1.vals <- rep("a", length(S3))
#' level2.vals <- rep("3", length(S3))
#' S3.data.frame <- as.data.frame(cbind(t, S3, level1.vals, level2.vals))
#' colnames(S3.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S3.data.frame[,"Signal"] <- as.numeric(S3.data.frame[,"Signal"])
#'
#' #Fourth signal
#' #1. 1 Hz with amplitude of -2
#' S4 <- -2*sin(2*pi*1*t)
#' level1.vals <- rep("b", length(S4))
#' level2.vals <- rep("3", length(S4))
#' S4.data.frame <- as.data.frame(cbind(t, S4, level1.vals, level2.vals))
#' colnames(S4.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S4.data.frame[,"Signal"] <- as.numeric(S4.data.frame[,"Signal"])
#'
#' #Fifth signal
#' #1. 5 Hz with amplitude of -2
#' S5 <- -2*sin(2*pi*5*t)
#' level1.vals <- rep("c", length(S5))
#' level2.vals <- rep("1", length(S5))
#' S5.data.frame <- as.data.frame(cbind(t, S5, level1.vals, level2.vals))
#' colnames(S5.data.frame) <- c("Time", "Signal", "level1.ID", "level2.ID")
#' S5.data.frame[,"Signal"] <- as.numeric(S5.data.frame[,"Signal"])
#'
#' #Extra representation of S2 dataframe to show an example that has enough samples
#' #to have significance for Kruskal-Wallis test
#' windows <- list(S1.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame,
#' S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S2.data.frame, S3.data.frame,
#' S4.data.frame,
#' S5.data.frame, S5.data.frame, S5.data.frame, S5.data.frame, S5.data.frame)
#'
#' #Gets the composite of the first, second, and third signal. Should result in a flat signal.
#' FirstComboToUse <- list( c("a"), c(1, 2, 3) )
#'
#' #Gets the composite of the third and fourth signal
#' SecondComboToUse <- list( c("a", "b"), c(3) )
#'
#' #Gets the composite of fifth signal
#' ThirdComboToUse <- list( c("c"), c(1) )
#'
#'
#' #PSD-------------------------------------------------------------------------
#'
#' PSD.results <- AutomatedCompositePlotting(list.of.windows = windows,
#'                            name.of.col.containing.time.series = "Signal",
#'                            x_start = 0,
#'                            x_end = 10,
#'                            x_increment = 0.01,
#'                            level1.column.name = "level1.ID",
#'                            level2.column.name = "level2.ID",
#'                            level.combinations = list(FirstComboToUse,
#'                                                     SecondComboToUse,
#'                                                     ThirdComboToUse),
#'                            level.combinations.labels = c("Signal 1 + 2 + 3",
#'                                                          "Signal 3 + 4",
#'                                                          "Signal 5"),
#'                            plot.title = "Example",
#'                            plot.xlab = "Hz",
#'                            plot.ylab = "(Original units)^2/Hz",
#'                            combination.index.for.envelope = 2,
#'                            TimeSeries.PSD.LogPSD = "PSD",
#'                            sampling_frequency = 100)
#'
#' ggplot.obj.PSD <- PSD.results[[2]]
#'
#' #Integration-------------------------------------------------------------------------
#'
#' #Compare integration for the 1.5-2.5 Hz bin. P-value should not indicate
#' #significant difference
#' integration.compare.res <- SingleBinPSDIntegrationOrDominantFreqComparison(
#' list.of.windows = windows,
#' name.of.col.containing.time.series = "Signal",
#' level1.column.name = "level1.ID",
#' level2.column.name = "level2.ID",
#' level.combinations = list(FirstComboToUse, SecondComboToUse),
#' level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
#' sampling_frequency = 100,
#' single.bin.boundary = c(1.5, 2.5),
#' integration.or.dominant.freq = "integration")
#'
#' #Kruskal-Wallis test results
#' integration.compare.res[[2]]
#'
#' #Compare integration for the 0.5-1.5 Hz bin. P-value should indicate
#' #significant difference
#' integration.compare.res2 <- SingleBinPSDIntegrationOrDominantFreqComparison(
#' list.of.windows = windows,
#' name.of.col.containing.time.series = "Signal",
#' level1.column.name = "level1.ID",
#' level2.column.name = "level2.ID",
#' level.combinations = list(FirstComboToUse, SecondComboToUse),
#' level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
#' sampling_frequency = 100,
#' single.bin.boundary = c(0.5,1.5),
#' integration.or.dominant.freq = "integration")
#'
#' #Kruskal-Wallis test results
#' integration.compare.res2[[2]]
#'
#'
#'
#' #Dominant Frequency---------------------------------------------------------------------
#'
#' #Compare dominant frequency P-value should not indicate
#' #significant difference
#' integration.compare.res3 <- SingleBinPSDIntegrationOrDominantFreqComparison(
#' list.of.windows = windows,
#' name.of.col.containing.time.series = "Signal",
#' level1.column.name = "level1.ID",
#' level2.column.name = "level2.ID",
#' level.combinations = list(FirstComboToUse, SecondComboToUse),
#' level.combinations.labels = c("Signal 1 + 2 + 3", "Signal 3 + 4"),
#' sampling_frequency = 100,
#' x_start = 0,
#' x_end = 10,
#' x_increment = 0.01,
#' integration.or.dominant.freq = "dominant_freq")
#'
#' #Kruskal-Wallis test results
#' integration.compare.res3[[2]]
#'
#'
#' #Compare dominant frequency P-value should indicate
#' #significant difference
#' integration.compare.res4 <- SingleBinPSDIntegrationOrDominantFreqComparison(
#' list.of.windows = windows,
#' name.of.col.containing.time.series = "Signal",
#' level1.column.name = "level1.ID",
#' level2.column.name = "level2.ID",
#' level.combinations = list(SecondComboToUse, ThirdComboToUse),
#' level.combinations.labels = c("Signal 3 + 4", "Signal 5"),
#' sampling_frequency = 100,
#' x_start = 0,
#' x_end = 10,
#' x_increment = 0.01,
#' integration.or.dominant.freq = "dominant_freq")
#'
#' #Kruskal-Wallis test results
#' integration.compare.res4[[2]]
#' #Values used in comparison of the two groups
#' integration.compare.res4[[1]]
#'
#'
SingleBinPSDIntegrationOrDominantFreqComparison <- function(list.of.windows,
                                              name.of.col.containing.time.series,
                                              level1.column.name,
                                              level2.column.name,
                                              level.combinations,
                                              level.combinations.labels,
                                              sampling_frequency,
                                              single.bin.boundary = NULL,
                                              x_start = NULL,
                                              x_end = NULL,
                                              x_increment = NULL,
                                              integration.or.dominant.freq){


  integrals.or.dominant.freq.for.each.combo <- list()

  #For each combo specified, get the integral value for the specified bin in all
  #windows that belong to the combo
  for(i in 1:length(level.combinations)){

    level1.categories.to.use <-  level.combinations[[i]][[1]]
    level2.categories.to.use <- level.combinations[[i]][[2]]

    subset.windows <- GetSubsetOfWindowsTwoLevels(list.of.windows, level1.column.name, level2.column.name,
                                                  level1.categories.to.use, level2.categories.to.use)

    if(integration.or.dominant.freq == "integration"){

      integration.res.for.subset.windows <- SingleBinPSDIntegrationForMultipleWindows(subset.windows,
                                                                                      name.of.col.containing.time.series,
                                                                                      sampling_frequency, single.bin.boundary)

      integrals.or.dominant.freq.for.each.combo[[i]] <- integration.res.for.subset.windows

    } else{

      dominant.freq.res.for.subset.windows <- PSDDominantFrequencyForMultipleWindows(subset.windows,
                                                                                     name.of.col.containing.time.series,
                                                                                     sampling_frequency,
                                                                                     x_start, x_end, x_increment)

      integrals.or.dominant.freq.for.each.combo[[i]] <- dominant.freq.res.for.subset.windows

    }

  }


  #At this point, we should have vectors of integrals or dominant frequency with each vector
  #corresponding to a different combo. Now we want to see if the integrals
  #in each combo significantly differ from the other combos. ANOVA can be used.

  #Combine the vectors of integrals or dominant frequency together
  vals.to.compare.combined <- NULL

  combo.labels.combined <- NULL

  for(i in 1:length(integrals.or.dominant.freq.for.each.combo)){

    #Combine integral/dominant_freq into one large vector
    integral.or.dominant.freq.vec.for.one.combo <- integrals.or.dominant.freq.for.each.combo[[i]]
    vals.to.compare.combined <- c(vals.to.compare.combined, integral.or.dominant.freq.vec.for.one.combo)

    #Make sure to have another column that labels which integrals belong in
    #which combo
    combo.labels.combined.temp <- rep(level.combinations.labels[[i]], length(integral.or.dominant.freq.vec.for.one.combo))
    combo.labels.combined <- c(combo.labels.combined, combo.labels.combined.temp)

  }

  vals.to.compare.with.combo.labels <- data.frame(vals.to.compare.combined, combo.labels.combined)

  #http://www.sthda.com/english/wiki/kruskal-wallis-test-in-r
  kruskal.test.res <- stats::kruskal.test(vals.to.compare.combined ~ combo.labels.combined, data = vals.to.compare.with.combo.labels)
  pairwise.wilcox.rest.res <- stats::pairwise.wilcox.test(vals.to.compare.with.combo.labels$vals.to.compare.combined,
                                                   vals.to.compare.with.combo.labels$combo.labels.combined,
                                                   p.adjust.method = "BH")


  output <- list(vals.to.compare.with.combo.labels, kruskal.test.res, pairwise.wilcox.rest.res)

  return(output)
}

#End of using integration and dominant frequency finder on multiple windows.
#-------------------------------------------------------------------------------

#Find the dominant frequency for many curves for a single combo.

#Compare the dominant frequency for different combos.
