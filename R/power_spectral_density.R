

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
