

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

#MakeOneSidedAmplitudeSpectrumOverlayed

#MakePowerSpectralDensity

#MakeCompositePSDForAllWindows
