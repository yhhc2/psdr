library(psdr)

#MakeOneSidedAmplitudeSpectrum

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
