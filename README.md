
<!-- README.md is generated from README.Rmd. Please edit that file -->

## Overview

Author: Yong-Han Hank Cheng

This package allows you to generate and compare power spectral density
(PSD) plots given timeseries data.

## Installation

``` r
# Install the package from GitHub
devtools::install_github("yhhc2/psdr")
```

``` r
# Load package
library("psdr")
```

## Usage

Visit the package’s website for function reference:
<https://yhhc2.github.io/psdr/>

## Examples

All functions with example code is run in this section. The functions
are listed below in alphabetical order with example code to illustrate
how each function should be used.

To see detailed descriptions for each function, please visit the
package’s website.

### AutomatedCompositePlotting()

``` r
#I want to create a plot that shows two curves:
#1. Composite of time series signals 1, 2, and 3.
#2. Composite of time series signals 3 and 4.

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

windows <- list(S1.data.frame, S2.data.frame, S3.data.frame, S4.data.frame)

#Gets the composite of the first, second, and third signal. Should result in a flat signal.
FirstComboToUse <- list( c("a"), c(1, 2, 3) )

#Gets the composite of the third and fourth signal
SecondComboToUse <- list( c("a", "b"), c(3) )


#Timeseries-----------------------------------------------------------------

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
```

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 1
    ## [1] 2

``` r
ggplot.obj.timeseries <- timeseries.results[[2]]

#Plot. Will see the 1+2+3 curve as a flat line. The 3+4 curve will only have 2 Hz.
#dev.new()
ggplot.obj.timeseries
```

![](README_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

``` r
#PSD-------------------------------------------------------------------------

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
                           combination.index.for.envelope = 2,
                           TimeSeries.PSD.LogPSD = "PSD",
                           sampling_frequency = 100)
```

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 1
    ## [1] 2

``` r
ggplot.obj.PSD <- PSD.results[[2]]

#Plot. For both plots, two peaks will be present, 1 Hz and 2 Hz. 1 Hz should be
#stronger in both cases because more signals have this frequency (even if amp is negative).
#Error envelope is specified for the second (red) curve. Envelope should only
#be present for 2 Hz signal.
#dev.new()
ggplot.obj.PSD
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 5 row(s) containing missing values (geom_path).

![](README_files/figure-gfm/unnamed-chunk-3-2.png)<!-- -->

``` r
 #PSD Zoomed in---------------------------------------------------------------

 PSD.results <- AutomatedCompositePlotting(list.of.windows = windows,
                            name.of.col.containing.time.series = "Signal",
                            x_start = 0,
                            x_end = 5,
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
```

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 1
    ## [1] 2

``` r
 ggplot.obj.PSD <- PSD.results[[2]]

 #Plot. For both plots, two peaks will be present, 1 Hz and 2 Hz. 1 Hz should be
 #stronger in both cases because more signals have this frequency (even if amp is negative).
 #Error envelope is specified for the second (red) curve. Envelope should only
 #be present for 2 Hz signal.
 #dev.new()
 ggplot.obj.PSD
```

![](README_files/figure-gfm/unnamed-chunk-3-3.png)<!-- -->

``` r
#LogPSD-------------------------------------------------------------------------

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
                           plot.ylab = "log((Original units)^2/Hz)",
                           combination.index.for.envelope = NULL,
                           TimeSeries.PSD.LogPSD = "LogPSD",
                           sampling_frequency = 100)
```

    ## [1] 1
    ## [1] 2
    ## [1] 3
    ## [1] 1
    ## [1] 2

``` r
ggplot.obj.LogPSD <- LogPSD.results[[2]]

#Plot. For both plots, two peaks will be present, 1 Hz and 2 Hz. 1 Hz should
#be stronger in both cases because more signals have this frequency (even if amp is negative).
#Error envelope is specified for the second (red) curve. Envelope should only
#be present for 1 Hz signal.
#dev.new()
ggplot.obj.LogPSD
```

    ## Warning: Removed 5 row(s) containing missing values (geom_path).
    
    ## Warning: Removed 5 row(s) containing missing values (geom_path).

![](README_files/figure-gfm/unnamed-chunk-3-4.png)<!-- -->
