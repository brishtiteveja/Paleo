f <- 1
dt <- 0.1
t <- seq(1,pi, by=dt)
y <- sin(2 * pi * f * t)
y
plot(t, y, t='l')
points(t,y)

plot.fourier <- function(fourier.series, f.0, ts) {
  w <- 2*pi*f.0
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
  trajectory
}

# An eg
trajectory <- plot.fourier(function(t,w) {sin(w*t)}, 1, ts=seq(0,1,1/100)) 

acq.freq <- 100               # data acquisition frequency (Hz)
time     <- 25                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
length(ts)
f.0      <- 1/time                 # fundamental frequency (Hz)

dc.component       <- 0
component.freqs    <- c(3)      # frequency of signal components (Hz)
component.delay    <- c(0)       # delay of signal components (radians)
component.strength <- c(1)    # strength of signal components

f <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}

trajectory <- plot.fourier(f,f.0,ts)   

# cs is the vector of complex points to convert
convert.fft <- function(cs, sample.rate=1) {
  cs <- cs / length(cs) # normalize
  
  distance.center <- function(c)signif( Mod(c),        4)
  angle           <- function(c)signif( 180*Arg(c)/pi, 3)
  
  df <- data.frame(cycle    = 0:(length(cs)-1),
                   freq     = 0:(length(cs)-1) * sample.rate / length(cs),
                   period   = 1/(0:(length(cs)-1) * sample.rate / length(cs)),
                   strength = sapply(cs, distance.center),
                   delay    = sapply(cs, angle))
  df
}
ft <- fft(trajectory)
df <-convert.fft(ft, sample.rate = 1/acq.freq)
library(DT)
datatable(df)
plot(df$freq, df$strength, t='l')
