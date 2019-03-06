# Understanding Fourier Spectral Analysis

sine <- function(freq, time_interval, rate, amp=1) {
  w = 2 * pi * freq
  t = seq(0, time_interval, by=1/rate)
  y = amp* sin(w * t)
  return(y)
}

showSignals <- function(signals, secs, fs) {
  nrSigs = length(signals)
  #par(mfrow=c(nrSigs,2))
  for(i in 1:nrSigs) {
    t = seq(0, secs, by=1/fs)
    s <- signals[[i]]
    plot(t, s, t='l',
         title='signal', xlab='time', ylab='amplitude', main='Signal')
    
    amps = abs(fft(s))/length(s)  # scaled power spectrum
    amps = amps[1:(length(amps)/2)]  # because of the symmetry
    amps = amps[1:50]  # only the first 50 frequencies, arbitrarily chosen
    # this should be close to the amplitude:
    freqs=seq(0, length(amps), by=1)/secs
    
    plot(freqs[1:50], amps[1:50], t='h',
         xlab='freq',ylab='amplitude', xlim=c(0,15))
  }
}

# Nyquist-Shannon sampling theorem
# If a function x(t) contains no frequencies higher than f hertz, 
### it is completely determined by sampling rate,fs = 2*f

#The highest meaningful sin wave frequency – after the fft-analysis of the original 
#waveform signal – is at half the data acquisition frequency, because our input signal 
#is composed of real values (ie, trajectory has no imaginary parts). The last useful bin 
#is at acq.freq/2. The Nyquist Frequency is half the sampling rate. The Nyquist frequency 
#is the maximum frequency that can be detected for a given sampling rate. This is because 
#in order to measure a wave you need at least one trough and one peak to identify it.

# Nyquist Frequency - Half of the sampling rate, fs

buildData <- function() {
  secs <- 3
  fs <- 100
  # frequency, duration, sampling rate, amplitude
  y1 = sine(0.5, secs, fs, 10)
  y2 = sine(1, secs, fs, 13)
  y3 = sine(2, secs, fs, 12)
  y4 = sine(5, secs, fs, 15)
  y = y1 + y2 + y3 + y4
  signals = list(y1, y2, y3, y4, y)
  showSignals(signals, secs, fs)
}

buildData()

# Let's investigate the effect of demean
data(ldeaths)
# Plotting monthly data, each point represents monthly death
plot(ldeaths)

# Are there any cyclicity?
f <- fft(ldeaths)
Amp <- Mod(f)
Amp <- Amp/length(Amp)
n <- length(Amp)/2
Amp <- Amp[1:n]
freq <- (0:(n-1))
# Let's plot frequency per time window (6 years = 72 months)
plot(freq, Amp, t='h', xaxp=c(0, 36, 6),  
     ylim=c(0,500),
     xlab='frequency (cycle per 1 years)', ylab='Amplitude')

# Let's plot frequency per year
time_window <- length(ldeaths)/12 # 72 months = 6 years
freq <- (0:(n-1))/time_period
plot(freq, Amp, t='h', xaxp=c(0, 6, 6),  
     ylim=c(0,500),
     xlab='frequency (cycle per 1 years)', ylab='Amplitude')

# plot with main timeseries
par(mfcol=c(2,1))
plot(ldeaths)
plot(freq, Amp, t='h', xaxp=c(0, 6, 6),  
     ylim=c(0,500),
     xlab='frequency (cycle per 1 years)', ylab='Amplitude')

# Clearly 6/6 = 1 cycle per year and 12/6 = 2 cycle per year 

# High amplitude in zero frequency ---> needs to demean
d <- ldeaths
d.m <- d - mean(d)
f <- fft(d.m)
Amp <- Mod(f)

n <- length(Amp) # 72 amplitudes
freq <- 0:(n-1)  # 72 frequencies
Amp <- Amp[1:n]
n_f <- length(freq)
n_a <- length(Amp)
freq <- freq[2:(n_f/2)]
Amp <- Amp[2:(n_a/2)]
plot(freq, Amp, t='h', xaxp = c(0, 36, 6), 
     xlab='frequency (cycle per 6 years)', ylab='Amplitude')

time_period <- length(d) / 12 # 6 years
period <- (time_period)/freq # months   
plot(period, Amp, t='l', #xaxp = c(0, 15, 30), 
     log='xy',
     xlab='period (years)', ylab='Amplitude')




# Check with library PSD (Adaptive sine multitaper)
s <- psdcore(d,ntaper=3)
plot(s, type='l'
  , xlab='frequency (1/year)', ylab='spectrum')
plot(s, type='h'
  , xlab='frequency (1/year)', ylab='spectrum')

# Adaptive multitaper
s.adapt <- pspectrum(d, ntaper=1)
plot(s, type='l'
  , xlab='frequency (1/year)', ylab='spectrum')


secs = 3
fs = 100
y1 = sine(0.5, secs, fs, 10)
y2 = sine(1, secs, fs, 13)
y3 = sine(2, secs, fs, 12)
y4 = sine(5, secs, fs, 15)
y = y1 + y2 + y3 + y4

d = y
df = fft(d)
N = length(d)
amp = Mod(df)/N
p = amp^2
n = 50
amp = amp[1:n]
freq = seq(0,n-1, by=1)/secs
plot(freq, amp, type='l')

t = seq(0, secs, by=1/fs)
ys <- ts(y, start=0, frequency=fs)

# stats:spectrum
Xspec <- spec.pgram(ys, pad = 1, taper = 0, detrend = TRUE, 
                    demean = TRUE, plot = FALSE, spans=3
)
                    fast=FALSE,spans=5)
plot(Xspec)
abline(v=0.5, lty=2)
abline(v=1, lty=2)
abline(v=2, lty=2)
abline(v=5, lty=2)
# psd: psdcore (fixed taper)
tapinit=1
Pspec <- psdcore(ys, ntaper = tapinit) 
plot(Pspec, col='red')
# psd: adaptive (adaptive taper)
Aspec <- pspectrum(ys, ntap.init = 1)
plot(Aspec, col='blue')

library(multitaper)
tapinit=10
Mtspec <- spec.mtm(ys, k = tapinit)
class(Mtspec)
plot(Mtspec, col='green')

# bspec
library(bspec)
Bspec <- bspec(ys)
plot(Bspec)
with (Pspec, lines(freq, spec, col='red', lwd=2))
print(Bspec)

par(las=0)
Bspec_plt <- plot(Bspec$freq, Bspec$scale/Bspec$datadf, type='l')

