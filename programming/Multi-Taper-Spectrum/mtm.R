# Understanding fourier transform
x <- 1:4
fft(x)
fft(fft(x), inverse = TRUE)/length(x)


## Slow Discrete Fourier Transform (DFT) - e.g. , for checking the formula
fft0 <- function(z, inverse = FALSE) {
  n <- length(z)
  if(n == 0)
    return (z)
  k <- 0:(n-1)
  ff <- (if(inverse) 1 else -1) * 2 * pi * 1i * k/n
  vapply(1:n, function(h) sum(z * exp(ff*(h-1))), complex(1))
}

# Uset the slow DFT
x <- 1:4
fft0(x)
fft0(fft0(x), inverse = TRUE)/length(x)

# See difference between fft and DFT
relD <- function(x, y) 2 * abs(x-y) / abs(x+y)
n <- 2^8
z <- complex(n, rnorm(n), rnorm(n))
summary(relD(fft(z), fft0(z)))
summary(relD(fft(z, inverse=TRUE), fft0(z, inverse=TRUE)))


# Explore spectral leakage
xFreq = 10.5
xSize = 100.0
xPeriod = xSize / xFreq # period=9.52
x = seq(1, xSize)
data = sin(2 * pi * x / xPeriod)

fft_a = fft(data)
fft_a
Mod(fft_a)
Arg(fft_a)

plot(Mod(fft_a))
lines(x, data)


require(graphics)

plot(ldeaths)
spectrum(ldeaths)
spectrum(ldeaths, spans = c(1))
spectrum(ldeaths, spans = c(3))
spectrum(ldeaths, spans = c(3,5))
spectrum(ldeaths, spans = c(5,7))
spectrum(mdeaths, spans = c(3,3))
spectrum(fdeaths, spans = c(5,7))

## bivariate example
mfdeaths.spc <- spec.pgram(ts.union(mdeaths, fdeaths), spans=c(3,3))
# plots marginal spectra: now plot coherency and phase
# plots marginal spectra: now plot coherency and phase
plot(mfdeaths.spc, plot.type = "coherency")
plot(mfdeaths.spc, plot.type = "phase")

## now impose a lack of alignment
mfdeaths.spc <- spec.pgram(ts.intersect(mdeaths, stats::lag(fdeaths, 4)),
                           spans = c(3,3), plot = FALSE)
plot(mfdeaths.spc, plot.type = "coherency")
plot(mfdeaths.spc, plot.type = "phase")

stocks.spc <- spectrum(EuStockMarkets, kernel("daniell", c(30,50)),
                       plot = FALSE)
plot(stocks.spc, plot.type = "marginal") # the default type
plot(stocks.spc, plot.type = "coherency")
plot(stocks.spc, plot.type = "phase")

sales.spc <- spectrum(ts.union(BJsales, BJsales.lead),
                      kernel("modified.daniell", c(5,7)))
plot(sales.spc, plot.type = "coherency")
plot(sales.spc, plot.type = "phase")


# Tutorial from PSD paper
# Multitaper comparison

library(psd)
# magnet data
data("magnet")
head(magnet)
magnet.m <- subset(magnet, abs(mdiff) > 0)
plot(magnet$km, magnet$clean+75, type='l') 
par(new=T)
plot(magnet$km, magnet$raw, type='l',
     col=2, axes = FALSE, xlab='', ylab='')


psdr <- pspectrum(magnet$raw, plot = TRUE)
psdc <- pspectrum(magnet$clean, plot = TRUE)

# plot them on the same scale
plot(psdc, log="dB",
     main="Raw and cleaned Project MAGNET power spectral density estimates",
     lwd=3, ci.col=NA, ylim=c(0,32), yaxs="i")
plot(psdr, log="dB", add=TRUE, lwd=3, lty=5)
text(c(0.25,0.34), c(11,24), c("Clean","Raw"), cex=1)

plot(pspectrum(magnet$clean, niter=1, x.frqsamp=10, plot=TRUE))

plot(0:10,dchisq(0:10, df=s$df), type='l', axes=FALSE, xlab='', ylab='', lty=2, col=3)
par(new=TRUE)
t <- 0:10
chisqv <- dchisq(0:10, df=s$df)
plot(t, chisqv, type='l', axes=FALSE, xlab='', ylab='', lty=2, col=3)

# Method of recovery
psdc_recovered <- psd_envGet("final_psd") 
all.equal(psdc, psdc_recovered)

# 
library(RSEIS)

# Create trended time series data from magnet$clean
dt = 1
mc <- ts(magnet$clean + 1000, deltat = dt) + seq_along(magnet$clean)
plot(mc)

# detrending, demeaning and 
pmc <- prewhiten(mc)
summary(pmc)

# Although the default operation of prewhiten is to fit a linear model of the form f (x) = αx + β + ε using ordinary linear least squares, setting AR.max higher than zero to fit an auto-regressive (AR) model to the data3. This fit uses the Akaike Infomation Criterion (AIC) to select the highest order appropriate for the data.
summary(atsar <- prewhiten(mc, AR.max = 100, plot = FALSE))

str(atsar[["lmdfit"]])

ats_lm <- atsar[["prew_lm"]] # AR model: 
str(atsar[["ardfit"]])
ats_ar <- atsar[["prew_ar"]]

plot(ts.union(orig.plus.trend = mc, linear = ats_lm, ar = ats_ar), yax.flip = TRUE, main = sprintf("Prewhitened Project MAGNET series"), las = 0)
mtext(sprintf("linear and linear+AR(%s)", atsar[["ardfit"]][["order"]]), line = 1.1)

# RSEIS comparison, we first estimate the PSD from mtapspec with 10 tapers:
tapinit <- 10
Mspec <- mtapspec(ats_lm, dt = deltat(ats_lm), MTP = list(kind = 2, inorm = 3,
                                                     nwin = tapinit, npi = 0))


#We will calculate the comparative spectra from
#1. spectrum (20% cosine taper),
#2. psdcore (with fixed tapers), and
#3. pspectrum (allowing adaptive taper refinement)
Xspec <- spec.pgram(ats_lm, pad = 1, taper = 0.2, detrend = TRUE, demean = TRUE, plot = FALSE)
Pspec <- psdcore(ats_lm, ntaper = tapinit) 
Aspec <- pspectrum(ats_lm, ntap.init = tapinit)

## Normalized  single-sided  psd estimates ( psd ) for sampling-freq.  1
# Correct for double-sidedness of spectrum and mtapspec results
class(Mspec)
## [1] "list"
Mspec <- normalize(Mspec, dt, "spectrum")
## Normalized double-sided psd estimates ( spectrum ) for sampling-freq. 1
nt <- seq_len(Mspec[["numfreqs"]]) 
mspec <- Mspec[["spec"]][nt] 

class(Xspec)
## [1] "spec"
Xspec <- normalize(Xspec, dt, "spectrum")
## Normalized double-sided psd estimates ( spectrum ) for sampling-freq. 1

library(RColorBrewer)
cols <- c("dark grey", brewer.pal(8, "Set1")[c(5:4, 2)])
lwds <- c(1, 2, 2, 5)
plot(Xspec, log = "dB", ylim = 40 * c(-0.4, 1), ci.col = NA, col = cols[1],
     lwd = lwds[1], main = "PSD Comparisons")
pltf <- Mspec[["freq"]]
pltp <- dB(mspec)
lines(pltf, pltp, col = cols[2], lwd = lwds[2])
plot(Pspec, log = "dB", add = TRUE, col = cols[3], lwd = lwds[3])
plot(Aspec, log = "dB", add = TRUE, col = cols[4], lwd = lwds[4]) 
legend("topright", c("spec.pgram", "RSEIS::mtapspec", "psdcore", "pspectrum"),
                                                                         title = "Estimator", col = cols, lwd = lwds, bg = "grey95", box.col = NA, cex = 0.8, inset = c(0.02, 0.03))
