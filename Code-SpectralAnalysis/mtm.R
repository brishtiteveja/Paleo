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
library(psd)
library(RSEIS)

# Create trended time series data from magnet$clean
dt = 1
data(magnet)
mc <- ts(magnet$clean + 1000, deltat = dt) + seq_along(magnet$clean)
plot(mc)

# detrending, demeaning and 
pmc <- prewhiten(mc)
summary(pmc)

# Although the default operation of prewhiten is to fit a linear model of the form f (x) = αx + β + ε using ordinary linear least squares, setting AR.max higher than zero to fit an auto-regressive (AR) model to the data3. This fit uses the Akaike Infomation Criterion (AIC) to select the highest order appropriate for the data.
summary(atsar <- prewhiten(mc, AR.max = 100, plot = FALSE))

str(atsar[["lmdfit"]])
ats_lm <- atsar[["prew_lm"]]  

str(atsar[["ardfit"]])
ats_ar <- atsar[["prew_ar"]] # AR model:

plot(ts.union(orig.plus.trend = mc, linear = ats_lm, ar = ats_ar), yax.flip = TRUE, main = sprintf("Prewhitened Project MAGNET series"), las = 0)
mtext(sprintf("linear and linear+AR(%s)", atsar[["ardfit"]][["order"]]), line = 1.1)

# RSEIS comparison, we first estimate the PSD from mtapspec with 10 tapers:
tapinit <- 7
Mspec <- mtapspec(ats_lm, dt = deltat(ats_lm), MTP = list(kind = 2, inorm = 3,
                                                     nwin = tapinit, npi = 0))
str(Mspec)

#We will calculate the comparative spectra from
#1. spectrum (20% cosine taper),
#2. psdcore (with fixed tapers), and
#3. pspectrum (allowing adaptive taper refinement)
Xspec <- spec.pgram(ats_lm, pad = 1, taper = 0.2, detrend = TRUE, demean = TRUE, plot = FALSE)
Pspec <- psdcore(ats_lm, ntaper = tapinit) 
Aspec <- pspectrum(ats_lm, ntap.init = tapinit)
library(multitaper)
Mtspec <- spec.mtm(ats_lm, k = tapinit)
class(Mtspec)
plot(Mtspec)
?spec.mtm


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
#Normalizations commonly encountered for power spectra depend on it's assumed sidedness: whether the spectrum is either single- or double-sided. The normalizations performed here enforce single-sidedness, and correct as necessary.
#Frequencies are assumed to be based on the Nyquist frequency (half the sampling rate). For example: If a series X has sampling frequency F_S, then the PSD frequencies will span [0,F_S/2].
Xspec <- normalize(Xspec, dt, "spectrum")
## Normalized double-sided psd estimates ( spectrum ) for sampling-freq. 1

library(RColorBrewer)
cols <- c("dark grey", brewer.pal(8, "Set1")[c(5:3, 2)])
lwds <- c(1, 2, 2, 5, 3)
plot(Xspec, log = "dB", ylim = 40 * c(-0.4, 1), ci.col = NA, col = cols[1],
     lwd = lwds[1], main = "PSD Comparisons")
pltf <- Mspec[["freq"]]
pltp <- dB(mspec)
lines(pltf, pltp, col = cols[2], lwd = lwds[2])
plot(Pspec, log = "dB", add = TRUE, col = cols[3], lwd = lwds[3])
plot(Aspec, log = "dB", add = TRUE, col = cols[4], lwd = lwds[4]) 
plot(Mtspec, log="dB", add=TRUE, col=cols[5], lwd=lwds[5])
legend("topright", c("spec.pgram", "RSEIS::mtapspec", "psdcore", "pspectrum"),
                                                                         title = "Estimator", col = cols, lwd = lwds, bg = "grey95", box.col = NA, cex = 0.8, inset = c(0.02, 0.03))


library(signal, warn.conflicts = FALSE)
# Because we did not specify the length of the FFT in mtapspec we end up with di↵erent length spectra. So, to form some statistical measure of the results, we can interpolate PSD levels onto the psd-based frequencies (or reciprocally):
# 
pltpi <- interp1(pltf, pltp, Pspec[['freq']])

# We regress the spectral values from mtapspec against the psdcore results because we have used them to produce uniformly tapered spectra with an equal number of sine tapers.
df <- data.frame(x = dB(Pspec[["spec"]]), y = pltpi, tap = unclass(Aspec[["taper"]]))
summary(dflm <- lm(y ~ x + 0, df))

df$res <- residuals(dflm)

library(ggplot2)
gr <- ggplot(df, aes(x = x, y = res)) + 
  geom_abline(intercept = 0, slope = 0,size = 2, color = "salmon") + 
  geom_point(aes(color = tap))

print(gr + theme_bw() + ggtitle("Regression residuals, colored by optimized tapers") +
xlab("Power levels, dB") + ylab(""))



# Bayesian spectral analysis
#An intriguing method for producing power spectral density estimates using Bayesian inference is presented by Röver et al. (2011) and included in the bspec package. Simplistically, the method uses a Student’s t likelihood function to estimate the distribution of spectral densities at a given frequency. 
#We will use the spectra from the previous calculation to compare with bspec results.
library(bspec)
Bspec <- bspec(ts(magnet$clean))
print(Bspec)
par(las=0)
Bspec_plt <- plot(Bspec$freq, Bspec$scale/Bspec$datadf, type='l')
with (Pspec, lines(freq, spec, col='red', lwd=2))

# Spectral uncertainties
# In a multitaper algorithm the uncertainty is distributed as a  chi-square variate where n is the number of degrees of freedom
 # Using ⌫ = 2 ⇤ K we can approximate the distribution of uncer- tainties from the tapers alone;
spp <- spectral_properties(Pspec[["taper"]], db.ci = TRUE) 
spa <- spectral_properties(Aspec[["taper"]], db.ci = TRUE)
str(spa)

psppu <- with(Pspec, create_poly(freq, dB(spec), spp$stderr.chi.upper))
plot(psppu, type='l')

pspau <- with(Aspec, create_poly(freq, dB(spec), spa$stderr.chi.upper))
plot(pspau, type='l')

Bspec_plt <- plot(Bspec)
pspb <- with(Bspec_plt, create_poly(freq, spectrum[, 1], spectrum[, 3], from.lower = TRUE))
plot(pspb, type='l')

plot(c(-0.005, 0.505), c(-5, 40), col = NA, xaxs = "i", main = "Project MAGNET Spectral Uncertainty (p > 0.95)", ylab = "", xlab = "spatial frequency, 1/km", yaxt = "n", frame.plot = FALSE)
lines(c(2, 1, 1, 2) * 0.01, c(0, 0, 7, 7))
text(0.04, 3.5, "7 dB")
with(pspb, polygon(x.x, dB(y.y), col = "light blue", border = NA))
text(0.26, 37, "Posterior distribution\n(bspec)", col = "#0099FF", cex = 0.8) 
with(psppu, polygon(x.x, y.y, col = "dark grey", border = "black", lwd = 0.2)) 
text(0.4, 22, "Dark: Uniform\ntapering (psdcore)", cex = 0.8)
with(pspau, polygon(x.x, y.y, col = "light grey", border = "black", lwd = 0.2)) 
text(0.15, 6, "Light: adaptive\ntaper refinement\n(pspectrum)", cex = 0.8) 
box()


# Spectral resolution
spp <- spectral_properties(Pspec[["taper"]], db.ci = TRUE) 
spa <- spectral_properties(Aspec[["taper"]], db.ci = TRUE) 
str(spa)
frq <- Aspec[["freq"]]
relp <- (spa$resolution - spp$resolution)/spp$resolution
yl <- range(pretty(relp))
par(las = 1, oma = rep(0, 4), omi = rep(0, 4), mar = c(4, 3, 2, 0))
layout(matrix(c(1, 2), 1), heights = c(2, 2), widths = c(3, 0.5), respect = TRUE) 
plot(frq, relp, main = "Percent change in spectral resolution", col = "light grey",
     ylim = yl,  ylab = "dB", xlab = "frequency, 1/km", type='h', yaxs='i')
lines(frq, relp)
text(0.25, 45, "Adaptive relative to fixed", cex = 0.9)
par(mar = c(4, 0, 2, 2))
# empirical distribution of values
boxplot(relp, range = 0, main = sprintf("%.01f", median(relp)), axes = FALSE, ylim = yl, yaxs = "i", notch = TRUE)
axis(4)
