#(I)  Synthetic Data 

### Step 1: Data Collection
# Read Lake Cycle excel sheet
lake_xls_f <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/SpectralAnalysis/LakeCycleHomework_AnalysisExcel.XLS'
excel_sheets(lake_xls_f)

lake_xls <- read_excel(lake_xls_f, sheet='Input Data')

# Synthetic Data
synth_dat <- data.frame(lake_xls[8:521,c(1,2,3)])
colnames(synth_dat) <- c('Point', 'Depth(m)', 'Data')

plot(synth_dat$`Depth(m)`, synth_dat$Data, xlab='Depth(meters)', 
     ylab='Value', type='l', col='darkblue', lwd=2,
     main='Input Data for Spectral Analysis'
     )

# Newark Basin Lake Sediments
# Selection from Interval I (lower lake series)
# 0.85m data spacing; 512  point section (+2 pts.)
nb_sed_d <- data.frame(lake_xls[8:521,c(6,7,8)])
colnames(nb_sed_d) <- c('Point', 'Depth (m)', 'Depth Rank')

# Simulated Hiatus
# Same Newark Interval
# With 100 pt removed (gap)
sim_hts_d <- data.frame(lake_xls[8:521,c(11,12,13)])
colnames(sim_hts_d) <- c('Point', 'Depth (m)', 'Depth Rank')

# Complete Interval I of Newark Basin
# Interval I = 2768-3370 m; 706 pts
nb_d <- data.frame(lake_xls[8:713,c(16,17,18)])
colnames(nb_d) <- c('Point', 'Depth (m)', 'Depth Rank')

# Depth Vec
dv <- as.numeric(nb_sed_d$`Depth (m)`)
diff(dv)
max_d <- max(dv)
min_d <- min(dv)
# window of depth
window <- max(dv)-min(dv)


#Mean of Points 1-512 =
synth_dat.avg <- mean(as.numeric((synth_dat$`Data`)[2:nrow(synth_dat)-1]))

#________________________________________________________________________________#
### Step 2. De-mean the data
# Demean synthetic dat
synth_dat.dm <- as.numeric(synth_dat$Data) - synth_dat.avg

# plot
plot(synth_dat$`Depth(m)`, synth_dat$Data, xlab='Depth(meters)', 
     ylab='Value', type='l', col='darkblue', lwd=2,
     main='Demeaned Input Data(Red Curve)', ylim=c(-8,8)
)
abline(h=synth_dat.avg, lty=2, col='darkblue')
lines(synth_dat$`Depth(m)`, synth_dat.dm, col='red')
abline(h=0, lty=2, col='red')

#________________________________________________________________________________#
### Step 3. Filter the data

# Filtering - White noise removed
# (3pt moving "Hanning" average)
# 0.5*n + 0.25*(n-1 + n+1)
hanning_ma <- function(df, n) {
   x <- 0.5 * df[n] + 0.25*(df[n-1] + df[n+1])
   return(x)
}

synth_dat.filt <- c()
i <- 1
nr <- length(synth_dat.dm)
for (n in 2:(nr-1)) {
   h <- hanning_ma(synth_dat.dm, n)
   synth_dat.filt <- c(synth_dat.filt, h)
}

# plot
plot(synth_dat$`Depth(m)`, synth_dat.dm, xlab='Depth(meters)', 
     ylab='Value', type='l', col='red', lwd=1,
     main='Demeaned + Filtered Input Data', ylim=c(-8,8)
)
abline(h=0, lty=2, col='red')
lines((synth_dat$`Depth(m)`)[2:(nr-1)], synth_dat.filt, col='green')

#________________________________________________________________________________#
### Step 5. Split-Cosine Tapering
synth_dat.tap <- synth_dat.filt
i <- 1
nr <- length(synth_dat$`Depth(m)`)
n <- length(synth_dat.filt)

tap_perc_n <- n * 10/100 # 10% tapering to reduce the effect of the terminations of data at the ends of the window

for (j in 1:tap_perc_n) {
  w = j * pi / tap_perc_n
  synth_dat.tap[j] = synth_dat.filt[j] * 0.5 * (1-cos(w))
}
for (j in (n-tap_perc_n):n) {
  w = (512-j) * pi / tap_perc_n
  synth_dat.tap[j] = synth_dat.filt[j] * 0.5 * (1-cos(w))
}
# plot
plot(synth_dat$`Depth(m)`, synth_dat.dm, xlab='Depth(meters)', 
     ylab='Value', type='l', col='red', lwd=1,
     main='Demeaned + Filtered Input Data', ylim=c(-8,8)
)
abline(h=0, lty=2, col='red')
lines((synth_dat$`Depth(m)`)[2:(nr-1)], synth_dat.filt, col='green')
lines((synth_dat$`Depth(m)`)[2:(nr-1)], synth_dat.tap, col='black')

#________________________________________________________________________________#
### Step 6. Fourier Spectral Analysis
synth_dat.fft <- fft(synth_dat.tap)
par(mfrow=c(2,1))
plot(synth_dat$`Depth(m)`, synth_dat.dm, xlab='Depth(meters)', 
     ylab='Value', type='l', col='red', lwd=1,
     main='Demeaned + Filtered Input Data', ylim=c(-8,8)
)
plot.frequency.spectrum(synth_dat.fft, 
                        #xlim=c(0,50),
                        xlab='Frequency (cycle per 76.65 meters)', 
                        ylab='Amplitude', 
                        plot.type = 'h')

hm <- c(5, 15, 20)
plot.show(synth_dat.tap, time = 76.65, harmonics = hm,
          xlab='Depth (meter)', ylab='value')
hm_l <- paste(hm, ' cycles')
hm_l <- c(hm_l, 'combined cycle')
legend('top', legend=hm_l, col = c('blue','green','red','darkblue'), lty = rep(1,4), cex=0.40)

Xspec <- spec.pgram(synth_dat.tap)
plot(Xspec$freq, Xspec$spec, type='h')


library(psd)
d <- ts(synth_dat.tap, frequency=1)
Pspec <- psdcore(d, ntapaer=2)
plot(Pspec, xlim=c(0,0.1))
spp <- spectral_properties(Pspec[["taper"]], db.ci = TRUE) 
psppu <- with(Pspec, create_poly(freq, dB(spec), spp$stderr.chi.upper))
par(new=T)
plot(psppu$x.x, psppu$y.y, lty=2, type='l', 
     xlim=c(0, 0.1), col='green',
     axes=FALSE, xlab='', ylab='')



# Calculate Amplitude
Amp <- Mod(synth_dat.fft)

# Calculate Power Spectrum
PowerSpec <- Amp^2

# Take half of it , coz mirrored
PowerSpec <- PowerSpec[1:(length(Amp)/2)]

PowerSpec.han <- c()
i <- 1
nr <- length(PowerSpec)
for (n in 2:(nr-1)) {
  h <- hanning_ma(PowerSpec, n)
  PowerSpec.han <- c(PowerSpec.han, h)
}

# Calculate Wavelength
dp <- as.numeric(synth_dat$`Depth(m)`)
window_width <- max(dp) - min(dp)
wavenum <- 1:(length(PowerSpec)-2)
wavelen <- window_width/wavenum

TotPowerSpec <- sum(PowerSpec)
RelPowerSpec <- PowerSpec.han/TotPowerSpec

# Charts of Wavenumber vs. Smoothed Power 
plot(wavenum, PowerSpec.han, t='h', 
     xlab='Wavenumber (cycle/Window)  Window=76.65m', 
     ylab='Power Spectra [Amplitude]^2',
     xlim=c(0,50))

# )Wavelength vs. Relative Power Spectra
plot(wavelen, RelPowerSpec, t='h', 
     xlab='Wavelength (m)', 
     ylab='Relative Power Spectra [Amplitude]^2',
     xlim=c(0,50))


d <- data.frame(WaveLength=wavelen, RelativePowerSpectra=RelPowerSpec)
RelPowerSpec.sorted <- d[order(d['RelativePowerSpectra'], decreasing=TRUE),]



#________________________________________________________________________________#
### Step 7. Calculation of Sedimentation Rate
# Method #1:  Assuming a peak is a Milankovitch Cycle;  computing Sedimentation Rate
# We shall assume that the middle major peak, wihich has an apparent wavelength of about 1/3rd 
# of the large long-wavelength peak, is exactly the 123 k.y. eccentricity cycle.  
#Eccentricity   412, 123,  and 95 k.y.
#Obliquity      41 k.y.
#Precession    23, and 19 k.y.


eccentricity <- 123
# Medium major peak is 15 cycles per 76.95m
wavelen1 <- 76.95/15 
sed_rate <-  wavelen1/ (eccentricity/1000)
# Therefore, sedimentation rate is 41.71 m/m.y.

#Assigning Remaining Peaks using the “123 k.y.” assumption:
#Use the sedimentation rate to calculate the periods (in k.y.) 
#of the remaining peaks.  Re-calculate the periods (in k.y.) by:  
#“= (1000 * (Wavelength of peak / Sedimentation rate ))”
wavelen2 <- 76.95/5 # 5 cycles 
first_eccentricity <- 1000 * (wavelen2 / sed_rate) # 369 k.y ~ 412 k.y

wavelen3 <- 76.95/20 # 20 cycles 
third_eccentricity <- 1000 * (wavelen3 / sed_rate) # 92.25 k.y ~ 95 k.y


# Method #2:  Using Ratios of Wavelengths
# Often the long-period peaks do not coincide with an integer spectral-wavenumber, 
# because there may not be a perfect integer-number of full long-period cycles within 
# the selected window.  Therefore, the assigned sedimentation rate (and associated 
# estimates of periods of other peaks) will have a relatively large error.
# One way to test whether the shorter-period peaks in the spectra are related to 
# Milankovitch is to compare the ratio of their wavelengths with the ratio of the 
# Milankovitch periods.
# Here, it exhibit peaks with wavelengths of 25.65 m and 19.23 m.  
# The ratio of 25.65/19.23 is  1.33.   
# The ratio of the two eccentricity periods of 123 k.y. and 95 k.y. is 1.295, 
# which is within 3% of the wavelength ratio.  
# Therefore, within the limitations of integer-wavenumbers, 
# these two peaks can be assigned to the two Eccentricity periods, 
# and a more accurate sedimentation rate can be computed.
wave_len3 <- 19.23
eccentricity3 <- 95
sed_rate3 <-  wavelen3 / (eccentricity3/1000)






















