### Step 1: Data Collection
# Read Lake Cycle excel sheet
library(readxl)
lake_xls_f <- './LakeCycleHomework_AnalysisExcel.XLS'
excel_sheets(lake_xls_f)

lake_xls <- read_excel(lake_xls_f, sheet='Input Data')

# Complete Interval I of Newark Basin
# Interval I = 2768-3370 m; 706 pts
nb_d <- data.frame(lake_xls[8:713,c(16,17,18)])
colnames(nb_d) <- c('Point', 'Depth (m)', 'Depth Rank')

# Depth Vec
dv <- as.numeric(nb_d$`Depth (m)`)
diff(dv)
max_d <- max(dv)
min_d <- min(dv)
# window of depth
window <- max(dv)-min(dv)

dr <- as.numeric(nb_d$`Depth Rank`)
max_dr <- max(dr)
min_dr <- min(dr)

plot(nb_d$`Depth (m)`, nb_d$`Depth Rank`, xlab='Depth(meters)', 
     ylab='Depth Rank', type='l', col='darkblue', lwd=2,
     main='Input Data for Spectral Analysis'
)

#________________________________________________________________________________#
### Step 2. De-mean the data
nb_d.avg <- mean(dr)
nb_d.dm <- as.numeric(nb_d$`Depth Rank`) - nb_d.avg

# plot
plot(nb_d$`Depth (m)`, nb_d$`Depth Rank`, xlab='Depth(meters)', 
     ylab='', type='l', col='darkblue', lwd=2,
     main='Demeaned Input Data(Red Curve)', ylim=c(-4,4)
)
abline(h=nb_d.avg, lty=2, col='darkblue')
lines(nb_d$`Depth (m)`, nb_d.dm, col='red')
abline(h=0, lty=2, col='red')
### Step 3. Filter the data

# Filtering - White noise removed
# (3pt moving "Hanning" average)
# 0.5*n + 0.25*(n-1 + n+1)
hanning_ma <- function(df, n) {
  x <- 0.5 * df[n] + 0.25*(df[n-1] + df[n+1])
  return(x)
}

nb_d.filt <- c()
i <- 1
nr <- length(nb_d.dm)
for (n in 2:(nr-1)) {
  h <- hanning_ma(nb_d.dm, n)
  nb_d.filt <- c(nb_d.filt, h)
}

# plot
plot(nb_d$`Depth (m)`, nb_d.dm, xlab='Depth(meters)', 
     ylab='Value', type='l', col='red', lwd=1,
     main='Demeaned + Filtered Input Data', ylim=c(-4,4)
)
abline(h=0, lty=2, col='red')
lines((nb_d$`Depth (m)`)[2:(nr-1)], nb_d.filt, col='green')

### Step 5. Split-Cosine Tapering
nb_d.tap <- nb_d.filt
i <- 1
nr <- length(nb_d$`Depth (m)`)
n <- length(nb_d.filt)

tap_perc_n <- n * 10/100 # 10% tapering to reduce the effect of the terminations of data at the ends of the window

for (j in 1:tap_perc_n) {
  w = j * pi / tap_perc_n
  nb_d.tap[j] = nb_d.filt[j] * 0.5 * (1-cos(w))
}
for (j in (n-tap_perc_n):n) {
  w = (512-j) * pi / tap_perc_n
  nb_d.tap[j] = nb_d.filt[j] * 0.5 * (1-cos(w))
}

# plot
plot(nb_d$`Depth (m)`, nb_d.dm, xlab='Depth(meters)', 
     ylab='', type='l', col='red', lwd=1,
     main='Demeaned + Filtered Input Data', ylim=c(-8,8)
)
abline(h=0, lty=2, col='red')
lines((nb_d$`Depth (m)`)[2:(nr-1)], nb_d.filt, col='green')
lines((nb_d$`Depth (m)`)[2:(nr-1)], nb_d.tap, col='black')

#________________________________________________________________________________#
### Step 6. Fourier Spectral Analysis
nb_d.fft <- fft(nb_d.tap)
#par(mfrow=c(2,1))
plot(nb_d$`Depth (m)`, nb_d.tap, xlab='Depth(meters)', 
     ylab='', type='l', col='red', lwd=1,
     main='Demeaned + Filtered Input Data', ylim=c(-8,8)
)
xlab = sprintf('Frequency (cycle per %f meters)', max_d - min_d)
plot.frequency.spectrum(nb_d.fft, 
                        xlim=c(0,200),
                        xlab=xlab, 
                        ylab='Amplitude'#, 
                        #,plot.type = 'h'
                        )

n <- length(nb_d.fft)
R.f <- Mod(nb_d.fft)
I.f <- n * (R.f^2) 
var.I.f <- var(I.f)

N <- length(R.f)
freq <- 1:N
df <- 1/sqrt(12)
lines(freq, R.f/qchisq(0.975, df), t='l', lty=2, col='green')
lines(freq, R.f/qchisq(0.025, df), t='l', lty=2, col='red')

amp <- Mod(nb_d.fft)
N <- length(amp)
amp.h <- amp[0:(N/2)]
x <- 0:(N/2-1)
d <- data.frame(x, amp.h)
d.sorted <- d[order(amp.h, decreasing = TRUE),]
head(d.sorted, 20)
  
hm <- c(8, 35, 26, 2, 110, 131)
plot.show(nb_d.tap, start_time = min_d, time = max_d - min_d, harmonics = hm,
          xlab='Depth (meter)', ylab='', scale=0.5)
hm_l <- paste(hm, ' cycles')
hm_l <- c(hm_l, 'combined cycle')
legend('topleft', legend=hm_l, col = c(1, 2, 3, 4, 5, 6,'darkblue'), 
       lty = rep(1,7), cex=0.40)

Xspec <- spec.pgram(nb_d.tap, log='dB')
ylimits <- c(1/10*min(Xspec$spec), 25*max(Xspec$spec))
plot(Xspec$freq, Xspec$spec, xlim=c(0,0.2), type='l', lwd=2
     #, ylim=ylimits
     )

df <- Xspec$df
lines(Xspec$freq, Xspec$spec/qchisq(0.975, df), t='l', lty=2, col='green')
lines(Xspec$freq, Xspec$spec/qchisq(0.025, df), t='l', lty=2, col='red')

bw <- Xspec$bandwidth

df <- Xspec$df
chi_lo <- df / qchisq(0.025, df)
chi_up <- df / qchisq(0.975, df)

lines(Xspec$freq, Xspec$spec / chi_lo, col='green')
par(new=T)
plot(Xspec$freq, Xspec$spec * chi_up, xlim=c(0,0.2), col='red', type='l')

plot(Xspec, xlim=c(0,0.2))
plot(Xspec$freq, Xspec$spec, xlim=c(0,0.2), type='l', lwd=2)
a <- qchisq(Xspec$spec * chi_up, df =Xspec$df)
lines(Xspec$freq, pchisq(Xspec$spec, df=Xspec$df), col='red', xlim=c(0,0.2)) 


library(psd)
d <- ts(nb_d.tap, frequency=1)
Pspec <- psdcore(d, ntapaer=2)
plot(Pspec$freq, Pspec$spec, type='l', xlim=c(0,0.2))

spp <- spectral_properties(Pspec[["taper"]], db.ci = TRUE) 
psppu <- with(Pspec, create_poly(freq, dB(spec), spp$stderr.chi.upper))
par(new=T)
plot(psppu$x.x, psppu$y.y, lty=2, type='l', 
     xlim=c(0, 0.2), col='green',
     axes=FALSE, xlab='', ylab='')


library(multitaper)
Mspec <- spec.mtm(nb_d.tap, deltat = 0.85, jackknife = TRUE,
                  k=7, nw=4,
                  log='no', lwd=2, 
                  xlim=c(0,0.25))
x <- Mspec$freq
y <- Mspec$spec
plot(x, y, type='l', lwd=2
     ,xlim=c(0,0.2)
     #,log='y'
     )

