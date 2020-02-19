PSD <- function(rxx) {
  fft(rxx)
}

lagEstimate <-function(x,k,N=length(x)){
  (1/N)*sum(x[1:(N-k)]*x[(k+1):N])
}

Lags <-function(x,kMax) {
  sapply(0:kMax,lagEstimate,x=x)
}

r1 <- Lags(y, kMax = 45)
par(mfrow=c(2,1))
plot(r1, t='b')

r2 <- acf(y, lag.max=45, plot=F)$acf
plot(r2, t='b')

acsWindowed <- function(x,kMax,Nzero=0){
  rHalf <- c(Lags(x,kMax),rep(0,Nzero))
  c(rev(rHalf[2:length(rHalf)]),rHalf)
}

acsW <- acsWindowed(y,9)
plot(acsW)

Textbook2R <- function(x,N=length(x),foldN=ceiling(N/2)) {
  c(x[foldN:N],x[1:(foldN-1)])
}

W <- c(rep(0,9),rep(1,9),rep(0,9))
W

FFT_W <- Re(fft(Textbook2R(W)))
plot(FFT_W)

BartlettWindow <- function(N,n=seq(0, (N-1)))  {
  1 - abs( (n-(N-1)/2) / ( (N-1)/2) )
}
Wb <- BartlettWindow(19)
plot(Wb)
WbFft <- Re(fft(Textbook2R(Wb)))
plot(WbFft)

Btse <- function(rHat,Wb) {
  Re(fft(rHat*Wb))
}

rHat   <- Textbook2R(acsWindowed(x,kMax=9))
Wb     <- Textbook2R(BartlettWindow(length(rHat)))
Pbtse9 <- Btse(rHat,Wb)
plot(Pbtse9)

rHat   <- Textbook2R(acsWindowed(x,kMax=18))
Wb     <- Textbook2R(BartlettWindow(length(rHat)))
Pbtse18 <- Btse(rHat,Wb)
plot(Pbtse18)


bt <- btpsd(y)
plot(y)
plot(bt)

par(mfrow=c(3,1))
y <- obliquity_df$obliquity
yspec <- spec.pgram(y, plot=F)
plot(yspec$freq, log(yspec$spec), t='l', )

yspec2 <- spec.ar(y)
plot(yspec2$freq, log(yspec2$spec), t= 'l')

library(astrochron)
yspec3 <- mtm(obliquity_df, detrend = T, output=1, pl=2)

plot(yspec3$freq, log(yspec3$spec), t='l')

yspec3$freq
#plot(spec2$freq, log(spec2$spec), t='l', xlab='freq', ylab='power')

dt <- diff(obliquity_df$age)[1]  
dt
Nspec3 <- length(yspec3$spec)
f_s <- 1/dt # sampling frequency
f_max <- 1/(2*dt) # Nyquist freq f_N sampling theorem: 2 * f_max <=  * f_s
f_min <- f_max/Nspec3
freqMTM <- seq(from=f_min,to=f_max, length.out=Nspec3)

yspec3$Period <- 1/yspec3$Frequency
yspec3 <- yspec3[,c("Frequency","Period","AR1_CL","Harmonic_CL")]
yspec31 <- subset(yspec3, (yspec3$Harmonic_CL >=90 & yspec3$AR1_CL >= 99) )
yspec32 <- yspec3[order(yspec3$Harmonic_CL, decreasing = T),]
head(yspec31)
yspec31

plot(yspec3$freq, log(yspec3$spec), t='l')

yspec4 <- mtm(eccentricity_df, detrend = T, output=1, pl=2)

plot(yspec4$Frequency, yspec4$Power, t='l')


# eccentricity calc

# periodogram
yspec42 <- spec.pgram(eccentricity_df$eccentricity)
yspec42$freqN <- normalize(yspec42$freq, a = f_min, b=f_max)

plot(yspec42$freq, yspec42$spec, t='l')
# multitaper
plot(yspec42$freqN, yspec42$spec, t='l')

# blackman tukey
# Blackman Tukey
N <- length(eccentricity_df$eccentricity)
sp <- btpsd(eccentricity_df$eccentricity, win=N/2)
spec3$freq <- yspec42$freq
ns <- length(spec3$spec)
spec3$spec <- sp[1:(ns/2)]
ns2 <- length(spec3$spec)
plot(spec3$spec, t='l')

par(mfrow=c(1,1))
p = 0.05
plot(yspec42$freqN, (yspec42$spec), 
     t='l',
     xlim=c(0, p * max(yspec42$freqN)),
     yaxt='n',
     xlab='Frequency (1/Myr)', ylab='Power')
par(new=T)
plot(yspec42$freq, (yspec42$spec), 
     col='red',t='l', 
     xlim=c(0, p * max(yspec42$freq)),
     axes=F, 
     xlab='',ylab='')
par(new=T)
plot(0:(ns2-1), (spec3$spec), 
     col='green',t='l', 
     xlim=c(0, p * (ns2-1)),
     axes=F, 
     xlab='',ylab='')


# Foram turnover
# periodogram
plot(PF_dff$age, PF_dff$N.turnover, t='l')
yspec42 <- spec.pgram(PF_dff$N.turnover)
yspec42$freqN <- normalize(yspec42$freq, a = f_min, b=f_max)

#foram
PF_dffm <- PF_dff[PF_dff$age<=34,]
df <- na.omit(data.frame(x=PF_dffm$age, y=PF_dffm$N.turnover))
yspec4 <- mtm(df, ar1=F, detrend = T, output=1, pl=2)

#nannos
NN_dffm <- NN_dff[NN_dff$age<=34,]
df <- na.omit(data.frame(x=NN_dffm$age, y=NN_dffm$N.turnover))
yspec5 <- mtm(df, ar1=F, detrend = T, output=1, pl=2)

#lineage
PFL_dffm <- PFL_dff[PFL_dff$age<=34,]
df <- na.omit(data.frame(x=PFL_dff$age, y=PFL_dff$N.turnover))
yspec6 <- mtm(df, ar1=F, detrend = T, output=1, pl=2)



plot(yspec4$Frequency, yspec4$Power, t='l')

plot(yspec42$freq, yspec42$spec, t='l')
# multitaper
plot(yspec42$freqN, yspec42$spec, t='l')

# blackman tukey
# Blackman Tukey
N <- length(PF_dff$N.turnover)
sp <- btpsd(eccentricity_df$eccentricity, win=N/2)
spec3$freq <- yspec42$freq
ns <- length(spec3$spec)
spec3$spec <- sp[1:(ns/2)]
ns2 <- length(spec3$spec)
plot(spec3$spec, t='l')

par(mfrow=c(1,1))
p = 0.05
plot(yspec42$freqN, (yspec42$spec), 
     t='l',
     xlim=c(0, p * max(yspec42$freqN)),
     yaxt='n',
     xlab='Frequency (1/Myr)', ylab='Power')
par(new=T)
plot(yspec42$freq, (yspec42$spec), 
     col='red',t='l', 
     xlim=c(0, p * max(yspec42$freq)),
     axes=F, 
     xlab='',ylab='')
par(new=T)
plot(0:(ns2-1), (spec3$spec), 
     col='green',t='l', 
     xlim=c(0, p * (ns2-1)),
     axes=F, 
     xlab='',ylab='')


#--- some test ---
  

par(mfrow=c(2,1))
x <- seq(0, 1, length.out=108)
f <- 2
t <- 2* pi * f * x
y <- sin(t)
plot(t,y)

N <- length(t)
N
t_max <- max(t)
t_min <- min(t)

# sampling frequency
f_s <- (t_max - t_min) / (N-1)
f_s
t

# nyquist frequency
f_N <- f_s / 2
f_N

freq <- seq.int(from = xfreq/N, by = xfreq/N, length.out = Nspec)

normalize <- function(x, a=0, b=1) {
  x_norm <- (x-min(x))/(max(x) - min(x))
  x_final <- (b-a) * x_norm + a
  x_final
}

freq1 <- seq(0, f_N, length.out = as.integer(N/2))
freq1
freqN1 <- length(freq1)
freqN1

freq1 <- normalize(freq1, 0, 0.5)
freq1R <- round(freq1, 3)
freq1R[2:length(freq1R)]


spec1 <- spec.pgram(y, fast=T)
spec1$n.used
freq2 <- spec1$freq
freq2R <- round(freq2,3)
freq2R

freqN2 <- length(freq2)
freqN2

d <- data.frame(x=t, y=y)
spec2 <- mtm(d, detrend= T, output=1, ar1=T, pl=1)
spec2$freq

#periodogram
plot(spec1$freq, spec1$spec, t='l')
plot(spec2$Frequency, spec2$Power, t='l', xlab='freq', ylab='power')

# Blackman Tukey
N <- length(y)
sp <- btpsd(as.ts(y), win=N/2)
spec3$freq <- spec1$freq
spec3$spec <- sp

plot(spec3$freq, spec3$spec, t='l')

dt <- diff(t)[1] 
Nspec2 <- length(spec2$spec)
f_s <- 1/dt # sampling frequency
f_max <- 1/(2*dt) # Nyquist freq f_N sampling theorem: 2 * f_max <=  * f_s
f_min <- f_max/Nspec2
freqMTM <- seq(from=f_min,to=f_max, length.out=Nspec2)

yts <- as.ts(y)
yts
yfreq <- frequency(yts) 
yfreq
Nspec <- floor(N/2)
freq <- seq.int(from = yfreq/N, by = yfreq/N, length.out = Nspec)
freq



-------------------normalization + kernel -------------------------------
x <- sample(10)
sort(x)
x_norm <- normalize(x, 0, 0.5)
sort(x_norm)


hist(x, breaks=50)
hist(x_norm, breaks=50)


#daniell kernel (rectangular window)
y.orig <- sample(100)
y <- y.orig
plot(y, t='l')
kern <- kernel("daniell",5)
par(new=T)
plot(c(rev(kern$coef),kern$coef), col='red', t='l', axes=F, xlab='', ylab='')
yk <- kernapply(y, kern)
plot(yk, t='l')

#modified daniell kernel (rectangular window with side lower)
y <- y.orig
plot(y, t='l')
kern <- kernel("modified.daniell",5)
par(new=T)
plot(c(rev(kern$coef),kern$coef), col='red', t='l', axes=F, xlab='', ylab='')
yk <- kernapply(y, kern)
plot(yk, t='l')

#dirichlet kernel with order (r), damped in the side (side lobe)
y <- y.orig
plot(y, t='l')
kern <- kernel("dirichlet",20, r=4)
par(new=T)
plot(c(rev(kern$coef),kern$coef), col='red', t='l', axes=F, xlab='', ylab='')
yk <- kernapply(y, kern)
plot(yk, t='l')

#fejer kernel
y <- y.orig
plot(y, t='l')
kern <- kernel("fejer",20, r=10)
par(new=T)
plot(c(rev(kern$coef),kern$coef), col='red', t='l', axes=F, xlab='', ylab='')
yk <- kernapply(y, kern)
plot(yk, t='l')


demo(smooth)
