set.seed(1234)
N <- 1028
x <- rnorm(N, mean = 0, sd=1)

# Load signal processing
library(signal, warn.conflicts = FALSE)

#construct an FIR filter
f <- c(0, 0.2, 0.2, 0.3, 0.3, 0.5) * 2 #band edges, strictly increasing vector in the range [0, 1] where 1 is the Nyquist frequency. The first element must be 0 and the last element must be 1. If elements are identical, it indicates a jump in frequency response.
m <- c(0, 0, 1, 1, 0, 0) #magnitude at band edges, a vector of length(f).
fw <- signal::fir2(N, f, m)
plot(fw, type='l')
# complex filter response
# Compute the z-plane frequency response of an ARMA model or IIR filter.
fh <- signal::freqz(fw, Fs=1) 
plot(fh)

# lower
f <- c(0, 0.12, 0.12, 0.22, 0.22, 0.5)*2
fwl <- signal::fir2(N, f, m)
fhl <- signal::freqz(fwl, Fs=1)

# upper
f <- c(0, 0.28, 0.28, 0.38, 0.38, 0.5)*2 
fwu <- signal::fir2(N, f, m)
fhu <- signal::freqz(fwu, Fs=1)

# convolution
xf <- signal::filter(fw, x)
# PSD using stats::spectrum
Sx <- spectrum(x, pad=1, plot=TRUE, taper=0.2) 
Sxf <- spectrum(xf, pad=1, plot=TRUE, taper=0.2)



xv <- var(x) 
X <- fft(x) 
class(X)
## [1] "complex" 
length(X)
## [1] 1028

Sa <- Mod(X) # Amplitude spectrum
Sp <- Arg(X) # Phase spectrum
XC <- Conj(X)
all.equal(Se <- Sa**2, Se_2 <- Mod(XC * X), Se_2R <- Mod(X * XC))

fsamp <- 1 # sampling freq, e.g. Hz
fNyq <- fsamp/2 # Nyquist freq
Nf <- N/2 # number of freqs
nyfreqs <- seq.int(from=0, to=fNyq, length.out=Nf) 
S <- Se[2:(Nf+1)] * 2 / N # Finally, the PSD!

# 0) Setup optimization function for dof, using conjugate gradients\\ # min L1 |PSD - Chi^2(dof)|
Chifit <- function(PSD){
            optim(list(dof=0.5), # initial parameter
             function(dof){
               sum(log(PSD)) - sum(log(dchisq(PSD, dof))) 
             }, method="CG")
           } # 1) run optimization
Schi <- Chifit(S)
# Get 'df', the degrees of freedom
print(dof <- Schi$par[[1]])

# compare with the mean and median
c(mSn <- mean(S), median(S))

mSn <- dof
test_norm <- function(sval, nyq, xvar){
  svar <- sval * nyq; 
  return(svar/xvar)
} 
print(xv_1 <- test_norm(mSn, fNyq, xv))
## [1] 1.003214
xv_2 <- sum(S)/Nf * fNyq / xv # an alternate test
all.equal(xv_1, xv_2)
