# ***** PART 1: Demonstrate the impact of tapering
# generate example series with 10 periods: 100, 40, 29, 21, 19, 14, 10, 5, 4 and 3 ka.
ex=cycles(freqs=c(1/100,1/40,1/29,1/21,1/19,1/14,1/10,1/5,1/4,1/3, 1/2.4, 1/1.2, 1/.97),
          amp=c(1,.75,0.01,.5,.25,0.01,0.1,0.05,0.001,0.01, .75, .5, .25), 
          dt = 0.002)
head(ex)

# set zero padding amount for spectral analyses
# (pad= 1 results in no zero padding, pad = 2 will pad the series to two times its original length)
# start with pad = 1, then afterwards evaluate pad=2
pad=1
# calculate the periodogram with no tapering applied (a "rectangular window")
res=periodogram(ex,output=1,padfac=pad)
# save the frequency grid and the power for plotting
freq=res[1]
pwr_rect=res[3]

length(freq$Frequency)
length(pwr_rect$Power)
par(mfrow=c(1,1))


# now compare with results obtained after applying four different tapers:
# Hann, 30% cosine taper, DPSS with a time-bandwidth product of 1, and DPSS
# with a time-bandwidth product of 3
pwr_hann=periodogram(hannTaper(ex,demean=FALSE),output=1,padfac=pad)[3]
pwr_cos=periodogram(cosTaper(ex,p=.3,demean=FALSE),output=1,padfac=pad)[3]
pwr_dpss1=periodogram(dpssTaper(ex,tbw=1,demean=FALSE),output=1,padfac=pad)[3]
pwr_dpss3=periodogram(dpssTaper(ex,tbw=3,demean=FALSE),output=1,padfac=pad)[3]

pwr = pwr_rect

df = data.frame(freq=freq$Frequency, power=pwr$Power)
df = df[order(df$power, decreasing = T), ]
df$period = 1/df$freq
head(df, n=20)
plot(freq$Frequency, pwr_rect$Power, t='l', xlim=c(0,1))



# Blackman tukey
library(timsac)
?auspec
auspec(delta_Dt.filt, lag=1/3)


n = dim(eccentricity_df)[1]


# frequency
n = dim(eccentricity_df)[1]
lag <- 4 * sqrt(n)
lag1 <- lag + 1
x <- rep(0, lag1)
for(i in 1:lag1) 
  x[i] <- (i - 1) / (2 * lag)

#a <- auspec(eccentricity_df$eccentricity, window = 'Hanning')
a <- auspec(eccentricity_df$eccentricity, lag=lag)

b<-bispec(eccentricity_df$eccentricity)

plot(a$stat, t='l')
plot(b$pspec, t='l')

length(a$stat)

dt = 0.002
fmax  = 1/(2 * dt)
N = length(a$stat)
N
fmin = fmax/N
fmin
fmax

fnorm <- (fmax - fmin)*x + fmin 
spec <- a$stat

btdf <- data.frame(freq=fnorm, spec=spec)
plot(btdf, t='l')

#btdf <- btdf[order(btdf$power, decreasing = T),]

d <- btdf
d <- spec.BT(eccentricity_df$eccentricity)
head(d[order(d$spec, decreasing = T),], n=20)

plot(d$freq, d$spec, t='l', xlim=c(0, 7))

length(a$stat)

n

