# install.packages('multitaper')
# install.packages('RSEIS')

library(multitaper)
library(RSEIS)

# Slepian Taper
nwin = 5
npi = 3
npoints = 1024
# https://github.com/cran/RSEIS/blob/master/R/get.slepians.R
sleps = get.slepians(npoints, nwin, npi)
linwds = rep(1, times=nwin)
linwds[1:2] = 2

matplot(sleps, type='l', xlab='Sample Index', ylab='Taper Amplitude')
legend('bottomright', legend=1:nwin, lty=1:nwin, col=1:nwin)
title(main='Splepian Tapers, npi=3')


# Power Spectrum Estimates
singleTaper <- function(y, dt, tappercent=0.1) {
  y = y - mean(y)
  if (missing(tappercent))
    tappercent = 0.1
  
  N = length(y)
  fn = 1/(2*dt)
  tapy = spec.taper(y, p = tappercent)
  # tapy = tapy - mean(tapy)
  
  Y = fft(tapy)
  Pyy = (Mod(Y)^2)/(N*N)
  ## Pyy = Y * Conj(Y)
  n = floor(length(Pyy)/2)
  Syy = Pyy[1:n]
  f = (0:(length(Syy)-1)) * fn / length(Syy)
  
  plot(f, Syy, type='l' , xlab = 'frequency', ylab='Power Density')
  invisible(list(f=f, syy=Syy))
}

singleTaper(ldeaths, dt=1/12, tappercent = 0.3)

multtap <- function(y, dt, tappercent=0.1) {
  y = y - mean(y)
  nn = next2(length(y)) # next power of 2 greater than n
  
  Mspec = mtapspec(y, dt, klen=nn, 
                   MTP = list(kind=2,
                              nwin=2,
                              npi=4,
                              inorm=1))
  f = Mspec$spec
  amp1 = Mspec$spec[1:length(f)]
  
  ###  
  singy = singleTaper(y, dt, tappercent=tappercent)
  
  stap1 = 10 * log10(singy$syy)
  

  frange = range(c(f))
  amprange = range(c(amp, stap1))
  
  #plot(frange, amprange, t='n', )
  
  plot(f, amp, ylab='Spectrum Amplitude', xlab='Frequency', type='l')
  
  #lines(ZIM$freq, AutoR, col='red')
  lines(singy$f, stap1, col='blue')
  
  title('Lees and Park (1995) multitaper spectrum')
  legend('topright', lty=1:2, col=c('black', 'blue'), legend=c('multitaper', 'Single-taper'))
  
}

y <- ldeaths
dt <- 1/12
multtap(y, dt=dt)


f1 = 20
f2=30
dt = 0.01
t = seq(0, 6, by=dt)
noise = runif(length(t), 0, 2)
y = 2*sin(2*pi*f1*t) + 3*sin(2*pi*f2*t) + noise

plot(t,y, t='l')
title("Two Sinusoids with Noise")

tappercent = 0.05
singleTaper(y, dt = dt, tappercent = tappercent)

multtap(y, dt, tappercent = tappercent)


#Code: https://github.com/wesleyburr/multitaper/blob/b3e7ca814a3e641f0c68d42cb434dc6d2f0ec735/R/multitaper.R

##     The multitaper R package
##     Multitaper and spectral analysis package for R
##     Copyright (C) 2011 Karim Rahim 
##
##     Written by Karim Rahim and Wesley S. Burr.
##
##     This file is part of the multitaper package for R.
##     http://cran.r-project.org/web/packages/multitaper/index.html
## 
##     The multitaper package is free software: you can redistribute it and 
##     or modify it under the terms of the GNU General Public License as 
##     published by the Free Software Foundation, either version 2 of the 
##     License, or any later version.
##
##     The multitaper package is distributed in the hope that it will be 
##     useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
##     of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##     GNU General Public License for more details.
##
##     You should have received a copy of the GNU General Public License
##     along with multitaper.  If not, see <http://www.gnu.org/licenses/>.
##
##     If you wish to report bugs please contact the author:
## 
##     Karim Rahim
##     karim.rahim@gmail.com
##     

##############################################################
##
##  spec.mtm
##
##  Wrapper routine for .spec.mtm.dpss and .spec.mtm.sine.
##
##############################################################
spec.mtm <- function(timeSeries,
                     nw=4.0,
                     k=7,
                     nFFT="default", 
                     taper=c("dpss"),
                     centre=c("Slepian"),
                     dpssIN=NULL,
                     returnZeroFreq=TRUE,
                     Ftest=FALSE,
                     jackknife=FALSE,
                     jkCIProb=.95,
                     adaptiveWeighting=TRUE,
                     maxAdaptiveIterations=100,
                     plot=TRUE,
                     na.action=na.fail,
                     returnInternals=FALSE,
                     sineAdaptive=FALSE,
                     sineSmoothFact=0.2,
                     dtUnits=c("default"),
                     deltat=NULL,
                     ...) {
  
  series <- deparse(substitute(timeSeries))
  taper <- match.arg(taper,c("dpss","sine"))
  centre <- match.arg(centre,c("Slepian","arithMean","trimMean","none"))
  dtUnits <- match.arg(dtUnits,c("second","hour","day","month","year","default"))
  
  ## deal with depreciated parameter dT is changed to deltat
  ## we strip dT before plotting in plotsHelper.R
  ## to prevent it getting passed to plot
  
  deltaT <- NULL
  if(!missing(deltat)) {
    deltaT <- deltat
  }
  
  dT <- match.call(expand.dots = )$dT
  
  if(missing(deltat) && !is.null(dT)) {
    warning("dT has been depreciated. Use either deltat or input a time series object.")
    deltaT <- dT
  }
  
  if( (taper=="sine") && is.complex(timeSeries)) {
    stop("Sine tapering not implemented for complex time series.") 
  }
  if( (taper=="sine") && jackknife) { 
    warning("Cannot jackknife over sine tapers.")
    jackknife <- FALSE
  }
  if( (taper=="sine") && Ftest) { 
    warning("Cannot compute Ftest over sine tapers.")
    Ftest <- FALSE
  } 
  if( (taper=="sine") && !returnZeroFreq) {
    returnZeroFreq = TRUE 
    warning("returnZeroFreq must be TRUE for sine taper option.")
  }
  if( (taper=="sine") && sineSmoothFact > 0.5) {
    warning("Smoothing Factor > 0.5 is very high!")
  }
  
  dtTmp <- NULL
  ## warning for deltaT missing: makes all frequency plots incorrect
  if(!is.ts(timeSeries) && is.null(deltaT)) {
    warning("Time series is not a ts object and deltat is not set. Frequency array and axes may be incorrect.")
  }
  if(!is.ts(timeSeries)) {
    if(!is.complex(timeSeries)) {
      timeSeries <- as.double(as.ts(timeSeries)) 
    }
  } else {
    ## Order matters here, because as.double breaks the ts() class
    dtTmp <- deltat(timeSeries)
    if(!is.complex(timeSeries)) {
      timeSeries <- as.double(timeSeries)
    }
  }
  
  ## in responese to delta T bug July 2, 2013
  ## modified to remove dT
  
  if(is.null(deltaT)) {
    if(!is.null(dtTmp)) {
      deltaT <- dtTmp
    } else{
      deltaT <- 1.0
    }           
  }
  
  n <- length(timeSeries)
  
  if(taper=="dpss") {
    stopifnot(nw >= 0.5, k >= 1, n > 8)
    ## replace stop if not with warning.
    ## the following was also in stopif not:
    ## nw <= 500, k <= 1.5+2*nw)
    if( nw > 500) {
      warning("nw > 500")
    }
    if( k > 1.5 * 2*nw ) {
      warning("k > 1.5+2*nw")
    }
    if (nw/n > 0.5) { 
      warning("half-bandwidth parameter (w) is greater than 1/2")
    }
    if(k==1) {
      Ftest=FALSE
      jackknife=FALSE
    }
  } else {
    stopifnot(k <= n, k >= 1, n > 8)
  }
  
  na.action(timeSeries)
  if(!is.complex(timeSeries)) {
    sigma2 <- var(timeSeries) * (n-1)/n
  } else {
    sigma2 <- var(Re(timeSeries)) * (n-1)/n + var(Im(timeSeries)) * (n-1)/n 
  }
  
  if(nFFT == "default") {
    nFFT <- 2* 2^ceiling(log2(n))
  } else {
    stopifnot(is.numeric(nFFT))
  }
  stopifnot(nFFT >= n)
  
  ## convert time-series to zero-mean by one of three methods, if set; default is Slepian
  if(centre=="Slepian") {
    if(taper=="dpss") {
      timeSeries <- centre(timeSeries, nw=nw, k=k, deltaT=deltaT)    
    } else {  # edge case: sine taper, set initial k, but too high for default nw=4.0
      timeSeries <- centre(timeSeries, nw=5.0, k=8, deltaT=deltaT)
    }
  } else if(centre=="arithMean") {
    timeSeries <- centre(timeSeries, trim=0) 
  } else if(centre=="trimMean") {
    timeSeries <- centre(timeSeries, trim=0.10)
  }
  
  if(taper=="dpss") { 
    mtm.obj <- .spec.mtm.dpss(timeSeries=timeSeries,
                              nw=nw, k=k, nFFT=nFFT, 
                              dpssIN=dpssIN, returnZeroFreq=returnZeroFreq, 
                              Ftest=Ftest, jackknife=jackknife, jkCIProb=jkCIProb, 
                              adaptiveWeighting = adaptiveWeighting, 
                              maxAdaptiveIterations=maxAdaptiveIterations, 
                              returnInternals=returnInternals, 
                              n=n, deltaT=deltaT, sigma2=sigma2, series=series,
                              dtUnits=dtUnits, ...) 
  } else if(taper=="sine") {
    mtm.obj <- .spec.mtm.sine(timeSeries=timeSeries, k=k, sineAdaptive=sineAdaptive,
                              nFFT=nFFT, dpssIN=dpssIN, returnZeroFreq=returnZeroFreq,
                              returnInternals=FALSE, n=n, deltaT=deltaT, sigma2=sigma2,
                              series=series,maxAdaptiveIterations=maxAdaptiveIterations,
                              smoothFact=sineSmoothFact, dtUnits=dtUnits, ...)
  }
  
  if(plot) {
    plot.mtm(mtm.obj, jackknife=jackknife, ...)
    return(invisible(mtm.obj))
  } else {
    return(mtm.obj)
  }
}

##############################################################
##
##  .spec.mtm.dpss
##
##  Computes multitaper spectrum using Slepian tapers
##  References: 
##    Percival and Walden "Spectral Analysis
##    for Physical Applications" 1993 and associated LISP code
##
##    Thomson, D.J. Spectrum Estimation and Harmonic Analysis,
##    Proceedings of the IEEE, 1982 and associated Fortran code
## 
##############################################################
.spec.mtm.dpss <- function(timeSeries,
                           nw,
                           k,
                           nFFT,
                           dpssIN,
                           returnZeroFreq,
                           Ftest,
                           jackknife,
                           jkCIProb,
                           adaptiveWeighting, 
                           maxAdaptiveIterations,
                           returnInternals,
                           n,
                           deltaT,
                           sigma2,
                           series,
                           dtUnits,
                           ...) {
  
  # Complex check case
  if(is.complex(timeSeries)) {
    if(!returnZeroFreq) {
      returnZeroFreq <- 1 
      warning("Cannot set returnZeroFreq to 0 for complex time series.")
    } 
  }
  
  dw <- NULL
  ev <- NULL
  receivedDW <- TRUE
  
  if(!.is.dpss(dpssIN)) {
    receivedDW <- FALSE
    dpssIN <- dpss(n, k, nw=nw, returnEigenvalues=TRUE)
    dw <- dpssIN$v*sqrt(deltaT)
    ev <- dpssIN$eigen 
  }
  else {
    dw <- .dpssV(dpssIN)
    ev <- .dpssEigen(dpssIN)
    if(all(is.null(ev))) {
      ev <- dpssToEigenvalues(dw, nw) }
    dw <- dw*sqrt(deltaT) 
  }
  
  nFreqs <- nFFT %/% 2 + as.numeric(returnZeroFreq)
  offSet <- if(returnZeroFreq) 0 else 1 
  
  # Note that the frequency axis is set by default to unit-less
  # scaling as 0 through 0.5 cycles/period. The user parameter
  # dtUnits modifies this scaling in the plot.mtm function.
  scaleFreq <- 1 / as.double(nFFT * deltaT)
  
  swz <- NULL ## Percival and Walden H0
  ssqswz <- NULL
  swz <- apply(dw, 2, sum)
  if(k >= 2) {
    swz[seq(2,k,2)] <- 0
  }
  ssqswz <- as.numeric(t(swz)%*%swz)
  
  taperedData <- dw * timeSeries
  
  nPadLen <- nFFT - n
  if(!is.complex(timeSeries)) {
    paddedTaperedData <- rbind(taperedData, matrix(0, nPadLen, k))
  } else {
    paddedTaperedData <- rbind(taperedData, matrix(complex(0,0), nPadLen, k)) 
  }
  cft <- mvfft(paddedTaperedData)
  if(!is.complex(timeSeries)) {
    cft <- cft[(1+offSet):(nFreqs+offSet),]
  } else {
    cft <- rbind(cft[(nFreqs+offSet+1):nFFT,],cft[(1+offSet):(nFreqs+offSet),])
  }
  sa <- abs(cft)^2
  
  if(!is.complex(timeSeries)) {
    resultFreqs <- ((0+offSet):(nFreqs+offSet-1))*scaleFreq 
  } else {
    resultFreqs <- (-(nFreqs-1):(nFreqs-2))*scaleFreq
  }
  
  adaptive <-  NULL
  jk <- NULL
  PWdofs <- NULL
  if(!jackknife) {
    if(!is.complex(timeSeries)) {
      adaptive <- .mw2wta(sa, nFreqs, k, sigma2, deltaT, ev)
    } else {
      adaptive <- .mw2wta(sa, nFFT, k, sigma2, deltaT, ev) 
    }
  } else {
    stopifnot(jkCIProb < 1, jkCIProb > .5)
    if(!is.complex(timeSeries) & adaptiveWeighting) {
      adaptive <- .mw2jkw(sa, nFreqs, k, sigma2, deltaT, ev)
    } else {
      adaptive <- .mw2jkw(sa, nFFT, k, sigma2, deltaT, ev)
    }
    scl <- exp(qt(jkCIProb,adaptive$dofs)*
                 sqrt(adaptive$varjk))
    upperCI <- adaptive$s*scl
    lowerCI <- adaptive$s/scl
    minVal = min(lowerCI)
    maxVal = max(upperCI)
    jk <- list(varjk=adaptive$varjk,
               bcjk=adaptive$bcjk,
               sjk=adaptive$sjk,
               upperCI=upperCI,
               lowerCI=lowerCI,
               maxVal=maxVal,
               minVal=minVal)
  } 
  
  ftestRes <- NULL
  
  if(Ftest) {
    if(is.null(swz)) {
      swz <- apply(dw, 2, sum)
    }
    ftestRes <- .HF4mp1(cft,
                        swz,
                        k,
                        ssqswz)
  }
  
  eigenCoef1 <- NULL
  wtCoef1 <- NULL
  
  if(returnInternals) {
    eigenCoef1 <- cft
    if(adaptiveWeighting) {
      wtCoef1 <- sqrt(adaptive$wt)
    } else {
      wtCoef <- rep(1, nFreqs)
    }
  }
  auxiliary <- list(dpss=dpssIN,
                    eigenCoefs=eigenCoef1,
                    eigenCoefWt=wtCoef1,
                    nfreqs=nFreqs,
                    nFFT=nFFT,
                    jk=jk,
                    Ftest=ftestRes$Ftest,
                    cmv=ftestRes$cmv,
                    dofs=adaptive$dofs,
                    nw=nw,
                    k=k,
                    deltaT=deltaT,
                    dtUnits=dtUnits,
                    taper="dpss")
  
  ##   Thomson, D.J. Spectrum Estimation and Harmonic Analysis,
  ##   Proceedings of the IEEE, 1982.
  
  ## note that the weights are squared, they are |d_k(f)^2 from equation
  ## (5.4)
  ## These weights correspond to Thomoson's 1982 Fortran code.
  ## dof fix for one taper, only value.
  if(k==1) {
    auxiliary$dofs <- 2
  }
  
  spec.out <- list(origin.n=n,
                   method="Multitaper Spectral Estimate",
                   pad= nFFT - n,
                   spec=adaptive$s,
                   freq=resultFreqs,
                   series=series,
                   adaptive=adaptiveWeighting, 
                   mtm=auxiliary)
  
  class(spec.out) <- c("mtm", "spec")
  
  if(Ftest) {
    class(spec.out) <- c("mtm", "Ftest", "spec")
  }
  return(spec.out)
}


#########################################################################
##
##  spec.mtm.sine 
##
##  Computes multitaper spectrum estimate using sine tapers, as in
## 
##  Riedel, Kurt S. and Sidorenko, Alexander, Minimum Bias Multiple 
##    Taper Spectral Estimation. IEEE Transactions on Signal Processing,
##    Vol. 43, No. 1, January 1995.
##
##  Algorithm implementation based on previous work by:
##    German Prieto, Universidad de los Andes
##       via \texttt{mtsepc}, a F90 package that can be found at
##       http://wwwprof.uniandes.edu.co/~gprieto/software/mwlib.html
##
##    and
##
##    Robert L. Parker, Scripps Institution of Oceanography
##      via \texttt{psd.f}, a F77 program that can be found at
##      http://igppweb.ucsd.edu/~parker/Software/Source/psd.f
## 
#########################################################################

.spec.mtm.sine <- function(timeSeries,
                           nFFT,
                           k,
                           sineAdaptive,
                           dpssIN, 
                           returnZeroFreq=TRUE,
                           n, 
                           deltaT, 
                           dtUnits,
                           sigma2,
                           series=series,
                           maxAdaptiveIterations,
                           smoothFact,
                           ...) {
  
  dw <- NULL
  receivedDW <- TRUE
  if(!.is.dpss(dpssIN)) {
    receivedDW <- FALSE
    dpssIN <- sineTaper(n, k)
    dw <- dpssIN$v
  }
  else {
    dw <- .dpssV(dpss)
  }
  
  # returnZeroFreq forced to TRUE, offset = 0
  # NOTE: sine tapers produce nFFT/4 unique results; need to scale nFFT and nFreqs accordingly
  nFFT <- nFFT*2
  nFreqs <- nFFT %/% 4 + as.numeric(returnZeroFreq)
  offSet <- if(returnZeroFreq) 0 else 1 
  scaleFreq <- 1 / as.double(nFFT/2 * deltaT)
  resultFreqs <- ((0+offSet):(nFreqs+offSet-1))*scaleFreq 
  nPadLen <- nFFT - n
  df <- 1/as.double(nFFT*deltaT)
  
  # compute a single FFT; since we are using sine tapers, this is all we need
  ones <- matrix(1,n,1)
  paddedData<- rbind(timeSeries*ones, matrix(0, nPadLen, 1))
  cft <- mvfft(paddedData)
  
  # constant number of tapers, or adaptive?
  spec <- as.double(matrix(0,1,nFreqs))
  
  if(!sineAdaptive) { # constant k tapers
    spec <- (.qsF(nFreqs=nFreqs,nFFT=nFFT,k=k,cft=cft,useAdapt=FALSE,kadapt=c(1)))$spec
    dofs <- NULL
  } else { # adaptively weighted tapers
    
    initTaper <- ceiling(3.0 + sqrt(smoothFact*n)/5.0);
    
    # pilot estimate of S
    spec0 <- (.qsF(nFreqs=nFreqs,nFFT=nFFT,k=k,cft=cft,useAdapt=FALSE,kadapt=c(1)))$spec
    
    out <- .adaptSine (ntimes=maxAdaptiveIterations, 
                       k=initTaper, 
                       nFreqs=nFreqs, 
                       sx=spec0, 
                       nFFT=nFFT, 
                       cft=cft,
                       df=df,
                       fact=smoothFact) 
    spec <- out$spec;
    dofs <- out$kadapt;
  } # end of adaptive logic
  
  # normalize spectrum
  const <- var(timeSeries)/sum(spec)/df
  specFinal <- const*spec
  
  ## set up return object
  
  if(sineAdaptive) { 
    method = "Sine-Taper Multitaper Spectrum (adaptive)"
  } else {
    method = paste("Sine-Taper Multitaper Spectrum (k=",k,")",sep="")
  }
  
  
  auxiliary <- list(dpss=dpssIN,
                    eigenCoefs=NULL,
                    eigenCoefWt=NULL,
                    nfreqs=nFreqs,
                    nFFT=nFFT,
                    jk=NULL,
                    Ftest=NULL,
                    cmv=NULL,
                    dofs=dofs,
                    nw=NULL,
                    k=k,
                    deltaT=deltaT,
                    dtUnits=dtUnits,
                    taper="sine")
  
  spec.out <- list(origin.n=n,
                   method=method,
                   pad= nFFT - n,
                   spec=specFinal,
                   spec = NULL,
                   freq=resultFreqs,
                   series=series,
                   mtm=auxiliary)
  
  class(spec.out) <- c("mtm", "spec")
  return(spec.out)
}

#########################################################################
##
## centre
##
## Takes a time series and converts to zero-mean using one of three 
## methods: Slepian projection, arithmetic mean, or trimmed mean.
## 
#########################################################################

centre <- function(x, nw=NULL, k=NULL, deltaT=NULL, trim=0) {
  na.fail(x)
  res <- NULL
  if(is.null(nw) && is.null(k) ) {
    res <- x - mean(x, trim=trim)
  } else {
    if(trim != 0) {
      warning(paste("Ignoring trim =", trim))
    }
    stopifnot(nw >= 0.5, k >= 1, nw <= 500, k <= 1.5+2*nw)
    if (nw/length(x) > 0.5) { 
      stop("half-bandwidth parameter (w) is greater than 1/2")
    }
    if(is.null(deltaT)) {
      if(is.ts(x)) {
        deltaT <- deltat(ts)
      } else {
        warning("deltaT not specified; using deltaT=1.")
        deltaT <- 1.0
      }
    }
    n <- length(x)
    dpssRes <- dpss(n, k=k, nw=nw,
                    returnEigenvalues=TRUE)
    dw <- dpssRes$v*sqrt(deltaT)
    ev <- dpssRes$eigen
    swz <- apply(dw, 2, sum)
    ## zero swz where theoretically zero; odd tapers
    if(k >=2) {
      swz[seq(2,k,2)] <- 0.0
    }
    ssqswz <- sum(swz^2)
    if(!is.complex(x)) {
      res <- .mweave(x, dw, swz,
                     n, k, ssqswz, deltaT)
      res <- x - res$cntr
    } else {
      res.r <- .mweave(Re(x), dw, swz,
                       n, k, ssqswz, deltaT)
      res.i <- .mweave(Im(x), dw, swz,
                       n, k, ssqswz, deltaT)
      res <- x - complex(real=res.r$cntr, imaginary=res.i$cntr)
    }
  }
  return(res)
}


#########################################################################
##
## jackknife coherence and helper smoother and plotting functions
## 
## Example: 
## jkRes <- jkcoh1(r1$auxiliary$cft, r2$auxiliary$cft,
##                 4,2048,4,4096,395)
## pGreater <-  percentjkMSCGreaterThan(jkRes$msc, 4)
## plotJkcoh1(r1$freqs, jkRes$TRmsc, jkRes$NTvar, 4, pGreater)
##
#########################################################################

mtm.coh <- function(mtm1, mtm2, fr=NULL, tau=0, phcorr = TRUE, 
                    plot=TRUE,...) {
  
  ## note Dave saves the cft
  ## in ./odinlibs-1.1/src/mw/mw2pakt as weighted
  ## 1000 blkcft(n,k,curblk,curset) =
  ##  cft(n*ndecfr,k)*sqrt(wt(n*ndecfr,k))
  
  ## we require auxiliary data
  if(is.null(mtm1$mtm$eigenCoefs) || is.null(mtm2$mtm$eigenCoefs)) {
    stop("Both mtm objects must have been computed with returnInternals=TRUE.")
  }
  
  if(mtm1$mtm$k != mtm1$mtm$k) {
    stop("Both mtm objects must have the same value for k.")
  }
  ##k <- mtm1$auxiliary$
  
  if(mtm1$mtm$nfreqs != mtm1$mtm$nfreqs) {
    stop("Both mtm objects must have the same value for nFFT.")
  }
  
  nord <- mtm1$mtm$k
  nfreqs <- mtm1$mtm$nfreqs
  cft1 <- mtm1$mtm$eigenCoefs
  cft2 <- mtm2$mtm$eigenCoefs
  
  fr <-  if(is.null(fr))  array(as.double(0), nfreqs) else fr
  
  blklof <-  if(nfreqs %%2 ==0) 1 else 0
  blkhif <- nfreqs -1 + blklof
  
  nordP2 <- nord +2
  out <- .Fortran("jkcoh1", cft1=as.complex(cft1),
                  cft2=as.complex(cft2), nord=as.integer(nord),
                  blklof=as.integer(blklof), blkhif=as.integer(blkhif),
                  fr=as.double(fr),  tau=as.double(tau),
                  phcorr=as.integer(phcorr),
                  NTmsc=double(nfreqs), NTvar=double(nfreqs),
                  msc=double(nfreqs), ph=double(nfreqs),
                  phvar=double(nfreqs),
                  s1=double(nordP2), s2=double(nordP2),
                  jkmsc=double(nordP2), TRmsc=double(nordP2),
                  bias=double(nfreqs),
                  cx=complex(nordP2),
                  PACKAGE="multitaper")
  
  auxiliary <- list(nfreqs=mtm1$mtm$nFreqs,
                    nFFT=mtm1$mtm$nFFT,
                    nw=mtm1$mtm$nw,
                    k=mtm1$mtm$k,
                    deltaT=mtm1$mtm$deltaT,
                    dtUnits=mtm1$mtm$dtUnits,
                    taper=mtm1$mtm$taper
  )
  
  
  coh.out <- list(NTmsc=out$NTmsc, NTvar=out$NTvar,
                  msc=out$msc, nfreqs=mtm1$mtm$nfreqs,
                  freq=mtm1$freq, k=nord,
                  ph=out$ph, phvar=out$phvar, mtm=auxiliary)
  class(coh.out) <- "mtm.coh"
  
  
  if(plot) {
    plot.mtm.coh(coh.out, ...)
    return(invisible(coh.out))
  } else {
    return(coh.out)
  }
}

##################################################################
##
##  plot.mtm
##
##  Takes a mtm object, and plots either the associated spectrum
##  (obj$spec) or the harmonic F-test statistic (obj$Ftest).
##
##################################################################
plot.mtm <- function(x, 
                     jackknife=FALSE, 
                     Ftest=FALSE, 
                     ftbase=1.01,
                     siglines=NULL, 
                     ...) {
  
  ## Set frequency axis and label
  dtUnits <- x$mtm$dtUnits
  deltaT <- x$mtm$deltaT
  
  ## if the user has not set 'xlab' ... set it for them:
  if(!hasArg("xlab")) {
    if(!(x$mtm$dtUnits == "default")) {
      xlab <- paste("Frequency in cycles/",dtUnits,sep="") }
    else {
      xlab <- paste("Frequency")
    }
  } 
  
  if(Ftest) {
    if(!hasArg("xlab")) {
      .plotFtest(x,xlab=xlab,siglines=siglines,ftbase=ftbase, ...)
    } else {
      .plotFtest(x, siglines=siglines, ftbase=ftbase, ...)
    }  
  } 
  else 
  { ## plot spectrum only
    ## modified to remove calls to plot.spec
    ## for R version 3.1.0
    ##
    class(x) <- "spec"
    if(x$mtm$taper=="sine") {
      if(!hasArg("xlab")) {
        plot( x, xlab=xlab, sub=" ", ...)
      } else {
        plot( x, sub=" ", ...) 
      }  
    }
    else { ## case of taper=="dpss"
      nw <- x$mtm$nw
      k <- x$mtm$k
      sub <- paste("(NW = ", nw, " K = ", k,")", sep="")
      log <- match.call(expand.dots = )$log
      if(jackknife) {
        dBPlot <- FALSE
        if(!is.null(log) && log== "dB" ) {
          dBPlot <- TRUE }
        if(jackknife && !is.null(x$mtm$jk)) {
          if(dBPlot) {
            upperCI <- 10*log10(x$mtm$jk$upperCI)
            lowerCI <- 10*log10(x$mtm$jk$lowerCI)
            minVal <- 10*log10(x$mtm$jk$minVal)
            maxVal <- 10*log10(x$mtm$jk$maxVal) 
          } 
          else {
            upperCI <- x$mtm$jk$upperCI
            lowerCI <- x$mtm$jk$lowerCI
            minVal <- x$mtm$jk$minVal
            maxVal <- x$mtm$jk$maxVal
          }
          yRange <- c(minVal, maxVal)
          if(!hasArg("xlab")) {
            .lplotSpec( x, xlab=xlab, sub=sub, ylim=yRange, ...)
          } else {
            .lplotSpec( x, sub=sub, ylim=yRange, ...)
          }  
          lines(x$freq, upperCI, lty=2, col=2)
          lines(x$freq, lowerCI, lty=2, col=3)
        }
      }
      else {
        if(!hasArg("xlab")) {
          .lplotSpec( x, xlab=xlab, sub=sub, ...) 
        } else {
          .lplotSpec( x, sub=sub, ...)
        }
      } 
    } ## end of dpss case
  } ## spectrum plot end
} ## end of function

##################################################################
##
##  plot.mtm.coh
##
##  Takes a mtm.coh object, and plots the Magnitude-Squared 
##  Coherence, with multiple y-axes.
##
##################################################################
plot.mtm.coh <- function(x, 
                         percentGreater=NULL,
                         nehlim=10, 
                         nehc=4,
                         cdfQuantilesTicks=NULL,
                         drawPercentLines=TRUE,
                         percentG=c(.1,.2,.5,.8,.9), 
                         ...) {
  
  if(  is.null(x$NTmsc) || is.null(x$NTvar)  || is.null(x$msc)
       || is.null(x$freq) || is.null(x$nfreqs) || is.null(x$k)) {
    stop("Requires mtm.coh object. Run mtm.coh on two mtm objects with returnInternals=TRUE.")
  }
  
  TRmsc <- x$NTmsc
  NTvar <- x$NTvar
  freqs <- x$freq
  nfreqs <- x$nfreqs
  k <- x$k
  
  ##nehlim and nehc are for smoothing 
  ## currently we plot the smoothed transformed coherence
  ## and lower CI after smoothing the variance
  plotTRmsc <- .lftr3p(TRmsc, NTvar, nfreqs,
                       nehlim,nehc, "even", "ext")
  trnrm_ <- .trnrm(k)
  par(oma=c(2,4,0,2))
  plot.new()
  ## note the ... was mainly implemented for xaxs="i"
  ## Undefined behaviour with other options 
  plot.window(range(freqs), range(plotTRmsc[,2]), ...)
  xy <- xy.coords(freqs,plotTRmsc[,2])
  ## plot smoothed msc
  plot.xy(xy, type="l", lwd=1, ...)
  ## plot one sd dev lower jackknife variance
  lines(freqs, plotTRmsc[,1], lty=3, lwd=1)
  box()
  axis(1)
  
  ## allow for user-settable xlabel, or unit display
  if(!hasArg("xlab")) {
    if(!(x$mtm$dtUnits == "default")) {
      xlab <- paste("Frequency in cycles/",x$mtm$dtUnits,sep="") }
    else {
      xlab <- paste("Frequency")
    }
  }
  mtext(xlab, side=1, line=3)
  
  ## basic left axis
  axis(2)
  mtext("Arctanh Transform of MSC",
        side=2, line=2, cex=par()$cex)
  
  ##  outer MSC axis on the left
  msc <- .FtoMSC(plotTRmsc[,2], trnrm_)
  mscTicks <- pretty(msc)
  
  
  ## transform ticks for at
  ##C2toF is coherence to inverse transform
  TRmscTicks <- .C2toF(mscTicks, trnrm_)
  axis(2, at=TRmscTicks, labels=mscTicks, outer=TRUE)
  mtext("Magnitude Squared Coherence", side=2, line=6)
  
  ##mscToCDF values may have issues for highly coherent values
  ## values over .9 will cause issues
  if(is.null(cdfQuantilesTicks)) {
    cdfQuantiles <- .mscToCDFquantiles(msc, k)
    cdfQuantilesTicks <- pretty(cdfQuantiles)
  }
  
  ## put right axis
  Qlvl <- .cdfToMSqCoh(cdfQuantilesTicks, k)
  TRQlvl <- .C2toF(Qlvl, trnrm_)
  
  cumulativeDistVals <- .C2toF(msc, trnrm_)
  axis(4, at=TRQlvl, labels=cdfQuantilesTicks)
  
  mtext("CDF for Independent Data",
        side=4, line=2) 
  
  if(drawPercentLines == TRUE) {
    percentGprob <- percentG
    percentG <- .C2toF(.cdfToMSqCoh(percentG, k),  trnrm_)
    lenPercentG <- length(percentG)
    for(i in 1:lenPercentG) {
      lines(freqs, array(percentG[i], nfreqs), lty=2)
    }
  }
  
  if(!is.null(percentGreater)) {
    mtext(paste("CDF for C=   10.0% 20.0% 50.0% 80.0% 90.0%"),
          side=1, line=4, adj=-1, cex=.8)
    mtext(paste("% of data > Q     ",
                100*round( percentGreater[1], digits=3),
                "% ",
                100*round( percentGreater[2], digits=3),
                "% ",
                100*round( percentGreater[3], digits=3),
                "% ",
                100*round( percentGreater[4], digits=3),
                "% ",
                100*round( percentGreater[5], digits=3),
                "%", sep=""),
          side=1, line=5, adj=-1, cex=0.8)
  }
  return(list(sigProb = percentGprob, sigNT = percentG))
}

# C convertion of fortran code of multitaper djt.f (
https://github.com/wesleyburr/multitaper/blob/b3e7ca814a3e641f0c68d42cb434dc6d2f0ec735/src/djt.f
)
void mw2wta(sa, wt, nfreq, nord, s, ev, evp , dofs, dofav, var, dt, tol, maxadit , mxiter, aviter);
void mw2jkw(sa, wt, nfreq, nord, s, ev, evp , dofs, dofav, var, dt, tol, sjk, varjk, bcjk, wjk, cwjk, vwj , maxadit, mxiter);
void mweave(x, dw, swz, ndata, nord, ssqswz, cntr, dt , spz, varc);
void setdp(npts, val, x);
void adstoa(x, y, ndata, xinc);
void sphsed(ph, nfreq);
void jkcoh1(cft1, cft2, nord, blklof, blkhif , fr, tau, phcorr, NTmsc, NTvar , msc, ph, phvar, s1, s2, jkmsc, TRmsc, bias , cx);


//C$$$    The multitaper R package
//C$$$    Multitaper and spectral analysis package for R
//C$$$    Copyright (C) 2011 Karim J. Rahim David J. Thomson 
//C$$$
  //C$$$    This file is part of the multitaper package for R.
//C$$$
  //C$$$    The multitaper package is free software: you can redistribute it and
//C$$$    or modify
//C$$$    it under the terms of the GNU General Public License as published by
//C$$$    the Free Software Foundation, either version 2 of the License, or
//C$$$    any later version.
//C$$$
  //C$$$    The multitaper package is distributed in the hope that it will be 
//C$$$    useful, but WITHOUT ANY WARRANTY; without even the implied warranty 
//C$$$    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//C$$$    GNU General Public License for more details.
//C$$$
  //C$$$    You should have received a copy of the GNU General Public License
//C$$$    along with multitaper.  If not, see <http://www.gnu.org/licenses/>.
//C$$$
  //C$$$    If you wish to report bugs please contact the author. 
//C$$$    karim.rahim@gmail.com
//C$$$
  
  //cccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
//cc This files contains modified djt multitaper files originally
//cc from libraries written at Bell Labs by David Thomson.

//c**************************************************************************
  //c     mw2wta multitaper using weights

// --------------------------------------------
  void mw2wta(sa, wt, nfreq, nord, s, ev, evp , dofs, dofav, var, dt, tol, maxadit , mxiter, aviter)
{
  
  // implicit none
  int nfreq, nord, maxadit, mxiter, n, niter, k;
  double sa(nfreq, nord), wt(nfreq, nord), dofs(nfreq) , s(nfreq), ev(nord), evp(nord), var, dt, tol, aviter, avewt , wtmin, dofmin, sewn, sbar, dk2, sum, cwt, dofav, wmin , dk2l;
  
  
  //c     Generate Weights
  mxiter = 0;
  aviter = 0.d0;
  avewt = 0.d0;
  wtmin = 1.d0;
  dofmin = dble(2*nord);
  cwt = 0.d0;
  wmin = 0.d0;
  ;
  //c     Equivalent white noise level for bias calculation
  sewn = var*dt;
  for(n=1; n<=nfreq; n++)
  {
    //c        start at estimate based on two best eigenvalues
    sbar = (sa(n, 1) + sa(n, 2))/2.d0;
    dk2 = 1.d0;
    //c     iterate
    for(niter=1; niter<=maxadit; niter++)
    {
      sum = 0.d0;
      cwt = 0.d0;
      wmin = 1.d0;
      dk2l = dk2;
      for(k=1; k<=nord; k++)
      {
        dk2 = (ev(k)*sbar/(ev(k)*sbar + evp(k)*sewn))**2;
        wt(n, k) = dk2;
        sum = sum + sa(n, k)*dk2;
        wmin = dmin1(wmin, dk2);
        cwt = cwt + dk2;
        
      }
      sbar = sum/cwt;
      if (dabs((dk2 - dk2l)/(dk2 + dk2l)) <= tol) exit;
      
    }
    mxiter = max0(mxiter, niter);
    aviter = aviter + niter;
    avewt = avewt + cwt;
    wtmin = dmin1(wtmin, wmin);
    dofs(n) = 2.d0*cwt;
    dofmin = dmin1(dofmin, dofs(n));
    s(n) = sbar;
    aviter = aviter/dble(nfreq);
    
  }
  dofav = 2.d0*avewt/dble(nfreq);
  
  } subroutine;


//c*****end mw2wta

//cc**********************************************************************
  //c     multiwindow jacknifed.

//c     Multi-Window Weighting, Jackknifed
// --------------------------------------------
  void mw2jkw(sa, wt, nfreq, nord, s, ev, evp , dofs, dofav, var, dt, tol, sjk, varjk, bcjk, wjk, cwjk, vwj , maxadit, mxiter)
{
  
  // implicit none
  int nfreq, nord, mxiter, n1, n2, j, n, niter, ks, maxadit , k;
  double sa(nfreq, nord), wt(nfreq, nord), dofs(nfreq) , s(nfreq) , ev(nord), evp(nord), sjk(nord + 2), varjk(nfreq), bcjk(nfreq) , wjk(nord, nord + 2), cwjk(nord + 2), vwj(nord) , total, avewt, wtmin, dofmin, bcor , fnord, vnrm, sewn, var, dt, dofav, varcwt, sbar, sum     , wmin, slast, tol;
  
  //c     Generate Weights
  
  mxiter = 0;
  total = 0.d0;
  avewt = 0.d0;
  wtmin = 1.d0;
  niter = 0;
  sbar = 0.d0;
  wmin = 0.d0;
  dofmin = dble(2*nord);
  bcor = dble(nord - 1);
  fnord = nord;
  vnrm = dble(nord - 1)/fnord;
  n1 = nord + 1;
  n2 = nord + 2;
  //c     Equivalent white noise level for bias calculation
  sewn = var*dt;
  //c
  for(n=1; n<=nfreq; n++)
  {
    //c     iterate
    for(433 ks = 1, nord + 1)\n{
      //c     start at estimate based on two best eigenvalues
      sbar = (sa(n, 1) + sa(n, 2))/2.;
      for(niter=1; niter<=maxadit; niter++)
      {
        sum = 0.;
        cwjk(ks) = 0.d0;
        wmin = 1.d0;
        slast = sbar;
        for(k=1; k<=nord; k++)
        {
          if (k == ks) goto g350;
          wjk(k, ks) = (ev(k)*sbar/(ev(k)*sbar + evp(k)*sewn))**2;
          sum = sum + sa(n, k)*wjk(k, ks);
          wmin = dmin1(wmin, wjk(k, ks));
          cwjk(ks) = cwjk(ks) + wjk(k, ks);
          
        }
        sbar = sum/cwjk(ks);
        sjk(ks) = dlog(sbar);
        if (dabs((sbar - slast)/(sbar + slast)) <= tol) exit;
        
      }
      
    }
    
    //c     Jackknife mean, variance of Log S
    sjk(n2) = 0.d0;
    cwjk(n2) = 0.d0;
    for(k=1; k<=nord; k++)
    {
      wjk(k, n2) = 0.d0;
      
    }
    for(k=1; k<=nord; k++)
    {
      cwjk(n2) = cwjk(n2) + cwjk(k);
      sjk(n2) = sjk(n2) + sjk(k);
      for(j=1; j<=nord; j++)
      {
        wjk(j, n2) = wjk(j, n2) + wjk(j, k);
        
      }
      
    }
    sjk(n2) = sjk(n2)/fnord;
    cwjk(n2) = cwjk(n2)/fnord;
    for(j=1; j<=nord; j++)
    {
      vwj(j) = 0.d0;
      wjk(j, n2) = wjk(j, n2)/fnord;
      wt(n, j) = wjk(j, n2);
      
    }
    ;
    //c     Jackknife Bias Estimate (Log S )
    bcjk(n) = bcor*(sjk(n2) - sjk(n1));
    ;
    //c     Variance Estimate
    varjk(n) = 0.d0;
    varcwt = 0.d0;
    for(k=1; k<=nord; k++)
    {
      varjk(n) = varjk(n) + (sjk(k) - sjk(n2))**2;
      varcwt = varcwt + (cwjk(k) - cwjk(n2))**2;
      for(j=1; j<=nord; j++)
      {
        vwj(j) = vwj(j) + (wjk(j, k) - wjk(j, n2))**2;
        
      }
      
    }
    ;
    varjk(n) = varjk(n)*vnrm;
    mxiter = max0(mxiter, niter);
    total = total + niter;
    avewt = avewt + cwjk(n1);
    wtmin = dmin1(wtmin, wmin);
    dofs(n) = 2.d0*cwjk(n1);
    dofmin = dmin1(dofmin, dofs(n));
    s(n) = sbar;
    
  }
  dofav = 2.d0*avewt/float(nfreq);
  
  } subroutine;


//c ****** end mw2jkw


//c     Multi-Window Average Estimation
// --------------------------------------------
  void mweave(x, dw, swz, ndata, nord, ssqswz, cntr, dt , spz, varc)
{
  // implicit none
  int ndata, nord, n, k, nnx;
  double x(ndata), dw(ndata, nord), swz(nord) , sm(nord), sum, spz, zero8, dt , ssqswz, cntr, varc;
  data zero8/0.d + 00/;
  
  //c     no need for a max of 9 
  nnx = nord;
  
  setdp(nnx, zero8, sm);
  for(k=1; k<=nnx; k++)
  {
    for(n=1; n<=ndata; n++)
    {
      sm(k) = sm(k) + dw(n, k)*x(n);
      
    }
    
  }
  
  sum = zero8;
  spz = zero8;
  for(300 k = 1, nnx, 2)\n{
    sum = sum + swz(k)*sm(k);
    
  }
  sum = sum/ssqswz;
  for(k=1; k<=nnx; k++)
  {
    spz = spz + (sm(k) - sum*swz(k))**2;
    
  }
  spz = spz/dble(nnx);
  varc = spz/(dt*dble(ndata));
  cntr = sum;
  } subroutine;


//c ******** end    mweave   

//c     Set Real*8 array
// --------------------------------------------
  void setdp(npts, val, x)
{
  // implicit none
  int npts, n;
  double val, x(npts);
  for(n=1; n<=npts; n++)
  {
    x(n) = val;
    
  }
  
  } subroutine;


//c ******* end setdp

//c ************************** helper functions used in coherence calculation
//c djt/ts/ adstoa.f  Add Scalar to Array
// --------------------------------------------
  void adstoa(x, y, ndata, xinc)
{
  // implicit none
  int n, ndata;
  double x(ndata), y(ndata), xinc;
  //c     djt/ts/tsu1 -2- add scalar to array
  for(n=1; n<=ndata; n++)
  {
    y(n) = x(n) + xinc;
    
  }
  
  } subroutine;


//c ********** end adstoa

//c djt/ts/sphsed.f   Basic Phase Unwrapping Routine, Degrees
// --------------------------------------------
  void sphsed(ph, nfreq)
{
  // implicit none
  int nfreq, n;
  double ph(nfreq), q, pinc, d, t;
  
  q = 0.d0;
  pinc = 0.d0;
  for(n=1; n<=nfreq; n++)
  {
    t = ph(n);
    d = q - t;
    q = t;
    if (dabs(d) > 180.d0) pinc = pinc + dsign(360.d0, d);
    ph(n) = t + pinc;
    
  }
  
  } subroutine;


//c ****** end       sphsed


//c*********************************************************************
  //cc calculated coherence estimates

// --------------------------------------------
  void jkcoh1(cft1, cft2, nord, blklof, blkhif , fr, tau, phcorr, NTmsc, NTvar , msc, ph, phvar, s1, s2, jkmsc, TRmsc, bias , cx)
{
  
  // implicit none
  int n1, n2, ks, nav, phcorr, blklof, blkhif , k, kc, n, nord, nfreqs;
  double fr(blklof:blkhif), tau , ph(blklof:blkhif), NTmsc(blklof:blkhif), s1(nord + 2) , s2(nord + 2) , jkmsc(nord + 2), TRmsc(nord + 2), bias(blklof:blkhif) , phvar(blklof:blkhif), NTvar(blklof:blkhif), cdabs2, phsed  , trnrm, fnavm, varc, RtoD, RtoD2, msc(blklof:blkhif)      , C2toF,  xx, FtoMSC, fnav, xsm2, ff, zpref, d1mach     , dphse;
  double complex cft1(blklof:blkhif, nord) , cft2(blklof:blkhif, nord) , cx(nord + 2), zz;
  bool phzref;
  //c     
  
  
  cdabs2(zz) = dfloat(zz)**2 + dimag(zz)**2;
  phsed(zz) = RtoD*datan2(dimag(zz), dfloat(zz));
  //c              Transforms from MSC to f, inverse
  C2toF(xx) = trnrm*dlog((1. + dsqrt(xx))/(1. - dsqrt(xx)))/2.;
  FtoMSC(ff) = dtanh(ff/trnrm)**2;
  //c      
  zpref = 0.d0;
  nfreqs = blkhif + 1 - blklof;
  nav = nord;
  n1 = nav + 1;
  n2 = nav + 2;
  trnrm = dsqrt(dble(2*nav - 2));
  fnavm = dble(nav - 1);
  fnav = dble(nav);
  varc = fnavm/fnav;
  RtoD = 45.d0/datan(1.d0);
  RtoD2 = RtoD**2;
  
  
  for(n=blklof; n<=blkhif; n++)
  {
    for(1400 ks = 1, nav + 1)\n{
      kc = 0;
      cx(ks) = (0.d0, 0.d0);
      s1(ks) = 0.d0;
      s2(ks) = 0.d0;
      for(k=1; k<=nav; k++)
      {
        //c     do 300 nb = ns1,ns1+nsav-1
        kc = kc + 1;
        if (kc == ks) cycle;
        cx(ks) = cx(ks) + cft1(n, k)*dconjg(cft2(n, k));
        s1(ks) = s1(ks) + cdabs2(cft1(n, k));
        s2(ks) = s2(ks) + cdabs2(cft2(n, k));
        
      }
      xsm2 = cdabs2(cx(ks));
      //c     Keep phase in (cos,sin) form
      cx(ks) = cx(ks)/dsqrt(xsm2);
      //c               MSC
      jkmsc(ks) = xsm2/(s1(ks)*s2(ks));
      //c     Transform MSC
      TRmsc(ks) = C2toF(jkmsc(ks));
      
    }
    //c             Bias
    TRmsc(n2) = 0.d0;
    cx(n2) = (0.d0, 0.d0);
    for(k=1; k<=nav; k++)
    {
      cx(n2) = cx(n2) + cx(k);
      TRmsc(n2) = TRmsc(n2) + TRmsc(k);
      
    }
    //c     Phase and Phase Variance
    cx(n2) = cx(n2)/fnav;
    if (cdabs(cx(n2)) <= 10.*d1mach(1)) 
    {
      if (n > blklof) 
      {
        ph(n) = ph(n - 1);
      } else
      {
        ph(n) = 0.d0;
      }
    } else
    {
      ph(n) = phsed(cx(n2)) + 360.d0*fr(n)*tau;
    }
    phvar(n) = dble(2*(nav - 1))*(1. - cdabs(cx(n2)))*RtoD2;
    //c     Jackknife average of transformed delete-one estimates
    TRmsc(n2) = TRmsc(n2)/fnav;
    NTmsc(n) = TRmsc(n1);
    bias(n) = fnavm*(TRmsc(n2) - TRmsc(n1));
    //c     J.K. Unbiased Normal Transform to msc
    msc(n) = FtoMSC(NTmsc(n));
    //c     Variance
    NTvar(n) = 0.d0;
    for(k=1; k<=nav; k++)
    {
      NTvar(n) = NTvar(n) + (TRmsc(k) - TRmsc(n2))**2;
      
    }
    NTvar(n) = NTvar(n)*varc;
    
  }
  //c      cx1(0) = 360.d0
  
  //c     Keep zero-frequency reference
  phzref = (blklof <= 0)) && ((blkhif >= 0);
                              if (phcorr  ==  1) 
                              {
                                if (phzref) zpref = ph(0);
                                sphsed(ph, nfreqs);
                                if (phzref) 
                                {
                                  dphse = ph(0) - zpref;
                                  adstoa(ph, ph, nfreqs, -dphse);
                                }
                              }
                              ;
  } subroutine;


//c **** end jkcoh

