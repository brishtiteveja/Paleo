#data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/EvolutionaryTree/Fordham and Zehady shared/180724'
data_dir <- '/Users/andy/Documents/TSCreator/EvolutionaryTree/Fordham and Zehady shared/180724'
setwd(data_dir)
#dp_fname <- 'qryTSCAze_MorphospeciesAzeTableS3_ColourMorphogroup.xls'
dp_fname <- 'qryTSCAze_BiospeciesAze_ColourMorphogroup.xls'

library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname) #, sheet=sheets[1], range = "A13:F1987")
c <- colnames(dfxl) # column header
c

dp_fname2 <- 'qryTSCAze_BiospeciesAze_ColourMorphogroup.xls'
library(readxl)
sheets2 <- excel_sheets(dp_fname2)
sheets2
dfxl2 <- read_excel(dp_fname2) #, sheet=sheets[1], range = "A13:F1987")
c2 <- colnames(dfxl2) # column header
c2

name_col2 <- dfxl2$`1.3`
name_col2

name_col <- dfxl$`1.3`
name_col
age_col <- dfxl$...3#dfxl$X__1
age_col
type_col <- dfxl$...4#dfxl$X__2
type_col
branch_to_col <- dfxl$...5#dfxl$X__3
branch_to_col
LAD_ix <- which(type_col == 'TOP')
LAD_ix
FAD_ix <- which(type_col == 'branch')
FAD_ix

lad_df <- data.frame(LAD=age_col[LAD_ix], name=name_col[LAD_ix])
lad_df <- lad_df[order(lad_df$LAD, decreasing = F),]
lad_df
fad_df <- data.frame(FAD=age_col[FAD_ix], name=branch_to_col[FAD_ix])
fad_df <- fad_df[order(fad_df$FAD, decreasing = F),]
fad_df

fad_lad_df <- fad_df
fad_lad_df$LAD <- rep(-1, dim(fad_lad_df)[1])
i <- 1
for(n in fad_lad_df$name) {
  ix <- which(lad_df$name == n)
  fad_lad_df$LAD[i] = lad_df$LAD[ix] 
  i <- i+1
}
head(fad_lad_df)
dim(fad_lad_df)

living_ix <- which(fad_lad_df$LAD == 0)
living_df <- fad_lad_df[living_ix,]
living_df
dim(living_df)


slide_window <- 1 # 100Ka interval

min_age <- min(fad_lad_df$LAD, fad_lad_df$FAD)
mn_a <- floor(min_age) - slide_window/2
max_age <- max(fad_lad_df$LAD, fad_lad_df$FAD)
mx_a <- ceiling(max_age) + slide_window/2


age_seq <- seq(mn_a, mx_a, by=slide_window)
age_seq

ages <- list()
LAD_cnts <- list()
FAD_cnts <- list()
LAD_names <- list()
FAD_names <- list()
n <- length(age_seq) - 1
for(k in 1:n) {
  start_age <- age_seq[k]
  end_age <- age_seq[k+1]
  mid_age <- (start_age + end_age)/2
  ixl <- which(fad_lad_df$LAD >= start_age 
              & fad_lad_df$LAD < end_age)

  ixf <- which(fad_lad_df$FAD >= start_age 
               & fad_lad_df$FAD < end_age)
  
  cnt_l = length(ixl)
  cnt_f = length(ixf)
  ln_l = list(levels(droplevels(fad_lad_df$name[ixl])))
  ln_f = list(levels(droplevels(fad_lad_df$name[ixf])))
  
  ages[[k]] <- mid_age
  LAD_cnts[[k]] <- cnt_l
  FAD_cnts[[k]] <- cnt_f
  LAD_names[[k]] <- ln_l
  FAD_names[[k]] <- ln_f
 }

FAD_LAD_per_df <- data.frame(age=unlist(ages), 
                         FAD_cnt = unlist(FAD_cnts), LAD_cnt = unlist(LAD_cnts))

FAD_LAD_per_df$total <- FAD_LAD_per_df$LAD_cnt + FAD_LAD_per_df$FAD_cnt

head(FAD_LAD_per_df)
tail(FAD_LAD_per_df)

N <- dim(FAD_LAD_per_df)[1]
FAD_LAD_per_df_ap <- FAD_LAD_per_df[2:N,]
head(FAD_LAD_per_df_ap)

ages_ap <- FAD_LAD_per_df$age[2:N]
extinction_rate <- FAD_LAD_per_df_ap$LAD/sum(FAD_LAD_per_df_ap$LAD) * 100
speciation_rate <- FAD_LAD_per_df_ap$FAD/sum(FAD_LAD_per_df_ap$FAD) * 100

par(mfrow=c(1,1))
par(mar=c(4,4,4,4))
plot(FAD_LAD_per_df_ap$age, 
     FAD_LAD_per_df_ap$LAD_cnt + FAD_LAD_per_df_ap$FAD_cnt, 
     t='l', lwd=1,
     col=1, 
     lty=2,
     xlab='Age (Ma)',
     ylab='Number of events', #'event rate(%)',
     #ylim=c(0,42),
     main='Speciation and extinction events of planktonic foraminifera during Cenozoic era')

lines(FAD_LAD_per_df_ap$age, FAD_LAD_per_df_ap$LAD_cnt, #ages, extinction_rate, 
      lwd=2,
      col=2)

lines(FAD_LAD_per_df_ap$age, FAD_LAD_per_df_ap$FAD_cnt, #ages, speciation_rate, 
      lwd=2,
      col=3)
legend('topright', legend=c('speciation + extinction', 'extinction', 'speciation'), col=c(1,2,3), lty=c(2,1,1))


df = data.frame(FAD_LAD_per_df_ap)

# At each pseudolevel, we count the number of speciations or
# extinctions—encoded as counts in the code (Dataset S2)—and
# the total number of species extant. For extinction calculations,
# the count of extant species includes the extinguishers (which
# form part of the pool of species that are exposed to extinction
# risk) but not originators. Conversely, for speciation calculations,
# the count of extant species includes the originators but not the
# extinguishers. 
# N_FAD[i] : Number of species exists through speciation at period i 
# subtract the number of extinctions happened in current period i
# Add the number of speciations happened in current period i
# N_FAD[i] = N_FAD[i-1] - LAD[i] + FAD[i]

N_FAD <- rev(df$FAD_cnt) # rev(df1$`N speciations`)  
r_LAD <- rev(df$LAD_cnt) # rev(df1$`N extinctions`) 
r_FAD <- rev(df$FAD_cnt) # rev(df1$`N speciations`)

#N_FAD[1] <- 0
for(i in 2:length(N_FAD)) {
  N_FAD[i] = N_FAD[i-1] - r_LAD[i] + r_FAD[i]
}

# N_LAD[i] : Number of species exists at period i after extinction events in period (i-1)
# subtract the number of extinctions happened in previous period (i-1)
# Add the number of speciations happened in previous period (i-1)
# N_LAD[i] = N_LAD[i-1] - LAD[i-1] + FAD[i-1]

N_LAD <- rev(df$FAD_cnt) # rev(df1$`N extinctions`)
r_LAD <- rev(df$LAD_cnt) # rev(df1$`N extinctions`) 
r_FAD <- rev(df$FAD_cnt) # rev(df1$`N speciations`)  

#N_LAD[1] <- 0
for(i in 2:length(N_FAD)) {
  N_LAD[i] = N_LAD[i-1] - r_LAD[i-1] + r_FAD[i-1]
}

N_FAD_LAD = data.frame(N_FAD=rev(N_FAD), N_LAD=rev(N_LAD))
tail(N_FAD_LAD)

df <- cbind(df, N_FAD_LAD)
#df[is.na(df)] <- 0
head(df)
tail(df)

# changing column names
df$`N.speciations` <- df$FAD_cnt
df$`N.extinctions` <- df$LAD_cnt
df$`N.turnover` <- df$FAD_cnt + df$LAD_cnt
df$`N.species.speciation` <- df$N_FAD
df$`N.species.extinction` <- df$N_LAD
df$`raw.speciation.probability` <- df$FAD_cnt/df$N_FAD
df$`raw.extinction.probability` <- df$LAD_cnt/df$N_LAD
df[is.na(df)] <- 0


# Extract only Cenozoic
CENOZOIC_BASE <- 67
df_C <- df[df$age > 0 & df$age <= CENOZOIC_BASE, ]
tail(df_C)

# Change the dff here depending on the time period being focused on
dff <- df
dff <- df_C

dff <- dff[,-c(2,3,4,5,6)]
dff$`raw.turnover.probability` <- dff$`raw.speciation.probability` + dff$`raw.extinction.probability`
which(is.na(dff))
head(dff)
tail(dff)


# HMM 

# To extract high-resolution macroevolutionary time series, we have used
# discrete time HMMs (30) to identify a parsimonious set of discrete speciation and extinction probability states in the data and to predict time series of those
#
#HMMs are an effective tool for
#change point analysis and can be used to determine whether abrupt changes
#in the time series exceed the noise and should be considered meaningful or
#not.

m <- 3
counts <- dff$N.speciations
head(counts)
pn <- dff$N.species.speciation
head(pn)
x <- dthmm(counts, Pi=setPi(m), delta= setDPm(m), pm= list(prob=setDPm(m)), 
           pn=list(size=pn), distn="binom", discrete=TRUE)
y <- BaumWelch(x)               # estimated parameters of the HMM

# Predict most likely sequence of states given observations and model y
v <- Viterbi(y)                 # predicted sequence of HMM states at each pseudolevel

dff$`HMM.speciation.state` <- v
dff$`HMM.speciation.state.probability` <- y$pm$prob[v]


m <- 2 # 3 state extinction
counts <- dff$N.extinctions
pn <- dff$N.species.extinction
x <- dthmm(counts, Pi=setPi(m), delta= setDPm(m), pm= list(prob=setDPm(m)), 
           pn=list(size=pn), distn="binom", discrete=TRUE)
y <- BaumWelch(x)               # estimated parameters of the HMM

# Predict most likely sequence of states given observations and model y
v <- Viterbi(y)                 # predicted sequence of HMM states at each pseudolevel

dff$`HMM.extinction.state` <- v
dff$`HMM.extinction.state.probability` <- y$pm$prob[v]

dff$`HMM.turnover.probability` <- dff$`HMM.speciation.state.probability` + dff$`HMM.extinction.state.probability`

head(dff)
library(DT)
datatable(dff)


par(mfrow=c(9,1))
par(mar=c(1,4,1,1))
plot(-dff$age, dff$N.speciations, t='l', col='green', 
     xlab='Ma', ylab='N speciation')
plot(-dff$age, dff$raw.speciation.probability, t='l', col='green',
     xlab='Ma', ylab='Raw spec. prob')
plot(-dff$age, dff$HMM.speciation.state.probability, t='l', col='green',
     xlab='Ma', ylab='HMM spec. prob')
plot(-dff$age, dff$N.extinctions, t='l', col='red', 
     xlab='Ma', ylab='N extinction')
plot(-dff$age, dff$raw.extinction.probability, t='l', col='red',
     xlab='Ma', ylab='Raw exti. prob')
plot(-dff$age, dff$HMM.extinction.state.probability, t='l', col='red',
     xlab='Ma', ylab='HMM exti. prob')
plot(-dff$age, dff$N.turnover, t='l', col='black',
     xlab='Ma', ylab='N turnover')
plot(-dff$age, dff$raw.turnover.probability, t='l', col='black',
     xlab='Ma', ylab='Raw turn. prob')
plot(-dff$age, dff$HMM.turnover.probability, t='l', col='black',
     xlab='Ma', ylab='HMM turn. prob')


AGE_DIFF = NA
if (floor(AGE_SLIDE) == 0) {
  AGE_DIFF = paste(AGE_SLIDE * 1000, "K", sep="")
} else {
  AGE_DIFF = paste(AGE_SLIDE, "M", sep="")
}

pfname <- paste(getwd(),"/marine_genera_event_extraction", "/cenozoic_marine_genera_speciation_extinction_age_slide_", AGE_DIFF, ".csv", sep="")
pfname
write.csv(dff, file=pfname, col.names = TRUE)


# Read the marine genera bin wise evolution frequency/turnover data
dff <- read.csv(file=pfname)
head(dff)
colnames(dff)

# R code to generate surrogate simulations in order to assess suitability of 
#   spectral analysis procedures provided in Astrochron

# Load the library Astrochron
library(astrochron)

library(astsa)
ar1_noise_model <- sarima(dff$HMM.turnover.probability, 1, 0, 0)
ar1_ttable = ar1_noise_model$ttable
rho = ar1_ttable[1,1] 
rho
npts = dim(dff)[1]
dt = 0.1

mtmAR(data.frame(dff$age, dff$HMM.turnover.probability))

par(mar=c(4,4,4,4))
# Conduct simulations using AR1 surrogates, and the Periodogram-AR1 approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff= 0.3082017, 
               method="periodogramAR1", detrend=T,iter=2000)

# Conduct simulations using AR1 surrogates and the MTM-AR1 approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff=rho, 
               method="mtmAR1", detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates and the MTM-ML96 approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff=rho, 
               method="mtmML96",detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates and the LOWSPEC approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff=rho, 
               method="lowspec",detrend=T, tbw=2, iter=2000)

mtmPL(data.frame(dff$age, dff$HMM.turnover.probability))
beta = 0.2590709

# Conduct simulations using power law surrogates, and the MTM-PL approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="mtmPL",detrend=T, tbw=2, iter=2000)

# Conduct simulations using power law surrogates, and the Periodogram-PL approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff= beta, 
               method="periodogramPL", detrend=T,iter=2000)

# Conduct simulations using power law surrogates and the LOWSPEC approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="lowspec",detrend=T, tbw=2, iter=2000)



# R code to generate surrogate simulations in order to assess suitability of 
#   spectral analysis procedures provided in Astrochron: testing the 
#   use of AR1-based models to analyze power law surrogates, and vice versa

# Load the library Astrochron
library(astrochron)

# Conduct simulations using power law surrogates and MTM-AR1 approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="mtmAR1", detrend=T,tbw=2,iter=2000, genplot = T)

# Conduct simulations using power law surrogates and the MTM-ML96 approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="mtmML96", detrend=T,tbw=2, iter=2000)

# Conduct simulations using power law surrogates and the LOWSPEC approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=beta, 
               method="lowspec", detrend=T,tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates, and the MTM-PL approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff= rho, 
               method="mtmPL", detrend=T, tbw=2, iter=2000)

# Conduct simulations using AR1 surrogates, and the Periodogram-PL approach
testBackground(npts=npts, dt=dt, noiseType="ar1", coeff= rho, 
               method="periodogramPL", detrend=T, tbw=2, iter=2000)

# Conduct simulations using power law surrogates, and the Periodogram-AR1 approach
testBackground(npts=npts, dt=dt, noiseType="pwrLaw", coeff=rho, 
               method="periodogramAR1", detrend=T, iter=2000)


# HMM turnover probabilities should be placed in the data frame "turnProb"
# Download these data from the Astrochron server
turnProb=getData("graptolite")

# Load the library Astrochron
library(astrochron)

dff = cenozoic_dat
turnProb = data.frame(age=dff$age, 
                      #turnProb=dff$HMM.turnover.probability,
                      #turnProb=dff$raw.turnover.probability
                      turnProb=dff$turnover
                      )

turnPwr=eha(turnProb, win=5, fmin=0, fmax=1.3, #step=0.2,
            pad=4000, 
            genplot=4, palette=5, pl=2, ydir=-1, 
            tbw=2, output=2)

# Conduct LOWSPEC analysis
specLow=lowspec(turnProb, detrend=T, tbw=2, padfac=1, 
                pl=2, sigID=F, output=1,
                xmin=0.3,
                xmax=1)
specLowDF = data.frame(Frequency=specLow$Frequency, Period=1/specLow$Frequency, 
                       Power_90=specLow$LOWSPEC_90_power, Power_95=specLow$LOWSPEC_95_power,
                       Power_99 = specLow$LOWSPEC_99_power,
                       Harmonic_CL = specLow$Harmonic_CL,
                       LowSpec_CL = specLow$LOWSPEC_CL)

specLowDFH = specLowDF[order(specLowDF$Harmonic_CL, decreasing = T),]
head(specLowDFH, n=100)

# Conduct MTM-PL analysis
specPL1

specMtmAR = mtmAR(turnProb, detrend=T, tbw=2, padfac=1, pl=2,
                  #sigID=F, 
                  output=1,
                  xmin=0.3,
                  xmax=1)
specMtmARDF = data.frame(Frequency=specMtmAR$Frequency,
                         Period=1/specMtmAR$Frequency,
                         Power=specMtmAR$`MTM/AR`,
                         AR_CL=specMtmAR$AR_CL)
specMtmARDFS = specMtmARDF[order(specMtmARDF$AR_CL, decreasing = T),]
head(specMtmARDFS, n=100)

npts = dim(turnProb)[1]
dt = diff(turnProb$age)[1]
# Conduct MTM spectral analysis with LOWSPEC background, apply Bonferroni correction, 
#   and evaluate a range of multiple testing procedures
turnProb = data.frame(age=dff$age, turnProb=dff$HMM.turnover.probability)
specLow=lowspec(turnProb, detrend=T, tbw=2, padfac=1, pl=2, sigID=F, output=1)
flow=c((1/2)- 0.3361345,(1/1.1)- 0.3361345)
fhigh=c((1/2)+ 0.3361345,(1/1.1)+ 0.3361345) 
confAdjust(specLow, npts=dim(turnProb)[1], dt=dt, tbw=2, ntap=3, pl=2, 
           flow=flow, fhigh=fhigh)
multiTest(specLow, flow=flow, fhigh=fhigh)

# Conduct MTM-PL analysis, apply Bonferroni correction, and evaluate a range of 
#   multiple testing procedures
specPL1=mtmPL(turnProb, detrend=T, tbw=2, padfac=1, pl=2, sigID=F, output=1)
flow=c((1/2.4)- 0.3333333,(1/1.2)- 0.3333333)
fhigh=c((1/2.4)+ 0.3333333,(1/1.2)+ 0.3333333) 
confAdjust(specPL1, npts=120, dt=0.05, tbw=2, ntap=3, pl=2, flow=flow, fhigh=fhigh)
multiTest(specPL1, flow=flow, fhigh=fhigh)

# Conduct Periodogram-AR1 analysis, apply Bonferroni correction, and evaluate a range 
#   of multiple testing procedures
specAR1=periodogram(cosTaper(turnProb, demean=T, detrend=T), demean=F, 
                    background=1, fNyq=F, padfac=1, output=1)
confAdjust(specAR1, npts=120, dt=0.05, tbw=2, ntap=3, pl=2, flow=flow, fhigh=fhigh)
multiTest(specAR1, flow=flow, fhigh=fhigh)

# Conduct Periodogram-PL analysis, apply Bonferroni correction, and evaluate a range 
#   of multiple testing procedures
specPL2=periodogram(cosTaper(turnProb, demean=T, detrend=T), demean=F, 
                    background=2, fNyq=F, padfac=1, output=1)
confAdjust(specPL2, npts=120, dt=0.05, tbw=2, ntap=1, pl=2, flow=flow, fhigh=fhigh)
multiTest(specPL2, flow=flow, fhigh=fhigh)



#turnProb = getData('graptolite')
turnPwr=eha(turnProb, win=25.001, fmin=0, fmax=1.2, step=0.2,
            pad=4000, 
            genplot=4, palette=5, pl=2, ydir=-1, 
            tbw=2, output=2)

# R code to calculate variance associated with cycles and apply bandpass filters
# Uses object created by Dataset S9

# Integrate power spectra 
pwrRes1.3=integratePower(turnPwr, npts=npts, pad=4000, ydir=-1, xmax=2, unity=F, ln=T, 
                         flow=0.53, fhigh=0.90, genplot=F) 
pwrRes2.6=integratePower(turnPwr,npts=npts,pad=4000,ydir=-1, xmax=2, unity=F, ln=T, 
                         flow=0.21, fhigh=0.52, genplot=F) 
pwrResBoth=cb(pwrRes1.3[1], pwrRes1.3[4]) 
pwrResBoth[2]=pwrRes1.3[4]+pwrRes2.6[4] 

# Bandpass filter 
ecc=taner(turnProb, xmax=2, padfac=5, detrend=T, flow=1/3.1, fhigh=1/2.1, roll=10^6, 
          genplot=F) 
obl=taner(turnProb, xmax=2, padfac=5, detrend=T, flow=1/1.18, fhigh=1/1.47, roll=10^6, 
          genplot=F)

# A summary plot
pl(r=1,c=4) 
plot(pwrRes1.3[,4], pwrRes1.3[,1], type="l", lwd=1.5, col="red", 
     ylim= c(470.925,429.025), ylab="Millions of years", xlab= "1.3 Myr variance", 
     cex.axis=1.2, cex.lab=1.3) 
plot(pwrRes2.6[,4], pwrRes2.6[,1], type="l", lwd=1.5, col="blue", 
     ylim=c(470.925,429.025), ylab="Millions of years", xlab= "2.6 Myr variance", 
     cex.axis=1.2, cex.lab=1.3)   
plot(pwrResBoth[,2], pwrResBoth[,1], type="l", lwd=1.5, ylim=c(470.925,429.025), 
     ylab="Millions of years", xlab="Combined variance", cex.axis=1.2, cex.lab=1.3) 
plot(ecc[,2], ecc[,1], type="l", lwd=1.5, col="blue", ylim=c(470.925,429.025), 
     ylab="Millions of years", xlab="Filter outputs", cex.axis=1.2, cex.lab=1.3)
lines(obl[,2], obl[,1], lwd=1.5, col="red", cex.axis=1.2, cex.lab=1.3) 





# Cenozoic-Campanian Marine Oxygen-18 Composite (per-mil PDB)
data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Datapacks'
setwd(data_dir)
dp_fname <- 'Phan_GTS2016_for_7.1_HaqJur_ForamMikrotax_28July2017.xls'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname) #, sheet=sheets[1], range = "A13:F1987")
c <- colnames(dfxl) # column header
c

# cenozoic oxy-18 row 
ages <- dfxl$`1.3`
oxy_18 <- dfxl$X__1
start_r <- 28227
end_r <- 31270
oxy_ages <- as.numeric(ages[start_r:end_r])
oxy_18 <- as.numeric(oxy_18[start_r:end_r])

plot(oxy_ages, -oxy_18, 
     t='l', yaxt='n',
     xlab='age (Ma)',
     ylab='Oxygen-18 ()',
     xlim=c(0, max(FAD_LAD_per_df_ap$age)),
     main = 'Benthic oxygen-18 isotope curve during Cenozoic era'
     )
yv <- pretty(-oxy_18)
ylbls <- -yv
axis(2, at=yv, labels = ylbls)
arrows(10, -1, 10, 0.5, col=2, lwd=2)
mtext("High Temp",  side = 3, at = c(16), line= -3.5, col=2)
arrows(10, -3, 10, -4.5, col='blue', lwd=2)
mtext("Low Temp",  side = 3, at = c(16), line= -10, col='blue', lwd=2)

cenozoic_oxy18_df <- data.frame(age=oxy_ages, oxy_18=oxy_18)
cenozoic_oxy18_df <- cenozoic_oxy18_df[cenozoic_oxy18_df$age <=max(LAD_per_df$age),]

# Sea level: short term Phanerozoic 
ages <- dfxl$`1.3`
sl_short <- dfxl$X__1
start_r <- 24220
end_r <- 24867
sl_ages <- as.numeric(ages[start_r:end_r])
sl_short <- as.numeric(sl_short[start_r:end_r])

cenozoic_sl_df <- data.frame(age=sl_ages, short_sea_level=sl_short)
cenozoic_sl_df <- cenozoic_sl_df[cenozoic_sl_df$age <= max(LAD_per_df$age),]



plot(cenozoic_sl_df$age,
     cenozoic_sl_df$short_sea_level,
     t='l', lwd=2,
     #yaxt='n',
     xlab='age (Ma)',
     ylab='Sea level (m)',
     xlim=c(0, max(FAD_LAD_per_df_ap$age)),
     main = 'Sea level in meters above present day during Cenozoic era'
)
abline(h=0, lty=2)

data_dir <- '/Users/andy/Documents/projects/ML-Data Mining/evolution/data'
setwd(data_dir)
write.csv(FAD_LAD_per_df_ap, 
          file='cenozoic_extinction_speciation_after_present.csv',
          col.names = T,
          row.names = F)
write.csv(FAD_LAD_per_df, 
          file='cenozoic_extinction_speciation.csv',
          col.names = T,
          row.names = F)
write.csv(cenozoic_oxy18_df,
          file='cenozoic_oxy_18.csv',
          col.names = T,
          row.names = F)
write.csv(cenozoic_sl_df,
          file='cenozoic_sea_level.csv',
          col.names = T,
          row.names = F)


min_age <- min(fad_lad_df$LAD, fad_lad_df$FAD)
mn_a <- floor(min_age)
max_age <- max(fad_lad_df$LAD, fad_lad_df$FAD)
mx_a <- ceiling(max_age)

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
n <- length(age_seq) - 1
ms_oxy_ages <- list()
ms_oxy_18 <- list()
ms_sl_ages <- list()
ms_sl <- list()
for(k in 1:n) {
  start_age <- age_seq[k]
  end_age <- age_seq[k+1]
  mid_age <- (start_age + end_age)/2
  ix_oxy <- which(cenozoic_oxy18_df$age >= start_age 
               & cenozoic_oxy18_df$age < end_age)
  
  mean_oxy_age <- mean(cenozoic_oxy18_df$age[ix_oxy])
  mean_oxy_18 <- mean(cenozoic_oxy18_df$oxy_18[ix_oxy])
  
  ms_oxy_ages[[k]] <- mean_oxy_age
  ms_oxy_18[[k]] <- mean_oxy_18
  
  ix_sl <- which(cenozoic_sl_df$age >= start_age 
                 & cenozoic_sl_df$age < end_age & !is.na(cenozoic_sl_df$age))
  
  mean_sl_age <- mean(cenozoic_sl_df$age[ix_sl])
  mean_sl <- mean(cenozoic_sl_df$short_sea_level[ix_sl])
  
  ms_sl_ages[[k]] <- mean_sl_age
  ms_sl[[k]] <- mean_sl
}

lines(unlist(ms_sl_ages), unlist(ms_sl), lty=2, col=3)
lines(unlist(ms_oxy_ages), -unlist(ms_oxy_18), lty=2, lwd=2, col=3)

cenozoic_oxy18_smoothed_df <- data.frame(age=unlist(ms_oxy_ages),
                                         oxy18_sm=unlist(ms_oxy_18))
cenozoic_sl_smoothed_df <- data.frame(age=unlist(ms_sl_ages),
                                         sl_sm=unlist(ms_sl))

write.csv(cenozoic_oxy18_smoothed_df,
            file='cenozoic_oxy_18_smoothed.csv',
            col.names = T,
            row.names = F)
write.csv(cenozoic_sl_smoothed_df,
          file='cenozoic_sea_level_smoothed.csv',
          col.names = T,
          row.names = F)

# spectral analysis
x<- FAD_LAD_per_df_ap$age #yr_sliding_50y 
y<- FAD_LAD_per_df_ap$total #as.numeric(temp_sliding_50y)
x = dff$age
y = dff$HMM.turnover.probability
yorig <- y
y <- y-mean(y)
plot(x, y, col=2, t='l')
y <- y/sd(yorig)
plot(x, y, col=3, t='l')
lmf <- lm(y~x)
y <- y.dtrnd <- lmf$residuals
lines(x, y, col=4)

# Using multitaper
library(astrochron)
#chr_model <- linterp(chr_df, dt=1)
evolution_model <- linterp(data.frame(year=x, evolution=y), dt=0.1)
Mspec <- mtm(evolution_model, demean = T, detrend = T, 
             #ntap = 5, tbw = 3, #ar1 = T,
             #xmin = 0,
             #xmax = 0.1,
             output = 1, pl=2)
Mspecdf <- data.frame(Mspec)
Mspecdf <- Mspecdf[order(Mspecdf$Harmonic_CL, decreasing = T),]
time_window <- max(x) - min(x)
N <- length(evolution_model$evolution)
Mspecdf$period <- (1/Mspecdf$Frequency) #* (time_window / N)
head(Mspecdf, n = 15)


# using redfit
library(dplR)
#x=as.numeric(dff$raw.turnover.probability) #temp_sliding_50y) #(temp_recon)
x=as.numeric(df$total)
#t=as.numeric(dff$age) #yr_sliding_50y) #(yr)
t=as.numeric(df$age)
par(mfrow=c(1,1))
plot(t,x, t='l')
redf.dat.evol <- redfit(x=x, nsim = 1000, mctest=TRUE)
redf.dat <- redf.dat.evol
#par(tcl = 0.5, mar = rep(3, 4), mgp = c(1.1, 0.1, 0))
plot(redf.dat[['freq']], redf.dat[['gxx']],
     #xlim = c(0, 0.01),
     #ylim = range(redf.dat[["ci99"]], redf.dat[["gxx"]]),
     #type ='l',
     #lwd=2,
     type = "n", axes = FALSE,
     ylab = "Spectral Power (dB)", 
     xlab = "Frequency (1/yr)"
)
grid()
lines(redf.dat[["freq"]], redf.dat[["gxx"]], col = "black", lwd=2)
lines(redf.dat[["freq"]], redf.dat[["ci99"]], col = "red")
lines(redf.dat[["freq"]], redf.dat[["ci95"]], col = "pink")
lines(redf.dat[["freq"]], redf.dat[["ci90"]], col = "green")
#lines(redf.dat[["freq"]], redf.dat[["ci80"]], col = "orange")
Rspecdf <- data.frame(freq=redf.dat[["freq"]], 
                      spec=redf.dat[["gxx"]],
                      period=1/redf.dat[["freq"]])
Rspecdf <- Rspecdf[order(Rspecdf$spec, decreasing = T),]
head(Rspecdf, 20)
#Rspecdf <- Rspecdf[Rspecdf$freq <= 0.01, ]
freqs <- pretty(Rspecdf$freq)
pers <- round(1 / freqs, 2)
axis(1, at = freqs, labels = TRUE)
axis(3, at = freqs, labels = pers)
mtext(text = "Period (yr)", side = 3, line = 2)
axis(2); #axis(4)
legend("topright", c("dat", "CI99", "CI95", "CI90", "CI80"), lwd = 2,
       col = c("black", "red", "pink", "green"),
       bg = "white", cex=0.5)
box()



