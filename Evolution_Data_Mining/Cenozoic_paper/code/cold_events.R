cold_events <- c('100/98',
                 'G16-10',
                 'M2/MG2',
                 'Gi2/6',
                 'Gi16/18',
                 'Si4/6',
                 'TG20/22',
                 'Tort./Mess. 1,2',
                 'Late Tort.',
                 'Mi7?',
                 'Mi7?',
                 'Mi6',
                 'Mi5',
                 'Mi4',
                 'Mi3B',
                 'Mi3A',
                 'Mi2',
                 'M1b',
                 'M1aa',
                 'M1a',
                 'Mi1'
)
cold_event_ages <- c('2.41-2.35',
                     '2.92-2.82',
                     '3.37-3.30',
                     '3.78-3.66',
                     '4.06-4.00',
                     '4.88-4.82',
                     '5.88-5.81',
                     '7.3-7.2', #; 7.0-6.9',
                     '7.6-7.6',
                     '8.8-8.8',
                     '9.6-9.0',
                     '10.45-10.35',
                     '11.8-11.7', #; 11.5-11.4',
                     '13.2-12.8',
                     '13.9-13.8',
                     '14.2-14.2',
                     '15.9-15.9',
                     '17.4-17.3',
                     '19.4-19.4',
                     '21.15-21.05',
                     '23.1-23.0'
)

width <- 10
by <- 3
pf_t <- data.frame(rollapply(PF_FAD_LAD_per_df_ap, width=width, by=by, FUN=mean))
pf_t$total <- pf_t$FAD_cnt + pf_t$LAD_cnt
head(pf_t)
xlim<-c(0,24)

nn_t <- data.frame(rollapply(NN_FAD_LAD_per_df_ap, width=width, by=by, FUN=mean))

library(grid)
library(gridBase)
dev.off()
plot.new()
par(mfrow=c(2,1))
par(mar=c(2,3,1,1))

xlim <- c(0, 34)

# plot foraminifier turnover, speciation and extinction
plot(pf_t$age, pf_t$total, t='hist', lwd=2, xlim=xlim)
lines(pf_t$age, pf_t$FAD_cnt, lwd=2, col='green')
lines(pf_t$age, pf_t$LAD_cnt, lwd=2, col='red')
Y1 <- grconvertY(3.0, "user", "ndc")

par(mfrow=c(2,1))
plot(PF_dff$age, PF_dff$N.turnover, t='l', xlim=xlim)
plot(PF_dff$age, PF_dff$raw.turnover.probability, t='l', xlim=xlim, ylim=c(0,0.4))

plot(PF_dff$age, PF_dff$N.speciations, t='l', xlim=xlim)
plot(PF_dff$age, PF_dff$raw.speciation.probability, t='l', xlim=xlim, ylim=c(0,0.2))


plot(PF_dff$age, PF_dff$N.extinctions, t='l', xlim=xlim)
plot(PF_dff$age, PF_dff$raw.extinction.probability, t='l', xlim=xlim, ylim=c(0,0.3))

peak(PF_dff[PF_dff$age <= 24, c('age', 'N.turnover')])
peak(PF_dff[PF_dff$age <= 24, c('age', 'N.speciations')])
peak(PF_dff[PF_dff$age <= 24, c('age', 'N.extinctions')])

plot(PF_dff$age, PF_dff$N.turnover, t='l', lwd=2, xlim=c(0,25), 
     xlab="Age(Myr)", ylab="Number of Events")
lines(PF_dff$age, PF_dff$N.speciations, col='green', lwd=1)
lines(PF_dff$age, PF_dff$N.extinctions, col='red', lwd=1)

c_x <- c()
c_y <- c()
for (a in cold_event_ages){
  s <- strsplit(a, split = "-")[[1]]
  x_l <- as.numeric(s[1])
  x_r <- as.numeric(s[2])
  color<-rgb(0,0,1, alpha=0.65)
  rect(x_l, 0, x_r, 10.0, col=color, border=color, lwd=2)
}

plot(nn_t$age, nn_t$total, t='hist', lwd=2, col='black', xlim=xlim)
lines(nn_t$age, nn_t$FAD_cnt, lwd=2, col='green')
lines(nn_t$age, nn_t$LAD_cnt, lwd=2, col='red')
Y2 <- grconvertY(0, "user", "ndc") # default width of bars = 1

X <- grconvertX(10, "user", "ndc")

pushViewport(viewport())
popViewport()

# grid.rect(X,Y2, height = Y2-Y1, width = 0.1, just = c('left','top'),
#           gp = gpar(col=rgb(1,0,0), fill=rgb(1,0,0), alpha=0.5))



#######

c_x <- c()
c_y <- c()
for (a in cold_event_ages){
  s <- strsplit(a, split = "-")[[1]]
  x_l <- as.numeric(s[1])
  x_r <- as.numeric(s[2])
  rect(x_l, 0, x_r, 3.0, col=rgb(0,0,1, alpha=0.5))
}



# Nanno fossil
par(mfrow=c(3,1))
plot(NN_dff$age, NN_dff$N.turnover, t='l', xlim=xlim)
plot(NN_dff$age, NN_dff$N.speciations, t='l', xlim=xlim)
plot(NN_dff$age, NN_dff$N.extinctions, t='l', xlim=xlim)

par(mfrow=c(3,1))
plot(NN_dff$age, NN_dff$raw.turnover.probability, t='l', xlim=xlim, ylim=c(0,0.4))
plot(NN_dff$age, NN_dff$raw.speciation.probability, t='l', xlim=xlim, ylim=c(0,0.2))
plot(NN_dff$age, NN_dff$raw.extinction.probability, t='l', xlim=xlim, ylim=c(0,0.3))

par(mar=c(4,4,4,4))
peak(NN_dff[PF_dff$age <= 25, c('age', 'N.turnover')])
peak(NN_dff[PF_dff$age <= 25, c('age', 'N.speciations')])
peak(NN_dff[PF_dff$age <= 25, c('age', 'N.extinctions')])

plot(NN_dff$age, NN_dff$N.turnover, t='l', lwd=2, xlim=c(0,25), 
     xlab="Age(Myr)", ylab="Number of Events")
lines(NN_dff$age, NN_dff$N.speciations, col='green', lwd=1)
lines(NN_dff$age, NN_dff$N.extinctions, col='red', lwd=1)

c_x <- c()
c_y <- c()
for (a in cold_event_ages){
  s <- strsplit(a, split = "-")[[1]]
  x_l <- as.numeric(s[1])
  x_r <- as.numeric(s[2])
  color<-rgb(0,0,1, alpha=0.65)
  rect(x_l, 0, x_r, 10.0, col=color, border=color, lwd=2)
}
