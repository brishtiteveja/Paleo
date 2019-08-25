data_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Cenozoic_Planktonic_Foraminifera/data'
setwd(data_dir)
dp_fname <- 'Bergen-etal_GSA2018_TableS1 theme-sorted.xlsx'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname, sheet=sheets[1])#, range = "A13:F1987")
c <- colnames(dfxl) # column header
c

for_ordering_col <- dfxl$`For ordering`
horizon_col <- dfxl$HORIZON
bp_historical_name_col <- dfxl$`BP HISTORICAL NAME`
chart_name_col <- dfxl$`2018 CHART NAME`
published_name_col <- dfxl$`PUBLISHED NAME (IF DIFFERENT)`
nn_col <- dfxl$NN
cnm_col <- dfxl$CNM
cn_col <- dfxl$CN
comments_col <- dfxl$COMMENTS
flag_col <- dfxl$Flag
nannos_col <- dfxl$Nannos
n_zone_col <- dfxl$`N-zone`
wade_zone_col <- dfxl$`Wade-zone`
subzone_col <- dfxl$Subzone
planktonic_forams_col <- dfxl$`Planktonic Forams`
benthic_forams_col <- dfxl$`Benthic Forams`
group_col <- dfxl$Group
gom_event_col <- dfxl$`GoM Event type`
odp_event_col <- dfxl$`ODP Event type`
chart_age_col <- dfxl$`2018 CHART AGE (Ma)`
calibration_col <- dfxl$`CALIBRATED TO`

nannos_row_id <- which(!is.na(nannos_col))
nannos_df_full <- data.frame(nannos=as.character(nannos_col[nannos_row_id]),
                        age=as.numeric(as.character(chart_age_col[nannos_row_id])),
                        bp_historical_name=bp_historical_name_col[nannos_row_id],
                        chart_name=chart_name_col[nannos_row_id],
                        published_name=published_name_col[nannos_row_id],
                        flag=flag_col[nannos_row_id],
                        nn=nn_col[nannos_row_id],
                        cn=cn_col[nannos_row_id],
                        cnm=cnm_col[nannos_row_id],
                        gom_event_type=as.character(gom_event_col[nannos_row_id]),
                        odp_event_type=as.character(odp_event_col[nannos_row_id]),
                        comments=as.character(comments_col[nannos_row_id]))

nannos_df <- nannos_df[1:462,]

head(nannos_df)
library(DT)
datatable(nannos_df)

name_col <- nannos_df$nannos
name_col
age_col <- nannos_df$age
age_col
type_col <- nannos_df$gom_event_type
type_col
as.character(unique(type_col))
#branch_to_col <- dfxl$X__3
#branch_to_col
LAD_cond <- (type_col == 'HO' 
  | type_col == 'HRO' 
  | type_col == 'HIO'
  | type_col == 'HFO'
  | type_col == 'HCO'
  | type_col == 'HO Acme'
  | type_col == 'HOAcme')
LAD_ix <- which(LAD_cond)

LAD_ix

FAD_cond <- (type_col == 'LO' 
  | type_col == 'LRO' 
  | type_col == 'LIO'
  | type_col == 'LFO' 
  | type_col == 'LCO'
  | type_col == 'LO Acme'
  | type_col == 'LOAcme')

FAD_ix <- which(FAD_cond)

FAD_ix

lad_df <- data.frame(LAD=age_col[LAD_ix], name=name_col[LAD_ix], event_type=type_col[LAD_ix])
lad_df <- lad_df[order(lad_df$LAD, decreasing = F),]
lad_df
fad_df <- data.frame(FAD=age_col[FAD_ix], name=name_col[FAD_ix], event_type=type_col[FAD_ix])
fad_df <- fad_df[order(fad_df$FAD, decreasing = F),]
fad_df

LAD_type <- c()
LAD <- c()
name <- c()
FAD <- c()
FAD_type <- c()

missing_fad <- which(!(fad_df$name %in% lad_df$name))
for(r in missing_fad) {
  FAD <- c(FAD, fad_df$FAD[r])
  FAD_type <- c(FAD_type, as.character(fad_df$event_type[r]))
  name <- c(name, as.character(fad_df$name[r]))
  LAD_type <- c(LAD_type, "NA")
  LAD <- c(LAD, 0)
}

i <- 1
n <- dim(lad_df)[1]
for(i in 1:n) {
  name1 <- as.character(lad_df$name[i])
  j <- 1
  n2 <- dim(fad_df)[1]
  found <- FALSE
  for(j in 1:n2) {
    name2 <- as.character(fad_df$name[j])
    if (name2 == name1) {
      FAD <- c(FAD, fad_df$FAD[j])
      FAD_type <- c(FAD_type, as.character(fad_df$event_type[j]))
      name <- c(name, name1)
      LAD_type <- c(LAD_type, as.character(lad_df$event_type[i]))
      LAD <- c(LAD, lad_df$LAD[i])
      found <- TRUE
    }
  }
  if (found == FALSE) {
    FAD <- c(FAD, 50)
    FAD_type <- c(FAD_type, 'NA')
    name <- c(name, name1)
    LAD_type <- c(LAD_type, as.character(lad_df$event_type[i]))
    LAD <- c(LAD, lad_df$LAD[i])
    found <- TRUE  
  }
}

fad_lad_df <- data.frame(FAD=FAD,
                         FAD_type=FAD_type,
                         name=name,
                         LAD_type=LAD_type,
                         LAD=LAD)


datatable(lad_df)
datatable(fad_df)
datatable(fad_lad_df)

dim(fad_lad_df)

living_ix <- which(fad_lad_df$LAD == 0)
living_df <- fad_lad_df[living_ix,]
living_df
dim(living_df)

# unique rows and columns in fad and lad df
u_fad_df <- unique(fad_lad_df[,c(1:3)])
datatable(u_fad_df)
u_lad_df <- unique(fad_lad_df[,c(3:5)])
datatable(u_lad_df)


min_age <- min(fad_lad_df$LAD, fad_lad_df$FAD)
mn_a <- 0# floor(min_age)
max_age <- max(fad_lad_df$LAD, fad_lad_df$FAD)
mx_a <- 25 #ceiling(max_age)

slide_window <- 1
window_size <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
LAD_cnts <- list()
FAD_cnts <- list()
LAD_names <- list()
FAD_names <- list()
n <- length(age_seq) - 1
for(k in 1:n) {
  start_age <- age_seq[k]
  end_age <- start_age + window_size
  mid_age <- (start_age + end_age)/2
  ixl <- which(u_lad_df$LAD >= start_age 
              & u_lad_df$LAD < end_age)

  ixf <- which(u_fad_df$FAD >= start_age 
               & u_fad_df$FAD < end_age)
  
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

N <- dim(FAD_LAD_per_df)[1]
FAD_LAD_per_df_ap <- FAD_LAD_per_df[2:N,]
head(FAD_LAD_per_df_ap)

ages_ap <- FAD_LAD_per_df$age[2:N]
extinction_rate <- FAD_LAD_per_df_ap$LAD/sum(FAD_LAD_per_df_ap$LAD) * 100
speciation_rate <- FAD_LAD_per_df_ap$FAD/sum(FAD_LAD_per_df_ap$FAD) * 100
par(mfrow=c(1,1))
plot(FAD_LAD_per_df_ap$age, 
     FAD_LAD_per_df_ap$LAD_cnt + FAD_LAD_per_df_ap$FAD_cnt, 
     t='l', lwd=1,
     col=1, 
     lty=2,
     xlab='Age (Ma)',
     ylab='Number of events', #'event rate(%)',
     ylim=c(0,max(FAD_LAD_per_df_ap$total) + 1),
     main=paste('Speciation and extinction events of nannofossils\n',
                '(window size = ', window_size, 'Ma,\n',
                'slide =', slide_window , 'Ma)' ) )

lines(FAD_LAD_per_df_ap$age, FAD_LAD_per_df_ap$LAD_cnt, #ages, extinction_rate, 
      lwd=2,
      col=2)

lines(FAD_LAD_per_df_ap$age, FAD_LAD_per_df_ap$FAD_cnt, #ages, speciation_rate, 
      lwd=2,
      col=3)
legend('topright', legend=c('speciation + extinction', 'extinction', 'speciation'), col=c(1,2,3), lty=c(2,1,1))
datatable(fad_lad_df)

# Using multitaper
library(astrochron)
#chr_model <- linterp(chr_df, dt=1)
x <- FAD_LAD_per_df_ap$age
y <- FAD_LAD_per_df_ap$total
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
Mspecdf$period <- (1/Mspecdf$Frequency) # * (time_window / N))
Mspecdf <- Mspecdf[order(Mspecdf$period, decreasing = T),]
Mspecdf2 <- subset(Mspecdf, Mspecdf$Harmonic_CL >=90 & Mspecdf$AR1_CL >= 90)
cols <- colnames(Mspecdf2)
cols
head(Mspecdf2[,c(1:4, 9)], n = 20)

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
sl_short <- as.numeric(oxy_18[start_r:end_r])

cenozoic_sl_df <- data.frame(age=sl_ages, short_sea_level=sl_short)
cenozoic_sl_df <- cenozoic_sl_df[cenozoic_sl_df$age <= max(LAD_per_df$age),]



plot(cenozoic_sl_df$age,
     cenozoic_sl_df$short_sea_level,
     t='l', lwd=2,
     #yaxt='n',
     xlab='age (Ma)',
     ylab='Sea level (m)',
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
