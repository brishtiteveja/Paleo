STARTING_AGE <- 0.000001
ENDING_AGE <- 600
AGE_SLIDE <- 1


setwd("~/Dropbox/TSCreator/TSCreator development/Developers/Andy/Datapacks")
dp_fname <- "Phan_GTS2016_for_7.1_HaqJur_ForamMikrotax_28July2017.xls"
library(readxl)
dfxl <- read_excel(dp_fname)

# second column
c <- list()
c <- colnames(dfxl)
df2c <- dfxl[[c[2]]]

column_type <- c('block', 'event', 'chron', 'sequence', 'facies', 'transect',  'curve', 
                 'range', 'freehand-overlay', 'overlay', 'point',
                 'point-overlay')

columns <- list()
for (ct in column_type) {
  columns[[ct]] <- list() 
  
  # get excel row num for each column
  rows <- which(df2c == ct) + 1
  
  cols <- list()
  if (ct == 'block') {
    cnum <- 1
    for (r in rows) {
      cols[[cnum]] <- list(name = c(), content=list())
      colname <- dfxl[[c[1]]][r-1]
      cols[[cnum]]$name <- colname

      r_i <- r
      column <- list()
      while(TRUE) {
        age <- dfxl[[c[3]]][r_i]
        if (is.na(age)) {
          break
        }
        name <- dfxl[[c[2]]][r_i]
        if (is.na(name) && !is.na(age))
          name <- ""

        column$name <- c(column$name, name)
        column$age <- c(column$age, as.numeric(age))
        r_i <- r_i + 1
      }
      cols[[cnum]]$content <- column
      cnum <- cnum + 1
    }
  }
  if (ct == 'event') {
    cnum <- 1
    for (r in rows) {
      cols[[cnum]] <- list(name = c(), content=list())
      colname <- dfxl[[c[1]]][r-1]
      cols[[cnum]]$name <- colname
      
      r_i <- r
      column <- list()
      type <- NA
      while(TRUE) {
        age <- dfxl[[c[3]]][r_i]
        if (is.na(age)) {
          if (is.na(dfxl[[c[1]]][r_i]))
            break
          else {
            type <- dfxl[[c[1]]][r_i]
            r_i <- r_i + 1
            next
          }
        }
        name <- dfxl[[c[2]]][r_i]
        if (is.na(name) && !is.na(age))
          name <- ""
        
        column$name <- c(column$name, name)
        column$age <- c(column$age, as.numeric(age))
        if (!is.na(type))
          column$type <- c(column$type, type)
        r_i <- r_i + 1
      }
      cols[[cnum]]$content <- column
      cnum <- cnum + 1
    }
  }
  columns[[ct]] <- cols
}

summary(columns['block'])
summary(columns['event'])

# regional stage cols
regional_units <- c()
regional_unit_row_nums <- c()
for(c_i in c[3:12]) {
  regional_unit <- dfxl[[c_i]][630]
  regional_units <- c(regional_units, regional_unit)
}

print(regional_units)

stages <- c()
series <- c()
lithostrat <- c()
chronostrat <- c()
others <- c()
for(r_u in regional_units) {
  rn <- which(dfxl[[1]] == r_u)
  for(c_i in c[3:10]) {
    str <- dfxl[[c_i]][rn]
    if(grepl('stage', tolower(str))) {
      stages <- c(stages, str) 
    }
    else if (grepl('series', tolower(str))) {
      series <- c(series, str)
    }
    else if (grepl('lithostratigraphy', tolower(str))) {
      lithostrat <- c(lithostrat, str)
    } 
    else if (grepl('chronostratigraphy', tolower(str))) {
      chronostrat <- c(chronostrat, str)
    }
    else if (!is.na(str) & str != "_METACOLUMN_OFF") {
      others <- c(others, str)
    }
    else {
      #print(str)
    }
  }
}

print(stages)
print(series)
print(lithostrat)
print(chronostrat)
print(others)

col_type <- 'event'

ncol <- length(columns[[col_type]])

start_age <- STARTING_AGE
end_age <- ENDING_AGE
age_diff <- AGE_SLIDE
events <- list()
event_names <- list()
event_types <- list()

while(start_age <= end_age) {
  next_age <- start_age + age_diff
  mid_age <- (start_age + next_age) / 2

  key <- as.character(mid_age)
  evs <- 0
  ev_names <- c()
  ev_types <- c()
  
  for(col in columns[[col_type]]){
    cn <- length(col$content$age)
    for (k in 1:cn) {
      if (length(col$content) == 0)
        next
      a <- col$content$age[k]
      n <- col$content$name[k]
      t <- col$content$type[k]
      if (a >= start_age && a < next_age) {
        evs <- as.numeric(evs) + 1
        ev_names <- c(ev_names, n)
        ev_types <- c(ev_types, t)
      }
      k <- k+1
    }
  }
  events[[key]] <- evs
  event_names[[key]] <- ev_names
  if(col_type == 'event')
    event_types[[key]] <- ev_types
  start_age <- next_age
}

age <- as.numeric(names(events))
freq <- as.numeric(unlist(events))
ev_df <- data.frame(age = age, freq = freq)
library(plotly)
p <- plot_ly(data=ev_df, 
             x=~age, y=~freq, 
             type='scatter',
             mode='lines',
             line=list(color='black'))
p

dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/Global_Sediment_Facies_Change/data/'
event_names_fn <- paste(col_type, '_column_per_mil_event_names_phanerozoic.txt', sep="")
fn0 <- paste(dir, event_names_fn, sep="")
fn0
sink(fn0)
print(event_names)
sink()

event_freq_datapack_file_name <- paste('Phanerozoic_', col_type, '_column_event_frequencies.txt', sep="")
fn <- paste(dir, event_freq_datapack_file_name, sep="")

sink(fn)
str <- 'format version:	1.3\nage units:	Ma\n'
writeLines(str)
str <- paste(col_type, ' Column Per Million Year Event Frequencies\tpoint\t150\t255/245/230', sep="")
writeLines(str)
nf <- length(freq)
for (l in 1:nf) {
  a <- age[l]
  f <- freq[l]
  ln <- paste("\t", a, "\t", f, sep='')
  writeLines(ln)
}
sink()
file.show(fn)

