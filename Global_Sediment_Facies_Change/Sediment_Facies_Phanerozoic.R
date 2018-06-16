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

column_type <- c('block', 'transect', 'facies', 'event', 'curve', 
                 'range', 'freehand-overlay', 'overlay', 'point',
                 'point-overlay')

columns <- list()
for (ct in column_type) {
  columns[[ct]] <- list() 
  
  # get excel row num for each column
  rows <- which(df2c == ct) + 1
  
  cols <- list()
  if (ct == 'block' || ct == 'event') {
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
  columns[[ct]] <- cols
}

col_type <- 'block'

events <- list()
event_names <- list()
ncol <- length(columns[[col_type]])

for(col in columns[[col_type]]){
  start_age <- STARTING_AGE
  end_age <- ENDING_AGE
  age_diff <- AGE_SLIDE
  
  while(start_age <= end_age) {
    next_age <- start_age + age_diff
    mid_age <- (start_age + next_age) / 2
    
    key <- as.character(mid_age)
    if(is.null(unlist(events[[key]]))) {
      events[[key]] <- 0
    }
    if(is.null(unlist(event_names[[key]]))) {
      event_names[[key]] <- c()
    }
    
    cn <- length(col$content$age)
    for (k in 1:cn) {
      a <- col$content$age[k]
      n <- col$content$name[k]
      if (a >= start_age && a < end_age) {
        events[[key]] <- events[[key]] + 1
        event_names[[key]] <- c(event_names[[key]], n)
      }
      k <- k+1
    }
    start_age <- next_age
  }
}

age <- as.numeric(names(events))
freq <- as.numeric(unlist(events))
ev_df <- data.frame(age = age, freq = freq)
library(plotly)
p <- plot_ly(data=ev_df, x=~age, y=~freq)
p
