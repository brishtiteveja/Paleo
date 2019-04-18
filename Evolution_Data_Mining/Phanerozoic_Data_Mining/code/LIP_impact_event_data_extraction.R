# LIP event data extraction
data_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/Datapacks'
setwd(data_dir)
dp_fname <- 'Phan_GTS2016_for_7.1_HaqJur_ForamMikrotax_28July2017.xls'
library(readxl)
sheets <- excel_sheets(dp_fname)
sheets
dfxl <- read_excel(dp_fname) #, sheet=sheets[1], range = "A13:F1987")
c <- colnames(dfxl) # column header
df2c <- dfxl[[c[2]]]

column_type <- c('event')

columns <- list()
for (ct in column_type) {
  msg <- paste("processing ", ct, " column.\n",
               "----------------------------------------------------------------------",
               sep="")
  print(msg)
  columns[[ct]] <- list() 
  
  # get excel row num for each column
  rows <- which(df2c == ct) + 1
  
  
  cols <- list()
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
        line_type <- dfxl[[c[4]]][r_i]
        popup <- dfxl[[c[5]]][r_i]
        
        column$name <- c(column$name, name)
        column$age <- c(column$age, as.numeric(age))
        if (!is.na(type))
          column$type <- c(column$type, type)
        column$line_type <- c(column$line_type, line_type)
        column$popup <- c(column$popup, popup)
        r_i <- r_i + 1
      }
      cols[[cnum]]$content <- column
      cnum <- cnum + 1
    }
  } 
  
  columns[[ct]] <- cols
}

get_parent_hierarchy <- function(col_name) {
  parent_columns <- list()
  if (col_name %in% main_columns) {
    parent_columns[["main column"]] = main_column
    return(parent_columns)
  } else {
    for(main_column in main_columns) {
      # number of sub-columns
      sub_column_num <- length(main_column_list[[main_column]])
      if (sub_column_num == 0)
        next
      sub_columns <- names(main_column_list[[main_column]][1:sub_column_num])
      if (col_name %in% sub_columns) {
        parent_columns[["main column"]] = main_column
        isx <- which(col_name == sub_columns)
        parent_columns[["sub column"]] = sub_columns[[isx]]
        return(parent_columns)
      }
      else {
        for(sub_column_n in sub_columns) {
          sub_column <- main_column_list[[main_column]][[sub_column_n]]
          sub_sub_columns <- names(sub_column)
          sub_sub_column_num <- length(sub_sub_columns)
          if (sub_sub_column_num == 0)
            next
          if (col_name %in% sub_sub_columns) {
            parent_columns[["main column"]] = main_column
            isx <- which(sub_column_n == sub_columns)
            parent_columns[["sub column"]] = sub_columns[[isx]]
            issx <- which(col_name == sub_sub_columns)
            parent_columns[["sub sub column"]] = sub_sub_columns[[issx]]
            return(parent_columns)
          }
          for(sub_sub_column_n in sub_sub_columns) {
            sub_sub_sub_columns <- main_column_list[[main_column]][[sub_column_n]][[sub_sub_column_n]]
            sub_sub_sub_column_num <- length(sub_sub_sub_columns)
            if (sub_sub_sub_column_num == 0)
              next
            if (col_name %in% sub_sub_sub_columns) {
              parent_columns[["main column"]] = main_column
              isx <- which(sub_column_n == sub_columns)
              parent_columns[["sub column"]] = sub_columns[[isx]]
              issx <- which(sub_sub_column_n == sub_sub_columns)
              parent_columns[["sub sub column"]] = sub_sub_columns[[issx]]
              isssx <- which(col_name == sub_sub_sub_columns)
              parent_columns[["sub sub sub column"]] = sub_sub_sub_columns[[isssx]]
              return(parent_columns)
            }
          }
        }
      }
    }
  }
  
  return(NA)
}

get_main_column_name <- function(col_name) {
  parent = get_parent_hierarchy(col_name)
  
  if(is.na(parent) || is.na(parent[["main column"]])) {
    return(NA)
  }
  
  mc = parent[["main column"]]
  
  return(mc)
}

get_sub_column_name <- function(col_name) {
  parent = get_parent_hierarchy(col_name)
  
  if(is.na(parent) || is.na(parent[["sub column"]])) {
    return(NA)
  }
  
  sc = parent[["sub column"]]
  
  return(sc)
}

get_sub_sub_column_name <- function(col_name) {
  parent = get_parent_hierarchy(col_name)
  if(is.na(parent) || is.na(parent[["sub sub column"]])) {
    return(NA)
  }
  
  ssc = parent[["sub sub column"]]
  
  return(ssc)
}

not_column <- function(col_name) {
  if (col_name == "_metacolumn_off" || col_name == "_metacolumn_on" || 
      col_name == "_METACOLUMN_OFF" || col_name == "_METACOLUMN_ON" 
      || col_name == "pass" ||
      substring(col_name, 2, 18) == "for details click")
    return(TRUE)
  else if (col_name == 'off' || col_name == 'on') {
    return(TRUE)
  } else {
    # # column width
    m <- regexec("^[0-9]+$",col_name)
    ml <- regmatches(col_name, m)
    if (length(ml[[1]]) == 1) {
      return(TRUE)
    }
    
    # color code
    m <- regexec("([0-9])+/([0-9])+/([0-9])+",col_name)
    ml <- regmatches(col_name, m)
    
    if (length(ml[[1]]) != 0) {
      return(TRUE)  
    }
    
    return(FALSE)
  }
}

# main columns
# 

column_names_by_category <- list()
extract_column_names_by_category <- function() {
  main_columns <- c()
  sub_columns <- c()
  
  rix <- which(df2c == ":")
  for (r in rix) {
    cn1 <- as.character(dfxl[r, 1])
    if (cn1 %in% sub_columns) {
      
    } else {
      main_columns <- c(main_columns, cn1)
      nc <- length(dfxl[r,])
      sub_col_ns <- as.character(dfxl[r, 3 : nc])
      naix <- which(is.na(sub_col_ns))
      naix <- c(naix, which(sub_col_ns == "na"))
      
      for(n in 1:nc) {
        if (n %in% naix) {
          
        } else {
          sub_col_name <- sub_col_ns[n]
          if(!is.na(sub_col_name) && 
             (sub_col_name != "_metacolumn_off" || sub_col_name != "_metacolumn_on" || 
              sub_col_name != "pass")) {
            sub_columns <- c(sub_columns, sub_col_name)
          }
        }
      }
    }
  }
  column_names_by_category[["main column"]] <- main_columns <- unique(na.omit(main_columns))
  column_names_by_category[["sub column"]] <- sub_columns <- unique(na.omit(sub_columns))
  
  return(column_names_by_category)
}

# extract column names
# main_column ~ group in marine genera
# sub_column ~ sub group
# sub_sub_column ~ column
column_names <- extract_column_names_by_category()
main_column_names <- main_columns <- column_names[["main column"]]
sub_column_names <- column_names[["sub column"]]
n_sub_sub_column <- length(columns$event)
sub_sub_column_names <- c()
for (nsc in 1:n_sub_sub_column) {
  sub_sub_column_names <- c(sub_sub_column_names, columns[["event"]][[nsc]][["name"]])
}

LIP_event_col_names <- c('Super LIP Events',
                         'Major LIP Events',
                         'Asian LIPs',
                         'Europe to Urals LIPs',
                         'African LIPs',
                         'N. American LIPs',
                         'S.America LIPs',
                         'India & Indian Ocean LIPs',
                         'Australia-Antarctica LIPs'
)

ages_from_col3 <- na.omit(as.numeric(as.character(na.omit(dfxl[[c[3]]]))))
age_slide <- 0.5 # every 500K years 
#starting_age <- floor(min(ages_from_col3)) - age_slide/2
starting_age <- 0 - age_slide/2
cenozoic_base <- 66.04
ending_age <- 600 #ceiling(max(ages_from_col3)) + age_slide/2
#ending_age <- ceiling(cenozoic_base)
ending_age

events_by_col <- list()
event_names_by_col <- list()
ev_df_by_col <- list()
events_by_col_by_main_columns <- list()
event_names_by_col_by_main_columns <- list()
ev_df_by_main_columns <- list()

column_type <- c('event')
for(col_type in column_type) {
  msg <- paste("processing ", col_type, " column events.\n",
               "----------------------------------------------------------------------",
               sep="")
  print(msg)
  ncol <- length(columns[[col_type]])
  
  msg <-paste("total number of " , col_type , " columns = ", ncol, sep ="") 
  print(msg)
  
  if (ncol == 0) {
    msg <- paste("no column data or not extracted yet.")
    next
  }
  
  start_age <- starting_age
  end_age <- ending_age
  age_diff <- age_slide
  
  events <- list()
  event_names <- list()
  event_types <- list()
  events_by_main_columns <- list()
  event_names_by_main_columns <- list()
  
  while(start_age <= end_age) {
    next_age <- start_age + age_diff
    mid_age <- (start_age + next_age) / 2
    print("")
    print(paste("Processing age = ", mid_age, sep=""))
    print("------------------------------------")
    
    key <- as.character(mid_age)
    evs <- 0
    ev_names <- c()
    ev_types <- c()
    
    # column wise events
    evs_by_main_columns <- list()
    ev_names_by_main_columns <- list()
    for(main_column in main_columns) {
      evs_by_main_columns[[main_column]] <- 0
      ev_names_by_main_columns[[main_column]] <- c()
    }
    
    for(col in columns[[col_type]]){
      # Here only process for the margin columns
      if (col$name %in% LIP_event_col_names) {
        print(col$name)
      }
      else{ 
        
        next 
      }
      
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
          
          # adding to the main_column 
          main_column <- get_main_column_name(col$name)
          if(is.na(main_column)) {
            #print(paste("something is wrong with column ", col, " for main_column ", main_column, sep=""))
          } else {
            evs_by_main_columns[[main_column]] <- as.numeric(evs_by_main_columns[[main_column]]) + 1
            ev_names_by_main_columns[[main_column]] <- c(ev_names_by_main_columns[[main_column]], n)
          }
        }
        k <- k+1
      }
    }
    events[[key]] <- evs
    event_names[[key]] <- ev_names
    if(col_type == 'event')
      event_types[[key]] <- ev_types
    
    events_by_main_columns[[key]] <- list()
    event_names_by_main_columns[[key]] <- list()
    for (main_column in main_columns) {
      events_by_main_columns[[key]][[main_column]] <- evs_by_main_columns[[main_column]]
      event_names_by_main_columns[[key]][[main_column]] <- ev_names_by_main_columns[[main_column]]
    }
    start_age <- next_age
  }
  
  age <- as.numeric(names(events))
  freq <- as.numeric(unlist(events))
  freq_lad <- c()
  freq_fad <- c()
  for(key in names(events)) {
    et <- event_types[[key]]
    if(is.null(et)) {
      fad = 0
      lad = 0
    } else {
      fad = length(et[et=="FAD"])
      lad = length(et[et=="LAD"])
    }
    freq_fad <- c(freq_fad, fad)
    freq_lad <- c(freq_lad, lad)
  }
  
  ev_df <- data.frame(age = age, freq = freq, freq_fad = freq_fad, freq_lad = freq_lad)
  
  data_mining_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Phanerozoic_Data_Mining'
  data_dir <- paste(data_mining_dir, '/Datapack/LIP_event_extraction/', sep="")
  event_names_fn <- paste(col_type, '_column_', age_slide, '_mil_event_names.txt', sep="")
  fn0 <- paste(data_dir, event_names_fn, sep="")
  msg <- paste("writing event names in ", fn0, sep="")
  print(msg)
  
  # printing event names to file
  sink(fn0)
  print(event_names)
  sink()
  
  event_freq_datapack_file_name <- paste(col_type, '_column_every_', age_slide,'_mil_event_frequencies_datapack.txt', sep="")
  fn <- paste(data_dir, event_freq_datapack_file_name, sep="")
  msg <- paste("writing datapack ", fn, sep="")
  print(msg)
  
  sink(fn)
  str <- 'format version:	1.3\nage units:	ma\n'
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year event frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  
  # write fad frequencies
  str="\n"
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year fad frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq_fad)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq_fad[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  
  # write lad frequencies
  str="\n"
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year lad frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq_lad)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq_lad[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  sink()
  #file.show(fn)
  
  event_names_by_col[[col_type]] <- event_names
  events_by_col[[col_type]] <- events
  ev_df_by_col[[col_type]] <- ev_df
  
  events_by_col_by_main_columns[[col_type]] <- list()
  event_names_by_col_by_main_columns[[col_type]] <- list()
  ev_df_by_main_columns[[col_type]] <- list()
  for (main_column in main_columns) {
    age_r <- c()
    freq_r <- c()
    events_r <- c()
    event_names_r <- list()
    for(age in names(events_by_main_columns)) {
      events_r <- c(events_r, events_by_main_columns[[age]][[main_column]])
      event_names_r[[age]] <- event_names_by_main_columns[[age]][[main_column]]
      age_r <- c(age_r, as.numeric(age))
      freq_r <- c(freq_r, as.numeric(events_by_main_columns[[age]][[main_column]]))
    }
    events_by_col_by_main_columns[[col_type]][[main_column]] <- events_r
    event_names_by_col_by_main_columns[[col_type]][[main_column]] <- event_names_r
    ev_df_by_main_columns[[col_type]][[main_column]] <- data.frame(age = age_r, freq = freq_r)
  }
}

df = data.frame(ev_df_by_col$event)
library(DT)
datatable(df)

plot(df$age, df$freq, 
     t='l', yaxt='n',
     xlab='age (Ma)',
     ylab='Number of opening and closing event',
     main = 'Passive margin opening and closing event'
)
points(df$age, df$freq, cex=0.5)
lines(df$age, df$freq_fad, t='l') # margin start/opening
lines(df$age, df$freq_lad, t='l') # marging end/ending
LIP_df <- data.frame(age=df$age,
                      LIP_cnt=df$freq
)
head(LIP_df)
tail(LIP_df)

min_age <- min(LIP_df$age, na.rm = T)
mn_a <- floor(min_age) - 0.5
max_age <- max(LIP_df$age)
mx_a <- ceiling(max_age) + 0.5

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
ages <- list()
LIP_avg <- list()

i <- 1
for(a in age_seq) {
  age <- (age_seq[i] + age_seq[i+1])/2
  ix <- which(LIP_df$age >= age_seq[i] & LIP_df$age < age_seq[i+1])
  nix <- length(ix)
  
  ages[i] <- age
  LIP_avg[i] <- sum(LIP_df$LIP_cnt[ix])/nix
  
  i <- i+1
}

LIP_avg_df <- data.frame(age=unlist(ages),
                         LIP_avg_cnt=unlist(LIP_avg))
LIP_avg_dff <- data.frame(na.approx(LIP_avg_df))

head(LIP_avg_dff)
tail(LIP_avg_dff)

plot(LIP_avg_dff$age, 
     LIP_avg_dff$LIP_avg_cnt,
     t='l', #yaxt='n',
     xlab='age (Ma)',
     ylab='Number of LIP events',
     main = 'Global LIP events',
     lty=1, lwd=2
)


# Impact event data extraction
impact_event_col_names <- c(
                         'Global effect (>50 km crater)',
                         'Major European (5-50 km)',
                         'Minor European (<5km)',
                         'Major Asian (5-50 km)',
                         'Minor Asian (<5km)',
                         'Major Australian (5-50 km)',
                         'Minor Australian (<5km)',
                         'Major African (5-50 km)',
                         'Minor African (<5km)',
                         'Major N.Amer. (5-50 km)',
                         'Minor N.Amer. (<5km)',
                         'Major S.Amer. (5-50 km)',
                         'Minor S.Amer. (<5km)'
                         )

ages_from_col3 <- na.omit(as.numeric(as.character(na.omit(dfxl[[c[3]]]))))
age_slide <- 0.5 # every 500K years 
#starting_age <- floor(min(ages_from_col3)) - age_slide/2
starting_age <- 0 - age_slide/2
cenozoic_base <- 66.04
ending_age <- 600 #ceiling(max(ages_from_col3)) + age_slide/2
#ending_age <- ceiling(cenozoic_base)
ending_age

events_by_col <- list()
event_names_by_col <- list()
ev_df_by_col <- list()
events_by_col_by_main_columns <- list()
event_names_by_col_by_main_columns <- list()
ev_df_by_main_columns <- list()

column_type <- c('event')
for(col_type in column_type) {
  msg <- paste("processing ", col_type, " column events.\n",
               "----------------------------------------------------------------------",
               sep="")
  print(msg)
  ncol <- length(columns[[col_type]])
  
  msg <-paste("total number of " , col_type , " columns = ", ncol, sep ="") 
  print(msg)
  
  if (ncol == 0) {
    msg <- paste("no column data or not extracted yet.")
    next
  }
  
  start_age <- starting_age
  end_age <- ending_age
  age_diff <- age_slide
  
  events <- list()
  event_names <- list()
  event_types <- list()
  events_by_main_columns <- list()
  event_names_by_main_columns <- list()
  
  while(start_age <= end_age) {
    next_age <- start_age + age_diff
    mid_age <- (start_age + next_age) / 2
    print("")
    print(paste("Processing age = ", mid_age, sep=""))
    print("------------------------------------")
    
    key <- as.character(mid_age)
    evs <- 0
    ev_names <- c()
    ev_types <- c()
    
    # column wise events
    evs_by_main_columns <- list()
    ev_names_by_main_columns <- list()
    for(main_column in main_columns) {
      evs_by_main_columns[[main_column]] <- 0
      ev_names_by_main_columns[[main_column]] <- c()
    }
    
    for(col in columns[[col_type]]){
      # Here only process for the margin columns
      if (col$name %in% impact_event_col_names) {
        print(col$name)
      }
      else{ 
        
        next 
      }
      
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
          
          # adding to the main_column 
          main_column <- get_main_column_name(col$name)
          if(is.na(main_column)) {
            #print(paste("something is wrong with column ", col, " for main_column ", main_column, sep=""))
          } else {
            evs_by_main_columns[[main_column]] <- as.numeric(evs_by_main_columns[[main_column]]) + 1
            ev_names_by_main_columns[[main_column]] <- c(ev_names_by_main_columns[[main_column]], n)
          }
        }
        k <- k+1
      }
    }
    events[[key]] <- evs
    event_names[[key]] <- ev_names
    if(col_type == 'event')
      event_types[[key]] <- ev_types
    
    events_by_main_columns[[key]] <- list()
    event_names_by_main_columns[[key]] <- list()
    for (main_column in main_columns) {
      events_by_main_columns[[key]][[main_column]] <- evs_by_main_columns[[main_column]]
      event_names_by_main_columns[[key]][[main_column]] <- ev_names_by_main_columns[[main_column]]
    }
    start_age <- next_age
  }
  
  age <- as.numeric(names(events))
  freq <- as.numeric(unlist(events))
  freq_lad <- c()
  freq_fad <- c()
  for(key in names(events)) {
    et <- event_types[[key]]
    if(is.null(et)) {
      fad = 0
      lad = 0
    } else {
      fad = length(et[et=="FAD"])
      lad = length(et[et=="LAD"])
    }
    freq_fad <- c(freq_fad, fad)
    freq_lad <- c(freq_lad, lad)
  }
  
  ev_df <- data.frame(age = age, freq = freq, freq_fad = freq_fad, freq_lad = freq_lad)
  
  data_mining_dir <- '/Users/andy/Documents/projects/ML-Data Mining/Evolution_Data_Mining/Phanerozoic_Data_Mining'
  data_dir <- paste(data_mining_dir, '/Datapack/impact_event_extraction/', sep="")
  event_names_fn <- paste(col_type, '_column_', age_slide, '_mil_event_names.txt', sep="")
  fn0 <- paste(data_dir, event_names_fn, sep="")
  msg <- paste("writing event names in ", fn0, sep="")
  print(msg)
  
  # printing event names to file
  sink(fn0)
  print(event_names)
  sink()
  
  event_freq_datapack_file_name <- paste(col_type, '_column_every_', age_slide,'_mil_event_frequencies_datapack.txt', sep="")
  fn <- paste(data_dir, event_freq_datapack_file_name, sep="")
  msg <- paste("writing datapack ", fn, sep="")
  print(msg)
  
  sink(fn)
  str <- 'format version:	1.3\nage units:	ma\n'
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year event frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  
  # write fad frequencies
  str="\n"
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year fad frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq_fad)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq_fad[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  
  # write lad frequencies
  str="\n"
  writeLines(str)
  str <- paste(col_type, ' column ', age_slide, ' million year lad frequencies\tpoint\t150\t255/245/230', sep="")
  writeLines(str)
  nf <- length(freq_lad)
  for (l in 1:nf) {
    a <- age[l]
    f <- freq_lad[l]
    ln <- paste("\t", a, "\t", f, sep='')
    writeLines(ln)
  }
  sink()
  #file.show(fn)
  
  event_names_by_col[[col_type]] <- event_names
  events_by_col[[col_type]] <- events
  ev_df_by_col[[col_type]] <- ev_df
  
  events_by_col_by_main_columns[[col_type]] <- list()
  event_names_by_col_by_main_columns[[col_type]] <- list()
  ev_df_by_main_columns[[col_type]] <- list()
  for (main_column in main_columns) {
    age_r <- c()
    freq_r <- c()
    events_r <- c()
    event_names_r <- list()
    for(age in names(events_by_main_columns)) {
      events_r <- c(events_r, events_by_main_columns[[age]][[main_column]])
      event_names_r[[age]] <- event_names_by_main_columns[[age]][[main_column]]
      age_r <- c(age_r, as.numeric(age))
      freq_r <- c(freq_r, as.numeric(events_by_main_columns[[age]][[main_column]]))
    }
    events_by_col_by_main_columns[[col_type]][[main_column]] <- events_r
    event_names_by_col_by_main_columns[[col_type]][[main_column]] <- event_names_r
    ev_df_by_main_columns[[col_type]][[main_column]] <- data.frame(age = age_r, freq = freq_r)
  }
}

df = data.frame(ev_df_by_col$event)
library(DT)
datatable(df)

plot(df$age, df$freq, 
     t='l', yaxt='n',
     xlab='age (Ma)',
     ylab='Number of opening and closing event',
     main = 'Passive margin opening and closing event'
)
points(df$age, df$freq, cex=0.5)
lines(df$age, df$freq_fad, t='l') # margin start/opening
lines(df$age, df$freq_lad, t='l') # marging end/ending
impact_df <- data.frame(age=df$age,
                     impact_cnt=df$freq
)
head(impact_df)
tail(impact_df)

min_age <- min(impact_df$age, na.rm = T)
mn_a <- floor(min_age) - 0.5
max_age <- max(impact_df$age)
mx_a <- ceiling(max_age) + 0.5

slide_window <- 1
age_seq <- seq(mn_a, mx_a, by=slide_window)
age_seq
ages <- list()
impact_avg <- list()

i <- 1
for(a in age_seq) {
  age <- (age_seq[i] + age_seq[i+1])/2
  ix <- which(impact_df$age >= age_seq[i] & impact_df$age < age_seq[i+1])
  nix <- length(ix)
  
  ages[i] <- age
  impact_avg[i] <- sum(impact_df$impact_cnt[ix])/nix
  
  i <- i+1
}

impact_avg_df <- data.frame(age=unlist(ages),
                         impact_avg_cnt=unlist(impact_avg))
impact_avg_dff <- data.frame(na.approx(impact_avg_df))

head(impact_avg_dff)
tail(impact_avg_dff)

plot(impact_avg_dff$age[-1], 
     impact_avg_dff$impact_avg_cnt[-1],
     t='l', 
     #yaxt='n',
     xlab='age (Ma)',
     ylab='Number of impact events',
     main = 'Global/Regional Impact events',
     lty=1, lwd=2
)

