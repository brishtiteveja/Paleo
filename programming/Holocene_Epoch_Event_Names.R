project_dir <- '/Users/andy/Dropbox/TSCreator/TSCreator development/Developers/Andy/Projects/ML-Data Mining/programming/'
subdir <- 'output/'
fn <- 'HumanCulture_MidEastIntervals_50.0yr_bin_50.0yr_slide_window.txt'
event_fn <- paste(project_dir, subdir, fn, sep='')

df <- data.frame(read.csv(event_fn, header = TRUE, sep = "\t"))

df.ev <- list()
i <- 1
for (e in df$events) {
  e <- gsub("\\[","",e)
  e <- gsub("\\]","",e)
  e <- gsub(" \'","\'",e)
  el <- strsplit(e, split=",")
  eln <- c()
  for(e2 in el[[1]]) {
    if (e2 != "\'TOP\'")
       eln <- c(eln, e2) 
  }
  df.ev[[i]] <- eln
  i <- i+1
}

df <- df[,-3]
df$evs <- df.ev
colnames(df) <- c('Begin Age(Ka)', 'End Age(Ka)', 'Event Names')
library(DT)
datatable(df)

library(wordcloud)
corp <- c()
for(e in df$`Event Names`) {
  if(length(e) > 0) {
   for (e2 in e) {
     corp <- c(corp, e2)
   }
  }
}
pal <- brewer.pal(6,"Dark2")
pal <- pal[-(1)]
w <- wordcloud(corp, max.words=500, random.order = FALSE,colors = pal)








library(tm)
data(SOTU)
corp <- SOTU
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, content_transformer(tolower))
corp <- tm_map(corp, removeNumbers)
corp <- tm_map(corp, function(x)removeWords(x, stopwords()))

term.matrix <- TermDocumentMatrix(corp)
term.matrix <- as.matrix(term.matrix)
comparison.cloud(term.matrix, max.words=100, random.order = FALSE)
commonality.cloud(term.matrix,max.words=40,random.order=FALSE)


#calculate standardized MDS coordinates
dat <- sweep(USArrests,2,colMeans(USArrests))
dat <- sweep(dat,2,sqrt(diag(var(dat))),"/")
loc <- cmdscale(dist(dat))
#plot with no overlap
textplot(loc[,1],loc[,2],rownames(loc))
#scale by urban population size
textplot(loc[,1],loc[,2],rownames(loc),cex=USArrests$UrbanPop/max(USArrests$UrbanPop))
#x limits sets x bounds of plot, and forces all words to be in bounds
textplot(loc[,1],loc[,2],rownames(loc),xlim=c(-3.5,3.5))
#compare to text (many states unreadable)
plot(loc[,1],loc[,2],type="n")
text(loc[,1],loc[,2],rownames(loc))

wordcloud(c(letters, LETTERS, 0:9), seq(1, 1000, len = 62))
if(require(tm)){
  ##### from character #####
  wordcloud(
    "Many years ago the great British explorer George Mallory, who
    was to die on Mount Everest, was asked why did he want to climb
    it. He said, \"Because it is there.\"
    Well, space is there, and we're going to climb it, and the
    moon and the planets are there, and new hopes for knowledge
    and peace are there. And, therefore, as we set sail we ask
    God's blessing on the most hazardous and dangerous and greatest
    adventure on which man has ever embarked.",
    ,random.order=FALSE)
  ## Not run:
  data(crude)
  crude <- tm_map(crude, removePunctuation)
  crude <- tm_map(crude, function(x)removeWords(x,stopwords()))
  ##### from corpus #####
  wordcloud(crude)
  ##### from frequency counts #####
  tdm <- TermDocumentMatrix(crude)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  wordcloud(d$word,d$freq)
  #A bigger cloud with a minimum frequency of 2
  wordcloud(d$word,d$freq,c(8,.3),2)
  #Now lets try it with frequent words plotted first
  wordcloud(d$word,d$freq,c(8,.5),2,,FALSE,.1)
  ##### with colors #####
  if(require(RColorBrewer)){
    pal <- brewer.pal(9,"BuGn")
    pal <- pal[-(1:4)]
    wordcloud(d$word,d$freq,c(8,.3),2,,FALSE,,.15,pal)
    pal <- brewer.pal(6,"Dark2")
    pal <- pal[-(1)]
    wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,,.15,pal)
    #random colors
    wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,TRUE,.15,pal)
  }
  ##### with font #####
  wordcloud(d$word,d$freq,c(8,.3),2,,TRUE,,.15,pal,
            vfont=c("gothic english","plain"))
  wordcloud(d$word,d$freq,c(8,.3),2,100,TRUE,,.15,pal,vfont=c("script","plain"))
  wordcloud(d$word,d$freq,c(8,.3),2,100,TRUE,,.15,pal,vfont=c("serif","plain"))
  ## End(Not run)
}