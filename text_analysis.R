#-------------------------------------#
#       Text analysis module          #
#-------------------------------------#

dir = "D:/Webscrape/webscrape"
setwd(dir)
source("functions.R")




# Generate Term Document matrix
### Corpus
clean_corpus = function(text) {
  docs = Corpus(VectorSource(text$content))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, tolower)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, removeNumbers) 
  docs <- tm_map(docs, PlainTextDocument)
  return(docs)
}
### Document term matrix
docs = clean_corpus(text_sample)
dtm <- DocumentTermMatrix(docs)
freq <- colSums(as.matrix(dtm))
ord <- order(freq)
freq[head(ord)]

findFreqTerms(dtm, lowfreq=1000)
wf <- data.frame(word=names(freq), freq=freq)


dtms <- removeSparseTerms(dtm, 0.4)
freq <- colSums(as.matrix(dtms))   
freq


library(ggplot2)   
p <- ggplot(subset(wf, freq>500), aes(word, freq))    
p <- p + geom_bar(stat="identity")   
p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
p   


findAssocs(dtm, c("hối" , "nhũng"), corlimit=0.4)

library(wordcloud)
set.seed(142)   
wordcloud(names(freq), freq, min.freq=200, scale=c(5, .1), colors=brewer.pal(6, "Dark2"))   
wordcloud(names(freq), freq, max.words=50, rot.per=0.2, colors=brewer.pal(6, "Dark2"))  
# clustering
d <- dist(t(dtms), method="euclidian")   
fit <- hclust(d=d, method="ward.D")   
fit
plot(fit, hang=-1) 


# Prediction model

# 




#-------------------------------------#
####          Execution            ####
#-------------------------------------#

# Call data
text = call_data("vnexpress","")

# Clean data
### Remove duplicated and non-existed records
text = text[!duplicated(text$link),]
text = text[!is.na(text$date) & text$content != "error",]

### Filter records in time range
start_date = clean_date("01/01/2006")
end_date = today()
text = text[(text$date >= start_date) & (text$date <= end_date),]

### Create column of month (=ending date of month)
text$month = ceiling_date(text$date, unit = "month")-1

# Basic count
count_results = basic_count(text, keywords, 1)
count_results_all = count_results[["title_count"]] + count_results[["content_count"]]
text = cbind(text, count = count_results_all)
text$contain = text$count > 0
text = text[text$contain==TRUE,]
text_sample = dplyr:: sample_frac(text, 0.1)
rm(text)

### Visuals
count_group = group_by(text,month,category) %>% summarise(contain = sum(contain))
count_group = count_group[order(count_group$month),]
plot_by_cat = ggplot(count_group,aes(x=month,y=contain)) +
  geom_line() + facet_wrap(~category) + theme_light()
plot_total = ggplot(count_group,aes(x=month,y=contain)) +
  geom_line()+ theme_light()



