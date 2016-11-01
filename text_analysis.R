
#---------------------------------------#
####            ANALYSIS             ####
#---------------------------------------#
dir = "D:/Webscrape/webscrape"
setwd(dir)
source("functions.R")
#**** Keywords count ****#
#__Data processing ####
site = c("vnexpress", "dantri", "thanhnien")
start_year = 2006
end_year = 2016
for (s in site) {
  for (year in c(start_year:end_year)) {
    cat("Processing ", s, " year ", year)
    # Call
    text = call_data(site = s, index = paste("_", year, sep = ""))
    
    # Split
    two_para = lapply(text$content,FUN = first_real_text, n=2) %>% unlist()
    text_intro = text
    text_intro$content = two_para
    
    # Clean
    text$title = stri_trans_tolower(text$title)
    text$content = stri_trans_tolower(text$content)
    text$content = removePunctuation(text$content)
    text$content = stripWhitespace(text$content)
    
    # Save
    save_final(text, s, paste(s, "_", year, "_processed.csv", sep = ""))
    save_final(text_intro, s, paste(s, "_", year, "_intro.csv", sep = ""))
    rm(text, text_intro, two_para)
    gc()
  }
}


#__Counting ####
#____ Input _____#
#1 
keywords = c("cá chết hàng loạt", "cá chết", "ô nhiễm biển miền trung", "formosa", "công bố nguyên nhân cá chết") %>% tolower()
keycode = c("cachethangloat", "cachet", "onhiembienmientrung", "formosa", "congbonguyennhancachet")
#2 
keywords = c("tham nhũng", "hối lộ", "đút lót", "lợi dụng chức vụ", "lợi dụng quyền hạn", "biển thủ công quỹ") %>% tolower()
keycode = c("thamnhung", "hoilo","dutlot", "loidungchucvu", "loidungquyenhan", "bienthucongquy")
#3
keywords = tinhthanh$tinhthanh
keycode = paste("tinh.", c(1:63), sep = "")

site = c("vnexpress", "dantri", "thanhnien")

start_year = 2006
end_year = 2016
### count in content? (contentcount: 0-no, 1-first 2 paragraphs; 2-full content; 3-all) 
contentcount = 2
### Autosave (auto write results and delete objects from RAM) ? (0-no, 1-yes)
autosave = 1

#______ Do ______#
#s = site[1]
#year = 2006

for (s in (site)) {
  for (kcode in keycode) {
    assign(paste(s, kcode, sep = "_"), c())
  }
  for (year in c(start_year:end_year)) {
    cat("Reading ", year, " of ", s)
    text = call_data(site = s, index = paste("_", year, "_processed", sep = ""))
    if (contentcount %in% c(1, 3)) {
      twopara = call_data(site = s, index = paste("_", year, "_intro", sep = ""))
    }
    # Cleaning
    ### Remove duplicated and non-existing records
    text = text[!duplicated(text$link),]
    text = text[!is.na(text$date),]
    
    ### Fix category (to be done..)
    
    # Counting
    count_result = basic_count(text, keywords, keycode, contentcount)
    
    # Creating objects
    for (i in c(1:length(count_result))) {
      text_count = cbind(text,count_result[[i]])
      text_count$title = NULL
      text_count$content = NULL
      assign(paste(s, names(count_result[i]), sep = "_"), 
             rbind(get(paste(s, names(count_result[i]), sep = "_")), text_count))
      rm(text_count)
    }
    rm(text, count_result)
    gc(verbose = TRUE)
    # Autosaving...
    if (autosave == 1) {
      for (kcode in keycode) {
        save_final(get(paste(s, kcode, sep = "_")), s, paste(s, "_", kcode, ".csv", sep = ""))
      }
    }
  }
  if (autosave == 1) {
    rm(list = paste(s, keycode, sep = "_"))
  }
}

#__Save files ####
var_list = ls()[which(str_count(ls(),paste(keycode, collapse = "|"))>0)]
keycode = str_split(var_list, "_") %>% lapply(`[`, 2) %>% unlist() %>% unique()
site = str_split(var_list, "_") %>% lapply(`[`, 1) %>% unlist() %>% unique()

for (s in site) {
  for (kcode in keycode) {
    save_final(get(paste(s, kcode, sep = "_")), s, paste(s, "_", kcode, ".csv", sep = ""))
  }
}


#__Call count results ####

keywords = c("tham nhũng", "hối lộ", "đút lót", "lợi dụng chức vụ", "lợi dụng quyền hạn", "biển thủ công quỹ") %>% tolower()
keycode = c("thamnhung", "hoilo","dutlot", "loidungchucvu", "loidungquyenhan", "bienthucongquy")
site = c("vnexpress", "dantri", "thanhnien")
for (s in site) {
  for (kcode in keycode) {
    count_result = call_count(s, kcode)
    assign(paste(s, kcode, sep = "_"), count_result)
    rm(count_result)
  }
}

#__Merge count results ####
# After loading up count results:
var_list = ls()[which(str_count(ls(),paste(keycode, collapse = "|"))>0)]
keycode = str_split(var_list, "_") %>% lapply(`[`, 2) %>% unlist() %>% unique()
site = str_split(var_list, "_") %>% lapply(`[`, 1) %>% unlist() %>% unique()
### NOTE: Make sure they have the same links in the same order
for (s in site) {
  sum = dplyr::select(get(paste(s, keycode[1], sep = "_")), c(link, date, category))
  sum$title_count = rep(0, nrow(sum))
  sum$para_count = rep(0, nrow(sum))
  sum$content_count = rep(0, nrow(sum))
  
  
  for (kcode in keycode) {
    sum$title_count = sum$title_count + get(paste(s, kcode, sep = "_"))$title_count
    sum$content_count = sum$content_count + get(paste(s, kcode, sep = "_"))$content_count
  }
  sum$total_count = sum$title_count + sum$content_count
  sum$contain = as.double(sum$total_count > 0)
  sum = time_round(sum, "date")
  assign(paste(s, "count", sep = "_"), sum)
  rm(sum)
}
### From all sites
allsite_count = c()
for (s in site) {
  allsite_count = rbind(allsite_count, get(paste(s, "_count", sep = "")))
}

for (kcode in keycode) {
  temp = c()
  for (s in site) {
    temp = rbind(temp, get(paste(s, kcode, sep = "_")))
  }
  assign(paste("allsite", kcode, sep = "_"), temp)
  rm(temp)
  gc()
}

#__Visualizing ####

count.column = c("title_count", "para_count")
count_col = c("total_count")
count_col = c("title_count")

### Total site

unit = "month"
dantri = plot_total(dantri_count, count_col, unit) + ggtitle("dantri")
vnexpress = plot_total(vnexpress_count, count_col, unit) + ggtitle("vnexpress")
thanhnien = plot_total(thanhnien_count, count_col, unit) + ggtitle("thanhnien") 
multiplot(dantri, vnexpress, thanhnien, cols = 2)

allsite = plot_total(allsite_count, count_col, unit) + ggtitle("ALL") 
allsite

### By category
unit = "month"
cat = "category"
dantri = plot_by_cat(dantri_count, count_col, unit, cat) + ggtitle("dantri")
vnexpress = plot_by_cat(vnexpress_count, count_col, unit, cat) + ggtitle("vnexpress")
thanhnien = plot_by_cat(thanhnien_count, count_col, unit, cat) + ggtitle("thanhnien")
dantri
vnexpress
thanhnien

### By month
unit = "month2"
cat = "year2"
dantri_count$month2 = month(dantri_count$date) %>% as.integer 
vnexpress_count$month2 = month(vnexpress_count$date) %>% as.integer
thanhnien_count$month2 = month(thanhnien_count$date) %>% as.integer
dantri_count$year2 = year(dantri_count$date) %>% as.integer 
vnexpress_count$year2 = year(vnexpress_count$date) %>% as.integer
thanhnien_count$year2 = year(thanhnien_count$date) %>% as.integer

### __All years
dantri = plot_total(dantri_count, count_col, unit) + ggtitle("dantri") + scale_x_discrete(limits=c(1:12))
vnexpress = plot_total(vnexpress_count, count_col, unit) + ggtitle("vnexpress") + scale_x_discrete(limits=c(1:12))
thanhnien = plot_total(thanhnien_count, count_col, unit) + ggtitle("thanhnien") + scale_x_discrete(limits=c(1:12))
multiplot(dantri, vnexpress, thanhnien, cols = 2)
### __By year
dantri = plot_by_cat(dantri_count, count_col, unit, cat) + ggtitle("dantri") + scale_x_discrete(limits=c(1:12))
vnexpress = plot_by_cat(vnexpress_count, count_col, unit, cat) + ggtitle("vnexpress") + scale_x_discrete(limits=c(1:12))
thanhnien = plot_by_cat(thanhnien_count, count_col, unit, cat) + ggtitle("thanhnien") + scale_x_discrete(limits=c(1:12))
dantri
vnexpress
thanhnien

### Back up
while (FALSE) {
  count_group = group_by(text,month,category) %>% summarise(contain = sum(contain))
  count_group = count_group[order(count_group$month),]
  plot_by_cat = ggplot(count_group,aes(x=month,y=contain)) +
    geom_bar() + facet_wrap(~category) + theme_light()
  plot_total = ggplot(count_group,aes(x=month,y=contain)) +
    geom_bar()+ theme_light()
  plot_total
}


#__Frequency analysis ####

### Regions

var_list = ls()[which(str_count(ls(),paste(keycode, collapse = "|"))>0)]
keycode = str_split(var_list, "_") %>% lapply(`[`, 2) %>% unlist() %>% unique()

site = c("vnexpress", "dantri", "thanhnien")
contentcount = 3
### 1. Filter articles in topic range
##### Make sure to have loaded merged count results
##### Remove count results of individual pages
for (s in site) {
  rm(list = ls(pattern = s))
}
##### Call and filter articles 
articlelink = allsite_count$link[allsite_count$title_count>0]
article = c()
for (s in site) {
  for (year in c(start_year:end_year)) {
    cat("Reading ", year, " of ", s)
    text = call_data(site = s, index = paste("_", year, "_processed", sep = ""))
    text = text[!duplicated(text$link),]
    text = text[!is.na(text$date),]
    text = text[text$link %in% articlelink,]
    article = rbind(article, text)
    rm(text)
  }
  gc()
}

### 2. Summarise by cities, provinces and regions
##### Clean first
while (FALSE) {
  article$title = article$title %>% stri_trans_tolower() %>% removePunctuation() %>% stripWhitespace() 
  article$content = article$content %>% stri_trans_tolower() %>% removePunctuation() %>% stripWhitespace() 
}
article$title = str_replace_all(string = article$title, pattern = "tphcm", replacement = "tp hồ chí minh")
article$title = str_replace_all(string = article$title, pattern = "tp hcm", replacement = "tp hồ chí minh")
article$title = str_replace_all(string = article$title, pattern = "thành phố hồ chí minh", replacement = "tp hồ chí minh")
article$title = str_replace_all(string = article$title, pattern = "thành phố hcm", replacement = "tp hồ chí minh")
article$title = str_replace_all(string = article$title, pattern = "thành phố mang tên bác", replacement = "tp hồ chí minh")
article$title = str_replace_all(string = article$title, pattern = "sài gòn", replacement = "tp hồ chí minh")

article$content = str_replace_all(string = article$content, pattern = "tphcm", replacement = "tp hồ chí minh")
article$content = str_replace_all(string = article$content, pattern = "tp hcm", replacement = "tp hồ chí minh")
article$content = str_replace_all(string = article$content, pattern = "thành phố hồ chí minh", replacement = "tp hồ chí minh")
article$content = str_replace_all(string = article$content, pattern = "thành phố hcm", replacement = "tp hồ chí minh")
article$content = str_replace_all(string = article$content, pattern = "thành phố mang tên bác", replacement = "tp hồ chí minh")
article$content = str_replace_all(string = article$content, pattern = "sài gòn", replacement = "tp hồ chí minh")

##### Count
####### Make sure to have run appendix
tinhthanh_count = c()
### 1. Call count results ( topic-identifying keywords )
for (i in c(1:length(tinhthanh$tinhthanh))) {
  count_result = basic_count(article, tinhthanh$tinhthanh[i], tinhthanh$tinhthanh[i], contentcount = 2)
  title = which(count_result[[1]][["title_count"]]>0)
  content = which(count_result[[1]][["content_count"]]>0)
  title_table = data.frame(province = rep(tinhthanh$tinhthanh[i], length(title)), 
                           position = rep("title", length(title)),
                           date = article$date[title],
                           link = article$link[title])
  content_table = data.frame(province = rep(tinhthanh$tinhthanh[i], length(content)), 
                             position = rep("content", length(content)),
                             date = article$date[content],
                             link = article$link[content])
  count_table = rbind(title_table, content_table)
  tinhthanh_count = rbind(tinhthanh_count, count_table)
  rm(count_result, title, content, title_table, content_table, count_table)
}
tinhthanh_count$page = NA
for (s in site) {
  temp = str_detect(tinhthanh_count$link, s)
  tinhthanh_count$page[temp == 1] = s
}
### 3. Merge results of keywords and regions
tinhthanh_count = left_join(tinhthanh_count, tinhthanh[,-3], by = c("province" = "tinhthanh"))
#sum = group_by(sum, khuvuc, date) %>% summarise(n = n())
tinhthanh_count$month = month(tinhthanh_count$date)
tinhthanh_count$year = year(tinhthanh_count)
ggplot(tinhthanh_count, aes(x = "", fill = khuvuc)) + geom_bar() +  coord_polar(theta = "y") + theme_void()
ggplot(tinhthanh_count, aes(x = month, fill = khuvuc)) + geom_bar() + facet_wrap(~year) + scale_x_discrete(limits = c(1:12))
ggplot(tinhthanh_count, aes(x = year, fill = khuvuc)) + geom_bar() + scale_x_discrete(limits = c(2006:2016))

### Most frequently mentioned words
##### Document term matrix (bigram)
BigramTokenizer <- function(x) unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
docs = VCorpus(VectorSource(article$content))
dtm = DocumentTermMatrix(docs, control = list(tokenize = BigramTokenizer))
freq <- colSums(as.matrix(dtm))

article$year = year(article$date)
article$month = month(article$date)

i = 0
docs = tm_map(docs, function(x) {
  i <<- i +1
  meta(x, "Year") <- article$year[i]
  meta(x, "Month") <- article$month[i]
  x
})

for (year in c(start_year:end_year)) {
  message("Converting ", year)
  index = meta(docs, "Year") == year
  assign(paste("docs", year, sep =  ""), docs[index])
  dtm = DocumentTermMatrix(get(paste("docs", year, sep =  "")), control = list(tokenize = BigramTokenizer))
  freq <- colSums(as.matrix(dtm))
  assign(paste("dtm", year, sep = ""), dtm)
  assign(paste("freq", year, sep = ""), freq)
}


##### Most frequently mentioned words (bigram) 
##### Total
dtms = removeSparseTerms(dtm, 0.98)
freq <- colSums(as.matrix(dtms))
ord <- order(freq)
freq[tail(ord)]
findFreqTerms(dtm, lowfreq=1000)

while (FALSE) {
  wf <- data.frame(word=names(freq), freq=freq)   
  head(wf) 
  p <- ggplot(subset(wf, freq>2000), aes(word, freq))
  p <- p + geom_bar(stat="identity")   
  p <- p + theme(axis.text.x=element_text(angle=45, hjust=1))   
  p 
}


#### By year
for (year in c(start_year:end_year)) {
  order = order(get(paste("freq", year, sep = "")))
  message("most frequently mentioned of ", year, " :")
  print(get(paste("freq", year, sep = ""))[tail(order, n = 5)])
}

##### Highly Correlated words
#####   Total
findAssocs(dtms, c("hối lộ", "biển thủ"), corlimit=0.7)


dtms = removeSparseTerms(get(paste("dtm", year, sep = "")), 0.9)

##### tf-idf
m = as.matrix(dtms)
tf = m
idf = log(nrow(m)/colSums(m))
tfidf <- m
for(word in names(idf)){
  tfidf[,word] <- tf[,word] * idf[word]
}
