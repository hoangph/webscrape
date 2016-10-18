################################################-
#####  Web scraping 
#####  written for DEPOCEN 
#####  by Hoang Phan. September 2016 
################################################-

#-------------------------#
####   CONTROL BOARD   ####
#-------------------------#

library(tidyverse)
library(Rfacebook)
library(lubridate)
library(rvest)
library(stringr)
dir = "D:/Webscrape/webscrape"

setwd(dir)
source("parameter.R")
source("functions.R")

#---------------------------------------#
####       Target identifier         ####
#---------------------------------------#


#### __Targets ####

site = "vietnamnet"

start_date = clean_date("01/01/2006")
end_date = today()

#### __Other inputs ####

# Do we have to compare with database? (0: no, 1: yes)
compare = 1
if (compare == 1){
  link_list = call_data(site, "_link")
}

#---------------------------------------#
####            Scrape               ####
#---------------------------------------#

cm_list = link_par(site)
for (j in c(1:nrow(cm_list))) {
  code = as.character(cm_list$tencm[j])
  source = cm_list$linkcm[j]
  par = node_par(site, code)
  link_prefix = par[["link_prefix"]]
  source_suffix = par[["source_suffix"]]
  content_selector = par[["content_selector"]]
  date_selector = par[["date_selector"]]
  article_selector = par[["article_selector"]]
  save_dir = par[["save_dir"]]
  #rm(par)
  # Check xem chuyen muc da scrape chua
  # NOTE: can file data cu co ten giong vs ten chuyen muc 
  scraped = 0
  if ("link_list" %in% ls()) {scraped = 1}
  
  #___Vong lap de lay link####
  # Starting point
  setwd(save_dir)
  file_list = list.files()[which(str_sub(list.files(), 1, str_locate(list.files(),"_")[,1]-1)==code)]
  k_index = str_locate(file_list,pattern = "file")[,1]
  p_index = str_locate(file_list,pattern = "page")[,1]
  e_index = str_locate(file_list,pattern = "_.csv")[,1]
  k_index = str_sub(file_list, k_index+4, p_index-1)
  p_index = str_sub(file_list, p_index+4, e_index-1)
  
  k = max(as.integer(k_index[!is.na(k_index)])) + 1
  i = max(as.integer(p_index[!is.na(p_index)])) + 1
  if (k==-Inf) {k = 1}
  if (i==-Inf) {i = 1}
  rm(k_index, p_index, e_index)
  # Loop
  ok = TRUE
  while (ok) {
    # Lay 200 link trong chuyen muc mot luc
    temp = rep(NA, 200)
    final = list(link = temp, title = temp, date = temp, content = temp)
    col_names = c("link", "title", "date", "content")
    rm(temp)
    # Dien vao
    skipped = c()
    last_count = 0
    while (sum(is.na(final[["link"]])) > 0) {
      cat("Looking into page", i," section: ", as.character(code),"\n")
      link_list_result = source %>% paste(i,source_suffix,sep = "") %>% 
        get_article(article_selector)
      if (length(link_list_result[[3]])==0) { last_count = last_count + 1 }
      if (last_count == 20) { 
        message("finising")
        ok = FALSE # chuyen qua chuyen muc khac
        lastrecord = max(which(!is.na(final[["link"]])))
        final[["link"]] = final[["link"]][1:lastrecord]
        final[["title"]] = final[["title"]][1:lastrecord]
        final[["date"]] = final[["date"]][1:lastrecord]
        final[["content"]] = final[["content"]][1:lastrecord]
      }
      if (link_list_result[1]==1) {
        message("Skipped page ", i)
        skipped = c(skipped, i)
        i = i+1
      } else {
        link = link_list_result[[2]]
        link[str_sub(link,1,4)!="http"] = paste(link_prefix,link[str_sub(link,1,4)!="http"],sep="")
        title = link_list_result[[3]]
        # Sua loi link bi lap lai
        index_rm = which(str_sub(as.character(title),1,3) %in% "\n  " )
        if (length(index_rm) > 0) {
          link = link[-index_rm]
          title = title[-index_rm]
        }
        if (length(link) != length(title)) {
          message("Error link and title mismatched, skipped page") 
          skipped = c(skipped, i)
          i = i + 1
          next
        }
        # _____Check xem link da scrape tu truoc chua ####
        if (scraped==1) {
          index_get = which(is.na(match(link, link_list$link)))
          link = link[index_get]
          title = title[index_get]
          rm(index_get)
        }
        
        # _____Dien link vao list  ####
        final = list_fill(list = final, vector = link, index = "link") %>% 
          list_fill(vector = title, index = "title")
        i = i+1
      }
    }
    
    #___Doc cac bai trong list link vua lay ####
    article_no = length(final[["link"]])
    save_count = 1
    for (a in c(1:article_no)) {
      url = final[["link"]][a]
      page = read_page(url, content_selector, date_selector)
      if (page[[1]] == 1) {
        final[["date"]][a]    <- "error"
        final[["content"]][a] <- "error"
      } else {
        final[["date"]][a]    <- as_date(page[[2]])
        final[["content"]][a] <- as.character(page[[3]])
      }
      # 50 bai thi save 1 lan
      if (save_count == ceiling(article_no/2)) {
        #cat("Saving...\n")
        #assign(paste("final",k,sep=""), final)
        #save_list_csv(final,save_dir,code,col_names,suffix = paste("file",k,"page",i-1,sep=""))
        save_count = save_count + 1
      } else {save_count = save_count + 1}
    }
    # Check xem bai cuoi cung da vuot qua gioi han thoi gian chua
    last_date = as_date(as.integer(final[["date"]][max(which(!is.na(final[["date"]])&final[["date"]]!="error"))]))
    if (last_date < start_date) { ok = FALSE } 
    if (ok == FALSE) {
      message("Done scraping with specified time range. Saving...")
      save_list_csv(final,save_dir,code,col_names,suffix = paste("file",k,"page",i-1,sep=""))
    } else {
      cat("Saving...\n")
      save_list_csv(final,save_dir,code,col_names,suffix = paste("file",k,"page",i-1,sep=""))
      k = k + 1
      gc()
    }
  }
  # merge cac file da scrape cua cac chuyen muc truoc va lay link de so sanh
  merge_result = merge_files(save_dir, code)
  setwd(paste(dir,"/",site,"/finalData",sep=""))
  if (length(list.files())!=0) {
    link_list = call_data(site,"_link")
    colnames(link_list) = "link"
    link_list = rbind(link_list, merge_result[,1]) %>% unique()
  } else { link_list = merge_result[,1] %>% unique() }
  rm(merge_result)
  write_excel_csv(link_list, paste(site,"_link.csv",sep=""))
  gc()
}


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
sum = left_join(tinhthanh_count, tinhthanh[,-3], by = c("province" = "tinhthanh"))
#sum = group_by(sum, khuvuc, date) %>% summarise(n = n())
sum$month = month(sum$date)
sum$year = year(sum$date)
ggplot(sum, aes(x = "", fill = khuvuc)) + geom_bar() +  coord_polar(theta = "y") + theme_void()
ggplot(sum, aes(x = month, fill = khuvuc)) + geom_bar() + facet_wrap(~year) + scale_x_discrete(limits = c(1:12))
ggplot(sum, aes(x = year, fill = khuvuc)) + geom_bar() + scale_x_discrete(limits = c(2006:2016))

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
