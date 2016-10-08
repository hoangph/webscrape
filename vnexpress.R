################################################-
#####  Web scraping 
#####  written for DEPOCEN 
#####  by Hoang Phan. September 2016 
################################################-
library(tidyverse)
library(Rfacebook)
library(lubridate)
library(rvest)
library(stringr)


dir = "D:/Webscrape/webscrape"
#dir = getwd()
setwd(dir)
##########################-
#####  Vnexpress    ######
##########################-

##############################################-
####### __General Purpose Functions ##########
##############################################-

# Try read HTML function
# Try multiple times if errors happen
tryRead = function (url, times, seconds) {
  ok <- FALSE
  counter <- 0
  while (ok == FALSE & counter <= times) { 
    counter <- counter + 1
    html <- tryCatch({
      read_html(url)
    },
    error = function(e) {
      Sys.sleep(seconds)
      e
    }
    )
    if ("error" %in% class(html)) {
      cat(".")
    } else {
      ok <- TRUE
      message(" Done.")
    }
  }
  if ("error" %in% class(html)) {
    return(list(1, 1))
  } else {
    return(list(0, html))
  }
}

# Get articles' link and title in one page
get_article = function(url, article_selector) {
  errorPage = 1
  page_html = tryRead(url, 20, 5)
  if (page_html[[1]] == 1) {
    errorPage = 1
    return(list(errorPage, 1, 1))
  } else {
    errorPage = 0
    article = page_html[[2]] %>% html_nodes(article_selector)
    article_link = article %>% html_attr("href")
    article_title = article %>% html_text()
    return(list(errorPage, article_link, article_title))
    }
}

# Translate date format from Vietnamese -> R (English)
clean_date = function (date) {
  date_s = str_sub(date, str_locate(date,"/")[1]-2,str_locate(date,"/")[1]-2+9)
  date_d = as.Date(date_s,"%d/%m/%Y")
  return(date_d)
}

# Read article's content
read_page = function(url, content_selector, date_selector) {
  errorPage = 1
  cat("Scraping: ", url,"\n")
  # try catch to avoid timeout error
  page_html = tryRead(url, 10, 5)
  if (page_html[[1]] == 1) {
    errorPage = 1
    return(list(errorPage, 1, 1))
  } else {
    ar_date = page_html[[2]] %>% html_nodes(date_selector) %>% html_text()%>%
      paste(collapse = "") %>% clean_date()
    if (is.na(ar_date)) {  # If there is no date in the article -> wrong format -> skip
      message("Skipped.")
      errorPage = 1
      return(list(errorPage, 1, 1))
    } else {
      message("Date: ", ar_date)
      paragraph = page_html[[2]] %>% html_nodes(content_selector) %>% 
        html_text() %>% paste(collapse = " ")
      paragraph = data.frame(ar_date,paragraph)
      errorPage = 0
      return(list(errorPage, paragraph$ar_date, paragraph$paragraph))
    }
  }
}

save_list_csv = function (list, save_dir, code, col_names, suffix) {
  setwd(save_dir)
  max_length = length(list[[1]])
  for (i in c(2:length(list))) {
    if (length(list[[i]]) > max_length) {max_length = length(list[[i]])}
  }
  file = data.frame(index = rep(NA,max_length))
  for (i in c(1:length(list)))  {
    col = c(list[[col_names[i]]], rep(NA, max_length - length(list[[i]])))
    file = cbind(file, col)
  }
  file = file[,-1]
  colnames(file) <- col_names
  write_excel_csv(file,paste(code,as.character(today()),suffix,".csv", sep = "_"))
}

list_fill = function(list, vector, index) {
  vacancy = min(which(is.na(list[[index]])))
  if (length(vector) != 0){
    list[[index]][vacancy:(vacancy+length(vector)-1)] <- vector
  }
  return(list)
}


##############################-
####### __Execution ##########
##############################-
while (0) {
  a = list(rep(NA,10000), rep(NA, 10000))
  a = list(NA,NA)
  time = now()
  l = length(a[[1]])
  for (i in c(1:100)) {
    print(i)
    if (i>l) {
      b = a
      a = list(rep(NA,10), rep(NA, 10))
      a[[1]][i] = i
      a[[2]][i] = i+1
    } else {
      a[[1]][i] = i
      a[[2]][i] = i+1
    }
  }
  print(now()-time)
}


#___ Merge files cu  ####
while (FALSE) {
  text = c()
  for (i in c(1:length(list.files()))) {
    message(i,"/",length(list.files()))
    table = read_csv(list.files()[i])
    code = str_split(list.files()[i],"_")[[1]][1]
    table$cm = rep(code, nrow(table))
    text = rbind(text, table)
  }
  colnames(text) = c("link", "title", "date", "content", "category")
  # xoa nhung link bi lap lai
  text_uniq = text[!duplicated(text$link),]
  text_uniq$date = as_date(as.integer(text_uniq$date))
  rm(text, table)
  # Xem date
  message("min date: ", min(text_uniq$date[!is.na(text_uniq$date)]))
  message("max date: ", max(text_uniq$date[!is.na(text_uniq$date)]))
  # Luu lai thanh 1 file
  # write_excel_csv(text_uniq, paste(dir,"/vnexpress/finalData/vnexpress.csv",sep=""))
  # Tach file theo chuyen muc
  cm = unique(text_uniq$category)
  for (i in c(1:length(cm))) {
    data = filter(text_uniq, category == cm[i])
    assign(cm[i], data)
  }
  rm(data)
}

#___ Goi file sau khi da merge  ####
while (FALSE) {
  setwd(paste(dir,"/vnexpress/finalData",sep=""))
  text_uniq = read_csv("vnexpress.csv")
  colnames(text_uniq) = c("link", "title", "date", "content", "category")
  text_uniq$date = as_date(as.integer(text_uniq$date))
  cm = unique(text_uniq$category)
  for (i in c(1:length(cm))) {
    data = filter(text_uniq, category == cm[i])
    assign(cm[i], data)
  }
  rm(data)
  rm(text_uniq)
}


# ___Scrape cac chuyen muc####

tencm = c("phapluat","thoisu","kinhdoanh","giaoduc","congdong")
linkcm = c("http://vnexpress.net/tin-tuc/phap-luat/page/",
           "http://vnexpress.net/tin-tuc/thoi-su/page/",
           "http://kinhdoanh.vnexpress.net/page/",
           "http://vnexpress.net/tin-tuc/giao-duc/page/",
           "http://vnexpress.net/tin-tuc/cong-dong/page/")
cm_list = data.frame(tencm,linkcm)
rm(tencm,linkcm)
for (j in c(1:nrow(cm_list))) {
  #Parameters
  code = as.character(cm_list$tencm[j])
  source = cm_list$linkcm[j]
  source_suffix = ".html"
  start_date = clean_date("01/01/2006")
  end_date = today()
  content_selector = ".short_intro , .Normal"
  date_selector = ".block_timer"
  article_selector = "#news_home .txt_link"
  # Save directory
  save_dir = paste(dir,"/vnexpress",sep="")
  
  # Check xem chuyen muc da scrape chua
  # NOTE: can file data cu co ten giong vs ten chuyen muc 
  scraped = 0
  if (sum(str_detect(ls(), pattern = as.character(code)) > 0)) { scraped = 1 }
  
  #_____Vong lap de lay link####
  k = 1
  ok = TRUE
  i = 1
  while (ok) {
    # Scrape loop
    temp = rep(NA, 1000)
    final = list(link = temp, title = temp, date = temp, content = temp)
    col_names = c("link", "title", "date", "content")
    rm(temp)
    
    # Lay 1.000 link trong chuyen muc mot luc
    skipped = c()
    while (sum(is.na(final[["link"]])) > 0) {
      cat("Looking into page", i," section: ", as.character(code),"\n")
      link_list_result = source %>% paste(i,source_suffix,sep = "") %>% 
        get_article(article_selector)
      if (link_list_result[1]==1) {
        message("Skipped page ", i)
        skipped = c(skipped, i)
        i = i+1
      } else {
        link = link_list_result[[2]] 
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
        # Check xem link da scrape tu truoc chua ####
        if (scraped==1) {
          index_get = which(is.na(match(link, get(code)$link)))
          link = link[index_get]
          title = title[index_get]
          rm(index_get)
        }
        
        # Dien vao list link
        final = list_fill(list = final, vector = link, index = "link") %>% 
          list_fill(vector = title, index = "title")
        i = i+1
      }
    }
    
    #_____Doc cac bai trong list link vua lay ####
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
        cat("Saving...\n")
        assign(paste("final",k,sep=""), final)
        save_list_csv(final,save_dir,code,col_names,suffix = paste("(",k,")",sep=""))
        save_count = 1
      } else {save_count = save_count + 1}
    }
    # Check xem bai cuoi cung da vuot qua gioi han thoi gian chua
    last_date = as_date(as.integer(final[["date"]][max(which(!is.na(final[["date"]])&final[["date"]]!="error"))]))
    if (last_date < start_date) {
      ok = FALSE
      message("Done scraping with specified time range. Saving...")
      save_list_csv(final,save_dir,code,col_names,suffix = paste("(",k,")",sep=""))
    } else {
      assign(paste("final",k,sep=""), final)
      cat("Saving...\n")
      save_list_csv(final,save_dir,code,col_names,suffix = paste("(",k,")",sep=""))
      k = k + 1
    }
  }
}


