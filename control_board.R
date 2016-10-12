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

#---------------------------------------#
####   __General Purpose Functions   ####
#---------------------------------------#

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

# Save the data list into dataframe
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


# Fill empty list with data
list_fill = function(list, vector, index) {
  vacancy = min(which(is.na(list[[index]])))
  if (length(vector) != 0){
    list[[index]][vacancy:(vacancy+length(vector)-1)] <- vector
  }
  return(list)
}

# Merge files
merge = function(directory) {
  setwd(directory)
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
  return(text_uniq)
}

# Call database
call_data = function(site, index) {
  setwd(paste(dir,"/",site,"/finalData",sep=""))
  text_uniq = read_csv(paste(site,index,".csv",sep=""))
  colnames(text_uniq) = c("link", "title", "date", "content", "category")
  text_uniq$date = as_date(as.integer(text_uniq$date))
  return(text_uniq)
}

#---------------------------------------#
####        __ Execution             ####
#---------------------------------------#

#### Targets ####
site = "thanhnien"
start_date = clean_date("01/01/2006")
end_date = today()

#### Other inputs ####

# Do we have to compare with database? (0: no, 1: yes)
compare = 1
if (compare == 1){
  database = call_data(site,"(3-2)")
  cm = unique(database$category)
  for (i in c(1:length(cm))) {
    data = filter(database, category == cm[i])
    # Giu lai link de so sanh (bo di neu muon lay ca noi dung)
    data = data[,1]
    assign(cm[i], data)
  }
  rm(data, cm)
}

#### Run ####
setwd(dir)
source("parameter.R")

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
  if (sum(str_detect(ls(), pattern = as.character(code)) > 0)) { scraped = 1 }
  
  #_____Vong lap de lay link####
  # Starting point
  setwd(save_dir)
  file_list = list.files()[which(str_sub(list.files(), 1, str_locate(list.files(),"_")-1)==code)]
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
    temp = rep(NA, 10)
    final = list(link = temp, title = temp, date = temp, content = temp)
    col_names = c("link", "title", "date", "content")
    rm(temp)
    # Dien vao
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
        #cat("Saving...\n")
        #assign(paste("final",k,sep=""), final)
        #save_list_csv(final,save_dir,code,col_names,suffix = paste("file",k,"page",i-1,sep=""))
        save_count = save_count + 1
      } else {save_count = save_count + 1}
    }
    # Check xem bai cuoi cung da vuot qua gioi han thoi gian chua
    last_date = as_date(as.integer(final[["date"]][max(which(!is.na(final[["date"]])&final[["date"]]!="error"))]))
    if (last_date < start_date) {
      ok = FALSE
      message("Done scraping with specified time range. Saving...")
      save_list_csv(final,save_dir,code,col_names,suffix = paste("file",k,"page",i-1,sep=""))
    } else {
      #assign(paste("final",k,sep=""), final)
      cat("Saving...\n")
      save_list_csv(final,save_dir,code,col_names,suffix = paste("file",k,"page",i-1,sep=""))
      k = k + 1
      gc()
    }
  }
}


