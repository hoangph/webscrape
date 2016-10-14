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

site = "laodong"

start_date = clean_date("01/01/2006")
end_date = today()

#### __Other inputs ####

# Do we have to compare with database? (0: no, 1: yes)
compare = 0
if (compare == 1){
  setwd(paste(dir,"/",site,"/finalData"))
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

#### __Run ####

cm_list = link_par(site)
for (j in c(7:6)) {
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
