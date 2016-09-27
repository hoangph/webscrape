################################################
#####  Web scraping 
#####  written for DEPOCEN 
#####  by Hoang Phan. September 2016 
################################################
library(tidyverse)
library(Rfacebook)
library(lubridate)
library(rvest)
library(stringr)

dir = "C:/Users/Windows/Dropbox/DEPOCEN/Scrape/webscrape"
#dir = getwd()
setwd(dir)
##########################
#####  Vnexpress    ######
##########################

############################
####### __Functions ##########
############################

# Get articles' link and title in one page
get_article = function(url, article_selector) {
  missing = 0
  ok <- FALSE
  counter <- 0
  while (ok == FALSE & counter <= 20) { #repeat 20 times
    counter <- counter + 1
    html <- tryCatch({                  
      read_html(url)
    },
    error = function(e) {
      Sys.sleep(5)
      e
    }
    )
    if ("error" %in% class(html)) {
      message(".")
    } else {
      ok <- TRUE
      cat(" Done.")
    }
  }
  if ("error" %in% class(html)) {
    cat("Skipping page...")
    missing = 1
    next
    return(list(missing,1))
  } else {
    article = html %>% html_nodes(article_selector) 
    article_link = article %>% html_attr("href")
    article_title = article %>% html_text()
    return(list(missing, data.frame(article_link, article_title)))
  }
}

# Translate date format from Vietnamese -> R (English)
clean_date = function (date) {
  date_s = str_sub(date, str_locate(date,"/")[1]-2,str_locate(date,"/")[1]-2+9)
  date_d = as.Date(date_s,"%d/%m/%Y")
  return(date_d)
}

# Read article's content
read_page = function(start_date, end_date, link, title, content_selector, date_selector) {
  result = c()
  stop = 0
  for (i in c(1:length(link))) {
    url = as.character(link[i])
    tit = as.character(title[i])
    # try catch to avoid timeout error
    ok <- FALSE
    counter <- 0
    while (ok == FALSE & counter <= 20) { #repeat 20 times
      counter <- counter + 1
      html <- tryCatch({                  
        read_html(url)
      },
      error = function(e) {
        Sys.sleep(5)
        e
      }
      )
      if ("error" %in% class(html)) {
        message(".")
      } else {
        ok <- TRUE
        cat(" Done.")
      }
    }
    ar_date = html %>% html_nodes(date_selector) %>% html_text()%>%
      paste(collapse = "") %>% clean_date()
    message("Scraping date: ", ar_date, ", link: ", url)
    if (!is.na(ar_date)) {
      if (ar_date >= start_date & ar_date <= end_date) { #get articles in the time range
        paragraph = html %>% html_nodes(content_selector) %>% 
          html_text() %>% paste(collapse = " ")
      } else {
        stop = 1 #signal to stop outside loop
        break
      }
      result = rbind(result,data.frame(url,tit,ar_date,paragraph))
    }
  }
  return(list(result,stop))
}

save = function(file, save_dir, code) {
  setwd(save_dir)
  write_excel_csv(as.data.frame(file), 
                  paste(code,
                        as.character(today()),
                        as.character(start_date),
                        as.character(end_date),
                        ".csv", sep = "_"))
}

##############################
####### __Execution ##########
##############################

# ___Scrape cac chuyen muc khac####

tencm = c("thoisu","kinhdoanh","giaoduc","congdong")
linkcm = c("http://vnexpress.net/tin-tuc/thoi-su/page/",
           "http://kinhdoanh.vnexpress.net/page/",
           "http://vnexpress.net/tin-tuc/giao-duc/page/",
           "http://vnexpress.net/tin-tuc/cong-dong/page/")
cm_list = data.frame(tencm,linkcm)
rm(tencm,linkcm)
for (j in c(1:nrow(cm_list))) {
  #Parameters
  code = cm_list$tencm[j]
  source = cm_list$linkcm[j]
  source_suffix = ".html"
  start_date = clean_date("01/01/2015") 
  end_date = today()
  content_selector = ".short_intro , .Normal"
  date_selector = ".block_timer"
  article_selector = "#news_home .txt_link"
  # Save directory
  save_dir = paste(dir,"/vnexpress",sep="")
  # Scrape loop
  i = 1
  final = c()
  #final = read_csv(list.files()[1])
  while (1) {
    cat("Scraping page", i," section: ", as.character(code))
    link_list_result = source %>% paste(i,source_suffix, sep = "") %>% 
      get_article(article_selector)
    if (link_list_result[1]!=1) {
      link_table=link_list_result[[2]]
      link = link_table$article_link
      title = link_table$article_title 
      content = read_page(start_date, end_date, link, title, 
                          content_selector, date_selector)
      final = rbind(final, content[[1]])
      if (content[[2]] == 1) {
        break
      } else {i=i+1}
      save(final, save_dir, as.character(code))
    }
  }
  
  #Save
  write_excel_csv(final,paste(code,"_final_",today(),".csv",sep=""))
}





