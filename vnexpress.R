################################################
#####  Web scraping 
#####  written for DEPOCEN 
#####  by Hoang Phan. September 2016 
################################################
library(readr)
library(tidyverse)
library(Rfacebook)
library(rvest)


##########################
#####  Vnexpress    ######
##########################

############################
####### __Functions ##########
############################

# Get articles' link and title in one page
get_article = function(url, article_selector) {
  html = read_html(url)
  article = html %>% html_nodes(article_selector) 
  article_link = article %>% html_attr("href")
  article_title = article %>% html_text()
  return(data.frame(article_link, article_title))
}

# Translate date format from Vietnamese -> R (English)
clean_date = function (date) {
  date_s = str_sub(date, str_locate(date,"/")[1]-2,str_locate(date,"/")[1]-2+9)
  date_d = as.Date(date_s,"%d/%m/%Y")
  return(date_d)
}


read_page = function(start, end, link, selector, date_selector) {
  result = c()
  error = 0
  for (i in c(1:length(link))) {
    url = link[i]
    html = read_html(url)
    date = html %>% html_nodes(date_selector) %>% html_text() %>% clean_date()
    if (date >= start & date <= end) {
      paragraph = html %>% html_nodes(selector) %>% html_text() %>% paste(collapse = " ")
      result = c(result, paste(date, paragraph, sep = "$:"))
    } else { 
      error = 1
      break
    }
  }
  return(list(result, error))
}

scrape_news = function (source, start, end, selector, date_selector, link_selector) {
  stop = 0
  i = 1
  content = c()
  while (1) {
    link = source %>% paste(i,".html", sep = "") %>% get_link(link_selector)
    output = read_page(start, end, link, selector, date_selector)
    content = c(content, output[[1]])
    if (output[2] == 1) {break} else {i=i+1}
  }
  return(content)
}