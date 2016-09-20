library(rvest)
library(XML)
library(stringr)
library(lubridate)
library(reshape)
library(dplyr)
library(Rfacebook)
library(readr)

# Hoang Huy Phan

##########################
###  1. Dantri.com   #####
##########################

#############################
##### 1.1. Functions ########
#############################

get_link = function(url, selector) {
  html = read_html(url)
  link = html %>% html_nodes(selector) %>% html_attr("href")
  return(link)
}
  
clean_date = function (date) {
  date_s = str_sub(date, str_locate(date,"/")[1]-2,str_locate(date,"/")[1]-2+9)
  date_d = as.Date(date_s,"%d/%m/%Y")
  return(date_d)
}

read_page = function(start, end, link, selector, date_selector) {
  result = c()
  error = 0
  for (i in c(1:length(link))) {
    url = paste("http://dantri.com.vn/",link[i],sep = "")
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
    link = source %>% paste(i,".htm", sep = "") %>% get_link(link_selector)
    output = read_page(start, end, link, selector, date_selector)
    content = c(content, output[[1]])
    if (output[2] == 1) {break} else {i=i+1}
  }
  return(content)
}



############################
#####  1.2. Thuc hien  #####
############################


source = "http://dantri.com.vn/the-thao/trang-"

start = clean_date("13/09/2016")
end = today()

timestart = now()

#scrape_news parameter: source,start,end,selector,date_selector,link_selector
content = scrape_news(source, start, end,"#divNewsContent p",".tt-capitalize","h2 a")

print(now()-timestart)

##############################
######  2. Cophieu68   #######
##############################

#############################
##### 2.1. Functions ########
#############################

read_feeds = function(link,table_xpath) {
  table = link %>% read_html(link) %>% html_nodes(xpath=table_xpath) %>% html_table %>% data.frame()
  header = table[1,]
  table = table[-1,]
  colnames(table) = header
  return(table)
}

############################
#####  2.2. Thuc hien  #####
############################

# Basic input
#"http://www.cophieu68.vn/events.php?currentPage=1&stockname=&event_type="
table_xpath = '//*[@id="events"]/table'
pre = "http://www.cophieu68.vn/events.php?currentPage"
stock = "&stockname="
event = "&event_type="

# Vong lap de tai tin chung khoan
feeds_table = c()
timestart = now()
for (i in c(1:387)) {
  link = paste(pre,i,stock,event, sep = "")
  table = read_feeds(link, table_xpath)
  feeds_table = rbind(feeds_table, table)
}
print(now()-timestart)

# Cleaning 
header = c("MaCK","Loai su kien","Ngay GDKHQ", "Ngay thuc hien", "Co tuc","Ghi chu")
colnames(feeds_table) = header
feeds_table = data.frame(feeds_table)
table$Loai.su.kien[table$Loai.su.kien=="Cổ tức bằng tiền"]    = 1
table$Loai.su.kien[table$Loai.su.kien=="Cổ phiếu thưởng"]     = 2
table$Loai.su.kien[table$Loai.su.kien=="Phát hành hiện hữu"]  = 3

# Output



##################################
###### 3. Danh sach VNR500 #######
##################################

#############################
##### 3.1. Functions ########
#############################

read_table = function(link,table_xpath) {
  html = link %>% read_html(link) 
  table = html %>% html_nodes(xpath=table_xpath) %>% html_table %>% data.frame() %>% t() %>% data.frame() 
  name = html %>% html_nodes(".more_info span") %>% html_text()
  intro = html %>% html_nodes(".dn-gioi-thieu p") %>% html_text() %>% paste(collapse=" ")
  intro = str_replace_all(intro,c("\n","\r"),c(" "," "))
  table = cbind(name,table[-1,],intro)
  return(table)
}

############################
#####  3.2. Thuc hien  #####
############################

table = c()
table_xpath = '//*[@id="home"]/div/div[1]/div[1]/table'
timestart = now()
for (i in c(1:20)) {
  link_don= as.character(listlink[i,1])
  bang = read_table(link_don,table_xpath)
  table = rbind(table, bang)
}
print(now()-timestart)

header = c("Ten.DN","Xep.hang.VNR500","Ma.so.thue","Ma.chung.khoan","Tru.so.chinh","Tel","Fax","Email","Website","Ten.nganh.cap.2","So.huu.von","Nam.thanh.lap","Gioi.thieu")
colnames(table) = header

# fix loi xuong dong
table$Gioi.thieu = str_replace_all(table$Gioi.thieu,"\r"," ")

#write_excel_csv(table,"danhsach.csv")


##################################
###### 4. Facebook mining  #######
##################################

#############################
##### 4.1. Functions ########
#############################


############################
#####  4.2. Thuc hien  #####
############################

fb_oauth = fbOAuth("1843029625934354","86fb3d4467f624610072a136e0fe8de7",extended_permissions = TRUE)
post = getPage("307919579218616", token = fb_oauth, since = '2016/01/01', until = '2016/09/14', n=100000)
post = data.frame(post)
setwd("C:/Users/Windows/Dropbox/DEPOCEN/Scrape")
write_excel_csv(post, "vietnamnetfbpost.csv")



##########################
###  5. vnexpress    #####
##########################

#############################
##### 5.1. Functions ########
#############################

get_link = function(url, selector) {
  html = read_html(url)
  link = html %>% html_nodes(selector) %>% html_attr("href")
  return(link)
}

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



############################
#####  5.2. Thuc hien  #####
############################


source = "http://kinhdoanh.vnexpress.net/page/"

start = clean_date("13/09/2016")
end = today()

timestart = now()

#scrape_news parameter: source,start,end,selector,date_selector,link_selector
content = scrape_news(source, start, end,"h1 , .short_intro, .Normal","#left_calculator .left","#news_home .txt_link")

print(now()-timestart)


##########################
######  6. cafef    ######
##########################

#############################
##### 6.1. Functions ########
#############################

get_link = function(url, link_selector) {
  html = read_html(url)
  link = html %>% html_nodes(link_selector) %>% html_attr("href")
  return(link)
}

clean_date = function (date) {
  date_s = str_sub(date, str_locate(date,"/")[1]-2,str_locate(date,"/")[1]-2+9)
  date_d = as.Date(date_s,"%d/%m/%Y")
  return(date_d)
}

read_page = function(start, end, link, selector, date_selector) {
  result = c()
  error = 0
  for (i in c(1:length(link))) {
    url = paste("http://cafef.vn",link[i], sep="")
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
    link = source %>% paste(i,".chn", sep = "") %>% get_link(link_selector)
    output = read_page(start, end, link, selector, date_selector)
    content = c(content, output[[1]])
    if (output[2] == 1) {break} else {i=i+1}
  }
  return(content)
}



############################
#####  6.2. Thuc hien  #####
############################


source = "http://cafef.vn/thi-truong-chung-khoan/trang-"
link_selector = ".list a"
date_selector = ".date_zoom .date"
selector = ".newsbody p"

start = clean_date("13/09/2016")
end = today()

timestart = now()

#scrape_news parameter: source,start,end,selector,date_selector,link_selector
content = scrape_news(source, start, end,"h1 , .short_intro, .Normal",".date_zoom .date",".list a")

print(now()-timestart)



#############################
######  7. f319.com    ######
#############################

#############################
##### 7.1. Functions ########
#############################

get_link = function(url, link_selector, reply_selector, link_date_selector) {
  html = read_html(url)
  link = html %>% html_nodes(link_selector) %>% html_attr("href")
  title = html %>% html_nodes(link_selector) %>% html_text()
  reply = html %>% html_nodes(reply_selector) %>% html_text() %>% as.integer()
  date = html %>% html_nodes(link_date_selector) %>% html_text() %>% clean_date()
  return(data.frame(link,date, title,reply))
}

clean_date = function (date) {
  date_s = str_sub(date, str_locate(date,"/")[1]-2,str_locate(date,"/")[1]-2+9)
  date_d = as.Date(date_s,"%d/%m/%Y")
  return(date_d)
}

read_page = function(start, end, link, selector, date_selector, page_selector, user_selector) {
  stop = 0
  result = c()
  for (i in c(1:length(link[,1]))) {
    thread = c()
    thread_title = link[i,3]
    thread_url = paste("http://f319.com/",link[i,1], sep="")
    html = read_html(thread_url)
    page_number = html %>% html_nodes(page_selector) %>% html_text()
    last_page = max(as.double(page_number)[!is.na(as.double(page_number))])
    if (last_page==-Inf) {
      last_page = 1
    }
    for (j in c(last_page:1)) {
      message("Scraping page ",j," thread: ", thread_url)
      page = paste(thread_url,"page-",j,sep = "") %>% read_html()
      date = page %>% html_nodes(date_selector) %>% html_text() %>% clean_date() %>% max()
      if (date >= start & date <= end) {
        paragraph = page %>% html_nodes(selector) %>% html_text() %>% 
          paste(collapse = " ")
        #str_split(pattern="#")     # tach text ra theo comment bang dau '#'
        split_location = str_locate_all(paragraph,"#")[[1]][,1]
        str_check = paragraph %>% str_sub(start = split_location+1,end = split_location+1) %>%
          as.double() %>% is.na() 
        split_location = split_location[str_check==0]
        paragraph = str_sub(paragraph,split_location,c(split_location[2:length(split_location)]-1,nchar(paragraph)))
        paragraph = paragraph[paragraph!=""]  #bo thanh phan thua
        user = page %>% html_nodes(user_selector) %>% html_text()
        nrows = length(paragraph)
        table = data.frame(page=rep(j,nrows), date=rep(date,nrows), user=user[1:nrows], content=paragraph)
        thread = data.frame(rbind(thread,table))  # lap bang cho 1 thread gom nhieu page
      } else {break}
    }
    if (length(thread)!=0) {
      thread = cbind(rep(thread_title,nrow(thread)),thread)
      result = rbind(result,thread)
    } else {
      stop = 1
      break
      }
  }
  return(list(result,stop))
}




scrape_forum = function (start, end, url, selector, link_selector, date_selector, page_selector, user_selector, reply_selector, link_date_selector, pinned) {
  p = 1
  message("Scraping big-page #", p)
  while (1) {
    link = url %>% paste(p, sep = "") %>% get_link(link_selector, reply_selector, link_date_selector)
    if (p==1) {
      link = link[-c(1:pinned),]
    }
    output = read_page(start, end, link, selector, date_selector, page_selector, user_selector)
    if (output[[2]] == 0) {
      p = p+1
    } else {
      write_excel_csv(output[[1]],"scrape_forum.csv")
      break
      }
    write_excel_csv(output[[1]],"scrape_forum.csv")
  }
  return(output[[1]])
}

############################
#####  7.2. Thuc hien  #####
############################

setwd("C:/Users/Administrator/Dropbox/Study/Algorithmic trading/f319_scrape/")

url = "http://f319.com/forums/thi-truong-chung-khoan.3/page-"
link_selector = ".PreviewTooltip"
date_selector = ".datePermalink"
selector = ".messageContent .baseHtml , .OverlayTrigger"
user_selector = ".userText .username"
page_selector = ".PageNav a"
reply_selector = ".major font"
link_date_selector = ".faint .DateTime"
start = clean_date("01/08/2016")
end = today()
pinned = 2


timestart = now()
output = scrape_forum(start, end, url, selector, link_selector, date_selector, page_selector, user_selector, reply_selector, link_date_selector, pinned)
#link[[1]]=link[[1]][-1]
#link[[2]]=link[[2]][-1]
#scrape_news parameter: source,start,end,selector,date_selector,link_selector
#content = scrape_news(url, start, end,selector,date_selector,link_selector)
print(now()-timestart)

# Doc title
link = c()
for (i in c(1:100)) {
  message("scraping page ", i)
  sub_link = scrape_forum(start, end, link, selector, date_selector, page_selector, user_selector, reply_selector, link_date_selector)
  link = rbind(link,sub_link)
}

