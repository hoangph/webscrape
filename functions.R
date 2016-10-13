################################################-
#####  Written by Hoang Phan. September 2016 
################################################-

###--------------------------------###
#     General purpose functions      #
###--------------------------------###


# Try read HTML function #
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
merge_files = function(directory) {
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

# Examine date
date_by_cat = function(x) {
  x$date = x$date %>% as_date()
  cm = unique(x$category)
  for (i in c(1:length(cm))) {
    message(cm[i],": min date: ", min(x$date[!is.na(x$date) & x$category == cm[i]]), 
              ", max date: ", max(x$date[!is.na(x$date) & x$category == cm[i]]),"\n")
  }
}

# Split data by year
split_by_year = function(x, site, savedir) {
  x$date = as_date(x$date)
  year = unique(year(x$date))
  for (y in year) {
    split = x[year(x$date) == y,]
    setwd(savedir)
    message("Writing ", y, "(", match(y,year),"/",length(year),")")
    write_excel_csv(split, paste(site,"_",y,".csv",sep=""))
  }
}

