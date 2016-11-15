################################################-
#####  Written by Hoang Phan. September 2016 
################################################-
call.library = function() {
  library(tidyverse)
  library(Rfacebook)
  library(lubridate)
  library(rvest)
  library(stringr)
  library(tm)
  library(stringi)
  library(purrr)
}
call.library()

###--------------------------------###
####   General purpose functions  ####
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

# Change the encoding
encode = function(char) {
  iconv(char)
}

# Translate date format from Vietnamese -> R (English)
clean_date = function (date) {
  edate = encode(date)
  date_s = str_sub(date, str_locate(date,"/")[1]-2, str_locate(date,"/")[1]-2+9)
  if (is.na(date_s)) { ## Date dang 30.11.2016 hoac 30-11-2016
    date = str_replace_all(date, pattern = "\\.", replacement = "/") 
    date = str_replace_all(date, pattern = "-", replacement = "/") 
    date_s = str_sub(date, str_locate(date,"/")[1]-2,str_locate(date,"/")[1]-2+9)
  }
  date_d = as.Date(date_s,"%d/%m/%Y")
  if (is.na(date_s)) {
    if (stri_detect_fixed(edate, "giờ trước")) date_d = today() - 1
    if (stri_detect_fixed(edate, "ngày trước")) {
      c = str_split(edate, " ")[[1]]
      t = c[which(stri_detect_fixed(c, "ngày"))[1] - 1] %>% as.integer()
      if (!is.na(t)) date_d = today() - t
    }
  }
  return(date_d)
}

a = function (edate) {
  stri_detect_fixed(edate,"ngày trước")
}

# Reformat date: US -> Euro
format_date = function (date) {
  format(date, "%d-%m-%Y")
}

# Read article's content
read_page = function(url, content_selector, date_selector) {
  errorPage = 1
  cat("Scraping: ", url,"\n")
  # try catch to avoid timeout error
  page_html = tryRead(url, 5, 2)
  if (page_html[[1]] == 1) {
    errorPage = 1
    return(list(errorPage, 1, 1))
  } else {
    ar_date = page_html[[2]] %>% html_nodes(date_selector) %>% html_text() %>%
      paste(collapse = "") %>% clean_date()
    if (is.na(ar_date)) {  # If there is no date in the article -> wrong format -> skip
      message("Skipped.")
      errorPage = 1
      return(list(errorPage, 1, 1))
    } else {
      message("Date: ", ar_date)
      paragraph = page_html[[2]] %>% html_nodes(content_selector) %>% 
        html_text() %>% unique() %>% paste(collapse = " ")
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
merge_temp = function(directory, code) {
  setwd(directory)
  text = c()
  merged = c()
  file_index = which(str_sub(list.files(), 1, str_locate(list.files(),"_")[,1]-1) %in% code)
  if (length(file_index) > 0) {
    c = 0
    for (i in file_index) {
      c = c+1
      message(c,"/",length(file_index))
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
}

# Call files
call_file = function(file.type, site, index) {
  if (file.type == "final") setwd(paste(dir,"/",site,"/finalData",sep=""))
  if (file.type == "temp") setwd(paste(dir,"/",site,"/tempData",sep=""))
  if (file.type == "tlink") setwd(paste(dir,"/",site,"/tempLink",sep=""))
  text = c()
  for (i in c(1:length(index))) {
    file.name = paste(site, "_", index[i], ".csv", sep = "")
    if (file.name %in% list.files()) {
      text_uniq = read_csv(file.name)
      if (!is.na(index[i]) & str_detect(index[i],"link")) {colnames(text_uniq) = "link"} 
      else {
        colnames(text_uniq) = c("link", "title", "date", "content", "category") 
        text_uniq$date = as_date(as.integer(text_uniq$date))
      }
      text = rbind(text, text_uniq)
      rm(text_uniq)
    }
  }
  return(text)
}


# Call data (raw, link, process)
call_final = partial(call_file, file.type = "final")
# Call temp files
call_temp = partial(call_file, file.type = "temp")
# Call temp link list
call_tlink = partial(call_file, file.type = "tlink")

# Call count result
call_count = function(site, key_code) {
  setwd(paste(dir,"/",site,"/finalData",sep=""))
  text_uniq = read_csv(paste(site, "_", key_code, ".csv", sep = ""))
  colnames(text_uniq) = c("link", "date", "category", "title_count", "para_count", "content_count", "total_count") 
  text_uniq$date = as_date(as.integer(text_uniq$date))
  return(text_uniq)
}

# Backup/Copy files using Freefilesync
filesync = function(operation, freefilesync.dir, batchfile) {
  if (operation == "ubuntu") command = "FreeFileSync "
  if (operation == "windows") command = "FreeFileSync.exe "
  setwd(freefilesync.dir)
  system(paste(command , dir, "/ffs_batch/", batchfile, sep = ""))
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
  year_col = year(x$date)
  year = unique(year_col)
  for (y in year) {
    if (is.na(y)) {
      split = x[is.na(year_col),]
    } else {
      split = x[year_col == y & !is.na(year_col),]
    }
    setwd(savedir)
    message("Writing ", y, "(", match(y,year),"/",length(year),")")
    write_excel_csv(split, paste(site,"_",y,".csv",sep=""))
  }
}

# Count keywords in text
count_key = function(text_vector, keywords_vector) {
  count = str_count(text_vector,pattern=paste(keywords_vector,collapse="|"))
  count[which(is.na(count))] <- 0
  return(count)
}
 
# Get the first n paragraphs 
first_real_text = function(string, n) {
  string = str_split(string, "\n")[[1]]
  string = string[nchar(string)>1]
  string = string[1:n] 
  string = string[which(!is.na(string))] %>% paste(collapse = "")
  return(string)
}

# Append new data in yearly database
update_final = function(x, site) {
  year_col = year(x$date)
  year = unique(year_col)
  for (y in year) {
    data = call_final(site, y)
    if (is.na(y)) {
      new = x[is.na(year_col),]
    } else {
      new = x[!is.na(year_col) & year_col == y,]
    }
    data = rbind(data, new)
    data = data[!duplicated(data$link),]
    cat("writing ", y, "\n")
    setwd(paste(dir,"/",site,"/finalData",sep=""))
    write_excel_csv(data, paste(site, "_", y, ".csv", sep = ""))
  }
  link = as.data.frame(x$link)
  colnames(link) = "link"
  link.list = call_final(site, "link")
  if (!is.null(link.list)) colnames(link.list) = "link"
  link.list = rbind(link.list, link) %>% unique()
  write_excel_csv(link.list, paste(site, "_link.csv", sep = ""))
  gc()
}

# Create list of links
create.linklist = function(site, start.year, end.year) {
  par = node_par(site, code)
  save_dir = par[["save_dir"]]
  start.year = as.integer(start.year)
  end.year = as.integer(end.year)
  linklist = c()
  for (year in as.character(c(start.year:end.year))){
    cat(year, "\n")
    data = call_final(site, as.character(year))
    linklist = rbind(linklist, data[,1])
  }
  linklist = unique(linklist)
  if (length(linklist) > 0) {
    colnames(linklist) = "link"
    setwd(paste(dir, "/", site, "/finalData", sep=""))
    write_excel_csv(linklist, paste(site,"_link.csv",sep=""))
  }
}


### Create column of month and year (= starting date)
time_round = function(x, date_col) {
  if (missing(date_col)) {
    date_col = "date"
  }
  date_index = which(names(x) == date_col)
  colnames(x)[date_index] = "date"
  x$month = floor_date(x$date, unit = "month")
  x$year = floor_date(x$date, unit = "year")
  return(x)
}

# Choose which parts are counted
total_count = function(x, col.count) {
  x$total_count = rowSums(x[,which(names(x) %in% col.count)])
  return(x)
}

# Visualizing 
plot_by_cat = function(text, count.column, unit, cat) {
  if (missing(unit)) { unit = "month"}
  if (!(unit %in% colnames(text))) {
    text = time_round(text)
  }
  count_index = which(names(text) %in% count.column)
  if (length(count_index>1)) {
    text = total_count(text, count.column)
    count_index = which(colnames(text) == "total_count")
  }
  text$contain = as.double(text[,count_index] > 0)
  cat_col = text[, which(colnames(text) == cat)]
  colnames(cat_col) = "cat"
  text = cbind(text, cat_col)
  ggplot(text[text$contain != 0,], aes(x = get(unit))) +
    geom_bar(fill="#CC0000") + facet_wrap(~cat)
}

plot_total = function(text, count.column, unit) {
  if (missing(unit)) { unit = "month"}
  if (!(unit %in% colnames(text))) {
    text = time_round(text)
  }
  count_index = which(names(text) %in% count.column)
  if (length(count_index>1)) {
    text = total_count(text, count.column)
    count_index = which(colnames(text) == "total_count")
  }
  text$contain = as.double(text[,count_index] > 0)
  ggplot(text[text$contain != 0,], aes(x = get(unit))) +
    geom_bar(fill="#CC0000") 
}
# Draw multiple plots
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Preview a vector
sumfactor = function(vector) {
  vector %>% factor %>% summary
}

# Save final data with suffix
save_final = function(x, site, name) {
  setwd(paste(dir,"/",site,"/finalData",sep=""))
  write_excel_csv(x, name)
}

#-------------------------------------#
#       Text analysis module          #
#-------------------------------------#

#------------------------------------------------------------------------------------#
# Basic analysis
# keywords = c("tham nhũng", "hối lộ")
### count in content? (contentcount: 0-title, 1-first 2 paragraphs; 2-full content; 3-all) 
basic_count = function(text, keywords, keycode, contentcount, twopara) {
  result = list()
  for (i in c(1:length(keywords))) {
    key = keywords[i]
    kcode = keycode[i]
    title_key = NA
    para_key = NA
    content_key = NA
    # Count in title
    message("counting in titles...")
    title_key = count_key(text$title, key)
    # Count in first 2 paragraphs
    if (contentcount == 1 | contentcount == 3) {
      ### Count in introductions
      message("counting in introductions...")
      para_key = count_key(twopara$content, key)
    }
    if (contentcount == 2 | contentcount == 3) {
      # Count in content
      message("counting in contents...")
      content_key = lapply(text$content, FUN = count_key, keywords_vector = key) %>% unlist()
    }
    # Merge into list
    result[[kcode]] = list(title_count = title_key, para_count = para_key, content_count = content_key)
    rm(title_key, para_key, content_key)
  }
  return(result)
}

#------------------------------------------------------------------------------------#