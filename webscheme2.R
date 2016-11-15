##############################
##    Scrape by date  ########
##    1 page / date   ########
##############################
#if (update == 2) start_date = clean_date("01-01-2010")
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
  #Check bai da scrape gan nhat
  if (update == 1) {
    d = last_date_table$date[last_date_table$category == code]
    if (length(d) > 0) end_date = d
    rm(d)
  }
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
  od = as.Date(p_index[!is.na(p_index)], "%Y-%m-%d")
  if (k==-Inf | is.na(k)) {k = 1}
  if (length(od) == 0) {
    d = end_date
    i = format_date(d)
  } else {
    d = od
    i = format_date(d)
  }
  
  rm(k_index, p_index, e_index)
  
  # Loop
  ok = TRUE
  if (exists("skip_cm")) {
    if (skip_cm > 0) {
      ok = FALSE
      skip_cm = skip_cm - 1
    }
    if (skip_cm == 0) rm(skip_cm)
  }
  while (ok) {
    # Lay nhieu link trong chuyen muc mot luc
    if (update == 1) temp = rep(NA, 20)
    if (update != 1) temp = rep(NA, 200)
    if (update == 2) temp = rep(NA, 20)
    final = list(link = temp, title = temp, date = temp, content = temp)
    col_names = c("link", "title", "date", "content")
    rm(temp)
    # Dien vao
    skipped = c()
    last_count = 0
    final.record.signal = 0
    while (sum(is.na(final[["link"]])) > 0) {
      cat("Looking into date", i," section: ", as.character(code),"\n")
      link_list_result = source %>% paste(i,source_suffix,sep = "") %>% 
        get_article(article_selector)
      if (length(link_list_result[[3]]) == 0) last_count = last_count + 1  
      if (length(link_list_result[[3]]) != 0 & length(unique(link_list_result[[3]])) < 10) {
        if (!exists("linkcheck")) { linkcheck = unique(link_list_result[[3]]) } else {
          if (mean(linkcheck == unique(link_list_result[[3]]))==1) last_count = last_count + 1
        }
      }
      
      if (last_count >= 20) final.record.signal = 1
      if (link_list_result[1]==1) {
        message("Skipped page ", i)
        skipped = c(skipped, i)
        d = d - 1
        i = format_date(d)
      } else {
        link = link_list_result[[2]]
        link[str_sub(link,1,4)!="http"] = paste(link_prefix,link[str_sub(link,1,4)!="http"],sep="")
        title = link_list_result[[3]]
        # Sua loi link bi lap lai
        index_rm = which(str_sub(as.character(title),1,3) %in% c("\n  ",""))
        if (length(index_rm) > 0) {
          link = link[-index_rm]
          title = title[-index_rm]
        }
        if (length(link) != length(title)) {
          message("Error link and title mismatched, skipped page") 
          skipped = c(skipped, i)
          d = d - 1
          i = format_date(d)
          next
        }
        # _____Check xem link da scrape tu truoc chua ####
        # _____NOTE: neu update thi chap nhan lap bai de tranh k dung duoc vong lap
        if (update != 1) {
          index_get = which(is.na(match(link, link_list$link)))
          link = link[index_get]
          title = title[index_get]
          rm(index_get)
        }
        # _____Dien link vao list  ####
        final = list_fill(list = final, vector = link, index = "link") %>% 
          list_fill(vector = title, index = "title")
        d = d - 1
        i = format_date(d)
      }
      if (final.record.signal == 1) { 
        message("finising")
        ok = FALSE # chuyen qua chuyen muc khac
        if (length(which(!is.na(final[["link"]]))) == 0) {
          message("no link") 
          break
        } else {
          lastrecord = max(which(!is.na(final[["link"]])))
          final[["link"]] = final[["link"]][1:lastrecord]
          final[["title"]] = final[["title"]][1:lastrecord]
          final[["date"]] = final[["date"]][1:lastrecord]
          final[["content"]] = final[["content"]][1:lastrecord]
        }
      }
    }
    rm(linkcheck)
    if (length(which(!is.na(final[["link"]]))) > 0) {
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
      }
      # Check xem bai cuoi cung da vuot qua gioi han thoi gian chua
      message("checking last record")
      last_date = as_date(min(as.integer(final[["date"]][which(!is.na(final[["date"]]) 
                                                               & as.character(final[["date"]])!="error")])))
      if (last_date < start_date) { ok = FALSE } 
      if (ok == FALSE) {
        message("Done scraping with specified time range. Saving...")
        save_list_csv(final, save_dir, code, col_names, suffix = paste("file",k,"page",d+1,sep=""))
      } else {
        cat("Saving...\n")
        save_list_csv(final,save_dir,code,col_names,suffix = paste("file",k,"page",d+1,sep=""))
        k = k + 1
        gc()
      }
    }
  }
  # merge cac file da scrape cua cac chuyen muc va lay link de so sanh
  merge_result = merge_temp(save_dir, code)
  link_list = rbind(link_list, merge_result[,1]) %>% unique()
  setwd(paste(dir,"/", site, "/tempLink",sep=""))
  write_excel_csv(merge_result[,1], paste(site,"_link.csv", sep=""))
  rm(merge_result)
  gc()
}