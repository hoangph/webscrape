#==================================#
### MODULE 1: PAGE LOOP ONLY #######
#==================================#
scrape.by_page = function(site, start_date, end_date, last_date_table, 
                          file_index_by = 'page', update,
                          link_list, cm_list, batch_size = 20, cm_done = c()) {
  cm_list = cm_list[!cm_list$tencm %in% cm_done,]
  # Vong lap chuyen muc
  for (j in c(1:nrow(cm_list))) {
    # Khai bao Parameters
    ten_cm = as.character(cm_list$tencm[j])
    source = cm_list$linkcm[j]
    scrape_params = node_par(site, ten_cm)
    link_prefix = scrape_params[["link_prefix"]]
    source_suffix = scrape_params[["source_suffix"]]
    content_selector = scrape_params[["content_selector"]]
    date_selector = scrape_params[["date_selector"]]
    article_selector = scrape_params[["article_selector"]]
    save_dir_prefix = scrape_params[["save_dir_prefix"]]
    link_structure = scrape_params[["link_structure"]]
    tencm.link = cm_list$tencm.link[j]
    # For updating: end at the date of the last record
    if (update == 1) {
      d = last_date_table$date[last_date_table$category == ten_cm]
      if (length(d) > 0) start_date = d
      rm(d)
    }
    skipped_page = c()
    last_count = 0
    empty_count = 0
    final.record.signal = 0
    sp = start_point(file_index_by = file_index_by, 
                     save_dir = paste(save_dir_prefix, site, sep = '/'),
                     site = site, code = ten_cm)
    k = sp[[1]]
    i = sp[[2]]
    
    # Loop within one section
    while (final.record.signal == 0) {
      # Create a big object for everything 
      temp = rep(NA, batch_size)
      final = list(link = temp, title = temp, date = temp, content = temp)
      col_names = c("link", "title", "date", "content")
      rm(temp)
      
      #==================================#
      #########  PREPARE LINKS ###########
      #==================================#
      
      ## While the object is still not filled completely
      while (sum(is.na(final[["link"]])) > 0) {
        ## Message
        cat("Looking into page", i," section: ", as.character(ten_cm),"\n")
        
        ## Indentify the articles on page i
        t.link_list_result = construct_link(link_structure, source = source,
                                            i = i, ten_cm = tencm.link, 
                                            source_suffix = source_suffix) %>%
          get_article(article_selector)
        
        ## If no article is found -> possibly last page of the section 
        ## -> count 20 times before stopping
        ## if there is article -> only some dates have no articles
        ## -> reset empty_count 
        if (length(link_list_result[[3]])==0) {
          empty_count = empty_count + 1
        } else { empty_count = 0 }
        ## Compare with last page -> if duplicated -> end of section
        if (length(link_list_result[[3]]) != 0 &
            !exists("last_page_links")) {
          # First time happens
          last_page_links = unique(link_list_result[[3]])
        } else {
          # Compare to last page
          if (length(link_list_result[[3]]) != 0 &
              exists("last_page_links")) {
            if (mean(unique(link_list_result[[3]])==1 %in% last_page_links)==1) last_count = last_count + 1
          }
        }
        ## After 20 counts -> signal to change section
        if (last_count >= 20 | empty_count >= 20) final.record.signal = 1
        if (final.record.signal == 1) { 
          message("Last page reached")
          rm(last_page_links)
          if (length(which(!is.na(final[["link"]]))) == 0) {
            message("No link in last page") 
            break
          } else {
            lastrecord = max(which(!is.na(final[["link"]])))
            final[["link"]] = final[["link"]][1:lastrecord]
            final[["title"]] = final[["title"]][1:lastrecord]
            final[["date"]] = final[["date"]][1:lastrecord]
            final[["content"]] = final[["content"]][1:lastrecord]
            break
          }
        }
        ## If not last page of the section then start preparing links before scraping
        ### If cannot get articles from page then skip page
        if (link_list_result[1]==1) {
          message("Skipping page...", i)
          skipped = c(skipped, i)
          i = i + 1
          ### Next page in loop
          next
        }
        link = link_list_result[[2]]
        ### Links without the domain need to be corrected
        link[str_sub(link,1,4)!="http"] = paste(link_prefix,link[str_sub(link,1,4)!="http"],sep="")
        title = link_list_result[[3]]
        ### Remove duplicated links from the list
        ### Check the first 3 characters in titles to see if they are duplicates of other titles
        index_rm = which(str_sub(as.character(title),1,3) %in% c("\n  ",""))
        if (length(index_rm) > 0) {
          link = link[-index_rm]
          title = title[-index_rm]
        }
        ### If links and titles have different lengths then skip page
        if (length(link) != length(title)) {
          message("Error: link and title mismatched, skipping page...") 
          skipped = c(skipped, i)
          i = i + 1
          ### Next page in loop
          next
        }
        ### Check xem link da scrape tu truoc chua
        ### NOTE: neu update thi chap nhan lap bai de tranh k dung duoc vong lap
        if (update != TRUE) {
          not_dup_index = which(is.na(match(link, link_list$link)))
          link = link[not_dup_index]
          title = title[not_dup_index]
          rm(not_dup_index)
        }
        ### Add current page's links and titles into the big object
        final = list_fill(list = final, vector = link, index = "link") %>% 
          list_fill(vector = title, index = "title")
        
        # Done with current page, move to next page of the same section
        i = i + 1
      } # End page loop
      
      # By now we have had the first few links of this section
      # to begin reading their contents
      
      #==================================#
      ##########  READING CONTENTS #######
      #==================================#
      if (length(which(!is.na(final[["link"]]))) > 0) {
        # Doc cac bai trong list link vua lay
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
      }
      # Check xem bai cuoi cung da vuot qua gioi han thoi gian chua
      message("checking last record")
      last_date = as_date(min(as.integer(final[["date"]][which(!is.na(final[["date"]]) 
                                                               & as.character(final[["date"]])!="error")])))
      if (last_date < start_date) {
        message("Reached start_date. Saving and moving to next section...")
        if (update != 'test') {
          save_list_csv(final, save_dir = paste(save_dir_prefix, site, sep = '/'), 
                        site = site, code = ten_cm, 
                        col_names, suffix = paste("file",k,"page",i-1,sep=""))
        }
        break
      } else {
        cat("Saving...\n")
        if (update != 'test') {
          save_list_csv(final, save_dir = paste(save_dir_prefix, site, sep = '/'),
                        site, code = ten_cm, col_names, 
                        suffix = paste("file",k,"page",i-1,sep=""))
        }
        k = k + 1
      }
      gc()
    } # within section loop
  } # section loop
}

#==================================#
### MODULE 2: DATE LOOP ONLY #######
#==================================#
scrape.by_date = function(site, start_date, end_date, last_date_table, 
                          file_index_by = 'date', cm_done = c(), update,
                          link_list, cm_list, batch_size = 20) {
  cm_list = cm_list[!cm_list$tencm %in% cm_done,]
  # Vong lap chuyen muc
  for (j in c(1:nrow(cm_list))) {
    # Khai bao Parameters
    ten_cm = as.character(cm_list$tencm[j])
    source = cm_list$linkcm[j]
    scrape_params = node_par(site, ten_cm)
    link_prefix = scrape_params[["link_prefix"]]
    source_suffix = scrape_params[["source_suffix"]]
    source_dateformat = scrape_params[["source_dateformat"]]
    content_selector = scrape_params[["content_selector"]]
    date_selector = scrape_params[["date_selector"]]
    article_selector = scrape_params[["article_selector"]]
    save_dir_prefix = scrape_params[["save_dir_prefix"]]
    link_structure = scrape_params[["link_structure"]]
    tencm.link = cm_list$tencm.link[j]
    # For updating: end at the date of the last record
    if (update == 1) {
      d = last_date_table$date[last_date_table$category == ten_cm]
      if (length(d) > 0) start_date = d
      rm(d)
    }
    skipped_page = c()
    last_count = 0
    empty_count = 0
    final.record.signal = 0
    sp = start_point(file_index_by = file_index_by, 
                     save_dir = paste(save_dir_prefix, site, sep = '/'),
                     site = site, code = ten_cm)
    k = sp[[1]]
    # This i is not in the right format to be added to the links
    i.wrong_format = sp[[2]]
    d = as.Date(i.wrong_format, format = '%d-%m-%Y')
    # Transform to the right format
    i = format_date(d, source_dateformat)
    
    # Loop within one section
    while (final.record.signal == 0) {
      # Create a big object for everything 
      temp = rep(NA, batch_size)
      final = list(link = temp, title = temp, date = temp, content = temp)
      col_names = c("link", "title", "date", "content")
      rm(temp)
      
      #====================================#
      ##########  PREPARE LINKS ############
      #====================================#
      
      ## While the object is still not filled completely
      while (sum(is.na(final[["link"]])) > 0) {
        ## Message
        cat("Looking into date", i," section: ", as.character(ten_cm),"\n")
        
        ## Indentify the articles on page i
        html_link = construct_link(link_structure, source = source,
                                   i = i, ten_cm = tencm.link, 
                                   source_suffix = source_suffix)
        message(html_link)
        link_list_result = html_link %>% get_article(article_selector)
        ## If no article is found -> possibly last page of the section 
        ## -> count 20 times before stopping
        ## if there is article -> only some dates have no articles
        ## -> reset empty_count 
        if (length(link_list_result[[3]])==0) {
          empty_count = empty_count + 1
        } else { empty_count = 0 }
        ## Compare with last page
        ## -> if duplicated -> end of section
        if (length(link_list_result[[3]]) != 0 &
            !exists("last_page_links")) {
          # First time happens
          last_page_links = unique(link_list_result[[3]])
        }
        if (length(link_list_result[[3]]) != 0 &
            exists("last_page_links")) {
          # Compare to last page
          if (length(link_list_result[[3]]) != 0 &
              exists("last_page_links")) {
            if (mean(unique(link_list_result[[3]])==1 %in% last_page_links)==1) last_count = last_count + 1
          }  
        }
        
        ## After 20 counts -> signal to change section
        if (last_count >= 20 | empty_count >= 20) final.record.signal = 1
        if (final.record.signal == 1) { 
          message("Last page reached")
          rm(last_page_links)
          if (length(which(!is.na(final[["link"]]))) == 0) {
            message("No link in last page") 
            break
          } else {
            lastrecord = max(which(!is.na(final[["link"]])))
            final[["link"]] = final[["link"]][1:lastrecord]
            final[["title"]] = final[["title"]][1:lastrecord]
            final[["date"]] = final[["date"]][1:lastrecord]
            final[["content"]] = final[["content"]][1:lastrecord]
            break
          }
        }
        ## If not last page of the section then start preparing links before scraping
        ### If cannot get articles from page then skip page
        if (link_list_result[1]==1) {
          message("Skipping page...", i)
          skipped = c(skipped, i)
          d = d - 1
          i = format_date(d, source_dateformat)
          ### Next page in loop
          next
        }
        link = link_list_result[[2]]
        ### Links without the domain need to be corrected
        link[str_sub(link,1,4)!="http" & !is.na(link)] = paste(link_prefix,link[str_sub(link,1,4)!="http" & !is.na(link)],sep="")
        title = link_list_result[[3]]
        ### Remove duplicated links from the list
        ### Check the first 3 characters in titles to see if they are duplicates of other titles
        index_rm = which(str_sub(as.character(title),1,3) %in% c("\n  ",""))
        ### Check duplicated links in the list
        index_rm = c(index_rm, duplicated(link)) %>% unique()
        ### Check NA link
        na.link.detect = which(is.na(link))
        if (length(na.link.detect) > 0) message("NA link detected")
        
        index_rm = c(index_rm, na.link.detect)
        if (length(index_rm[index_rm > 0]) > 0) {
          link = link[-index_rm]
          title = title[-index_rm]
        }
        
        ### If links and titles have different lengths then skip page
        if (length(link) != length(title)) {
          message("Error: link and title mismatched, skipping page...") 
          skipped = c(skipped, i)
          d = d - 1
          i = format_date(d, source_dateformat)
          ### Next page in loop
          next
        }
        ### Check xem link da scrape tu truoc chua
        ### NOTE: neu update thi chap nhan lap bai de tranh k dung duoc vong lap
        if (update != TRUE) {
          not_dup_index = which(is.na(match(link, link_list$link)))
          link = link[not_dup_index]
          title = title[not_dup_index]
          rm(not_dup_index)
        }
        ### Add current page's links and titles into the big object
        final = list_fill(list = final, vector = link, index = "link") %>% 
          list_fill(vector = title, index = "title")
        
        # Done with current page, move to next page of the same section
        d = d - 1
        i = format_date(d, source_dateformat)
      } # End page loop
      
      # By now we have had the first few links of this section
      # to begin reading their contents
      
      #==================================#
      ########  READING CONTENTS #########
      #==================================#
      if (length(which(!is.na(final[["link"]]))) > 0) {
        # Doc cac bai trong list link vua lay
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
      }
      # Check xem bai cuoi cung da vuot qua gioi han thoi gian chua
      message("checking last record")
      last_date = as_date(min(as.integer(final[["date"]][which(!is.na(final[["date"]]) 
                                                               & as.character(final[["date"]])!="error")])))
      if (last_date < start_date) {
        message("Reached start_date. Saving and moving to next section...")
        if (update != 'test') {
          save_list_csv(final, save_dir = paste(save_dir_prefix, site, sep = '/'),
                        site, code = ten_cm, 
                        col_names, suffix = paste("file",k,"page",d+1,sep=""))
        }
        break
      } else {
        cat("Saving...\n")
        if (update != 'test') {
          save_list_csv(final, save_dir = paste(save_dir_prefix, site, sep = '/'),
                        site, code = ten_cm, 
                        col_names, suffix = paste("file",k,"page",d+1,sep=""))
        }
        k = k + 1
      }
      gc()
    } # within section loop
  } # section loop
}

#====================================#
### MODULE 3: DATE & PAGE LOOP #######
#====================================#
scrape.by_date_page = function(site, start_date, end_date, last_date_table, 
                               file_index_by='date_page', cm_done = c(),update,
                               link_list, cm_list, batch_size = 20) {
  cm_list = cm_list[!cm_list$tencm %in% cm_done,]
  # Vong lap chuyen muc
  for (j in c(1:nrow(cm_list))) {
    # Khai bao Parameters
    ten_cm = as.character(cm_list$tencm[j])
    source = cm_list$linkcm[j]
    scrape_params = node_par(site, ten_cm)
    
    link_prefix = scrape_params[["link_prefix"]]
    source_suffix = scrape_params[["source_suffix"]]
    source_pagenumber = scrape_params[["source_pagenumber"]]
    source_dateformat = scrape_params[["source_dateformat"]]
    content_selector = scrape_params[["content_selector"]]
    date_selector = scrape_params[["date_selector"]]
    article_selector = scrape_params[["article_selector"]]
    save_dir_prefix = scrape_params[["save_dir_prefix"]]
    link_structure = scrape_params[["link_structure"]]
    tencm.link = cm_list$tencm.link[j]
    # For updating: end at the date of the last record
    if (update == 1) {
      d = last_date_table$date[last_date_table$category == ten_cm]
      if (length(d) > 0) start_date = d
      rm(d)
    }
    skipped_page = c()
    last_count = 0
    empty_count = 0
    final.record.signal = 0
    sp = start_point(file_index_by = file_index_by,
                     save_dir = paste(save_dir_prefix, site, sep = '/'),
                     site = site, code = ten_cm)
    k = sp[[1]]
    # This i is not in the right format to be added to the links
    i.wrong_format = sp[[2]]
    d = as.Date(i.wrong_format, format = '%d-%m-%Y')
    # Transform to the right format
    i = format_date(d, source_dateformat)
    # Loop within one section
    while (final.record.signal == 0) {
      # Create a big object for everything 
      temp = rep(NA, batch_size)
      final = list(link = temp, title = temp, date = temp, content = temp)
      col_names = c("link", "title", "date", "content")
      rm(temp)
      
      #====================================#
      ##########  PREPARE LINKS ############
      #====================================#
      
      ## While the object is still not filled completely, loop through date
      while (sum(is.na(final[["link"]])) > 0) {
        ## Loop within pages of each date 
        last_daily = 0 
        page = 0
        while (last_daily < 2) { #try twice before stop
          page = page + 1
          ## Message
          cat("Looking into date", i," page:", page, ", section: ", as.character(ten_cm),"\n")      
          ## Indentify the articles on page i
          html_link = construct_link(link_structure, source = source,
                                     i = i, ten_cm = tencm.link, page= page,
                                     source_suffix = source_suffix)
          message(html_link)
          t.link_list_result = html_link %>% get_article(article_selector)
          ## If no article is found -> last page of that day 
          if (length(t.link_list_result[[3]]) == 0) last_daily = last_daily + 1
          ## Join links from several pages to one object
          if (page == 1) {link_list_result = t.link_list_result} else {
            link_list_result[[2]] = c(link_list_result[[2]], t.link_list_result[[2]])
            link_list_result[[3]] = c(link_list_result[[3]], t.link_list_result[[3]])
          }
        } #End loop within one date
        
        ## If no article is found -> possibly last page of the section 
        ## -> count 20 times before stopping
        ## if there is article -> only some dates have no articles
        ## -> reset empty_count 
        if (length(link_list_result[[3]])==0) {
          empty_count = empty_count + 1
        } else { empty_count = 0 }
        ## Compare with last page
        ## -> if duplicated -> end of section
        if (length(link_list_result[[3]]) != 0 &
            !exists("last_page_links")) {
          # First time happens
          last_page_links = unique(link_list_result[[3]])
        } else {
          # Compare to last page
          if (length(link_list_result[[3]]) != 0 &
              exists("last_page_links")) {
            if (mean(unique(link_list_result[[3]])==1 %in% last_page_links)==1) last_count = last_count + 1
          }
        }
        ## After 20 counts -> signal to change section
        if (last_count >= 20 | empty_count >= 20) final.record.signal = 1
        if (final.record.signal == 1) { 
          message("Last page reached")
          rm(last_page_links)
          if (length(which(!is.na(final[["link"]]))) == 0) {
            message("No link in last page") 
            break
          } else {
            lastrecord = max(which(!is.na(final[["link"]])))
            final[["link"]] = final[["link"]][1:lastrecord]
            final[["title"]] = final[["title"]][1:lastrecord]
            final[["date"]] = final[["date"]][1:lastrecord]
            final[["content"]] = final[["content"]][1:lastrecord]
            break
          }
        }
        ## If not last page of the section then start preparing links before scraping
        ### If cannot get articles from page then skip page
        if (link_list_result[1]==1) {
          message("Skipping page...", i)
          skipped = c(skipped, i)
          d = d - 1
          i = format_date(d, source_dateformat)
          ### Next page in loop
          next
        }
        link = link_list_result[[2]]
        ### Links without the domain need to be corrected
        link[str_sub(link,1,4)!="http"] = paste(link_prefix,link[str_sub(link,1,4)!="http"],sep="")
        title = link_list_result[[3]]
        ### Remove duplicated links from the list
        ### Check the first 3 characters in titles to see if they are duplicates of other titles
        index_rm = which(str_sub(as.character(title),1,3) %in% c("\n  ",""))
        ### Check duplicated links in the list
        index_rm = c(index_rm, duplicated(link)) %>% unique()
        if (length(index_rm[index_rm > 0]) > 0) {
          link = link[-index_rm]
          title = title[-index_rm]
        }
        
        ### If links and titles have different lengths then skip page
        if (length(link) != length(title)) {
          message("Error: link and title mismatched, skipping page...") 
          skipped = c(skipped, i)
          d = d - 1
          i = format_date(d, source_dateformat)
          ### Next page in loop
          next
        }
        ### Check xem link da scrape tu truoc chua
        ### NOTE: neu update thi chap nhan lap bai de tranh k dung duoc vong lap
        if (update != TRUE) {
          not_dup_index = which(is.na(match(link, link_list$link)))
          link = link[not_dup_index]
          title = title[not_dup_index]
          rm(not_dup_index)
        }
        ### Add current page's links and titles into the big object
        final = list_fill(list = final, vector = link, index = "link") %>% 
          list_fill(vector = title, index = "title")
        
        # Done with current page, move to next page of the same section
        d = d - 1
        i = format_date(d, source_dateformat)
      } # End page loop
      
      # By now we have had the first few links of this section
      # to begin reading their contents
      
      #########################################
      ##########  READING CONTENTS ############
      #########################################
      if (length(which(!is.na(final[["link"]]))) > 0) {
        # Doc cac bai trong list link vua lay
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
      }
      # Check xem bai cuoi cung da vuot qua gioi han thoi gian chua
      message("checking last record")
      last_date = as_date(min(as.integer(final[["date"]][which(!is.na(final[["date"]]) 
                                                               & as.character(final[["date"]])!="error")])))
      if (last_date < start_date) {
        message("Reached start_date. Saving and moving to next section...")
        if (update != 'test') {
          save_list_csv(final, save_dir = paste(save_dir_prefix, site, sep = '/'),
                        site, code = ten_cm, 
                        col_names, suffix = paste("file",k,"page",d+1,sep=""))
        }
        break
      } else {
        cat("Saving...\n")
        if (update != 'test') {
          save_list_csv(final, save_dir = paste(save_dir_prefix, site, sep = '/'),
                        site, code = ten_cm, 
                        col_names, suffix = paste("file",k,"page",d+1,sep=""))
        }
        k = k + 1
      }
      gc()
    } # within section loop
  } # section loop
}

