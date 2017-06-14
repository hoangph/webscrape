################################################-
#####  Web scraping 
#####  written for DEPOCEN 
#####  by Hoang Phan. September 2016 
################################################-

#-------------------------#
####   CONTROL PANEL   ####
#-------------------------#
#dir = "/home/hoangph/Documents/Webscrape"
dir = "D:/Webscrape/webscrape"
setwd(dir)
source("parameter.R", encoding = "UTF-8")
source("functions.R")
source('one_code_rules_all.R')
machine = "ser"
operation = "windows"

#---------------------------------------#
####        Update database          ####
#---------------------------------------#
if (machine != "ser" & operation == "ubuntu") filesync("ubuntu", "/usr/bin", paste("final", "sto", machine, "ffs_batch", sep = "."))

#---------------------------------------#
####       Target identifier         ####
#---------------------------------------#

#### __Targets ####

sites = c("baodatviet", "baophapluat", "cafef", "dantri", 
          "laodong", "ndh", "nhandan", "thanhnien", "toquoc", 
          "vietnamnet", "vneconomy", "vnexpress", "vov", "vtc")
#### __configurations ####
#Update: FALSE-no, TRUE-yes, 'test'-test
#update = 2
#if (update == 2) start_date = today()

#---------------------------------------#
####            Scrape               ####
#---------------------------------------#
scrape_site = function(site, update, file_index_by, 
                       cm_done = c(), batch_size=200) {
  
  start_date = clean_date("01/01/2010")
  if (update == 'test') start_date <- today()
  end_date = today()

  link_list = call_file(file.type = "final.link", site = site, index = "link")
  link_temp = call_tlink(site, "link")
  link_list = rbind(link_list, link_temp) %>% unique
  rm(link_temp)
  
  if (update == 1) {
    year_end = year(end_date)
    if (machine == 'ser') final.source = 'final'
    if (machine == 'scr1') final.source = 'final.store'
    latest.data = call_file(site = site, file.type = final.source, year_end)
    # In case latest data is from last year
    while (length(latest.data) == 0) {
      year_end = year_end-1
      latest.data = call_file(site = site, file.type = final.source, year_end)
    }
    latest.data = latest.data[!is.na(latest.data$date),]
    last_date_table = c()
    for (cm in unique(latest.data$category)) {
      maxd = max(latest.data$date[latest.data$category == cm])
      mind = min(latest.data$date[latest.data$category == cm])
      last_date_table = rbind(last_date_table, data.frame(category = cm, date = maxd, min_date = mind))
      rm(maxd, mind)
    }
    rm(latest.data)
    gc()
  }
  
  # List of sections to scrape
  cm_list = link_par(site)
  
  cat('done preparing \n')
  
  setwd(dir)
  source('one_code_rules_all.R')
  
  if (file_index_by == 'page') {
    scrape.by_page(site = site, start_date = start_date, end_date = end_date,
                   link_list = link_list, last_date_table = last_date_table, 
                   update = update,
                   cm_done = cm_done, cm_list = cm_list, batch_size = batch_size)  
  }
  if (file_index_by == 'date') {
    scrape.by_date(site = site, start_date = start_date, end_date = end_date,
                   link_list = link_list, last_date_table = last_date_table, 
                   update = update,
                   cm_done = cm_done, cm_list = cm_list, batch_size = batch_size)  
  }
  if (file_index_by == 'date_page') {
    scrape.by_date_page(site = site, start_date = start_date, end_date = end_date,
                        link_list = link_list, last_date_table = last_date_table, 
                        update = update,
                        cm_done = cm_done, cm_list = cm_list, batch_size = batch_size)  
  }
  
  if (update == 2) {
      setwd(save_dir)
      testdata = c()
      for (file in list.files()) {
          t = read_csv(file)
          testdata = rbind(testdata, t) %>% unique()
      }
      gc()
      sum(testdata$content == "")
  }
  

} # end scrape_site


#---------------------------------------#
####          Syncronize             ####
#---------------------------------------#
sync_temp_to_store = function(site) {
  if (update != 'test' & machine != 'ser') {
    filesync(operation = "ubuntu", freefilesync.dir = "/usr/bin", 
             batchfile = paste(site, "temp", machine, "sto", "ffs_batch", sep = "."))
    filesync(operation = "ubuntu", freefilesync.dir = "/usr/bin", 
             batchfile = paste(site, "rmtemp", machine, "ffs_batch", sep = "."))
  }
}
# Update temp files: scraper -> Storage (Update)

#---------------------------------------------------#
####          MERGE FILES INTO DATABASE          ####
#--------------------------------------------------#
update_final = function(sites) {
  setwd(dir)
  source("parameter.R")
  source("functions.R")
  # Update temp files: storage -> server (update)
  filesync(operation = "windows", freefilesync.dir = "C:/Program Files/FreeFileSync", 
           batchfile = "temp.sto.ser.ffs_batch")
  # Delete temp files in storage
  filesync(operation = "windows", freefilesync.dir = "C:/Program Files/FreeFileSync", 
           batchfile = "rmtemp.sto.ffs_batch")
  # Merge temp files into final data: on server
  for (site in sites) {
    cm_list = link_par(site)
    par = node_par(site, cm = unique(cm_list$tencm)[1])
    save_dir = par[["save_dir"]]
    new.data = merge_temp(save_dir, site, code = unique(cm_list$tencm))
    if (!is.null(new.data)) update_final(x = new.data, site = site)
    gc()
  }
  # Delete temp files in server
  setwd(paste(dir,"ffs_batch", sep = "/"))
  filesync(operation = "windows", freefilesync.dir = "C:/Program Files/FreeFileSync", 
           batchfile = paste("rmtemp", "ser", "ffs_batch", sep = "."))

  # Update final files: server -> Storage (Mirror)
  filesync(operation = "windows", freefilesync.dir = "C:/Program Files/FreeFileSync", 
           batchfile = "final.ser.sto.ffs_batch")
  
} #end update_final

# (Optional) Update final link list
while (FALSE) {
  for (site in sites) {
    message(site)
    start.year = 2000
    end.year = 2016 
    create.linklist(site, start.year, end.year)
    gc()
  }
}



#--------------------------------------------#
####          UPDATE WEBSITES             ####
#--------------------------------------------#

#Test tools
test_all = function(sites) {
  for (i in c(1:length(sites))) {
    site = sites[i]
    webscheme = node_par(up.site)[['webscheme']]
    scrape_site(site = up.site, update = 'test', webscheme = webscheme)
  }
}

scrape_all = function(sites) {
  for (i in c(1:length(sites))) {
    up.site = sites[i]
    webscheme = node_par(up.site)[['webscheme']]
    scrape_site(site = up.site, update = 1, webscheme = webscheme)
  }
}




