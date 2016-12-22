################################################-
#####  Web scraping 
#####  written for DEPOCEN 
#####  by Hoang Phan. September 2016 
################################################-

#-------------------------#
####   CONTROL BOARD   ####
#-------------------------#
#dir = "/home/hoangph/Documents/Webscrape"
dir = "D:/Webscrape/webscrape"
setwd(dir)
source("parameter.R", encoding = "UTF-8")
source("functions.R")
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

sites_scr1 = c("vneconomy","ndh", "nguoiduatin", "vtc", "toquoc", 'doanhnhansaigon', 
               'thanhnien', 'laodong')
sites_scr2 = c('vietnamnet', 'dantri', 'congluan', 'baodatviet', 'cafef', 'nhandan', 
               'baophapluat', 'vnexpress')

#### __configurations ####
#Update: FALSE-no, TRUE-yes, 'test'-test
#update = 2
#if (update == 2) start_date = today()

#---------------------------------------#
####            Scrape               ####
#---------------------------------------#
scrape_site = function(site, update, webscheme) {
  
  start_date = clean_date("01/01/2010")
  if (update == 'test') start_date <- today()
  end_date = today()
  
  link_list = call_link(site, "link")
  link_temp = call_tlink(site, "link")
  link_list = rbind(link_list, link_temp) %>% unique
  rm(link_temp)
  
  if (update == 1) {
    latest.data = call_final(site, year(end_date))
    last_date_table = c()
    for (cm in unique(latest.data$category)) {
      d = max(latest.data$date[latest.data$category == cm])
      last_date_table = rbind(last_date_table, data.frame(category = cm, date = d))
      rm(d)
    }
    rm(latest.data)
    gc()
  }
  
  cat('done preparing')
  
  setwd(dir)
  source(paste0('webscheme', webscheme, '.R'))
  runscheme(site, update, last_date_table, link_list, start_date, end_date)
  
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
  
  #---------------------------------------#
  ####          Syncronize             ####
  #---------------------------------------#
  
  # Update temp files: scraper -> Storage (Update)
  if (update != 2) {
      filesync(operation = "ubuntu", freefilesync.dir = "/usr/bin", 
               batchfile = paste(site, "temp", machine, "sto", "ffs_batch", sep = "."))
      filesync(operation = "ubuntu", freefilesync.dir = "/usr/bin", 
               batchfile = paste(site, "rmtemp", machine, "ffs_batch", sep = "."))
  }

} # end scrape_site

#---------------------------------------------------#
####          MERGE FILES INTO DATABASE          ####
#--------------------------------------------------#
update_final = function() {
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

scrape_all = function(sites) {
  for (i in c(1:length(sites))) {
    up.site = sites[i]
    webscheme = node_par(up.site)[['webscheme']]
    scrape_site(site = up.site, update = 1, webscheme = webscheme)
  }
}




