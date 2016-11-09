################################################-
#####  Web scraping 
#####  written for DEPOCEN 
#####  by Hoang Phan. September 2016 
################################################-

#-------------------------#
####   CONTROL BOARD   ####
#-------------------------#
#dir = "/home/hoangph2/Documents/Webscraping"
dir = "D:/Webscrape/webscrape"
setwd(dir)
source("parameter.R")
source("functions.R")

#---------------------------------------#
####        Update database          ####
#---------------------------------------#
machine = "scr2"
operation = "ubuntu"
if (operation == "ubuntu") filesync("ubuntu", "/usr/bin", paste("final", "sto", machine, "ffs_batch", sep = "."))

#---------------------------------------#
####       Target identifier         ####
#---------------------------------------#

#### __Targets ####
site = "dantri"
start_date = today() #clean_date("01/01/2006")
end_date = today()
#### __configurations ####
#Update: 0-no, 1-yes, 2-test
update = 2

#---------------------------------------#
####            Scrape               ####
#---------------------------------------#

cm_list = link_par(site)
link_list = call_final(site, "link")
link_temp = call_tlink(site, "link")
link_list = rbind(link_list, link_temp) %>% unique
rm(link_temp)

if (update == 1) {
  latest.data = call_final(site, year(end_date))
  start_date = max(latest.data$date)
  rm(latest.data)
  gc()
}

setwd(dir)
source("webscheme1.R")

#---------------------------------------#
####          Syncronize             ####
#---------------------------------------#

# Update temp files: scraper -> Storage (Update)
if (update != 2) {
  filesync(operation = "ubuntu", freefilesync.dir = "/usr/bin", 
           batchfile = paste("temp", machine, "sto", "ffs_batch", sep = "."))
  filesync(operation = "ubuntu", freefilesync.dir = "/usr/bin", 
           batchfile = paste("rmtemp", machine, "ffs_batch", sep = "."))
}

while (FALSE) {
  # Update temp files: storage -> server (update)
  filesync(operation = "windows", freefilesync.dir = "C:/Program Files/FreeFileSync", 
           batchfile = "temp.sto.ser.ffs_batch")
  # Delete temp files in storage
  filesync(operation = "windows", freefilesync.dir = "C:/Program Files/FreeFileSync", 
           batchfile = "rmtemp.sto.ffs_batch")
  # Merge temp files into final data: on server
  sites = c("dantri", "vnexpress", "laodong", "thanhnien", "vietnamnet")
  for (site in sites) {
    cm_list = link_par(site)
    par = node_par(site, code)
    save_dir = par[["save_dir"]]
    new.data = merge_temp(save_dir, code = unique(cm_list$tencm))
    if (!is.null(new.data)) update_final(x = new.data, site = site)
  }
  
  # Delete temp files in server
  for (site in sites) {
    setwd(paste(dir,"ffs_batch", sep = "/"))
    filesync(operation = "windows", freefilesync.dir = "C:/Program Files/FreeFileSync", 
             batchfile = paste(site, "rm", "temp", "ser", "ffs_batch", sep = "."))
  }
  
  # Update final files: server -> Storage (Mirror)
  filesync(operation = "windows", freefilesync.dir = "C:/Program Files/FreeFileSync", 
           batchfile = "final.ser.sto.ffs_batch")
}

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


