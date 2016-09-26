tencm = c("thoisu","kinhdoanh","giaoduc","congdong")
database = c() 
for (i in c(1:length(tencm))) {
  chuyenmuc = tencm[i]
  pattern = paste(chuyenmuc,"_final",sep="")
  file_index = match(pattern, str_match(list.files(),pattern))
  table = read_csv(list.files()[file_index])
  sub_data = rep(chuyenmuc,nrow(table)) %>% cbind(table)
  database = rbind(database,sub_data)
}

colnames(database) <- c("chuyenmuc","link","title","date","content")

"tham nhung"

#for (i in c(1:length(tencm))) {
#  message(tencm[i],": ",as.character(min(data$date[data$chuyenmuc==tencm[i]])))
#}

keywords = "bi<U+1EC7>t th<U+1EF1>"
thamnhung = match(keywords, str_match(database$title,keywords))

find = str_count(string = sample,pattern = a)

