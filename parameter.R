#-----------------------------#
#     Website parameters      #
#-----------------------------#

#### Links ####
link_par = function(site) {
  
  # Thanhnien.vn
  if (site == "thanhnien") {
    tencm = c("thoisu","doisong","toiviet","giaoduc","kinhdoanh","gioitre","suckhoe","vanhoa")
    linkcm = c("http://thanhnien.vn/thoi-su/date/trang-",
               "http://thanhnien.vn/doi-song/date/trang-",
               "http://thanhnien.vn/toi-viet/date/trang-",
               "http://thanhnien.vn/giao-duc/date/trang-",
               "http://thanhnien.vn/kinh-doanh/date/trang-",
               "http://thanhnien.vn/gioi-tre/date/trang-",
               "http://thanhnien.vn/suc-khoe/date/trang-",
               "http://thanhnien.vn/van-hoa/date/trang-")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # Vietnamnet.vn
  if (site == "vietnamnet") {
    tencm = c("thoisu","kinhdoanh","giaoduc","phapluat","doisong","bandoc")
    linkcm = c("http://vietnamnet.vn/vn/thoi-su/trang",
               "http://vietnamnet.vn/vn/kinh-doanh/trang",
               "http://vietnamnet.vn/vn/giao-duc/trang",
               "http://vietnamnet.vn/vn/phap-luat/trang",
               "http://vietnamnet.vn/vn/doi-song/trang",
               "http://vietnamnet.vn/vn/ban-doc/trang")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  
  # END #
  return(cm_list)
}

#### Nodes and directory ####

node_par = function(site, cm) {
  
  # Thanhnien.vn
  if (site == "thanhnien") {
    # Link prefix
    link_prefix = "http://thanhnien.vn"
    # Nodes
    source_suffix = ".html"
    if (code =="toiviet") {
      content_selector = "strong , #abody div, #chapeau div"
    } else {
      content_selector = ".content div div"
    }
    date_selector = ".meta time"
    article_selector = ".clearfix .title"
    # Save directory
    save_dir = paste(dir,"/thanhnien",sep="")
  }
  
  
  # Vietnamnet.vn
  if (site == "vietnamnet") {
    # Link prefix
    link_prefix = "http://vietnamnet.vn"
    # Nodes
    source_suffix = "/index.html"
    content_selector = "#ArticleContent p , strong"
    date_selector = ".ArticleDate"
    article_selector = ".dotter .f-16"
    # Save directory
    save_dir = paste(dir,"/vietnamnet",sep="")
  }
  
  # END #
  return(list(link_prefix = link_prefix,
              source_suffix = source_suffix,
              content_selector=content_selector,
              date_selector=date_selector,
              article_selector=article_selector,
              save_dir=save_dir))
}




