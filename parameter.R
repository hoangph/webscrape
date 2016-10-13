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
  
  # dantri.com.vn
  if (site == "dantri") {
    tencm = c("sukien", "xahoi","giaoduc","kinhdoanh","vanhoa","phapluat","nhipsongtre","suckhoe")
    linkcm = c("http://dantri.com.vn/su-kien/trang-",
               "http://dantri.com.vn/xa-hoi/trang-",
               "http://dantri.com.vn/giao-duc-khuyen-hoc/trang-",
               "http://dantri.com.vn/kinh-doanh/trang-",
               "http://dantri.com.vn/van-hoa/trang-",
               "http://dantri.com.vn/phap-luat/trang-",
               "http://dantri.com.vn/nhip-song-tre/trang-",
               "http://dantri.com.vn/suc-khoe/trang-")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # laodong.com.vn
  if (site == "laodong") {
    tencm = c("thoisuxahoi","phapluat","kinhte","congdoan","suckhoe","vanhoagiaitri","tuvanphapluat")
    linkcm = c("http://laodong.com.vn/thoi-su-xa-hoi/?trang=",
               "http://laodong.com.vn/phap-luat/?trang=",
               "http://laodong.com.vn/kinh-te/?trang=",
               "http://laodong.com.vn/cong-doan/?trang=",
               'http://laodong.com.vn/suc-khoe/?trang=',
               "http://laodong.com.vn/van-hoa-giai-tri/?trang=",
               "http://laodong.com.vn/tu-van-phap-luat/?trang=")
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
  
  # dantri.com.vn
  if (site == "dantri") {
    # Link prefix
    link_prefix = "http://dantri.com.vn"
    # Nodes
    source_suffix = ".htm"
    content_selector = "#divNewsContent p"
    date_selector = ".tt-capitalize"
    article_selector = ".fon6"
    # Save directory
    save_dir = paste(dir,"/dantri",sep="")
  }
  
  #laodong.com.vn
  if (site == "laodong") {
    # Link prefix
    link_prefix = "http://laodong.com.vn"
    # Nodes
    source_suffix = ""
    content_selector = ".content p, .cms-desc"
    date_selector = ".cms-date"
    article_selector = ".hzol-clear .cms-link"
    # Save directory
    save_dir = paste(dir,"/laodong",sep="")
  }
  
  # END #
  return(list(link_prefix = link_prefix,
              source_suffix = source_suffix,
              content_selector=content_selector,
              date_selector=date_selector,
              article_selector=article_selector,
              save_dir=save_dir))
}




