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
    linkcm = c("http://dantri.com.vn/su-kien/",
               "http://dantri.com.vn/xa-hoi/",
               "http://dantri.com.vn/giao-duc-khuyen-hoc/",
               "http://dantri.com.vn/kinh-doanh/",
               "http://dantri.com.vn/van-hoa/",
               "http://dantri.com.vn/phap-luat/",
               "http://dantri.com.vn/nhip-song-tre/",
               "http://dantri.com.vn/suc-khoe/")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # laodong.com.vn
  if (site == "laodong") {
    tencm = c("thoisuxahoi","phapluat","kinhte","congdoan","suckhoe","vanhoagiaitri")
    linkcm = c("http://laodong.com.vn/thoi-su-xa-hoi/?trang=",
               "http://laodong.com.vn/phap-luat/?trang=",
               "http://laodong.com.vn/kinh-te/?trang=",
               "http://laodong.com.vn/cong-doan/?trang=",
               'http://laodong.com.vn/suc-khoe/?trang=',
               "http://laodong.com.vn/van-hoa-giai-tri/?trang=")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # vnexpress.vn
  if (site == "vnexpress") {
    tencm = c("phapluat","thoisu","kinhdoanh","giaoduc","congdong")
    linkcm = c("http://vnexpress.net/tin-tuc/phap-luat/page/",
               "http://vnexpress.net/tin-tuc/thoi-su/page/",
               "http://kinhdoanh.vnexpress.net/page/",
               "http://vnexpress.net/tin-tuc/giao-duc/page/",
               "http://vnexpress.net/tin-tuc/cong-dong/page/")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # vneconomy.vn
  if (site == "vneconomy") {
    tencm = c("thoisu","taichinh","chungkhoan","doanhnhan","diaoc","thitruong","cuocsongso")
    linkcm = c("http://vneconomy.vn/thoi-su/trang-",
               "http://vneconomy.vn/tai-chinh/trang-",
               "http://vneconomy.vn/chung-khoan/trang-",
               "http://vneconomy.vn/doanh-nhan/trang-",
               "http://vneconomy.vn/bat-dong-san/trang-",
               "http://vneconomy.vn/thi-truong/trang-",
               "http://vneconomy.vn/cuoc-song-so/trang-")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # ndh.vn
  if (site == "ndh") {
    tencm = c("vimo", "taichinh", "doanhnghiep", "chungkhoan", "batdongsan", "hanghoa")
    linkcm = c("http://ndh.vn/vi-mo-c145/trang-",
               "http://ndh.vn/tai-chinh-c149/trang-",
               "http://ndh.vn/doanh-nghiep-c147/trang-",
               "http://ndh.vn/chung-khoan-c146/trang-",
               "http://ndh.vn/bat-dong-san-c148/trang-",
               "http://ndh.vn/hang-hoa-c150/trang-" )
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # baodatviet.vn
  if (site == "baodatviet") {
    tencm = c("chinhtrixahoi","diendantrithuc", "khoahoc", "kinhte", "quocphong", "batdongsan", "phapluat")
    linkcm = c("http://baodatviet.vn/chinh-tri-xa-hoi/?paged=",
               "http://baodatviet.vn/dien-dan-tri-thuc/?paged=",
               "http://baodatviet.vn/khoa-hoc/?paged=",
               "http://baodatviet.vn/kinh-te/?paged=",
               "http://baodatviet.vn/quoc-phong/?paged=",
               "http://baodatviet.vn/bat-dong-san/?paged=",
               "http://baodatviet.vn/phap-luat/?paged="
               )
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # qdnd.vn
  if (site == "qdnd") {
    tencm = c("")
    linkcm = c("http://www.qdnd.vn/chinh-tri/tin-tuc-su-kien/p/",
               "http://www.qdnd.vn/chinh-tri/cac-van-de/p/",
               "http://www.qdnd.vn/chinh-tri/doi-ngoai-doi-ngoai-quoc-phong/p/",
               "http://www.qdnd.vn/chinh-tri/xa-luan/p/",
               "http://www.qdnd.vn/quoc-phong-an-ninh/tin-tuc/p/" )
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # congluan.vn
  if (site == "congluan") {
    tencm = c("thoisu", "diendan", "baochi", "phapluatdieutra", "doisongxahoi", "vanhoa",
              "giaitri", "kinhte", "bandoc")
    linkcm = c("http://congluan.vn/cat/thoi-su/page/",
               "http://congluan.vn/cat/dien-dan/page/",
               "http://congluan.vn/cat/bao-chi/page/",
               "http://congluan.vn/cat/phap-luat-dieu-tra/page/",
               "http://congluan.vn/cat/doi-song-xa-hoi/page/",
               "http://congluan.vn/cat/van-hoa/page/",
               "http://congluan.vn/cat/giai-tri/page/",
               "http://congluan.vn/cat/kinh-te/page/",
               "http://congluan.vn/cat/ban-doc/page/")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # nguoiduatin.vn
  if (site == "nguoiduatin") {
    tencm = c("thoisu", "dachieu", "kinhdoanh", "phapluat", "doison", "congdongmang", "giaitri")
    linkcm = c("http://www.nguoiduatin.vn/c/thoi-su/page/",
               "http://www.nguoiduatin.vn/c/da-chieu/page/",
               "http://www.nguoiduatin.vn/c/kinh-doanh/page/",
               "http://www.nguoiduatin.vn/c/phap-luat/page/",
               "http://www.nguoiduatin.vn/c/doi-song/page/",
               "http://www.nguoiduatin.vn/c/cong-dong-mang/page/",
               "http://www.nguoiduatin.vn/c/giai-tri/page/")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # doanhnhansaigon
  if (site == "doanhnhansaigon") {
    tencm = c("thoisu", "doanhnhan", "kinhdoanh", "thegioiquantri", "diendan", "cohoigiaothuong")
    linkcm = c("http://www.doanhnhansaigon.vn/thoi-su/p",
               "http://www.doanhnhansaigon.vn/doanh-nhan/p",
               "http://www.doanhnhansaigon.vn/kinh-doanh/p",
               "http://www.doanhnhansaigon.vn/the-gioi-quan-tri/p",
               "http://www.doanhnhansaigon.vn/dien-dan-doanh-nhan/p",
               "http://www.doanhnhansaigon.vn/su-kien-doanh-nghiep/p")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # cafef.vn
  if (site == "cafef") {
    tencm = c("thoisu", "chungkhoan", "batdongsan", "doanhnghiep", "taichinhnganhang", "vimo", "hanghoanguyenlieu")
    linkcm = c("http://cafef.vn/thoi-su/",
               "http://cafef.vn/thi-truong-chung-khoan/",
               "http://cafef.vn/bat-dong-san/",
               "http://cafef.vn/doanh-nghiep/",
               "http://cafef.vn/tai-chinh-ngan-hang/",
               "http://cafef.vn/vi-mo-dau-tu/",
               "http://cafef.vn/hang-hoa-nguyen-lieu/" )
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # vtc.vn
  if (site == "vtc") {
    tencm = c("tatca")
    linkcm = c("http://www.vtc.vn/tin-hang-ngay/" )
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # toquoc.vn (nhieu kha nang co doi ten chuyen muc)
  if (site == "toquoc") {
    tencm = c("thoisu", "kinhte", "giaoduc", "suckhoe", "chinhtri", "vietkieu", "phapluatdoisong", "anninh", "gocnhin", "batdongsan",
              "taichinh", "thitruong", "chuyenkinhdoanh", "thoisugiaoduc", "tuyensinh", "vieclam", "thoisuyte")
    linkcm = c("http://toquoc.vn/Thoi_su/?trang=",
               "http://toquoc.vn/kinh-te/?trang=",
               "http://toquoc.vn/giao-duc/?trang=",
               "http://toquoc.vn/y-te/?trang=",
               "http://toquoc.vn/gio-thu-25/?trang=",
               "http://toquoc.vn/Viet-kieu/?trang=",
               "http://toquoc.vn/doi-song/?trang=",
               "http://toquoc.vn/An-ninh-tra-tu/?trang=",
               "http://toquoc.vn/goc-nhin/?trang=",
               "http://toquoc.vn/dia-oc/?trang=",
               "http://toquoc.vn/tai-chinh-chung-khoan/?trang=",
               "http://toquoc.vn/kinh-te-viet-nam/?trang=",
               "http://toquoc.vn/kinh-te-the-gioi/?trang=",
               "http://toquoc.vn/thoi-su-giao-duc/?trang=",
               "http://toquoc.vn/dau-tu-giao-duc/?trang=",
               "http://toquoc.vn/giao-duc-va-viec-lam/?trang=",
               "http://toquoc.vn/thoi-su-y-te/?trang=")
               
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # nhandan.org.vn
  if (site == "nhandan") {
    tencm = c("chinhtri", "kinhte", "vanhoa", "xahoi", "khoahoc", "giaoduc", "suckhoe", "phapluat",
              "hanoi", "tphcm")
    linkcm = c("http://www.nhandan.com.vn/chinhtri?fromdate=",
               "http://www.nhandan.com.vn/kinhte?fromdate=",
               "http://www.nhandan.com.vn/vanhoa?fromdate=",
               "http://www.nhandan.com.vn/xahoi?fromdate=",
               "http://www.nhandan.com.vn/khoahoc?fromdate=",
               "http://www.nhandan.com.vn/giaoduc?fromdate=",
               "http://www.nhandan.com.vn/suckhoe?fromdate=",
               "http://www.nhandan.com.vn/phapluat?fromdate=",
               "http://www.nhandan.com.vn/hanoi?fromdate=",
               "http://www.nhandan.com.vn/tphcm?fromdate="
               )
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # baophapluat
  if (site == "baophapluat") {
    tencm = c('thoisu', 'kinhte', 'phapluat', 'xahoi', 'bandoc', 'suckhoe', 'nhipsonghomnay')
    linkcm = c('http://baophapluat.vn/chinh-tri/?page=',
               'http://baophapluat.vn/kinh-te/?page=',
               'http://baophapluat.vn/xa-lo-phap-luat/?page=',
               'http://baophapluat.vn/xa-hoi/?page=',
               'http://baophapluat.vn/ban-doc/?page=',
               'http://baophapluat.vn/song-khoe/?page=',
               'http://baophapluat.vn/nhip-song-hom-nay/?page=')
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
    if (cm =="toiviet") {
      content_selector = "strong , #abody div, #chapeau div"
    } else {
      content_selector = ".content div div"
    }
    date_selector = ".meta time"
    article_selector = ".clearfix .title"

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

  }
  
  # dantri.com.vn
  if (site == "dantri") {
    # Link prefix
    link_prefix = "http://dantri.com.vn"
    # Nodes
    source_suffix = ".htm"
    content_selector = "#divNewsContent, .sapo"
    date_selector = ".tt-capitalize"
    article_selector = ".fon6"

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
  }
  
  # vnexpress
  if (site == "vnexpress") {
    # Link prefix
    link_prefix = ""
    # Nodes
    source_suffix = ".html"
    content_selector = ".short_intro , .Normal"
    date_selector = ".block_timer"
    article_selector = "#news_home .txt_link"
  }
  
  # vneconomy
  if (site == "vneconomy") {
    # Nodes
    source_suffix = ".htm"
    article_selector = ".flie .titletopmid2 , .titletopfooter, .ultopmidtitle, .hdlcmtitlel"
    content_selector = ".detailsbaiviet , strong"
    date_selector = ".timverbvvth"
    # Link prefix
    link_prefix = "http://vneconomy.vn"
  }
  
  # ndh
  if (site == "ndh") {
    # Nodes
    source_suffix = ".htm"
    article_selector = "#listNews div.tit-story h2 a"
    content_selector = ".shapo-detail , .main-detail p"
    date_selector = ".nav-detail .sub-time"
    # Link prefix
    link_prefix = "http://ndh.vn"
  }
  
  # baodatviet.vn
  if (site == "baodatviet") {
    # Nodes
    source_suffix = ""
    article_selector = "#left_col .title a"
    content_selector = ".detail p , .Normal, .lead"
    date_selector = ".time"
    # Link prefix
    link_prefix = "http://baodatviet.vn"
  }
  
  # qdnd.vn
  if (site == "qdnd") {
    # Nodes
    source_suffix = ""
    article_selector = ".h3cate"
    content_selector = "#dnn_VIEWSINDEX_ctl00_viewhinone h2, .post-content div"
    date_selector = ".post-subinfo"
    # Link prefix
    link_prefix = "http://qdnd.vn"
  }
  
  # congluan.vn
  if (site == "congluan") {
    # Nodes
    source_suffix = ""
    article_selector = ".entry-title a"
    content_selector = "p"
    date_selector = ".date-published"
    # Link prefix
    link_prefix = "http://congluan.vn"
  }
  # Save directory
  save_dir = paste(dir, "/", site, "/tempData", sep = "")
  
  # nguoiduatin.vn
  if (site == "nguoiduatin") {
    # Nodes
    source_suffix = ""
    article_selector = ".catPost-list a"
    content_selector = ".art-lead h2 , #main-detail p"
    date_selector = ".datetime"
    # Link prefix
    link_prefix = "http://nguoiduatin.vn"
  }
  
  # doanhnhansaigon.vn
  if (site == "doanhnhansaigon") {
    # Nodes
    source_suffix = ""
    article_selector = ".category-wnews a"
    content_selector = "#ar-content-html"
    date_selector = ".date"
    # Link prefix
    link_prefix = "http://doanhnhansaigon.vn"
  }
  
  # cafef.vn
  if (site == "cafef") {
    # Nodes
    source_suffix = ".chn"
    source_pagenumber = "trang-"
    source_dateformat = "%d/%m/%Y"
    article_selector = "h3 a"
    content_selector = ".sapo, .newsbody p"
    date_selector = ".date_zoom .date"
    # Link prefix
    link_prefix = "http://cafef.vn"
  }
  
  # vtc.vn
  if (site == "vtc") {
    # Nodes
    source_suffix = ""
    source_pagenumber = "p"
    source_dateformat = "%d-%m-%Y"
    article_selector = ".title_list_news_cate"
    content_selector = "#content_detail > p, #content_detail > h2, #content_detail > div> font > b"
    date_selector = ".time_detail a"
    # Link prefix
    link_prefix = "http://vtc.vn"
  }
  
  # toquoc.vn
  if (site == "toquoc") {
    # Nodes
    source_suffix = ""
    source_pagenumber = ""
    source_dateformat = ""
    article_selector = ".info_cate .cms-link"
    content_selector = "#cotent_detail span, #cotent_detail p, .cms-desc div, #cotent_detail div"
    date_selector = ".fontOP"
    # Link prefix
    link_prefix = "http://toquoc.vn"
  }
  
  # nhandan.org.vn
  if (site == "nhandan") {
    # Nodes
    source_suffix = ""
    source_pagenumber = ""
    source_dateformat = "%d-%m-%Y"
    article_selector = ".media-heading .pull-left"
    content_selector = "p"
    date_selector = ".icon_date_top"
    # Link prefix
    link_prefix = "http://nhandan.org.vn"
  }
  
  # baophapluat
  if (site == "baophapluat") {
    # Nodes
    source_suffix = ""
    source_pagenumber = "" # for loop by date and page
    source_dateformat = "" #for loop by date
    article_selector = ".acolumn a:nth-child(2)"
    content_selector = "em , #ctl00_mainContent_abody p, .cms-desc"
    date_selector = ".head-post span"
    # Link prefix
    link_prefix = "http://baophapluat.vn"
  }
  
  # Save directory
  save_dir = paste(dir, "/", "/tempData", sep = "")
  if (!exists("source_pagenumber")) source_pagenumber = ""
  if (!exists("source_dateformat")) source_dateformat = ""
  # END #
  return(list(link_prefix = link_prefix,
              source_suffix = source_suffix,
              source_pagenumber = source_pagenumber,
              source_dateformat = source_dateformat,
              content_selector=content_selector,
              date_selector=date_selector,
              article_selector=article_selector,
              save_dir=save_dir))
}


#### Category links ####







