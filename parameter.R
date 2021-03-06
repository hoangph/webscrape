#-----------------------------#
#     Website parameters      #
#-----------------------------#

#### Links ####
link_par = function(site) {
  
  # Thanhnien.vn
  if (site == "thanhnien") {
    tencm = c("thoisu","doisong","toiviet","giaoduc","kinhdoanh","gioitre","suckhoe","vanhoa")
    linkcm = c("http://thanhnien.vn/thoi-su",
               "http://thanhnien.vn/doi-song",
               "http://thanhnien.vn/toi-viet",
               "http://thanhnien.vn/giao-duc",
               "http://thanhnien.vn/kinh-doanh",
               "http://thanhnien.vn/gioi-tre",
               "http://thanhnien.vn/suc-khoe",
               "http://thanhnien.vn/van-hoa")
    cm_list = data.frame(tencm,linkcm)
    rm(tencm,linkcm)
  }
  
  # Vietnamnet.vn
  if (site == "vietnamnet") {
    tencm = c("thoisu","kinhdoanh","giaoduc","phapluat","doisong","bandoc","batdongsan")
    linkcm = c("http://vietnamnet.vn/vn/date",
               "http://vietnamnet.vn/vn/date",
               "http://vietnamnet.vn/vn/date",
               "http://vietnamnet.vn/vn/date",
               "http://vietnamnet.vn/vn/date",
               "http://vietnamnet.vn/vn/date",
               "http://vietnamnet.vn/vn/date")
    tencm.link = c('thoi-su', "kinh-doanh", "giao-duc", "phap-luat", "doi-song",
                   "ban-doc","bat-dong-san")
    cm_list = data.frame(tencm,linkcm, tencm.link)
    rm(tencm,linkcm)
  }
  
  # Vov.vn
  if (site == "vov") {
    tencm = c("chinhtri", "doisong", "kinhte", "xahoi", "phapluat",
              "vanhoagiaitri", "nguoiviet")
    linkcm = rep("http://vov.vn", length(tencm.link))
    tencm.link = c('chinh-tri', "doi-song", "kinh-te", "xa-hoi", "phap-luat",
                   "van-hoa-giai-tri", 'nguoi-viet')
    cm_list = data.frame(tencm,linkcm, tencm.link)
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

node_par = function(site, cm = NULL) {
  
  # Thanhnien.vn
  if (site == "thanhnien") {
    # Nodes
    source_suffix = ".html"
    source_pagenumber = "trang-"
    source_dateformat = "%Y-%m-%d"
    article_selector = ".title"
    if (cm =="toiviet") {
      content_selector = "#abody div , #chapeau"
    } else {
      content_selector = ".content div div"
    }
    date_selector = ".meta time"
    # Link prefix
    link_prefix = "http://thanhnien.vn"
    link_structure = list("%s/%s/trang-%s%s",
                          "source/i/page/source_suffix")
    webscheme = 3
  }
  
  
  # Vietnamnet.vn
  if (site == "vietnamnet") {
    # Nodes
    source_suffix = "index.html"
    source_pagenumber = ""
    source_dateformat = "%Y%m%d"
    article_selector = ".f-16"
    content_selector = "#ArticleContent p , strong"
    date_selector = ".ArticleDate"
    # Link prefix
    link_prefix = "http://vietnamnet.vn"
    link_structure = "source/i/ten_cm/source_suffix"
    link_structure = list("%s/%s/%s/%s",
                          "source/i/ten_cm/source_suffix")
    webscheme = 2
  }
  
  # vov.vn
  if (site == "vov") {
    # Nodes
    source_suffix = ""
    source_pagenumber = "trang"
    source_dateformat = "%d-%m-%Y"
    article_selector = ".cat-listnews .cms-link"
    content_selector = "#article-body"
    date_selector = "#ctl00_mainContent_ctl00_pnMeta time"
    # Link prefix
    link_prefix = "http://vov.vn"
    link_structure = list("%s/%s/trang%s?bydate=%s",
                          "source/ten_cm/page/i")
    webscheme = 3
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
    webscheme = 1
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
    webscheme = 1
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
    webscheme = 1
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
    webscheme = 1
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
    webscheme = 1
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
    webscheme = 1
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
    webscheme = 1
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
    
    webscheme = 1

  }
  
  # nguoiduatin.vn
  if (site == "nguoiduatin") {
    # Nodes
    source_suffix = ""
    article_selector = ".catPost-list a"
    content_selector = ".art-lead h2 , #main-detail p"
    date_selector = ".datetime"
    # Link prefix
    link_prefix = "http://nguoiduatin.vn"
    webscheme = 1
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
    webscheme = 1
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
    webscheme = 3
  }
  
  # vtc.vn
  if (site == "vtc") {
    # Nodes
    source_suffix = ""
    source_pagenumber = "p"
    source_dateformat = "%d-%m-%Y"
    article_selector = ".title a"
    content_selector = ".single-excerpt , #content_detail p"
    date_selector = ".post-date"
    # Link prefix
    link_prefix = "http://vtc.vn"
    webscheme = 3
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
    article_selector = ".col-sm-12 .col-sm-12 .pull-left , .img3 .pull-left"
    content_selector = "p"
    date_selector = ".icon_date_top"
    # Link prefix
    link_prefix = "http://nhandan.org.vn"
    webscheme = 2
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
    webscheme = 1
  }
  
  # Save directory
  save_dir_prefix = paste(dir, "/", "tempData", sep = "")
  if (!exists("source_pagenumber")) source_pagenumber = ""
  if (!exists("source_dateformat")) source_dateformat = ""
  if (!exists("link_structure")) link_structure = ""
  # END #
  return(list(link_prefix = link_prefix,
              source_suffix = source_suffix,
              source_pagenumber = source_pagenumber,
              source_dateformat = source_dateformat,
              content_selector=content_selector,
              date_selector=date_selector,
              article_selector=article_selector,
              save_dir_prefix=save_dir_prefix,
              link_structure = link_structure,
              webscheme=webscheme))
}


#### Category links ####







