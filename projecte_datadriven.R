install.packages("iptools")
install.packages("stringr")

library(iptools)
library(stringr)

mainDir <- getwd()
subDir <- "DownloadedData"

dir.create(file.path(mainDir, subDir))
setwd(paste(mainDir,subDir, sep = '/'))
downloadsDirectory <- getwd()

font <- "http://dns-bh.sagadc.org/"
links <- readLines(font)

patro <- 'href=\\"([[:alnum:]\\.]*)'
links2 <- str_extract(links,patro)

links3 <- str_remove(links2,'href=\"')

linksss <- sapply(links3, function(link){
  paste(font, link, sep="")
})
linksss <- sapply(links3, function(link){
  paste(font, link, sep="/")
})

full_links <- sapply(links3, function(link){
  paste(font, link, sep="")
})

iters <- grepl(pattern = "txt", x = full_links)

full_links_filtered <- full_links[iters]

any(grepl("zip", full_links_filtered))

sapply(full_links_filtered, function(link) {
  download.file(url = link, destfile = paste(downloadsDirectory,basename(link),sep = "/") )
})
file.remove("test.txt")
file.remove("freewebhosts.txt")
file.remove("pushdo.txt")
file.remove("domains.txt")

downloaded_files <- list.files(downloadsDirectory)
file2read <- downloaded_files[[1]]

sapply(downloaded_files, function(file2read){
  df <- read.table(paste(downloadsDirectory,file2read, sep = "/"), col.names = c("domain", "date"), stringsAsFactors = F)
})


df <- read.table(paste(getwd(),"20150825.txt", sep = "/"), col.names = c("domain", "date"), stringsAsFactors = F)
df$date <- as.Date(as.character(df$date), "%Y%m%d") 


df$ip <- hostname_to_ip(df$domain)
df$ip2 <- sapply(df$ip, '[[', 1)
