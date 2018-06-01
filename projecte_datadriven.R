#instala paquets necessaris
install.packages("iptools")
install.packages("stringr")

#carrega les llibreries necessaries en el workspace
library(iptools)
library(stringr)

#agafa el working directory i el fica a "mainDir" i el subdirectori DownloadedData a "subDir"
mainDir <- getwd()
subDir <- "DownloadedData"

#crea la carpeta downloadeddata al main directory, seteja el working directory a aquest i ho posa a la variable "downloadsdirectory"
dir.create(file.path(mainDir, subDir))
setwd(paste(mainDir,subDir, sep = '/'))
downloadsDirectory <- getwd()

#fica a "font" la URL base i la descarrega a "links"
font <- "http://dns-bh.sagadc.org/"
links <- readLines(font)

#fica a "patro" una regex que detecta enllasos i els extreu a "links2"
patro <- 'href=\\"([[:alnum:]\\.]*)'
links2 <- str_extract(links,patro)

#treiem "href=\" dels links i els fiquem a links3
links3 <- str_remove(links2,'href=\"')

#iterem pels links de links3 i afegim la URL base per crear URL's completes (pero mal creades)
#linksss <- sapply(links3, function(link){
#  paste(font, link, sep="")
#})

#iterem pels links de links3 i afegim la URL base per crear URL's completes pero amb 2 barres (//)
#linksss <- sapply(links3, function(link){
#  paste(font, link, sep="/")
#})

#iterem pels links de links3 i afegim la URL base per crear URL's completes correctes
full_links <- sapply(links3, function(link){
  paste(font, link, sep="")
})

#busquem el patro "txt" al vector full_links i creem un vector de "True" i "False"
iters <- grepl(pattern = "txt", x = full_links)

#fiquem a "full_links_filtered" els links que tenien un "txt"
full_links_filtered <- full_links[iters]

#Treiem qualsevol link que tingui les paraules .zip
any(grepl("zip", full_links_filtered))

#iterem per "full_links_filtered" i descarreguem tots els arxius al working directory
sapply(full_links_filtered, function(link) {
  download.file(url = link, destfile = paste(downloadsDirectory,basename(link),sep = "/") )
})

#esborrem arxius innecessaris
file.remove("test.txt")
file.remove("freewebhosts.txt")
file.remove("pushdo.txt")
file.remove("domains.txt")

#listem els arxius descarregats a "downloaded_files"
downloaded_files <- list.files(downloadsDirectory)

#inicialitzo "file2read" amb el primer arxiu de la llista
file2read <- downloaded_files[[1]]

#aqui hauriem de llegir tots els arxius amb un sapply o lapply i ficarlos tots al dataframe
##################################################################################
#################AREA DE PROVES###################################################
sapply(downloaded_files, function(file2read){
  df2 <-read.csv(file = paste(downloadsDirectory,file2read, sep = "/"), sep = "", header = FALSE, fill = TRUE, col.names = c("domain", "date", "type", "origin", "1", "2", "3", "4"), stringsAsFactors = F)
})

lapply(downloaded_files, function(file2read){
  full_name_file <- paste(downloadsDirectory, file2read, sep = "/")
  df2 <- read.csv(file = full_name_file, sep = "", header = FALSE, fill = TRUE, col.names = c("domain", "date", "type", "origin", "1", "2", "3"), stringsAsFactors = F)
})

downloaded_files_full <- paste(downloadsDirectory,downloaded_files,sep = "")
df3 <- lapply(file = downloaded_files_full, read.csv(), sep = "", header = FALSE, fill = TRUE, col.names = c("domain", "date", "type", "origin", stringsAsFactors = F))


sapply(downloaded_files, function(file2read){
  full_name_file <- paste(downloadsDirectory, file2read, sep = "/")
  df2 <- read.csv(file = full_name_file, sep = "", header = FALSE, fill = TRUE, col.names = c("domain", "date", "type", "origin"), stringsAsFactors = F) 
})

df2 <-read.csv(file = paste(downloadsDirectory,file2read, sep = "/"), sep = "", header = FALSE, fill = TRUE, col.names = c("domain", "date", "type", "origin", "1", "2", "3", "4"), stringsAsFactors = F)

?read.csv
df <- read.table(paste(getwd(),"20150825.txt", sep = "/"), col.names = c("domain", "date"), stringsAsFactors = F)
##################################################################################################
#####################################################################################################

#defineix la columna de la data com a tipus data
df$date <- as.Date(as.character(df$date), "%Y%m%d") 

#busca la IP del hostname cridant a la funcio hostname_to_ip amb la columna domain del dataframe
df$ip <- hostname_to_ip(df$domain)

#deixa nomes la primera IP de cada domini si hi ha mes de una
df$ip2 <- sapply(df$ip, '[[', 1)
