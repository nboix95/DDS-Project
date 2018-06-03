#Instalación de paquetes necesarios
install.packages("iptools")
install.packages("stringr")

#Cargar las librerias necesarias
library(iptools)
library(stringr)

#Asignación variables para working directory y DownloaodedData.
mainDir <- getwd()
subDir <- "DownloadedData"

dir.create(file.path(mainDir, subDir)) #creación carpeta "DownloadedData" al main directory
setwd(paste(mainDir,subDir, sep = '/')) #set del working directori de la carpeta
downloadsDirectory <- getwd() #guardar en la variable downloadsDirectory


fuente <- "http://dns-bh.sagadc.org/" #asignación URL a la variable "fuente"
links1 <- readLines(fuente) #lee cada linea de la "fuente" y la asigna a "links1".

patron <- 'href=\\"([[:alnum:]\\.]*)' #regex para detectar enlaces
links2 <- str_extract(links1,patron) #extraer el resultado en "links2".

links3 <- str_remove(links2,'href=\"') #eliminar "href=\" de links2 y guardar en links3

#iterem pels links de links3 i afegim la URL base per crear URL's completes (pero mal creades)
#links <- sapply(links3, function(link){
#  paste(font, link, sep="")
#})

#iterem pels links de links3 i afegim la URL base per crear URL's completes pero amb 2 barres (//)
#links <- sapply(links3, function(link){
#  paste(font, link, sep="/")
#})

#Iteración para links3: añadir la URL base para que devuelva las URL's completas en formato correcto
full_links <- sapply(links3, function(link){
  paste(fuente, link, sep="")
})

#Búsqueda patrón "txt" en el vector full_links, devolviendo un vector de "True/False"
iters <- grepl(pattern = "txt", x = full_links)

#Guardar los links con "txt" en la variable "full_links_filtered"
full_links_filtered <- full_links[iters]

#Eliminar links que contengan la extensión "zip"
any(grepl("zip", full_links_filtered))

#Iteración para descargar todos los archivos de "full_links_filtered" y guardarlos al working directory
sapply(full_links_filtered, function(link) {
  download.file(url = link, destfile = paste(downloadsDirectory,basename(link),sep = "/") )
})

#Borrar archivos innecesários
file.remove("test.txt")
file.remove("freewebhosts.txt")
file.remove("pushdo.txt")
file.remove("domains.txt")

#Listar archivos descargados a "downlodaded_files"
downloaded_files <- list.files(downloadsDirectory)

#Guardar el primer archivo de downloaded_files en "file2read"
file2read <- downloaded_files[[1]]

#Leer todos los archivos(sapply o lapply) y añadirlos al dataframe
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
dftest<- read.table(paste(getwd(),"20150825.txt", sep = "/"), col.names = c("domain", "date"), stringsAsFactors = F)
##################################################################################################
#####################################################################################################

#Definir la columna de la fecha como tipo fecha para obtener el formato YYYY/MM/D.
dftest$date <- as.Date(as.character(dftest$date), "%Y%m%d") 

#Búsqueda de la IP del hostname, a través de la columna domain del Dataframe, mediante la función hostname_to_ip
dftest$ip <- hostname_to_ip(dftest$domain)

#Para los casos que un dominio tenga más de una IP, guardar solo la primera.
dftest$ip2 <- sapply(dftest$ip, '[[', 1)



#Crea un Data frame con todas las caracteristicas de las IP (rgeolocate)
dfIP1<- ip_api(dftest$ip2, as_data_frame = TRUE, delay = FALSE)
#Eliminamos columnas que no nos interesan y guardamos la nueva data.frame en dfIP
cols.dont.want <- c("as_code", "country_code", "isp", "latitude", "longitude", "organisation", "region_code", "region_name", "timezone", "zip_code")
dfIP <- dfIP1[, ! names(dfIP1) %in% cols.dont.want, drop = F]
#Renombramos la columna de country_name
library(data.table)
setnames(dfIP, old=c("city_name","country_name"), new=c("Cities", "Countries"))

#Unimos data frame: dfIP con dftest en dftest1--> para juntar TODOS LOS DATOS
dftest1 = cbind(dftest, IP)
View (dftest1)

#data frame: dfIP para el mapa
#Mostrar en mapa las IP: rworldmap vignette
library(rworldmap)
par(mai=c(0,0,0.2,0), xaxs="i", yaxs="i")
#Adjuntar los datos al country map
sPDF<- joinCountryData2Map(dfIP,
                           joinCode = "NAME",
                           nameJoinColumn ="Countries" )
#mapping
mapCountryData(sPDF, 
               nameColumnToPlot = "Countries",
               mapTitle = 'Country risk',
               oceanCol = 'lightblue',
               missingCountryCol = 'white')

#Proveedor IP (whois): domaintools-> se tiene que pagar 100 euros...
#install.packages("devtools")
#devtools::install_github("hrbrmstr/domaintools")
#library(domaintools) 

#histograma tiempo--> falta contab. (en proceso)
library(datasets)
hist(dftest1$date)