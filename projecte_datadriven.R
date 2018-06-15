#Instalacion paquetes necesarios.
install.packages("iptools")
install.packages("stringr")

#Cargar las librerias necesarias
library(iptools)
library(stringr)

#Asignacion variables para working directory y DownloaodedData.
mainDir <- getwd()
subDir <- "DownloadedData"

dir.create(file.path(mainDir, subDir)) #creacion carpeta "DownloadedData" al main directory
setwd(paste(mainDir,subDir, sep = '/')) #set del working directori de la carpeta
downloadsDirectory <- getwd() #guardar en la variable downloadsDirectory


fuente <- "http://dns-bh.sagadc.org/" #asignacion URL a la variable "fuente"
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

#Iteracion para links3: añadir la URL base para que devuelva las URL's completas en formato correcto
full_links <- sapply(links3, function(link){
  paste(fuente, link, sep="")
})

#Busqueda patron "txt" en el vector full_links, devolviendo un vector de "True/False"
iters <- grepl(pattern = "txt", x = full_links)

#Guardar los links con "txt" en la variable "full_links_filtered"
full_links_filtered <- full_links[iters]

#Eliminar links que contengan la extension "zip"
any(grepl("zip", full_links_filtered))

#Iteracion para descargar todos los archivos de "full_links_filtered" y guardarlos al working directory
sapply(full_links_filtered, function(link) {
  download.file(url = link, destfile = paste(downloadsDirectory,basename(link),sep = "/") )
})

#Borrar archivos innecesarios
file.remove("test.txt")
file.remove("freewebhosts.txt")
file.remove("pushdo.txt")
file.remove("domains.txt")

#Listar archivos descargados a "downlodaded_files"
downloaded_files <- list.files(downloadsDirectory)

#Guardar el primer archivo de downloaded_files en "file2read"
file2read <- downloaded_files[[1]]

#Combina todos los archivos en un mismo dataframe (7 columnas)
mainDataFrame <- do.call("rbind", lapply(downloaded_files, function(file2read){
  read.csv(file = paste(downloadsDirectory, file2read, sep = "/"), sep = "", header = F, fill = T, stringsAsFactors = F, col.names = c("domain", "type", "origin", "date", "1", "2", "3"))
}))

#Coger la ultima columna (sin N/A) y la añade a Data Frame
mainDataFrame$date <- apply(mainDataFrame, 1, function(x) tail(na.omit(x), 1))

#Definir la columna del data frame como tipo data
mainDataFrame$date <- as.Date(as.character(mainDataFrame$date), "%Y%m%d") 

#Borrar las columnas restantes
mainDataFrame[5:7] <- list(NULL)

#Borrar lineas mal formateadas buscando un "origin" o "dato" vacio
mainDataFrame <- mainDataFrame[!(mainDataFrame$origin==""),]
mainDataFrame <- mainDataFrame[!(is.na(mainDataFrame$date)),]

#Buscar la IP del hostname (columna domain del dataframe) usando la función hostname_to_ip
mainDataFrame$ip <- hostname_to_ip(mainDataFrame$domain)
totalIP <- as.data.frame(hostname_to_ip(mainDataFrame$domain))

#En caso de que haya más de una IP, dejar la primera
mainDataFrame$ip <- sapply(mainDataFrame$ip, '[[', 1)

#Guardar el dataframe, en un archivo local
save(mainDataFrame,file = "mainDataframe.Rda")

testframe <- mainDataFrame[dim(mainDataFrame)[1]:1,]
save(testframe, file = "testframe.Rda")


#Sacar las filas que tengan IP not resolved y guardarlo en otro dataframe
mainDataFrame1<- subset(mainDataFrame, ip != "Not resolved")

#Crea un Data frame con todas las caracteristicas de las IP (rgeolocate)
install.packages("rgeolocate")
library(rgeolocate)

file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
results <- maxmind(mainDataFrame1$ip, file, "country_name")

#Renombramos la columna de country_name
install.packages(data.table)
library(data.table)
setnames(results, old="country_name", new="Countries")

#Unimos data frame: dfIP con dftest en dftest1--> para juntar TODOS LOS DATOS
mainDataFrameIP = cbind(mainDataFrame1, results)
View (mainDataFrameIP)

#Eliminar filas con valor NA
mainDFIP<-na.omit(mainDataFrameIP)

#data frame: mainDFIP para el mapa
#Mostrar en mapa las IP: rworldmap vignette
library(rworldmap)
par(mai=c(0,0,0.2,0), xaxs="i", yaxs="i")
#Adjuntar los datos al country map
sPDF<- joinCountryData2Map(mainDFIP,
                          joinCode = "NAME",
                          nameJoinColumn ="Countries")
#mapping
mapCountryData(sPDF, 
               mapTitle = 'Country risk',
               oceanCol = 'lightblue',
               missingCountryCol = 'white')

#Proveedor IP (whois): domaintools-> se tiene que pagar 100 euros...
#install.packages("devtools")
#devtools::install_github("hrbrmstr/domaintools")
#library(domaintools) 

#Contar num de veces que se repite cada país
install.packages("plyr")
library(plyr)
resultsIP<-na.omit(results)
x = count(resultsIP, 'Countries')

#Contar cuantas webs vulneradas hay por mes y año
fechas=as.Date(mainDataFrame$date)
Año <-format(fechas,"%Y%m")
xtiempo <- as.data.frame(Año)
dfyearmonth = count(xtiempo, "Año")



#Contar frecuencia de tiempo
tiempo = count(mainDataFrame, 'date')
#Histograma tiempo
#library(datasets)
#hist(tiempo$freq)

#Histograma tiempo con estimador de núcleo de la densidad
library(datasets)
hist(dfyearmonth$freq,freq=FALSE,col="lightcyan", ylim=c(0,0.0003),main="Histograma de ataques",xlab="")
lines(density(dfyearmonth$freq),col="red",lwd=2)

#hist(tiempo$freq)
