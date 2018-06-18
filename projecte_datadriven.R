#Instalacion paquetes necesarios.
install.packages("iptools")
install.packages("stringr")
install.packages("rgeolocate")
install.packages("data.table")
install.packages("plyr")
install.packages("ggplot2")


#Cargar las librerias necesarias
library(iptools)
library(stringr)
library(rgeolocate)
library(data.table)
library(plyr)
library(datasets)
library(ggplot2)

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

#En caso de que haya más de una IP, dejar la primera
mainDataFrame$ip <- sapply(mainDataFrame$ip, '[[', 1)

#Guardar el dataframe, en un archivo local
save(mainDataFrame,file = "mainDataframe.Rda")

testframe <- mainDataFrame[dim(mainDataFrame)[1]:1,]
save(testframe, file = "testframe.Rda")


#Sacar las filas que tengan IP not resolved y guardarlo en otro dataframe
mainDataFrame1<- subset(mainDataFrame, ip != "Not resolved")

#Crea un Data frame con todos los países de las IP's (rgeolocate)
file <- system.file("extdata","GeoLite2-Country.mmdb", package = "rgeolocate")
results <- maxmind(mainDataFrame1$ip, file, "country_name")

#Renombramos la columna de country_name
setnames(results, old="country_name", new="Countries")

#Unimos data frame "results" con mainDataFrame1, siendo el resultado: mainDataFrameIP
mainDataFrameIP = cbind(mainDataFrame1, results)
View (mainDataFrameIP)

#Eliminar filas con valor NA
mainDFIP<-na.omit(mainDataFrameIP) #data frame mainDFIP: para el mapa

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

#Contar número de veces que se repite cada país
resultsIP<-na.omit(results)
x = count(resultsIP, 'Countries')

#Contar cuantas webs vulneradas hay por mes de cada año
fechas=as.Date(mainDataFrame$date)
Año <-format(fechas,"%Y%m")
xtiempo <- as.data.frame(Año)
dfyearmonth = count(xtiempo, "Año")

#Contar frecuencia de tiempo (por día)
tiempo = count(mainDataFrame, 'date')

#tabla
barplot(prop.table(table(mainDataFrame$date)), main = "Histograma de ataques diarios 2013-2018", ylim = c(0,0.012))

#domaintools | extracción los proveedores de las IP's -> todas son de pago

#10 primeros paises de maximas webs vulneradas 
diezpaises <- subset(x, freq>100)
diezpaises #se observa que Estados Unidos y Reino Unido son los paises más afectados

#Grafico que muestra la cantidad de webs vulneradas en los 10 primeros paises
install.packages("rlang")
library(rlang)
install.packages("plotly")
library(plotly)
devtools::install_github('hadley/ggplot2')

.rs.restartR() #para restaurar la sesión

p <- ggplot(data=diezpaises, aes(x=Countries, y=freq)) +
  geom_bar(stat="identity")

p <- ggplotly(p)
p #muestra grafico
