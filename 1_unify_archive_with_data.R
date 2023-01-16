# Cambia nombres de spicesLink originales
# Trabaja sobre los archivos originales y genera unos modificados

library(xlsx)
library(data.table)
library(tibble)
library(stringr)

dir<-"G:/Cristian_data/Humboldt/Set16/datos_originales/"

nombres <- read.xlsx("G:/Cristian_data/Humboldt/Set16/datos_originales/Indicaciones-Mapeos.xlsx", as.data.frame = T, 
                     sheetIndex = 2, header = FALSE, colClasses = 'character')

######################
######### GBIF###########
### 
## Cargar datos mapeo para contrastar con los del formato LBAB
outDir_gbif <- 'G:/Cristian_data/Humboldt/Set16/datos_originales/GBIF&SiB/raw/Descarga_2022_01_13_directGBIF/'
gbif<-fread ("G:/Cristian_data/Humboldt/Set16/datos_originales/GBIF&SiB/raw/Descarga_2022_01_13_directGBIF/0112920-210914110416597/filteredOcc2.txt", encoding = 'UTF-8')

nombres.current2 <- nombres[grep('GBIF', nombres[, 1]), -1]
#na.cols <- is.na(nombres.current2[1, ])
nombres.current2 <- nombres.current2[, !is.na(nombres.current2[1, ])]
oriColnames2 <- c(t(nombres.current2[grep('originales', nombres.current2[, 1]), -1]))
finColnames2 <- c(t(nombres.current2[grep('set', nombres.current2[, 1]), -1]))
oriColnames3<-oriColnames2[which(!is.na(finColnames2))]#columnas que conciden 
finColnames3<-finColnames2[which(!is.na(finColnames2))]#columnas que conciden 

gbif<-as.data.frame(gbif)
gb.i<-gbif[,c(oriColnames3)]# acotar columnas fuente a columnas del formato
colnames(gb.i)<- finColnames3

gbif<-gb.i


### si se descargo la matriz simple usar estas lineas
#gbif2<-gbif[,c(3, 30, 37,38, 36, 39, 45, 16, 18, 17, 22, 23, 24, 26, 13)]
#gbif2$county<- 'no_information'
#gbif<-gbif2[, c(1:7, 16, 8, 9, 17, 10:15)]

write.csv(gbif, paste0(outDir_gbif,'filteredOcc.csv'), row.names = F)

######################
##### SpeciesLink
#######################
bd<-"speciesLink"
setwd(paste0(dir,bd, "/raw/2022-01-13/"))

archivos <- list.files(pattern = ".txt$")
archivos <- archivos[order(file.size(archivos))]
outDir <- "G:/Cristian_data/Humboldt/Set16/datos_originales/speciesLink/mapped/"
# spLink <- read.delim(archivos[i], encoding = 'UTF-8') # "PANAMA_speciesLink_all_16296_5166registros.txt")
# head(spLink)

## Nombres de archivos en tabla con nombres para set16
nombres.current <- nombres[grep('speciesLink', nombres[, 1]), -1]
na.cols <- is.na(nombres.current[1, ])
nombres.current <- nombres.current[, !na.cols ]
oriColnames <- c(t(nombres.current[grep('originales', nombres.current[, 1]), -1]))
finColnames <- c(t(nombres.current[grep('set', nombres.current[, 1]), -1]))
oriColnames[which(!is.na(finColnames))]
finColnames[which(!is.na(finColnames))]

un <- NULL
i <- 1
for (i in 1:length(archivos)){
  archivo.i <- read.delim(archivos[i], encoding = 'UTF-8', colClasses = 'character', stringsAsFactors = F)
  archivo.i$date <- paste(archivo.i$yearcollected, archivo.i$monthcollected, archivo.i$daycollecte, sep = '-')
  archivo.i <- archivo.i[, c(oriColnames[which(!is.na(finColnames))], 'date')]
  colnames(archivo.i) <- c(finColnames[which(!is.na(finColnames))], 'latestDateCollected')
  archivo.i$resourceName <- archivos[i]
  archivo.i$resourceFolder <- paste0(getwd(), '/mapped')
  archivo.i$resourceIncorporationDate <- Sys.Date()
  un <- rbind(un, archivo.i)
  cat(i)
}

un$speciesOriginal <- un$species
#rm(list.files(path = 'C:/IAvH/DINAVIS_set16/datos_originales/speciesLink/', pattern = '.txt'))
write.csv(un, paste0(outDir,'/speciesLink_', Sys.Date(), '.csv'), row.names = FALSE)
save(un, file = paste0(outDir,'/speciesLink_', Sys.Date(), '.RData'))


#####

#############
### eBird ###
#############


dir <- "eBird"
setwd("G:/Cristian_data/Humboldt/Set16/datos_originales/eBird/raw/2022-01-20/")

list<-list.files(pattern = '.zip')
i<-1
for (i in 1:length(list)) {
  unzip(zipfile = list[i])
}

archivos2 <- list.files(pattern = "ebd.*.2021.txt$", recursive = T)
outDir <- "G:/Cristian_data/Humboldt/Set16/datos_originales/eBird/mapped/"

## Cargar datos mapeo para contrastar con los del formato LBAB

nombres.current2 <- nombres[grep('eBird', nombres[, 1]), -1]
nombres.current2 <- nombres.current2[, !(is.na(nombres.current2[2, ])) ]
oriColnames2 <- c(t(nombres.current2[grep('originales', nombres.current2[, 1]), -1]))
finColnames2 <- c(t(nombres.current2[grep('set', nombres.current2[, 1]), -1]))
oriColnames3<-oriColnames2[which(!is.na(finColnames2))]#columnas que conciden 
finColnames3<-finColnames2[which(!is.na(finColnames2))]#columnas que conciden 

 
approv2 <- un2 <- NULL

i <- 1
for (i in 1:length(archivos2)){
  cat('procesing', archivos2[i], 'file\n' )
  archivo.i <- read.delim(archivos2[i])
  colnames(archivo.i) <- gsub("//."," ",colnames(archivo.i))
  approv2.i <- archivo.i[, c('GLOBAL.UNIQUE.IDENTIFIER', 'APPROVED', 'REVIEWED', 'REASON', 'CATEGORY')]
  archivo.i$collector <- 'no_collector_information'
  #archivo.i <- archivo.i[, c(oriColnames2[which(!is.na(finColnames2))], 'collector', 'APPROVED')]
  #colnames(archivo.i) <- c(finColnames2[which(!is.na(finColnames2))], 'colector', 'APPROVED')
  archivo.i$resourceName <- archivos2[i]
  archivo.i$resourceFolder <- paste0(getwd(), '/mapped')
  archivo.i$resourceIncorporationDate <- Sys.Date()
  un2 <- rbind(un2, archivo.i)
  approv2.i$resourceName <- archivo.i$resourceName
  approv2 <- rbind(approv2, approv2.i)
  cat('done \n')
}

colnames(un2)<-gsub("\\.", " ", colnames(un2))## remove dot to colnames
eb.i<-un2[,c(oriColnames3)]# acotar columnas fuente a columnas del formato
colnames(eb.i)<- finColnames3

eb.i$locality <- stringi::stri_encode(eb.i$locality, 'UTF-8') # Ajustar codificación localidad 
eb.i$adm1 <- stringi::stri_encode(eb.i$adm1, 'UTF-8') # Ajustar codificación departamento 
eb.i$adm2 <- stringi::stri_encode(eb.i$adm2, 'UTF-8') # Ajustar codificación municipio 

un2<-cbind(eb.i, un2[,c(49:51)]) #unir columnas loop, con variables ajustadas 

un2$species <- un2$speciesOriginal

date<- as.data.frame(do.call(rbind, str_split(un2$earliestDateCollected, '-')))
names(date)<- c('year', 'month', 'day')
un2<-cbind(un2, date) #unir columnas loop, con variables ajustadas 

keep <- which(approv2$CATEGORY == 'species' & approv2$APPROVED == 1)
un3<- un2[keep,]

write.csv(un3, paste0(outDir,'/eBird_', Sys.Date(), '.csv'), row.names = FALSE)
ebird <- un3
save(ebird, file = paste0(outDir,'eBird_', Sys.Date(), '.RData'))
write.csv(approv2, paste0(outDir,'/approv2edCols_', Sys.Date(), '.csv'), row.names = FALSE)

#write.table(approv2ed, paste0(outDir,'/eBird_', Sys.Date(), '_justapprov2EDfield.txt'), row.names = FALSE)
#notApp <- approv2ed[which(approv2ed$approv2ED == 0), ]
#write.table(notApp, paste0(outDir,'/eBird_', Sys.Date(), '_justNOT_approv2EDfield.txt'), row.names = FALSE)

########################
### Colecciones IAvH ###
#######################

bd <- "Colecciones_IAvH"
setwd(paste0(dir,bd, "/raw/2022/"))
outDir <- "G:/Cristian_data/Humboldt/Set16/datos_originales/Colecciones_IAvH/mapped/"

archivos<- list.files(full.names = T, pattern = '.csv')
x<-NULL

nombres.current2 <- nombres[grep('Colecciones', nombres[, 1]), -1]
nombres.current2 <- nombres.current2[, !(is.na(nombres.current2[2, ])) ]
oriColnames2 <- c(t(nombres.current2[grep('originales', nombres.current2[, 1]), -1]))
finColnames2 <- c(t(nombres.current2[grep('set', nombres.current2[, 1]), -1]))
oriColnames3 <-oriColnames2[which(!is.na(finColnames2))]#columnas que conciden 
finColnames3 <-finColnames2[which(!is.na(finColnames2))]#columnas que conciden 

x<-NULL


for (i in 1:length(archivos)) {
    cat('procesing', archivos[i], 'data \n')
    archivo.i <- read.csv(archivos[i], header = T, sep = ';', encoding = 'UTF-8')
    archivo.i <- archivo.i[,oriColnames3]
    archivo.i$resourceName <- archivos[i]
    archivo.i$resourceFolder <- paste0(getwd(), '/mapped')
    archivo.i$resourceIncorporationDate <- Sys.Date()
    x<-rbind(x, archivo.i)
}


colnames(x)<-finColnames3
date<- as.data.frame(do.call(rbind, str_split(x$eventDate, '-')))
names(date)<- c('year', 'month', 'day')
x<-cbind(x, date) #unir columnas loop, con variables ajustadas 

write.csv(x, paste0(outDir,'/colecciones_', Sys.Date(), '.csv'), row.names = FALSE)
colecciones <- x
save(colecciones, file = paste0(outDir,'coleciones_', Sys.Date(), '.RData'))
#load(paste0(outDir,'coleciones_', Sys.Date(), '.RData'))

str(colecciones)
