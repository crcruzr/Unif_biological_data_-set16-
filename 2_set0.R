#     Cargar archivos con registros de fuentes varias junto a los descargados del GBIF y SiB
#     y ponerlos en el formato en los que vienen los registros de esas ultimas fuentes.
#     El formato es el que tendra el 'set16', objeto con el cual se realiza la modelacisn de 
#     distribucisn de especies. Cada fuente de datos dara logar a un objeto .RData 'set0' que se almacenara
#     en 'datos_otros'. Posteriormente estos 'set0' seran unidos para conformar el set1.RData
# 
#     Los archivos a ser cargados y convertidos en el set0 deben tener los titulos de los campos que sean comunes a la
#     informacisn del set16 del mismo modo que los tiene este objeto. 
# 
#     El csdigo aplica para los datos provenientes de:
#       - GBIF (1 archivo .RData)
#       - eBird (5 archivos .RData)
#       - speciesLink (1 archivo .RData)

# load ---------------------------------------------------------------
## Cargar funciones, librer[i]as y mapas
library(rgdal)
library(maptools)
library(terra)
library(xlsx)
library(data.table)

  
source("G:/Cristian_data/Humboldt/Set16/scripts/tools.R")
data(wrld_simpl)
aoi <- readOGR('G:/Cristian_data/Humboldt/Set16/maps/version2', 'aoi')

## Establecer ruta de trabajo
setwd("G:/Cristian_data/Humboldt/Set16/datos_originales/")
workDir <- getwd()

# Cargar la tabla que tiene la estructura de los registros del GBIF
format.datos <- read.csv("estructuraset16.csv")
format.datos$year<-NA
format.datos$month<-NA
format.datos$day<-NA

#format.datos <- read.csv("C:/IAvH/DINAVIS_set16/datos_originales/estructuraset16.csv")
emptyFormat <- format.datos[-(1:nrow(format.datos)), ]

resumen <- NULL
dir2 <- "G:/Cristian_data/Humboldt/Set16/set0/Plots_registros_originales"
noCoordOutDir <- 'G:/Cristian_data/Humboldt/Set16/datos_originales/noCoords/'

# GBIF ---------------------------------------------------------------
dir <- "GBIF&SiB"
setwd(paste0("G:/Cristian_data/Humboldt/Set16/datos_originales/", dir ,"/"))

gbif<-fread('mapped/filteredOcc.csv') #mover de la carpeta raw
gbif<-as.data.frame(gbif)
gbif2<-gbif
table(is.na(gbif$lat))
gbif$lat <- as.numeric(as.character(gbif$lat))
gbif$lon<- as.numeric(as.character(gbif$lon))
table(is.na(gbif$lon)) # 36 NA
table(is.na(gbif$lat)) # 37 NA
gbif<-gbif[!is.na(gbif$lat),] 
table(is.na(gbif$lon))

gbifF<-nrow(gbif) # 34292365 

gbif <- subset(gbif, gbif$lon > -83 & gbif$lon < -60 &
                 gbif$lat > -14 & gbif$lat < 13)

nrow(gbif) # 34282012
gbif <- subset(gbif, gbif$collection != 'EBIRD') 
nrow(gbif) # 13105664
gc()

gbif$privateData <- 0
gbif$resourceName <- 'occurrence.txt'
gbif$resourceFolder <- 'G:/Cristian_data/Humboldt/Set16/datos_originales/GBIF&SiB/raw/Descarga_2022_01_13_directGBIF'
gbif$resourceIncorporationDate <- '2022-01-28'
gbif$downloadDate <- '2022-01-13'
gbif$source <- 'GBIF'

system.time(gbif$species <- cleanSciNames(gbif$species)) #1841.22 

gbif.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(gbif)))
colnames(gbif.i) <- colnames(emptyFormat)
comCols <- colnames(gbif)[colnames(gbif) %in% colnames(emptyFormat)]#columnas unificacion, que coinciden con formato final set16
gbif.i[, comCols] <- gbif[,comCols] #unir capa base con datos de entrada

gbif<-gbif.i
maxV<-max(gbif$ID)
gbif$ID<-as.(gbif$ID)
save(gbif, file = paste0(dir,"_set0.RData"))
#load(paste0(dir, '_set0.RData'))
file.copy(paste0(dir,"_set0.RData"), paste0("2021-01-26-", dir, "_set0.RData"))
rm(gbif.i, gbif2);gc()

#load(paste0(dir,"_set0.RData"))


# eBird  ---------------------------------------------------------------
dir <- "ebird"
setwd(paste0("G:/Cristian_data/Humboldt/Set16/datos_originales/",dir,"/"))
#ebird <-read.csv("G:/Cristian_data/Humboldt/Set16/datos_originales/eBird/mapped/eBird_2022-01-27.csv", encoding = 'UTF-8', header = T)
load(paste0('mapped/eBird_2022-01-28.RData'))

tabla_final1 <- emptyFormat

eb<-ebird
eb <- subset(eb, eb$lon > ext(aoi)[1] & eb$lon < ext(aoi)[2] &
                eb$lat > ext(aoi)[3] & eb$lat < ext(aoi)[4])

nRowTF1<-nrow(eb) #11819395 records

eb.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(eb)))
colnames(eb.i) <- colnames(emptyFormat)
comCols <- colnames(eb)[colnames(eb) %in% colnames(emptyFormat)]#columnas unificacion, que coinciden con formato final set16
eb.i[, comCols] <- eb[, comCols] #unir capa base con datos de entrada

eb.i$privateData <- 0
eb.i$downloadDate <- '2022-01-21'
eb.i$source <- 'eBird'
eb.i$institution <- 'eBird'
eb.i$catalogNumber <- substr(gsub("URN:CornellLabOfOrnithology:EBIRD:","",eb.i$occurrenceID),0,12)
eb.i$collection <- paste0("EBIRD_", gsub('ebd_|_reljul-2020.txt', '', eb.i$resourceName)) 

tabla_final1 <- rbind(tabla_final1, eb.i)
tablafinal1 <- overaoi(tabla_final1, dir, aoi, dir2, noCoordDir = noCoordOutDir)
tablafinal1$species <- cleanSciNames(tablafinal1$species)

tablafinal1$ID <- seq(((((maxV)+5000000))+1), (((maxV)+5000000)+nrow(tablafinal1))) # 5M de espacio

save(tablafinal1, nRowTF1, file = paste0(dir,"_set0.RData"))
#load(paste0(dir, '_set0.RData'))
file.copy(from = paste0(dir,"_set0.RData"), to = paste0(Sys.Date(), "_", dir, "_set0.RData"))

rm(eb, eb.i, ebird); gc()


# SpeciesLink  ---------------------------------------------------------------
dir <- "speciesLink"
setwd(paste0("G:/Cristian_data/Humboldt/Set16/datos_originales/",dir,"/"))

load('mapped/speciesLink_2022-01-27.RData')
tabla_final2 <- emptyFormat

spLink <- un
spLink[, c('lon', 'lat')] <- apply(spLink[, c('lon', 'lat')], 2, as.numeric)
spLink$lon<-as.numeric(spLink$lon)
spLink$lat<-as.numeric(spLink$lat)

nRowTF2<- nrow(spLink) #1838288
spl <- subset(spLink, spLink$lon > ext(aoi)[1] & spLink$lon < ext(aoi)[2] &
                spLink$lat > ext(aoi)[3] & spLink$lat < ext(aoi)[4])
nrow(spl) #1339655
sp.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(spl)))#matriz vacia

colnames(sp.i) <- colnames(emptyFormat)
comCols <- colnames(spl)[colnames(spl) %in% colnames(emptyFormat)]
sp.i[, comCols] <- spl[, comCols]

sp.i$privateData <- 0
sp.i$resourceName <- 'speciesLink_2021-01-20.csv'
sp.i$resourceFolder <- 'G:/Cristian_data/Humboldt/Set16/datos_originales/speciesLink/mapped'
sp.i$resourceIncorporationDate <- '2022-01-27'
sp.i$downloadDate <- '2022-01-27'
sp.i$source <- 'speciesLink'
sp.i$speciesOriginal <- sp.i$species
sp.i$occurrenceID <- paste0('spLink', 1:nrow(sp.i))

tabla_final2 <- rbind(tabla_final2, sp.i)
tablafinal2 <- overaoi(tabla_final2, dir, aoi, dir2, noCoordDir = noCoordOutDir)
tablafinal2$species <- cleanSciNames(tablafinal2$species)

tablafinal2$ID <- seq(((((max(tablafinal1$ID))))+1), (((max(tablafinal1$ID)))+nrow(tablafinal2))) 
save(tablafinal2, nRowTF2, file = paste0(dir,"_set0.RData"))
file.copy(from = paste0(dir,"_set0.RData"), to = paste0(Sys.Date(), "_", dir, "_set0.RData"))
#load(paste0(dir, "_set0.RData"))

rm(spl, un, sp.i, spLink); gc()

# I2D  ---------------------------------------------------------------

dir <- "Biota-I2D"
setwd(paste0("G:/Cristian_data/Humboldt/Set16/datos_originales/",dir,"/"))

load(paste0('mapped/i2d_2021-01-25.RData'))

tabla_final3 <- emptyFormat

nrow(i2d) # 1153307 datos 2020--- a la fecha no hay nuava información
colnames(i2d)
i2d$lon<-as.numeric(i2d$lon)
i2d$lat<-as.numeric(i2d$lat)

i2d <- subset(i2d, i2d$lon > ext(aoi)[1] & i2d$lon < ext(aoi)[2] &
                i2d$lat > ext(aoi)[3] & i2d$lat < ext(aoi)[4])
  

nRowTF3<-nrow(i2d) #936957
i2d.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(i2d)))
colnames(i2d.i) <- colnames(emptyFormat)
comCols <- colnames(i2d)[colnames(i2d) %in% colnames(emptyFormat)]#columnas unificacion, que coinciden con formato final set16
i2d.i[, comCols] <- i2d[, comCols] #unir capa base con datos de entrada

tabla_final3 <- rbind(tabla_final3, i2d.i)
tablafinal3 <- overaoi(tabla_final3, dir, aoi, dir2, noCoordDir = noCoordOutDir)
tablafinal3$species <- cleanSciNames(tablafinal3$species)

nRowTF3 <- nrow(tabla_final3); rm(tabla_final3); gc()
tablafinal3$ID <- seq(((((max(tablafinal2$ID))))+1), (((max(tablafinal2$ID)))+nrow(tablafinal3))) 

save(tablafinal3, nRowTF3, file = paste0(dir,"_set0.RData"))
file.copy(from = paste0(dir,"_set0.RData"), to = paste0(Sys.Date(), "_", dir, "_set0.RData"))
#load(paste0(dir, "_set0.RData"))

rm(i2d.i, i2d); gc()

# ANLA  ---------------------------------------------------------------

dir <- "ANLA"
setwd(paste0("G:/Cristian_data/Humboldt/Set16/datos_originales/",dir,"/"))

load("mapped/anla_2021-01-25.RData") # datos 2020 --- no hay nueva info de ellos
tabla_final4 <- emptyFormat

nRowTF4<-nrow(anla) #626009
anla$lon<-as.numeric(anla$lon)
anla$lat<-as.numeric(anla$lat)

anla <- subset(anla, anla$lon > ext(aoi)[1] & anla$lon < ext(aoi)[2] &
                  anla$lat > ext(aoi)[3] & anla$lat < ext(aoi)[4])

anla.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(anla)))
colnames(anla.i) <- colnames(emptyFormat)
comCols <- colnames(anla)[colnames(anla) %in% colnames(emptyFormat)]#columnas unificacion, que coinciden con formato final set16
anla.i[, comCols] <- anla[, comCols] #unir capa base con datos de entrada

tabla_final4 <- rbind(tabla_final4, anla.i)
tablafinal4 <- overaoi(tabla_final4, dir, aoi, dir2, noCoordDir = noCoordOutDir)
tablafinal4 <- tabla_final4[!is.na(tablafinal4$species),]
tablafinal4$species <- cleanSciNames(tablafinal4$species)

tablafinal4$ID <- seq(((((max(tablafinal3$ID))))+1), (((max(tablafinal3$ID)))+nrow(tablafinal4))) 
save(tablafinal4, nRowTF4, file = paste0(dir,"_set0.RData"))
file.copy(from = paste0(dir,"_set0.RData"), to = paste0(Sys.Date(), "_", dir, "_set0.RData"))

#load(paste0(dir, "_set0.RData"))
rm(anla.i, anla); gc()

# colecciones IAvH  ---------------------------------------------------------------

dir <- "Colecciones_IAvH"
setwd(paste0("G:/Cristian_data/Humboldt/Set16/datos_originales/",dir,"/"))

load("mapped/coleciones_2022-02-24.RData") # 

nRowTF5<-nrow(colecciones) #384831
tabla_final5 <- emptyFormat

str(colecciones$lat)

colecciones$lat<-as.numeric(colecciones$lat)
colecciones$lon<-as.numeric(colecciones$lon)
colecciones<- colecciones[!is.na(colecciones$lat),]
str(colecciones$decimalLatitude)

colecciones <- subset(colecciones, colecciones$lon > ext(aoi)[1] & colecciones$lon < ext(aoi)[2] &
                        colecciones$lat > ext(aoi)[3] & colecciones$lat < ext(aoi)[4])

nrow(colecciones) #297349

colecciones.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(colecciones)))
colnames(colecciones.i) <- colnames(emptyFormat)
comCols <- colnames(colecciones)[colnames(colecciones) %in% colnames(emptyFormat)]#columnas unificacion, que coinciden con formato final set16
colecciones.i[, comCols] <- colecciones[, comCols] #coleccionesir capa base con datos de entrada
colecciones.i$species<- colecciones.i$speciesOriginal
colecciones.i$privateData<-0

tabla_final5 <- rbind(tabla_final5, colecciones.i)
tabla_final5$source <- 'Colecciones IAvH'
tabla_final5$institution <- 'IAvH'
tabla_final5$downloadDate<-'22-02-2020'
tablafinal5$ID <- seq(((((max(tablafinal4$ID))))+1), (((max(tablafinal4$ID)))+nrow(tablafinal5))) 
#tablafinal5 <- overaoi(tabla_final5, dir, aoi, dir2, noCoordDir = noCoordOutDir)# Pendiente evaluar porque no fcoleccionesciona
#tablafinal5$species <- cleanSciNames(tablafinal5$species)
tablafinal5<- tabla_final5

save(tablafinal5, nRowTF5, file = paste0(dir,"_set0.RData"))
file.copy(from = paste0(dir,"_set0.RData"), to = paste0(Sys.Date(), "_", dir, "_set0.RData"))
#load(paste0(dir, "_set0.RData"))
rm(colecciones.i, colecciones); gc()

###################
### Unificacion registros
###################

resumen <- data.frame(Source = c("GBIF", "eBird", "speciesLink", 'I2D', "ANLA", "Colecciones" ), 
                      Original = c(gbifF, nRowTF1, nRowTF2, nRowTF3, nRowTF4, nRowTF5),
                      Final = c(nrow(gbif), nrow(tablafinal1), nrow(tablafinal2), nrow(tablafinal3), nrow(tablafinal4), nrow(tablafinal5)))

resumen$Porc <- (100*(resumen$Final/resumen$Original))

registros <- rbind(gbif, tablafinal1, tablafinal2, tablafinal3, tablafinal4, tablafinal5)
table(is.na(registros$ID))

registros.na<- registros[is.na(registros$ID),]
nrow(registros.na) #14722004
registros.no.na<- registros[!is.na(registros$ID),]
nrow(registros.no.na) #13403013

registros.na$ID <- paste0(1:nrow(registros.na), "_Set16")

registos<-rbind(registros.no.na, registros.na)

nrow(registros)

lat <- !is.na(registros$lat)
lon <- !is.na(registros$lon)
table(registros$source, lon, useNA = 'always')
table(registros$source, lat, useNA = 'always')


write.csv(resumen, "G:/Cristian_data/Humboldt/Set16/set0/resumen.csv")
write.csv(resumen, paste0("G:/Cristian_data/Humboldt/Set16/set0/resumen_",Sys.Date(),".csv"))

naSp <- which(is.na(registros$species))
naSpO <- which(is.na(registros$species))
naSour <- which(is.na(registros$source))

write.csv(head(registros), paste0('G:/Cristian_data/Humboldt/Set16/set0/',Sys.Date(),'_headRegistros.csv'), row.names = FALSE)
write.csv(tail(registros), paste0('G:/Cristian_data/Humboldt/Set16/set0/',Sys.Date(),'_tailRegistros.csv'), row.names = FALSE)

save(registros,file = paste0("G:/Cristian_data/Humboldt/Set16/set0/set0_",as.Date(Sys.Date()),".RData"))
file.copy(paste0("G:/Cristian_data/Humboldt/Set16/set0/set0_",as.Date(Sys.Date()),".RData"),
          paste0("G:/Cristian_data/Humboldt/Set16/set0/set0_",as.Date(Sys.Date()),".RData"))
file.copy(paste0("G:/Cristian_data/Humboldt/Set16/set0/set0_",as.Date(Sys.Date()),".RData"),
          paste0("G:/Cristian_data/Humboldt/Set16/set0/set0.RData"), overwrite = TRUE)

dim(registros)

unique<-unique(registros$species)

save(unique, file ='G:/Cristian_data/Humboldt/Set16/set0/unique_spp.RData')
