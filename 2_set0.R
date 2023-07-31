# Load files with records from various sources together with those downloaded from GBIF and SiB 
# and put them in the format in which the records from the decimalLatitudeter sources come. 
# The format is the one that will have the 'set16', the object with which the species
# distribution modeling is performed. Each data source will give rise to a .RData object 
# 'set0' that will be stored in 'other_data'. Subsequently these 'set0' will be joined 
# together to form the set1.RData
#
# The files to be loaded and converted into set0 must have the field titles that 
#are common to the information in set16 in the same way as this object has them. 

#     The  code apply by data of:
#       - GBIF 
#       - eBird 
#       - speciesLink 
#       - Colecciones I.Humboldt
#       - ANLA
#       - I2D I.Humboldt

# load ---------------------------------------------------------------

library(rgdal)
library(maptools)
library(terra)
library(xlsx)
library(data.table)

dir<-"D:/Set16/" #directory with original files

source('G:/Cristian_data/Humboldt/Git/Unif_biological_data_-set16-/addicional_data/tools.R')

data(wrld_simpl)
aoi <- vect(paste0(dir, 'maps/version2/aoi.shp'))

## Establecer ruta de trabajo
setwd(paste0(dir, 'datos_originales/'))
workDir <- getwd()
format.datos <- fread("estructuraset16_2023.csv")
#format.datos <- read.csv("C:/IAvH/DINAVIS_set16/datos_originales/estructuraset16.csv")
emptyFormat <- as.data.frame(format.datos[-(1:nrow(format.datos)), ])

resumen <- NULL
dir2 <- paste0(dir, 'set0/Plots_registros_originales')
noCoordOutDir <- paste0(dir, '/Set16/datos_originales/noCoords/')

# GBIF ---------------------------------------------------------------
# 1. Ajust and add data 
bd <- "GBIF&SiB"
setwd(paste0('/Set16/datos_originales/',bd,"/"))
gbif<-fread('mapped/filteredOcc2.csv')
gbif<-as.data.frame(gbif)
gbif2<-gbif
table(is.na(gbif$decimalLatitude))
str(gbif$decimalLatitude); str(gbif$decimalLongitude)
gbif$decimalLatitude <- as.numeric(as.character(gbif$decimalLatitude))
gbif$decimalLongitude<- as.numeric(as.character(gbif$decimalLongitude))
table(is.na(gbif$decimalLongitude)) # 34 NA
table(is.na(gbif$decimalLatitude)) # 35 NA
gbif<-gbif[!is.na(gbif$decimalLatitude),] 
table(is.na(gbif$decimalLongitude))
gbifF<-nrow(gbif) # 44139939 
gbif$resourceName <- 'occurrence.txt'
gbif$resourceFolder <- paste0(dir, '/GBIF&SiB/raw/Descarga_2022_01_13_directGBIF')
gbif$resourceIncorporationDate <- Sys.Date()
gbif$downloadDate <- '2022-01-13'
#names(gbif)[2]<- 'associatedReferences'
gbif$associatedReferences  <- 'Downloaded from GBIF'
gbif$privateData <- 0
gbif<-gbif[,-2] # Elimiar columna source, si esta aparece aquí
gbif<-gbif[,-22] # revisar si es una columna duplicada

#2. Delete data outside the study region, Scientific Name author name and eBird records.
gbif <- subset(gbif, gbif$decimalLongitude > -83 & gbif$decimalLongitude < -60 &
                 gbif$decimalLatitude > -14 & gbif$decimalLatitude < 13)
nrow(gbif) # 44129303
gbif <- subset(gbif, gbif$collection != 'EBIRD') 
nrow(gbif) # 17517838
gc()

#3. Create Set16 matrix and add GBIF records
gbif.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(gbif)))
colnames(gbif.i) <- colnames(emptyFormat)
comCols <- colnames(gbif)[colnames(gbif) %in% colnames(emptyFormat)]#columns matching end format set16
gbif.i[, comCols] <- gbif[,comCols] # join base layer with input data
gbif<-gbif.i
gbif$verbatimIdentification<- gbif$scientificName
system.time(gbif$scientificName<- cleanSciNames(gbif$scientificName)) #1841.22 
gbif$ID<-as.character(gbif$ID)

#4. Save data
save(gbif, gbifF, file = paste0(bd,"_set0.RData"))
#load(paste0(workDir, '/', bd, 'GBIF&SiB_set0.RData'))
file.copy(paste0(bd,"GBIF&SiB_set0.RData"), paste0(bd, "2023-01-27-GBIF&SiB_set0.RData"))
rm(gbif.i, gbif2);gc()
#load(paste0(dir,"_set0.RData"))

# eBird  ---------------------------------------------------------------
# 1. Ajust and add data 
bd <- "ebird"
setwd(paste0(workDir,"/", bd,"/"))
load(paste0('mapped/eBird_2023-01-26.RData'))
tabla_final1 <- emptyFormat
eb<-ebird
nRowTF1<-nrow(eb);nRowTF1 #18773733 records
eb.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(eb)))
colnames(eb.i) <- colnames(emptyFormat)
comCols <- colnames(eb)[colnames(eb) %in% colnames(emptyFormat)]#columns matching end format set16
eb.i[, comCols] <- eb[, comCols] # join base layer with input data
eb.i$privateData <- 0
eb.i$downloadDate <- '2023-01-25'
eb.i$associatedReferences  <- 'Downloaded from eBird'
eb.i$institutionCode <- 'eBird'
eb.i$collectionCode <- paste0("EBIRD_", gsub('ebd_|_reljul-2022.txt', '', eb.i$resourceName)) 
eb.i$catalogNumber <- substr(gsub("URN:CornellLabOfOrnithology:EBIRD:","",eb.i$occurrenceID),0,12)
tabla_final1 <- rbind(tabla_final1, eb.i)

#2. Create Set16 matrix and add records
tablafinal1 <- overaoi(tabla_final1, bd, aoi, dir2, noCoordDir = noCoordOutDir)
tablafinal1 <- subset(tabla_final1, tabla_final1$decimalLongitude > ext(aoi)[1] & tabla_final1$decimalLongitude < ext(aoi)[2] &
                        tabla_final1$decimalLatitude > ext(aoi)[3] & tabla_final1$decimalLatitude < ext(aoi)[4])
tablafinal1$verbatimIdentification<- tablafinal1$scientificName
tablafinal1$scientificName <- cleanSciNames(tablafinal1$scientificName)
tablafinal1$ID <- paste0(tablafinal1$institutionCode, '_rec_',seq(1, nrow(tablafinal1)), '_ebird') # 5M de espacio

#4. Save data
save(tablafinal1, nRowTF1, file = paste0(bd,"_set0.RData"))
#load(paste0(dir, '_set0.RData'))
file.copy(from = paste0(bd,"_set0.RData"), to = paste0(Sys.Date(), "_", bd, "_set0.RData"))

rm(eb, eb.i, ebird); gc()

# SpeciesLink  ---------------------------------------------------------------
# 1. Ajust and add data 
bd <- "speciesLink"
setwd(paste0('/Set16/datos_originales/',bd,"/"))
load('mapped/speciesLink_2023-01-24.RData')
tabla_final2 <- emptyFormat
specieslink[, c('decimalLongitude', 'decimalLatitude')] <- apply(specieslink[, c('decimalLongitude', 'decimalLatitude')], 2, as.numeric)
specieslink$decimalLongitude<-as.numeric(specieslink$decimalLongitude)
specieslink$decimalLatitude<-as.numeric(specieslink$decimalLatitude)
nRowTF2<- nrow(specieslink) #1143036

#2. Create Set16 matrix and add records
sp.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(specieslink)))#matriz vacia
colnames(sp.i) <- colnames(emptyFormat)
comCols <- colnames(specieslink)[colnames(specieslink) %in% colnames(emptyFormat)]
sp.i[, comCols] <- specieslink[, comCols]
sp.i$privateData <- 0
sp.i$downloadDate <- '2023-01-23'
sp.i$associatedReferences  <- 'Downloaded from SpeciesLink'
tablafinal2 <- rbind(tabla_final2, sp.i)

#3. Delete data outside the study region, and ajust Scientific Name.
tablafinal2 <- overaoi(tabla_final2, bd, aoi, dir2, noCoordDir = noCoordOutDir)
tablafinal2 <- subset(tablafinal2, tablafinal2$decimalLongitude > ext(aoi)[1] & tablafinal2$decimalLongitude < ext(aoi)[2] &
                tablafinal2$decimalLatitude > ext(aoi)[3] & tablafinal2$decimalLatitude < ext(aoi)[4])
nrow(tablafinal2) #1339655
tablafinal2$occurrenceID <- paste0('specieslink', 1:nrow(tablafinal2))
tablafinal2$ID <- paste0(tablafinal2$institutionCode, '_rec_',seq(1, nrow(tablafinal2)), '_SpeciesLink') # 5M de espacio
tablafinal2$verbatimIdentification<- tablafinal2$scientificName
tablafinal2$scientificName <- cleanSciNames(tablafinal2$scientificName)

#4. Save data
save(tablafinal2, nRowTF2, file = paste0(bd,"_set0.RData"))
file.copy(from = paste0(bd,"_set0.RData"), to = paste0(Sys.Date(), "_", bd, "_set0.RData"))
#load(paste0(dir, "_set0.RData"))
rm(spl, un, sp.i, specieslink); gc()

# I2D  ---------------------------------------------------------------
# 1. Ajust and add data 
bd <- "Biota-I2D"
setwd(paste0('/Set16/datos_originales/',bd,"/"))
load(paste0('mapped/i2d_2023-02-01.RData'))
tabla_final3 <- emptyFormat
nrow(i2d) # 1153307 datos 2020--- a la fecha (01-02-2023) no hay nuava informaci?n
colnames(i2d)
i2d$decimalLongitude<-as.numeric(i2d$decimalLongitude)
i2d$decimalLatitude<-as.numeric(i2d$decimalLatitude)
i2d<-i2d[!is.na(i2d$decimalLatitude),]
i2d<-i2d[!is.na(i2d$decimalLongitude),]
i2d$ID<- paste0(seq(1, nrow(i2d)), '_', i2d$ID)
i2d$privateData[i2d$privateData == ""] <- 0
nm0 <- c(2019, 'Subdirección de Investigaciones', 'Registros biológicos','Primaria','PARAMOS/ANDES-UNIÓN EUROPEA','Libre a nivel interno y externo (SiB)', 'Libre a nivel interno y externo','Individuos', 'Ejemplar', 'Caracterización','Boyacá BIO', 'Actual')
nm1<- c('Restringido temporalmente', 'Libre a nivel interno y externo (info restringida)', 'Libre a nivel interno con notificación previa', 'Libre a nivel interno', 'Individuo', 'Ciencias Básicas de la Biodiversidad','CEIBA/IPT_IAvH')
i2da <- i2d[i2d$privateData %in%  nm0,]
i2db <- i2d[i2d$privateData %in% nm1,]
i2da$privateData<- 0
i2db$privateData<- 1
i2dab<-rbind(i2da,i2db)
fal<-i2d[!i2d$ID %in% i2dab$ID,]
fal$privateData<- 0
i2d<-rbind(i2dab, fal)

#2. Create Set16 matrix and add records
nRowTF3<-nrow(i2d) #936957
i2d.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(i2d)))
colnames(i2d.i) <- colnames(emptyFormat)
comCols <- colnames(i2d)[colnames(i2d) %in% colnames(emptyFormat)]#columnas unificacion, que coinciden con formato final set16
i2d.i[, comCols] <- i2d[, comCols] #unir capa base con datos de entrada
tabla_final3 <- rbind(tabla_final3, i2d.i)

#3. Delete data outside the study region, and ajust Scientific Name.
tablafinal3 <- overaoi(tabla_final3, bd, aoi, dir2, noCoordDir = noCoordOutDir)
tablafinal3 <- subset(tablafinal3, tablafinal3$decimalLongitude > ext(aoi)[1] & tablafinal3$decimalLongitude < ext(aoi)[2] &
                        tablafinal3$decimalLatitude > ext(aoi)[3] & tablafinal3$decimalLatitude < ext(aoi)[4])

tablafinal3$verbatimIdentification<- tablafinal3$scientificName
tablafinal3$scientificName <- cleanSciNames(tablafinal3$scientificName)
tablafinal3$associatedReferences  <- 'Obtained from I2D - IAvH'

#4. Save data
nRowTF3 <- nrow(tabla_final3);gc()
save(tablafinal3, nRowTF3, file = paste0(bd,"_set0.RData"))
file.copy(from = paste0(bd,"_set0.RData"), to = paste0(Sys.Date(), "_", bd, "_set0.RData"))
#load(paste0(dir, "_set0.RData"))

rm(i2d.i, i2d, i2da, i2db, i2dab, fal, tabla_final3); gc()

# ANLA  ---------------------------------------------------------------

# 1. Ajust and add data
bd <- "ANLA"
setwd(paste0('/Set16/datos_originales/',bd,"/"))
load("mapped/anla_2023-02-01.RData") # datos 2020 --- no hay nueva info de ellos
tabla_final4 <- emptyFormat
nRowTF4<-nrow(anla) #626009
anla$decimalLongitude<-as.numeric(anla$decimalLongitude)
anla$decimalLatitude<-as.numeric(anla$decimalLatitude)
anla<-anla[!is.na(anla$decimalLatitude),]
anla<-anla[!is.na(anla$decimalLongitude),]

#2. Create Set16 matrix and add records
anla.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(anla)))
colnames(anla.i) <- colnames(emptyFormat)
comCols <- colnames(anla)[colnames(anla) %in% colnames(emptyFormat)]#columnas unificacion, que coinciden con formato final set16
anla.i[, comCols] <- anla[, comCols] #unir capa base con datos de entrada
tabla_final4 <- rbind(tabla_final4, anla.i)

#3. Delete data outside the study region, and ajust Scientific Name.
tablafinal4 <- overaoi(tabla_final4, bd, aoi, dir2, noCoordDir = noCoordOutDir)
tablafinal4 <- tablafinal4[!is.na(tablafinal4$scientificName),]
tablafinal4 <- subset(tablafinal4, tablafinal4$decimalLongitude > ext(aoi)[1] & tablafinal4$decimalLongitude < ext(aoi)[2] &
                  tablafinal4$decimalLatitude > ext(aoi)[3] & tablafinal4$decimalLatitude < ext(aoi)[4])
tablafinal4$verbatimIdentification<- tablafinal4$scientificName
tablafinal4$scientificName <- cleanSciNames(tablafinal4$scientificName)
tablafinal4$ID <-paste0(tablafinal4$institutionCode, '_rec_',seq(1, nrow(tablafinal4)), '_Anla')
tablafinal4$privateData <- 1
tablafinal4$associatedReferences  <- 'Obtained from ANLA'

#4. Save data
save(tablafinal4, nRowTF4, file = paste0(bd,"_set0.RData"))
file.copy(from = paste0(bd,"_set0.RData"), to = paste0(Sys.Date(), "_", bd, "_set0.RData"))
#load(paste0(bd, "_set0.RData"))
rm(anla.i, anla, tabla_final4); gc()

# colecciones IAvH  ---------------------------------------------------------------

# 1. Ajust and add data 
bd <- "Colecciones_IAvH"
setwd(paste0('/Set16/datos_originales/',bd,"/"))
load("mapped/coleciones_2023-02-08.RData") # 
nRowTF5<-nrow(colecciones) #415460
tabla_final5 <- emptyFormat
str(colecciones$decimalLatitude)
colecciones$decimalLatitude<-as.numeric(colecciones$decimalLatitude)
colecciones$decimalLongitude<-as.numeric(colecciones$decimalLongitude)
colecciones<- colecciones[!is.na(colecciones$decimalLatitude),]
colecciones<- colecciones[!is.na(colecciones$decimalLongitude),]
str(colecciones$decimalLatitude)

#2. Create Set16 matrix and add records
nrow(colecciones) #329721
colecciones.i <- as.data.frame(matrix(NA, ncol = ncol(emptyFormat), nrow = nrow(colecciones)))
colnames(colecciones.i) <- colnames(emptyFormat)
comCols <- colnames(colecciones)[colnames(colecciones) %in% colnames(emptyFormat)]#columnas unificacion, que coinciden con formato final set16
colecciones.i[, comCols] <- colecciones[, comCols] #coleccionesir capa base con datos de entrada
colecciones.i$species<- colecciones.i$speciesOriginal
colecciones.i$privateData<-0
tabla_final5 <- rbind(tabla_final5, colecciones.i)
tabla_final5$associatedReferences  <- 'Obtained from Colecciones IAvH'
tabla_final5$institutionCode <- 'IAvH'
tabla_final5$downloadDate<-'07-02-2022'

#3. Delete data outside the study region, and ajust Scientific Name.
tablafinal5 <- overaoi(tabla_final5, bd, aoi, dir2, noCoordDir = noCoordOutDir)# Pendiente evaluar porque no fcoleccionesciona
tablafinal5 <- subset(tablafinal5, tablafinal5$decimalLongitude > ext(aoi)[1] & tablafinal5$decimalLongitude < ext(aoi)[2] &
                        tablafinal5$decimalLatitude > ext(aoi)[3] & tablafinal5$decimalLatitude < ext(aoi)[4])
tablafinal5$verbatimIdentification<- tablafinal5$scientificName
tablafinal5$scientificName <- cleanSciNames(tablafinal5$scientificName)
tablafinal5$ID <-paste0(tablafinal5$institutionCode, '_rec_',seq(1, nrow(tablafinal5)), '_col_IAvH')

#4. Save data
save(tablafinal5, nRowTF5, file = paste0(bd,"_set0.RData"))
file.copy(from = paste0(bd,"_set0.RData"), to = paste0(Sys.Date(), "_", bd, "_set0.RData"))
#load(paste0(dir, "_set0.RData"))
rm(colecciones.i, colecciones); gc()

###################
### Unificacion registros
###################
setwd(paste0(dir, 'datos_originales/'))
rm(list=ls())

#1. Open all datasets
bd<- 'GBIF&SiB'; load(paste0(bd,"/", bd,"_set0.RData"))
bd<- 'ebird'; load(paste0(bd,"/", bd,"_set0.RData"))
bd<- 'speciesLink'; load(paste0(bd,"/", bd,"_set0.RData"))
bd<- 'Biota-I2D'; load(paste0(bd,"/", bd,"_set0.RData"))
bd<- 'ANLA'; load(paste0(bd,"/", bd,"_set0.RData"))
bd<- 'Colecciones_IAvH'; load(paste0(bd,"/", bd,"_set0.RData"))


#2 summary of data
resumen <- data.frame(Source = c("GBIF", "eBird", "speciesLink", 'I2D', "ANLA", "Colecciones" ), 
                      Original = c(gbifF, nRowTF1, nRowTF2, nRowTF3, nRowTF4, nRowTF5),
                      Final = c(nrow(gbif), nrow(tablafinal1), nrow(tablafinal2), nrow(tablafinal3), nrow(tablafinal4), nrow(tablafinal5)))

resumen$percentage_records_kept <- (100*(resumen$Final/resumen$Original))
write.csv(resumen, 'D:/Set16/set0/resumen.csv')
write.csv(resumen, paste0('D:/Set16/set0/resumen_', Sys.Date(),".csv"))

#2 Unify all data and obtained set 0
registros <- rbind(gbif, tablafinal1, tablafinal2, tablafinal3, tablafinal4, tablafinal5)
save(registros,file = paste0("D:/Set16/set0/set0.RData"))
nrow(registros)
dim(registros)
save(registros,file = paste0("D:/Set16/set0/set0_",as.Date(Sys.Date()),".RData"))
file.copy(paste0('D:/Set16/set0/set0_',as.Date(Sys.Date()),".RData"),
          paste0( 'D:/Set16/set0/set0_',as.Date(Sys.Date()),".RData"))
file.copy(paste0( 'D:/Set16/set0/set0_',as.Date(Sys.Date()),".RData"),
          paste0('D:/Set16/set0/set0.RData'), overwrite = TRUE)

##### END Step 0 ###########