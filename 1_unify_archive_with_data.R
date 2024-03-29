# Unify files from the same source and standardize them from the file "Indicaciones-Mapeos_2023.xlsx".

library(xlsx)
library(data.table)
library(tibble)
library(stringr)

dir<-"D:/Set16/datos_originales" #directory with original files

nombres <- read.xlsx("G:/Cristian_data/Humboldt/Git/Unif_biological_data_-set16-/addicional_data/Indicaciones-Mapeos_2023.xlsx", as.data.frame = T, 
                     sheetIndex = 2, header = FALSE, colClasses = 'character') ## path with the Indicaciones-Mapeos_2023.xlsx" file

######################
######### GBIF###########
### 
## Open download data to compare with BioModelos Format
outDir_gbif <- paste0(dir,'/GBIF&SiB/mapped/Descarga_2023_01_23_directGBIF/')
gbif<-fread (paste0(dir,"/GBIF&SiB/raw/Descarga_2023_01_23_directGBIF/0255995-220831081235567/filteredOcc.txt"), encoding = 'UTF-8') #file with GBIF data

#1 .column names original files
nombres.current2 <- nombres[grep('GBIF', nombres[, 1]), -1]
nombres.current2 <- nombres.current2[, !is.na(nombres.current2[1, ])]
oriColnames2 <- c(t(nombres.current2[grep('originales', nombres.current2[, 1]), -1])) #original names
finColnames2 <- c(t(nombres.current2[grep('set', nombres.current2[, 1]), -1])) #BioModelos names

#matching columns
oriColnames3<-oriColnames2[which(!is.na(finColnames2))]
finColnames3<-finColnames2[which(!is.na(finColnames2))]

gbif<-as.data.frame(gbif)
gb.i<-gbif[,c(oriColnames3)]# Maintain columns defined in mapping indicators matrix
colnames(gb.i)<- finColnames3

gbif<-gb.i

###2. Export data set 
write.csv(gbif, paste0(outDir_gbif,'filteredOcc2.csv'), row.names = F)
rm(outDir_gbif, nombres.current2, oriColnames2,finColnames2, oriColnames3, finColnames3, gb.i, bd); gc()


######################
##### SpeciesLink
#######################
bd<-"speciesLink"
setwd(paste0(dir,"/",bd, "/raw/2023-01-23/"))
outDir <- paste0(dir,"/",bd, "/mapped/")

## #1.column names original files
nombres.current <- nombres[grep('speciesLink', nombres[, 1]), -1]
na.cols <- is.na(nombres.current[1, ])
nombres.current <- nombres.current[, !na.cols ]
oriColnames <- c(t(nombres.current[grep('originales', nombres.current[, 1]), -1]))
finColnames <- c(t(nombres.current[grep('set', nombres.current[, 1]), -1]))
oriColnames[which(!is.na(finColnames))]
finColnames[which(!is.na(finColnames))]

#matching columns
archivos <- list.files(pattern = ".txt$")
specieslink <- fread(archivos[1], encoding = 'UTF-8', quote = "")
specieslink$eventDate <- paste(specieslink$yearcollected, specieslink$monthcollected, specieslink$daycollected, sep = '-')
specieslink<-as.data.frame(specieslink)
specieslink <- specieslink[, c(oriColnames[which(!is.na(finColnames))], 'eventDate')]
colnames(specieslink) <- c(finColnames[which(!is.na(finColnames))], 'eventDate')
specieslink$resourceName <- archivos[1]
specieslink$resourceFolder <- paste0(getwd(), '/mapped')

###3. Export data set 
write.csv(specieslink, paste0(outDir,'/speciesLink_', Sys.Date(), '.csv'), row.names = FALSE)
save(specieslink, file = paste0(outDir,'/speciesLink_', Sys.Date(), '.RData'))
rm(archivos, bd, outDir, nombres.current, na.cols, oriColnames, finColnames)

#####

#############
### eBird ###
#############


bd <- "eBird"
setwd(paste0(dir,"/",bd, "/raw/2023-01-25/"))

#1. Unzip and liste the files
list<-list.files(pattern = '.zip')
for (i in 1:length(list)) {
  unzip(zipfile = list[i])
}
archivos2 <- list.files(pattern = "ebd.*.2022.txt$", recursive = T)
outDir <- paste0(dir,"/",bd, "/mapped/")

##2.column names original files
nombres.current2 <- nombres[grep('eBird', nombres[, 1]), -1]
nombres.current2 <- nombres.current2[, !(is.na(nombres.current2[2, ])) ]
oriColnames2 <- c(t(nombres.current2[grep('originales', nombres.current2[, 1]), -1]))
finColnames2 <- c(t(nombres.current2[grep('set', nombres.current2[, 1]), -1]))
oriColnames3<-oriColnames2[which(!is.na(finColnames2))]
finColnames3<-finColnames2[which(!is.na(finColnames2))] 

###3. Unify files in a unique matrix data 
approv2 <- un2 <- NULL
for (i in 1:length(archivos2)){
  cat('procesing', archivos2[i], 'file\n' )
  archivo.i <- read.delim(archivos2[i])
  colnames(archivo.i) <- gsub("//."," ",colnames(archivo.i))
  approv2.i <- archivo.i[, c('GLOBAL.UNIQUE.IDENTIFIER', 'APPROVED', 'REVIEWED', 'REASON', 'CATEGORY')]
  archivo.i$recordedBy <- 'no_collector_information'
  archivo.i$resourceName <- archivos2[i]
  archivo.i$resourceFolder <- paste0(getwd(), '/mapped')
  archivo.i$resourceIncorporationDate <- Sys.Date()
  un2 <- rbind(un2, archivo.i)
  approv2.i$resourceName <- archivo.i$resourceName
  approv2 <- rbind(approv2, approv2.i)
  cat('done \n')
}

###4. Ajust records 
colnames(un2)<-gsub("\\.", " ", colnames(un2))## remove dot to colnames
eb.i<-un2[,c(oriColnames3)]# acotar columnas fuente a columnas del formato
colnames(eb.i)<- finColnames3
eb.i$locality <- stringi::stri_encode(eb.i$locality, 'UTF-8') # Ajust loclaity coding  

un2<-cbind(eb.i, un2[,c(51:54)]) #Join columns, with modified records ver1
date<- as.data.frame(do.call(rbind, str_split(un2$eventDate, '-')))
names(date)<- c('year', 'month', 'day')
un2<-cbind(un2, date) #Join columns, with modified records ver2 
keep <- which(approv2$CATEGORY == 'species' & approv2$APPROVED == 1)
un3<- un2[keep,]
ebird <- un3
names(ebird)[10]<- 'recordedBy'

###5. Export data set 
write.csv(ebird, paste0(outDir,'eBird_', Sys.Date(), '.csv'), row.names = FALSE)
save(ebird, file = paste0(outDir,'eBird_', Sys.Date(), '.RData'))
write.csv(approv2, paste0(outDir,'/approv2edCols_', Sys.Date(), '.csv'), row.names = FALSE)
rm(bd, list,archivos2, outDir, nombres.current2,oriColnames2,finColnames2, oriColnames3, finColnames3, archivo.i, i, keep, approv2, approv2.i, date, un2, un3, eb.i);gc()

########################
### Colecciones IAvH ###
#######################

bd <- "Colecciones_IAvH"
setwd(paste0(dir,"/",bd, "/raw/2023/"))
outDir <- paste0(dir,"/",bd, "/mapped/")

#1 .column names original files
nombres.current2 <- nombres[grep('Colecciones', nombres[, 1]), -1]
nombres.current2 <- nombres.current2[, !(is.na(nombres.current2[2, ])) ]
oriColnames2 <- c(t(nombres.current2[grep('originales', nombres.current2[, 1]), -1]))
finColnames2 <- c(t(nombres.current2[grep('set', nombres.current2[, 1]), -1]))
oriColnames3 <-oriColnames2[which(!is.na(finColnames2))]
finColnames3 <-finColnames2[which(!is.na(finColnames2))] 

###2. Unify files in a unique matrix data 
archivos<- list.files(full.names = T, pattern = '.csv')
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

###3. Ajust records 
colnames(x)<-c(finColnames3,'resourceName', 'resourceFolder', 'resourceIncorporationDate')
date<- as.data.frame(do.call(rbind, str_split(x$eventDate, '-')))
names(date)<- c('year', 'month', 'day')
x<-cbind(x, date) #unir columnas loop, con variables ajustadas 

write.csv(x, paste0(outDir,'colecciones_', Sys.Date(), '.csv'), row.names = FALSE)
colecciones <- x
save(colecciones, file = paste0(outDir,'coleciones_', Sys.Date(), '.RData'))
#load(paste0(outDir,'coleciones_', Sys.Date(), '.RData'))

str(colecciones)
rm(bd, outDir, nombres.current2, oriColnames2, finColnames2, archivos, x, archivo.i, date, i, finColnames3,oriColnames3)


######################
### Datos I2D IAvH ###
######################

bd <- "Biota-I2D"
setwd(paste0( dir,"/Biota-I2D","/"))
outDir <- paste0(dir,"/",bd, "/mapped/")
data_I2D<-readRDS("2020-04-20/BIOMODELOS_CEIBA_20200330.rds")#datos originales

#1 .column names original files
nombres.current <-nombres[grep('i2d', nombres[, 1]), -1]
na.cols <- is.na(nombres.current[1, ])
nombres.current <- nombres.current[, !na.cols ]
oriColnames <- c(t(nombres.current[grep('originales', nombres.current[, 1]), -1]))
finColnames <- c(t(nombres.current[grep('set', nombres.current[, 1]), -1]))
oriColnames[which(!is.na(finColnames))]
finColnames[which(!is.na(finColnames))]

###2. Unify files in a unique matrix data 
i2d <- data_I2D[,colnames(data_I2D) %in% oriColnames]
i2d<- i2d[,oriColnames]
colnames(i2d)<- finColnames #columnas unificacion, que coinciden con formato final set16
i2d$downloadDate <- '2020-04-20'
i2d$resourceFolder <- 'D/Set16/datos_originales/Biota-I2D/mapped/'
i2d$resourceIncorporationDate <- '2023-02-01'

save(i2d, file = paste0(outDir,'i2d_', Sys.Date(), '.RData'))

rm(i2d, bd, outDir, nombres.current, oriColnames, finColnames, na.cols, archivos, x, archivo.i, date, i, data_I2D)

##############
#### ANLA ####
#############
bd <- "ANLA"
setwd(paste0(dir,  "/", bd, "/raw/"))
outDir <- paste0(dir,"/",bd, "/mapped/")
load("datos_GDB_ANLA_en_DwC_&_WGS84.RData")

#1 .column names original files
nombres.current <-nombres[grep('ANLA_dep', nombres[, 1]), -1]
na.cols <- is.na(nombres.current[2, ])
nombres.current <- nombres.current[, !na.cols ]
oriColnames <- c(t(nombres.current[grep('originales', nombres.current[, 1]), -1]))
finColnames <- c(t(nombres.current[grep('set 16', nombres.current[, 1]), -1]))
oriColnames[which(!is.na(finColnames))]
finColnames[which(!is.na(finColnames))]


###2. Unify files in a unique matrix data 
anla <- datos_GDB_ANLA[,colnames(datos_GDB_ANLA) %in% finColnames]
anla<- anla[,oriColnames]
colnames(anla)<- oriColnames  #columnas unificacion, que coinciden con formato final set16
anla$downloadDate <- '2020-04-20'
anla$resourceFolder <- 'D/Set16/datos_originales/ANLA/mapped'
anla$resourceIncorporationDate <- '2023-02-01'
save(anla, file = paste0(outDir,'anla_', Sys.Date(), '.RData'))

##### END Step  ###########