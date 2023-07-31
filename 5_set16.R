
#--------------------------------------------#
#  1. Load packages, dataset,  and routines  # 
#--------------------------------------------# 
library(CoordinateCleaner)
library(terra)
library(data.table)

RoutineType <- routineType <- 'Colombia'
setwd('D:/Set16/')
load('./set2/set2.RData')
source('set16/verifGeo/script.R')
load('set16/verifGeo/Colombia.RData')

#--------------------------#
#  2. Add column to match  #
#--------------------------#

set2$scriptID <- 1:nrow(set2)
set3 <- set2[, c("scriptID", "scientificName", "country", "stateProvince", "municipality", "decimalLatitude", "decimalLongitude")]
colnames(set3) <- c("scriptID", "nombre", "pais", "departamento", "municipio", "latitud", "longitud")

#Piloto para evaluar que la matriz de datos esté funcionando bien
#set3A<- rbind((head(set3, 10000)), (tail(set3, 10000)))

#-------------------------------------------------------------------#
#  3. Verify countries, states, counties, altitude, dup in records  #
#-------------------------------------------------------------------#
# Requieres 'set2', 'set3' and 'routineType'
system.time(verif <- VERIFICACION_PAISES(set3, routineType = RoutineType, rdata = TRUE)) #205310.22
set16<-verif$set16

#--------------------------------------#
#  4. Manual ajust to country names  #
#--------------------------------------#
col<- c('CO', 'Colombia', 'Colombie', 'COLOMBIA', 'Colômbia', 'Colombia ', ' colombia', 'colombia')
bo<- c('Bolivia', 'Bolívia', 'BO', 'Bolívia', 'Bolivie')
br<- c('BR', 'Brasil', 'BRASIL', 'brazil', 'Brazil', 'BRAZIL', 'Bradil')
ec<- c('EC', 'Ecuador', 'ECUADOR', 'Equador', 'EQUADOR', 'Equateur', 'Equateur')
pa<-c('Gulf of Panama', 'PA', 'Panama', 'Pnamá')
pe<-c('PE', 'Perou', 'P?rou', 'Peru', 'PERU', 'Perú')
ve<- c('VE', 'venezuela', 'Venezuela', 'VENEZULA', 'V?n?zuela', 'V?n?zu?la' ,' VENEZUELA')


table(set16$pais)
nrow(set16)
set16.a<- set16[set16$pais %in% col,]
set16.a$pais<-'Colombia'
set16.b<- set16[!set16$pais %in% col,]
table(set16.b$pais)
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% bo,]
set16.b<- set16[!set16$pais %in% bo,]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<-'Bolivia'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% br,]
set16.b<- set16[!set16$pais %in% br,]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<-'Brazil'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% ec,]
set16.b<- set16[!set16$pais %in% ec,]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<-'Ecuador'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% pe,]
set16.b<- set16[!set16$pais %in% pe,]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<-'Peru'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% ve,]
set16.b<- set16[!set16$pais %in% ve,]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<- 'Venezuela'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% 'NI',]
set16.b<- set16[!set16$pais %in% 'NI',]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<- 'Nicaragua'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% 'CR',]
set16.b<- set16[!set16$pais %in% 'CR',]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<- 'Costa Rica'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% 'CW',]
set16.b<- set16[!set16$pais %in% 'CW',]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<- 'Curazao'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% 'GD',]
set16.b<- set16[!set16$pais %in% 'GD',]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<- 'Grenada'
set16<-rbind(set16.a, set16.b)
nrow(set16)

set16.a<- set16[set16$pais %in% 'GY',]
set16.b<- set16[!set16$pais %in% 'GY',]
nrow(set16.a) + nrow(set16.b)
set16.a$pais<- 'Guyana'
set16<-rbind(set16.a, set16.b)
nrow(set16)

table(set16$pais)

#------------------------------------------------#
#  5. match between correct state ans counties   #
#------------------------------------------------#
set16$correctstateProvince<- ifelse(set16$departamento == set16$suggestedStateProvince, 1, 0)
set16$correctstateProvince[is.na(set16$correctstateProvince)] <- 0
set16$correctCounty[is.na(set16$correctCounty)] <- 0
set16$correctCounty<- ifelse(set16$municipio == set16$suggestedCounty,1,0)
set16$alt[is.na(set16$alt)] <- NA
set16$extremo[is.na(set16$extremo)] <- NA
set16<- merge(set2, set16[c(1,8:22)], by ='scriptID', all = T)
dt<-ls()
dt<-(dt[!grepl('set16', dt)])
rm(list = dt); rm(dt); gc()

#---------------------------------------------#
#  6.  Add flags coordinate cleaner package   #
#---------------------------------------------#
land<- vect("maps/10m_admin_0_countries.shp")
set16 <- clean_coordinates(x = set16, lon = "decimalLongitude", lat = "decimalLatitude",
                           countries = "country", 
                           species = "scientificName",
                           tests = c("capitals" , "centroids", "seas", "equal","gbif", "institutions", "zeros", "duplicates"),
                           centroids_rad 	= 10, ##10 meters buffer
                           seas_ref = land, # shp to land area
                           inst_rad = 1,  ## one meter buffer
                           country_ref=world)

set16<-as.data.frame(set16)
set16$.val<- ifelse(set16$.val == 'TRUE',0,1)
set16$.equ<- ifelse(set16$.equ == 'TRUE',0,1)
set16$.zer<- ifelse(set16$.zer == 'TRUE',0,1)
set16$.cap<- ifelse(set16$.cap == 'TRUE',0,1)
set16$.cen<- ifelse(set16$.cen == 'TRUE',0,1)
set16$.sea<- ifelse(set16$.sea == 'TRUE',0,1)
set16$.gbf<- ifelse(set16$.gbf == 'TRUE',0,1)
set16$.inst<- ifelse(set16$.inst == 'TRUE',0,1)
set16$.dpl<- ifelse(set16$.dpl == 'TRUE',0,1)
set16$.summary<- ifelse(set16$.summary == 'TRUE',0,1)
colnames(set16)  
colnames(set16)[65:74]<-c('coord.validity', 'equal.lat/lon', 'Zero.in.coords', 'cap.centroid', 'dept/country.centroid', 'in.sea', 'gbif.centroid', 'institution.centroid', 'dupl.record', 'summary')

#--------------------------#
#  7.  Save set16 dataset  #
#--------------------------#
setwd('D:/Set16/Set16/')
write.csv(set16, row.names = FALSE, fileEncoding = 'UTF-8', 
          'D:/Set16/set16/set16.csv')
save(set16, file = paste0('set16_',Sys.Date(), '.RData'))
save(set16, file = paste0('set16.RData'))

################
## set16 Done ##
################