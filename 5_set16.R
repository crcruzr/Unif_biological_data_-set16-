library(CoordinateCleaner)

RoutineType <- routineType <- 'Colombia'

## Para proceso del set16: 
# Load 
setwd('G:/Cristian_data/Humboldt/Set16/')
load('./set2/set2.RData')

setwd('G:/Cristian_data/Humboldt/Set16/set16/verifGeo/')
source('script.R')
load('Colombia.RData')

set2 <- as.data.frame(set2)

set2$scriptID <- 1:nrow(set2)
set3 <- set2[, c("scriptID", "acceptedNameUsage", "country", "adm1", "adm2", "lat", "lon")]
#set3 <- set2[, c("scriptID", "acceptedNameUsage", "Pais", "Departamento", "Municipio", "lat", "lon")]
colnames(set3) <- c("scriptID", "nombre", "pais", "departamento", "municipio", "latitud", "longitud")

# Requieres 'set2', 'set3' and 'routineType'
system.time(verif <- VERIFICACION_PAISES(set3, routineType = RoutineType, rdata = TRUE)) #205310.22

set16$correctstateProvince<- ifelse(set16$departamento == set16$suggestedStateProvince, 1, 0)
set16$correctstateProvince[is.na(set16$correctstateProvince)] <- 0

set16$correctCounty[is.na(set16$correctCounty)] <- 0
set16$correctCounty<- ifelse(set16$county == set16$suggestedCounty,1,0)

##ajustar nombres países
col<- c('CO', 'Colombia', 'Colombie', 'COLOMBIA', 'Colômbia', 'Colombia ', ' colombia')
bo<- c('Bolivia', 'Bolívia', 'BO', 'BOLÍVIA', 'Bolivie')
br<- c('BR', 'Brasil', 'BRASIL', 'brazil', 'Brazil', 'BRAZIL', 'Brésil')
ec<- c('EC', 'Ecuador', 'ECUADOR', 'Equador', 'EQUADOR', 'Equateur', 'Équateur')
pa<-c('Gulf of Panama', 'PA', 'Panama', 'Panamá')
pe<-c('PE', 'Perou', 'Pérou', 'Peru', 'PERU', 'Perú')
ve<- c('VE', 'venezuela', 'Venezuela', 'VENEZULA', 'Vénézuela', 'Vénézuéla' ,' VENEZUELA')

dr<-set16
#set16 <- dr
table(set16$pais)

nrow(set16)
set16.a<- set16[set16$pais %in% col,]
set16.a$pais<-'Colombia'
set16.b<- set16[!set16$pais %in% col,]
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
set16.a$pais<- 'Curaçao'
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
set16$correctCountry<- ifelse(set16$pais == set16$suggestedCountry, 1, 0)
set16$correctCountry[is.na(set16$correctCountry)] <- 0
set16$alt[is.na(set16$alt)] <- 0
set16$extremo[is.na(set16$extremo)] <- 0


set16<- merge(set2, set16[c(1,10:22)], by ='scriptID', all = T)

dt<-ls()
dt<-(dt[!grepl('set16', dt)])
rm(list = dt); rm(dt); gc()

### Identificar los datos erroneso por especie y generaciÃ³n de mapas para cada uno de ellos
## evaluaci[o]n de coordenadas por: a) Centroide, b) OcÃ©nao, c) registros idÃ©nticos entre el set de datos, d)centroide de GBIF, e) centroide de museos de HN, f) las coordenadas son ceros absolutos, g) las coordenadas son duplicados


land<- readOGR("G:/Cristian_data/Humboldt/capas base/politico/world/10m_admin_0_countries.shp")

set16 <- clean_coordinates(x = set16, lon = "lon", lat = "lat",
                           countries = "country", 
                           species = "species",
                           tests = c("capitals" , "centroids", "seas", "equal","gbif", "institutions", "zeros", "duplicates"),
                           centroids_rad 	= 10, ##10 meters buffer
                           seas_ref = land, # shp to land area
                           inst_rad = 1,  ## one meter buffer
                           country_ref=world)

set16<-as.data.frame(set16)
##Unify evaluation with politico-administrative flags
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
colnames(set16)[56:65]<-c('coord.validity', 'equal.lat/lon', 'Zero.in.coords', 'cap.centroid', 'dept/country.centroid', 'in.sea', 'gbif.centroid', 'institution.centroid', 'dupl.record', 'summary')

write.csv(set16, row.names = FALSE, fileEncoding = 'UTF-8', 
          'G:/Cristian_data/Humboldt/Set16/set16/set16.csv')

setwd('G:/Cristian_data/Humboldt/Set16/set16/')

save.image(set16, file = paste0('set16_',Sys.Date(), '.RData'))

################
## set16 Done ##
################