#
"""Codigo del flujo del se16
  Generacion del set2, union y depuracion espacial de registros de varias fuentes

  En orden el proceso es el siguiente:
        - Cargar registros validados taxonomicamente
        - Seleccionar de los registros de paises vecinos solo las especies que se encuentren en Colombia
        - Generar columna 'DupIndex' para evaluar la 'duplicidad' a traves de la combinacion entre los campos de insitucion, catalogo y 
            numero de catalogo.

"""
#----------------------------------------#
#  1. Liberar espacio y cargar paquetes  # 
#----------------------------------------# 

rm(list = ls())
gc()
memory.limit(size = 1000000) 
library(terra)
library(sp)
library(tidyr)
library(data.table)
library(dplyr)
library(rgdal)
library(maptools)

#------------------------------------------#
#  2. Cargar datos de una o varias fuentes # 
#------------------------------------------# 

setwd("G:/Cristian_data/Humboldt/Set16/set1")

load("set1.RData")
# mal.x <- grep('\\.x', colnames(registros))
# colnames(registros) <- gsub('\\.y', '', colnames(registros))
# set2 <- registros[, -c(mal.y)]
set2 <- set1
rm(set1)
nrow(set2) #28125017

#-------------------------------------------------------------------#
#  3. Identificar especies en Colombia segun coordenadas            # 
#-------------------------------------------------------------------# 

co <- readOGR("G:/Cristian_data/Humboldt/Set16/maps/col_buffer10km.shp")
aoi <- readOGR("G:/Cristian_data/Humboldt/Set16/maps/version2/aoi.shp")
#plot(aoi);plot(co, add = T)

str(set2$lat)
str(set2$lon)
set2$lat<- as.numeric(set2$lat)
set2$lon<- as.numeric(set2$lon)

no.coords <- which(is.na(set2$lat) | is.na(set2$lon))
if (length(no.coords)>0){
  #write.csv(set2[no.coords, ], paste0('datos_sin_coords_set1_',Sys.Date(),'.csv'))
  set2 <- set2[-no.coords, ]
}

set2 <- subset(set2, set2$lon > extent(aoi)@xmin & set2$lon < extent(aoi)@xmax &
                 set2$lat > extent(aoi)@ymin & set2$lat < extent(aoi)@ymax)


coords <- set2[ , c("ID", "lat","lon")]

rm(no.coords)

coordinates(coords) =~ lon + lat
CRS.new<-"+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
proj4string(coords)<-CRS.new
coords <- spTransform(coords, proj4string(co))

system.time(over.coords <- over(coords, co)); head(over.coords) #437.2 minutos, 7.2 hrs # 395 minutos

rm(coords)


enCol <- as.numeric(over.coords$OBJECTID)
enCol[enCol>=1] <- 1
enCol[is.na(enCol)] <- 0
set2$enCol <- enCol

nrow.over.coords <- nrow(over.coords)
rm(over.coords, enCol, aoi, co)

set2$acceptedNameUsage<-set2$species

tabla.over <- (table(set2$species, set2$enCol))
tabla.overO <- (table(set2$speciesOriginal, set2$enCol))
tabla.overV <- (table(set2$acceptedNameUsage, set2$enCol))

tabla.over <- cbind(tabla.over,rowSums(tabla.over),rowSums(tabla.over)>0); head(tabla.over)
tabla.overO <- cbind(tabla.overO,rowSums(tabla.overO),rowSums(tabla.overO)>0); head(tabla.overO)
tabla.overV <- cbind(tabla.overV,rowSums(tabla.overV),rowSums(tabla.overV)>0); head(tabla.overV)

sp.col <- rownames(tabla.over)[unique(c(which(tabla.over[, 4]>0)))]; #sp.col[300:800]
sp.colO <- rownames(tabla.overO)[unique(c(which(tabla.overO[, 4]>0)))];# sp.colO[300:800]
sp.colV <- rownames(tabla.overV)[unique(c(which(tabla.overV[, 4]>0), which(rownames(tabla.overV) == "NA")))]; head(sp.colV)

rm(tabla.over, tabla.overO, tabla.overV)

sp.pos <-  which(set2$species %in% sp.col)
sp.posO <- which(set2$speciesOriginal %in% sp.colO)
sp.posV <- which(set2$acceptedNameUsage %in% sp.colV)


#----------------------------------------------------------------#
#  4. Identificar especies en Colombia segun listado en DINAVIS  # 
#----------------------------------------------------------------# 

load("G:/Cristian_data/Humboldt/Set16/scripts/Cristian/TAX2022.RData")
TAXsciNames <- unique(TAX$scientificName)
TAXaccepNames <- unique(TAX$acceptedNameUsage)
TAXoriNNames <- unique(TAX$originalNameUsage)
rm(TAX)

TAXNames <- sort(gsub(' ', '', unique(c(TAXsciNames, TAXaccepNames, TAXoriNNames))))

sp.TAX1 <- which(gsub(' ', '', set2$species) %in% TAXNames)
sp.TAX2 <- which(gsub(' ', '', set2$speciesOriginal) %in% TAXNames)
sp.TAX3 <- which(gsub(' ', '', set2$acceptedNameUsage) %in% TAXNames)
sp.TAX <- unique(c(sp.TAX1, sp.TAX2, sp.TAX3))

rm(sp.TAX1, sp.TAX2, sp.TAX3, TAXNames, TAXsciNames, TAXaccepNames, TAXorigNames)

#---------------------------------------#
#  4. Seleccionar especies en Colombia  # 
#---------------------------------------# 
indexspcol <- unique(c(sp.pos,sp.posO,sp.TAX))
rm(sp.pos,sp.posO,sp.TAX, sp.col, sp.colO, sp.colV)

 points.col <- set2[indexspcol,]; dim(points.col)
 points.nocol <- set2[-indexspcol,]; dim(points.nocol)

 coordinates(points.nocol) =~ lon + lat
 coordinates(points.col) =~ lon + lat
 plot(aoi, add = T);plot(co, add = T)
 points(points.col, col = "blue", cex=0.75, pch = 20)
 points(points.nocol, col = "orange", cex=0.75, pch = 20)
# 
# sp <- "Accipiter ventralis"
# prueba.sp <- subset(over, over$species == sp |over$speciesOriginal == sp |over$acceptedNameUsage == sp ); dim(prueba.sp)
# coordinates(prueba.sp)=~lon+lat;plot(aoi);plot(co, add = T);points(prueba.sp, col = "orange", cex=0.75, pch = 20)
# over.sp <- over(SpatialPoints(cbind(prueba.sp$lon,prueba.sp$lat)),co); head(over.sp)

#----------------------------------------------#
#  5. Guardar listado de especies descartadas  # 
#----------------------------------------------# 
write.csv(set2[-indexspcol, ], paste0("G:/Cristian_data/Humboldt/Set16/set2/sppSinRegistrosEnColombia", Sys.Date(), ".csv"))

set2 <- set2[indexspcol, ]
rm(indexspcol)
#-------------------------------#
#  6. Aplicar indice duplicidad # 
#-------------------------------# 

indDup <- paste(set2$collection, set2$catalogNumber, sep ="-")
dup <- duplicated(indDup, fromLast = TRUE)
dupNA <- which(indDup == "NA-NA" | indDup == "-" | indDup == "-NA" | indDup == "NA-")
set2$DupIndex <- 0
set2$DupIndex[dup] <- 1
set2$DupIndex[dupNA] <- 0
rm(indDup, dup, dupNA)

#-----------------------------------------------#
#  7. Generar campo de incertidumbre taxonomica # 
#-----------------------------------------------# 

cf <- grep(" cf\\. ", set2$speciesOriginal)
cf2 <- grep(" cf ", set2$speciesOriginal)
inter <- grep("\\?", set2$speciesOriginal)
taxDoubdt <- unique(c(cf, cf2, inter))
set2$taxDoubdt <- 0
set2$taxDoubdt[taxDoubdt] <- 1
rm(cf, cf2, inter, taxDoubdt)

#------------------------------#
#  8. Escribir datos y resumen # 
#------------------------------# 

 resumen.sp.col <- cbind(rbind(length(unique(set2$acceptedNameUsage)), 
                               length(unique(points.col$acceptedNameUsage)),
                               length(unique(points.nocol$acceptedNameUsage))), 
                         rbind(nrow.over.coords,nrow(points.col),nrow(points.nocol)))
 colnames(resumen.sp.col) <- c("Especies","Registros")
 rownames(resumen.sp.col) <- c("En poligono","Colombia","Fuera Colombia")
# 
resumen <- summary(set2[, c("lat","lon","alt","coordUncertaintyM","source")]); resumen

tabla <- table(set2$source, set2$DupIndex)
tabla <- cbind(tabla, round((tabla[, 2]/tabla[, 1])*100, 2))
colnames(tabla) <- c("Registros", "Repetidos", "%"); tabla

setwd("G:/Cristian_data/Humboldt/Set16/set2")
write.csv(tabla,paste0("02_RegistrosXfuente&Duplicados_", Sys.Date(), ".csv"))
#write.csv(tabla, paste0("../Resumen/02_RegistrosXfuente&Duplicados_", Sys.Date(), ".csv"))
write.csv(resumen, paste0("resumen/04_Resumen_variables_num_", Sys.Date(), ".csv"))
write.csv(resumen, paste0("Resumen/04_Resumen_variables_num_", Sys.Date(), ".csv"))
write.csv(resumen.sp.col, paste0("03_Resumen_sp_registros_", Sys.Date(), ".csv"))
write.csv(resumen.sp.col, paste0("../Resumen/03_Resumen_sp_registros_",Sys.Date(),".csv"))

save(file = 'set2.RData', set2)
file.copy('set2.RData', paste0("set2_", Sys.Date(), ".RData"), overwrite = TRUE)

# speciesLink <- unique(c(unique(set2$species),unique(set2$speciesOriginal)))
# write.csv(speciesLink,"G:/Cristian_data/Humboldt/Set16/datos_originales/Originales/speciesLink/Listado_R_buscar2.csv",row.names = F)

#----------------------#
#  9. Generar graficas # 
#----------------------# 

(sources <- unique(set2$source))
data(wrld_simpl)
dir.create(paste0('Plots_',Sys.Date()))
s <- 1
for (s in 1:length(sources)){
  fuente.s <- set2[which(set2$source == sources[s]), c("lat","lon")]
  coordinates(fuente.s) =~ lon + lat
  png(paste0('Plots_',Sys.Date(),"/set2_", sources[s], "_",Sys.Date(),".png"), width = 1000, height = 700, units = 'px')
  plot(wrld_simpl, xlim = c(-85, -60), ylim = c(-20, 20), axes = TRUE, col='light yellow')
  plot(aoi, col = rgb(0.5, 0.3, 0.4, 0.1), add = T)
  plot(fuente.s, add = T, col = rgb(0.8, 0.3, 0, 0.5), cex = 0.05, pch = 20)
  text(x = -90, y = -16, as.character(sources[s]), cex = 3)
  box()
  dev.off()
}
