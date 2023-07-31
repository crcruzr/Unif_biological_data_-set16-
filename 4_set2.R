
#----------------------------------------#
#  1. Free up space and load packages  # 
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

#----------------#
#  2. Load Data  # 
#----------------# 

setwd('D:/Set16/set1/')

load("set1.RData")
# mal.x <- grep('\\.x', colnames(registros))
# colnames(registros) <- gsub('\\.y', '', colnames(registros))
# set2 <- registros[, -c(mal.y)]
set2 <- set1
rm(set1)
nrow(set2) #39417891

#-------------------------------------------------#
#  3. Identify species in Colombia by coordinates # 
#-------------------------------------------------# 

co <- readOGR("D:/Set16/maps/col_buffer10km.shp")
aoi <- readOGR("D:/Set16/maps/version2/aoi.shp")

str(set2$decimalLatitude)
str(set2$decimalLongitude)
#set2$decimalLatitude<- as.numeric(set2$decimalLatitude)
#set2$decimalLongitude<- as.numeric(set2$decimalLongitude)


set2 <- subset(set2, set2$decimalLongitude > extent(aoi)@xmin & set2$decimalLongitude < extent(aoi)@xmax &
                 set2$decimalLatitude > extent(aoi)@ymin & set2$decimalLatitude < extent(aoi)@ymax)


coords <- set2[ , c("ID", "decimalLatitude","decimalLongitude")]

coordinates(coords) =~ decimalLongitude + decimalLatitude
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
gc()
rm(over.coords, enCol, aoi, co)



tabla.over <- (table(set2$scientificName, set2$enCol))
tabla.overO <- (table(set2$verbatimIdentification, set2$enCol))


tabla.over <- cbind(tabla.over,rowSums(tabla.over),rowSums(tabla.over)>0); head(tabla.over)
tabla.overO <- cbind(tabla.overO,rowSums(tabla.overO),rowSums(tabla.overO)>0); head(tabla.overO)

sp.col <- rownames(tabla.over)[unique(c(which(tabla.over[, 4]>0)))]; #sp.col[300:800]
sp.colO <- rownames(tabla.overO)[unique(c(which(tabla.overO[, 4]>0)))];# sp.colO[300:800]

rm(tabla.over, tabla.overO)

sp.pos <-  which(set2$scientificName %in% sp.col)
sp.posO <- which(set2$verbatimIdentification %in% sp.colO)


#----------------------------------------------------#
#  4. Identify species in Colombia as listed in I2D  # 
#----------------------------------------------------# 

load("D:/Set16/scripts/Cristian/TAX2022.RData")
TAXsciNames <- unique(TAX$scientificName)
TAXoriNNames <- unique(TAX$originalNameUsage)
rm(TAX)

TAXNames <- sort(gsub(' ', '', unique(c(TAXsciNames, TAXoriNNames))))

sp.TAX1 <- which(gsub(' ', '', set2$scientificName) %in% TAXNames)
sp.TAX2 <- which(gsub(' ', '', set2$verbatimIdentification) %in% TAXNames)
sp.TAX <- unique(c(sp.TAX1, sp.TAX2))

rm(sp.TAX1, sp.TAX2, TAXNames, TAXsciNames, TAXoriNNames)

#---------------------------------#
#  4. Select species in Colombia  # 
#---------------------------------# 

indexspcol <- unique(c(sp.pos,sp.posO,sp.TAX))
rm(sp.pos,sp.posO,sp.TAX, sp.col, sp.colO)

 points.col <- set2[indexspcol,]; dim(points.col)
 points.nocol <- set2[-indexspcol,]; dim(points.nocol)

 coordinates(points.nocol) =~ decimalLongitude + decimalLatitude
 coordinates(points.col) =~ decimalLongitude + decimalLatitude
 
 #Por el volumen de la información, se demora demasiado 
 #points(points.col, col = "blue", cex=0.75, pch = 20)
 #points(points.nocol, col = "orange", cex=0.75, pch = 20)
 #plot(aoi);plot(co, add = T)
 
#-------------------------------------#
#  5. Save list of discarded species  # 
#-------------------------------------# 
 
write.csv(set2[-indexspcol, ], paste0("D:/Set16/set2/sppSinRegistrosEnColombia", Sys.Date(), ".csv"))
set2 <- set2[indexspcol, ]
rm(indexspcol)

#---------------------------#
#  6. Apply duplicity index # 
#---------------------------# 

indDup <- paste(set2$collectionCode, set2$catalogNumber, sep ="-")
dup <- duplicated(indDup, fromLast = TRUE)
dupNA <- which(indDup == "NA-NA" | indDup == "-" | indDup == "-NA" | indDup == "NA-")
set2$DupIndex <- 0
set2$DupIndex[dup] <- 1
set2$DupIndex[dupNA] <- 0
rm(indDup, dup, dupNA)

#------------------------------------------#
#  7. Generate taxonomic uncertainty field # 
#------------------------------------------# 

cf <- grep(" cf\\. ", set2$verbatimIdentification)
cf2 <- grep(" cf ", set2$verbatimIdentification)
inter <- grep("\\?", set2$verbatimIdentification)
taxDoubdt <- unique(c(cf, cf2, inter))
set2$taxDoubdt <- 0
set2$taxDoubdt[taxDoubdt] <- 1
rm(cf, cf2, inter, taxDoubdt)

#-----------------------------#
#  8. Write data and summary  # 
#-----------------------------# 

 resumen.sp.col <- cbind(rbind(length(unique(set2$scientificName)), 
                               length(unique(points.col$scientificName)),
                               length(unique(points.nocol$scientificName))), 
                         rbind(nrow.over.coords,nrow(points.col),nrow(points.nocol)))
 colnames(resumen.sp.col) <- c("Especies","Registros")
 rownames(resumen.sp.col) <- c("En poligono","Colombia","Fuera Colombia")
# 
resumen <- summary(set2[, c("decimalLatitude","decimalLongitude","verbatimElevation","coordinateUncertaintyInMeters","associatedReferences")]); resumen

tabla <- table(set2$associatedReferences, set2$DupIndex)
tabla <- cbind(tabla, round((tabla[, 2]/tabla[, 1])*100, 2))
colnames(tabla) <- c("Registros", "Repetidos", "%"); tabla

setwd("D:/Set16/set2")
write.csv(tabla,paste0("02_RegistrosXfuente&Duplicados_", Sys.Date(), ".csv"))
write.csv(resumen.sp.col, paste0("03_Resumen_sp_registros_", Sys.Date(), ".csv"))
write.csv(resumen, paste0("resumen/04_Resumen_variables_num_", Sys.Date(), ".csv"))

save(file = 'set2.RData', set2)
file.copy('set2.RData', paste0("set2_", Sys.Date(), ".RData"), overwrite = TRUE)

# speciesLink <- unique(c(unique(set2$species),unique(set2$speciesOriginal)))
# write.csv(speciesLink,"D:/Set16/datos_originales/Originales/speciesLink/Listado_R_buscar2.csv",row.names = F)

#----------------------#
#  9. Generate graphs  # 
#----------------------# 

associatedReferences <- unique(set2$associatedReferences)
data(wrld_simpl)
dir.create(paste0('Plots_',Sys.Date()))
s <- 1
for (s in 1:length(associatedReferences)){
  fuente.s <- set2[which(set2$associatedReferences == associatedReferences[s]), c("decimalLatitude","decimalLongitude")]
  coordinates(fuente.s) =~ decimalLongitude + decimalLatitude
  png(paste0('Plots_',Sys.Date(),"/set2_", associatedReferences[s], "_",Sys.Date(),".png"), width = 1000, height = 700, units = 'px')
  plot(wrld_simpl, xlim = c(-85, -60), ylim = c(-20, 20), axes = TRUE, col='light yellow')
  plot(aoi, col = rgb(0.5, 0.3, 0.4, 0.1), add = T)
  plot(fuente.s, add = T, col = rgb(0.8, 0.3, 0, 0.5), cex = 0.05, pch = 20)
  text(x = -90, y = -16, as.character(associatedReferences[s]), cex = 3)
  box()
  dev.off()
}

##### END Step 2 ###########
