# Depurar nombres cientificos a ginero y eimteto eliminando caracteres como:
#  Confirmation: cf., ' cf '.
#  Afinis: sp. aff., aff., affin. http://en.wikipedia.org/wiki/Species_affinis
#  Variety: var., var 
#  
# 
# # (x <- "\"\"\" Panara \"\"sicora\"")
# x <- testName <- c("\"Pristimantis ta-eniatus\"", "Pristimantis \ taeniatus",
#               "\"Bolitoglossa vallecula\"", "Piper amalago L. var. medium (Jacq.) Yunck.",
#               "Ronabea emetica (L. f.) A. Rich.", "Chamaepetes goudotii goudotii (Lesson, 1828)",
#               "\"Mabuya altamazonica Miralles, Barrio-amoros, Rivas, Chaparro-auza, 2006\"",
#               "\"\"\" Panara \"\"sicora\"",
#               "ANNONACEAE", 'Apidea apis mielifera', 'Apidea apis mielifera (Grooves 2001)',
#               'Apidae  apis mielifera (Grooves 2001) cf mielifera',
#               ' Apidae  apis mielifera (Grooves 2001) spp  mielifera',
#               'Apidae apis aff. mielifera (Grooves 2001)  mielifera',
#               'Apidae apis af mielifera (Grooves 2001)  mielifera',
#               '  Apidea apis affin. mielifera (Grooves 2001) mielifera')
#  y <- x
# cleanSciNames(x)
# c <- "\"\"\"Euptychia\"\"\""
#str_replace_all(x, "[^[:alnum:]]", " ")

cleanSciNames  <- function(y){
  

  require(R.utils)
  if (!require(R.utils)) {
    stop("You need to install the 'R.utils' package to use this function")
  }
  
  (y <- gsub("[[:blank:]]"," ",y))
  (y <- gsub("\n"," ",y))
  (y <- gsub("'","", y))
  (y <- gsub('"','', y))
  (y <- gsub('[[:space:]][[:space:]]|[[:space:]][[:space:]][[:space:]]',' ', y))
  (y <- gsub('[[:space:]][[:space:]]|[[:space:]][[:space:]][[:space:]]',' ', y))
  
  (y <- gsub("^[[:space:]][[:space:]]|^[[:space:]]|[[:space:]][[:space:]]$|[[:space:]]$","", y))
  (y <- gsub("^[[:space:]][[:space:]]|^[[:space:]]|[[:space:]][[:space:]]$|[[:space:]]$","", y))

  y <- sapply(y, FUN = function(x){     
    spaces <- gregexpr(" ", x)[[1]]
    if (gregexpr("idae ", x)[[1]][1] != -1 & length(spaces) >= 2){
      famPos <- gregexpr("idae ", x)[[1]]
      x <- substr(x, famPos[1] + attr(famPos,"match.length"), nchar(x))
    }
      
    if (gregexpr("aceae ", x)[[1]][1] != -1 & length(spaces) >= 2){
      famPos <- gregexpr("aceae ", x)[[1]]
      x <- substr(x, famPos[1] + attr(famPos,"match.length"), nchar(x))
    }
    
    if (gregexpr("\\).*", x)[[1]][1] != -1){
      ip <- gregexpr("\\(.*",x)[[1]][1]
      fp <- gregexpr("\\).*",x)[[1]][1]
      x1 <- substr(x, 0, ip - 2)
      x2 <- substr(x, fp + 1, nchar(x))
      x <- paste(x1, x2)
    }
    
    (x <- gsub(" cf\\.| cf ","",x))
    (x <- gsub(" var\\.| var "," ",x))
    (x <- gsub(" sp\\.| sp\\.| sp | spp | sp$","",x))
    (x <- gsub(" aff\\. | affin\\. | aff | affin "," ",x))
    (x <- gsub("  "," ", x))
    (x <- gsub("  "," ", x))
    
    if (!is.na(gregexpr(" ", x)[[1]][2])){
      pos <- gregexpr(" ", x)[[1]][2]
      (x <- substr(x, 0, pos - 1))
    
    }
    return(capitalize(tolower(x)))
  })

  (y <- gsub('‘|’|"|\\*|\\.', '', y))
  (y <- gsub('-', 'XYZ', y))
  (y <- gsub('[[:punct:]]', '', y))
  (y <- as.character(gsub('XYZ', '-', y)))
  y <- gsub("(?!-)[[:punct:]]", "", y, perl=TRUE)
  #"[^'[:lower:] ]"
  return(y)
}
#(a <- t(t(cleanSciNames(testName))))
#(cleanSciNames(x))

# orig2set16
orig2set16 <- function(file.i, format, Source = NULL, occID = NULL, 
                       cleanscinames = TRUE, startID = 0, cleanCoords = FALSE, trashFile = NULL){
  
  #  Se carga el formato 'set16' para que los datos cargados poseriormente sean establecidos con estos estandares
  #  Esta instruccion ya no se cargara desde la funcion sino que se pasara como argumento de la funcion
  
  #format.datos <- read.csv("G:/Cristian_data/Humboldt/Set16/datos_originales/estructuraset16.csv")
  #format <- emptyFormat
  #Source = NULL; occID = NULL; cleanscinames = TRUE; startID = 0; cleanCoords = FALSE
  #file.i <- read.delim("G:/Cristian_data/Humboldt/Set16/datos_originales/VerNet/Vernet 2014 04 11.txt")
  #Source <- "Vernet"; occID <- "w"; trashDir <- 'C:/IAvH'
  
  #  Se crea una tabla vacia con la estructura set 16 que se llenara con la informacion del archivo
  format.i <- as.data.frame(matrix("", nrow(file.i), ncol(format)), stringsAsFactors = FALSE); dim(format.i)
  colnames(format.i) <- colnames(emptyFormat)
  #  Ciclo entre el numero de campos del archivo cargado
  j <-1
  for (j in 1:ncol(format.i)){
    (pos <- which(colnames(file.i) == colnames(format)[j]))
    if (length(pos)!=0 ){
      format.i[, j] <- as.character(file.i[, pos])
      #cat(Source,j,"columnas de",ncol(file.i),"para", ncol(format), "esperadas","\n")
    }
    # head(format.i)
  }
  
  if (!is.null(Source)){
    format.i$'source' <- Source # Agregar fuente de la informacisn
    if (is.null(occID)){
      format.i$occurrenceID <- paste0(Source,"-",startID:(startID+nrow(format.i)-1)) #Creo un ID consecutivo
    }}
  if (!is.null(occID)){
    format.i$occurrenceID <- occID#paste0(occID,"-",startID:(startID+nrow(format.i)-1)) #Creo un ID consecutivo
  }
  
  # Volver formato numirico las variables decimalLatitude, decimalLongitude, alt
  if (cleanCoords != TRUE){
    if (length(which(colnames(format.i) == "decimalLatitude"))>0){
      format.i$decimalLatitude <- as.numeric(format.i$decimalLatitude); format.i$decimalLongitude <- as.numeric(format.i$decimalLongitude)
      # Eliminar los registros que no tengan valores en coordenadas en caso de haber
      no_coords <- unique(append(which(is.na(format.i$decimalLatitude)),which(is.na(format.i$decimalLongitude))))
      if (length(no_coords)>0) {format.i <- format.i[-no_coords,]}
    }
  }
  
  # Cambiar a 2 palabras el nombre cientifico
  if  (cleanscinames == TRUE){
    naNames <- which(is.na(format.i$speciesOriginal))
    if (any(naNames)){
      format.i <- format.i[!is.na(format.i$speciesOriginal), ]
    }
    format.i$species <- cleanSciNames(gsub("_", " ", format.i$speciesOriginal))
  } else {
    format.i$species <- format.i$speciesOriginal
  }
  
  if (!is.null(trashFile)){
    trashCols <- which(!(colnames(file.i) %in% colnames(format.i)))
    if (length(trashCols)>0) {
      trashTable <- file.i[, trashCols]
      if (length(trashCols)==1) {
        trashTable <- as.data.frame(trashTable)
      }
      trash <- apply(trashTable, 2, FUN = function(x) {
        uniqueVals <- unique(x)
        uniqueVals <- uniqueVals[!is.na(uniqueVals) & uniqueVals != '' & uniqueVals != ' '][1:6]
      })
      dir.create('Trash', showWarnings = FALSE)
      write.csv(trash, file = paste0('Trash/Trash_', trashFile, '.csv'))
    }
  }
  return(format.i)
}

# Rescate de localidades
grep2 <- function(y, line){
  patterns <- c(paste0(c(" ","-"), tolower(y)), paste0(tolower(y),c("."," ","-")))
  multigrep <- sapply(patterns, FUN = function(x){
    g1 <- grep(x, tolower(line))
  })
  pos <- which(tolower(line) == tolower(y))
  return(unique(c(unlist(multigrep), pos)))
}


overaoi <- function(tabla_final, name, aoi, dir2 = FALSE, noCoordDir = FALSE){
  #tabla_final <- tabla_final5
  ## name <- bd
  #noCoordDir <- 'G:/Cristian_data/Humboldt/Set16/datos_originales/noCoords/'
  tabla_salida <- tabla_final
  cat('Converting matriz to spatial object\n')
  tabla_final<-vect(tabla_final, geom = c('decimalLongitude', 'decimalLatitude'), crs="+proj=longlat +datum=WGS84")
  cat('DONE\n')

  ov.table<-extract(aoi, tabla_final)[,-1]
  salida.id <- which(!is.na(ov.table))
  
  cat('Creating points plot by', name, 'data\n')
  png(paste0(name,'-',Sys.Date(),".png"), width = 1000, height = 700, units = 'px')
  plot(wrld_simpl, xlim = c(-120,120), ylim = c(-80,80), axes=TRUE, col='light yellow', main = name)
  #plot(mapWorld, add=T)
  plot(tabla_final,  col='orange', cex=0.75, pch = 20, add = T)
  plot(aoi, col=rgb(0.5,0.3,0.4,0.1), add=T)
  points(tabla_salida$decimalLongitude[salida.id], tabla_salida$decimalLatitude[salida.id], col='blue', cex=0.5, pch = 20)
  text(x=10, y=-50, name, cex = 3)
  box()  
  dev.off()
  cat('DONE\n')

  dir.create(as.character(Sys.Date()), showWarnings = FALSE)
  
  file.copy(paste0(name,".png"), paste0(Sys.Date(),"/",name,".png"))
  if (dir2 != FALSE){
    file.copy(paste0(name,".png"),paste0(dir2,"/",name,'-',Sys.Date(),".png"))
  }
  if (noCoordDir != FALSE){
    if (!is.null(nrow(salida.id))){
    noCoordTable <- tabla_salida[-salida.id, ]
    head(noCoordTable)
    save(noCoordTable, file = paste0(noCoordDir,'/', name,'_nocoords_',Sys.Date(),".RData")
    )
    }
  }
  return(tabla_salida[salida.id, ])
}

#textVector <- colnames(archivo.i); from = oriColnames; to = finColnames
#textVector == colnames(archivo.i); oriColnames == finColnames
#colnames(archivo.i) == multiReplace(colnames(archivo.i), from = oriColnames, to = finColnames)
multiReplace <- function(textVector, from, to){
  eq <- which(from != to)
  for (r in 1:length(eq)){
    #grep(pattern = paste0('^', from[i],'$'), x = textVector)
    textVector <- gsub(pattern = from[eq][r], #paste0('^', from[i],'$') 
                   replacement = , to[eq][r], 
                   x = textVector)
  }
  return(textVector)
}

##Function for add taxonRank in Data Frame

#By: Cristian Cruz-Rodr[i]guez 
#Date: 2-12-2022

' Revisa las columnas taxonómicas e indica el nivel taxonómico de la determinación
#' 
#' @param x matriz de datos en formato Data Frame
#' @return matriz de datos con la validación del nivel taxonómico de la informaci[o]n suministrada
#' @examples
#' @details requiere la la matriz de datos tenga las columnas scientificName, specificEpithet, genus, family, order, phylum y kingdom con los mismos nombres

taxrank <- function(x) {
  library(stringi)
  
  if(!class(x)== 'data.frame')
    stop('add table  in data frame format')
  else{
    
    ### remove spaces in matriz
    cat('remove spaces in matriz  ')
    for (h in 1:ncol(x)) {
      x[,h]<- trimws(x[,h])
    }
    cat('DONE \n')
    if(!"verbatimIdentification" %in% colnames(x)){
      colnames(x)[colnames(x) == "scientificName"] ="verbatimIdentification"
      x$scientificName<- ''
    } else NULL
    
    
    x[x == ""] <- NA
    cat('Review Taxon Rank for',nrow(x), 'records \n')
    progress_bar = txtProgressBar(min=0, max=nrow(x), style = 3, char="=")
    for (i in 1:nrow(x)) {
      ifelse(((is.na(x[i,'verbatimIdentification'])) && (!is.na(x[i,'scientificName']))), (x[i,'verbatimIdentification'] <- x[i,'scientificName']), NA )
      
      if (!is.na(x[i,'specificEpithet'])){
        x[i,'scientificName'] <-paste(x[i,'genus'], x[i,'specificEpithet'])
        x[i,'taxonRank'] <-'specie'
      } else if ((!is.na(x[i,'genus'])) && (is.na(x[i,'specificEpithet']))) {
        x[i,'scientificName'] <-x[i,'genus']
        x[i,'taxonRank'] <-'genus'
      }else if (!is.na(x[i,'family'])) {
        x[i,'scientificName'] <-x[i,'family']
        x[i,'taxonRank'] <-'family'
      }else if (!is.na(x[i,'order'])) {
        x[i,'scientificName'] <-x[i,'order']
        x[i,'taxonRank'] <-'order'
      }else if (!is.na(x[i,'class'])) {
        x[i,'scientificName'] <-x[i,'class']
        x[i,'taxonRank'] <-'class'
      }else if (!is.na(x[i,'phylum'])) {
        x[i,'scientificName'] <-x[i,'phylum']
        x[i,'taxonRank'] <-'phylum'
      }else if (!is.na(x[i,'kingdom'])) {
        x[i,'scientificName'] <-x[i,'kingdom']
        x[i,'taxonRank'] <-'kingdom'
      } else if (is.na(x[i,'kingdom'])) {
        x[i,'scientificName'] <- 'negative'
        x[i,'taxonRank'] <-'negative'  
        
      }
      setTxtProgressBar(progress_bar, value = i) 
    }
    close(progress_bar)
    return(x)
  }
}

