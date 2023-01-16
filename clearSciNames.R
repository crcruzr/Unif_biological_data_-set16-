


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
  
  (y <- gsub('â|â|"|\\*|\\.', '', y))
  (y <- gsub('-', 'XYZ', y))
  (y <- gsub('[[:punct:]]', '', y))
  (y <- as.character(gsub('XYZ', '-', y)))
  y <- gsub("(?!-)[[:punct:]]", "", y, perl=TRUE)
  #"[^'[:lower:] ]"
  return(y)
}