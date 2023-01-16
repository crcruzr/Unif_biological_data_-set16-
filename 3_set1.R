# set 1
#rm(list = ls())
setwd('G:/Cristian_data/Humboldt/Set16/set1')
gc()
memory.limit(size = 1000000000)

# Load functions
library(taxize)
library(dplyr)
library(maditr)
library(tibble)
library(plyr)

source("G:/Cristian_data/Humboldt/Set16/scripts/tools.R")
#source('C:/Users/aves/Documents/GitHub/dataDownload/nameValidation.R')

# Cargar datos y hacer limpieza del nombre original y convertir en binomial
load('G:/Cristian_data/Humboldt/Set16/set0/set0.RData')


# Crear tabla a partir de vector con s[o]lo nombres
system.time(
  cofTable <- t(sapply(registros$species, FUN = function(y){
    c(y, strsplit(y, ' ')[[1]][1:2])
    }))
)# elapsed 253.01 for 14kk

id <- 1:nrow(cofTable)
cofTable <- data.frame(id, cofTable[, 1], cofTable[, 2], cofTable[, 3], row.names = id, stringsAsFactors = FALSE)
colnames(cofTable) <- c("id","nombre", "genero","epiteto_especifico")

#remove  celds but only blackspaces
names_spp<-(unique(registros$species))
length(names_spp)#184346
rm.emp<-(grepl("^\\s*$", names_spp))
names_spp<-as.data.frame(names_spp)
names_spp$rm.empty<-as.character(rm.emp)
names_spp<-names_spp[names_spp$rm.empty ==FALSE,]
names_spp<-as.character(names_spp$names_spp)
length(names_spp)#184344

#2. Review the accepted name with the bd present in Taxize package (in this case is only COL)

acc_names<-data.frame(user_supplied_name = character(),
                      submitted_name = character(),
                      data_source_title = character(),
                      score = numeric(),
                      matched_name2 = character(),
                      stringsAsFactors = FALSE)
i<-1
for(i in 182202:length(names_spp)){
  acc_names.i<-gnr_resolve(names=names_spp[i], best_match_only=TRUE, canonical =T, preferred_data_sources = 11)
  if (!is.null(acc_names.i ))
        acc_names<-rbind(acc_names, acc_names.i)
}
nrow(acc_names)#176514

# spp that aren?t verified
mtch<-names_spp %in% acc_names$submitted_name
sp_lost<-names_spp[-which(mtch)]
sp_lost <- as.data.frame(sp_lost[2:length(sp_lost)])
colnames(sp_lost)<-'species'
sp_lost$kingdom <-'no data avaliable'
sp_lost$phylum <-'no data avaliable'
sp_lost$class <-'no data avaliable'
sp_lost$order <-'no data avaliable'
sp_lost$family <-'no data avaliable'
sp_lost$genus <-'no data avaliable'
sp_lost$taxonomicStatus <- "invalid"
sp_lost$Validsource <- "GBIF Backbone Taxonomy. https://doi.org/10.15468/39omei"


sp_lost<-sp_lost[, c(2,3,4,5,6,7,1,8,9)] # reorder columns

#table(sp_lost)
#out<-gnr_datasources() # verify was been updated the datasets


# Remove duplicates
mached_mane_uni<-as.data.frame(unique(acc_names$matched_name2))
#3. Generate the higher taxa
system.time(clas_spp<-classification(acc_names$matched_name2, db =  "gbif", rows = 1))# 958 Minutos - 16 horas/ 898 minutos - 15 horas -> 855 minutos 14.2 horas en 2021

1# 4. extract the data 
system.time(clas_spp_table<-do.call(rbind.data.frame, clas_spp)) # 16 seg

#5. For the analysis, is necesary that the rownames has a column
clas_spp_table2<-rownames_to_column(clas_spp_table, "species")

# 6. Eliminate the number in the species
clas_spp_table2$species_gr <-substr(clas_spp_table2$species, 1, nchar(clas_spp_table2$species)-2) 

#. 7 Define the categories to group the species

clas_spp_table2$rank <- factor(clas_spp_table2$rank, levels=c("kingdom", "phylum", "class",  "order",  "family", "superfamily",  "genus", "species"),
                               labels=c("kingdom",  "phylum", "class",  "order",  "family", "superfamily", "genus", "species"))


### remove duplicated spp
clas_spp_table2<-dplyr::filter(clas_spp_table2, !grepl('\\.', species_gr))

#Generate table with taxonomic information
clas_spp_fin<-arrange(dcast(clas_spp_table2, species_gr ~ rank, value.var = "name"), species_gr)

#clas_spp_fin_2 <- cbind(clas_spp_fin, "speciesoriginal"=acc_names$submitted_name)

clas_spp_fin$taxonomicStatus <- "accepted"
clas_spp_fin$Validsource <- "GBIF Backbone Taxonomy. https://doi.org/10.15468/39omei"
#names(clas_spp_fin$NA)<- "Subspecies"

clas_spp_fin<-clas_spp_fin[,-1]


tax_spp<-rbind(clas_spp_fin, sp_lost)

dt<-ls()
dt<-(dt[!grepl('tax_spp', dt)])
rm(list = dt); rm(dt)

#load(tax_spp, file ='G:/Cristian_data/Humboldt/Set16/set1/valid_taxonomic_status.RData')
load('G:/Cristian_data/Humboldt/Set16/set0/set0.RData')

registros$ID_T<- 1:nrow(registros)
set1<-join(registros, tax_spp, by = 'species', type = 'left')
set1<-set1[!duplicated(set1$ID_T), ]
set1<- set1[,!names(set1) %in% 'ID_T']

class(set1)

## Para proceso del set16
# getwd()
setwd('G:/Cristian_data/Humboldt/Set16/set1')
save(file = 'set1.RData', set1)
#save(file = 'set1_biom&odon2016-08-12.RData', set1)
file.copy('set1.RData', paste0("set1_", Sys.Date(), ".RData"))

#  ## Contar especies aceptadas con mas de 10 registros
# spCountTable <- table(set1$nombre_aceptado)
# spCountTable <- t(t(spCountTable))
# sum(spCountTable >= 10)
