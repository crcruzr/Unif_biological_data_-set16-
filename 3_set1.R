# set 1
#rm(list = ls())
setwd('D:/Set16/set1')
gc()
memory.limit(size = 1000000000)

# Load functions
library(taxize)
library(dplyr)
library(maditr)
library(tibble)
library(plyr)

source('G:/Cristian_data/Humboldt/Git/Unif_biological_data_-set16-/addicional_data/tools.R')

#--------------------------------------------------------------------#
# 1. Load data and do original name cleanup and convert to binomial  #
#--------------------------------------------------------------------#
load('D:/Set16/set0/set0.RData')

system.time(
  cofTable <- t(sapply(registros$scientificName, FUN = function(y){
    c(y, strsplit(y, ' ')[[1]][1:2])
    }))
)# elapsed 253.01 for 14kk

id <- 1:nrow(cofTable)
cofTable <- data.frame(id, cofTable[, 1], cofTable[, 2], cofTable[, 3], row.names = id, stringsAsFactors = FALSE)
colnames(cofTable) <- c("id","nombre", "genero","epiteto_especifico")

#---------------------------------------#
# 2.Remove  celds but only blackspaces  # 
#---------------------------------------#
registros$scientificName_dep <- cleanSciNames(registros$scientificName)

names_spp<-(unique(registros$scientificName_dep))
length(names_spp)#268721
rm.emp<-(grepl("^\\s*$", names_spp))
names_spp<-as.data.frame(names_spp)
names_spp$rm.empty<-as.character(rm.emp)
names_spp<-names_spp[names_spp$rm.empty ==FALSE,]
names_spp<-as.character(names_spp$names_spp)
length(names_spp)#333294

#----------------------------------------------------------------------------------------------#
# 3. Review the accepted name with the bd present in Taxize package (in this case is only COL)  #
#----------------------------------------------------------------------------------------------#
acc_names<-data.frame(user_supplied_name = character(),
                      submitted_name = character(),
                      data_source_title = character(),
                      score = numeric(),
                      matched_name2 = character(),
                      stringsAsFactors = FALSE)

#load(file = paste0("acc_names.RData"))

#Nota, en el 2023 la conexión hizo que fallara el analisis de corrido. Por ello se debe retomar en el último número que se revitó, para no reniciar el proceso
for(i in 199027:length(names_spp)){
  acc_names.i<-gnr_resolve(names=names_spp[i], best_match_only=TRUE, canonical =T, preferred_data_sources = 11)
  if (!is.null(acc_names.i ))
        acc_names<-rbind(acc_names, acc_names.i)
}
#17 times dont connect with DB in 2023
nrow(acc_names)#9719
save(acc_names, file = paste0("acc_names2.RData"))
#load(file = "acc_names2.RData")

#------------------------------#
# 4. Species that arent verified  #
#------------------------------#

mtch<-names_spp %in% acc_names$submitted_name
sp_lost<-names_spp[-which(mtch)]
sp_lost <- as.data.frame(sp_lost[2:length(sp_lost)])
  colnames(sp_lost)<-'scientificName'
sp_lost$kingdom <-'not evaluated'
sp_lost$phylum <-'not evaluated'
sp_lost$class <-'not evaluated'
sp_lost$order <-'not evaluated'
sp_lost$family <-'not evaluated'
sp_lost$genus <-'not evaluated'
sp_lost$taxonomicStatus <- "invalid"
sp_lost$Validsource <- "GBIF Backbone Taxonomy. https://doi.org/10.15468/39omei"


sp_lost<-sp_lost[, c(2,3,4,5,6,7,1,8,9)] # reorder columns

#table(sp_lost)
#out<-gnr_datasources() # verify was been updated the datasets

#----------------------------------------------------#
# 5. Remove duplicates and generate the higher taxa  #
#----------------------------------------------------#
mached_mane_uni<-as.data.frame(unique(acc_names$matched_name2))
#Nota, en el 2023 se dividió en dos grupos, porque la conexión a internet impedia hacerlo todo de corrido. No obstante, en años anteriores funcionó sin problemas
acc_names2<-head(acc_names, 62136)
system.time(clas_spp2<-classification(acc_names2$matched_name2, db =  "gbif", rows = 1))#62136

acc_names3<-tail(acc_names, 153789)
system.time(clas_spp3<-classification(acc_names3$matched_name2, db =  "gbif", rows = 1))# 958 Minutos - 16 horas/ 898 minutos - 15 horas -> 855 minutos 14.2 horas en 2021

#----------------------#
# 6. extract the data  #
#----------------------#
clas_spp_table2<-do.call(rbind.data.frame, clas_spp2) # 16 seg
clas_spp_table3<-do.call(rbind.data.frame, clas_spp3) # 16 seg
clas_spp_table<-rbind(clas_spp_table2, clas_spp_table3)

#------------------------------------------------------------------#
# 7. For the analysis, is necesary that the rownames has a column  #
#------------------------------------------------------------------#
clas_spp_table2<-rownames_to_column(clas_spp_table, "species")

#--------------------------------------------------------------------#
# 8. Eliminate the number in the species
#--------------------------------------------------------------------#
clas_spp_table2$species_gr <-substr(clas_spp_table2$species, 1, nchar(clas_spp_table2$species)-2) 

#-----------------------------------------------#
# 9. Define the categories to group the species #
#-----------------------------------------------#
clas_spp_table2$rank <- factor(clas_spp_table2$rank, levels=c("kingdom", "phylum", "class",  "order",  "family", "superfamily",  "genus", "species"),
                               labels=c("kingdom",  "phylum", "class",  "order",  "family", "superfamily", "genus", "species"))

#----------------------------#
# 10. remove duplicated spp  #
#----------------------------#
clas_spp_table2<-dplyr::filter(clas_spp_table2, !grepl('\\.', species_gr))
# 2023-  Species Gr_duplicated that afect the process
#clas_spp_table2<- clas_spp_table2[!clas_spp_table2$species %in%  'Arthrodesmus2',]
#clas_spp_table2<- clas_spp_table2[!clas_spp_table2$species %in%  'Pileata',]

#------------------------------------------------#
# 11. Generate table with taxonomic information  #
#------------------------------------------------#
clas_spp_fin<-arrange(dcast(clas_spp_table2, species_gr ~ rank, value.var = "name"), species_gr)
clas_spp_fin<-clas_spp_fin[,-1]
tax_spp<-rbind(clas_spp_fin, sp_lost)
dt<-ls()
dt<-(dt[!grepl('tax_spp', dt)])
rm(list = dt); rm(dt)
#load(tax_spp, file ='G:/Cristian_data/Humboldt/Set16/set1/valid_taxonomic_status.RData')
load('D:/Set16/set0/set0.RData')

#-------------------------------------------#
## 12. Add specific Epithet and taxon rank  #
#-------------------------------------------#
Spe_ephi<-as.data.frame(stringr::str_split_fixed(tax_spp$species, ' ', 2))
tax_spp$specificEpithet<-Spe_ephi$V2
tax_spp<-taxrank(as.data.frame(tax_spp))

tax_spp<-tax_spp[!tax_spp$taxonRank %in% 'negative',]
tax_spp<-tax_spp[,-7]#remove species column
names(tax_spp)<-c('suggested_kingdom', 'suggested_phylum', 'suggested_class', 'suggested_order', 'suggested_family', 'suggested_genus',
                  'suggested_speEpithet','scientificName',  'taxonRank')
tax_spp$taxonomicStatus <- "accepted"
tax_spp$Validsource <- "GBIF Backbone Taxonomy. https://doi.org/10.15468/39omei"
registros$ID_T<- 1:nrow(registros)
set1<-join(registros, tax_spp, by = 'scientificName', type = 'left')
set1<-set1[!duplicated(set1$ID_T), ]
set1<- set1[,!names(set1) %in% 'ID_T']
class(set1)

#-----------------------#
# 13. Save the results  #
#-----------------------#
# getwd()
setwd('D:/Set16/set1/')
save(file = 'set1.RData', set1)
#save(file = 'set1_biom&odon2016-08-12.RData', set1)
file.copy('set1.RData', paste0("set1_", Sys.Date(), ".RData"))

##### END Step 1 ###########
