setwd("~/Library/CloudStorage/OneDrive-ImperialCollegeLondon/MSc EEC/Project")

library(FD)
library(tidyverse)

#Import data

LandUse_Data <- read.csv('LandUse_Data.csv')

BirdTraits <- read.csv('AVONET_Ebird.csv')

PCA_values <- read.csv("PCA_values.csv")
PCA_values <- PCA_values %>% dplyr::select(c(-"X"))

#Merge bird traits and land use/point count data
Trait_and_LU <- left_join(LandUse_Data, BirdTraits, by = c("Ebird_Species" = "Species2"))

#Set habitat and trophic niche as factors
Trait_and_LU$Habitat <- as.factor(Trait_and_LU$Habitat)
Trait_and_LU$Trophic.Niche <- as.factor(Trait_and_LU$Trophic.Niche)

Abundance_data <- Trait_and_LU %>% 
  ## group by site and birdlife name a occasionally the same species pops up in the same site twice.
  dplyr::group_by(Site_ID, Ebird_Species) %>% dplyr::mutate(n_spp = n()) %>%
  ## filter so that species isn't duplicated and then ungroup 
  filter(!duplicated(n_spp)| n_spp == 1) %>% ungroup() %>% 
  #### group by site and calculate metrics of how many species are in each site and the total site abundance
  group_by(Transect,Ebird_Species) %>% dplyr::mutate(Abundance = n()) %>%
  ungroup() %>% 
  ## droplevels
  droplevels() %>%
  #as data frame
  data.frame()

#Get abundance of each bird per transect
All_birds <- Abundance_data %>% dplyr::arrange(Transect, Ebird_Species) %>%
  dplyr::group_by(Transect, Ebird_Species) %>% dplyr::mutate(Transect_abundance = sum(Abundance))

All_birds <- All_birds %>% dplyr::select(c(-"X"))

All_birds2 <- All_birds %>% dplyr::select(c("Transect", "Ebird_Species","Abundance")) %>% distinct()

All_birds$Habitat.Type <- factor(All_birds$Habitat) # First step: copy vector and make it factor
# Change levels:
levels(All_birds$Habitat.Type) <- list(Forest = c("Forest","Woodland"),
                                       `Non Forest` = c("Shrubland","Grassland","Wetland","Riverine","Human Modified","Desert","Rock"))


##Land use data##

#Get mean forest cover
transect_forest_cover <- All_birds %>% ungroup() %>% dplyr::select(c("Transect", "ForestCover200m":"ForestCover5000m")) 
transect_forest_cover <- transect_forest_cover %>%  group_by(Transect) %>%
  dplyr::summarise(MeanForestCover200m=mean(ForestCover200m), MeanForestCover500m=mean(ForestCover500m),
                   MeanForestCover1000m=mean(ForestCover1000m), MeanForestCover3000m=mean(ForestCover3000m), 
                   MeanForestCover5000m=mean(ForestCover5000m))

#Get number of trees in each transect
transect_tree_counts <- All_birds %>% ungroup() %>% dplyr::select(c("Transect","TreeML")) %>%  group_by(Transect) %>%
  dplyr::summarise(Trees = sum(TreeML))


#Join both
transect_LU <- left_join(transect_forest_cover, transect_tree_counts, by = "Transect")

#Get season and locality for each transect

transect_season_and_locality <- All_birds %>% ungroup() %>% dplyr::select(c("Transect","Locality","Season","Transect_Type")) %>%  group_by(Transect) %>% distinct() 

transect_LU <- left_join(transect_LU, transect_season_and_locality, by = "Transect")

write.csv(transect_LU, "Land_Use_BySite.csv")

Overall_Species_Richness <- TraitData_Hansen %>% 
  dplyr::select(c("Transect","Ebird_Species","Trophic.Niche","Habitat"))
                              
Overall_Species_Richness <- Overall_Species_Richness %>% 
  ## group by site and birdlife name a occasionally the sme species pops up in the same site twice.
  dplyr::group_by(Transect, Ebird_Species) %>% dplyr::mutate(n_spp = n()) %>%
  ## filter so that species isn't duplicated and then ungroup 
  filter(!duplicated(n_spp)| n_spp == 1) %>% ungroup() %>%
  #### group by site and calculate metrics of how many species are in each site and the total site abundance
  group_by(Transect) %>% dplyr::mutate(Richness = n_distinct(Ebird_Species)) %>%
  ungroup() %>% filter(Richness > 1) %>%
  ## droplevels
  droplevels() %>%
  #as data frame
  data.frame()

Species_Richness_Data <- Overall_Species_Richness %>% 
  dplyr::select(c("Transect","Richness")) %>% 
  dplyr::distinct() %>% 
  left_join(transect_LU, by = "Transect")


#SR by trophic niche#

Species_Richness_Trophic <- Overall_Species_Richness %>% 
  dplyr::select(c("Transect","Ebird_Species","Trophic.Niche","Habitat")) %>% 
  group_by(Transect,Trophic.Niche) %>% dplyr::mutate(Richness = n()) %>% 
  ungroup() %>% 
  ## droplevels
  droplevels() %>%
  #as data frame
  data.frame()

Species_Richness_Trophic_Data <- Species_Richness_Trophic %>% 
  dplyr::select(c("Transect","Trophic.Niche","Richness")) %>% 
  dplyr::distinct() %>%
  left_join(transect_LU, by = "Transect")

#SR by habitat type

#Overall

Overall_Species_Richness$Habitat.Type <- factor(Overall_Species_Richness$Habitat) # First step: copy vector and make it factor
# Change levels:
levels(Overall_Species_Richness$Habitat.Type) <- list(Forest = c("Forest","Woodland"),
                                                      `Non Forest` = c("Shrubland","Grassland","Wetland","Riverine","Human Modified","Desert","Rock"))

Species_Richness_Habitat <- Overall_Species_Richness %>% 
  dplyr::select(c("Transect","Ebird_Species","Trophic.Niche","Habitat.Type")) %>% 
  group_by(Transect,Habitat.Type) %>% dplyr::mutate(Richness = n()) %>% 
  ungroup() %>% 
  ## droplevels
  droplevels() %>%
  #as data frame
  data.frame()

Species_Richness_Habitat_Overall_Data <- Species_Richness_Habitat %>% 
  dplyr::select(c("Transect","Habitat.Type","Richness")) %>% 
  dplyr::distinct() %>% 
  left_join(transect_LU, by = "Transect")

#Invertivores

Species_Richness_Habitat_Invert <- Overall_Species_Richness %>% 
  dplyr::select(c("Transect","Ebird_Species","Trophic.Niche","Habitat.Type")) %>% 
  subset(Trophic.Niche=="Invertivore") %>%
  group_by(Transect,Habitat.Type) %>% dplyr::mutate(Richness = n()) %>% 
  ungroup() %>% 
  ## droplevels
  droplevels() %>%
  #as data frame
  data.frame()

Species_Richness_Habitat_Invert_Data <- Species_Richness_Habitat_Invert %>% 
  dplyr::select(c("Transect","Habitat.Type","Richness")) %>% 
  dplyr::distinct() %>% 
  left_join(transect_LU, by = "Transect")

###########################
###Functional Dispersion###
###########################

######
##Overall species functional dispersion##
######

#Merge bird traits and land use/point count data
Trait_and_LU <- left_join(LandUse_Data, BirdTraits, by = c("Ebird_Species" = "Species2"))

#Set habitat and trophic niche as factors
Trait_and_LU$Habitat <- as.factor(Trait_and_LU$Habitat)
Trait_and_LU$Trophic.Niche <- as.factor(Trait_and_LU$Trophic.Niche)

Abundance_data <- Trait_and_LU %>% 
## group by site and birdlife name a occasionally the same species pops up in the same site twice.
dplyr::group_by(Site_ID, Ebird_Species) %>% dplyr::mutate(n_spp = n()) %>%
## filter so that species isn't duplicated and then ungroup 
filter(!duplicated(n_spp)| n_spp == 1) %>% ungroup() %>% 
#### group by site and calculate metrics of how many species are in each site and the total site abundance
group_by(Transect,Ebird_Species) %>% dplyr::mutate(Abundance = n()) %>%
ungroup() %>% 
## droplevels
droplevels() %>%
#as data frame
data.frame()

#Get abundance of each bird per transect
All_birds <- Abundance_data %>% dplyr::arrange(Transect, Ebird_Species) %>%
  dplyr::group_by(Transect, Ebird_Species) %>% dplyr::mutate(Transect_abundance = sum(Abundance))

All_birds <- All_birds %>% dplyr::select(c(-"X"))

All_birds2 <- All_birds %>% dplyr::select(c("Transect", "Ebird_Species","Abundance")) %>% distinct()

All_birds$Habitat.Type <- factor(All_birds$Habitat) # First step: copy vector and make it factor
# Change levels:
levels(All_birds$Habitat.Type) <- list(Forest = c("Forest","Woodland"),
                                       `Non Forest` = c("Shrubland","Grassland","Wetland","Riverine","Human Modified","Desert","Rock"))

  
#Convert so that rows are sites and columns are species
Birds_in_sites <- All_birds2 %>% tidyr::pivot_wider(names_from = 'Ebird_Species', values_from = 'Abundance') 
Birds_in_sites[is.na(Birds_in_sites)] = 0
Birds_in_sites <- Birds_in_sites[ -c(1) ]
Birds_in_sites <- Birds_in_sites[,order(colnames(Birds_in_sites))]
  
#Make species the rownames
PCA_FuncMetrics <- PCA_values[,-1]
rownames(PCA_FuncMetrics) <- PCA_values[,1]
colnames(Birds_in_sites)

#Calculate functional metrics
FD <- dbFD(PCA_FuncMetrics,Birds_in_sites, w.abun = T)
summary(FD)

#Add all sites to a vector
Sites <- All_birds2[,1]
Sites <- unique(Sites)
  
FRich <- FD$FRic 
FEve <- FD$FEve
FDis <- FD$FDis
FDiv <- FD$FDiv
  
#Create data frame with sites, functional metrics and land use
FD_data <- data.frame(Sites,FRich,FEve,FDis,FDiv)

FD_data_Overall <- left_join(FD_data,transect_LU, by = "Transect")

######
##Invertivore species functional dispersion##
######

All_invert_birds <- All_birds %>% subset(Trophic.Niche=="Invertivore")

All_birds2_invert <- All_invert_birds %>% dplyr::select(c("Transect", "Ebird_Species","Abundance")) %>% distinct()

#Convert so that rows are sites and columns are species
Birds_in_sites_invert <- All_birds2_invert %>% tidyr::pivot_wider(names_from = 'Ebird_Species', values_from = 'Abundance') 
Birds_in_sites_invert[is.na(Birds_in_sites_invert)] = 0
Birds_in_sites_invert <- Birds_in_sites_invert[ -c(1) ]
Birds_in_sites_invert <- Birds_in_sites_invert[,order(colnames(Birds_in_sites_invert))]

##PCA values for invertivores

## filter out any species that need to be dropped 

TPD_data_Invert <- Trait_and_LU %>% 
  ## group by site and birdlife name a occasionally the sme species pops up in the same site twice.
  dplyr::group_by(Transect, Ebird_Species) %>% dplyr::mutate(n_spp = n()) %>%
  ## filter so that species isn't duplicated and then ungroup 
  filter(!duplicated(n_spp)| n_spp == 1) %>% ungroup() %>%
  #### group by site and calculate metrics of how many species are in each site and the total site abundance
  group_by(Transect) %>% dplyr::mutate(Site_spp = n_distinct(Ebird_Species)) %>%
  ungroup() %>% filter(Site_spp > 1) %>% subset(Trophic.Niche == "Invertivore") %>%
  ## droplevels
  droplevels() %>%
  #as data frame
  data.frame()

TPD_data_Invert <- TPD_data_Invert %>% dplyr::select(c(-"X"))

#Create a column with just invertivores
Invert_Species <- data.frame(unique(TPD_data_Invert$Ebird_Species))
names(Invert_Species)[names(Invert_Species) == "unique.TPD_data_Invert.Ebird_Species."] <- 'Ebird_Species'
colnames(Invert_Species)

#Get the PCA values for just invertebrates
PCA_values_Invert <- left_join(Invert_Species, PCA_values, by = "Ebird_Species")

#Make species the rownames
PCA_values_Invert2 <- PCA_values_Invert #%>% dplyr::select(-c("Bodysize_sd","Trophic_sd","Locomotory_sd"))

#Order species alphabetically
PCA_values_Invert2 <- PCA_values_Invert2[order(PCA_values_Invert2$Ebird_Species),]

PCA_FuncMetrics_invert <- PCA_values_Invert2[,-1]
rownames(PCA_FuncMetrics_invert) <- PCA_values_Invert2[,1]

#Calculate functinal metrics
FD_invert <- dbFD(PCA_FuncMetrics_invert,Birds_in_sites_invert, w.abun = T)

#Add all sites to a vector
Sites <- All_birds2_invert[,1]
Sites <- unique(Sites)

FRich <- FD_invert$FRic 
FEve <- FD_invert$FEve
FDis <- FD_invert$FDis
FDiv <- FD_invert$FDiv

#Create data frame with sites, functional metrics and land use
FD_data_invert <- data.frame(Sites,FRich,FEve,FDis,FDiv)

FD_data_invert <- left_join(FD_data_invert,transect_LU, by = "Transect")

######
##Granivore species functional dispersion##
######

All_Gran_birds <- All_birds %>% subset(Trophic.Niche=="Granivore")

All_birds2_gran <- All_Gran_birds %>% dplyr::select(c("Transect", "Ebird_Species","Abundance")) %>% distinct()

#Convert so that rows are sites and columns are species
Birds_in_sites_gran <- All_birds2_gran %>% tidyr::pivot_wider(names_from = 'Ebird_Species', values_from = 'Abundance') 
Birds_in_sites_gran[is.na(Birds_in_sites_gran)] = 0
Birds_in_sites_gran <- Birds_in_sites_gran[ -c(1) ]
Birds_in_sites_gran <- Birds_in_sites_gran[,order(colnames(Birds_in_sites_gran))]

#Get granivore PCA values 

Gran_Species <- data.frame(unique(All_Gran_birds$Ebird_Species))
colnames(Gran_Species)
names(Gran_Species)[names(Gran_Species) == "unique.All_Gran_birds.Ebird_Species."] <- 'Ebird_Species'

#Get the PCA values for just invertebrates
PCA_values_Gran <- left_join(Gran_Species, PCA_values, by = "Ebird_Species")

#Make species the rownames
PCA_values_Gran2 <- PCA_values_Gran #%>% dplyr::select(-c("Bodysize_sd","Trophic_sd","Locomotory_sd"))

#Order species alphabetically
PCA_values_Gran2 <- PCA_values_Gran2[order(PCA_values_Gran2$Ebird_Species),]

PCA_FuncMetrics_gran <- PCA_values_Gran2[,-1]
rownames(PCA_FuncMetrics_gran) <- PCA_values_Gran2[,1]

#Calculate functinal metrics
FD_gran <- dbFD(PCA_FuncMetrics_gran,Birds_in_sites_gran, w.abun = T)

#Add all sites to a vector
Sites <- All_birds2_gran[,1]
Sites <- unique(Sites)

FRich <- FD_gran$FRic 
FEve <- FD_gran$FEve
FDis <- FD_gran$FDis #FDis: Equals 0 in communities with only one functionally singular species.
FDiv <- FD_gran$FDiv

#Create data frame with sites, functional metrics and land use
FD_data_gran <- data.frame(Sites,FRich,FEve,FDis,FDiv)

FD_data_gran <- left_join(FD_data_gran,transect_LU, by = "Transect")

######################
#FDis by habitat type#
######################

######
#Overall forest species
######

All_OF_birds <- All_birds %>% subset(Habitat.Type=="Forest")

All_birds2_of <- All_OF_birds %>% dplyr::select(c("Transect", "Ebird_Species","Abundance")) %>% distinct()

#Convert so that rows are sites and columns are species
Birds_in_sites_of <- All_birds2_of %>% tidyr::pivot_wider(names_from = 'Ebird_Species', values_from = 'Abundance') 
Birds_in_sites_of[is.na(Birds_in_sites_of)] = 0
Birds_in_sites_of <- Birds_in_sites_of[ -c(1) ]
Birds_in_sites_of <- Birds_in_sites_of[,order(colnames(Birds_in_sites_of))]

#Get overall forest species PCA values 

OF_Species <- data.frame(unique(All_OF_birds$Ebird_Species))
colnames(OF_Species)
names(OF_Species)[names(OF_Species) == "unique.All_OF_birds.Ebird_Species."] <- 'Ebird_Species'

#Get the PCA values for just invertebrates
PCA_values_OF <- left_join(OF_Species, PCA_values, by = "Ebird_Species")

#Make species the rownames
PCA_values_OF2 <- PCA_values_OF #%>% dplyr::select(-c("Bodysize_sd","Trophic_sd","Locomotory_sd"))

#Order species alphabetically
PCA_values_OF2 <- PCA_values_OF2[order(PCA_values_OF2$Ebird_Species),]

PCA_FuncMetrics_of <- PCA_values_OF2[,-1]
rownames(PCA_FuncMetrics_of) <- PCA_values_OF2[,1]

#Calculate functinal metrics
FD_of <- dbFD(PCA_FuncMetrics_of,Birds_in_sites_of, w.abun = T)

#Add all sites to a vector
Sites <- All_birds2_of[,1]
Sites <- unique(Sites)

FRich <- FD_of$FRic 
FEve <- FD_of$FEve
FDis <- FD_of$FDis 
FDiv <- FD_of$FDiv

#Create data frame with sites, functional metrics and land use
FD_data_of <- data.frame(Sites,FRich,FEve,FDis,FDiv)

FD_data_of <- left_join(FD_data_of,transect_LU, by = "Transect")

######
#Overall non forest species
######

All_ONF_birds <- All_birds %>% subset(Habitat.Type=="Non Forest")

All_birds2_onf <- All_ONF_birds %>% dplyr::select(c("Transect", "Ebird_Species","Abundance")) %>% distinct()

#Convert so that rows are sites and columns are species
Birds_in_sites_onf <- All_birds2_onf %>% tidyr::pivot_wider(names_from = 'Ebird_Species', values_from = 'Abundance') 
Birds_in_sites_onf[is.na(Birds_in_sites_onf)] = 0
Birds_in_sites_onf <- Birds_in_sites_onf[ -c(1) ]
Birds_in_sites_onf <- Birds_in_sites_onf[,order(colnames(Birds_in_sites_onf))]

#Get overall non forest species PCA values 

ONF_Species <- data.frame(unique(All_ONF_birds$Ebird_Species))
colnames(ONF_Species)
names(ONF_Species)[names(ONF_Species) == "unique.All_ONF_birds.Ebird_Species."] <- 'Ebird_Species'

#Get the PCA values for just invertebrates
PCA_values_ONF <- left_join(ONF_Species, PCA_values, by = "Ebird_Species")

#Make species the rownames
PCA_values_ONF2 <- PCA_values_ONF #%>% dplyr::select(-c("Bodysize_sd","Trophic_sd","Locomotory_sd"))

#Order species alphabetically
PCA_values_ONF2 <- PCA_values_ONF2[order(PCA_values_ONF2$Ebird_Species),]

PCA_FuncMetrics_onf <- PCA_values_ONF2[,-1]
rownames(PCA_FuncMetrics_onf) <- PCA_values_ONF2[,1]

#Calculate functinal metrics
FD_onf <- dbFD(PCA_FuncMetrics_onf,Birds_in_sites_onf, w.abun = T)

#Add all sites to a vector
Sites <- All_birds2_onf[,1]
Sites <- unique(Sites)

FRich <- FD_onf$FRic 
FEve <- FD_onf$FEve
FDis <- FD_onf$FDis #Equals 0 in communities with only one functionally singular species. 
FDiv <- FD_onf$FDiv

#Create data frame with sites, functional metrics and land use
FD_data_onf <- data.frame(Sites,FRich,FEve,FDis,FDiv)

FD_data_onf <- left_join(FD_data_onf,transect_LU, by = "Transect")

######
#Invertivore forest species
######

All_IF_birds <- All_birds %>% subset(Trophic.Niche=="Invertivore")
All_IF_birds <- All_IF_birds %>% subset(Habitat.Type=="Forest")

All_birds2_if <- All_IF_birds %>% dplyr::select(c("Transect", "Ebird_Species","Abundance")) %>% distinct()

#Convert so that rows are sites and columns are species
Birds_in_sites_if <- All_birds2_if %>% tidyr::pivot_wider(names_from = 'Ebird_Species', values_from = 'Abundance') 
Birds_in_sites_if[is.na(Birds_in_sites_if)] = 0
Birds_in_sites_if <- Birds_in_sites_if[ -c(1) ]
Birds_in_sites_if <- Birds_in_sites_if[,order(colnames(Birds_in_sites_if))]

#Get invertivore forest species PCA values 

IF_Species <- data.frame(unique(All_IF_birds$Ebird_Species))
colnames(IF_Species)
names(IF_Species)[names(IF_Species) == "unique.All_IF_birds.Ebird_Species."] <- 'Ebird_Species'

#Get the PCA values for just invertebrates
PCA_values_IF <- left_join(IF_Species, PCA_values, by = "Ebird_Species")

#Make species the rownames
PCA_values_IF2 <- PCA_values_IF #%>% dplyr::select(-c("Bodysize_sd","Trophic_sd","Locomotory_sd"))

#Order species alphabetically
PCA_values_IF2 <- PCA_values_IF2[order(PCA_values_IF2$Ebird_Species),]

PCA_FuncMetrics_if <- PCA_values_IF2[,-1]
rownames(PCA_FuncMetrics_if) <- PCA_values_IF2[,1]

#Calculate functinal metrics
FD_if <- dbFD(PCA_FuncMetrics_if,Birds_in_sites_if, w.abun = T)

#Add all sites to a vector
Sites <- All_birds2_if[,1]
Sites <- unique(Sites)

FRich <- FD_if$FRic 
FEve <- FD_if$FEve
FDis <- FD_if$FDis 
FDiv <- FD_if$FDiv

#Create data frame with sites, functional metrics and land use
FD_data_if <- data.frame(Sites,FRich,FEve,FDis,FDiv)

FD_data_if <- left_join(FD_data_if,transect_LU, by = "Transect")

######
#Invertivore non forest
######

All_INF_birds <- All_birds %>% subset(Trophic.Niche=="Invertivore")
All_INF_birds <- All_INF_birds %>% subset(Habitat.Type=="Non Forest")

All_birds2_inf <- All_INF_birds %>% dplyr::select(c("Transect", "Ebird_Species","Abundance")) %>% distinct()

#Convert so that rows are sites and columns are species
Birds_in_sites_inf <- All_birds2_inf %>% tidyr::pivot_wider(names_from = 'Ebird_Species', values_from = 'Abundance') 
Birds_in_sites_inf[is.na(Birds_in_sites_inf)] = 0
Birds_in_sites_inf <- Birds_in_sites_inf[ -c(1) ]
Birds_in_sites_inf <- Birds_in_sites_inf[,order(colnames(Birds_in_sites_inf))]

write.csv(Birds_in_sites_inf, "NonForest_Invertivore_Abundance_BySite.csv")

#Get Invertivore non forest species PCA values 

INF_Species <- data.frame(unique(All_INF_birds$Ebird_Species))
colnames(INF_Species)
names(INF_Species)[names(INF_Species) == "unique.All_INF_birds.Ebird_Species."] <- 'Ebird_Species'

#Get the PCA values for just invertebrates
PCA_values_INF <- left_join(INF_Species, PCA_values, by = "Ebird_Species")

#Make species the rownames
PCA_values_INF2 <- PCA_values_INF #%>% dplyr::select(-c("Bodysize_sd","Trophic_sd","Locomotory_sd"))

#Order species alphabetically
PCA_values_INF2 <- PCA_values_INF2[order(PCA_values_INF2$Ebird_Species),]

PCA_FuncMetrics_inf <- PCA_values_INF2[,-1]
rownames(PCA_FuncMetrics_inf) <- PCA_values_INF2[,1]

#Calculate functinal metrics
FD_inf <- dbFD(PCA_FuncMetrics_inf,Birds_in_sites_inf, w.abun = T)

#Add all sites to a vector
Sites <- All_birds2_inf[,1]
Sites <- unique(Sites)

FRich <- FD_inf$FRic 
FEve <- FD_inf$FEve
FDis <- FD_inf$FDis #Equals 0 in communities with only one functionally singular species. 
FDiv <- FD_inf$FDiv

#Create data frame with sites, functional metrics and land use
FD_data_inf <- data.frame(Sites,FRich,FEve,FDis,FDiv)

FD_data_inf <- left_join(FD_data_inf,transect_LU, by = "Transect")

###Save invertivore forest and non forest data sets###

write.csv(PCA_FuncMetrics_if, "Forest_Invertivores_PCA.csv")
write.csv(PCA_FuncMetrics_inf, "NonForest_Invertivores_PCA.csv")

write.csv(Birds_in_sites_if, "Forest_Invertivore_Abundance.csv")
write.csv(Birds_in_sites_inf, "NonForest_Invertivore_Abundance.csv")








