setwd("~/Library/CloudStorage/OneDrive-ImperialCollegeLondon/MSc EEC/Project")

#Get packages

library(tidyverse)


#Import data

Vegetation <- read.csv("Vegetation.csv")
Pointcounts <- read.csv("Pointcounts.csv")

#Drop duplicate columns in vegetation

colnames(Vegetation)
Vegetation <- Vegetation[ -c(1,2,4:7,9:14)]

#Merge vegetation data and point counts
Data_main <- left_join(Pointcounts, Vegetation, by = c("Site_ID_CORRECTED" = "Site_ID"))

names(Data_main)[names(Data_main) == 'Site_ID_CORRECTED'] <- 'Site_ID'

##Import forest cover data##

##200m Hansen

Zambia_Sentinel_forest_cover_GPS_200m_2020 <- read.csv("Hansen Forest Cover 2/Zambia_Sentinel_forest_cover_GPS_200m_2020.csv")

colnames(Zambia_Sentinel_forest_cover_GPS_200m_2020)[3] <- "Site_ID"

Zambia_Sentinel_forest_cover_GPS_200m_2020$ForestCover200m <- Zambia_Sentinel_forest_cover_GPS_200m_2020$count/125664*100

Zambia_Sentinel_forest_cover_GPS_200m_2020 <- Zambia_Sentinel_forest_cover_GPS_200m_2020[ -c(1,2,4)]

LandUse_Data <- left_join(Data_main, Zambia_Sentinel_forest_cover_GPS_200m_2020, by = "Site_ID")

##500m Hansen

Zambia_Sentinel_forest_cover_GPS_500m_2020 <- read.csv("Hansen Forest Cover 2/Zambia_Sentinel_forest_cover_GPS_500m_2020.csv")

colnames(Zambia_Sentinel_forest_cover_GPS_500m_2020)[3] <- "Site_ID"

Zambia_Sentinel_forest_cover_GPS_500m_2020$ForestCover500m <- Zambia_Sentinel_forest_cover_GPS_500m_2020$count/785398*100

Zambia_Sentinel_forest_cover_GPS_500m_2020 <- Zambia_Sentinel_forest_cover_GPS_500m_2020[ -c(1,2,4)]

LandUse_Data <- left_join(LandUse_Data, Zambia_Sentinel_forest_cover_GPS_500m_2020, by = "Site_ID")

##1000m Hansen

Zambia_Sentinel_forest_cover_GPS_1000m_2020 <- read.csv("Hansen Forest Cover 2/Zambia_Sentinel_forest_cover_GPS_1000m_2020.csv")

colnames(Zambia_Sentinel_forest_cover_GPS_1000m_2020)[3] <- "Site_ID"

Zambia_Sentinel_forest_cover_GPS_1000m_2020$ForestCover1000m <- Zambia_Sentinel_forest_cover_GPS_1000m_2020$count/3141593*100

Zambia_Sentinel_forest_cover_GPS_1000m_2020 <- Zambia_Sentinel_forest_cover_GPS_1000m_2020[ -c(1,2,4)]

LandUse_Data <- left_join(LandUse_Data, Zambia_Sentinel_forest_cover_GPS_1000m_2020, by = "Site_ID")

##3000m Hansen

Zambia_Sentinel_forest_cover_GPS_3000m_2020 <- read.csv("Hansen Forest Cover 2/Zambia_Sentinel_forest_cover_GPS_3000m_2020.csv")

colnames(Zambia_Sentinel_forest_cover_GPS_3000m_2020)[3] <- "Site_ID"

Zambia_Sentinel_forest_cover_GPS_3000m_2020$ForestCover3000m <- Zambia_Sentinel_forest_cover_GPS_3000m_2020$count/28274334*100

Zambia_Sentinel_forest_cover_GPS_3000m_2020 <- Zambia_Sentinel_forest_cover_GPS_3000m_2020[ -c(1,2,4)]

LandUse_Data <- left_join(LandUse_Data, Zambia_Sentinel_forest_cover_GPS_3000m_2020, by = "Site_ID")

##5000m Hansen

Zambia_Sentinel_forest_cover_GPS_5000m_2020 <- read.csv("Hansen Forest Cover 2/Zambia_Sentinel_forest_cover_GPS_5000m_2020.csv")

colnames(Zambia_Sentinel_forest_cover_GPS_5000m_2020)[3] <- "Site_ID"

Zambia_Sentinel_forest_cover_GPS_5000m_2020$ForestCover5000m <- Zambia_Sentinel_forest_cover_GPS_5000m_2020$count/78539816*100

Zambia_Sentinel_forest_cover_GPS_5000m_2020 <- Zambia_Sentinel_forest_cover_GPS_5000m_2020[ -c(1,2,4)]

LandUse_Data <- left_join(LandUse_Data, Zambia_Sentinel_forest_cover_GPS_5000m_2020, by = "Site_ID")

#Forest cover and site types#

par(mfcol = c(2,3))

LandUse_Data$Site_Type_Minor <- factor(LandUse_Data$Site_Type_Minor, levels=c("Agriculture", "Forest Edge", "Forest"))
LandUse_Data$Transect_Type <- factor(LandUse_Data$Transect_Type, levels=c("Agriculture", "Forest Edge", "Forest"))

boxplot(ForestCover200m ~ Site_Type_Minor, data = LandUse_Data)
boxplot(ForestCover500m ~ Site_Type_Minor, data = LandUse_Data)
boxplot(ForestCover1000m ~ Site_Type_Minor, data = LandUse_Data)
boxplot(ForestCover3000m ~ Site_Type_Minor, data = LandUse_Data)
boxplot(ForestCover5000m ~ Site_Type_Minor, data = LandUse_Data)

dev.off()

#Forest cover and tree counts#

#Total trees

par(mfcol = c(2,3))

plot(ForestCover200m ~ TreeTotal, data = LandUse_Data)
plot(ForestCover500m ~ TreeTotal, data = LandUse_Data)
plot(ForestCover1000m ~ TreeTotal, data = LandUse_Data)
plot(ForestCover3000m ~ TreeTotal, data = LandUse_Data)
plot(ForestCover5000m ~ TreeTotal, data = LandUse_Data)

dev.off()

#Medium and large trees only

LandUse_Data$TreeML <- LandUse_Data$TreeM + LandUse_Data$TreeL

write.csv(LandUse_Data,"LandUse_Data.csv")

par(mfcol = c(2,3))

plot(ForestCover200m ~ TreeML, data = LandUse_Data)
plot(ForestCover500m ~ TreeML, data = LandUse_Data)
plot(ForestCover1000m ~ TreeML, data = LandUse_Data)
plot(ForestCover3000m ~ TreeML, data = LandUse_Data)
plot(ForestCover5000m ~ TreeML, data = LandUse_Data)

#Forest cover and canopy height#

par(mfcol = c(2,3))

boxplot(ForestCover200m ~ MeanCan, data = LandUse_Data)
boxplot(ForestCover500m ~ MeanCan, data = LandUse_Data)
boxplot(ForestCover1000m ~ MeanCan, data = LandUse_Data)
boxplot(ForestCover3000m ~ MeanCan, data = LandUse_Data)
boxplot(ForestCover5000m ~ MeanCan, data = LandUse_Data)

dev.off()

#Forest cover and visibility#

par(mfcol = c(2,3))

plot(ForestCover200m ~ MeanVis, data = LandUse_Data)
plot(ForestCover500m ~ MeanVis, data = LandUse_Data)
plot(ForestCover1000m ~ MeanVis, data = LandUse_Data)
plot(ForestCover3000m ~ MeanVis, data = LandUse_Data)
plot(ForestCover5000m ~ MeanVis, data = LandUse_Data)


#######################################
###Investigate functional trait data###
#######################################

BirdTraits <- read.csv('AVONET_Ebird.csv')

TraitData_Hansen <- left_join(LandUse_Data, BirdTraits, by = c("Ebird_Species" = "Species2"))

summary(TraitData_Hansen)

BirdNAs <- TraitData_Hansen[is.na(TraitData_Hansen$Habitat), ]   

BirdNAs <- unique(BirdNAs$Ebird_Species)

TraitData_Hansen$Habitat <- as.factor(TraitData_Hansen$Habitat)
TraitData_Hansen$Trophic.Niche <- as.factor(TraitData_Hansen$Trophic.Niche)

plot(TraitData_Hansen$ForestCover200m ~ TraitData_Hansen$Habitat)
plot(TraitData_Hansen$ForestCover200m ~ TraitData_Hansen$Trophic.Niche)


###############
#Vegetation cover between shifted and natural baselines#
###############

CM <- SR_Data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")
ZM <- SR_Data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")

hist(CM$Trees)
hist(ZM$Trees)

hist(CM$MeanForestCover1000m)
hist(ZM$MeanForestCover1000m)

a <- CM %>% dplyr::filter(MeanForestCover1000m >= 100) #18%
b <- ZM %>% dplyr::filter(MeanForestCover1000m >= 100) #33%

max(log(CM$Trees))
max(log(ZM$Trees))
c <- ZM %>% dplyr::filter(Trees >= 549) #7 sites out of 67


##########################
#####Data Exploration#####
##########################

#######################
####Species richness of each functional group at each locality by transect####
#######################

#Select useful columns from all data 
colnames(TraitData_Hansen)
trophic_richness_data <- TraitData_Hansen %>% dplyr::select(c(2, 20, 33, 34, 44, 48:52, 78, 82, "TreeML"))
colnames(trophic_richness_data)

# filter by desired functional group; trophic niche (grouped by transect)
unique(trophic_richness_data$Trophic.Niche)
colnames(trophic_richness_data)
trophic_guilds_rich_by_transect <- trophic_richness_data %>%
  dplyr::select(c(1,2,3,12)) %>% distinct() %>%
  dplyr::count(Transect, Trophic.Niche) %>%
  tidyr::pivot_wider(names_from = 'Trophic.Niche', values_from = 'n') 

trophic_guilds_rich_by_transect[is.na(trophic_guilds_rich_by_transect)] = 0
trophic_guilds_rich_by_transect <- trophic_guilds_rich_by_transect %>% pivot_longer(names_to = 'Trophic.Niche', values_to = 'Richness', cols=2:10)

#Get mean forest cover
transect_forest_cover <- TraitData_Hansen %>% dplyr::select(c("Transect", "ForestCover200m":"ForestCover5000m")) 
transect_forest_cover <- transect_forest_cover %>%  group_by(Transect) %>%
  dplyr::summarise(MeanForestCover200m=mean(ForestCover200m), MeanForestCover500m=mean(ForestCover500m),
                   MeanForestCover1000m=mean(ForestCover1000m), MeanForestCover3000m=mean(ForestCover3000m), 
                   MeanForestCover5000m=mean(ForestCover5000m))

#Get number of trees in each transect
transect_tree_counts <- TraitData_Hansen %>% dplyr::select(c("Transect","TreeML")) %>%  group_by(Transect) %>%
  dplyr::summarise(Trees = sum(TreeML))

#Join both
transect_LU <- left_join(transect_forest_cover, transect_tree_counts, by = "Transect")

#Get season and locality for each transect

transect_season_and_locality <- TraitData_Hansen %>% dplyr::select(c("Transect","Locality","Season","Transect_Type")) %>%  group_by(Transect) %>% distinct()

transect_LU <- left_join(transect_LU, transect_season_and_locality, by = "Transect")

# rejoin into dataframe with all e.g. tropic guilds 
trophic_guilds_rich_by_transect <- trophic_guilds_rich_by_transect %>%
  left_join(transect_LU, by = "Transect") %>% 
  subset(Trophic.Niche %in% c('Frugivore', 'Invertivore', 'Omnivore', 'Granivore'))
colnames(trophic_guilds_rich_by_transect)  


#plot
test_plot <- ggplot(trophic_guilds_rich_by_transect, aes(MeanForestCover200m, Richness, col = Trophic.Niche)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(~Locality) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Species Richness') +
  theme(text = element_text(size = 20))  
test_plot

test_plot2 <- ggplot(trophic_guilds_rich_by_transect, aes(MeanForestCover200m, Richness, col = Trophic.Niche)) +
  geom_point() + geom_smooth(method = lm) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Species Richness') +
  theme(text = element_text(size = 20))  
test_plot2

trophic.labs <- c("a) Frugivore","b) Granivore","c) Invertivore","d) Omnivore")
names(trophic.labs) <- c("Frugivore", "Granivore", "Invertivore", "Omnivore")


#plot
test_plot <- ggplot(trophic_guilds_rich_by_transect, aes(MeanForestCover200m, Richness, color = Trophic.Niche), show.legend = FALSE) +
  geom_point() + geom_smooth(method = glm) +
  facet_wrap(~Trophic.Niche, labeller = labeller(Trophic.Niche = 
                                                   c("Frugivore" = "a) Frugivore",
                                                     "Granivore" = "b) Granivore",
                                                     "Invertivore" = "c) Invertivore",
                                                     "Omnivore" = "d) Omnivore"))) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Species Richness') +
  theme(text = element_text(size = 25))  
test_plot

################
###Remove MIN###
################

xMin_trophic_guilds_rich_by_transect <- trophic_guilds_rich_by_transect %>% subset(Locality %in% c('Mayukwayukwa', 'Chitokoloki', 'Zambezi'))

#plot
test_plot <- ggplot(xMin_trophic_guilds_rich_by_transect, aes(MeanForestCover200m, Richness), show.legend = FALSE) +
  geom_point(aes(col = Season)) + geom_smooth(method = glm) +
  facet_wrap(~Trophic.Niche) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Species Richness') +
  theme(text = element_text(size = 23))  
test_plot

?ggplot


####Species richness of forest vs non-forest at each locality by transect####

Habitat_richness_data <- TraitData_Hansen %>% dplyr::select(c(2, 20, 33, 34, 44, 48:52, 78, 82))
colnames(Habitat_richness_data)

#Filter by desired habitat type (grouped by transect)
unique(trophic_richness_data$Habitat)
colnames(trophic_richness_data)
Habitat_rich_by_transect <- trophic_richness_data %>%
  dplyr::select(c(1,2,3,11)) %>% distinct() %>%
  dplyr::count(Transect, Habitat) %>%
  tidyr::pivot_wider(names_from = 'Habitat', values_from = 'n') 

Habitat_rich_by_transect[is.na(Habitat_rich_by_transect)] = 0

colnames(Habitat_rich_by_transect)

Habitat_rich_by_transect <- Habitat_rich_by_transect %>%
  rowwise() %>%
  mutate(ForestAll = sum(Forest,Woodland))

Habitat_rich_by_transect <- Habitat_rich_by_transect %>%
  rowwise() %>%
  mutate(NonForest = sum(Grassland, Shrubland, Wetland, Desert, `Human Modified`, Riverine, Rock))

#Remove all columns expect forest and non forest
Habitat_rich_by_transect <- Habitat_rich_by_transect %>% dplyr::select(c(1,11,12))

Habitat_rich_by_transect <- Habitat_rich_by_transect %>% pivot_longer(names_to = 'Habitat', values_to = 'Richness', cols=2:3)

Habitat_rich_by_transect <- Habitat_rich_by_transect %>%
  left_join(transect_forest_cover) %>% left_join(dplyr::select(trophic_richness_data, 'Locality', 'Transect')) %>%
  subset(Habitat %in% c('ForestAll', 'NonForest'))
colnames(trophic_guilds_rich_by_transect)  


Habitat_rich_by_transect$Habitat <- gsub('ForestAll', 'Forest',
                                         gsub('NonForest', 'Non Forest', Habitat_rich_by_transect$Habitat))

#plot
test_plot3 <- ggplot(Habitat_rich_by_transect, aes(MeanForestCover200m, Richness, col = Habitat)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(~Locality) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 


test_plot4 <- ggplot(Habitat_rich_by_transect, aes(MeanForestCover200m, Richness, col = Habitat)) +
  geom_point() + geom_smooth(method = lm) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Species Richness') +
  theme(text = element_text(size = 20)) 
test_plot4


####Species richness of forest vs non-forest invertivores at each locality by transect####

Invert_richness_data <- TraitData_Hansen %>% dplyr::select(c(2, 20, 33, 34, 44, 48:52, 53, 78, 82)) %>% subset(Trophic.Niche == "Invertivore")
colnames(Invert_richness_data)
colnames(TraitData_Hansen)

#Filter by desired habitat type (grouped by transect)
unique(Invert_richness_data$Habitat)
colnames(Invert_richness_data)
Invert_rich_habitat_transect <- Invert_richness_data %>%
  dplyr::select(c(1,2,3,11,12)) %>% distinct() %>%
  dplyr::count(Transect, Habitat) %>%
  tidyr::pivot_wider(names_from = 'Habitat', values_from = 'n') 

Invert_rich_habitat_transect[is.na(Invert_rich_habitat_transect)] = 0

colnames(Invert_rich_habitat_transect)

Invert_rich_habitat_transect <- Invert_rich_habitat_transect %>%
  rowwise() %>%
  mutate(ForestAll = sum(Forest,Woodland))

Invert_rich_habitat_transect <- Invert_rich_habitat_transect %>%
  rowwise() %>%
  mutate(NonForest = sum(Grassland, Shrubland, Desert, `Human Modified`, Riverine, Wetland))

#Remove all columns expect forest and non forest
Invert_rich_habitat_transect <- Invert_rich_habitat_transect %>% dplyr::select(c(1,10,11))

Invert_rich_habitat_transect <- Invert_rich_habitat_transect %>% pivot_longer(names_to = 'Habitat', values_to = 'Richness', cols=2:3)

Invert_rich_habitat_transect <- Invert_rich_habitat_transect %>%
  left_join(transect_forest_cover) %>% left_join(dplyr::select(Invert_richness_data, 'Trophic.Niche', 'Transect')) %>%
  subset(Habitat %in% c('ForestAll', 'NonForest'))
colnames(Invert_rich_habitat_transect)  


Invert_rich_habitat_transect$Habitat <- gsub('ForestAll', 'Forest',
                                             gsub('NonForest', 'Non Forest', Invert_rich_habitat_transect$Habitat))

Invert_data <- Invert_richness_data %>% dplyr::select(c("Locality","Transect","Season","ForestCover200m","ForestCover500m","ForestCover1000m","ForestCover3000m","ForestCover5000m", "TreeML"))
colnames(Invert_data)

#Get mean forest cover
transect_forest_cover <- TraitData_Hansen %>% dplyr::select(c("Transect", "ForestCover200m":"ForestCover5000m")) 
transect_forest_cover <- transect_forest_cover %>%  group_by(Transect) %>%
  dplyr::summarise(MeanForestCover200m=mean(ForestCover200m), MeanForestCover500m=mean(ForestCover500m),
                   MeanForestCover1000m=mean(ForestCover1000m), MeanForestCover3000m=mean(ForestCover3000m), 
                   MeanForestCover5000m=mean(ForestCover5000m))

#Get number of trees in each transect
transect_tree_counts <- TraitData_Hansen %>% dplyr::select(c("Transect","TreeML")) %>%  group_by(Transect) %>%
  dplyr::summarise(Trees = sum(TreeML))

#Join both
transect_LU <- left_join(transect_forest_cover, transect_tree_counts, by = "Transect")

#Get season and locality for each transect

transect_season_and_locality <- TraitData_Hansen %>% dplyr::select(c("Transect","Locality","Season", "Site_Type_Major")) %>%  group_by(Transect) %>% distinct()

transect_LU <- left_join(transect_LU, transect_season_and_locality, by = "Transect")

#Add to invertivore richness data
Invert_rich_habitat_transect <- Invert_rich_habitat_transect %>% dplyr::select(-c("MeanForestCover200m":"MeanForestCover5000m"))

Invert_rich_habitat_transect_data <- left_join(Invert_rich_habitat_transect, transect_LU, by = "Transect")

#plot
test_plot3 <- ggplot(Invert_rich_habitat_transect_data, aes(MeanForestCover200m, Richness, col = Habitat)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(~Locality) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Invertivore Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 


test_plot3 <- ggplot(Invert_rich_habitat_transect_data, aes(MeanForestCover200m, Richness, col = Habitat)) +
  geom_point() + geom_smooth(method = lm) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Invertivore Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 


test_plot3 <- ggplot(Invert_rich_habitat_transect_data, aes(Trees, Richness, col = Habitat)) +
  geom_point() + geom_smooth(method = lm) +
  theme_classic() +
  labs(x = 'Number of trees', y = 'Invertivore Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 

test_plot3 <- ggplot(Invert_rich_habitat_transect_data, aes(Trees, Richness, col = Habitat)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(~Locality) +
  theme_classic() +
  labs(x = 'Number of trees', y = 'Invertivore Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 

###################
###Invertivore species richness by transect###
###################

Invert_rich_by_transect_data <- trophic_guilds_rich_by_transect %>%
  subset(Trophic.Niche %in% c('Invertivore'))

#plot
test_plot3 <- ggplot(Invert_rich_by_transect_data, aes(MeanForestCover200m, Richness)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(~Locality) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Invertivore Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 

#plot
test_plot3 <- ggplot(Invert_rich_by_transect_data, aes(MeanForestCover500m, Richness)) +
  geom_point() + geom_smooth(method = lm) +
  theme_classic() +
  labs(x = 'Forest Cover at 500m (%)', y = 'Invertivore Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 

#plot
test_plot3 <- ggplot(Invert_rich_by_transect_data, aes(Trees, Richness)) +
  geom_point() + geom_smooth(method = lm) +
  theme_classic() +
  labs(x = 'Number of Trees', y = 'Invertivore Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 

#plot
test_plot3 <- ggplot(Invert_rich_by_transect_data, aes(Trees, Richness)) +
  geom_point() + geom_smooth(method = lm) +
  facet_wrap(~Locality) +
  theme_classic() +
  labs(x = 'Number of Trees', y = 'Invertivore Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 



################
###Overall SR###
###############

Overall_rich_transect_data <- left_join(SpeciesRichness_ByTransect2, transect_LU, by = "Transect")
Overall_rich_transect_data <- Overall_rich_transect_data %>% dplyr::filter(Transect != "T1_17322")

#plot
test_plot3 <- ggplot(Overall_rich_transect_data, aes(MeanForestCover200m, Species_Richness)) +
  geom_point() + geom_smooth(method = lm) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Species Richness') +
  theme(text = element_text(size = 23))   
test_plot3 


#plot
test_plot3 <- ggplot(Overall_rich_transect_data, aes(Trees, Species_Richness)) +
  geom_point() + geom_smooth(method = lm) +
  theme_classic() +
  labs(x = 'Forest Cover at 200m (%)', y = 'Species Richness') +
  theme(text = element_text(size = 20))   
test_plot3 





