setwd("~/Library/CloudStorage/OneDrive-ImperialCollegeLondon/MSc EEC/Project")

library('vegan')
library(tidyverse)

#Get Community data
LandUse_Data <- read.csv('LandUse_Data.csv')
BirdTraits <- read.csv('AVONET_Ebird.csv')

TraitData_Hansen <- left_join(LandUse_Data, BirdTraits, by = c("Ebird_Species" = "Species2"))

#Extract columns that I need for accumulation curve
Community_Data <- TraitData_Hansen %>% dplyr::select("Locality", "Site_ID", "Ebird_Species", "Perch.Under.50m", "Flyover.Under.50m", "Over.50m", "Transect_Type", "Season")

Community_Data <- Community_Data %>% 
  ## group by site and birdlife name a occasionally the sme species pops up in the same site twice.
  dplyr::group_by(Site_ID, Ebird_Species) %>% dplyr::mutate(n_spp = n()) %>%
  ## filter so that species isn't duplicated and then ungroup 
  filter(!duplicated(n_spp)| n_spp == 1) %>% ungroup() %>%
  ## droplevels
  droplevels() %>%
  #as data frame
  data.frame()

Community_Data$Totalbirds <- Community_Data$Perch.Under.50m + Community_Data$Flyover.Under.50m + Community_Data$Over.50m

Community_Data <- Community_Data %>% group_by(Site_ID) %>% group_by(Ebird_Species) %>% mutate(Totalbirds = sum(Totalbirds))

spec_accum_data <- Community_Data %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data)){
  if (spec_accum_data$Totalbirds[i] == 0) {
    spec_accum_data$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data$Totalbirds[i] > 0){
    spec_accum_data$Totalbirds[i] <- 1
  }
}


class(spec_accum_data$Ebird_Species)

#Remove duplicated species per site ID 
spec_accum_data <- distinct(spec_accum_data)

#Pivot so species are columns
spec_accum_data <- pivot_wider(spec_accum_data, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                               values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data<-spec_accum_data[-1]

#Convert NAs to 0
spec_accum_data[is.na(spec_accum_data)] = 0

#Species accumulation curve
sp.a1 <- specaccum(spec_accum_data)

AccumCurve_all = specaccum(spec_accum_data, method = "random", 
                           permutations = 100)

plot(AccumCurve_all)


#Chitokoloki

Chitokoloki_Data <- Community_Data %>% subset(Season=="Season 1") %>% subset(Locality=="Chitokoloki")

spec_accum_data_chi <- Chitokoloki_Data %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_chi)){
  if (spec_accum_data_chi$Totalbirds[i] == 0) {
    spec_accum_data_chi$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_chi$Totalbirds[i] > 0){
    spec_accum_data_chi$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_chi <- pivot_wider(spec_accum_data_chi, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                               values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_chi<-spec_accum_data_chi[-1]

#Convert NAs to 0
spec_accum_data_chi[is.na(spec_accum_data_chi)] = 0

#Species accumulation curve
sp.a2 <- specaccum(spec_accum_data_chi)

AccumCurve_chi = specaccum(spec_accum_data_chi, method = "random", 
                           permutations = 100)

plot(AccumCurve_chi)


Chitokoloki_Data_F <- Chitokoloki_Data %>% subset(Transect_Type=="Forest")
Chitokoloki_Data_A <- Chitokoloki_Data %>% subset(Transect_Type=="Agriculture")
Chitokoloki_Data_FE <- Chitokoloki_Data %>% subset(Transect_Type=="Forest Edge")  

#Forest 

spec_accum_data_chiF <- Chitokoloki_Data_F %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_chiF)){
  if (spec_accum_data_chiF$Totalbirds[i] == 0) {
    spec_accum_data_chiF$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_chiF$Totalbirds[i] > 0){
    spec_accum_data_chiF$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_chiF <- pivot_wider(spec_accum_data_chiF, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                   values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_chiF<-spec_accum_data_chiF[-1]

#Convert NAs to 0
spec_accum_data_chiF[is.na(spec_accum_data_chiF)] = 0

#Species accumulation curve
sp.a3 <- specaccum(spec_accum_data_chiF)

AccumCurve_chiF = specaccum(spec_accum_data_chiF, method = "random", 
                           permutations = 100)

plot(AccumCurve_chiF)

#Forest Edge

spec_accum_data_chiFE <- Chitokoloki_Data_FE %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_chiFE)){
  if (spec_accum_data_chiFE$Totalbirds[i] == 0) {
    spec_accum_data_chiFE$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_chiFE$Totalbirds[i] > 0){
    spec_accum_data_chiFE$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_chiFE <- pivot_wider(spec_accum_data_chiFE, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                    values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_chiFE<-spec_accum_data_chiFE[-1]

#Convert NAs to 0
spec_accum_data_chiFE[is.na(spec_accum_data_chiFE)] = 0

#Species accumulation curve
AccumCurve_chiFE = specaccum(spec_accum_data_chiFE, method = "random", 
                            permutations = 100)

plot(AccumCurve_chiFE)

##Agriculture

spec_accum_data_chiA <- Chitokoloki_Data_A %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_chiA)){
  if (spec_accum_data_chiA$Totalbirds[i] == 0) {
    spec_accum_data_chiA$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_chiA$Totalbirds[i] > 0){
    spec_accum_data_chiA$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_chiA <- pivot_wider(spec_accum_data_chiA, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                     values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_chiA<-spec_accum_data_chiA[-1]

#Convert NAs to 0
spec_accum_data_chiA[is.na(spec_accum_data_chiA)] = 0

#Species accumulation curve
AccumCurve_chiA = specaccum(spec_accum_data_chiA, method = "random", 
                             permutations = 100)

plot(AccumCurve_chiA)

plot(AccumCurve_chiA,  col = "Purple", xlim = c(0,60),xlab = "Points", ylab = "Species", main = "Chitokoloki")
plot(AccumCurve_chiF, add = TRUE, col = "Green") #col is COLOUR setting, so change it to something else if you want
plot(AccumCurve_chiFE, add = TRUE, col = "Cyan")

legend(x = "bottomright", legend = c("Forest", "Forest Edge", "Agriculture"), cex = 1.3, fill = c("Green","Cyan", "Purple"))



############
#Mayukwayukwa

Mayukwayukwa_Data <- Community_Data %>% subset(Season=="Season 1") %>% subset(Locality=="Mayukwayukwa")

Mayukwayukwa_Data_F <- Mayukwayukwa_Data %>% subset(Transect_Type=="Forest")
Mayukwayukwa_Data_A <- Mayukwayukwa_Data %>% subset(Transect_Type=="Agriculture")
Mayukwayukwa_Data_FE <- Mayukwayukwa_Data %>% subset(Transect_Type=="Forest Edge")  

#Forest 

spec_accum_data_myaF <- Mayukwayukwa_Data_F %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_myaF)){
  if (spec_accum_data_myaF$Totalbirds[i] == 0) {
    spec_accum_data_myaF$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_myaF$Totalbirds[i] > 0){
    spec_accum_data_myaF$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_myaF <- pivot_wider(spec_accum_data_myaF, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                    values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_myaF<-spec_accum_data_myaF[-1]

#Convert NAs to 0
spec_accum_data_myaF[is.na(spec_accum_data_myaF)] = 0

#Species accumulation curve

AccumCurve_myaF = specaccum(spec_accum_data_myaF, method = "random", 
                            permutations = 100)

plot(AccumCurve_myaF)

#Forest Edge

spec_accum_data_myaFE <- Mayukwayukwa_Data_FE %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_myaFE)){
  if (spec_accum_data_myaFE$Totalbirds[i] == 0) {
    spec_accum_data_myaFE$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_myaFE$Totalbirds[i] > 0){
    spec_accum_data_myaFE$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_myaFE <- pivot_wider(spec_accum_data_myaFE, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                     values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_myaFE<-spec_accum_data_myaFE[-1]

#Convert NAs to 0
spec_accum_data_myaFE[is.na(spec_accum_data_myaFE)] = 0

#Species accumulation curve
AccumCurve_myaFE = specaccum(spec_accum_data_myaFE, method = "random", 
                             permutations = 100)

plot(AccumCurve_myaFE)

##Agriculture

spec_accum_data_myaA <- Mayukwayukwa_Data_A %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_myaA)){
  if (spec_accum_data_myaA$Totalbirds[i] == 0) {
    spec_accum_data_myaA$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_myaA$Totalbirds[i] > 0){
    spec_accum_data_myaA$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_myaA <- pivot_wider(spec_accum_data_myaA, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                    values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_myaA<-spec_accum_data_myaA[-1]

#Convert NAs to 0
spec_accum_data_myaA[is.na(spec_accum_data_myaA)] = 0

#Species accumulation curve
AccumCurve_myaA = specaccum(spec_accum_data_myaA, method = "random", 
                            permutations = 100)

plot(AccumCurve_myaA)

plot(AccumCurve_myaA,  col = "Purple", xlim = c(0,100),xlab = "Points", ylab = "Species", main = "Mayukwayukwa")
plot(AccumCurve_myaF, add = TRUE, col = "Green") #col is COLOUR setting, so change it to something else if you want
plot(AccumCurve_myaFE, add = TRUE, col = "Cyan")

legend(x = "bottomright", legend = c("Forest", "Forest Edge", "Agriculture"), cex = 1.3, fill = c("Green","Cyan", "Purple"))


#Mwinilunga

Mwinilunga_Data <- Community_Data %>% subset(Season=="Season 3") %>% subset(Locality=="Mwinilunga")

Mwinilunga_Data_F <- Mwinilunga_Data %>% subset(Transect_Type=="Forest")
Mwinilunga_Data_A <- Mwinilunga_Data %>% subset(Transect_Type=="Agriculture")
Mwinilunga_Data_FE <- Mwinilunga_Data %>% subset(Transect_Type=="Forest Edge")  

#Forest 

spec_accum_data_minF <- Mwinilunga_Data_F %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_minF)){
  if (spec_accum_data_minF$Totalbirds[i] == 0) {
    spec_accum_data_minF$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_minF$Totalbirds[i] > 0){
    spec_accum_data_minF$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_minF <- pivot_wider(spec_accum_data_minF, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                    values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_minF<-spec_accum_data_minF[-1]

#Convert NAs to 0
spec_accum_data_minF[is.na(spec_accum_data_minF)] = 0

#Species accumulation curve

AccumCurve_minF = specaccum(spec_accum_data_minF, method = "random", 
                            permutations = 100)

plot(AccumCurve_minF)

#Forest Edge

spec_accum_data_minFE <- Mwinilunga_Data_FE %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_minFE)){
  if (spec_accum_data_minFE$Totalbirds[i] == 0) {
    spec_accum_data_minFE$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_minFE$Totalbirds[i] > 0){
    spec_accum_data_minFE$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_minFE <- pivot_wider(spec_accum_data_minFE, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                     values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_minFE<-spec_accum_data_minFE[-1]

#Convert NAs to 0
spec_accum_data_minFE[is.na(spec_accum_data_minFE)] = 0

#Species accumulation curve
AccumCurve_minFE = specaccum(spec_accum_data_minFE, method = "random", 
                             permutations = 100)

plot(AccumCurve_minFE)

##Agriculture

spec_accum_data_minA <- Mwinilunga_Data_A %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_minA)){
  if (spec_accum_data_minA$Totalbirds[i] == 0) {
    spec_accum_data_minA$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_minA$Totalbirds[i] > 0){
    spec_accum_data_minA$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_minA <- pivot_wider(spec_accum_data_minA, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                    values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_minA<-spec_accum_data_minA[-1]

#Convert NAs to 0
spec_accum_data_minA[is.na(spec_accum_data_minA)] = 0

#Species accumulation curve
AccumCurve_minA = specaccum(spec_accum_data_minA, method = "random", 
                            permutations = 100)

plot(AccumCurve_minA)

plot(AccumCurve_minF,  col = "Purple", ylim = c(0,150), xlim = c(0,130),xlab = "Points", ylab = "Species", main = "Mwinilunga")
plot(AccumCurve_minFE, add = TRUE, col = "Green") #col is COLOUR setting, so change it to something else if you want
plot(AccumCurve_minA, add = TRUE, col = "Cyan")

legend(x = "bottomright", legend = c("Forest", "Forest Edge", "Agriculture"), cex = 1.3, fill = c("Green","Cyan", "Purple"))



#Zambezi

Zambezi_Data <- Community_Data %>% subset(Season=="Season 2") %>% subset(Locality=="Zambezi")

Zambezi_Data_F <- Zambezi_Data %>% subset(Transect_Type=="Forest")
Zambezi_Data_A <- Zambezi_Data %>% subset(Transect_Type=="Agriculture")
Zambezi_Data_FE <- Zambezi_Data %>% subset(Transect_Type=="Forest Edge")  

#Forest 

spec_accum_data_zamF <- Zambezi_Data_F %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_zamF)){
  if (spec_accum_data_zamF$Totalbirds[i] == 0) {
    spec_accum_data_zamF$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_zamF$Totalbirds[i] > 0){
    spec_accum_data_zamF$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_zamF <- pivot_wider(spec_accum_data_zamF, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                    values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_zamF<-spec_accum_data_zamF[-1]

#Convert NAs to 0
spec_accum_data_zamF[is.na(spec_accum_data_zamF)] = 0

#Species accumulation curve

AccumCurve_zamF = specaccum(spec_accum_data_zamF, method = "random", 
                            permutations = 100)

plot(AccumCurve_zamF)

#Forest Edge

spec_accum_data_zamFE <- Zambezi_Data_FE %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_zamFE)){
  if (spec_accum_data_zamFE$Totalbirds[i] == 0) {
    spec_accum_data_zamFE$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_zamFE$Totalbirds[i] > 0){
    spec_accum_data_zamFE$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_zamFE <- pivot_wider(spec_accum_data_zamFE, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                     values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_zamFE<-spec_accum_data_zamFE[-1]

#Convert NAs to 0
spec_accum_data_zamFE[is.na(spec_accum_data_zamFE)] = 0

#Species accumulation curve
AccumCurve_zamFE = specaccum(spec_accum_data_zamFE, method = "random", 
                             permutations = 100)

plot(AccumCurve_zamFE)

##Agriculture

spec_accum_data_zamA <- Zambezi_Data_A %>% dplyr::select("Site_ID", "Ebird_Species", "Totalbirds")

#Convert to presence/absence
for(i in 1:nrow(spec_accum_data_zamA)){
  if (spec_accum_data_zamA$Totalbirds[i] == 0) {
    spec_accum_data_zamA$Totalbirds[i] <- 0 
  } 
  if (spec_accum_data_zamA$Totalbirds[i] > 0){
    spec_accum_data_zamA$Totalbirds[i] <- 1
  }
}

#Pivot so species are columns
spec_accum_data_zamA <- pivot_wider(spec_accum_data_zamA, id_cols = 'Site_ID', names_from = 'Ebird_Species',
                                    values_from = 'Totalbirds')

#Remove Site ID column 
spec_accum_data_zamA<-spec_accum_data_zamA[-1]

#Convert NAs to 0
spec_accum_data_zamA[is.na(spec_accum_data_zamA)] = 0

#Species accumulation curve
AccumCurve_zamA = specaccum(spec_accum_data_zamA, method = "random", 
                            permutations = 100)

plot(AccumCurve_zamA)

plot(AccumCurve_zamF,  col = "Purple", ylim = c(0,100), xlim = c(0,100),xlab = "Points", ylab = "Species", main = "Zambezi")
plot(AccumCurve_zamFE, add = TRUE, col = "Green") #col is COLOUR setting, so change it to something else if you want
plot(AccumCurve_zamA, add = TRUE, col = "Cyan")

legend(x = "bottomright", legend = c("Forest", "Forest Edge", "Agriculture"), cex = 1.3, fill = c("Green","Cyan", "Purple"))


##All plots##

par(mfrow = c(2,2))
par(mar=c(5,6,4,1)+.1)

#Chitokoloki
plot(AccumCurve_chiA,  col = "Purple", ylim = c(0,150), xlim = c(0,60),xlab = "Points", ylab = "Species", main = "Chitokoloki", cex.axis =2, cex.main = 2, cex.lab = 2)
plot(AccumCurve_chiF, add = TRUE, col = "Green") #col is COLOUR setting, so change it to something else if you want
plot(AccumCurve_chiFE, add = TRUE, col = "Cyan")


#Mayukwayukwa
plot(AccumCurve_myaA,  col = "Purple", ylim = c(0,160), xlim = c(0,100),xlab = "Points", ylab = "Species", main = "Mayukwayukwa", cex.axis =2, cex.main = 2, cex.lab = 2)
plot(AccumCurve_myaF, add = TRUE, col = "Green") #col is COLOUR setting, so change it to something else if you want
plot(AccumCurve_myaFE, add = TRUE, col = "Cyan")

#Mwinilunga
plot(AccumCurve_minF,  col = "Green", ylim = c(0,150), xlim = c(0,130),xlab = "Points", ylab = "Species", main = "Mwinilunga", cex.axis =2, cex.main = 2, cex.lab = 2)
plot(AccumCurve_minFE, add = TRUE, col = "Cyan") #col is COLOUR setting, so change it to something else if you want
plot(AccumCurve_minA, add = TRUE, col = "Purple")

#Zambezi
plot(AccumCurve_zamF,  col = "Green", ylim = c(0,150), xlim = c(0,90),xlab = "Points", ylab = "Species", main = "Zambezi", cex.axis =2, cex.main = 2, cex.lab = 2)
plot(AccumCurve_zamFE, add = TRUE, col = "Cyan") #col is COLOUR setting, so change it to something else if you want
plot(AccumCurve_zamA, add = TRUE, col = "Purple")

dev.off()

legend(x = "bottomright", legend = c("Forest", "Forest Edge", "Agriculture"), cex = 2, fill = c("Green","Cyan", "Purple"))
