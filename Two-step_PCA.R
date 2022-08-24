###############
#####PCAs######
###############

#Import data

PCA_values <- read.csv("PCA_values.csv")
LandUse_Data <- read.csv('LandUse_Data.csv')
BirdTraits <- read.csv('AVONET_Ebird.csv')

TraitData_Hansen <- left_join(LandUse_Data, BirdTraits, by = c("Ebird_Species" = "Species2"))

#Get all the unique species present in the dataset

require(usdm)
require(psych)
require(vegan)
require(factoextra)
require(ggpubr)
require(tidyverse)

PCA_Data <- TraitData_Hansen[!duplicated(TraitData_Hansen$Ebird_Species),]

colnames(TraitData_Hansen)
PCA_Data <- PCA_Data %>% dplyr::select(c("Ebird_Species","Beak.Length_Culmen":"Mass","Habitat","Trophic.Niche"))

colnames(PCA_Data)

summary(PCA_Data)

hist(PCA_Data$Beak.Depth)

#Transform trait data
PCA_Data <- PCA_Data %>% mutate_if(is.numeric, log)

#omit NAs
PCA_Data <- na.omit(PCA_Data)

###PCA###

##1st step##

#Trophic
Trophic_pca_data <- PCA_Data %>% dplyr::select(-c(6:12))
colnames(PCA_Data)
colnames(Trophic_pca_data)
summary(Trophic_pca_data)

Trophic_pca <- Trophic_pca_data %>%
  dplyr::select(where(is.numeric)) %>%
  prcomp()

#Extract PC scores for first two component and add to data frame
Trophic_pca_data$PC1 <- Trophic_pca$x[, 1] # indexing the first column
Trophic_pca_data$PC2 <- Trophic_pca$x[, 2] # indexing the second column


p <- ggplot(data = Trophic_pca_data, aes(x = PC1, y = PC2, color = Trophic.Niche, shape = Trophic.Niche)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  guides(color = guide_legend(title = "Trophic Niche"), shape = guide_legend(title = "Trophic Niche")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  geom_point(alpha = 0.8, size = 2) 
p

p <- p + stat_ellipse(data = Trophic_pca_data %>% filter(Trophic.Niche != "Aquatic predator", Trophic.Niche != "Herbivore aquatic", Trophic.Niche != "Herbivore terrestrial", Trophic.Niche != "Nectarivore", Trophic.Niche != "Vertivore"),
                      geom="polygon", aes(fill = Trophic.Niche), 
                      alpha = 0.2, 
                      show.legend = FALSE, 
                      level = 0.95) +
  xlab("PC1 (85.9%)") + 
  ylab("PC2 (11.6%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  theme(text = element_text(size = 20))  
p


summary(Trophic_pca)
Trophic_pca$rotation

#Scree plot
plot(Trophic_pca, type="lines", ylim=c(0,2))

#Eigenvalues 
eigenvalues <- Trophic_pca$sdev^2
eigenvalues

#Basic plots
fviz_pca_biplot(Trophic_pca, repel = TRUE, geom.ind = "point", ellipse.level=0.95, col.var = "black", labelsize=4)


#Locomotory
Locomotory_pca_data <- PCA_Data %>% dplyr::select(-c(2:5,12))

colnames(PCA_Data)
colnames(Locomotory_pca_data)
summary(Locomotory_pca_data)

Locomotory_pca <- Locomotory_pca_data %>%
  dplyr::select(where(is.numeric)) %>%
  prcomp()

#Extract PC scores for first two component and add to data frame
Locomotory_pca_data$PC1 <- Locomotory_pca$x[, 1] # indexing the first column
Locomotory_pca_data$PC2 <- Locomotory_pca$x[, 2] # indexing the second column


p2 <- ggplot(data = Locomotory_pca_data, aes(x = PC1, y = PC2, color = Trophic.Niche, shape = Trophic.Niche)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  guides(color = guide_legend(title = "Trophic Niche"), shape = guide_legend(title = "Trophic Niche")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  geom_point(alpha = 0.8, size = 2) 
p2

p2 <- p2 + stat_ellipse(data = Locomotory_pca_data %>% filter(Trophic.Niche != "Aquatic predator", Trophic.Niche != "Herbivore aquatic", Trophic.Niche != "Herbivore terrestrial", Trophic.Niche != "Nectarivore", Trophic.Niche != "Vertivore"),
                        geom="polygon", aes(fill = Trophic.Niche), 
                        alpha = 0.2, 
                        show.legend = FALSE, 
                        level = 0.95) +
  xlab("PC1 (78.9%)") + 
  ylab("PC2 (16%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  theme(text = element_text(size = 20))  
p2


summary(Locomotory_pca)
Locomotory_pca$rotation

#Scree plot
plot(Locomotory_pca, type="lines", ylim=c(0,2))

#Eigenvalues 
eigenvalues <- Locomotory_pca$sdev^2
eigenvalues

#Basic plots
fviz_pca_biplot(Locomotory_pca, repel = TRUE, geom.ind = "point", ellipse.level=0.95, col.var = "black", labelsize=4)


##2nd step##

#Body mass
Bodymass_pca_data <- PCA_Data
colnames(PCA_Data)

Bodymass_pca_data <- Bodymass_pca_data %>% dplyr::select(-c(2:14))
colnames(Bodymass_pca_data)

#Merge species names with PCs from 1st step PCA
Bodymass_pca_data  <- merge(x = Bodymass_pca_data, y = Trophic_pca_data[ , c("Ebird_Species","PC1", "PC2")], by = "Ebird_Species", all.x=TRUE)

#Change names of Principle components
names(Bodymass_pca_data)[names(Bodymass_pca_data) == 'PC1'] <- 'PC1_Trophic'
names(Bodymass_pca_data)[names(Bodymass_pca_data) == 'PC2'] <- 'PC2_Trophic'

#Merge species names with PCs from 1st step PCA and trophic niche and habitat data
Bodymass_pca_data  <- merge(x = Bodymass_pca_data, y = Locomotory_pca_data[ , c("Ebird_Species","PC1", "PC2", "Trophic.Niche", "Habitat")], by = "Ebird_Species", all.x=TRUE)

#Change names of Principle components
names(Bodymass_pca_data)[names(Bodymass_pca_data) == 'PC1'] <- 'PC1_Locomotory'
names(Bodymass_pca_data)[names(Bodymass_pca_data) == 'PC2'] <- 'PC2_Locomotory'

#Check names
colnames(Bodymass_pca_data)

#Do 2nd step PCA 
Bodymass_pca <- prcomp(Bodymass_pca_data[,c(2,4)])

summary(Bodymass_pca)

#Extract PC scores for first two component and add to data frame
Bodymass_pca_data$PC1_Bodymass <- Bodymass_pca$x[, 1] # indexing the first column
Bodymass_pca_data$PC2_Bodymass <- Bodymass_pca$x[, 2] # indexing the second column

#Plot
p3 <- ggplot(data = Bodymass_pca_data, aes(x = PC1_Bodymass, y = PC2_Bodymass, color = Trophic.Niche, shape = Trophic.Niche)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  guides(color = guide_legend(title = "Trophic Niche"), shape = guide_legend(title = "Trophic Niche")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  geom_point(alpha = 0.8, size = 2) 
p3

p3 <- p3 + stat_ellipse(data = Bodymass_pca_data %>% filter(Trophic.Niche != "Aquatic predator", Trophic.Niche != "Herbivore aquatic", Trophic.Niche != "Herbivore terrestrial", Trophic.Niche != "Nectarivore", Trophic.Niche != "Vertivore"),
                        geom="polygon", aes(fill = Trophic.Niche), 
                        alpha = 0.2, 
                        show.legend = FALSE, 
                        level = 0.95) +
  xlab("PC1 (85.3%)") + 
  ylab("PC2 (14.7%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  theme(text = element_text(size = 20))  
p3

#Scree plot
plot(Bodymass_pca, type="lines", ylim=c(0,2))

#Eigenvalues 
eigenvalues <- Bodymass_pca$sdev^2
eigenvalues

#Basic plots
fviz_pca_biplot(Bodymass_pca, repel = TRUE, geom.ind = "point", ellipse.level=0.95, col.var = "black", labelsize=4)

#Plot all
require(gridExtra)
grid.arrange(p, p2, p3, ncol=3)
dev.off()

#Data frame with just the PCs needed for functional metric calculations
Species_PC <- Bodymass_pca_data %>% dplyr::select(c("Ebird_Species", "PC1_Bodymass", "PC2_Trophic", "PC2_Locomotory"))
colnames(Species_PC)

write.csv(Species_PC, "PCA_values.csv")


#PCA with both traits and body size

colnames(Bodymass_pca_data)
Final_pca_data <- Bodymass_pca_data %>% dplyr::select(c("Ebird_Species","PC2_Trophic","PC2_Locomotory","PC1_Bodymass","Trophic.Niche","Habitat"))

colnames(Final_pca_data)
Final_pca <- prcomp(Final_pca_data[,c(2:4)])

#Extract PC scores for first two component and add to data frame
Final_pca_data$PC1 <- Final_pca$x[,1]  # indexing the first column
Final_pca_data$PC2 <- Final_pca$x[, 2] # indexing the second column

#Basic plots
fviz_pca_biplot(Final_pca, repel = TRUE, geom.ind = "point", ellipse.level=0.95, col.var = "black", labelsize=4)

#Final plot

p4 <- ggplot(data = Final_pca_data, aes(x = PC1, y = PC2, color = Trophic.Niche, shape = Trophic.Niche)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  guides(color = guide_legend(title = "Trophic Niche"), shape = guide_legend(title = "Trophic Niche")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  geom_point(alpha = 0.8, size = 2) 
p4

p4 <- p4 + stat_ellipse(data = Final_pca_data %>% filter(Trophic.Niche != "Aquatic predator", Trophic.Niche != "Herbivore aquatic", Trophic.Niche != "Herbivore terrestrial", Trophic.Niche != "Nectarivore", Trophic.Niche != "Vertivore"),
                        geom="polygon", aes(fill = Trophic.Niche), 
                        alpha = 0.2, 
                        show.legend = FALSE, 
                        level = 0.95) +
  xlab("PC1 (83.1%)") + 
  ylab("PC2 (11.9%)") +
  theme_minimal() +
  theme(panel.grid = element_blank(), 
        panel.border = element_rect(fill= "transparent")) +
  theme(text = element_text(size = 20))  
p4
