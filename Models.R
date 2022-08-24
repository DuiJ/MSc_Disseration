setwd("~/Library/CloudStorage/OneDrive-ImperialCollegeLondon/MSc EEC/Project")

#Get packages

require(usdm)
require(psych)
require(lmtest)
require(lme4)
require(sjPlot)
require(lmerTest)

###Data###

###Species Richness###

#Overall 

Species_Richness_Data <- Species_Richness_Data %>% dplyr::filter(Transect != "T1_17322")

SR_Data <- Species_Richness_Data 
summary(SR_Data)
SR_Data <- SR_Data %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
SR_Data <- SR_Data %>% 
  na.omit()

SR_Data$Locality <- as.factor(SR_Data$Locality)
SR_Data$Season <- as.factor(SR_Data$Season)


#Granivores

Species_Richness_Trophic_Data <- Species_Richness_Trophic_Data %>% dplyr::filter(Transect != "T1_17322")

SRG_Data <- Species_Richness_Trophic_Data %>% subset(Trophic.Niche=="Granivore") 

SRG_Data <- SRG_Data %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
SRG_Data <- SRG_Data %>% 
  na.omit()

SRG_Data$Locality <- as.factor(SRG_Data$Locality)
SRG_Data$Season <- as.factor(SRG_Data$Season)

#Invertivores

Species_Richness_Trophic_Data <- Species_Richness_Trophic_Data %>% dplyr::filter(Transect != "T1_17322")

SRI_Data <- Species_Richness_Trophic_Data %>% subset(Trophic.Niche=="Invertivore") 

SRI_Data <- SRI_Data %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
SRI_Data <- SRI_Data %>% 
  na.omit()

SRI_Data$Locality <- as.factor(SRI_Data$Locality)
SRI_Data$Season <- as.factor(SRI_Data$Season)


#Overall forest v non forest#

Species_Richness_Habitat_Overall_Data <- Species_Richness_Habitat_Overall_Data %>% dplyr::filter(Transect != "T1_17322")

SR_H_Data <- Species_Richness_Habitat_Overall_Data 

SR_H_Data <- SR_H_Data %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
SR_H_Data <- SR_H_Data %>% 
  na.omit()

SR_H_Data$Locality <- as.factor(SR_H_Data$Locality)
SR_H_Data$Season <- as.factor(SR_H_Data$Season)

#Overall forest 
SR_H_F_Data <- SR_H_Data %>% subset(Habitat.Type=="Forest")

#Overall non-forest 
SR_H_NF_Data <- SR_H_Data %>% subset(Habitat.Type=="Non Forest")


#Invertivore forest v non forest#

Species_Richness_Habitat_Invert_Data <- Species_Richness_Habitat_Invert_Data %>% dplyr::filter(Transect != "T1_17322")

SR_HI_Data <- Species_Richness_Habitat_Invert_Data 

SR_HI_Data <- SR_HI_Data %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
SR_HI_Data <- SR_HI_Data %>% 
  na.omit()

SR_HI_Data$Locality <- as.factor(SR_HI_Data$Locality)
SR_HI_Data$Season <- as.factor(SR_HI_Data$Season)

#Invertivore forest 
SR_HI_F_Data <- SR_HI_Data %>% subset(Habitat.Type=="Forest")

#Invertivore non-forest
SR_HI_NF_Data <- SR_HI_Data %>% subset(Habitat.Type=="Non Forest")

##Species Richness by habitat type and locality##

#Overall forest species richness#

#CHI & MYA
SR_HF_CM_data <- SR_H_F_Data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")

#MIN & ZAM

SR_HF_ZM_data <- SR_H_F_Data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")

#Overall non forest species richness#

#CHI & MYA
SR_HNF_CM_data <- SR_H_NF_Data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")

#MIN & ZAM

SR_HNF_ZM_data <- SR_H_NF_Data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")

##Invertivore forest species richness##

#CHI & MYA
SR_HIF_CM_data <- SR_HI_F_Data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")

#MIN & ZAM

SR_HIF_ZM_data <- SR_HI_F_Data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")

##Invertivore non forest species richness##

#CHI & MYA
SR_HINF_CM_data <- SR_HI_NF_Data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")

#MIN & ZAM

SR_HINF_ZM_data <- SR_HI_NF_Data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")


##Functional dispersion data##

#Overall 

FD_data_Overall <- FD_data_Overall %>% dplyr::filter(Transect != "T1_17322")

FDO_data <- FD_data_Overall %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
FDO_data <- FDO_data %>% 
  na.omit()

FDO_data$Locality <- as.factor(FDO_data$Locality)
FDO_data$Season <- as.factor(FDO_data$Season)

#Granivores

FD_data_gran <- FD_data_gran %>% dplyr::filter(Transect != "T1_17322")

FDG_data <- FD_data_gran %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
FDG_data <- FDG_data %>% 
  na.omit()

FDG_data$Locality <- as.factor(FDG_data$Locality)
FDG_data$Season <- as.factor(FDG_data$Season)

#Invertivores

FD_data_invert <- FD_data_invert %>% dplyr::filter(Transect != "T1_17322")

FDI_data <- FD_data_invert %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
FDI_data <- FDI_data %>% 
  na.omit()

FDI_data$Locality <- as.factor(FDI_data$Locality)
FDI_data$Season <- as.factor(FDI_data$Season)

#Overall forest v non forest#

FD_data_of <- FD_data_of %>% dplyr::filter(Transect != "T1_17322")

FD_HF_data <- FD_data_of %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
FD_HF_data <- FD_HF_data %>% 
  na.omit()

FD_HF_data$Locality <- as.factor(FD_HF_data$Locality)
FD_HF_data$Season <- as.factor(FD_HF_data$Season)


FD_data_onf <- FD_data_onf %>% dplyr::filter(Transect != "T1_17322")

FD_HNF_data <- FD_data_onf %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
FD_HNF_data <- FD_HNF_data %>% 
  na.omit()

FD_HNF_data$Locality <- as.factor(FD_HNF_data$Locality)
FD_HNF_data$Season <- as.factor(FD_HNF_data$Season)

#Invertivore forest v non forest#

FD_data_if <- FD_data_if %>% dplyr::filter(Transect != "T1_17322")

FD_HIF_data <- FD_data_if %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
FD_HIF_data <- FD_HIF_data %>% 
  na.omit()

FD_HIF_data$Locality <- as.factor(FD_HIF_data$Locality)
FD_HIF_data$Season <- as.factor(FD_HIF_data$Season)


FD_data_inf <- FD_data_inf %>% dplyr::filter(Transect != "T1_17322")

FD_HINF_data <- FD_data_inf %>% 
  separate(Transect, into = c("Site", "Transect"), sep = "_") 
FD_HINF_data <- FD_HINF_data %>% 
  na.omit()

FD_HINF_data$Locality <- as.factor(FD_HINF_data$Locality)
FD_HINF_data$Season <- as.factor(FD_HINF_data$Season)

###Functional dispersion by habitat type and locality###

write.csv(FD_HF_data, "FD_scores_AllForest_species")
write.csv(FD_HNF_data, "FD_scores_AllNonForest_species")
write.csv(FD_HIF_data, "FD_scores_InvertForest_species")
write.csv(FD_HINF_data, "FD_scores_InvertNonForest_species")

##Overall forest functional dispersion##

#CHI & MYA
FD_HF_CM_data <- FD_HF_data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")

#MIN & ZAM

FD_HF_ZM_data <- FD_HF_data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")

##Overall non forest functional dispersion##

#CHI & MYA
FD_HNF_CM_data <- FD_HNF_data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")

#MIN & ZAM

FD_HNF_ZM_data <- FD_HNF_data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")

##Invertivore forest functional dispersion##

#CHI & MYA
FD_HIF_CM_data <- FD_HIF_data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")

#MIN & ZAM

FD_HIF_ZM_data <- FD_HIF_data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")

##Invertivore non forest functional dispersion##

#CHI & MYA
FD_HINF_CM_data <- FD_HINF_data %>% subset(Locality == "Chitokoloki"|Locality == "Mayukwayukwa")

#MIN & ZAM

FD_HINF_ZM_data <- FD_HINF_data %>% subset(Locality == "Mwinilunga"|Locality == "Zambezi")



###Models###


##Overall species richness##

model_SR1 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SR_Data)
summary(model_SR1) #Non significant #Boundary singular fit

model_SR2 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Locality/Transect), family = poisson, data=SR_Data)
summary(model_SR2) #Non significant #Boundary singular fit

model_SR3 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Transect), family = poisson, data=SR_Data)
summary(model_SR3) #Non significant #Boundary singular fit

model_SR4 <- glmer(Richness ~ log(MeanForestCover1000m+1) + log(Trees+1) + (1|Transect), family = poisson, data=SR_Data)
summary(model_SR4) #Model failed to converge with max|grad| = 0.00233658 (tol = 0.002, component 1)

model_SR5 <- glmer(Richness ~ log(MeanForestCover1000m+1) + (1|Transect), family = poisson, data=SR_Data)
summary(model_SR5) #Model failed to converge with max|grad| = 0.00233658 (tol = 0.002, component 1)

model_SR6 <- glmer(Richness ~ log(MeanForestCover1000m+1) + (1|Site), family = poisson, data=SR_Data)
summary(model_SR6) 

anova(model_SR3,model_SR4, test = "logLik") #model 4/5 is the optimum model 


##Granivore species richness##

model_SRG1 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SRG_Data)
summary(model_SRG1) #Non significant #Boundary singular fit

model_SRG2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SRG_Data)
summary(model_SRG2) #Non significant #Boundary singular fit

model_SRG3 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Season/Transect), family = poisson, data=SRG_Data)
summary(model_SRG3) # Model failed to converge with max|grad| = 0.00362967 (tol = 0.002, component 1)

model_SRG4 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Season), family = poisson, data=SRG_Data)
summary(model_SRG4) # Model failed to converge with max|grad| = 0.00362967 (tol = 0.002, component 1)


anova(model_SRG4,model_SRG3, test = "logLik") #model 3 is the optimum model 

model_SRG6 <- glm(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1), family = poisson, data=SRG_Data)
summary(model_SRG6) #Non significant #Boundary singular fit


##Invertivore species richness##

model_SRI1 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SRI_Data)
summary(model_SRI1) #Non significant #Boundary singular fit

model_SRI2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SRI_Data)
summary(model_SRI2) #Non significant #Boundary singular fit

model_SRI3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), family = poisson, data=SRI_Data)
summary(model_SRI3) 

model_SRI4 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = poisson, data=SRI_Data)
summary(model_SRI4) #Significant increase with trees - almost significant increase with FC

model_SRI5 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Site), family = poisson, data=SRI_Data)
summary(model_SRI5) 

model_SRI6 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), family = poisson, data=SRI_Data)
summary(model_SRI6)

anova(model_SRI3,model_SRI4, test = "logLik") #model 3 is the optimum model 


##Overall forest species richness##

model_SR_H_F_1 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SR_H_F_Data)
summary(model_SR_H_F_1) #Model failed to converge with max|grad| = 0.64773 (tol = 0.002, component 1)

model_SR_H_F_2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SR_H_F_Data)
summary(model_SR_H_F_2) #Model failed to converge with max|grad| = 0.64773 (tol = 0.002, component 1)

model_SR_H_F_3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality), family = poisson, data=SR_H_F_Data)
summary(model_SR_H_F_3) 

model_SR_H_F_4 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality), family = poisson, data=SR_H_F_Data)
summary(model_SR_H_F_4) #Model failed to converge with max|grad| = 0.64773 (tol = 0.002, component 1)

plot_model(model_SR_H_F_3, show.values = T, show.intercept = T)

anova(model_SR_H_F_3,model_SR_H_F_4, test = "logLik") #model 3 is the optimum model 


##Overall non forest species richness##

model_SR_H_NF_1 <- glmer(Richness ~ log(MeanForestCover1000m+1)*log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SR_H_NF_Data)
summary(model_SR_H_NF_1) #Model failed to converge with max|grad| = 0.64773 (tol = 0.002, component 1)

model_SR_H_NF_2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SR_H_NF_Data)
summary(model_SR_H_NF_2) #boundary (singular) fit

model_SR_H_NF_3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = poisson, data=SR_H_NF_Data)
summary(model_SR_H_NF_3) 

model_SR_H_NF_4 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = poisson, data=SR_H_NF_Data)
summary(model_SR_H_NF_4) #Model failed to converge with max|grad| = 0.00325447 (tol = 0.002, component 1)

model_SR_H_NF_5 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality), family = poisson, data=SR_H_NF_Data)
summary(model_SR_H_NF_5) #Model failed to converge with max|grad| = 0.64773 (tol = 0.002, component 1)

anova(model_SR_H_NF_3,model_SR_H_NF_5, test = "logLik") #model 3 is the optimum model 

require(usdm)
colnames(SR_H_NF_Data)
df = data.frame(SR_H_NF_Data[,c(7,10)])
corvif(df)
usdm::vif(df)


##Invertivore forest species richness##

model_SR_HI_F_1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SR_HI_F_Data)
summary(model_SR_HI_F_1) 

model_SR_HI_F_2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality), family = poisson, data=SR_HI_F_Data)
summary(model_SR_HI_F_2)

model_SR_HI_F_3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), family = poisson, data=SR_HI_F_Data)
summary(model_SR_HI_F_3)

model_SR_HI_F_4 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality), family = poisson, data=SR_HI_F_Data)
summary(model_SR_HI_F_4)

anova(model_SR_HI_F_4,model_SR_HI_F_2, test = "logLik") #model 3 is the optimum model 


##Invertivore non forest species richness##

model_SR_HI_NF_1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SR_HI_NF_Data)
summary(model_SR_HI_NF_1) 

model_SR_HI_NF_2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = poisson, data=SR_HI_NF_Data)
summary(model_SR_HI_NF_2)

model_SR_HI_NF_3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = poisson, data=SR_HI_NF_Data)
summary(model_SR_HI_NF_3)

model_SR_HI_NF_4 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality), family = poisson, data=SR_HI_NF_Data)
summary(model_SR_HI_NF_4)

anova(model_SR_HI_NF_1,model_SR_HI_NF_2, test = "logLik") #model 2 is the optimum model 


###Functional Dispersion###


##Overall functional dispersion##

model_FD1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FDO_data)
summary(model_FD1) 

model_FD2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FDO_data)
summary(model_FD2)

model_FD3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FDO_data)
summary(model_FD3)

anova(model_FD3,model_FD2, test = "logLik") #model 2 is the optimum model 


##Granivore functional dispersion##

model_FDG1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FDG_data)
summary(model_FDG1)#boundary (singular) fit: see ?isSingular

model_FDG2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FDG_data)
summary(model_FDG2)

model_FDG3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FDG_data)
summary(model_FDG3)

anova(model_FDG3,model_FDG2, test = "logLik") #model 3 is the optimum model 


##Invertivore functional dispersion##

model_FDI1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FDI_data)
summary(model_FDI1)

model_FDI2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FDI_data)
summary(model_FDI2)

model_FDI3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FDI_data)
summary(model_FDI3)

anova(model_FDI3,model_FDI2, test = "logLik") #model 2 is the optimum model 


##Overall forest functional dispersion##

model_FDHF1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HF_data)
summary(model_FDHF1)

model_FDHF2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HF_data)
summary(model_FDHF2)

model_FDHF3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HF_data)
summary(model_FDHF3)

anova(model_FDHF3,model_FDHF2, test = "logLik") #model 2 is the optimum model 

##Overall non forest functional dispersion##

model_FDHNF1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HNF_data)
summary(model_FDHNF1)

model_FDHNF2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HNF_data)
summary(model_FDHNF2)

model_FDHNF3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FD_HNF_data)
summary(model_FDHNF3)

anova(model_FDHNF3,model_FDHNF2, test = "logLik") #model 3 is the optimum model 


##Invertivore forest functional dispersion##

model_FDHIF1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HIF_data)
summary(model_FDHIF1) #boundary (singular) fit

model_FDHIF2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HIF_data)
summary(model_FDHIF2)

model_FDHIF3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HIF_data)
summary(model_FDHIF3)

anova(model_FDHIF3,model_FDHIF2, test = "logLik") #model 2 is the optimum model 


##Invertivore non forest functional dispersion##

model_FDHINF1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HINF_data)
summary(model_FDHINF1) 

model_FDHINF2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HINF_data)
summary(model_FDHINF2)

model_FDHINF3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FD_HINF_data)
summary(model_FDHINF3)

anova(model_FDHINF3,model_FDHINF2, test = "logLik") #model 2 is the optimum model 


###Functional dispersion by habitat type and locality###

##Overall forest functional dispersion##

#CHI & MYA

model_FDHFCM1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HF_CM_data)
summary(model_FDHFCM1) #boundary (singular) fit: see ?isSingular

model_FDHFCM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), data=FD_HF_CM_data)
summary(model_FDHFCM2)

model_FDHFCM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HF_CM_data)
summary(model_FDHFCM3)

anova(model_FDHFCM3,model_FDHFCM2, test = "logLik") #model 3 is the optimum model 


#MIN & ZAM

model_FDHFZM1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HF_ZM_data)
summary(model_FDHFZM1) 

model_FDHFZM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HF_ZM_data)
summary(model_FDHFZM2) 

model_FDHFZM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HF_ZM_data)
summary(model_FDHFZM3)

anova(model_FDHFZM3,model_FDHFZM2, test = "logLik") #model 3 is the optimum model 

##Overall non forest functional dispersion##

#CHI & MYA

model_FDHNFCM1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HNF_CM_data)
summary(model_FDHNFCM1) 

model_FDHNFCM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), data=FD_HNF_CM_data)
summary(model_FDHNFCM2)

model_FDHNFCM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality), data=FD_HNF_CM_data)
summary(model_FDHNFCM3)

anova(model_FDHNFCM3,model_FDHNFCM2, test = "logLik") #model 3 is the optimum model 

#MIN & ZAM

model_FDHNFZM1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HNF_ZM_data)
summary(model_FDHNFZM1) 

model_FDHNFZM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), data=FD_HNF_ZM_data)
summary(model_FDHNFZM2)

model_FDHNFZM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HNF_ZM_data)
summary(model_FDHNFZM3)

anova(model_FDHNFZM3,model_FDHNFZM2, test = "logLik") #model 3 is the optimum model 


##Invertivore forest functional dispersion##

#CHI & MYA

model_FDHIFCM1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HIF_CM_data)
summary(model_FDHIFCM1) 

model_FDHIFCM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HIF_CM_data)
summary(model_FDHIFCM2)

model_FDHIFCM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FD_HIF_CM_data)
summary(model_FDHIFCM3)

model_FDHIFCM4 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HIF_CM_data)
summary(model_FDHIFCM4)

anova(model_FDHIFCM4,model_FDHIFCM2, test = "logLik") #model 2 is the optimum model 


#MIN & ZAM

model_FDHIFZM1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HIF_ZM_data)
summary(model_FDHIFZM1) 

model_FDHIFZM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HIF_ZM_data)
summary(model_FDHIFZM2)

model_FDHIFZM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FD_HIF_ZM_data)
summary(model_FDHIFZM3)

anova(model_FDHIFZM3,model_FDHIFZM2, test = "logLik") #model 3 is the optimum model 

##Overall non forest functional dispersion##

#CHI & MYA

model_FDHINFCM1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HINF_CM_data)
summary(model_FDHINFCM1) 

model_FDHINFCM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), data=FD_HINF_CM_data)
summary(model_FDHINFCM2)

model_FDHINFCM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HINF_CM_data)
summary(model_FDHINFCM3)

anova(model_FDHINFCM2,model_FDHINFCM3, test = "logLik") #model 3 is the optimum model 

#MIN & ZAM

model_FDHINFZM1 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), data=FD_HINF_ZM_data)
summary(model_FDHINFZM1) 

model_FDHINFZM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), data=FD_HINF_ZM_data)
summary(model_FDHINFZM2)

model_FDHINFZM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HINF_ZM_data)
summary(model_FDHINFZM3)

anova(model_FDHINFZM2,model_FDHINFZM3, test = "logLik") #model 3 is the optimum model 


###Species richness by habitat type and locality###

##Overall forest species richness##

#CHI & MYA

model_SRHFCM1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = "poisson", data=SR_HF_CM_data)
summary(model_SRHFCM1) 

model_SRHFCM2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality), family = "poisson", data=SR_HF_CM_data)
summary(model_SRHFCM2) 

model_SRHFCM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), family = "poisson", data=SR_HF_CM_data)
summary(model_SRHFCM3)

anova(model_SRHFCM3,model_SRHFCM2, test = "logLik") #model 3 is the optimum model 

#MIN & ZAM

model_SRHFZM1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = "poisson", data=SR_HF_ZM_data)
summary(model_SRHFZM1) 

model_SRHFZM2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = "poisson", data=SR_HF_ZM_data)
summary(model_SRHFZM2) 

model_SRHFZM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HF_ZM_data)
summary(model_SRHFZM3)

anova(model_SRHFZM2,model_SRHFZM3, test = "logLik") #model 3 is the optimum model 

##Overall non forest species richness##

#CHI & MYA
model_SRHNFCM1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = "poisson", data=SR_HNF_CM_data)
summary(model_SRHNFCM1) 

model_SRHNFCM2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = "poisson", data=SR_HNF_CM_data)
summary(model_SRHNFCM2) 

model_SRHNFCM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HNF_CM_data)
summary(model_SRHNFCM3) 

anova(model_SRHNFCM3,model_SRHNFCM2, test = "logLik") #model 3 is the optimum model 

#MIN & ZAM
model_SRHNFZM1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = "poisson", data=SR_HNF_ZM_data)
summary(model_SRHNFZM1) 

model_SRHNFZM2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = "poisson", data=SR_HNF_ZM_data)
summary(model_SRHNFZM2)

model_SRHNFZM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HNF_ZM_data)
summary(model_SRHNFZM3)

anova(model_SRHNFZM3,model_SRHNFZM2, test = "logLik") #model 3 is the optimum model 


##Invertivore forest species richness##

#CHI & MYA

model_SRHIFCM1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = "poisson", data=SR_HIF_CM_data)
summary(model_SRHIFCM1) 

model_SRHIFCM2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), family = "poisson", data=SR_HIF_CM_data)
summary(model_SRHIFCM2) 

model_SRHIFCM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), family = "poisson", data=SR_HIF_CM_data)
summary(model_SRHIFCM3)

anova(model_SRHIFCM3,model_SRHIFCM2, test = "logLik") #model 3 is the optimum model 

#MIN & ZAM

model_SRHIFZM1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = "poisson", data=SR_HIF_ZM_data)
summary(model_SRHIFZM1) 

model_SRHIFZM2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = "poisson", data=SR_HIF_ZM_data)
summary(model_SRHIFZM2) 

model_SRHIFZM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HIF_ZM_data)
summary(model_SRHIFZM3)

anova(model_SRHIFZM2,model_SRHIFZM3, test = "logLik") #model 3 is the optimum model 

##Invertivore non forest species richness##

#CHI & MYA

model_SRHINFCM1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = "poisson", data=SR_HINF_CM_data)
summary(model_SRHINFCM1) 

model_SRHINFCM2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = "poisson", data=SR_HINF_CM_data)
summary(model_SRHINFCM2) 

model_SRHINFCM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HINF_CM_data)
summary(model_SRHINFCM3)

anova(model_SRHINFCM1,model_SRHINFCM2, test = "logLik") #model 3 is the optimum model 

#MIN & ZAM

model_SRHINFZM1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = "poisson", data=SR_HINF_ZM_data)
summary(model_SRHINFZM1) 

model_SRHINFZM2 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = "poisson", data=SR_HINF_ZM_data)
summary(model_SRHINFZM2) 

model_SRHINFZM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HINF_ZM_data)
summary(model_SRHINFZM3) 

anova(model_SRHINFZM2,model_SRHINFZM3, test = "logLik") #model 3 is the optimum model 

#######################
#####Final models######
#######################

###Species richness by trophic niche###

model_SR4 <- glmer(Richness ~ log(MeanForestCover1000m+1) + log(Trees+1) + (1|Transect), family = poisson, data=SR_Data)
summary(model_SR4) #Mean forest cover is significant #Model failed to converge with max|grad| = 0.00233658 (tol = 0.002, component 1) 

model_SRG3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), family = poisson, data=SRG_Data)
summary(model_SRG3) #Mean forest cover is significant #Trees are significant

model_SRI3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), family = poisson, data=SRI_Data)
summary(model_SRI3) #trees are significant 
?tab_model

#Model table
sjPlot:: tab_model(model_SR4,model_SRI3,model_SRG3, 
                   show.stat=T, show.est =T, string.stat = "z value", string.p = "p value", show.ci = F, dv.labels = c("Overall SR","Invertivore SR","Granivore SR"),
                   pred.labels =c("(Intercept)", "log(Mean Forest Cover)", "log(Trees)"),
                   CSS = list(css.firsttablecol = 'font-weight: bold; text-align:left;',
                              css.table = '+font-size: 25;',
                              css.depvarhead = 'font-weight: bold; text-align:center;padding:0.5cm;',
                              css.thead = 'font-weight: bold; text-align:center;',
                              css.tdata = 'padding:0.2cm'))

###Species richness by habitat type###

model_SR_H_F_3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality), family = poisson, data=SR_H_F_Data)
summary(model_SR_H_F_3) #Trees are significant (increase)

model_SR_H_NF_3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Locality/Transect), family = poisson, data=SR_H_NF_Data)
summary(model_SR_H_NF_3) #Trees and forest cover are significant (decrease)

model_SR_HI_F_3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), family = poisson, data=SR_HI_F_Data)
summary(model_SR_HI_F_3) #Trees are significant (increase)

model_SR_HI_NF_1 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Locality/Transect), family = poisson, data=SR_HI_NF_Data)
summary(model_SR_HI_NF_1) #Trees are significant (decrease)



###Functional dispersion by trophic niche###

model_FD2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FDO_data)
summary(model_FD2) #Trees and forest cover are significant (decrease)

model_FDG3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FDG_data)
summary(model_FDG3) #No significance

model_FDI2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FDI_data)
summary(model_FDI2) #Trees are significant (decrease)

#Model table
sjPlot:: tab_model(model_FD2,model_FDI2,model_FDG3, 
                   show.stat=T, show.est =T, string.stat = "t value", string.p = "p value", show.ci = F, dv.labels = c("Overall FDis","Invertivore FDis","Granivore FDis"),
                   pred.labels =c("(Intercept)", "log(Mean Forest Cover)", "log(Trees)"),
                   CSS = list(css.firsttablecol = 'font-weight: bold; text-align:left;',
                              css.table = '+font-size: 25;',
                              css.depvarhead = 'font-weight: bold; text-align:center;padding:0.5cm;',
                              css.thead = 'font-weight: bold; text-align:center;',
                              css.tdata = 'padding:0.2cm'))


###Functional dispersion by habitat type###

model_FDHF2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HF_data)
summary(model_FDHF2) #Trees and forest cover are significant (decrease)

model_FDHNF3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FD_HNF_data)
summary(model_FDHNF3) #No significance

model_FDHIF2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HIF_data)
summary(model_FDHIF2) #Trees are significant (decrease)

model_FDHINF2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HINF_data)
summary(model_FDHINF2) #No significance

###Functional dispersion by habitat type and locality###

#Overall forest and non forest

#CHI & MYA
model_FDHFCM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HF_CM_data)
summary(model_FDHFCM3) #Non significant but decline

model_FDHNFCM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HNF_CM_data)
summary(model_FDHNFCM3) #Non significant but decline

#MIN & ZAM
model_FDHFZM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HF_ZM_data)
summary(model_FDHFZM3) #Trees and forest cover are significant (decrease)

model_FDHNFZM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HNF_ZM_data)
summary(model_FDHNFZM3) #Forest cover is significant (decrease)

#Invertivore forest and non forest

#CHI & MYA
model_FDHIFCM2 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season/Transect), data=FD_HIF_CM_data)
summary(model_FDHIFCM2) #Non significant but decline

model_FDHINFCM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HINF_CM_data)
summary(model_FDHINFCM3) #Non significant but decline

#MIN & ZAM

model_FDHIFZM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), data=FD_HIF_ZM_data)
summary(model_FDHIFZM3) #Trees and forest cover are significant (decrease)

model_FDHINFZM3 <- lmer(FDis ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), data=FD_HINF_ZM_data)
summary(model_FDHINFZM3) #Forest cover is significant (decrease)


###Species richness by habitat type and locality###

#Overall forest and non forest

#CHI & MYA
model_SRHFCM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), family = "poisson", data=SR_HF_CM_data)
summary(model_SRHFCM3) #Trees are significant (Increase)

model_SRHNFCM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HNF_CM_data)
summary(model_SRHNFCM3) #forest cover is significant (decrease)

#MIN & ZAM
model_SRHFZM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HF_ZM_data)
summary(model_SRHFZM3) #Trees are significant (Increase)

model_SRHNFZM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HNF_ZM_data)
summary(model_SRHNFZM3) #Trees and forest cover are significant (decrease)

#Invertivore forest and non forest

#CHI & MYA

model_SRHIFCM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Season), family = "poisson", data=SR_HIF_CM_data)
summary(model_SRHIFCM3) #Trees are significant (increase)

model_SRHINFCM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HINF_CM_data)
summary(model_SRHINFCM3) #No significance

#MIN & ZAM

model_SRHIFZM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HIF_ZM_data)
summary(model_SRHIFZM3) #Trees are significant (Increase)

model_SRHINFZM3 <- glmer(Richness ~ log(MeanForestCover1000m+1)+log(Trees+1) + (1|Transect), family = "poisson", data=SR_HINF_ZM_data)
summary(model_SRHINFZM3) #Trees and forest cover are significant (decrease)

