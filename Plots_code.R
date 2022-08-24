setwd("~/Library/CloudStorage/OneDrive-ImperialCollegeLondon/MSc EEC/Project")

library("ggplot2")
library("gridExtra")
library("grid")
library(patchwork)

#############Figure 3#############
##################################
#Species richness by trophic group 

SRplot <- ggplot(data = SR_Data, aes(MeanForestCover1000m, Richness)) +
  geom_point() + geom_smooth(method = glm, se = F, col = "black") +
  theme_classic() +
  scale_y_continuous(limits = c(0,60),breaks = seq(0, 60, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  ggtitle("All Species") +
  labs(x = '', y = 'SR') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=30, y=5, label= "p = 0.034*, z = -2.123", cex = 7)

SRGplot <- ggplot(data = SRG_Data, aes(MeanForestCover1000m, Richness)) +
  geom_point(col = "#7CAE00") + geom_smooth(method = glm, se=F, col = "#7CAE00") +
  theme_classic() +
  scale_y_continuous(limits = c(0,60),breaks = seq(0, 60, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  ggtitle("Granivores") +
  labs(x = '', y = 'SR') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=30, y=55, label= "p = 0.011*, z = -2.553", cex = 7, col = "#7CAE00")

SRIplot <- ggplot(data = SRI_Data, aes(MeanForestCover1000m, Richness)) +
  geom_point(col = "#C77CFF") + geom_smooth(method = glm, se=F, col = "#C77CFF") +
  theme_classic() +
  scale_y_continuous(limits = c(0,60),breaks = seq(0, 60, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  ggtitle("Invertivores") +
  labs(x = 'Mean Forest Cover at 1000m (%)', y = 'SR') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=30, y=55, label= "p = 0.183, z = 1.333", cex = 7, col = "#C77CFF")


SRplot2 <- ggplot(data = SR_Data, aes(log(Trees+1), Richness)) +
  geom_point() + geom_smooth(method = glm, se = F, col = "black") +
  theme_classic() +
  scale_y_continuous(limits = c(0,60),breaks = seq(0, 60, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = '', y = 'SR') +
  theme(text = element_text(size = 20)) +
  annotate("text", x=2.2, y=5, label= "p = 0.625, z = 0.489", cex = 7)

SRGplot2 <- ggplot(data = SRG_Data, aes(log(Trees+1), Richness)) +
  geom_point(col = "#7CAE00") + geom_smooth(method = glm, se=F, col = "#7CAE00") +
  theme_classic() +
  scale_y_continuous(limits = c(0,60),breaks = seq(0, 60, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = '', y = 'SR') +
  theme(text = element_text(size = 20)) +
  annotate("text", x=2.2, y=55, label= "p < 0.001*, z = -3.887", cex = 7, col = "#7CAE00")

SRIplot2 <- ggplot(data = SRI_Data, aes(log(Trees+1), Richness)) +
  geom_point(col = "#C77CFF") + geom_smooth(method = glm, se=F, col = "#C77CFF") +
  theme_classic() +
  scale_y_continuous(limits = c(0,60),breaks = seq(0, 60, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = 'log(Trees)', y = 'SR') +
  theme(text = element_text(size = 20)) +
  annotate("text", x=2.2, y=55, label= "p = 0.003*, z = 3.003", cex = 7, col = "#C77CFF")

grid.arrange(SRplot, SRIplot, SRGplot, SRplot2, SRIplot2, SRGplot2, ncol = 3, nrow = 2)

####################Figure 4##################
##############################################
#Species richness by habitat type and baseline

SR_H_plot2 <- ggplot(data = SR_H_Data, aes(log(Trees+1), Richness, col = Habitat.Type), show.legend = FALSE) +
  geom_point() + geom_smooth(method = glm, se = F) +
  theme_classic() +
  scale_y_continuous(limits = c(0,50),breaks = seq(0, 50, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = '', y = 'Overall SR') +
  ggtitle("All sites") +
  theme(text = element_text(size = 20), legend.position="none", plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=4.2, y=50, label= "p < 0.001*, z = 6.086", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=45, label= " p < 0.001*, z = -3.577", cex = 7, col = "#00BFC4")  

SR_HI_plot2 <- ggplot(data = SR_HI_Data, aes(log(Trees+1), Richness, col = Habitat.Type), show.legend = FALSE) +
  geom_point() + geom_smooth(method = glm, se = F) +
  theme_classic() +
  scale_y_continuous(limits = c(0,40),breaks = seq(0, 40, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = '', y = 'Invertivore SR') +
  theme(text = element_text(size = 20), legend.position="none") +
  annotate("text", x=4.2, y=40, label= "p < 0.001*, z = 7.148", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=35, label= " p < 0.001*, z = -4.060", cex = 7, col = "#00BFC4")


SR_HCM_plot <- ggplot() +
  geom_point(data = SR_HF_CM_data, aes(log(Trees+1), Richness, col = "#00BFC4")) +
  geom_point(data = SR_HNF_CM_data, aes(log(Trees+1), Richness, col = "#F8766D")) + 
  geom_smooth(data = SR_HF_CM_data, aes(x = log(Trees+1), y = Richness), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = SR_HNF_CM_data, aes(x = log(Trees+1), y = Richness), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() + 
  scale_y_continuous(limits = c(0,50),breaks = seq(0, 50, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  ggtitle("Shifted Baseline") +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=50, label= "p = 0.008*, z = 2.673", cex = 7, col = "#F8766D") +
  annotate("text", x=4.3, y=45, label= "p = 0.742, z = 0.329", cex = 7, col = "#00BFC4")  

SR_HZM_plot <- ggplot() +
  geom_point(data = SR_HF_ZM_data, aes(log(Trees+1), Richness, col = "#00BFC4")) +
  geom_point(data = SR_HNF_ZM_data, aes(log(Trees+1), Richness, col = "#F8766D")) + 
  geom_smooth(data = SR_HF_ZM_data, aes(x = log(Trees+1), y = Richness), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = SR_HNF_ZM_data, aes(x = log(Trees+1), y = Richness), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() +
  scale_y_continuous(limits = c(0,50),breaks = seq(0, 50, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  ggtitle("Natural Baseline") +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=50, label= "p < 0.001*, z = 5.086 ", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=45, label= "p < 0.001*, z = -3.836", cex = 7, col = "#00BFC4")  


SR_HICM_plot <- ggplot() +
  geom_point(data = SR_HIF_CM_data, aes(log(Trees+1), Richness, col = "#00BFC4")) +
  geom_point(data = SR_HINF_CM_data, aes(log(Trees+1), Richness, col = "#F8766D")) + 
  geom_smooth(data = SR_HIF_CM_data, aes(x = log(Trees+1), y = Richness), col = "#F8766D", method=glm, se=F) +
  geom_smooth(data = SR_HINF_CM_data, aes(x = log(Trees+1), y = Richness), col = "#00BFC4", method=glm, se=F) +
  theme_classic() +
  scale_y_continuous(limits = c(0,40),breaks = seq(0, 40, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = 'log(Trees)', y = '') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=40, label= "p < 0.001*, z = 3.550", cex = 7, col = "#F8766D") +
  annotate("text", x=4.3, y=35, label= "p = 0.795, z = -0.260", cex = 7, col = "#00BFC4")  

SR_HIZM_plot <- ggplot() +
  geom_point(data = SR_HIF_ZM_data, aes(log(Trees+1), Richness, col = "#00BFC4")) +
  geom_point(data = SR_HINF_ZM_data, aes(log(Trees+1), Richness, col = "#F8766D")) + 
  geom_smooth(data = SR_HIF_ZM_data, aes(x = log(Trees+1), y = Richness), col = "#F8766D", method=glm, se=F) +
  geom_smooth(data = SR_HINF_ZM_data, aes(x = log(Trees+1), y = Richness), col = "#00BFC4", method=glm, se=F) +
  theme_classic() +
  scale_y_continuous(limits = c(0,40),breaks = seq(0, 40, 10)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=40, label= "p < 0.001*, z = 5.645", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=35, label= " p < 0.001*, z = -4.636", cex = 7, col = "#00BFC4")  


combined <- SR_H_plot2 + SR_HCM_plot + SR_HZM_plot + SR_HI_plot2 + SR_HICM_plot + SR_HIZM_plot & theme(legend.position = "right")
combined + plot_layout(guides = "collect")


###############Figure 5################
#######################################
#Functional dispersion by trophic group 

FDplot <- ggplot(data = FDO_data, aes(MeanForestCover1000m, FDis)) +
  geom_point() + geom_smooth(method = glm, se = F, col = "black") +
  theme_classic() +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  scale_x_reverse(limits = c(100, 0)) +
  ggtitle("All Species") +
  labs(x = '', y = 'FDis') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=30, y=0.5, label= "p = 0.018*, t = -2.390", cex = 7)

FDGplot <- ggplot(data = FDG_data, aes(MeanForestCover1000m, FDis)) +
  geom_point(col = "#7CAE00") + geom_smooth(method = glm, se=F, col = "#7CAE00") +
  theme_classic() +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  scale_x_reverse(limits = c(100, 0)) +
  ggtitle("Granivores") +
  labs(x = '', y = 'FDis') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=30, y=0.5, label= "p = 0.250, t = -1.158", cex = 7, col = "#7CAE00")

FDIplot <- ggplot(data = FDI_data, aes(MeanForestCover1000m, FDis)) +
  geom_point(col = "#C77CFF") + geom_smooth(method = glm, se=F, col = "#C77CFF") +
  theme_classic() +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  scale_x_reverse(limits = c(100, 0)) +
  ggtitle("Invertivores") +
  labs(x = 'Mean Forest Cover at 1000m (%)', y = 'FDis') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=30, y=0.5, label= "p = 0.268, t = -1.112", cex = 7, col = "#C77CFF")

FDplot2 <- ggplot(data = FDO_data, aes(log(Trees+1), FDis)) +
  geom_point() + geom_smooth(method = glm, se = F, col = "black") +
  theme_classic() +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = '', y = 'FDis') +
  theme(text = element_text(size = 20)) +
  annotate("text", x=2.2, y=0.5, label= "p < 0.001*, t = -4.645", cex = 7)

FDGplot2 <- ggplot(data = FDG_data, aes(log(Trees+1), FDis)) +
  geom_point(col = "#7CAE00") + geom_smooth(method = glm, se=F, col = "#7CAE00") +
  theme_classic() +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = '', y = 'FDis') +
  theme(text = element_text(size = 20)) +
  annotate("text", x=2.2, y=0.5, label= "p = 0.896, t = -0.131", cex = 7, col = "#7CAE00")

FDIplot2 <- ggplot(data = FDI_data, aes(log(Trees+1), FDis)) +
  geom_point(col = "#C77CFF") + geom_smooth(method = glm, se=F, col = "#C77CFF") +
  theme_classic() +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  scale_x_reverse(limits = c(7, 0)) +
  labs(x = 'log(Trees)', y = 'FDis') +
  theme(text = element_text(size = 20)) +
  annotate("text", x=2.2, y=0.5, label= "p = 0.002*, t = -3.120", cex = 7, col = "#C77CFF")

grid.arrange(FDplot, FDIplot, FDGplot, FDplot2, FDIplot2, FDGplot2, ncol = 3, nrow = 2)


####################Figure 6#######################
###################################################
#Functional dispersion by habitat type and baseline

FD_H_plot2 <- ggplot() +
  geom_point(data = FD_HF_data, aes(MeanForestCover1000m, FDis, col = "#00BFC4")) +
  geom_point(data = FD_HNF_data, aes(MeanForestCover1000m, FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HF_data, aes(x = MeanForestCover1000m, y = FDis), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = FD_HNF_data, aes(x = MeanForestCover1000m, y = FDis), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() +
  scale_x_reverse(limits = c(100, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  ggtitle("All sites") +
  labs(x = '', y = 'Overall FDis') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=65, y=2.5, label= "  p = 0.012*, t = -2.537", cex = 7, col = "#F8766D") +
  annotate("text", x=65, y=2.2, label= " p = 0.131, t = -1.518", cex = 7, col = "#00BFC4")  

FD_HI_plot2 <- ggplot() +
  geom_point(data = FD_HIF_data, aes(MeanForestCover1000m, FDis, col = "#00BFC4")) +
  geom_point(data = FD_HINF_data, aes(MeanForestCover1000m, FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HIF_data, aes(x = MeanForestCover1000m, y = FDis), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = FD_HINF_data, aes(x = MeanForestCover1000m, y = FDis), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() +
  scale_x_reverse(limits = c(100, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  labs(x = '', y = 'Invertivore FDis') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=65, y=2.5, label= "p = 0.355, t = -0.928", cex = 7, col = "#F8766D") +
  annotate("text", x=65, y=2.2, label= "p = 0.458, t = -0.744", cex = 7, col = "#00BFC4")  


FD_HCM_plot <- ggplot() +
  geom_point(data = FD_HF_CM_data, aes(MeanForestCover1000m, FDis, col = "#00BFC4")) +
  geom_point(data = FD_HNF_CM_data, aes(MeanForestCover1000m, FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HF_CM_data, aes(x = MeanForestCover1000m, y = FDis), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = FD_HNF_CM_data, aes(x = MeanForestCover1000m, y = FDis), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() + 
  scale_x_reverse(limits = c(100, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  ggtitle("Shifted Baseline") +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=65, y=2.5, label= "p = 0.065, t = -1.866", cex = 7, col = "#F8766D") +
  annotate("text", x=65, y=2.2, label= "p = 0.391, t = -0.861", cex = 7, col = "#00BFC4")  

FD_HZM_plot <- ggplot() +
  geom_point(data = FD_HF_ZM_data, aes(MeanForestCover1000m, FDis, col = "#00BFC4")) +
  geom_point(data = FD_HNF_ZM_data, aes(MeanForestCover1000m, FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HF_ZM_data, aes(x = MeanForestCover1000m, y = FDis), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = FD_HNF_ZM_data, aes(x = MeanForestCover1000m, y = FDis), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() +
  scale_x_reverse(limits = c(100, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  ggtitle("Natural Baseline") +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=62, y=2.5, label= "p = 0.004*, t = -2.978 ", cex = 7, col = "#F8766D") +
  annotate("text", x=62, y=2.2, label= "p < 0.001*, t = -3.933", cex = 7, col = "#00BFC4")  


FD_HICM_plot <- ggplot() +
  geom_point(data = FD_HIF_CM_data, aes(MeanForestCover1000m, FDis, col = "#00BFC4")) +
  geom_point(data = FD_HINF_CM_data, aes(MeanForestCover1000m, FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HIF_CM_data, aes(x = MeanForestCover1000m, y = FDis), col = "#F8766D", method=glm, se=F) +
  geom_smooth(data = FD_HINF_CM_data, aes(x = MeanForestCover1000m, y = FDis), col = "#00BFC4", method=glm, se=F) +
  theme_classic() +
  scale_x_reverse(limits = c(100, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  labs(x = 'Mean Forest Cover (%)', y = '') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=65, y=2.5, label= "p = 0.474, t = -0.719", cex = 7, col = "#F8766D") +
  annotate("text", x=65, y=2.2, label= "p = 0.639, t = -0.471", cex = 7, col = "#00BFC4")  

FD_HIZM_plot <- ggplot() +
  geom_point(data = FD_HIF_ZM_data, aes(MeanForestCover1000m, FDis, col = "#00BFC4")) +
  geom_point(data = FD_HINF_ZM_data, aes(MeanForestCover1000m, FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HIF_ZM_data, aes(x = MeanForestCover1000m, y = FDis), col = "#F8766D", method=glm, se=F) +
  geom_smooth(data = FD_HINF_ZM_data, aes(x = MeanForestCover1000m, y = FDis), col = "#00BFC4", method=glm, se=F) +
  theme_classic() +
  scale_x_reverse(limits = c(100, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=62, y=2.5, label= "p = 0.004*, t = -2.985", cex = 7, col = "#F8766D") +
  annotate("text", x=62, y=2.2, label= "p = 0.016*, t = -2.506", cex = 7, col = "#00BFC4")  


combined <- FD_H_plot2 + FD_HCM_plot + FD_HZM_plot + FD_HI_plot2 + FD_HICM_plot + FD_HIZM_plot & theme(legend.position = "right")
combined + plot_layout(guides = "collect")



#########################
###Supplementary Plots###
#########################

###Fig S1###

p4 <- ggplot(data = Final_pca_data, aes(x = PC1, y = PC2, color = Trophic.Niche, shape = Trophic.Niche)) +
  geom_hline(yintercept = 0, lty = 2) +
  geom_vline(xintercept = 0, lty = 2) +
  guides(color = guide_legend(title = "Trophic Niche"), shape = guide_legend(title = "Trophic Niche")) +
  scale_shape_manual(values = c(15, 16, 17, 18, 19, 20, 21, 22, 23)) +
  geom_point(alpha = 0.8, size = 2) 

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


###Fig S2###

SupSR_H_plot2 <- ggplot(data = SR_H_Data, aes(MeanForestCover1000m, Richness, col = Habitat.Type), show.legend = FALSE) +
  geom_point() + geom_smooth(method = glm, se = F) +
  theme_classic() +
  scale_y_continuous(limits = c(0,50),breaks = seq(0, 50, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  labs(x = '', y = 'Overall SR') +
  ggtitle("All sites") +
  theme(text = element_text(size = 20), legend.position="none", plot.title = element_text(hjust = 0.5)) +
  annotate("text", x=65, y=50, label= "p = 0.532, z = 0.624", cex = 7, col = "#F8766D") +
  annotate("text", x=65, y=45, label= " p < 0.015*, z = -2.423", cex = 7, col = "#00BFC4")  

SupSR_HI_plot2 <- ggplot(data = SR_HI_Data, aes(MeanForestCover1000m, Richness, col = Habitat.Type), show.legend = FALSE) +
  geom_point() + geom_smooth(method = glm, se = F) +
  theme_classic() +
  scale_y_continuous(limits = c(0,40),breaks = seq(0, 40, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  labs(x = '', y = 'Invertivore SR') +
  theme(text = element_text(size = 20), legend.position="none") +
  annotate("text", x=65, y=40, label= "p = 0.065, z = 1.848", cex = 7, col = "#F8766D") +
  annotate("text", x=65, y=35, label= " p = 0.946, z = -0.067", cex = 7, col = "#00BFC4")


SupSR_HCM_plot <- ggplot() +
  geom_point(data = SR_HF_CM_data, aes(MeanForestCover1000m, Richness, col = "#00BFC4")) +
  geom_point(data = SR_HNF_CM_data, aes(MeanForestCover1000m, Richness, col = "#F8766D")) + 
  geom_smooth(data = SR_HF_CM_data, aes(x = MeanForestCover1000m, y = Richness), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = SR_HNF_CM_data, aes(x = MeanForestCover1000m, y = Richness), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() + 
  scale_y_continuous(limits = c(0,50),breaks = seq(0, 50, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  ggtitle("Shifted Baseline") +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=65, y=50, label= "p = 0.508, z = 0.662", cex = 7, col = "#F8766D") +
  annotate("text", x=65, y=45, label= " p = 0.010*, z = -2.571", cex = 7, col = "#00BFC4")  

SupSR_HZM_plot <- ggplot() +
  geom_point(data = SR_HF_ZM_data, aes(MeanForestCover1000m, Richness, col = "#00BFC4")) +
  geom_point(data = SR_HNF_ZM_data, aes(MeanForestCover1000m, Richness, col = "#F8766D")) + 
  geom_smooth(data = SR_HF_ZM_data, aes(x = MeanForestCover1000m, y = Richness), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = SR_HNF_ZM_data, aes(x = MeanForestCover1000m, y = Richness), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() +
  scale_y_continuous(limits = c(0,50),breaks = seq(0, 50, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  ggtitle("Natural Baseline") +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=62, y=50, label= "p = 0.440, z = -0.772 ", cex = 7, col = "#F8766D") +
  annotate("text", x=62, y=45, label= "p < 0.001*, z = -3.377", cex = 7, col = "#00BFC4")  


SupSR_HICM_plot <- ggplot() +
  geom_point(data = SR_HIF_CM_data, aes(MeanForestCover1000m, Richness, col = "#00BFC4")) +
  geom_point(data = SR_HINF_CM_data, aes(MeanForestCover1000m, Richness, col = "#F8766D")) + 
  geom_smooth(data = SR_HIF_CM_data, aes(x = MeanForestCover1000m, y = Richness), col = "#F8766D", method=glm, se=F) +
  geom_smooth(data = SR_HINF_CM_data, aes(x = MeanForestCover1000m, y = Richness), col = "#00BFC4", method=glm, se=F) +
  theme_classic() +
  scale_y_continuous(limits = c(0,40),breaks = seq(0, 40, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  labs(x = 'Mean Forest Cover (%)', y = '') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=65, y=40, label= "p = 0.102, z = 1.637", cex = 7, col = "#F8766D") +
  annotate("text", x=65, y=35, label= "p = 0.880, z = -0.151", cex = 7, col = "#00BFC4")  

SupSR_HIZM_plot <- ggplot() +
  geom_point(data = SR_HIF_ZM_data, aes(MeanForestCover1000m, Richness, col = "#00BFC4")) +
  geom_point(data = SR_HINF_ZM_data, aes(MeanForestCover1000m, Richness, col = "#F8766D")) + 
  geom_smooth(data = SR_HIF_ZM_data, aes(x = MeanForestCover1000m, y = Richness), col = "#F8766D", method=glm, se=F) +
  geom_smooth(data = SR_HINF_ZM_data, aes(x = MeanForestCover1000m, y = Richness), col = "#00BFC4", method=glm, se=F) +
  theme_classic() +
  scale_y_continuous(limits = c(0,40),breaks = seq(0, 40, 10)) +
  scale_x_reverse(limits = c(100, 0)) +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=62, y=40, label= "p = 0.292, z = -1.053", cex = 7, col = "#F8766D") +
  annotate("text", x=62, y=35, label= " p = 0.002*, z = -3.051", cex = 7, col = "#00BFC4")  


combined <- SupSR_H_plot2 + SupSR_HCM_plot + SupSR_HZM_plot + SupSR_HI_plot2 + SupSR_HICM_plot + SupSR_HIZM_plot & theme(legend.position = "right")
combined + plot_layout(guides = "collect")


###Fig S3###


SupFD_H_plot2 <- ggplot() +
  geom_point(data = FD_HF_data, aes(log(Trees+1), FDis, col = "#00BFC4")) +
  geom_point(data = FD_HNF_data, aes(log(Trees+1), FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HF_data, aes(x = log(Trees+1), y = FDis), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = FD_HNF_data, aes(x = log(Trees+1), y = FDis), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() +
  scale_x_reverse(limits = c(7, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  ggtitle("All sites") +
  labs(x = '', y = 'Overall FDis') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=2.5, label= "  p < 0.001*, t = -3.703", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=2.2, label= " p = 0.221, t = -1.228", cex = 7, col = "#00BFC4")  

SupFD_HI_plot2 <- ggplot() +
  geom_point(data = FD_HIF_data, aes(log(Trees+1), FDis, col = "#00BFC4")) +
  geom_point(data = FD_HINF_data, aes(log(Trees+1), FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HIF_data, aes(x = log(Trees+1), y = FDis), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = FD_HINF_data, aes(x = log(Trees+1), y = FDis), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() +
  scale_x_reverse(limits = c(7, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  labs(x = '', y = 'Invertivore FDis') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=2.5, label= "p < 0.001*, t = -3.265", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=2.2, label= "p = 0.543, t = -0.610", cex = 7, col = "#00BFC4")  


SupFD_HCM_plot <- ggplot() +
  geom_point(data = FD_HF_CM_data, aes(log(Trees+1), FDis, col = "#00BFC4")) +
  geom_point(data = FD_HNF_CM_data, aes(log(Trees+1), FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HF_CM_data, aes(x = log(Trees+1), y = FDis), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = FD_HNF_CM_data, aes(x = log(Trees+1), y = FDis), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() + 
  scale_x_reverse(limits = c(7, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  ggtitle("Shifted Baseline") +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=2.5, label= "p = 0.068, t = -1.850", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=2.2, label= "p = 0.149, t = -1.455", cex = 7, col = "#00BFC4")  

SupFD_HZM_plot <- ggplot() +
  geom_point(data = FD_HF_ZM_data, aes(log(Trees+1), FDis, col = "#00BFC4")) +
  geom_point(data = FD_HNF_ZM_data, aes(log(Trees+1), FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HF_ZM_data, aes(x = log(Trees+1), y = FDis), col = "#F8766D", method=glm, se=FALSE) +
  geom_smooth(data = FD_HNF_ZM_data, aes(x = log(Trees+1), y = FDis), col = "#00BFC4", method=glm, se=FALSE) +
  theme_classic() +
  scale_x_reverse(limits = c(7, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  ggtitle("Natural Baseline") +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20), plot.title = element_text(hjust = 0.5)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=2.5, label= "p = 0.006*, t = -2.843 ", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=2.2, label= "p = 0.567, t = -0.576", cex = 7, col = "#00BFC4")  


SupFD_HICM_plot <- ggplot() +
  geom_point(data = FD_HIF_CM_data, aes(log(Trees+1), FDis, col = "#00BFC4")) +
  geom_point(data = FD_HINF_CM_data, aes(log(Trees+1), FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HIF_CM_data, aes(x = log(Trees+1), y = FDis), col = "#F8766D", method=glm, se=F) +
  geom_smooth(data = FD_HINF_CM_data, aes(x = log(Trees+1), y = FDis), col = "#00BFC4", method=glm, se=F) +
  theme_classic() +
  scale_x_reverse(limits = c(7, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  labs(x = 'log(Trees)', y = '') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=2.5, label= "p = 0.431, t = -0.791", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=2.2, label= "p = 0.835, t = -0.209", cex = 7, col = "#00BFC4")  

SupFD_HIZM_plot <- ggplot() +
  geom_point(data = FD_HIF_ZM_data, aes(log(Trees+1), FDis, col = "#00BFC4")) +
  geom_point(data = FD_HINF_ZM_data, aes(log(Trees+1), FDis, col = "#F8766D")) + 
  geom_smooth(data = FD_HIF_ZM_data, aes(x = log(Trees+1), y = FDis), col = "#F8766D", method=glm, se=F) +
  geom_smooth(data = FD_HINF_ZM_data, aes(x = log(Trees+1), y = FDis), col = "#00BFC4", method=glm, se=F) +
  theme_classic() +
  scale_x_reverse(limits = c(7, 0)) +
  scale_y_continuous(limits = c(0,2.5),breaks = seq(0, 2.5, 0.5)) +
  labs(x = '', y = '') +
  theme(text = element_text(size = 20)) +
  scale_color_discrete(name = "Habitat", labels = c("Forest","Non Forest")) +
  annotate("text", x=4.2, y=2.5, label= "p = 0.002*, t = -3.144", cex = 7, col = "#F8766D") +
  annotate("text", x=4.2, y=2.2, label= "p = 0.802, t = 0.252", cex = 7, col = "#00BFC4")  


combined <- SupFD_H_plot2 + SupFD_HCM_plot + SupFD_HZM_plot + SupFD_HI_plot2 + SupFD_HICM_plot + SupFD_HIZM_plot & theme(legend.position = "right")
combined + plot_layout(guides = "collect")


