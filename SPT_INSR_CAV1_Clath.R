library(tidyverse)
library("readxl")
library(matrixStats)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set working directory to where this file is.

IR.0nM.002 <- as.data.frame(t(read.csv(file="Input/002_0nM_CAV1_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="002",coexp="CAV1", Insulin="0nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
head(IR.0nM.002)
IR.2nM.002 <- as.data.frame(t(read.csv(file="Input/002_2nM_CAV1_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="002",coexp="CAV1", Insulin="2nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
IR.2nM.005 <- as.data.frame(t(read.csv(file="Input/005_2nM_CAV1_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="005",coexp="CAV1", Insulin="2nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
IR.0nM.006 <- as.data.frame(t(read.csv(file="Input/006_0nM_Clath_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="006",coexp="Clathrin", Insulin="0nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
IR.0nM.007 <- as.data.frame(t(read.csv(file="Input/007_0nM_Clath_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="007",coexp="Clathrin", Insulin="0nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
IR.0nM.011 <- as.data.frame(t(read.csv(file="Input/011_0nM_Clath_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="011",coexp="Clathrin", Insulin="0nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
IR.2nM.011 <- as.data.frame(t(read.csv(file="Input/011_2nM_Clath_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="011",coexp="Clathrin", Insulin="2nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
IR.2nM.013 <- as.data.frame(t(read.csv(file="Input/013_2nM_Clath_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="013",coexp="Clathrin", Insulin="2nM")  %>% dplyr::rename("Diffusion_coeff"="V1")

IR.0nM.105 <- as.data.frame(t(read.csv(file="Input/105_0nM_CAV1_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="105",coexp="CAV1", Insulin="0nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
IR.0nM.102 <- as.data.frame(t(read.csv(file="Input/102_0nM_Clath_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="102",coexp="Clathrin", Insulin="0nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
#IR.2nM.112 <- as.data.frame(t(read.csv(file="Input/112_50nM_Clath_IRB_C0_dc.csv",header=F))) %>% 
#  mutate(cell="112",coexp="Clathrin", Insulin="2nM")  %>% dplyr::rename("Diffusion_coeff"="V1")

IR.2nM.002.i <- as.data.frame(t(read.csv(file="Input/002_2nM_CAV1_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="002.i",coexp="CAV1", Insulin="2nM")  %>% dplyr::rename("Diffusion_coeff"="V1")
IR.2nM.011.i <- as.data.frame(t(read.csv(file="Input/011_2nM_Clath_IRB_C0_dc.csv",header=F))) %>% 
  mutate(cell="011.i",coexp="Clathrin", Insulin="2nM")  %>% dplyr::rename("Diffusion_coeff"="V1")

IR.dc <- Reduce(rbind,list(IR.0nM.002,IR.2nM.002,IR.2nM.005,IR.0nM.006,IR.0nM.007,IR.0nM.011,IR.2nM.011,IR.2nM.013,
                           IR.0nM.105,IR.0nM.102))
IR.dc <- IR.dc %>% mutate(subgroup=paste(coexp,Insulin,sep = "_")) %>% filter(Diffusion_coeff>0.01) # removeed 1 artifact dot that doesn't move 
nrow(IR.dc)
IR.dc.i <- Reduce(rbind,list(IR.0nM.002,IR.2nM.002.i,IR.2nM.005,IR.0nM.006,IR.0nM.007,IR.0nM.011,IR.2nM.011.i,IR.2nM.013,
                             IR.0nM.105,IR.0nM.102))
IR.dc.i <- IR.dc.i %>% mutate(subgroup=paste(coexp,Insulin,sep = "_")) %>% filter(Diffusion_coeff>0.01) # removeed 1 artifact dot that doesn't move 

IR.dc$subgroup <- factor(IR.dc$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))
summary(IR.dc$subgroup)


install.packages("survival")
install.packages("lattice")
install.packages("ggplot2")
install.packages("Hmisc")
library("Hmisc")
ReplicateAverages <- IR.dc %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
ggplot(IR.dc, aes(x=subgroup,y=Diffusion_coeff,color=factor(cell))) + 
  labs(x=" ", y="Diffusion coefficient (µm2/s)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
 # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="dc_CCins_super.png",width=5.5,height=3,units="in",dpi=600)

#
ReplicateAverages <- IR.dc.i %>% 
  group_by(coexp, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
ggplot(IR.dc.i, aes(x=coexp,y=Diffusion_coeff,color=factor(cell))) + 
  labs(x=" ", y="Diffusion coefficient (µm2/s)")+
  
  geom_beeswarm(cex=1.2,alpha=0.5,groupOnX=TRUE,size=1) + 
  #geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=4,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=4,shape = 1,size = 3,colour = "black")+
  stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="dc_CC_super.png",width=3.5,height=3.5,units="in",dpi=600)

#---
ins.0nM <- IR.dc[c(IR.dc$Insulin=="0nM"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins.0nM <- ins.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.0nM))

ins.2nM <- IR.dc[c(IR.dc$Insulin=="2nM"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins.2nM <-  ins.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.2nM))

IR.ins <- rbind(ins.0nM,ins.2nM)

ggplot(IR.ins, aes(x=Diffusion_coeff, y=cum_frequency,color= Insulin)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Diffusion coefficient (µm2/s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-2),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="dc_0nM2nM.png",width=3,height=3,units="in",dpi=600)

ks.test(x=ins.2nM$Diffusion_coeff, y=ins.0nM$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.17585, p-value = 1.584e-08

#--
ins.CAV1 <- IR.dc[c(IR.dc$coexp=="CAV1"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins.CAV1 <- ins.CAV1 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1))

ins.Clath <- IR.dc[c(IR.dc$coexp=="Clathrin"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins.Clath <- ins.Clath %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath))

IR.CC <- rbind(ins.CAV1,ins.Clath)
ggplot(IR.CC, aes(x=Diffusion_coeff, y=cum_frequency,color= coexp)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Diffusion coefficient (µm2/s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-2),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="dc_Cav1Clath.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.CAV1$Diffusion_coeff, y=ins.Clath$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.29357, p-value < 2.2e-16 ,Two-sample Kolmogorov-Smirnov test

#--
ins.CAV1.0nM <- IR.dc[c(IR.dc$subgroup=="CAV1_0nM"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- IR.dc[c(IR.dc$subgroup=="CAV1_2nM"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- IR.dc[c(IR.dc$subgroup=="Clathrin_0nM"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))

ins.Clath.2nM <- IR.dc[c(IR.dc$subgroup=="Clathrin_2nM"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)

ggplot(IR.CCins, aes(x=Diffusion_coeff, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Diffusion coefficient (µm2/s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-2),10^1.5))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="dc_CCins.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.2nM$Diffusion_coeff, y=ins.CAV1.0nM$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.067945, p-value = 0.5432

ks.test(x=ins.Clath.2nM$Diffusion_coeff, y=ins.Clath.0nM$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.099446, p-value = 0.1952

ks.test(x=ins.CAV1.0nM$Diffusion_coeff, y=ins.Clath.0nM$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.28949, p-value = 2.706e-13

ks.test(x=ins.CAV1.2nM$Diffusion_coeff, y=ins.Clath.2nM$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) #D = 0.26716, p-value = 1.359e-06



#==============================================================================
# Lifetime and distance

IR.speed.0nM.002 <- as.data.frame(read_excel("Input/002_0nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002",coexp="CAV1", Insulin="0nM")   
view(IR.speed.0nM.002)
IR.speed.2nM.002 <- as.data.frame(read_excel("Input/002_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002",coexp="CAV1", Insulin="2nM")   
IR.speed.2nM.005 <- as.data.frame(read_excel("Input/005_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="005",coexp="CAV1", Insulin="2nM")   
IR.speed.0nM.006 <- as.data.frame(read_excel("Input/006_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="006",coexp="Clathrin", Insulin="0nM")   
IR.speed.0nM.007 <- as.data.frame(read_excel("Input/007_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="007",coexp="Clathrin", Insulin="0nM")   
IR.speed.0nM.011 <- as.data.frame(read_excel("Input/011_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011",coexp="Clathrin", Insulin="0nM")   
IR.speed.2nM.011 <- as.data.frame(read_excel("Input/011_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011",coexp="Clathrin", Insulin="2nM")   
IR.speed.2nM.013 <- as.data.frame(read_excel("Input/013_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="013",coexp="Clathrin", Insulin="2nM")   

IR.speed.0nM.105 <- as.data.frame(read_excel("Input/105_0nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="105",coexp="CAV1", Insulin="0nM")   
IR.speed.0nM.102 <- as.data.frame(read_excel("Input/102_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="102",coexp="Clathrin", Insulin="0nM")   

IR.speed.2nM.002.i <- as.data.frame(read_excel("Input/002_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002i",coexp="CAV1", Insulin="2nM")   
IR.speed.2nM.011.i <- as.data.frame(read_excel("Input/011_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011i",coexp="Clathrin", Insulin="2nM")   

IR.speed <- Reduce(rbind,list(IR.speed.0nM.002,IR.speed.2nM.002,IR.speed.2nM.005,IR.speed.0nM.006,IR.speed.0nM.007,IR.speed.0nM.011,IR.speed.2nM.011,IR.speed.2nM.013,
                           IR.speed.0nM.105,IR.speed.0nM.102))

IR.speed <- IR.speed %>% dplyr::rename(Track='Track #',Start_frames='Start (frames)', End_frames='End (frames)',Duration_frames='Duration (frames)',
                                Total_disp_px="Total disp. (px)",Net_disp_px="Net disp. (px)",Linearity="Linearity (%)",
                                Track_radius="Search radius (px)",Min_disp_px="Min. disp. (px)",Max_disp_px="Max. disp. (px)",
                                Avg_disp_px="Avg. disp. (px)" ) %>% 
  mutate(subgroup=paste(coexp,Insulin,sep = "_"),Lifetime=Duration_frames*2) %>% filter(Duration_frames>4) 
colnames(IR.speed)

nrow(IR.speed)
IR.speed.i <- Reduce(rbind,list(IR.speed.0nM.002,IR.speed.2nM.002.i,IR.speed.2nM.005,IR.speed.0nM.006,IR.speed.0nM.007,IR.speed.0nM.011,IR.speed.2nM.011.i,IR.speed.2nM.013,
                                IR.speed.0nM.105,IR.speed.0nM.102))
IR.speed.i <- IR.speed.i %>%dplyr::rename(Track='Track #',Start_frames='Start (frames)', End_frames='End (frames)',Duration_frames='Duration (frames)',
                                          Total_disp_px="Total disp. (px)",Net_disp_px="Net disp. (px)",Linearity="Linearity (%)",
                                          Track_radius="Search radius (px)",Min_disp_px="Min. disp. (px)",Max_disp_px="Max. disp. (px)",
                                          Avg_disp_px="Avg. disp. (px)" ) %>% 
  mutate(subgroup=paste(coexp,Insulin,sep = "_"),Lifetime=Duration_frames*2) %>% filter(Duration_frames>4)

IR.speed$subgroup <- factor(IR.speed$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))
summary(IR.speed$subgroup)

#

ReplicateAverages <- IR.speed %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)

# track radius
ggplot(IR.speed, aes(x=subgroup,y=Track_radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="radius_CCins_super.png",width=5.5,height=3,units="in",dpi=600)

#
ReplicateAverages <- IR.speed.i %>% 
  group_by(coexp, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
ggplot(IR.speed.i, aes(x=coexp,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  
  #geom_beeswarm(cex=1.2,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=4,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=4,shape = 1,size = 3,colour = "black")+
  stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="lifetime_CC_super.png",width=3.5,height=3.5,units="in",dpi=600)
#ggsave(filename="lifetime_CC_super.log.png",width=3.5,height=3.5,units="in",dpi=600)

# Track radius
ggplot(IR.speed.i, aes(x=coexp,y=Track_radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  
  geom_beeswarm(cex=1.2,alpha=0.5,groupOnX=TRUE,size=1) + 
  #geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=4,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=4,shape = 1,size = 3,colour = "black")+
  stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="radius_CC_super.png",width=3.5,height=3.5,units="in",dpi=600)


#---
ins.0nM <- IR.speed[c(IR.speed$Insulin=="0nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.0nM <- ins.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.0nM))

ins.2nM <- IR.speed[c(IR.speed$Insulin=="2nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.2nM <-  ins.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.2nM))

IR.ins <- rbind(ins.0nM,ins.2nM)

ggplot(IR.ins, aes(x=Track_radius, y=cum_frequency,color= Insulin)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="Track_radius_0nM2nM.png",width=3,height=3,units="in",dpi=600)

ks.test(x=ins.2nM$Track_radius, y=ins.0nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.14015, p-value = 4.175e-06

#--
ins.CAV1 <- IR.speed[c(IR.speed$coexp=="CAV1"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.CAV1 <- ins.CAV1 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1))

ins.Clath <- IR.speed[c(IR.speed$coexp=="Clathrin"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.Clath <- ins.Clath %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath))

IR.CC <- rbind(ins.CAV1,ins.Clath)
ggplot(IR.CC, aes(x=Track_radius, y=cum_frequency,color= coexp)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="Track_radius_Cav1Clath.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.CAV1$Track_radius, y=ins.Clath$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.29357, p-value < 2.2e-16 ,Two-sample Kolmogorov-Smirnov test

#--
ins.CAV1.0nM <- IR.speed[c(IR.speed$subgroup=="CAV1_0nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- IR.speed[c(IR.speed$subgroup=="CAV1_2nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- IR.speed[c(IR.speed$subgroup=="Clathrin_0nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))

ins.Clath.2nM <- IR.speed[c(IR.speed$subgroup=="Clathrin_2nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)

ggplot(IR.CCins, aes(x=Track_radius, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="Track_radius_CCins.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.2nM$Track_radius, y=ins.CAV1.0nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.093704, p-value = 0.09887

ks.test(x=ins.Clath.2nM$Track_radius, y=ins.Clath.0nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.12092, p-value = 0.05015

ks.test(x=ins.CAV1.0nM$Track_radius, y=ins.Clath.0nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.20658, p-value = 1.304e-09

ks.test(x=ins.CAV1.2nM$Track_radius, y=ins.Clath.2nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.23816, p-value = 1.744e-05

#-----
#lifetime
ReplicateAverages <- IR.speed %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
ggplot(IR.speed, aes(x=subgroup,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="lifetime_CCins_super.png",width=5.5,height=3,units="in",dpi=600)
#--
ins.CAV1 <- IR.speed[c(IR.speed$coexp=="CAV1"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1 <- ins.CAV1 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1))

ins.Clath <- IR.speed[c(IR.speed$coexp=="Clathrin"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath <- ins.Clath %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath))

IR.CC <- rbind(ins.CAV1,ins.Clath)
ggplot(IR.CC, aes(x=Lifetime, y=cum_frequency,color= coexp)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="Lifetime_Cav1Clath.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.CAV1$Lifetime, y=ins.Clath$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.29357, p-value < 2.2e-16 ,Two-sample Kolmogorov-Smirnov test

#--
ins.CAV1.0nM <- IR.speed[c(IR.speed$subgroup=="CAV1_0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- IR.speed[c(IR.speed$subgroup=="CAV1_2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- IR.speed[c(IR.speed$subgroup=="Clathrin_0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))

ins.Clath.2nM <- IR.speed[c(IR.speed$subgroup=="Clathrin_2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)

ggplot(IR.CCins, aes(x=Lifetime, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(1),10^2.5))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="Lifetime_CCins.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.2nM$Lifetime, y=ins.CAV1.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.0497, p-value = 0.7915

ks.test(x=ins.Clath.2nM$Lifetime, y=ins.Clath.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.14814, p-value = 0.00791

ks.test(x=ins.CAV1.0nM$Lifetime, y=ins.Clath.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.19074, p-value = 2.953e-08

ks.test(x=ins.CAV1.2nM$Lifetime, y=ins.Clath.2nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.080543, p-value = 0.518

#=========================================================
# paired tracks
colnames(pair.0nM.002)
pair.0nM.002 <- as.data.frame(read_excel("Input/CCpair/002_0nM_CAV1_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.0nM.002 <- as.data.frame(read_excel("Input/CCpair/002_0nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002",coexp="CAV1", Insulin="0nM") %>% filter(`Track #` %in% pair.0nM.002$Track) %>% mutate(Interact="Y")   
IR.speed.0nM.002N <- as.data.frame(read_excel("Input/CCpair/002_0nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002",coexp="CAV1", Insulin="0nM") %>% filter(!`Track #` %in% pair.0nM.002$Track) %>% mutate(Interact="N")      

pair.2nM.002 <- as.data.frame(read_excel("Input/CCpair/002_2nM_CAV1_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.2nM.002 <- as.data.frame(read_excel("Input/CCpair/002_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002",coexp="CAV1", Insulin="2nM") %>% filter(`Track #` %in% pair.2nM.002$Track) %>% mutate(Interact="Y")     
IR.speed.2nM.002N <- as.data.frame(read_excel("Input/CCpair/002_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002",coexp="CAV1", Insulin="2nM") %>% filter(!`Track #` %in% pair.2nM.002$Track) %>% mutate(Interact="N")    

pair.2nM.005 <- as.data.frame(read_excel("Input/CCpair/005_2nM_CAV1_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.2nM.005 <- as.data.frame(read_excel("Input/CCpair/005_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="005",coexp="CAV1", Insulin="2nM") %>% filter(`Track #` %in% pair.2nM.005$Track) %>% mutate(Interact="Y")   
IR.speed.2nM.005N <- as.data.frame(read_excel("Input/CCpair/005_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="005",coexp="CAV1", Insulin="2nM") %>% filter(!`Track #` %in% pair.2nM.005$Track) %>% mutate(Interact="N")   

pair.0nM.006 <- as.data.frame(read_excel("Input/CCpair/006_0nM_Clath_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.0nM.006 <- as.data.frame(read_excel("Input/CCpair/006_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="006",coexp="Clathrin", Insulin="0nM") %>% filter(`Track #` %in% pair.0nM.006$Track) %>% mutate(Interact="Y")     
IR.speed.0nM.006N <- as.data.frame(read_excel("Input/CCpair/006_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="006",coexp="Clathrin", Insulin="0nM") %>% filter(!`Track #` %in% pair.0nM.006$Track) %>% mutate(Interact="N") 

pair.0nM.007 <- as.data.frame(read_excel("Input/CCpair/007_0nM_Clath_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.0nM.007 <- as.data.frame(read_excel("Input/CCpair/007_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="007",coexp="Clathrin", Insulin="0nM") %>% filter(`Track #` %in% pair.0nM.007$Track) %>% mutate(Interact="Y")   
IR.speed.0nM.007N <- as.data.frame(read_excel("Input/CCpair/007_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="007",coexp="Clathrin", Insulin="0nM") %>% filter(!`Track #` %in% pair.0nM.007$Track) %>% mutate(Interact="N")  

pair.0nM.011 <- as.data.frame(read_excel("Input/CCpair/011_0nM_Clath_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.0nM.011 <- as.data.frame(read_excel("Input/CCpair/011_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011",coexp="Clathrin", Insulin="0nM") %>% filter(`Track #` %in% pair.0nM.011$Track) %>% mutate(Interact="Y")   
IR.speed.0nM.011N <- as.data.frame(read_excel("Input/CCpair/011_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011",coexp="Clathrin", Insulin="0nM") %>% filter(!`Track #` %in% pair.0nM.011$Track) %>% mutate(Interact="N")

pair.2nM.011 <- as.data.frame(read_excel("Input/CCpair/011_2nM_Clath_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.2nM.011 <- as.data.frame(read_excel("Input/CCpair/011_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011",coexp="Clathrin", Insulin="2nM") %>% filter(`Track #` %in% pair.2nM.011$Track) %>% mutate(Interact="Y")   
IR.speed.2nM.011N <- as.data.frame(read_excel("Input/CCpair/011_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011",coexp="Clathrin", Insulin="2nM") %>% filter(!`Track #` %in% pair.2nM.011$Track) %>% mutate(Interact="N")

pair.2nM.013 <- as.data.frame(read_excel("Input/CCpair/013_2nM_Clath_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.2nM.013 <- as.data.frame(read_excel("Input/CCpair/013_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="013",coexp="Clathrin", Insulin="2nM") %>% filter(`Track #` %in% pair.2nM.013$Track) %>% mutate(Interact="Y")   
IR.speed.2nM.013N <- as.data.frame(read_excel("Input/CCpair/013_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="013",coexp="Clathrin", Insulin="2nM") %>% filter(!`Track #` %in% pair.2nM.013$Track) %>% mutate(Interact="N")  

pair.0nM.105 <- as.data.frame(read_excel("Input/CCpair/105_0nM_CAV1_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.0nM.105 <- as.data.frame(read_excel("Input/CCpair/105_0nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="105",coexp="CAV1", Insulin="0nM") %>% filter(`Track #` %in% pair.0nM.105$Track) %>% mutate(Interact="Y")   
IR.speed.0nM.105N <- as.data.frame(read_excel("Input/CCpair/105_0nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="105",coexp="CAV1", Insulin="0nM") %>% filter(!`Track #` %in% pair.0nM.105$Track) %>% mutate(Interact="N")

pair.0nM.102 <- as.data.frame(read_excel("Input/CCpair/102_0nM_Clath_pair.xls")) %>% 
  dplyr::rename(Track="Track ID...2",Nb.frames="Nb. frames") %>% select(Track, Nb.frames) %>% 
  group_by(Track) %>% summarise_each(funs(sum)) %>% filter(Nb.frames>4) 
IR.speed.0nM.102 <- as.data.frame(read_excel("Input/CCpair/102_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="102",coexp="Clathrin", Insulin="0nM") %>% filter(`Track #` %in% pair.0nM.102$Track) %>% mutate(Interact="Y")   
IR.speed.0nM.102N <- as.data.frame(read_excel("Input/CCpair/102_0nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="102",coexp="Clathrin", Insulin="0nM") %>% filter(!`Track #` %in% pair.0nM.102$Track) %>% mutate(Interact="N")   

IR.speed.2nM.002.i <- as.data.frame(read_excel("Input/CCpair/002_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002i",coexp="CAV1", Insulin="2nM") %>% filter(`Track #` %in% pair.2nM.002$Track) %>% mutate(Interact="Y")   
IR.speed.2nM.002.iN <- as.data.frame(read_excel("Input/CCpair/002_2nM_CAV1_C0_Speed.xls")) %>% 
  mutate(cell="002i",coexp="CAV1", Insulin="2nM") %>% filter(!`Track #` %in% pair.2nM.002$Track) %>% mutate(Interact="N")   

IR.speed.2nM.011.i <- as.data.frame(read_excel("Input/CCpair/011_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011i",coexp="Clathrin", Insulin="2nM") %>% filter(`Track #` %in% pair.2nM.011$Track) %>% mutate(Interact="Y")   
IR.speed.2nM.011.iN <- as.data.frame(read_excel("Input/CCpair/011_2nM_Clath_C0_Speed.xls")) %>% 
  mutate(cell="011i",coexp="Clathrin", Insulin="2nM") %>% filter(!`Track #` %in% pair.2nM.011$Track) %>% mutate(Interact="N")   

IR.speed <- Reduce(rbind,list(IR.speed.0nM.002,IR.speed.2nM.002,IR.speed.2nM.005,IR.speed.0nM.006,IR.speed.0nM.007,IR.speed.0nM.011,IR.speed.2nM.011,IR.speed.2nM.013,
                              IR.speed.0nM.105,IR.speed.0nM.102))
IR.speed <- IR.speed %>% dplyr::rename(Track='Track #',Start_frames='Start (frames)', End_frames='End (frames)',Duration_frames='Duration (frames)',
                                       Total_disp_px="Total disp. (px)",Net_disp_px="Net disp. (px)",Linearity="Linearity (%)",
                                       Track_radius="Search radius (px)",Min_disp_px="Min. disp. (px)",Max_disp_px="Max. disp. (px)",
                                       Avg_disp_px="Avg. disp. (px)" ) %>% 
  mutate(subgroup=paste(coexp,Insulin,sep = "_"),Lifetime=Duration_frames*2) %>% filter(Duration_frames>4) 
colnames(IR.speed)

nrow(IR.speed)
IR.speed.i <- Reduce(rbind,list(IR.speed.0nM.002,IR.speed.2nM.002.i,IR.speed.2nM.005,IR.speed.0nM.006,IR.speed.0nM.007,IR.speed.0nM.011,IR.speed.2nM.011.i,IR.speed.2nM.013,
                                IR.speed.0nM.105,IR.speed.0nM.102))
IR.speed.i <- IR.speed.i %>%dplyr::rename(Track='Track #',Start_frames='Start (frames)', End_frames='End (frames)',Duration_frames='Duration (frames)',
                                          Total_disp_px="Total disp. (px)",Net_disp_px="Net disp. (px)",Linearity="Linearity (%)",
                                          Track_radius="Search radius (px)",Min_disp_px="Min. disp. (px)",Max_disp_px="Max. disp. (px)",
                                          Avg_disp_px="Avg. disp. (px)" ) %>% 
  mutate(subgroup=paste(coexp,Insulin,sep = "_"),Lifetime=Duration_frames*2) %>% filter(Duration_frames>4)


IR.speed$subgroup <- factor(IR.speed$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))
IR.speed.i$subgroup <- factor(IR.speed$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))
summary(IR.speed$subgroup)
#
IR.speed.pN <- Reduce(rbind,list(IR.speed.0nM.002,IR.speed.2nM.002,IR.speed.2nM.005,IR.speed.0nM.006,IR.speed.0nM.007,IR.speed.0nM.011,IR.speed.2nM.011,IR.speed.2nM.013,
                                 IR.speed.0nM.105,IR.speed.0nM.102, 
                                 IR.speed.0nM.002N,IR.speed.2nM.002N,IR.speed.2nM.005N,IR.speed.0nM.006N,IR.speed.0nM.007N,IR.speed.0nM.011N,IR.speed.2nM.011N,IR.speed.2nM.013N,
                                 IR.speed.0nM.105N,IR.speed.0nM.102N))
IR.speed.pN <- IR.speed.pN %>% dplyr::rename(Track='Track #',Start_frames='Start (frames)', End_frames='End (frames)',Duration_frames='Duration (frames)',
                                             Total_disp_px="Total disp. (px)",Net_disp_px="Net disp. (px)",Linearity="Linearity (%)",
                                             Track_radius="Search radius (px)",Min_disp_px="Min. disp. (px)",Max_disp_px="Max. disp. (px)",
                                             Avg_disp_px="Avg. disp. (px)" ) %>% 
  mutate(subgroup=paste(coexp,Insulin,sep = "_"),Lifetime=Duration_frames*2,coexp.pair=paste(coexp,Interact,sep = ".")) %>% filter(Duration_frames>4) 

IR.speed.iPN <- Reduce(rbind,list(IR.speed.0nM.002,IR.speed.2nM.002.i,IR.speed.2nM.005,IR.speed.0nM.006,IR.speed.0nM.007,IR.speed.0nM.011,IR.speed.2nM.011.i,IR.speed.2nM.013,
                                  IR.speed.0nM.105,IR.speed.0nM.102,
                                  IR.speed.0nM.002N,IR.speed.2nM.002.iN,IR.speed.2nM.005N,IR.speed.0nM.006N,IR.speed.0nM.007N,IR.speed.0nM.011N,IR.speed.2nM.011.iN,IR.speed.2nM.013N,
                                  IR.speed.0nM.105N,IR.speed.0nM.102N))
IR.speed.iPN <- IR.speed.iPN %>%dplyr::rename(Track='Track #',Start_frames='Start (frames)', End_frames='End (frames)',Duration_frames='Duration (frames)',
                                              Total_disp_px="Total disp. (px)",Net_disp_px="Net disp. (px)",Linearity="Linearity (%)",
                                              Track_radius="Search radius (px)",Min_disp_px="Min. disp. (px)",Max_disp_px="Max. disp. (px)",
                                              Avg_disp_px="Avg. disp. (px)" ) %>% 
  mutate(subgroup=paste(coexp,Insulin,sep = "_"),Lifetime=Duration_frames*2,coexp.pair=paste(coexp,Interact,sep = ".")) %>% filter(Duration_frames>4) 

IR.speed.pN$subgroup <- factor(IR.speed.pN$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))
IR.speed.iPN$subgroup <- factor(IR.speed.iPN$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))

IR.speed.pN$subgroup <- factor(IR.speed.pN$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))
IR.speed.iPN$subgroup <- factor(IR.speed.iPN$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))

IR.speed.pN$coexp.pair <- factor(IR.speed.pN$coexp.pair, levels=c("CAV1.Y","CAV1.N","Clathrin.Y", "Clathrin.N"))
IR.speed.iPN$coexp.pair <- factor(IR.speed.iPN$coexp.pair, levels=c("CAV1.Y","CAV1.N","Clathrin.Y", "Clathrin.N"))

summary(IR.speed$subgroup)

#

ReplicateAverages <- IR.speed %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)

# track radius
ggplot(IR.speed, aes(x=subgroup,y=Track_radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pair_radius_CCins_super.png",width=5.5,height=3,units="in",dpi=600)

#
ReplicateAverages <- IR.speed.i %>% 
  group_by(coexp, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)

ReplicateAverages <- IR.speed.iPN %>% 
  group_by(coexp.pair, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)

# Track radius

ReplicateAverages <- ReplicateAverages %>% separate(coexp.pair,c("coexp","Interact"),sep = "([.])") %>% mutate(coexp.pair=paste(coexp,Interact,sep = "."))
library(rstatix)
aov <- ReplicateAverages %>% anova_test(Track_radius ~ coexp + Interact)
aov
stat<- ReplicateAverages %>% pairwise_t_test(Track_radius ~ coexp.pair, p.adjust.method = "BH") %>%
  add_significance()
stat
stat<- stat %>% add_xy_position(x = "coexp.pair")

ggplot(IR.speed.iPN, aes(x=coexp.pair,y=Track_radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  
  geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  #geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=3,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
    # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +

  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYN_radius_CC_super.png",width=5.5,height=3.5,units="in",dpi=600)

#---
ins.0nM <- IR.speed[c(IR.speed$Insulin=="0nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.0nM <- ins.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.0nM))

ins.2nM <- IR.speed[c(IR.speed$Insulin=="2nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.2nM <-  ins.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.2nM))

IR.ins <- rbind(ins.0nM,ins.2nM)

ggplot(IR.ins, aes(x=Track_radius, y=cum_frequency,color= Insulin)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="Track_radius_0nM2nM.png",width=3,height=3,units="in",dpi=600)

ks.test(x=ins.2nM$Track_radius, y=ins.0nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.14015, p-value = 4.175e-06

#--
ins.CAV1 <- IR.speed[c(IR.speed$coexp=="CAV1"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.CAV1 <- ins.CAV1 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1))

ins.Clath <- IR.speed[c(IR.speed$coexp=="Clathrin"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.Clath <- ins.Clath %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath))

IR.CC <- rbind(ins.CAV1,ins.Clath)
ggplot(IR.CC, aes(x=Track_radius, y=cum_frequency,color= coexp)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="pair_Track_radius_Cav1Clath.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.CAV1$Track_radius, y=ins.Clath$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.27465, p-value < 2.2e-16

#--
ins.CAV1.0nM <- IR.speed[c(IR.speed$subgroup=="CAV1_0nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- IR.speed[c(IR.speed$subgroup=="CAV1_2nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- IR.speed[c(IR.speed$subgroup=="Clathrin_0nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))

ins.Clath.2nM <- IR.speed[c(IR.speed$subgroup=="Clathrin_2nM"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)

ggplot(IR.CCins, aes(x=Track_radius, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="pair_Track_radius_CCins.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.2nM$Track_radius, y=ins.CAV1.0nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.10556, p-value = 0.1285

ks.test(x=ins.Clath.2nM$Track_radius, y=ins.Clath.0nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.19847, p-value = 0.01523

ks.test(x=ins.CAV1.0nM$Track_radius, y=ins.Clath.0nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.2586, p-value = 6.199e-10

ks.test(x=ins.CAV1.2nM$Track_radius, y=ins.Clath.2nM$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.28419, p-value = 0.0002746
#=============
# plot CAV1 or Clath colocalized separately
head(IR.speed.iPN) 
IR.speed.iPN.cav <- IR.speed.iPN %>% filter(coexp=="CAV1")
ReplicateAverages.cav <- IR.speed.iPN.cav %>% 
  group_by(coexp.pair, cell) %>%  
  summarise_each(funs(mean))
IR.speed.iPN.clath <- IR.speed.iPN %>% filter(coexp=="Clathrin")
ReplicateAverages.clath <- IR.speed.iPN.clath %>% 
  group_by(coexp.pair, cell) %>%  
  summarise_each(funs(mean))

ggplot(IR.speed.iPN.cav, aes(x=coexp.pair,y=Track_radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  
  geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  #geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,shape = 1,size = 3,colour = "black")+
  stat_compare_means(data=ReplicateAverages.cav, comparisons = list(c("CAV1.Y", "CAV1.N")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYN_radius_CAV1_super.png",width=3.5,height=3.5,units="in",dpi=600)
#
ggplot(IR.speed.iPN.clath, aes(x=coexp.pair,y=Track_radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  
  geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  #geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,shape = 1,size = 3,colour = "black")+
  stat_compare_means(data=ReplicateAverages.clath, comparisons = list(c("Clathrin.Y", "Clathrin.N")), method="t.test", paired=F) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYN_radius_Clath_super.png",width=3.5,height=3.5,units="in",dpi=600)

#-- CAV1 paired
ins.CAV1.Y <- IR.speed.iPN.cav[c(IR.speed.iPN.cav$Interact=="Y"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.CAV1.Y <- ins.CAV1.Y %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.Y))

ins.CAV1.N <- IR.speed.iPN.cav[c(IR.speed.iPN.cav$Interact=="N"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.CAV1.N <- ins.CAV1.N %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.N))

ins.Clath.Y <- IR.speed.iPN.clath[c(IR.speed.iPN.clath$Interact=="Y"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.Clath.Y <- ins.Clath.Y %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.Y))

ins.Clath.N <- IR.speed.iPN.clath[c(IR.speed.iPN.clath$Interact=="N"),] %>%
  arrange(Track_radius)  %>% mutate(freq=1)
ins.Clath.N <- ins.Clath.N %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.N))

IR.cav <- rbind(ins.CAV1.Y,ins.CAV1.N)
ggplot(IR.cav, aes(x=Track_radius, y=cum_frequency,color= coexp.pair)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="pairYN_Track_radius_CAV1.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.CAV1.Y$Track_radius, y=ins.CAV1.N$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.23081, p-value = 7.168e-07

IR.clath <- rbind(ins.Clath.Y,ins.Clath.N)
ggplot(IR.clath, aes(x=Track_radius, y=cum_frequency,color= coexp.pair)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-0.5),10^1.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="pairYN_Track_radius_Clath.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.Clath.Y$Track_radius, y=ins.Clath.N$Track_radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.1496, p-value = 1.209e-05
#-----=============================
#lifetime
ReplicateAverages <- IR.speed %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
ReplicateAverages <- ReplicateAverages %>% separate(coexp.pair,c("coexp","Interact"),sep = "([.])") %>% mutate(coexp.pair=paste(coexp,Interact,sep = "."))
library(rstatix)
aov <- ReplicateAverages %>% anova_test(Lifetime ~ coexp + Interact)
aov
stat<- ReplicateAverages %>% pairwise_t_test(Lifetime ~ coexp.pair, p.adjust.method = "BH") %>%
  add_significance()
stat
stat<- stat %>% add_xy_position(x = "coexp.pair")

ggplot(IR.speed.iPN, aes(x=coexp.pair,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=3,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  # annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYN_liftime_CC_super.png",width=5.5,height=3.5,units="in",dpi=600)


# plot CAV1 or Clath colocalized separately
head(IR.speed.iPN) 
IR.speed.iPN.cav <- IR.speed.iPN %>% filter(coexp=="CAV1")
ReplicateAverages.cav <- IR.speed.iPN.cav %>% 
  group_by(coexp.pair, cell) %>%  
  summarise_each(funs(mean))
IR.speed.iPN.clath <- IR.speed.iPN %>% filter(coexp=="Clathrin")
ReplicateAverages.clath <- IR.speed.iPN.clath %>% 
  group_by(coexp.pair, cell) %>%  
  summarise_each(funs(mean))

ggplot(IR.speed.iPN.cav, aes(x=coexp.pair,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime(s)")+
  
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,shape = 1,size = 3,colour = "black")+
  stat_compare_means(data=ReplicateAverages.cav, comparisons = list(c("CAV1.Y", "CAV1.N")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYN_lifetime_CAV1_super.png",width=3.5,height=3.5,units="in",dpi=600)
#
ggplot(IR.speed.iPN.clath, aes(x=coexp.pair,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,shape = 1,size = 3,colour = "black")+
  stat_compare_means(data=ReplicateAverages.clath, comparisons = list(c("Clathrin.Y", "Clathrin.N")), method="t.test", paired=F) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYN_lifetime_Clath_super.png",width=3.5,height=3.5,units="in",dpi=600)

#-- CAV1 or clathrin paired
ins.CAV1.Y <- IR.speed.iPN.cav[c(IR.speed.iPN.cav$Interact=="Y"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.Y <- ins.CAV1.Y %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.Y))

ins.CAV1.N <- IR.speed.iPN.cav[c(IR.speed.iPN.cav$Interact=="N"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.N <- ins.CAV1.N %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.N))

ins.Clath.Y <- IR.speed.iPN.clath[c(IR.speed.iPN.clath$Interact=="Y"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.Y <- ins.Clath.Y %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.Y))

ins.Clath.N <- IR.speed.iPN.clath[c(IR.speed.iPN.clath$Interact=="N"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.N <- ins.Clath.N %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.N))

IR.cav <- rbind(ins.CAV1.Y,ins.CAV1.N)
ggplot(IR.cav, aes(x=Lifetime, y=cum_frequency,color= coexp.pair)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(1),10^2.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="pairYN_liftime_CAV1.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.CAV1.Y$Lifetime, y=ins.CAV1.N$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.39701, p-value < 2.2e-16

IR.clath <- rbind(ins.Clath.Y,ins.Clath.N)
ggplot(IR.clath, aes(x=Lifetime, y=cum_frequency,color= coexp.pair)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(1),10^2.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="pairYN_Lifetime_Clath.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.Clath.Y$Lifetime, y=ins.Clath.N$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.36022, p-value < 2.2e-16
#
ggplot(IR.speed, aes(x=subgroup,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pair_lifetime_CCins_super.png",width=5.5,height=3,units="in",dpi=600)

#--
ReplicateAverages <- IR.speed.i %>% 
  group_by(coexp, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)

ggplot(IR.speed.i, aes(x=coexp,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  
  #geom_beeswarm(cex=1.2,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=4,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=4,shape = 1,size = 3,colour = "black")+
  stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pair_lifetime_CC_super.png",width=3.5,height=3.5,units="in",dpi=600)
#ggsave(filename="lifetime_CC_super.log.png",width=3.5,height=3.5,units="in",dpi=600)

#


#--
ins.CAV1 <- IR.speed[c(IR.speed$coexp=="CAV1"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1 <- ins.CAV1 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1))

ins.Clath <- IR.speed[c(IR.speed$coexp=="Clathrin"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath <- ins.Clath %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath))

IR.CC <- rbind(ins.CAV1,ins.Clath)
ggplot(IR.CC, aes(x=Lifetime, y=cum_frequency,color= coexp)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(1),10^2.5))+
  theme(legend.position="top",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="pair_Lifetime_Cav1Clath.png",width=3,height=3,units="in",dpi=600)


ks.test(x=ins.CAV1$Lifetime, y=ins.Clath$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.17121, p-value = 7.711e-07

#--
ins.CAV1.0nM <- IR.speed[c(IR.speed$subgroup=="CAV1_0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- IR.speed[c(IR.speed$subgroup=="CAV1_2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- IR.speed[c(IR.speed$subgroup=="Clathrin_0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))

ins.Clath.2nM <- IR.speed[c(IR.speed$subgroup=="Clathrin_2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)

ggplot(IR.CCins, aes(x=Lifetime, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(1),10^2.5))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="Lifetime_CCins.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.2nM$Lifetime, y=ins.CAV1.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.048718, p-value = 0.9319

ks.test(x=ins.Clath.2nM$Lifetime, y=ins.Clath.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.26119, p-value = 0.0004289

ks.test(x=ins.CAV1.0nM$Lifetime, y=ins.Clath.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.22217, p-value = 1.916e-07

ks.test(x=ins.CAV1.2nM$Lifetime, y=ins.Clath.2nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.15278, p-value = 0.153
