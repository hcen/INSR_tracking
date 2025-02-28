library(readxl)
library(tidyverse)
library("ggbeeswarm")
library(matrixStats)
library("ggpubr")
library(scales)

domain <- read_excel("Input/CCpair/002_0nM_CAV1_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/002_0nM_CAV1_pair.xls") 
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/002_2nM_CAV1_C0_Speed.xls") %>% mutate(Group="002_2nM_CAV1_C0")
pair <- read_excel("Input/CCpair/002_2nM_CAV1_pair.xls") 
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/005_2nM_CAV1_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/005_2nM_CAV1_pair.xls") 
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/006_0nM_Clath_C0_Speed.xls") %>% mutate(Group="006_0nM_Clath_C0")
pair <- read_excel("Input/CCpair/006_0nM_Clath_pair.xls") 
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/007_0nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/007_0nM_Clath_pair.xls") 
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/011_0nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/011_0nM_Clath_pair.xls") 
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/011_2nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/011_2nM_Clath_pair.xls")
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/013_2nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/013_2nM_Clath_pair.xls")
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/105_0nM_CAV1_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/105_0nM_CAV1_pair.xls")
paired_domains(domain,pair) # function below

domain <- read_excel("Input/CCpair/102_0nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/102_0nM_Clath_pair.xls") 
paired_domains(domain,pair) # function below

#======
# plot track interaction heatmap
install.packages("pheatmap")
library(pheatmap)

p.002.0 <- read.csv(file="data/paired_002_0nM_CAV1_C0.csv", row.names = 1)
p.002.2 <- read.csv(file="data/paired_002_2nM_CAV1_C0.csv", row.names = 1)
p.005.2 <- read.csv(file="data/paired_005_2nM_CAV1_C0.csv", row.names = 1)
p.006.0 <- read.csv(file="data/paired_006_0nM_Clath_C0.csv", row.names = 1)
p.007.0 <- read.csv(file="data/paired_007_0nM_Clath_C0.csv", row.names = 1)
p.011.0 <- read.csv(file="data/paired_011_0nM_Clath_C0.csv", row.names = 1)
p.011.2 <- read.csv(file="data/paired_011_2nM_Clath_C0.csv", row.names = 1)
p.013.2 <- read.csv(file="data/paired_013_2nM_Clath_C0.csv", row.names = 1)
p.102.0 <- read.csv(file="data/paired_102_0nM_Clath_C0.csv", row.names = 1)
p.105.0 <- read.csv(file="data/paired_105_0nM_CAV1_C0.csv", row.names = 1)

cav.paired <- rbind(p.002.0,p.105.0,p.002.2,p.005.2)
cav.paired$n0 <- apply(cav.paired, 1, function(x) length(which(x==0)))
cav.paired$n1 <- apply(cav.paired, 1, function(x) length(which(x==1)))
cav.paired$n2 <- apply(cav.paired, 1, function(x) length(which(x==2)))
cav.paired <- cav.paired[order(cav.paired$n0,-cav.paired$n2),]

cav.paired.0 <- rbind(p.002.0,p.105.0)
cav.paired.2 <- rbind(p.002.2,p.005.2)
cav.paired.0$n0 <- apply(cav.paired.0, 1, function(x) length(which(x==0)))
cav.paired.0$n2 <- apply(cav.paired.0, 1, function(x) length(which(x==2)))
cav.paired.2$n0 <- apply(cav.paired.2, 1, function(x) length(which(x==0)))
cav.paired.2$n2 <- apply(cav.paired.2, 1, function(x) length(which(x==2)))
cav.paired.0 <- cav.paired.0[order(cav.paired.0$n0,-cav.paired.0$n2),]
cav.paired.2 <- cav.paired.2[order(cav.paired.2$n0,-cav.paired.2$n2),]

clath.paired <- rbind(p.006.0,p.007.0,p.011.0,p.102.0,p.011.2,p.013.2)
clath.paired$n0 <- apply(clath.paired, 1, function(x) length(which(x==0)))
clath.paired$n1 <- apply(clath.paired, 1, function(x) length(which(x==1)))
clath.paired$n2 <- apply(clath.paired, 1, function(x) length(which(x==2)))
clath.paired <- clath.paired[order(clath.paired$n0,-clath.paired$n2),]

clath.paired.0 <- rbind(p.006.0,p.007.0,p.011.0,p.102.0)
clath.paired.2 <- rbind(p.011.2,p.013.2)
clath.paired.0$n0 <- apply(clath.paired.0, 1, function(x) length(which(x==0)))
clath.paired.0$n2 <- apply(clath.paired.0, 1, function(x) length(which(x==2)))
clath.paired.2$n0 <- apply(clath.paired.2, 1, function(x) length(which(x==0)))
clath.paired.2$n2 <- apply(clath.paired.2, 1, function(x) length(which(x==2)))
clath.paired.0 <- clath.paired.0[order(clath.paired.0$n0,-clath.paired.0$n2),]
clath.paired.2 <- clath.paired.2[order(clath.paired.2$n0,-clath.paired.2$n2),]

df_num = as.matrix(cav.paired[,c(1:101)])

df_num = as.matrix(cav.paired.0[,c(1:101)])
df_num = as.matrix(cav.paired.2[,c(1:101)])
#pdf("pair_cav1.pdf",width = 4,height = 7)
pheatmap(df_num, #main="Interaction with CAV1",
         scale = "none",
         cluster_cols = F,
         cluster_rows = F,
         show_rownames = F,
         show_colnames = F,
         legend = F,
         color = colorRampPalette(c("white", "lightblue2", "coral2"))(50),
         cellheight = 1 ,filename = "cav1_pair2.pdf",width = 4,height = 7
         #cellwidth = 1
         #clustering_method = "ward.D2", #"ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
         #clustering_distance_rows= #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
         )
dev.off()
?colorRampPalette
?pdf
df_num = as.matrix(clath.paired[,c(1:101)])
df_num = as.matrix(clath.paired.0[,c(1:101)])
df_num = as.matrix(clath.paired.2[,c(1:101)])
#pdf("pair_clath.pdf",width = 4,height = 7)
pheatmap(df_num, #main="Interaction with Clathrin",
         scale = "none",
         cluster_cols = F,
         cluster_rows = F,
         show_rownames = F,
         show_colnames = F,
         legend = F,
         color = colorRampPalette(c("white", "lightblue2", "coral2"))(50),
         cellheight = 1, filename = "clath_pair2.pdf",width = 4,height = 7
         #cellwidth = 1
         #clustering_method = "ward.D2", #"ward.D", "ward.D2", "single", "complete", "average" (= UPGMA), "mcquitty" (= WPGMA), "median" (= WPGMC) or "centroid" (= UPGMC).
         #clustering_distance_rows= #"euclidean", "maximum", "manhattan", "canberra", "binary" or "minkowski"
         )
dev.off()
#=============================================
# fuction
paired_domains <- function(domain,pair){
domain <- domain %>% select(1:5,11) %>% 
    dplyr::rename(Track=`Track #`,start_frame=`Start (frames)`,end_frame=`End (frames)`,duration=`Duration (frames)`,max.disp="Max. disp. (px)") %>%
  filter(max.disp<8 & 
           duration>3)

pair <- pair %>% select(2,4,5,6) %>% 
  dplyr::rename(Track=`Track ID...2`,TrackB=`Track ID...4`,start_frame=`Start frame`,nb_frames=`Nb. frames`) %>% 
  mutate(end_frame=start_frame+nb_frames-1) %>% filter(Track %in% domain$Track)
#pair1 <- pair %>% group_by(Track) %>% summarise_each(funs(sum)) %>% filter(nb_frames>4) # select tracks with total interaction >4
pair2 <- pair %>% filter(nb_frames>3) # only select tracks with continous interaction >3 frames

domain <- domain %>% filter(Track %in% pair2$Track) 
pair <- pair %>% filter(Track %in% domain$Track)

# right join on pair, which eliminates any rows in pair that does not have Track_ID
merged <- right_join(domain, pair, by = 'Track') %>% 
  dplyr::rename(domian_start_frame = start_frame.x) %>%
  dplyr::rename(domian_end_frame = end_frame.x) %>%
  dplyr::rename(pair_start_frame = start_frame.y) %>%
  dplyr::rename(pair_end_frame = end_frame.y)
# build empty dataframe
df <- data.frame(matrix(0,
                  nrow = length(unique(merged$Track)),
                  ncol = 101,
                  dimnames = list(NULL, paste0("Frame_", 0:100))) ) %>% 
  mutate(TrackerID=unique(merged$Track))

# populate dataframe
for (i in 1:nrow(merged)) {
  pair_frames = c(merged$pair_start_frame[i]:merged$pair_end_frame[i])
  domain_frames = c(merged$domian_start_frame[i]:merged$domian_end_frame[i])
  # get the 1s
  for (frame in setdiff(domain_frames, pair_frames)) {
    if (df[match(merged$Track[i],df$TrackerID),frame+1] == 0) {
      df[match(merged$Track[i],df$TrackerID),frame+1] = 1
    }
  }
  # get the 2s
  for (frame in pair_frames) {
    df[match(merged$Track[i],df$TrackerID),frame+1] = 2
  }

}
rownames(df) = df$TrackerID
df$TrackerID = NULL

# shift to the left
for (i in 1:nrow(df)) {
  data = df[i,]
  
  left_index = 1
  right_index = length(data)
  while (left_index < right_index) {
    if ((data[right_index] != 0)&&(data[left_index] != 0)) {
      break
    } else {
      if (data[left_index] == 0) {
        left_index = left_index + 1
      }
      if (data[right_index] == 0) {
        right_index = right_index - 1
      }
    }
  }
  new = append(data[left_index:right_index],rep(0, ((length(data)-(right_index-left_index+1))  )))
  df[i,] = new
}
write.csv(df, file=paste0("data/paired_", merged[1,1], ".csv"))
}

#===========================================================================================
# plot cav1 or clathrin interaction

domain <- read_excel("Input/CCpair/002_0nM_CAV1_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/002_0nM_CAV1_pair.xls") 
filter_paired(domain,pair) # function below

IR.speed.0nM.002 <- domain.y %>% mutate(cell="002",coexp="CAV1", Insulin="0nM") %>% mutate(Interact="Y")
IR.speed.0nM.002N <- domain.n %>% mutate(cell="002",coexp="CAV1", Insulin="0nM") %>% mutate(Interact="N")

Y.0.002 <- IR.speed.0nM.002 %>% rename(Track_ID=Track)
write.csv(Y.0.002,file = "filterTracks/Y-0-002.csv")

N.0.002 <- IR.speed.0nM.002N %>% rename(Track_ID=Track)
write.csv(N.0.002,file = "filterTracks/N-0-002.csv")

domain <- read_excel("Input/CCpair/002_2nM_CAV1_C0_Speed.xls") %>% mutate(Group="002_2nM_CAV1_C0")
pair <- read_excel("Input/CCpair/002_2nM_CAV1_pair.xls") 
filter_paired(domain,pair)

IR.speed.2nM.002 <- domain.y %>% mutate(cell="002",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="Y")     
IR.speed.2nM.002N <- domain.n %>% mutate(cell="002",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="N")  

Y.2.002 <- IR.speed.2nM.002 %>% rename(Track_ID=Track)
write.csv(Y.2.002,file = "filterTracks/Y-2-002.csv")

N.2.002 <- IR.speed.2nM.002N %>% rename(Track_ID=Track)
write.csv(N.2.002,file = "filterTracks/N-2-002.csv")

IR.speed.2nM.002.i <- domain.y %>% mutate(cell="002i",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="Y")   
IR.speed.2nM.002.iN <- domain.n %>% mutate(cell="002i",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="N")  

domain <- read_excel("Input/CCpair/005_2nM_CAV1_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/005_2nM_CAV1_pair.xls") 
filter_paired(domain,pair)

IR.speed.2nM.005 <- domain.y %>% mutate(cell="005",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="Y")   
IR.speed.2nM.005N <- domain.n %>% mutate(cell="005",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="N")   

Y.2.005 <- IR.speed.2nM.005 %>% rename(Track_ID=Track)
write.csv(Y.2.005,file = "filterTracks/Y-2-005.csv")

N.2.005 <- IR.speed.2nM.005N %>% rename(Track_ID=Track)
write.csv(N.2.005,file = "filterTracks/N-2-005.csv")

domain <- read_excel("Input/CCpair/006_0nM_Clath_C0_Speed.xls") %>% mutate(Group="006_0nM_Clath_C0")
pair <- read_excel("Input/CCpair/006_0nM_Clath_pair.xls") 
filter_paired(domain,pair)

IR.speed.0nM.006 <- domain.y %>%  mutate(cell="006",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="Y")     
IR.speed.0nM.006N <- domain.n %>% mutate(cell="006",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="N") 

Y.0.006 <- IR.speed.0nM.006 %>% rename(Track_ID=Track)
write.csv(Y.0.006,file = "filterTracks/Y-0-006.csv")

N.0.006 <- IR.speed.0nM.006N %>% rename(Track_ID=Track)
write.csv(N.0.006,file = "filterTracks/N-0-006.csv")

domain <- read_excel("Input/CCpair/007_0nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/007_0nM_Clath_pair.xls") 
filter_paired(domain,pair)

IR.speed.0nM.007 <- domain.y %>% mutate(cell="007",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="Y")   
IR.speed.0nM.007N <- domain.n %>% mutate(cell="007",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="N")  

Y.0.007 <- IR.speed.0nM.007 %>% rename(Track_ID=Track)
write.csv(Y.0.007,file = "filterTracks/Y-0-007.csv")

N.0.007 <- IR.speed.0nM.007N %>% rename(Track_ID=Track)
write.csv(N.0.007,file = "filterTracks/N-0-007.csv")


domain <- read_excel("Input/CCpair/011_0nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/011_0nM_Clath_pair.xls") 
filter_paired(domain,pair)

IR.speed.0nM.011 <- domain.y %>% mutate(cell="011",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="Y")   
IR.speed.0nM.011N <- domain.n %>% mutate(cell="011",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="N")

Y.0.011 <- IR.speed.0nM.011 %>% rename(Track_ID=Track)
write.csv(Y.0.011,file = "filterTracks/Y-0-011.csv")

N.0.011 <- IR.speed.0nM.011N %>% rename(Track_ID=Track)
write.csv(N.0.011,file = "filterTracks/N-0-011.csv")

domain <- read_excel("Input/CCpair/011_2nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/011_2nM_Clath_pair.xls")
filter_paired(domain,pair)

IR.speed.2nM.011 <- domain.y %>% mutate(cell="011",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="Y")   
IR.speed.2nM.011N <- domain.n %>% mutate(cell="011",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="N")

Y.2.011 <- IR.speed.2nM.011 %>% rename(Track_ID=Track)
write.csv(Y.2.011,file = "filterTracks/Y-2-011.csv")

N.2.011 <- IR.speed.2nM.011N %>% rename(Track_ID=Track)
write.csv(N.2.011,file = "filterTracks/N-2-011.csv")

IR.speed.2nM.011.i <- domain.y %>% mutate(cell="011i",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="Y")   
IR.speed.2nM.011.iN <- domain.n %>% mutate(cell="011i",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="N") 

domain <- read_excel("Input/CCpair/013_2nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/013_2nM_Clath_pair.xls")
filter_paired(domain,pair)

IR.speed.2nM.013 <- domain.y %>% mutate(cell="013",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="Y")   
IR.speed.2nM.013N <- domain.n %>% mutate(cell="013",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="N")  

Y.2.013 <- IR.speed.2nM.013 %>% rename(Track_ID=Track)
write.csv(Y.2.013,file = "filterTracks/Y-2-013.csv")

N.2.013 <- IR.speed.2nM.013N %>% rename(Track_ID=Track)
write.csv(N.2.013,file = "filterTracks/N-2-013.csv")

domain <- read_excel("Input/CCpair/105_0nM_CAV1_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/105_0nM_CAV1_pair.xls")
filter_paired(domain,pair)

IR.speed.0nM.105 <- domain.y %>% mutate(cell="105",coexp="CAV1", Insulin="0nM") %>% mutate(Interact="Y")   
IR.speed.0nM.105N <- domain.n %>% mutate(cell="105",coexp="CAV1", Insulin="0nM") %>% mutate(Interact="N")

Y.0.105 <- IR.speed.0nM.105 %>% rename(Track_ID=Track)
write.csv(Y.0.105,file = "filterTracks/Y-0-105.csv")

N.0.105 <- IR.speed.0nM.105N %>% rename(Track_ID=Track)
write.csv(N.0.105,file = "filterTracks/N-0-105.csv")

domain <- read_excel("Input/CCpair/102_0nM_Clath_C0_Speed.xls") 
pair <- read_excel("Input/CCpair/102_0nM_Clath_pair.xls")
filter_paired(domain,pair)

IR.speed.0nM.102 <- domain.y %>% mutate(cell="102",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="Y")   
IR.speed.0nM.102N <- domain.n %>% mutate(cell="102",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="N")   

Y.0.102 <- IR.speed.0nM.102 %>% rename(Track_ID=Track)
write.csv(Y.0.102,file = "filterTracks/Y-0-102.csv")

N.0.102 <- IR.speed.0nM.102N %>% rename(Track_ID=Track)
write.csv(N.0.102,file = "filterTracks/N-0-102.csv")

#
IR.speed.yn <- rbind(IR.speed.0nM.002,IR.speed.2nM.002,IR.speed.2nM.005,IR.speed.0nM.006,IR.speed.0nM.007,IR.speed.0nM.011,IR.speed.2nM.011,IR.speed.2nM.013,
                IR.speed.0nM.105,IR.speed.0nM.102, 
                IR.speed.0nM.002N,IR.speed.2nM.002N,IR.speed.2nM.005N,IR.speed.0nM.006N,IR.speed.0nM.007N,IR.speed.0nM.011N,IR.speed.2nM.011N,IR.speed.2nM.013N,
                IR.speed.0nM.105N,IR.speed.0nM.102N)
IR.speed.yn <- IR.speed.yn %>% mutate(Radius=Search_radius_px*0.129,subgroup=paste(coexp,Insulin,sep = "_"),Lifetime=Duration_frames*2,coexp.pair=paste(coexp,Interact,sep = ".")) %>% 
  filter(Duration_frames>3) %>% mutate(subtype=paste(coexp.pair,Insulin,sep="."))

IR.speed.i.yn <- rbind(IR.speed.0nM.002,IR.speed.2nM.002.i,IR.speed.2nM.005,IR.speed.0nM.006,IR.speed.0nM.007,IR.speed.0nM.011,IR.speed.2nM.011.i,IR.speed.2nM.013,
                                  IR.speed.0nM.105,IR.speed.0nM.102,
                                  IR.speed.0nM.002N,IR.speed.2nM.002.iN,IR.speed.2nM.005N,IR.speed.0nM.006N,IR.speed.0nM.007N,IR.speed.0nM.011N,IR.speed.2nM.011.iN,IR.speed.2nM.013N,
                                  IR.speed.0nM.105N,IR.speed.0nM.102N)
IR.speed.i.yn <- IR.speed.i.yn %>% mutate(Radius=Search_radius_px*0.129,subgroup=paste(Radius=coexp,Insulin,sep = "_"),Lifetime=Duration_frames*2,coexp.pair=paste(coexp,Interact,sep = ".")) %>% 
  filter(Duration_frames>3) %>% mutate(subtype=paste(coexp.pair,Insulin,sep="."))


IR.speed.yn$subgroup <- factor(IR.speed.yn$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))
IR.speed.i.yn$subgroup <- factor(IR.speed.i.yn$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))

IR.speed.yn$subtype <- factor(IR.speed.yn$subtype, levels=c("CAV1.N.0nM","CAV1.N.2nM","CAV1.Y.0nM","CAV1.Y.2nM",
                                                                "Clathrin.N.0nM","Clathrin.N.2nM","Clathrin.Y.0nM","Clathrin.Y.2nM"))
IR.speed.i.yn$subtype <- factor(IR.speed.i.yn$subtype, levels=c("CAV1.N.0nM","CAV1.N.2nM","CAV1.Y.0nM","CAV1.Y.2nM",
                                                                "Clathrin.N.0nM","Clathrin.N.2nM","Clathrin.Y.0nM","Clathrin.Y.2nM"))

IR.speed.yn$coexp.pair <- factor(IR.speed.yn$coexp.pair, levels=c("CAV1.N","CAV1.Y", "Clathrin.N","Clathrin.Y"))
IR.speed.i.yn$coexp.pair <- factor(IR.speed.i.yn$coexp.pair, levels=c("CAV1.N","CAV1.Y", "Clathrin.N","Clathrin.Y"))

summary(IR.speed.yn$subgroup)
summary(IR.speed.yn$coexp.pair)
summary(IR.speed.i.yn$subtype)

#--- 
# filter track files by Python, calculate diffussion coefficient in MATLAB, then read the output files here.
dc.0nM.002 <- t(read.csv(file="input/CCpair/002-0-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="002",coexp="CAV1", Insulin="0nM") %>% mutate(Interact="Y")
dc.0nM.002N <- t(read.csv(file="input/CCpair/002-0-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="002",coexp="CAV1", Insulin="0nM") %>% mutate(Interact="N")

dc.2nM.002 <- t(read.csv(file="input/CCpair/002-2-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="002",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="Y")     
dc.2nM.002N <- t(read.csv(file="input/CCpair/002-2-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="002",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="N")  

dc.2nM.002.i <- t(read.csv(file="input/CCpair/002-2-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="002i",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="Y")   
dc.2nM.002.iN <- t(read.csv(file="input/CCpair/002-2-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="002i",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="N")  

dc.2nM.005 <- t(read.csv(file="input/CCpair/005-2-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="005",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="Y")   
dc.2nM.005N <- t(read.csv(file="input/CCpair/005-2-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="005",coexp="CAV1", Insulin="2nM") %>% mutate(Interact="N")   

dc.0nM.006 <- t(read.csv(file="input/CCpair/006-0-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="006",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="Y")     
dc.0nM.006N <- t(read.csv(file="input/CCpair/006-0-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="006",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="N") 

dc.0nM.007 <- t(read.csv(file="input/CCpair/007-0-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="007",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="Y")   
dc.0nM.007N <- t(read.csv(file="input/CCpair/007-0-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="007",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="N")  

dc.0nM.011 <- t(read.csv(file="input/CCpair/011-0-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="011",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="Y")   
dc.0nM.011N <- t(read.csv(file="input/CCpair/011-0-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="011",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="N")

dc.2nM.011 <- t(read.csv(file="input/CCpair/011-2-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="011",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="Y")   
dc.2nM.011N <- t(read.csv(file="input/CCpair/011-2-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="011",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="N")

dc.2nM.011.i <- t(read.csv(file="input/CCpair/011-2-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="011i",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="Y")   
dc.2nM.011.iN <- t(read.csv(file="input/CCpair/011-2-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="011i",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="N") 

dc.2nM.013 <- t(read.csv(file="input/CCpair/013-2-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="013",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="Y")   
dc.2nM.013N <- t(read.csv(file="input/CCpair/013-2-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="013",coexp="Clathrin", Insulin="2nM") %>% mutate(Interact="N")  

dc.0nM.105 <- t(read.csv(file="input/CCpair/105-0-C0-Y.csv", header = F )) %>% as.data.frame() %>% mutate(cell="105",coexp="CAV1", Insulin="0nM") %>% mutate(Interact="Y")   
dc.0nM.105N <- t(read.csv(file="input/CCpair/105-0-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="105",coexp="CAV1", Insulin="0nM") %>% mutate(Interact="N")

dc.0nM.102 <- t(read.csv(file="input/CCpair/102-0-C0-Y.csv", header = F ))  %>% as.data.frame() %>% mutate(cell="102",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="Y")   
dc.0nM.102N <- t(read.csv(file="input/CCpair/102-0-C0-N.csv", header = F )) %>% as.data.frame() %>% mutate(cell="102",coexp="Clathrin", Insulin="0nM") %>% mutate(Interact="N")   

dc.yn <- rbind(dc.0nM.002,dc.2nM.002,dc.2nM.005,dc.0nM.006,dc.0nM.007,dc.0nM.011,dc.2nM.011,dc.2nM.013,
                     dc.0nM.105,dc.0nM.102, 
                     dc.0nM.002N,dc.2nM.002N,dc.2nM.005N,dc.0nM.006N,dc.0nM.007N,dc.0nM.011N,dc.2nM.011N,dc.2nM.013N,
                     dc.0nM.105N,dc.0nM.102N)
dc.yn <- dc.yn %>% mutate(subgroup=paste(coexp,Insulin,sep = "_"),coexp.pair=paste(coexp,Interact,sep = ".")) %>% 
  mutate(subtype=paste(coexp.pair,Insulin,sep=".")) %>% rename(diffusion.coeff=V1)

dc.i.yn <- rbind(dc.0nM.002,dc.2nM.002.i,dc.2nM.005,dc.0nM.006,dc.0nM.007,dc.0nM.011,dc.2nM.011.i,dc.2nM.013,
                       dc.0nM.105,dc.0nM.102,
                       dc.0nM.002N,dc.2nM.002.iN,dc.2nM.005N,dc.0nM.006N,dc.0nM.007N,dc.0nM.011N,dc.2nM.011.iN,dc.2nM.013N,
                       dc.0nM.105N,dc.0nM.102N)
dc.i.yn <- dc.i.yn %>% mutate(subgroup=paste(coexp,Insulin,sep = "_"),coexp.pair=paste(coexp,Interact,sep = ".")) %>% 
  mutate(subtype=paste(coexp.pair,Insulin,sep=".")) %>% rename(diffusion.coeff=V1)
view(dc.i.yn)

dc.yn$subgroup <- factor(dc.yn$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))
dc.i.yn$subgroup <- factor(dc.i.yn$subgroup, levels=c("CAV1_0nM","CAV1_2nM","Clathrin_0nM", "Clathrin_2nM"))

dc.yn$subtype <- factor(dc.yn$subtype, levels=c("CAV1.N.0nM","CAV1.Y.0nM","CAV1.N.2nM","CAV1.Y.2nM",
                                                            "Clathrin.N.0nM","Clathrin.Y.0nM","Clathrin.N.2nM","Clathrin.Y.2nM"))
dc.i.yn$subtype <- factor(dc.i.yn$subtype, levels=c("CAV1.N.0nM","CAV1.N.2nM","CAV1.Y.0nM","CAV1.Y.2nM",
                                                                "Clathrin.N.0nM","Clathrin.N.2nM","Clathrin.Y.0nM","Clathrin.Y.2nM"))

dc.yn$coexp.pair <- factor(dc.yn$coexp.pair, levels=c("CAV1.N","CAV1.Y", "Clathrin.N","Clathrin.Y"))
dc.i.yn$coexp.pair <- factor(dc.i.yn$coexp.pair, levels=c("CAV1.N","CAV1.Y", "Clathrin.N","Clathrin.Y"))

summary(dc.yn$subgroup)
summary(dc.yn$coexp.pair)
summary(dc.i.yn$subtype)

#=============
#plot all conditions together - lifetime and radius

#lifetime 
head(IR.speed.yn)
ReplicateAverages <- IR.speed.yn %>% 
  group_by(subtype, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)

ggplot(IR.speed.yn, aes(x=subtype,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  geom_violin(aes(x=subtype,y=Lifetime),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
    scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=2,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=2,shape = 1,size = 3,colour = "grey30")+
  
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  # annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYNins_liftime_CC_super'.png",width=5,height=3.5,units="in",dpi=600)

ggplot(IR.speed.i.yn, aes(x=subtype,y=Radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  geom_violin(aes(x=subtype,y=Radius),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=2,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=2,shape = 1,size = 3,colour = "grey30")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  coord_cartesian(ylim=c(10^(-1.5),10^0.65))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYNins_radius_CC_super'.png",width=5,height=3.5,units="in",dpi=600)


#---------
ins.CAV1.0nM <- IR.speed.i.yn[c(IR.speed.i.yn$subgroup=="CAV1_0nM"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- IR.speed.i.yn[c(IR.speed.i.yn$subgroup=="CAV1_2nM"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- IR.speed.i.yn[c(IR.speed.i.yn$subgroup=="Clathrin_0nM"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))

ins.Clath.2nM <- IR.speed.i.yn[c(IR.speed.i.yn$subgroup=="Clathrin_2nM"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)
View(IR.CCins)

medians <- IR.CCins %>%
  group_by(subgroup) %>%
  dplyr::summarize(median_value = median(Radius, na.rm = TRUE))

ggplot(IR.CCins, aes(x=Radius, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-1.5),10^0.65))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1) +
  geom_point(data=medians, aes(x=median_value, y=0.5), size=2, ) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subgroup), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value

ggsave(filename="figure/Track_radius_CCins_median_new.svg",width=4.5,height=3,units="in", device = svg)

ks.test(x=ins.CAV1.2nM$Radius, y=ins.CAV1.0nM$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.076543, p-value = 0.2705

ks.test(x=ins.Clath.2nM$Radius, y=ins.Clath.0nM$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.10316, p-value = 0.137

ks.test(x=ins.CAV1.0nM$Radius, y=ins.Clath.0nM$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.20978, p-value = 2.627e-10

ks.test(x=ins.CAV1.2nM$Radius, y=ins.Clath.2nM$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.22349, p-value = 8.601e-05

# lifetime and radius - subgroup, coexp+Insulin, cav & clath

ReplicateAverages <- IR.speed.yn %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
ReplicateAverages <- ReplicateAverages %>% separate(subgroup,c("coexp","Insulin"),sep = "([_])") %>% 
  mutate(subgroup=paste(coexp,Insulin,sep = "_"))

aov <- ReplicateAverages %>% anova_test(Lifetime ~ coexp * Insulin)
aov
stat<- ReplicateAverages %>% pairwise_t_test(Lifetime ~ subgroup, p.adjust.method = "BH") %>%
  add_significance()
stat

aov <- ReplicateAverages %>% anova_test(Radius ~ coexp * Insulin)
aov
stat<- ReplicateAverages %>% pairwise_t_test(Radius ~ subgroup, p.adjust.method = "BH") %>%
  add_significance()
stat
#stat<- stat %>% add_xy_position(x = "subgroup")
# Radius
ggplot(IR.speed.yn, aes(x=subgroup,y=Radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  
  geom_violin(aes(x=subgroup,y=Radius),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
  
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=3,shape = 1,size = 3,colour = "grey30")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  coord_cartesian(ylim=c(10^(-1.5),10^0.65))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle=45,hjust=1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in
ggsave(filename="figure/radius_CCins_super'.svg",width=3.5,height=3.5,units="in",device = svg)
ggsave(filename="radius_CCins_super'.png",width=3.5,height=3.5,units="in",dpi=600)

# Lifetime
ggplot(IR.speed.yn, aes(x=subgroup,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  
  geom_violin(aes(x=subgroup,y=Lifetime),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=3,shape = 1,size = 3,colour = "grey30")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle=45,hjust=1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10))  #4.5 x 3.5 in

ggsave(filename="figure/lifetime_CCins_super'_new.svg",width=3.5,height=3.5,units="in",device=svg)

ggsave(filename="lifetime_CCins_super'.png",width=3.5,height=3.5,units="in",dpi=600)

#-------------
ins.CAV1.0nM <- IR.speed.yn[c(IR.speed.yn$subgroup=="CAV1_0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- IR.speed.yn[c(IR.speed.yn$subgroup=="CAV1_2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- IR.speed.yn[c(IR.speed.yn$subgroup=="Clathrin_0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))

ins.Clath.2nM <- IR.speed.yn[c(IR.speed.yn$subgroup=="Clathrin_2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)

medians <- IR.CCins %>%
  group_by(subgroup) %>%
  dplyr::summarize(median_value = median(Lifetime, na.rm = TRUE))

ggplot(IR.CCins, aes(x=Lifetime, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
  #scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="b")+  # "lrtb"
  #coord_cartesian(xlim=c(10^(1),10^2.5))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1) +
  geom_point(data=medians, aes(x=median_value, y=0.5), size=2,) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subgroup), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value


ggsave(filename="figure/Lifetime_CCins'_median_new.svg",width=4.5,height=3,units="in",device = svg)

ks.test(x=ins.CAV1.2nM$Lifetime, y=ins.CAV1.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.047222, p-value = 0.8415

ks.test(x=ins.Clath.2nM$Lifetime, y=ins.Clath.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.16384, p-value = 0.002315

ks.test(x=ins.CAV1.0nM$Lifetime, y=ins.Clath.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.20216, p-value = 1.331e-09

ks.test(x=ins.CAV1.2nM$Lifetime, y=ins.Clath.2nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.072594, p-value = 0.6638

ks.test(x=ins.CAV1.2nM$Lifetime, y=ins.Clath.0nM$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.17346, p-value = 1.107e-06


#========
# plot radius and lifetime, CAV1 or Clath colocalized separately

head(IR.speed.i.yn) 
IR.speed.i.yn.cav <- IR.speed.i.yn %>% filter(coexp=="CAV1")
ReplicateAverages.cav <- IR.speed.i.yn.cav %>% 
  group_by(subtype, cell) %>%  
  summarise_each(funs(mean))
IR.speed.i.yn.clath <- IR.speed.i.yn %>% filter(coexp=="Clathrin")
ReplicateAverages.clath <- IR.speed.i.yn.clath %>% 
  group_by(subtype, cell) %>%  
  summarise_each(funs(mean))
#---
view(ReplicateAverages.cav)
ReplicateAverages.cav <- ReplicateAverages.cav %>% separate(subtype,c("coexp","Interact","Insulin"),sep = "([.])") %>% 
  mutate(coexp.pair=paste(coexp,Interact,sep = "."),subgroup=paste(coexp,Insulin,sep="_"),subtype=paste(coexp.pair,Insulin,sep="."))
library(rstatix)
aov <- ReplicateAverages.cav %>% anova_test(Lifetime ~ Interact * Insulin)
aov
aov <- ReplicateAverages.cav %>% anova_test(Radius ~ Interact * Insulin)
aov
stat<- ReplicateAverages.cav %>% pairwise_t_test(Lifetime ~ subtype, p.adjust.method = "BH") %>%
  add_significance()
stat
stat<- ReplicateAverages.cav %>% pairwise_t_test(Radius ~ subtype, p.adjust.method = "BH") %>%
  add_significance()
stat

#
ReplicateAverages.clath <- ReplicateAverages.clath %>% separate(subtype,c("coexp","Interact","Insulin"),sep = "([.])") %>% 
  mutate(coexp.pair=paste(coexp,Interact,sep = "."),subgroup=paste(coexp,Insulin,sep="_"),subtype=paste(coexp.pair,Insulin,sep="."))
library(rstatix)
aov <- ReplicateAverages.clath %>% anova_test(Lifetime ~ Interact * Insulin)
aov
aov <- ReplicateAverages.clath %>% anova_test(Radius ~ Interact * Insulin)
aov
stat<- ReplicateAverages.clath %>% pairwise_t_test(Lifetime ~ subtype, p.adjust.method = "BH") %>%
  add_significance()
stat
stat<- ReplicateAverages.clath %>% pairwise_t_test(Radius ~ subtype, p.adjust.method = "BH") %>%
  add_significance()
stat
#
aov <- ReplicateAverages %>% anova_test(Radius ~ coexp + Interact)
aov
stat<- ReplicateAverages %>% pairwise_t_test(Radius ~ coexp.pair, p.adjust.method = "BH") %>%
  add_significance()
stat

#---
ggplot(IR.speed.i.yn.cav, aes(x=subtype,y=Radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  
  geom_violin(aes(x=subtype,y=Radius),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 

  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,shape = 1,size = 3,colour = "grey30")+
  #stat_compare_means(data=ReplicateAverages.cav, comparisons = list(c("CAV1.Y", "CAV1.N")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  coord_cartesian(ylim=c(10^(-1.5),10^0.655))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="figure/pairYN_radius_CAV1_super'_new.svg",width=3.5,height=3.5,units="in",device = svg)

ggsave(filename="pairYN_radius_CAV1_super'.png",width=3.5,height=3.5,units="in",dpi=600)
#
ggplot(IR.speed.i.yn.clath, aes(x=subtype,y=Radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  
  geom_violin(aes(x=subtype,y=Radius),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
 
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,shape = 1,size = 3,colour = "grey30")+
  #stat_compare_means(data=ReplicateAverages.clath, comparisons = list(c("Clathrin.Y", "Clathrin.N")), method="t.test", paired=F) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  coord_cartesian(ylim=c(10^(-1.5),10^0.65))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="figure/pairYN_radius_Clath_super'_new.svg",width=3.5,height=3.5,units="in",device = svg)

ggsave(filename="pairYN_radius_Clath_super'.png",width=3.5,height=3.5,units="in",dpi=600)

#
# plot lifetime, CAV1 or Clath colocalized separately
ggplot(IR.speed.i.yn.cav, aes(x=subtype,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime(s)")+
  
  geom_violin(aes(x=subtype,y=Lifetime),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,shape = 1,size = 3,colour = "grey30")+
  #stat_compare_means(data=ReplicateAverages.cav, comparisons = list(c("CAV1.Y", "CAV1.N")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="figure/pairYN_lifetime_CAV1_super'_new.svg",width=3.5,height=3.5,units="in",device = svg)

ggsave(filename="pairYN_lifetime_CAV1_super'.png",width=3.5,height=3.5,units="in",dpi=600)
#
ggplot(IR.speed.i.yn.clath, aes(x=subtype,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  
  geom_violin(aes(x=subtype,y=Lifetime),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,shape = 1,size = 3,colour = "grey30")+
  #stat_compare_means(data=ReplicateAverages.clath, comparisons = list(c("Clathrin.Y", "Clathrin.N")), method="t.test", paired=F) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="figure/pairYN_lifetime_Clath_super'_new.svg",width=3.5,height=3.5,units="in",device = svg)

ggsave(filename="pairYN_lifetime_Clath_super'.png",width=3.5,height=3.5,units="in",dpi=600)

#-- CAV1 paired

ins.CAV1.N.0 <- IR.speed.i.yn.cav[c(IR.speed.i.yn.cav$subtype=="CAV1.N.0nM"),] %>%
  arrange(Search_radius_px)  %>% mutate(freq=1)
ins.CAV1.N.0 <- ins.CAV1.N.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.N.0))

ins.CAV1.Y.0 <- IR.speed.i.yn.cav[c(IR.speed.i.yn.cav$subtype=="CAV1.Y.0nM"),] %>%
  arrange(Search_radius_px)  %>% mutate(freq=1)
ins.CAV1.Y.0 <- ins.CAV1.Y.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.Y.0))

ins.CAV1.N.2 <- IR.speed.i.yn.cav[c(IR.speed.i.yn.cav$subtype=="CAV1.N.2nM"),] %>%
  arrange(Search_radius_px)  %>% mutate(freq=1)
ins.CAV1.N.2 <- ins.CAV1.N.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.N.2))

ins.CAV1.Y.2 <- IR.speed.i.yn.cav[c(IR.speed.i.yn.cav$subtype=="CAV1.Y.2nM"),] %>%
  arrange(Search_radius_px)  %>% mutate(freq=1)
ins.CAV1.Y.2 <- ins.CAV1.Y.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.Y.2))


ins.Clath.N.0 <- IR.speed.i.yn.clath[c(IR.speed.i.yn.clath$subtype=="Clathrin.N.0nM"),] %>%
  arrange(Search_radius_px)  %>% mutate(freq=1)
ins.Clath.N.0 <- ins.Clath.N.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.N.0))

ins.Clath.Y.0 <- IR.speed.i.yn.clath[c(IR.speed.i.yn.clath$subtype=="Clathrin.Y.0nM"),] %>%
  arrange(Search_radius_px)  %>% mutate(freq=1)
ins.Clath.Y.0 <- ins.Clath.Y.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.Y.0))

ins.Clath.N.2 <- IR.speed.i.yn.clath[c(IR.speed.i.yn.clath$subtype=="Clathrin.N.2nM"),] %>%
  arrange(Search_radius_px)  %>% mutate(freq=1)
ins.Clath.N.2 <- ins.Clath.N.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.N.2))

ins.Clath.Y.2 <- IR.speed.i.yn.clath[c(IR.speed.i.yn.clath$subtype=="Clathrin.Y.2nM"),] %>%
  arrange(Search_radius_px)  %>% mutate(freq=1)
ins.Clath.Y.2 <- ins.Clath.Y.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.Y.2))

IR.cav <- rbind(ins.CAV1.N.0,ins.CAV1.N.2,ins.CAV1.Y.0,ins.CAV1.Y.2)

# calculate median
medians <- IR.cav %>%
  group_by(subtype) %>%
  dplyr::summarize(median_value = median(Radius, na.rm = TRUE))

ggplot(IR.cav, aes(x=Radius, y=cum_frequency,color= subtype)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  #scale_color_manual(values=c("cyan", "coral2"))+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-1.5),10^0.65))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1) +
  geom_point(data=medians, aes(x=median_value, y=0.5), size=2,) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subtype), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value

ggsave(filename="figure/pairYN_Track_radius_CAV1'_median.svg",width=4.5,height=3,units="in",device = svg)

ggsave(filename="pairYN_Track_radius_CAV1'.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.N.0$Search_radius_px, y=ins.CAV1.N.2$Search_radius_px,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.074667, p-value = 0.9226
ks.test(x=ins.CAV1.Y.0$Search_radius_px, y=ins.CAV1.Y.2$Search_radius_px,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.089772, p-value = 0.3086
ks.test(x=ins.CAV1.N.0$Search_radius_px, y=ins.CAV1.Y.0$Search_radius_px,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.18451, p-value = 0.00773
ks.test(x=ins.CAV1.N.2$Search_radius_px, y=ins.CAV1.Y.2$Search_radius_px,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.24945, p-value = 0.0004464

IR.clath <- rbind(ins.Clath.N.0,ins.Clath.N.2,ins.Clath.Y.0,ins.Clath.Y.2)

# calculate median
medians <- IR.clath %>%
  group_by(subtype) %>%
  dplyr::summarize(median_value = median(Radius, na.rm = TRUE))

ggplot(IR.clath, aes(x=Radius, y=cum_frequency,color= subtype)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
  #scale_color_manual(values=c("cyan", "coral2"))+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-1.5),10^0.65))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1)+
  geom_point(data=medians, aes(x=median_value, y=0.5), size=2,) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subtype), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value

ggsave(filename="figure/pairYN_radius_Clath'_median.svg",width=4.5,height=3,units="in",device=svg)

ggsave(filename="pairYN_radius_Clath'.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.Clath.N.0$Search_radius_px, y=ins.Clath.N.2$Search_radius_px,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.15081, p-value = 0.0551
ks.test(x=ins.Clath.Y.0$Search_radius_px, y=ins.Clath.Y.2$Search_radius_px,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.1429, p-value = 0.2933
ks.test(x=ins.Clath.N.0$Search_radius_px, y=ins.Clath.Y.0$Search_radius_px,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.070896, p-value = 0.2459
ks.test(x=ins.Clath.N.2$Search_radius_px, y=ins.Clath.Y.2$Search_radius_px,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.22058, p-value = 0.07116

#-- CAV1 or clathrin paired
#---
ins.CAV1.N.0 <- IR.speed.i.yn.cav[c(IR.speed.i.yn.cav$subtype=="CAV1.N.0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.N.0 <- ins.CAV1.N.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.N.0))

ins.CAV1.Y.0 <- IR.speed.i.yn.cav[c(IR.speed.i.yn.cav$subtype=="CAV1.Y.0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.Y.0 <- ins.CAV1.Y.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.Y.0))

ins.CAV1.N.2 <- IR.speed.i.yn.cav[c(IR.speed.i.yn.cav$subtype=="CAV1.N.2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.N.2 <- ins.CAV1.N.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.N.2))

ins.CAV1.Y.2 <- IR.speed.i.yn.cav[c(IR.speed.i.yn.cav$subtype=="CAV1.Y.2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.CAV1.Y.2 <- ins.CAV1.Y.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.Y.2))


ins.Clath.N.0 <- IR.speed.i.yn.clath[c(IR.speed.i.yn.clath$subtype=="Clathrin.N.0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.N.0 <- ins.Clath.N.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.N.0))

ins.Clath.Y.0 <- IR.speed.i.yn.clath[c(IR.speed.i.yn.clath$subtype=="Clathrin.Y.0nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.Y.0 <- ins.Clath.Y.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.Y.0))

ins.Clath.N.2 <- IR.speed.i.yn.clath[c(IR.speed.i.yn.clath$subtype=="Clathrin.N.2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.N.2 <- ins.Clath.N.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.N.2))

ins.Clath.Y.2 <- IR.speed.i.yn.clath[c(IR.speed.i.yn.clath$subtype=="Clathrin.Y.2nM"),] %>%
  arrange(Lifetime)  %>% mutate(freq=1)
ins.Clath.Y.2 <- ins.Clath.Y.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.Y.2))

IR.cav <- rbind(ins.CAV1.N.0,ins.CAV1.N.2,ins.CAV1.Y.0,ins.CAV1.Y.2)

# calculate median
medians <- IR.cav %>%
  group_by(subtype) %>%
  dplyr::summarize(median_value = median(Lifetime, na.rm = TRUE))


ggplot(IR.cav, aes(x=Lifetime, y=cum_frequency,color= subtype)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
  #scale_color_manual(values=c("cyan", "coral2"))+
  
 # scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="b")+  # "lrtb"
  #coord_cartesian(xlim=c(10^(-1.5),10^0.65))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1)+  
  geom_point(data=medians, aes(x=median_value, y=0.5), size=2,) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subtype), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value

ggsave(filename="figure/pairYN_lifetime_CAV1'_median.svg",width=4.5,height=3,units="in",device=svg)

ggsave(filename="pairYN_lifetime_CAV1'.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.N.0$Lifetime, y=ins.CAV1.N.2$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.088667, p-value = 0.7868
ks.test(x=ins.CAV1.Y.0$Lifetime, y=ins.CAV1.Y.2$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.077996, p-value = 0.4821
ks.test(x=ins.CAV1.N.0$Lifetime, y=ins.CAV1.Y.0$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.44647, p-value = 1.488e-14
ks.test(x=ins.CAV1.N.2$Lifetime, y=ins.CAV1.Y.2$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.38925, p-value = 2.572e-09

IR.clath <- rbind(ins.Clath.N.0,ins.Clath.N.2,ins.Clath.Y.0,ins.Clath.Y.2)

# calculate median
medians <- IR.clath %>%
  group_by(subtype) %>%
  dplyr::summarize(median_value = median(Lifetime, na.rm = TRUE))

ggplot(IR.clath, aes(x=Lifetime, y=cum_frequency,color= subtype)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
  #scale_color_manual(values=c("cyan", "coral2"))+
  
  #scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="b")+  # "lrtb"
  #coord_cartesian(xlim=c(10^(-1.5),10^0.65))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1) +
  geom_point(data=medians, aes(x=median_value, y=0.5), size=2,) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subtype), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value

ggsave(filename="figure/pairYN_Lifetime_Clath'_median.svg",width=4.5,height=3,units="in",device = svg)

ggsave(filename="pairYN_Lifetime_Clath'.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.Clath.N.0$Lifetime, y=ins.Clath.N.2$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.12305, p-value = 0.1829
ks.test(x=ins.Clath.Y.0$Lifetime, y=ins.Clath.Y.2$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.33097, p-value = 6.849e-05
ks.test(x=ins.Clath.N.0$Lifetime, y=ins.Clath.Y.0$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.28432, p-value = 4.663e-15
ks.test(x=ins.Clath.N.2$Lifetime, y=ins.Clath.Y.2$Lifetime,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.44076, p-value = 3.285e-06
#---
#
#=================
# plot diffusion coefficient, CAV1 or Clath colocalized separately
head(dc.i.yn) 
dc.i.yn.cav <- dc.i.yn %>% filter(coexp=="CAV1")
ReplicateAverages.cav <- dc.i.yn.cav %>% 
  group_by(subtype, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages.cav)
dc.i.yn.clath <- dc.i.yn %>% filter(coexp=="Clathrin")
ReplicateAverages.clath <- dc.i.yn.clath %>% 
  group_by(subtype, cell) %>%  
  summarise_each(funs(mean))
#---
view(ReplicateAverages.cav)
ReplicateAverages.cav <- ReplicateAverages.cav %>% separate(subtype,c("coexp","Interact","Insulin"),sep = "([.])") %>% 
  mutate(coexp.pair=paste(coexp,Interact,sep = "."),subgroup=paste(coexp,Insulin,sep="_"),subtype=paste(coexp.pair,Insulin,sep="."))
library(rstatix)
aov <- ReplicateAverages.cav %>% anova_test(diffusion.coeff ~ Interact * Insulin)
aov

stat<- ReplicateAverages.cav %>% pairwise_t_test(diffusion.coeff ~ subtype, p.adjust.method = "BH") %>%
  add_significance()
stat

#
ReplicateAverages.clath <- ReplicateAverages.clath %>% separate(subtype,c("coexp","Interact","Insulin"),sep = "([.])") %>% 
  mutate(coexp.pair=paste(coexp,Interact,sep = "."),subgroup=paste(coexp,Insulin,sep="_"),subtype=paste(coexp.pair,Insulin,sep="."))
library(rstatix)
aov <- ReplicateAverages.clath %>% anova_test(diffusion.coeff ~ Interact * Insulin)
aov
stat<- ReplicateAverages.clath %>% pairwise_t_test(diffusion.coeff ~ subtype, p.adjust.method = "BH") %>%
  add_significance()
stat

#---

ggplot(dc.i.yn.cav, aes(x=subtype,y=diffusion.coeff,color=factor(cell))) + 
  labs(x=" ", y="Diffusion coefficient (µm2/s)")+
  
  geom_violin(aes(x=subtype,y=diffusion.coeff),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.cav,cex=3,shape = 1,size = 3,colour = "black")+
  #stat_compare_means(data=ReplicateAverages.cav, comparisons = list(c("CAV1.Y", "CAV1.N")), method="t.test", paired=T) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
   coord_cartesian(ylim=c(10^(-1.8),10^1.4))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle=45,hjust=1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in
ggsave(filename="figure/pairYN_diffusion_CAV1_super'_new.svg",width=3.5,height=3.5,units="in",device = svg)

ggsave(filename="pairYN_diffusion_CAV1_super'.png",width=3.5,height=3.5,units="in",dpi=600)
#

ggplot(dc.i.yn.clath, aes(x=subtype,y=diffusion.coeff,color=factor(cell))) + 
  labs(x=" ", y="Diffusion coefficient (µm2/s)")+
  
  geom_violin(aes(x=subtype,y=diffusion.coeff),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages.clath,cex=3,shape = 1,size = 3,colour = "black")+
  #stat_compare_means(data=ReplicateAverages.cav, comparisons = list(c("CAV1.Y", "CAV1.N")), method="t.test", paired=T) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  coord_cartesian(ylim=c(10^(-1.8),10^1.4))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle=45, hjust=1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in
ggsave(filename="figure/pairYN_diffusion_Clath_super'_new.svg",width=3.5,height=3.5,units="in",device = svg)

ggsave(filename="pairYN_diffusion_Clath_super'.png",width=3.5,height=3.5,units="in",dpi=600)

# ------
head(dc.yn)
ins.CAV1.0nM <- dc.yn[c(dc.yn$subgroup=="CAV1_0nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- dc.yn[c(dc.yn$subgroup=="CAV1_2nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- dc.yn[c(dc.yn$subgroup=="Clathrin_0nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))
view(ins.Clath.0nM)

ins.Clath.2nM <- dc.yn[c(dc.yn$subgroup=="Clathrin_2nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)

str(IR.CCins)
View(IR.CCins)
# calculate median
medians <- IR.CCins %>%
  group_by(subgroup) %>%
  dplyr::summarize(median_value = median(diffusion.coeff, na.rm = TRUE))

ggplot(IR.CCins, aes(x=diffusion.coeff, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Diffusion coefficient (µm2/s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-1.8),10^1.3))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1) +
geom_point(data=medians, aes(x=median_value, y=0.5), size=2,) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subgroup), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value

ggsave(filename="figure/diffusion_CCins_median.svg",width=4.5,height=3,units="in",device=svg)

ggsave(filename="diffusion_CCins.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.2nM$diffusion.coeff, y=ins.CAV1.0nM$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.082894, p-value = 0.1952

ks.test(x=ins.Clath.2nM$diffusion.coeff, y=ins.Clath.0nM$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.14501, p-value = 0.01158

ks.test(x=ins.CAV1.0nM$diffusion.coeff, y=ins.Clath.0nM$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.29132, p-value < 2.2e-16

ks.test(x=ins.CAV1.2nM$diffusion.coeff, y=ins.Clath.2nM$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.24634, p-value = 1.027e-05
12345
#-----
ins.CAV1.N.0 <- dc.yn[c(dc.yn$subtype=="CAV1.N.0nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.CAV1.N.0 <- ins.CAV1.N.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.N.0))

ins.CAV1.Y.0 <- dc.yn[c(dc.yn$subtype=="CAV1.Y.0nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.CAV1.Y.0 <- ins.CAV1.Y.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.Y.0))

ins.CAV1.N.2 <- dc.yn[c(dc.yn$subtype=="CAV1.N.2nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.CAV1.N.2 <- ins.CAV1.N.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.N.2))

ins.CAV1.Y.2 <- dc.yn[c(dc.yn$subtype=="CAV1.Y.2nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.CAV1.Y.2 <- ins.CAV1.Y.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.Y.2))


ins.Clath.N.0 <- dc.yn[c(dc.yn$subtype=="Clathrin.N.0nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.Clath.N.0 <- ins.Clath.N.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.N.0))

ins.Clath.Y.0 <- dc.yn[c(dc.yn$subtype=="Clathrin.Y.0nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.Clath.Y.0 <- ins.Clath.Y.0 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.Y.0))

ins.Clath.N.2 <- dc.yn[c(dc.yn$subtype=="Clathrin.N.2nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.Clath.N.2 <- ins.Clath.N.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.N.2))

ins.Clath.Y.2 <- dc.yn[c(dc.yn$subtype=="Clathrin.Y.2nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.Clath.Y.2 <- ins.Clath.Y.2 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.Y.2))

IR.cav <- rbind(ins.CAV1.N.0,ins.CAV1.N.2,ins.CAV1.Y.0,ins.CAV1.Y.2)


# calculate median
medians <- IR.cav %>%
  group_by(subtype) %>%
  dplyr::summarize(median_value = median(Lifetime, na.rm = TRUE))


ggplot(IR.cav, aes(x=diffusion.coeff, y=cum_frequency,color= subtype)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Diffusion coefficient (µm2/s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-1.8),10^1.3))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1)+
  geom_point(data=medians, aes(x=median_value, y=0.5), size=2,) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subtype), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value

ggsave(filename="pairYN_diffusion_CAV1'.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.N.0$diffusion.coeff, y=ins.CAV1.N.2$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.12316, p-value = 0.3941
ks.test(x=ins.CAV1.Y.0$diffusion.coeff, y=ins.CAV1.Y.2$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.086137, p-value = 0.3584
ks.test(x=ins.CAV1.N.0$diffusion.coeff, y=ins.CAV1.Y.0$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.15333, p-value = 0.0449
ks.test(x=ins.CAV1.N.2$diffusion.coeff, y=ins.CAV1.Y.2$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.16379, p-value = 0.0565

#
IR.clath <- rbind(ins.Clath.N.0,ins.Clath.N.2,ins.Clath.Y.0,ins.Clath.Y.2)

# calculate median
medians <- IR.cav %>%
  group_by(subtype) %>%
  dplyr::summarize(median_value = median(Lifetime, na.rm = TRUE))


ggplot(IR.clath, aes(x=diffusion.coeff, y=cum_frequency,color= subtype)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Diffusion coefficient (µm2/s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-1.8),10^1.3))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1)+
  geom_point(data=medians, aes(x=median_value, y=0.5), size=2,) +  # larger point for median
  geom_text_repel(data=medians, 
                  aes(x=median_value, y=0.5, 
                      label=round(median_value, 2), 
                      color=subtype), 
                  size=4, vjust=-1, show.legend = FALSE)  # label for median value

ggsave(filename="pairYN_diffusion_Clath'.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.Clath.N.0$diffusion.coeff, y=ins.Clath.N.2$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.14612, p-value = 0.07617
ks.test(x=ins.Clath.Y.0$diffusion.coeff, y=ins.Clath.Y.2$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.14318, p-value = 0.3027
ks.test(x=ins.Clath.N.0$diffusion.coeff, y=ins.Clath.Y.0$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.20397, p-value = 8.045e-07
ks.test(x=ins.Clath.N.2$diffusion.coeff, y=ins.Clath.Y.2$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.196, p-value = 0.1217

#-----
#
# diffusion together
ReplicateAverages <- dc.i.yn %>% 
  group_by(subtype, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
ReplicateAverages <- ReplicateAverages %>% separate(subtype,c("coexp","Interact","Insulin"),sep = "([.])") %>% 
  mutate(coexp.pair=paste(coexp,Interact,sep = "."),subtype=paste(coexp,Interact,Insulin,sep = "."))

ggplot(dc.i.yn, aes(x=subtype,y=diffusion.coeff,color=factor(cell))) + 
  labs(x=" ", y="Diffusion coefficient (µm2/s)")+
  
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=2,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=2,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  coord_cartesian(ylim=c(10^(-1.8),10^1.4))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="pairYNins_diffusion_CC_super'.png",width=5,height=3.5,units="in",dpi=600)

#
ggplot(dc.i.yn, aes(x=coexp.pair,y=diffusion.coeff,color=factor(cell))) + 
  labs(x=" ", y="Diffusion coefficient (µm2/s)")+
  
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.5,size=1) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=2,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=2,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  coord_cartesian(ylim=c(10^(-1.8),10^1.4))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in
ggsave(filename="pairYN_diffusion_CC_super'.png",width=5,height=3.5,units="in",dpi=600)

#======= subgroup, coexp + insulin
ReplicateAverages <- dc.yn %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
ReplicateAverages <- ReplicateAverages %>% separate(subgroup,c("coexp","Insulin"),sep = "([_])") %>% 
  mutate(subgroup=paste(coexp,Insulin,sep = "_"))

aov <- ReplicateAverages %>% anova_test(diffusion.coeff ~ coexp * Insulin)
aov
stat<- ReplicateAverages %>% pairwise_t_test(diffusion.coeff ~ subgroup, p.adjust.method = "BH") %>%
  add_significance()
stat

12345
ggplot(dc.yn, aes(x=subgroup,y=diffusion.coeff,color=factor(cell))) + 
  labs(x=" ", y="Diffusion coefficient (µm2/s)")+
  
  geom_violin(aes(x=subgroup,y=diffusion.coeff),color="grey30",trim=T)+
  #geom_beeswarm(cex=0.9,alpha=0.5,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.7,size=0.5) + 
  
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=2,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=2,shape = 1,size = 3,colour = "grey30")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("CAV1", "Clathrin")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  # stat_pvalue_manual(stat, label = "p.adj", tip.length = 0, size = 3,hide.ns = T) +
  
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  coord_cartesian(ylim=c(10^(-1.8),10^1.4))+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black",angle=45, hjust=1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in

ggsave(filename="diffusion_CCins_super'.png",width=3.5,height=3.5,units="in",dpi=600)


#
head(dc.yn)
ins.CAV1.0nM <- dc.yn[c(dc.yn$subgroup=="CAV1_0nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.CAV1.0nM <- ins.CAV1.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.0nM))

ins.CAV1.2nM <- dc.yn[c(dc.yn$subgroup=="CAV1_2nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.CAV1.2nM <- ins.CAV1.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.CAV1.2nM))

ins.Clath.0nM <- dc.yn[c(dc.yn$subgroup=="Clathrin_0nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.Clath.0nM <- ins.Clath.0nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.0nM))
view(ins.Clath.0nM)

ins.Clath.2nM <- dc.yn[c(dc.yn$subgroup=="Clathrin_2nM"),] %>%
  arrange(diffusion.coeff)  %>% mutate(freq=1)
ins.Clath.2nM <- ins.Clath.2nM %>% mutate(cum_frequency=cumsum(freq)/nrow(ins.Clath.2nM))

IR.CCins <- rbind(ins.CAV1.0nM,ins.CAV1.2nM,ins.Clath.0nM,ins.Clath.2nM)

ggplot(IR.CCins, aes(x=diffusion.coeff, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Diffusion coefficient (µm2/s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-1.8),10^1.3))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1)

ggsave(filename="diffusion_CCins.png",width=4.5,height=3,units="in",dpi=600)

ks.test(x=ins.CAV1.2nM$diffusion.coeff, y=ins.CAV1.0nM$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.082894, p-value = 0.1952

ks.test(x=ins.Clath.2nM$diffusion.coeff, y=ins.Clath.0nM$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.14501, p-value = 0.01158

ks.test(x=ins.CAV1.0nM$diffusion.coeff, y=ins.Clath.0nM$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.29132, p-value < 2.2e-16

ks.test(x=ins.CAV1.2nM$diffusion.coeff, y=ins.Clath.2nM$diffusion.coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.24634, p-value = 1.027e-05

#------------
filter_paired <- function(domain,pair){
domain <- domain %>%  
  dplyr::rename(Track=`Track #`,Start_frames='Start (frames)', End_frames='End (frames)',Duration_frames='Duration (frames)',
                Total_disp_px="Total disp. (px)",Net_disp_px="Net disp. (px)",Linearity="Linearity (%)",
                Search_radius_px="Search radius (px)",Min_disp_px="Min. disp. (px)",Max_disp_px="Max. disp. (px)",Avg_disp_px="Avg. disp. (px)") %>%
  filter(Max_disp_px<8 & 
           Duration_frames>3)

pair <- pair %>% select(2,4,5,6) %>% 
  dplyr::rename(Track=`Track ID...2`,TrackB=`Track ID...4`,start_frame=`Start frame`,nb_frames=`Nb. frames`) %>% 
  mutate(end_frame=start_frame+nb_frames-1) %>% filter(Track %in% domain$Track)
#pair1 <- pair %>% group_by(Track) %>% summarise_each(funs(sum)) %>% filter(nb_frames>4) # select tracks with total interaction >4
pair2 <- pair %>% filter(nb_frames>4) # only select tracks with continous interaction >4 frames

domain.y <<- domain %>% filter(Track %in% pair2$Track) # <<- can make the object avalible outside the function
domain.n <<- domain %>% filter(!Track %in% pair2$Track)

}

