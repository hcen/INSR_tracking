library(tidyverse)
library("readxl")
library(matrixStats)
library("ggbeeswarm")
library("RColorBrewer")

library("ggplot2")
library("dplyr")
library("readr")
library("psych")
library("plyr")
library("ggpubr")
library(scales)

library("here")
library("tidyverse")
library("gganimate")
library("extrafont")


library("grid")
library("gtable")
library("emmeans")
library("lme4")
#library("lmerTest")
library("data.table")
library("Amelia")
library("nlme")
library("readxl")
library("janitor")


setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) ##Set working directory to where this file is.

#======================================================================
# organize Area
Area <- read_excel("input/con_0min_001_Area.xlsx")
Area <- read_excel("input/con_0min_002_Area.xlsx")
Area <- read_excel("input/con_0min_004_Area.xlsx")
Area <- read_excel("input/con_11min_001_Area.xlsx")
Area <- read_excel("input/con_11min_002_Area.xlsx")
Area <- read_excel("input/con_11min_004_Area.xlsx")
Area <- read_excel("input/con_6min_001_Area.xlsx")
Area <- read_excel("input/con_6min_002_Area.xlsx")
Area <- read_excel("input/con_6min_004_Area.xlsx")

Area <- read_excel("input/ins_0min_001_Area.xlsx")
Area <- read_excel("input/ins_0min_003_Area.xlsx")
Area <- read_excel("input/ins_0min_004_Area.xlsx")
Area <- read_excel("input/ins_0min_007_Area.xlsx")
Area <- read_excel("input/ins_0min_009_Area.xlsx")
Area <- read_excel("input/ins_0min_010_Area.xlsx")
Area <- read_excel("input/ins_0min_013_Area.xlsx")
Area <- read_excel("input/ins_0min_014_Area.xlsx")
Area <- read_excel("input/ins_0min_015_Area.xlsx")
Area <- read_excel("input/ins_0min_016_Area.xlsx")


Area <- read_excel("input/ins_11min_001_Area.xlsx")
Area <- read_excel("input/ins_11min_003_Area.xlsx")
Area <- read_excel("input/ins_11min_004_Area.xlsx")
Area <- read_excel("input/ins_11min_007_Area.xlsx")
Area <- read_excel("input/ins_11min_009_Area.xlsx")
Area <- read_excel("input/ins_11min_010_Area.xlsx")
Area <- read_excel("input/ins_11min_013_Area.xlsx")
Area <- read_excel("input/ins_11min_014_Area.xlsx")
Area <- read_excel("input/ins_11min_015_Area.xlsx")
Area <- read_excel("input/ins_11min_016_Area.xlsx")

Area <- read_excel("input/ins_6min_001_Area.xlsx")
Area <- read_excel("input/ins_6min_003_Area.xlsx")
Area <- read_excel("input/ins_6min_004_Area.xlsx")
Area <- read_excel("input/ins_6min_007_Area.xlsx")
Area <- read_excel("input/ins_6min_009_Area.xlsx")
Area <- read_excel("input/ins_6min_010_Area.xlsx")
Area <- read_excel("input/ins_6min_013_Area.xlsx")
Area <- read_excel("input/ins_6min_014_Area.xlsx")
Area <- read_excel("input/ins_6min_015_Area.xlsx")
Area <- read_excel("input/ins_6min_016_Area.xlsx")

view(Area)

##==
Area[Area==0] <- NA
sd <- colSds(x=as.matrix(Area[,-1]), na.rm = TRUE)
max <- colMaxs(x=as.matrix(Area[,-1]), na.rm = TRUE)
min <- colMins(x=as.matrix(Area[,-1]), na.rm = TRUE)
mean <- colMeans(x=Area[,-1], na.rm = TRUE) %>% as.data.frame() %>% rename(c(meanArea=.)) %>% mutate(sdArea=sd, maxArea=max, minArea=min) # mean and SD of tracked particle's median intensity
head(mean)
tail(mean)

##==

write.csv(mean, sep="\t",file="data/con_0min_001_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/con_0min_002_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/con_0min_004_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/con_11min_001_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/con_11min_002_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/con_11min_004_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/con_6min_001_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/con_6min_002_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/con_6min_004_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)

write.csv(mean, sep="\t",file="data/ins_0min_001_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_003_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_004_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_007_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_009_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_010_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_013_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_014_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_015_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_0min_016_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)


write.csv(mean, sep="\t",file="data/ins_11min_001_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_003_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_004_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_007_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_009_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_010_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_013_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_014_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_015_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_11min_016_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)


write.csv(mean, sep="\t",file="data/ins_6min_001_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_003_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_004_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_007_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_009_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_010_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_013_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_014_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_015_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)
write.csv(mean, sep="\t",file="data/ins_6min_016_Area.csv", row.names=TRUE,col.names=NA,quote=FALSE)





a.c.0.1 <- read.csv(file="data/con_0min_001_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
view(a.c.0.1)
a.c.0.2 <- read.csv(file="data/con_0min_002_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.c.0.4 <- read.csv(file="data/con_0min_004_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.c.11.1 <- read.csv(file="data/con_11min_001_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.c.11.2 <- read.csv(file="data/con_11min_002_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.c.11.4 <- read.csv(file="data/con_11min_004_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.c.6.1 <- read.csv(file="data/con_6min_001_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.c.6.2 <- read.csv(file="data/con_6min_002_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.c.6.4 <- read.csv(file="data/con_6min_004_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 

a.i.0.1 <- read.csv(file="data/ins_0min_001_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.3 <- read.csv(file="data/ins_0min_003_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.4 <- read.csv(file="data/ins_0min_004_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.7 <- read.csv(file="data/ins_0min_007_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.9 <- read.csv(file="data/ins_0min_009_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.10 <- read.csv(file="data/ins_0min_010_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.13 <- read.csv(file="data/ins_0min_013_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.14 <- read.csv(file="data/ins_0min_014_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.15 <- read.csv(file="data/ins_0min_015_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.0.16 <- read.csv(file="data/ins_0min_016_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 

a.i.11.1 <- read.csv(file="data/ins_11min_001_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.11.3 <- read.csv(file="data/ins_11min_003_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.11.4 <- read.csv(file="data/ins_11min_004_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.11.7 <- read.csv(file="data/ins_11min_007_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])")
a.i.11.9 <- read.csv(file="data/ins_11min_009_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.11.10 <- read.csv(file="data/ins_11min_010_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.11.13 <- read.csv(file="data/ins_11min_013_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.11.14 <- read.csv(file="data/ins_11min_014_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.11.15 <- read.csv(file="data/ins_11min_015_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.11.16 <- read.csv(file="data/ins_11min_016_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 

a.i.6.1 <- read.csv(file="data/ins_6min_001_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.3 <- read.csv(file="data/ins_6min_003_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.4 <- read.csv(file="data/ins_6min_004_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.7 <- read.csv(file="data/ins_6min_007_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.9 <- read.csv(file="data/ins_6min_009_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.10 <- read.csv(file="data/ins_6min_010_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.13 <- read.csv(file="data/ins_6min_013_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.14 <- read.csv(file="data/ins_6min_014_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.15 <- read.csv(file="data/ins_6min_015_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 
a.i.6.16 <- read.csv(file="data/ins_6min_016_Area.csv", row.names=1) %>% rownames_to_column() %>% 
  separate(rowname,c(NA,"Track"),sep = "([#])") 

#======================================================================
d.c.0.1 <- t(read.csv(file="input/con_0_001_dc.csv", header = F )) dplyr::rename(Diffussion_coefficient=d.c.0.1) %>% mutate(cell="c101", group="control", timepoint="0min")
view(d.c.0.1)
d.c.0.2 <- t(read.csv(file="input/con_0_002_dc.csv", header = F ))
d.c.0.4 <- t(read.csv(file="input/con_0_004_dc.csv", header = F ))
d.c.11.1 <- t(read.csv(file="input/con_11_001_dc.csv", header = F ))
d.c.11.2 <- t(read.csv(file="input/con_11_002_dc.csv", header = F ))
d.c.11.4 <- t(read.csv(file="input/con_11_004_dc.csv", header = F ))
d.c.6.1 <- t(read.csv(file="input/con_6_001_dc.csv", header = F ))
d.c.6.2 <- t(read.csv(file="input/con_6_002_dc.csv", header = F ))
d.c.6.4 <- t(read.csv(file="input/con_6_004_dc.csv", header = F ))

d.i.0.1 <- t(read.csv(file="input/ins_0_001_dc.csv", header = F ))
d.i.0.3 <- t(read.csv(file="input/ins_0_003_dc.csv", header = F ))
d.i.0.4 <- t(read.csv(file="input/ins_0_004_dc.csv", header = F ))
d.i.0.7 <- t(read.csv(file="input/ins_0_007_dc.csv", header = F ))
d.i.0.9 <- t(read.csv(file="input/ins_0_009_dc.csv", header = F ))
d.i.0.10 <- t(read.csv(file="input/ins_0_010_dc.csv", header = F ))
d.i.0.13 <- t(read.csv(file="input/ins_0_013_dc.csv", header = F ))
d.i.0.14 <- t(read.csv(file="input/ins_0_014_dc.csv", header = F ))
d.i.0.15 <- t(read.csv(file="input/ins_0_015_dc.csv", header = F ))
d.i.0.16 <- t(read.csv(file="input/ins_0_016_dc.csv", header = F ))

d.i.11.1 <- t(read.csv(file="input/ins_11_001_dc.csv", header = F ))
d.i.11.3 <- t(read.csv(file="input/ins_11_003_dc.csv", header = F ))
d.i.11.4 <- t(read.csv(file="input/ins_11_004_dc.csv", header = F ))
d.i.11.7 <- t(read.csv(file="input/ins_11_007_dc.csv", header = F ))
d.i.11.9 <- t(read.csv(file="input/ins_11_009_dc.csv", header = F ))
d.i.11.10 <- t(read.csv(file="input/ins_11_010_dc.csv", header = F ))
d.i.11.13 <- t(read.csv(file="input/ins_11_013_dc.csv", header = F ))
d.i.11.14 <- t(read.csv(file="input/ins_11_014_dc.csv", header = F ))
d.i.11.15 <- t(read.csv(file="input/ins_11_015_dc.csv", header = F ))
d.i.11.16 <- t(read.csv(file="input/ins_11_016_dc.csv", header = F ))

d.i.6.1 <- t(read.csv(file="input/ins_6_001_dc.csv", header = F ))
d.i.6.3 <- t(read.csv(file="input/ins_6_003_dc.csv", header = F ))
d.i.6.4 <- t(read.csv(file="input/ins_6_004_dc.csv", header = F ))
d.i.6.7 <- t(read.csv(file="input/ins_6_007_dc.csv", header = F ))
d.i.6.9 <- t(read.csv(file="input/ins_6_009_dc.csv", header = F ))
d.i.6.10 <- t(read.csv(file="input/ins_6_010_dc.csv", header = F ))
d.i.6.13 <- t(read.csv(file="input/ins_6_013_dc.csv", header = F ))
d.i.6.14 <- t(read.csv(file="input/ins_6_014_dc.csv", header = F ))
d.i.6.15 <- t(read.csv(file="input/ins_6_015_dc.csv", header = F ))
d.i.6.16 <- t(read.csv(file="input/ins_6_016_dc.csv", header = F ))

#======================================================================
str(a.c.0.1)
str(a.c.0.1)
c.0min.101 <- read_excel("input/con_0min_001_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
   mutate(cell="c101", group="control", timepoint="0min")
c.0min.102 <- read_excel("input/con_0min_002_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
   mutate(cell="c102", group="control", timepoint="0min")
c.0min.104 <- read_excel("input/con_0min_004_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c104", group="control", timepoint="0min")
c.11min.101 <- read_excel("input/con_11min_001_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
   mutate(cell="c101", group="control", timepoint="11min")
c.11min.102 <- read_excel("input/con_11min_002_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c102", group="control", timepoint="11min")
c.11min.104 <- read_excel("input/con_11min_004_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
 mutate(cell="c104", group="control", timepoint="11min")
c.6min.101 <- read_excel("input/con_6min_001_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c101", group="control", timepoint="6min")
c.6min.102 <- read_excel("input/con_6min_002_Speed.xls")  %>% dplyr::rename('Track'='Track #') %>% 
   mutate(cell="c102", group="control", timepoint="6min") # dc  cbind(d.c.6.2) %>% dplyr::rename(Diffussion_coefficient=d.c.6.2)
c.6min.104 <- read_excel("input/con_6min_004_Speed.xls") %>% dplyr::rename('Track'='Track #')  %>% 
  mutate(cell="c104", group="control", timepoint="6min") # dc  cbind(d.c.6.4) %>% dplyr::rename(Diffussion_coefficient=d.c.6.4)
#----------
head(c.6min.101)
t.c.6min.102 <- read_excel("input/con_6min_002_Tracks.xls", col_names = F) 
t.c.6min.102 <- t.c.6min.102[!is.na(t.c.6min.102$...2),]
nrow(t.c.6min.102)
view(t.c.6min.102)
t.c.6min.104 <- read_excel("input/con_6min_004_Tracks.xls", col_names = F) 
t.c.6min.104 <- t.c.6min.104[!is.na(t.c.6min.104$...2),]
nrow(t.c.6min.104)
a <- read.csv("input/Stream-002.csv", header = F) #%>% filter(V1=="777")
view(a)
#----------
i.0min.001 <- read_excel("input/ins_0min_001_Speed.xls") %>% dplyr::rename('Track'='Track #')  %>% 
  mutate(cell="c001", group="insulin", timepoint="0min") # dc cbind(d.i.0.1) %>% dplyr::rename(Diffussion_coefficient=d.i.0.1)
i.0min.003 <- read_excel("input/ins_0min_003_Speed.xls") %>% dplyr::rename('Track'='Track #')  %>% 
  mutate(cell="c003", group="insulin", timepoint="0min") # dc cbind(d.i.0.3) %>% dplyr::rename(Diffussion_coefficient=d.i.0.3)
i.0min.004 <- read_excel("input/ins_0min_004_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c004", group="insulin", timepoint="0min")
i.0min.007 <- read_excel("input/ins_0min_007_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c007", group="insulin", timepoint="0min")
i.0min.009 <- read_excel("input/ins_0min_009_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c009", group="insulin", timepoint="0min")
i.0min.010 <- read_excel("input/ins_0min_010_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c010", group="insulin", timepoint="0min")
i.0min.013 <- read_excel("input/ins_0min_013_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c013", group="insulin", timepoint="0min")
i.0min.014 <- read_excel("input/ins_0min_014_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c014", group="insulin", timepoint="0min")
i.0min.015 <- read_excel("input/ins_0min_015_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c015", group="insulin", timepoint="0min")
#i.0min.016 <- read_excel("input/ins_0min_016_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c016", group="insulin", timepoint="0min") ## dc  cbind(d.i.0.16) %>% dplyr::rename(Diffussion_coefficient=d.i.0.16)

i.11min.001 <- read_excel("input/ins_11min_001_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
   mutate(cell="c001", group="insulin", timepoint="11min")
i.11min.003 <- read_excel("input/ins_11min_003_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
   mutate(cell="c003", group="insulin", timepoint="11min")
i.11min.004 <- read_excel("input/ins_11min_004_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c004", group="insulin", timepoint="11min")
i.11min.007 <- read_excel("input/ins_11min_007_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c007", group="insulin", timepoint="11min")
i.11min.009 <- read_excel("input/ins_11min_009_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c009", group="insulin", timepoint="11min")
i.11min.010 <- read_excel("input/ins_11min_010_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c010", group="insulin", timepoint="11min")
i.11min.013 <- read_excel("input/ins_11min_013_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c013", group="insulin", timepoint="11min")
i.11min.014 <- read_excel("input/ins_11min_014_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c014", group="insulin", timepoint="11min")
i.11min.015 <- read_excel("input/ins_11min_015_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c015", group="insulin", timepoint="11min")
#i.11min.016 <- read_excel("input/ins_11min_016_Speed.xls") %>% dplyr::rename('Track'='Track #')  %>% 
  mutate(cell="c016", group="insulin", timepoint="11min") # dc  cbind(d.i.11.16) %>% dplyr::rename(Diffussion_coefficient=d.i.11.16)

i.6min.001 <- read_excel("input/ins_6min_001_Speed.xls") %>% dplyr::rename('Track'='Track #')  %>% 
  mutate(cell="c001", group="insulin", timepoint="6min") # dc  cbind(d.i.6.1) %>% dplyr::rename(Diffussion_coefficient=d.i.6.1)
i.6min.003 <- read_excel("input/ins_6min_003_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c003", group="insulin", timepoint="6min")
i.6min.004 <- read_excel("input/ins_6min_004_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c004", group="insulin", timepoint="6min")
i.6min.007 <- read_excel("input/ins_6min_007_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c007", group="insulin", timepoint="6min")
i.6min.009 <- read_excel("input/ins_6min_009_Speed.xls") %>% dplyr::rename('Track'='Track #')  %>% 
  mutate(cell="c009", group="insulin", timepoint="6min") # dc cbind(d.i.6.9) %>% dplyr::rename(Diffussion_coefficient=d.i.6.9)
i.6min.010 <- read_excel("input/ins_6min_010_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c010", group="insulin", timepoint="6min")
i.6min.013 <- read_excel("input/ins_6min_013_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c013", group="insulin", timepoint="6min")
i.6min.014 <- read_excel("input/ins_6min_014_Speed.xls") %>% dplyr::rename('Track'='Track #') %>% 
  mutate(cell="c014", group="insulin", timepoint="6min") # dc  cbind(d.i.6.14) %>% dplyr::rename(Diffussion_coefficient=d.i.6.14)
i.6min.015 <- read_excel("input/ins_6min_015_Speed.xls") %>% dplyr::rename('Track'='Track #')  %>% 
  mutate(cell="c015", group="insulin", timepoint="6min") # dc  cbind(d.i.6.15) %>% dplyr::rename(Diffussion_coefficient=d.i.6.15)
#i.6min.016 <- read_excel("input/ins_6min_016_Speed.xls") %>% dplyr::rename('Track'='Track #')  %>% 
  mutate(cell="c016", group="insulin", timepoint="6min") ## dc cbind(d.i.6.16) %>% dplyr::rename(Diffussion_coefficient=d.i.6.16)


a.c.0.1$Track <- as.numeric(a.c.0.1$Track)
c.0min.101 <- c.0min.101 %>% left_join(a.c.0.1,by=c("Track"="Track")) 
a.c.0.2$Track <- as.numeric(a.c.0.2$Track)
c.0min.102 <- c.0min.102 %>% left_join(a.c.0.2,by=c("Track"="Track")) 
a.c.0.4$Track <- as.numeric(a.c.0.4$Track)
c.0min.104 <- c.0min.104 %>% left_join(a.c.0.4,by=c("Track"="Track")) 

a.c.11.1$Track <- as.numeric(a.c.11.1$Track)
c.11min.101 <- c.11min.101 %>% left_join(a.c.11.1,by=c("Track"="Track")) 
a.c.11.2$Track <- as.numeric(a.c.11.2$Track)
c.11min.102 <- c.11min.102 %>% left_join(a.c.11.2,by=c("Track"="Track")) 
a.c.11.4$Track <- as.numeric(a.c.11.4$Track)
c.11min.104 <- c.11min.104 %>% left_join(a.c.11.4,by=c("Track"="Track")) 

a.c.6.1$Track <- as.numeric(a.c.6.1$Track)
c.6min.101 <- c.6min.101 %>% left_join(a.c.6.1,by=c("Track"="Track")) 
a.c.6.2$Track <- as.numeric(a.c.6.2$Track)
c.6min.102 <- c.6min.102 %>% left_join(a.c.6.2,by=c("Track"="Track")) 
a.c.6.4$Track <- as.numeric(a.c.6.4$Track)
c.6min.104 <- c.6min.104 %>% left_join(a.c.6.4,by=c("Track"="Track")) 


a.i.0.1$Track <- as.numeric(a.i.0.1$Track)
i.0min.001 <- i.0min.001 %>% left_join(a.i.0.1,by=c("Track"="Track")) 
a.i.0.3$Track <- as.numeric(a.i.0.3$Track)
i.0min.003 <- i.0min.003 %>% left_join(a.i.0.3,by=c("Track"="Track")) 
a.i.0.4$Track <- as.numeric(a.i.0.4$Track)
i.0min.004 <- i.0min.004 %>% left_join(a.i.0.4,by=c("Track"="Track")) 
a.i.0.7$Track <- as.numeric(a.i.0.7$Track)
i.0min.007 <- i.0min.007 %>% left_join(a.i.0.7,by=c("Track"="Track")) 
a.i.0.9$Track <- as.numeric(a.i.0.9$Track)
i.0min.009 <- i.0min.009 %>% left_join(a.i.0.9,by=c("Track"="Track")) 
a.i.0.10$Track <- as.numeric(a.i.0.10$Track)
i.0min.010 <- i.0min.010 %>% left_join(a.i.0.10,by=c("Track"="Track")) 
a.i.0.13$Track <- as.numeric(a.i.0.13$Track)
i.0min.013 <- i.0min.013 %>% left_join(a.i.0.13,by=c("Track"="Track")) 
a.i.0.14$Track <- as.numeric(a.i.0.14$Track)
i.0min.014 <- i.0min.014 %>% left_join(a.i.0.14,by=c("Track"="Track")) 
a.i.0.15$Track <- as.numeric(a.i.0.15$Track)
i.0min.015 <- i.0min.015 %>% left_join(a.i.0.15,by=c("Track"="Track")) 
a.i.0.16$Track <- as.numeric(a.i.0.16$Track)
i.0min.016 <- i.0min.016 %>% left_join(a.i.0.16,by=c("Track"="Track")) 

a.i.11.1$Track <- as.numeric(a.i.11.1$Track)
i.11min.001 <- i.11min.001 %>% left_join(a.i.11.1,by=c("Track"="Track")) 
a.i.11.3$Track <- as.numeric(a.i.11.3$Track)
i.11min.003 <- i.11min.003 %>% left_join(a.i.11.3,by=c("Track"="Track")) 
a.i.11.4$Track <- as.numeric(a.i.11.4$Track)
i.11min.004 <- i.11min.004 %>% left_join(a.i.11.4,by=c("Track"="Track")) 
a.i.11.7$Track <- as.numeric(a.i.11.7$Track)
i.11min.007 <- i.11min.007 %>% left_join(a.i.11.7,by=c("Track"="Track")) 
a.i.11.9$Track <- as.numeric(a.i.11.9$Track)
i.11min.009 <- i.11min.009 %>% left_join(a.i.11.9,by=c("Track"="Track")) 
a.i.11.10$Track <- as.numeric(a.i.11.10$Track)
i.11min.010 <- i.11min.010 %>% left_join(a.i.11.10,by=c("Track"="Track")) 
a.i.11.13$Track <- as.numeric(a.i.11.13$Track)
i.11min.013 <- i.11min.013 %>% left_join(a.i.11.13,by=c("Track"="Track")) 
a.i.11.14$Track <- as.numeric(a.i.11.14$Track)
i.11min.014 <- i.11min.014 %>% left_join(a.i.11.14,by=c("Track"="Track")) 
a.i.11.15$Track <- as.numeric(a.i.11.15$Track)
i.11min.015 <- i.11min.015 %>% left_join(a.i.11.15,by=c("Track"="Track")) 
a.i.11.16$Track <- as.numeric(a.i.11.16$Track)
i.11min.016 <- i.11min.016 %>% left_join(a.i.11.16,by=c("Track"="Track")) 

a.i.6.1$Track <- as.numeric(a.i.6.1$Track)
i.6min.001 <- i.6min.001 %>% left_join(a.i.6.1,by=c("Track"="Track")) 
a.i.6.3$Track <- as.numeric(a.i.6.3$Track)
i.6min.003 <- i.6min.003 %>% left_join(a.i.6.3,by=c("Track"="Track")) 
a.i.6.4$Track <- as.numeric(a.i.6.4$Track)
i.6min.004 <- i.6min.004 %>% left_join(a.i.6.4,by=c("Track"="Track")) 
a.i.6.7$Track <- as.numeric(a.i.6.7$Track)
i.6min.007 <- i.6min.007 %>% left_join(a.i.6.7,by=c("Track"="Track")) 
a.i.6.9$Track <- as.numeric(a.i.6.9$Track)
i.6min.009 <- i.6min.009 %>% left_join(a.i.6.9,by=c("Track"="Track")) 
a.i.6.10$Track <- as.numeric(a.i.6.10$Track)
i.6min.010 <- i.6min.010 %>% left_join(a.i.6.10,by=c("Track"="Track")) 
a.i.6.13$Track <- as.numeric(a.i.6.13$Track)
i.6min.013 <- i.6min.013 %>% left_join(a.i.6.13,by=c("Track"="Track")) 
a.i.6.14$Track <- as.numeric(a.i.6.14$Track)
i.6min.014 <- i.6min.014 %>% left_join(a.i.6.14,by=c("Track"="Track")) 
a.i.6.15$Track <- as.numeric(a.i.6.15$Track)
i.6min.015 <- i.6min.015 %>% left_join(a.i.6.15,by=c("Track"="Track")) 
a.i.6.16$Track <- as.numeric(a.i.6.16$Track)
i.6min.016 <- i.6min.016 %>% left_join(a.i.6.16,by=c("Track"="Track")) 

view(i.0min.001)


allTracks <- rbind(c.0min.101,c.0min.102,c.0min.104,c.6min.101,c.6min.102,c.6min.104,c.11min.101,c.11min.102,c.11min.104,
                i.0min.001,i.0min.003,i.0min.004,i.0min.007,i.0min.009,i.0min.010,i.0min.013,i.0min.014,i.0min.015,
                i.6min.001,i.6min.003,i.6min.004,i.6min.007,i.6min.009,i.6min.010,i.6min.013,i.6min.014,i.6min.015,
                i.11min.001,i.11min.003,i.11min.004,i.11min.007,i.11min.009,i.11min.010,i.11min.013,i.11min.014,i.11min.015)
view(allTracks)
#allTracks$ID <- paste(allTracks$group,allTracks$timepoint,allTracks$cell,allTracks$Track,sep = "_")
colnames(allTracks)
allTracks <- allTracks %>% dplyr::rename(Start_frames='Start (frames)', End_frames='End (frames)',Duration_frames='Duration (frames)',
                                  Total_disp_px="Total disp. (px)",Net_disp_px="Net disp. (px)",Linearity="Linearity (%)",
                                  Search_radius_px="Search radius (px)",Min_disp_px="Min. disp. (px)",Max_disp_px="Max. disp. (px)",
                                  Avg_disp_px="Avg. disp. (px)")
allTracks$subgroup <- paste(allTracks$group,allTracks$timepoint,sep = "_")
allTracks <- allTracks %>% mutate(Lifetime=Duration_frames*0.2, Radius=Search_radius_px*0.129, Avg_displacement=Avg_disp_px*0.129)
 
write.csv(allTracks, sep="\t",file="data/allTracks_Speed.csv", row.names=TRUE,col.names=NA,quote=FALSE)

allTracks <- read.csv(file="data/allTracks_Speed_Area_diffusion.csv", row.names=1)

allTracks$subgroup<-factor(allTracks$subgroup,levels = c("control_0min","control_6min","control_11min","insulin_0min","insulin_6min","insulin_11min"))
allTracks$timepoint<-factor(allTracks$timepoint,levels = c("0min","6min","11min"))
allTracks$group<-factor(allTracks$group,levels = c("control","insulin"))

#---
meta.data$Glucose_log <- log10(meta.data$Glucose) #glucose in mM
# Shapiro-Wilk normality test for mpg
shapiro.test(allTracks$Duration_frames)
ggqqplot(meta.data$Insulin_pM_log, ylab = "Insulin")
#---
#=====================================
### Show cumulative frequency
allTracks.filter <- allTracks %>% filter(Duration_frames>4 #& 
                                          # Start_frames!=0 & 
                                           #End_frames!=499 & 
                                           #Max_disp_px<8 & 
                                           #Lifetime>5&Lifetime<50 &
                                         #group=="insulin"
                                         )
view(allTracks.filter)
nrow(allTracks)
nrow(allTracks.filter)
ins00 <- allTracks.filter[c(allTracks.filter$subgroup=="insulin_0min"),] %>%
  arrange(Duration_frames)  %>% mutate(freq=1)
ins00 <-  ins00 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins00))
view(ins00)
ins06 <- allTracks.filter[c(allTracks.filter$subgroup=="insulin_6min"),] %>%
  arrange(Duration_frames)  %>% mutate(freq=1)
ins06 <- ins06 %>%mutate(cum_frequency=cumsum(freq)/nrow(ins06))
view(ins06)
ins11 <- allTracks.filter[c(allTracks.filter$subgroup=="insulin_11min"),] %>%
  arrange(Duration_frames)  %>% mutate(freq=1)
ins11 <-  ins11 %>% mutate(cum_frequency=cumsum(freq/nrow(ins11)))
view(ins11)
con00 <- allTracks.filter[c(allTracks.filter$subgroup=="control_0min"),] %>%
  arrange(Duration_frames)  %>% mutate(freq=1)
con00 <-  con00 %>% mutate(cum_frequency=cumsum(freq)/nrow(con00))
con06 <- allTracks.filter[c(allTracks.filter$subgroup=="control_6min"),] %>%
  arrange(Duration_frames)  %>% mutate(freq=1)
con06 <- con06 %>% mutate(cum_frequency=cumsum(freq)/nrow(con06))
con11 <- allTracks.filter[c(allTracks.filter$subgroup=="control_11min"),] %>%
  arrange(Duration_frames)  %>% mutate(freq=1)
con11 <-  con11 %>% mutate(cum_frequency=cumsum(freq/nrow(con11)))


ks.test(x=ins06$Duration_frames, y=ins00$Duration_frames,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.040731, p-value = 2.944e-05

ks.test(x=ins11$Duration_frames, y=ins00$Duration_frames,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.069317, p-value = 5.84e-14
ks.test(x=ins11$Duration_frames, y=ins06$Duration_frames,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.041356, p-value = 3.876e-05

ks.test(x=con06$Duration_frames, y=con00$Duration_frames,
        alternative = c("two.sided"),
        exact = NULL) #D = 0.083335, p-value = 2.487e-08

ks.test(x=con11$Duration_frames, y=con00$Duration_frames,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.11228, p-value = 6.628e-14
ks.test(x=con11$Duration_frames, y=con06$Duration_frames,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.043968, p-value = 0.0257
#--------------
IR.inscon <- rbind(ins00,ins06,ins11,con00,con06,con11)

ggplot(IR.inscon, aes(x=Lifetime, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Lifetime (s)") + ylab("Cumulative probability") +theme_classic()+
  
#  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
#                labels = trans_format("log10", math_format(10^.x))) +
#  annotation_logticks(sides="b")+  # "lrtb"
#  coord_cartesian(xlim=c(10^(-0.1),10^2.1))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="figure/lifetime_inscon.png",width=4.5,height=3,units="in",dpi=600)


#--------------
ReplicateAverages <- IR.inscon %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

mcolors <- c(rainbow(13))

ggplot(IR.inscon, aes(x=subgroup,y=Lifetime,color=factor(cell))) + 
  labs(x=" ", y="Lifetime (s)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.2,size=0.5) + 
  #scale_colour_brewer(palette = "Set3",name="cells") + 
  scale_fill_manual(values = mcolors) + scale_color_discrete("cells") + # legend title
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  #scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
  #              labels = trans_format("log10", math_format(10^.x))) +
  #annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  guides(color = guide_legend(label.position = "right", ncol = 2)) +           #puts legend on right side of graph, ncol=10 [didn't seem to do anything - need to use 'color' instead of 'fill']
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        #legend.spacing.x = unit(0.01, "mm"),                                   #decreased space between labels in legend to save space - didn't do anything
        legend.spacing.y = unit(0.01, "cm")
        )     #4.5 x 3.5 in

ggsave(filename="figure/lifetime_inscon_super.png",width=6,height=3,units="in",dpi=600)


ReplicateAverages <- ReplicateAverages %>% separate(subgroup,c("Insulin","time"),sep = "([_])") %>% mutate(subgroup=paste(Insulin,time,sep = "_"))
library(rstatix)
aov <- ReplicateAverages %>% anova_test( Lifetime~ Insulin + time)
aov
stat<- ReplicateAverages %>% pairwise_t_test(Lifetime ~ subgroup, p.adjust.method = "BH") %>%
  add_significance()
stat
#==============================
# Track radius
ins00 <- allTracks.filter[c(allTracks.filter$subgroup=="insulin_0min"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
ins00 <-  ins00 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins00))
view(ins00)
ins06 <- allTracks.filter[c(allTracks.filter$subgroup=="insulin_6min"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
ins06 <- ins06 %>%mutate(cum_frequency=cumsum(freq)/nrow(ins06))
view(ins06)
ins11 <- allTracks.filter[c(allTracks.filter$subgroup=="insulin_11min"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
ins11 <-  ins11 %>% mutate(cum_frequency=cumsum(freq/nrow(ins11)))
view(ins11)
con00 <- allTracks.filter[c(allTracks.filter$subgroup=="control_0min"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
con00 <-  con00 %>% mutate(cum_frequency=cumsum(freq)/nrow(con00))
con06 <- allTracks.filter[c(allTracks.filter$subgroup=="control_6min"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
con06 <- con06 %>% mutate(cum_frequency=cumsum(freq)/nrow(con06))
con11 <- allTracks.filter[c(allTracks.filter$subgroup=="control_11min"),] %>%
  arrange(Radius)  %>% mutate(freq=1)
con11 <-  con11 %>% mutate(cum_frequency=cumsum(freq/nrow(con11)))

ks.test(x=ins06$Radius, y=ins00$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.033024, p-value = 0.001333

ks.test(x=ins11$Radius, y=ins00$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.063785, p-value = 6.923e-12
ks.test(x=ins11$Radius, y=ins06$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.036043, p-value = 0.0005267

ks.test(x=con06$Radius, y=con00$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.054212, p-value = 0.0009027

ks.test(x=con11$Radius, y=con00$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.080779, p-value = 2.109e-07
ks.test(x=con11$Radius, y=con06$Radius,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.044016, p-value = 0.02546
#--------------
IR.inscon <- rbind(ins00,ins06,ins11,con00,con06,con11)

ggplot(IR.inscon, aes(x=Radius, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Track radius (µm)") + ylab("Cumulative probability") +theme_classic()+
  
    scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                  labels = trans_format("log10", math_format(10^.x))) +
    annotation_logticks(sides="b")+  # "lrtb"
    coord_cartesian(xlim=c(10^(-1.5),10^1))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="figure/radius_inscon.png",width=4.5,height=3,units="in",dpi=600)


#------------------------------
ReplicateAverages <- IR.inscon %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)

#mcolors <- c(rainbow(13))

ggplot(IR.inscon, aes(x=subgroup,y=Radius,color=factor(cell))) + 
  labs(x=" ", y="Track radius (µm)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.2,size=0.5) + 
  #scale_colour_brewer(palette = "Set3",name="cells") + 
  scale_fill_manual(values = mcolors) + scale_color_discrete("cells") + # legend title
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  guides(color = guide_legend(label.position = "right", ncol = 2)) +           #puts legend on right side of graph, ncol=10 [didn't seem to do anything - need to use 'color' instead of 'fill']
  #guides(color = guide_legend(label.position = "top", nrow = 2)) +   
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        #legend.spacing.x = unit(0.01, "mm"),                                   #decreased space between labels in legend to save space - didn't do anything
        legend.spacing.y = unit(0.01, "cm")
  )     #4.5 x 3.5 in


ggsave(filename="figure/radius_inscon_super.png",width=6,height=3,units="in",dpi=600)


ReplicateAverages <- ReplicateAverages %>% separate(subgroup,c("Insulin","time"),sep = "([_])") %>% mutate(subgroup=paste(Insulin,time,sep = "_"))
library(rstatix)
aov <- ReplicateAverages %>% anova_test( Radius~ Insulin + time)
aov
stat<- ReplicateAverages %>% pairwise_t_test(Radius ~ subgroup, p.adjust.method = "BH") %>%
  add_significance()
stat

write.csv(ReplicateAverages, file = "ReplicateAverages_lifetime_trackradius.csv")
#===============================================
# diffusion coeff



d.c.0.1 <- as.data.frame(t(read.csv(file="input/con_0_001_dc.csv", header = F )))%>% mutate(cell="101", Insulin="control", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
view(d.c.0.1)
d.c.0.2 <- as.data.frame(t(read.csv(file="input/con_0_002_dc.csv", header = F )))%>% mutate(cell="102", Insulin="control", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.c.0.4 <- as.data.frame(t(read.csv(file="input/con_0_004_dc.csv", header = F )))%>% mutate(cell="104", Insulin="control", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.c.11.1 <- as.data.frame(t(read.csv(file="input/con_11_001_dc.csv", header = F )))%>% mutate(cell="101", Insulin="control", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.c.11.2 <- as.data.frame(t(read.csv(file="input/con_11_002_dc.csv", header = F )))%>% mutate(cell="102", Insulin="control", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.c.11.4 <- as.data.frame(t(read.csv(file="input/con_11_004_dc.csv", header = F )))%>% mutate(cell="104", Insulin="control", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.c.6.1 <- as.data.frame(t(read.csv(file="input/con_6_001_dc.csv", header = F )))%>% mutate(cell="101", Insulin="control", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.c.6.2 <- as.data.frame(t(read.csv(file="input/con_6_002_dc.csv", header = F )))%>% mutate(cell="102", Insulin="control", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.c.6.4 <- as.data.frame(t(read.csv(file="input/con_6_004_dc.csv", header = F )))%>% mutate(cell="104", Insulin="control", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")

d.i.0.1 <- as.data.frame(t(read.csv(file="input/ins_0_001_dc.csv", header = F )))%>% mutate(cell="001", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.0.3 <- as.data.frame(t(read.csv(file="input/ins_0_003_dc.csv", header = F )))%>% mutate(cell="003", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.0.4 <- as.data.frame(t(read.csv(file="input/ins_0_004_dc.csv", header = F )))%>% mutate(cell="004", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.0.7 <- as.data.frame(t(read.csv(file="input/ins_0_007_dc.csv", header = F )))%>% mutate(cell="007", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.0.9 <- as.data.frame(t(read.csv(file="input/ins_0_009_dc.csv", header = F )))%>% mutate(cell="009", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.0.10 <- as.data.frame(t(read.csv(file="input/ins_0_010_dc.csv", header = F )))%>% mutate(cell="010", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.0.13 <- as.data.frame(t(read.csv(file="input/ins_0_013_dc.csv", header = F )))%>% mutate(cell="013", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.0.14 <- as.data.frame(t(read.csv(file="input/ins_0_014_dc.csv", header = F )))%>% mutate(cell="014", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.0.15 <- as.data.frame(t(read.csv(file="input/ins_0_015_dc.csv", header = F )))%>% mutate(cell="015", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")
#d.i.0.16 <- as.data.frame(t(read.csv(file="input/ins_0_016_dc.csv", header = F )))%>% mutate(cell="016", Insulin="insulin", timepoint="0min")  %>% dplyr::rename("Diffusion_coeff"="V1")

d.i.11.1 <- as.data.frame(t(read.csv(file="input/ins_11_001_dc.csv", header = F )))%>% mutate(cell="001", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.11.3 <- as.data.frame(t(read.csv(file="input/ins_11_003_dc.csv", header = F )))%>% mutate(cell="003", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.11.4 <- as.data.frame(t(read.csv(file="input/ins_11_004_dc.csv", header = F )))%>% mutate(cell="004", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.11.7 <- as.data.frame(t(read.csv(file="input/ins_11_007_dc.csv", header = F )))%>% mutate(cell="007", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.11.9 <- as.data.frame(t(read.csv(file="input/ins_11_009_dc.csv", header = F )))%>% mutate(cell="009", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.11.10 <- as.data.frame(t(read.csv(file="input/ins_11_010_dc.csv", header = F )))%>% mutate(cell="010", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.11.13 <- as.data.frame(t(read.csv(file="input/ins_11_013_dc.csv", header = F )))%>% mutate(cell="013", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.11.14 <- as.data.frame(t(read.csv(file="input/ins_11_014_dc.csv", header = F )))%>% mutate(cell="014", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.11.15 <- as.data.frame(t(read.csv(file="input/ins_11_015_dc.csv", header = F )))%>% mutate(cell="015", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")
#d.i.11.16 <- as.data.frame(t(read.csv(file="input/ins_11_016_dc.csv", header = F )))%>% mutate(cell="016", Insulin="insulin", timepoint="11min")  %>% dplyr::rename("Diffusion_coeff"="V1")

d.i.6.1 <- as.data.frame(t(read.csv(file="input/ins_6_001_dc.csv", header = F )))%>% mutate(cell="001", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.6.3 <- as.data.frame(t(read.csv(file="input/ins_6_003_dc.csv", header = F )))%>% mutate(cell="003", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.6.4 <- as.data.frame(t(read.csv(file="input/ins_6_004_dc.csv", header = F )))%>% mutate(cell="004", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.6.7 <- as.data.frame(t(read.csv(file="input/ins_6_007_dc.csv", header = F )))%>% mutate(cell="007", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.6.9 <- as.data.frame(t(read.csv(file="input/ins_6_009_dc.csv", header = F )))%>% mutate(cell="009", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.6.10 <- as.data.frame(t(read.csv(file="input/ins_6_010_dc.csv", header = F )))%>% mutate(cell="010", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.6.13 <- as.data.frame(t(read.csv(file="input/ins_6_013_dc.csv", header = F )))%>% mutate(cell="013", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.6.14 <- as.data.frame(t(read.csv(file="input/ins_6_014_dc.csv", header = F )))%>% mutate(cell="014", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
d.i.6.15 <- as.data.frame(t(read.csv(file="input/ins_6_015_dc.csv", header = F )))%>% mutate(cell="015", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")
#d.i.6.16 <- as.data.frame(t(read.csv(file="input/ins_6_016_dc.csv", header = F )))%>% mutate(cell="016", Insulin="insulin", timepoint="6min")  %>% dplyr::rename("Diffusion_coeff"="V1")


allTracks.dc <- rbind(d.c.0.1,d.c.0.2,d.c.0.4,d.c.6.1,d.c.6.2,d.c.6.4,d.c.11.1,d.c.11.2,d.c.11.4,
                      d.i.0.1,d.i.0.3,d.i.0.4,d.i.0.7,d.i.0.9,d.i.0.10,d.i.0.13,d.i.0.14,d.i.0.15,
                      d.i.6.1,d.i.6.3,d.i.6.4,d.i.6.7,d.i.6.9,d.i.6.10,d.i.6.13,d.i.6.14,d.i.6.15,
                      d.i.11.1,d.i.11.3,d.i.11.4,d.i.11.7,d.i.11.9,d.i.11.10,d.i.11.13,d.i.11.14,d.i.11.15) %>% 
  mutate(subgroup=paste(Insulin,timepoint,sep="_"))
allTracks.dc$subgroup <- factor(allTracks.dc$subgroup,levels=c("control_0min", "control_6min", "control_11min", "insulin_0min","insulin_6min","insulin_11min"))
allTracks.dc$timepoint<-factor(allTracks.dc$timepoint,levels = c("0min","6min","11min"))
allTracks.dc$Insulin<-factor(allTracks.dc$Insulin,levels = c("control","insulin"))

head(allTracks.dc)
ins00 <- allTracks.dc[c(allTracks.dc$subgroup=="insulin_0min"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins00 <-  ins00 %>% mutate(cum_frequency=cumsum(freq)/nrow(ins00))
view(ins00)
ins06 <- allTracks.dc[c(allTracks.dc$subgroup=="insulin_6min"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins06 <- ins06 %>%mutate(cum_frequency=cumsum(freq)/nrow(ins06))

ins11 <- allTracks.dc[c(allTracks.dc$subgroup=="insulin_11min"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
ins11 <-  ins11 %>% mutate(cum_frequency=cumsum(freq/nrow(ins11)))

con00 <- allTracks.dc[c(allTracks.dc$subgroup=="control_0min"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
con00 <-  con00 %>% mutate(cum_frequency=cumsum(freq)/nrow(con00))
con06 <- allTracks.dc[c(allTracks.dc$subgroup=="control_6min"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
con06 <- con06 %>% mutate(cum_frequency=cumsum(freq)/nrow(con06))
con11 <- allTracks.dc[c(allTracks.dc$subgroup=="control_11min"),] %>%
  arrange(Diffusion_coeff)  %>% mutate(freq=1)
con11 <-  con11 %>% mutate(cum_frequency=cumsum(freq/nrow(con11)))

ks.test(x=ins06$Diffusion_coeff, y=ins00$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.025615, p-value = 0.02455

ks.test(x=ins11$Diffusion_coeff, y=ins00$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.033338, p-value = 0.00148
ks.test(x=ins11$Diffusion_coeff, y=ins06$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.021361, p-value = 0.1106

ks.test(x=con06$Diffusion_coeff, y=con00$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.03027, p-value = 0.181

ks.test(x=con11$Diffusion_coeff, y=con00$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) #D = 0.061765, p-value = 0.0001668
ks.test(x=con11$Diffusion_coeff, y=con06$Diffusion_coeff,
        alternative = c("two.sided"),
        exact = NULL) # D = 0.041788, p-value = 0.03915
#--------------
IR.inscon <- rbind(ins00,ins06,ins11,con00,con06,con11)
head(IR.inscon)
ggplot(IR.inscon, aes(x=Diffusion_coeff, y=cum_frequency,color= subgroup)) +
  geom_line() +  
  #geom_line(data=ins.2nM, color= "cyan")+
  xlab("Diffusion coefficient (µm2/s)") + ylab("Cumulative probability") +theme_classic()+
  
  scale_x_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="b")+  # "lrtb"
  coord_cartesian(xlim=c(10^(-1.6),10^1.3))+
  theme(legend.position="right",legend.title = element_blank(),aspect.ratio = 1/1.1)

ggsave(filename="figure/dc_inscon.png",width=4.5,height=3,units="in",dpi=600)

write.csv(IR.inscon,file = "data/diff.coeff.csv")
#------------------------------
ReplicateAverages <- IR.inscon %>% 
  group_by(subgroup, cell) %>%  
  summarise_each(funs(mean))
view(ReplicateAverages)
library(RColorBrewer)
# Define the number of colors you want
nb.cols <- 13
mycolors <- colorRampPalette(brewer.pal(12, "Set3"))(nb.cols)
mcolors <- c(rainbow(13))

ggplot(IR.inscon, aes(x=subgroup,y=Diffusion_coeff,color=factor(cell))) + 
  labs(x=" ", y="Diffusion coefficient (µm2/s)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.2,size=0.5) + 
  #scale_colour_brewer(palette = "Set3",name="cells") + 
  scale_fill_manual(values = mcolors) + scale_color_discrete("cells") + # legend title
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
  # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
  # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  scale_y_log10(breaks = trans_breaks("log10", function(x) 10^x),
                labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides="l")+  # "lrtb"
  # coord_cartesian(ylim=c(10^(-2),10^1.5))+
  theme_classic()+
  guides(color = guide_legend(label.position = "right", ncol = 2)) +           #puts legend on right side of graph, ncol=10 [didn't seem to do anything - need to use 'color' instead of 'fill']
  theme(axis.text.x=element_text(size=10,colour="black",angle = 45,hjust = 1), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10),
        #legend.spacing.x = unit(0.01, "mm"),                                   #decreased space between labels in legend to save space - didn't do anything
        legend.spacing.y = unit(0.01, "cm")
  )     #4.5 x 3.5 in



ggsave(filename="figure/dc_inscon_super.png",width=6,height=3,units="in",dpi=600)


ReplicateAverages <- ReplicateAverages %>% separate(subgroup,c("Insulin","time"),sep = "([_])") %>% mutate(subgroup=paste(Insulin,time,sep = "_"))
library(rstatix)
aov <- ReplicateAverages %>% anova_test(Diffusion_coeff ~ Insulin + time)
aov
stat<- ReplicateAverages %>% pairwise_t_test(Diffusion_coeff ~ subgroup, p.adjust.method = "BH") %>%
  add_significance()
stat
#stat<- stat %>% add_xy_position(x = "coexp.pair")
write.csv(ReplicateAverages,file = "ReplicateAverages_dc.csv")
#==============================
# https://www.datanovia.com/en/blog/how-to-perform-t-test-for-multiple-groups-in-r/
# +labs(subtitle = get_test_label(aov, detailed = TRUE),caption = get_pwc_label(stat))


#----

colnames(allTracks.filter)
allTracks.plot <- allTracks.ins %>% select(cell,timepoint, Lifetime)
view(allTracks.plot)
head(allTracks.plot)
str(allTracks.plot)
write.csv(allTracks.plot, sep="\t",file="data/allTracks_plotLifetime.csv", row.names=TRUE,col.names=NA,quote=FALSE)

ReplicateAverages <- allTracks.plot %>% 
  group_by(timepoint, cell) %>%  
  summarise_each(funs(mean))

install.packages("survival")
install.packages("lattice")
install.packages("ggplot2")
install.packages("Hmisc")
library("Hmisc")
p <- ggplot(allTracks.plot, aes(x=timepoint,y=Lifetime,color=factor(cell))) + 
  labs(x="Insulin", y="Lifetime(s)")+
  #geom_beeswarm(cex=0.1,alpha=0.2,groupOnX=TRUE,size=1) + 
  geom_jitter(position=position_jitter(0.2),alpha=0.2,size=1) + 
  scale_colour_brewer(palette = "Set3",name="cells") + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,aes(colour=factor(cell)), size=3) + 
  geom_beeswarm(data=ReplicateAverages,cex=1.5,shape = 1,size = 3,colour = "black")+
 # stat_compare_means(data=ReplicateAverages, comparisons = list(c("0nM", "2nM")), method="t.test", paired=FALSE) + 
 # stat_summary(fun.data=mean_sdl, fun.args = list(mult=1),  geom="errorbar", color="red", width=0.5)+
  theme_classic()+
  theme(axis.text.x=element_text(size=10,colour="black"), 
        axis.text.y=element_text(size=10,colour="black"),
        axis.title.x=element_text(size=10),
        axis.title.y=element_text(size=10)) #4.5 x 3.5 in
p

ggsave(p_age101,filename="p_age101.png",width=7,height=10,units="cm",dpi=600)