library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(ggh4x)
library(RColorBrewer)
library(forcats)
library(gridExtra)

dr_names <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","B162:E170")
ind_names <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","B162:E170")
dr_bf <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","P162:S170")
ind_bf <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","P162:S170")
dr_rbf <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AD162:AG170")
ind_rbf <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AD162:AG170")
dr_act <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AR162:AU170")
ind_act <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AR162:AU170")

all_dr <- na.omit(data.frame(as.vector(as.matrix(dr_names)),as.vector(as.matrix(dr_bf)),as.vector(as.matrix(dr_rbf)),as.vector(as.matrix(dr_act))))
all_ind <- na.omit(data.frame(as.vector(as.matrix(ind_names)),as.vector(as.matrix(ind_bf)),as.vector(as.matrix(ind_rbf)),as.vector(as.matrix(ind_act))))

colnames(all_dr) <- c('clones','BF','RBF','Activity')
colnames(all_ind) <- c('clones','BF','RBF','Activity')

# all_dr$clones <- factor(all_dr$clones, levels=c(all_dr$clones[c(1:27)]))
# all_ind$clones<- factor(all_ind$clones, levels=c(all_ind$clones[c(1:27)]))


all_dr$BF <- all_dr$BF*250
all_dr$RBF <- all_dr$RBF*250
all_ind$BF <- all_ind$BF*250
all_ind$RBF <- all_ind$RBF*250


all_dr <- pivot_longer(all_dr,cols=c('BF','RBF','Activity'),names_to = 'parameters', values_to = 'Values')
all_ind <- pivot_longer(all_ind,cols=c('BF','RBF','Activity'),names_to = 'parameters', values_to = 'Values')

CBH <- bind_rows(all_ind[grepl("^.[CS]C", all_ind$clones),],all_dr[grepl("^.[CS]C", all_dr$clones),])
GOX <- bind_rows(all_ind[grepl("^.[CS]G", all_ind$clones),],all_dr[grepl("^.[CS]G", all_dr$clones),])
NNNb <- bind_rows(all_ind[grepl("^.[CS]N", all_ind$clones),],all_dr[grepl("^.[CS]N", all_dr$clones),])
lipa <- bind_rows(all_ind[grepl("^.[CS][KL]", all_ind$clones),],all_dr[grepl("^.[CS][KL]", all_dr$clones),])

cbh_cols <- c('#F686A2','#F686A2','#F686A2',"#91399E","#91399E","#91399E","#77AE59","#77AE59","#77AE59","#116154","#116154","#116154")
lipa_cols <- c('#FF8AB6','#FF8AB6','#FF8AB6',"#BF71A7","#BF71A7","#BF71A7","#8E52AA","#8E52AA","#8E52AA","#80B26D","#80B26D","#80B26D","#40995E","#40995E","#40995E","#0F7A60","#0F7A60","#0F7A60")
gox_cols <- c('#E37FA3','#E37FA3','#E37FA3',"#922493","#922493","#922493","#64A75A","#64A75A","#64A75A","#134C4A","#134C4A","#134C4A")
nnnb_cols <- c("#A065AC","#A065AC","#A065AC","#841876","#841876","#841876","#218D62","#218D62","#218D62","#04412C","#04412C","#04412C")
p_cbh<-
  ggplot(CBH, aes(x=clones,y=Values, fill=parameters)) + 
  geom_col(position='dodge')+
  scale_y_continuous(sec.axis = sec_axis(~./250, name='Bradford values'), name='Activity values')+
  theme(axis.text.y = element_text(color = cbh_cols),legend.position='none') +
  coord_flip()+
  labs(title='CBH2 clone analysis')


p_lipa<-
  ggplot(lipa, aes(x=clones,y=Values, fill=parameters)) + 
  geom_col(position='dodge')+
  scale_y_continuous(sec.axis = sec_axis(~./250, name='Bradford values'), name='Activity values')+
  theme(axis.text.y = element_text(color = lipa_cols),legend.position='none')+
  coord_flip()+
  labs(title='lipase clone analysis', x=NULL)


p_gox<-
  ggplot(GOX, aes(x=clones,y=Values, fill=parameters)) + 
  geom_col(position='dodge')+
  scale_y_continuous(sec.axis = sec_axis(~./250, name='Bradford values'), name='Activity values')+
  theme(axis.text.y = element_text(color = gox_cols), legend.position='none')+
  coord_flip()+
  labs(title='GOX1 clone analysis', x=NULL)


p_nnnb<-
  ggplot(NNNb, aes(x=clones,y=Values, fill=parameters)) + 
  geom_col(position='dodge')+
  scale_y_continuous(sec.axis = sec_axis(~./250, name='Bradford values'), name='Activity values')+
  theme(axis.text.y = element_text(color = nnnb_cols), legend.position='none')+
  coord_flip()+
  labs(title='nanobody clone analysis', x=NULL)

grid.arrange(p_cbh, p_lipa, p_gox, p_nnnb, ncol=4)


# all_dr[grepl("^.C", all_dr$clones),]


