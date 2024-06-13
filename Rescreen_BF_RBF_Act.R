library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(hash)
library(gridExtra)

DR_BF <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/Rescreen.xlsx","OD","O31:AA39")
IND_BF <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/Rescreen.xlsx","OD","O41:AA49")
DR_RBF <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/Rescreen.xlsx","OD","AC1:AO9")
IND_RBF <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/Rescreen.xlsx","OD","AC11:AO19")
DR_Act <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/Rescreen.xlsx","OD","AQ1:BC9")
IND_Act <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/Rescreen.xlsx","OD","AQ11:BC19")
DR_names <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/Rescreen.xlsx","OD","A21:M29")
IND_names <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/Rescreen.xlsx","OD","A31:M39")

DR_BF <- subset(DR_BF, select = -c(1))
IND_BF <- subset(IND_BF, select = -c(1))
DR_RBF <- subset(DR_RBF, select = -c(1))
IND_RBF <- subset(IND_RBF, select = -c(1))
DR_Act <- subset(DR_Act, select = -c(1))
IND_Act <- subset(IND_Act, select = -c(1))
DR_names <- subset(DR_names, select = -c(1))
IND_names <- subset(IND_names, select = -c(1))


DR_names <- as.vector(as.matrix(DR_names))
DR_BF <- as.vector(as.matrix(DR_BF))
DR_RBF <- as.vector(as.matrix(DR_RBF))
DR_Act <- as.vector(as.matrix(DR_Act))
IND_names <- as.vector(as.matrix(IND_names))
IND_BF <- as.vector(as.matrix(IND_BF))
IND_RBF <- as.vector(as.matrix(IND_RBF))
IND_Act <- as.vector(as.matrix(IND_Act))

dict_DR_BF <- hash()
dict_DR_BF[DR_names] <- replace_na(DR_BF,0)
dict_DR_RBF <- hash()
dict_DR_RBF[DR_names] <- replace_na(DR_RBF,0)
dict_DR_Act <- hash()
dict_DR_Act[DR_names] <- replace_na(DR_Act,0)
dict_IND_BF <- hash()
dict_IND_BF[IND_names] <- replace_na(IND_BF,0)
dict_IND_RBF <- hash()
dict_IND_RBF[IND_names] <- replace_na(IND_RBF,0)
dict_IND_Act <- hash()
dict_IND_Act[IND_names] <- replace_na(IND_Act,0)

meta_dict=c('dict_DR_BF','dict_DR_RBF','dict_DR_Act','dict_IND_BF','dict_IND_RBF','dict_IND_Act')
real_key=c('dict_DR_BF','dict_IND_BF')

clones_DR <- unique(as.vector(str_match(keys(get(real_key[1])),'^.+\\s.+\\s.?[A-Z][\\d]+')))
clones_IND <- unique(as.vector(str_match(keys(get(real_key[2])),'^.+\\s.+\\s.?[A-Z][\\d]+')))
clones_DR <- clones_DR[!is.na(clones_DR)]
clones_IND <- clones_IND[!is.na(clones_IND)]

der <- c()
for (i in clones_DR){
  p <- i
  for (j in meta_dict[1:3]){
    i <- as.vector(i)
    i <- append(i, values(get(j),na.omit(str_extract(keys(get(j)),paste0(p,'.+')))))
  }
  der <- append(der,i)
}

ind <- c()
for (i in clones_IND){
  p <- i
  for (j in meta_dict[4:6]){
    i <- as.vector(i)
    i <- append(i, values(get(j),na.omit(str_extract(keys(get(j)),paste0(p,'.+')))))
  }
  ind <- append(ind,i)
}

clones_dr <- c()
means_bf_dr <- c()
sd_bf_dr <- c()
means_rbf_dr <- c()
sd_rbf_dr <- c()
means_act_dr <- c()
sd_act_dr <- c()

clones_ind <- c()
means_bf_ind <- c()
sd_bf_ind <- c()
means_rbf_ind <- c()
sd_rbf_ind <- c()
means_act_ind <- c()
sd_act_ind <- c()

for (i in der){
  if(str_starts(i,'P')){
    clones_dr <- append(clones_dr,paste0('D',gsub('PDC','C',gsub('PDES','S',gsub('CalA','K',gsub('CBH2','C',gsub('GOX','G',gsub('Nb','N',gsub('LipT','L',gsub(' ','',i))))))))))
    means_bf_dr <- append(means_bf_dr,mean(c(as.numeric(der[which(i==der)+1]),as.numeric(der[which(i==der)+2]),as.numeric(der[which(i==der)+3]))))
    means_rbf_dr <- append(means_rbf_dr,mean(c(as.numeric(der[which(i==der)+4]),as.numeric(der[which(i==der)+5]),as.numeric(der[which(i==der)+6]))))
    means_act_dr <- append(means_act_dr,mean(c(as.numeric(der[which(i==der)+7]),as.numeric(der[which(i==der)+8]),as.numeric(der[which(i==der)+9]))))
    sd_bf_dr <- append(sd_bf_dr,sd(c(as.numeric(der[which(i==der)+1]),as.numeric(der[which(i==der)+2]),as.numeric(der[which(i==der)+3]))))
    sd_rbf_dr <- append(sd_rbf_dr,sd(c(as.numeric(der[which(i==der)+4]),as.numeric(der[which(i==der)+5]),as.numeric(der[which(i==der)+6]))))
    sd_act_dr <- append(sd_act_dr,sd(c(as.numeric(der[which(i==der)+7]),as.numeric(der[which(i==der)+8]),as.numeric(der[which(i==der)+9]))))
  }
}

for (i in ind){
  if(str_starts(i,'P')){
    clones_ind <- append(clones_ind,paste0('I',gsub('PDC','C',gsub('PDES','S',gsub('CalA','K',gsub('CBH2','C',gsub('GOX','G',gsub('Nb','N',gsub('LipT','L',gsub(' ','',i))))))))))
    means_bf_ind <- append(means_bf_ind,mean(c(as.numeric(ind[which(i==ind)+1]),as.numeric(ind[which(i==ind)+2]),as.numeric(ind[which(i==ind)+3]))))
    means_rbf_ind <- append(means_rbf_ind,mean(c(as.numeric(ind[which(i==ind)+4]),as.numeric(ind[which(i==ind)+5]),as.numeric(ind[which(i==ind)+6]))))
    means_act_ind <- append(means_act_ind,mean(c(as.numeric(ind[which(i==ind)+7]),as.numeric(ind[which(i==ind)+8]),as.numeric(ind[which(i==ind)+9]))))
    sd_bf_ind <- append(sd_bf_ind,sd(c(as.numeric(ind[which(i==ind)+1]),as.numeric(ind[which(i==ind)+2]),as.numeric(ind[which(i==ind)+3]))))
    sd_rbf_ind <- append(sd_rbf_ind,sd(c(as.numeric(ind[which(i==ind)+4]),as.numeric(ind[which(i==ind)+5]),as.numeric(ind[which(i==ind)+6]))))
    sd_act_ind <- append(sd_act_ind,sd(c(as.numeric(ind[which(i==ind)+7]),as.numeric(ind[which(i==ind)+8]),as.numeric(ind[which(i==ind)+9]))))
  }
}

derepression_m <- data.frame(clones_dr,means_bf_dr,means_rbf_dr,means_act_dr)
induction_m <- data.frame(clones_ind,means_bf_ind,means_rbf_ind,means_act_ind)

derepression_sd <- data.frame(clones_dr,sd_bf_dr,sd_rbf_dr,sd_act_dr)
induction_sd <- data.frame(clones_ind,sd_bf_ind,sd_rbf_ind,sd_act_ind)

colnames(derepression_m) <- c('clones','mean_BF','mean_RBF','mean_Activity')
colnames(induction_m) <- c('clones','mean_BF','mean_RBF','mean_Activity')
colnames(derepression_sd) <- c('clones','sd_BF','sd_RBF','sd_Activity')
colnames(induction_sd) <- c('clones','sd_BF','sd_RBF','sd_Activity')

# derepression_m$clones <- factor(derepression_m$clones, levels=c(derepression_m$clones[c(1:27)]))
# induction_m$clones<- factor(induction_m$clones, levels=c(induction_m$clones[c(1:27)]))
# derepression_sd$clones <- factor(derepression_sd$clones, levels=c(derepression_sd$clones[c(1:27)]))
# induction_sd$clones<- factor(induction_sd$clones, levels=c(induction_sd$clones[c(1:27)]))

derepression_sd$sd_BF <- derepression_sd$sd_BF*250
derepression_sd$sd_RBF <- derepression_sd$sd_RBF*250
induction_sd$sd_BF <- induction_sd$sd_BF*250
induction_sd$sd_RBF <- induction_sd$sd_RBF*250

derepression_m$mean_BF <- derepression_m$mean_BF*250
derepression_m$mean_RBF <- derepression_m$mean_RBF*250
induction_m$mean_BF <- induction_m$mean_BF*250
induction_m$mean_RBF <- induction_m$mean_RBF*250

derepression_m <- pivot_longer(derepression_m,cols=c('mean_BF','mean_RBF','mean_Activity'),names_to = 'means', values_to = 'Average')
induction_m <- pivot_longer(induction_m,cols=c('mean_BF','mean_RBF','mean_Activity'),names_to = 'means', values_to = 'Average')
derepression_sd <- pivot_longer(derepression_sd,cols=c('sd_BF','sd_RBF','sd_Activity'),names_to = 'standard deviation', values_to = 'SD')
induction_sd <- pivot_longer(induction_sd,cols=c('sd_BF','sd_RBF','sd_Activity'),names_to = 'standard deviation', values_to = 'SD')

derepression<-cbind(derepression_m,derepression_sd[!names(derepression_m) %in% names(derepression_sd)])
induction<-cbind(induction_m,induction_sd[!names(induction_m) %in% names(induction_sd)])


CBH <- bind_rows(induction[grepl("^.[CS]C", induction$clones),],derepression[grepl("^.[CS]C", derepression$clones),])
GOX <- bind_rows(induction[grepl("^.[CS]G", induction$clones),],derepression[grepl("^.[CS]G", derepression$clones),])
NNNb <- bind_rows(induction[grepl("^.[CS]N", induction$clones),],derepression[grepl("^.[CS]N", derepression$clones),])
lipa <- bind_rows(induction[grepl("^.[CS][KL]", induction$clones),],derepression[grepl("^.[CS][KL]", derepression$clones),])

cbh_cols <- c('#F686A2','#F686A2','#F686A2',"#91399E","#91399E","#91399E","#77AE59","#77AE59","#77AE59","#116154","#116154","#116154")
lipa_cols <- c('#FF8AB6','#FF8AB6','#FF8AB6',"#BF71A7","#BF71A7","#BF71A7","#8E52AA","#8E52AA","#8E52AA","#80B26D","#80B26D","#80B26D","#40995E","#40995E","#40995E","#0F7A60","#0F7A60","#0F7A60")
gox_cols <- c('#E37FA3','#E37FA3','#E37FA3',"#922493","#922493","#922493","#64A75A","#64A75A","#64A75A","#134C4A","#134C4A","#134C4A")
nnnb_cols <- c("#A065AC","#A065AC","#A065AC","#841876","#841876","#841876","#218D62","#218D62","#218D62","#04412C","#04412C","#04412C")

p_cbh <- ggplot(CBH,aes(x=clones,y=Average, fill=means)) +
  geom_col(position='dodge')+
  scale_y_continuous(sec.axis = sec_axis(~./250, name='Bradford values'), name='Activity values')+
  geom_errorbar(aes(x=clones, ymin=Average-SD, ymax=Average+SD), position='dodge')+
  theme(axis.text.y = element_text(color = cbh_cols),legend.position='none') +
  coord_flip()+
  labs(title='CBH2 clone analysis')

p_lipa <- ggplot(lipa,aes(x=clones,y=Average, fill=means)) +
  geom_col(position='dodge')+
  scale_y_continuous(sec.axis = sec_axis(~./250, name='Bradford values'), name='Activity values')+
  geom_errorbar(aes(x=clones, ymin=Average-SD, ymax=Average+SD), position='dodge')+
  theme(axis.text.y = element_text(color = lipa_cols),legend.position='none') +
  coord_flip()+
  labs(title='lipase clone analysis', x=NULL)

p_gox <- ggplot(GOX,aes(x=clones,y=Average, fill=means)) +
  geom_col(position='dodge')+
  scale_y_continuous(sec.axis = sec_axis(~./250, name='Bradford values'), name='Activity values')+
  geom_errorbar(aes(x=clones, ymin=Average-SD, ymax=Average+SD), position='dodge')+
  theme(axis.text.y = element_text(color = gox_cols),legend.position='none') +
  coord_flip()+
  labs(title='GOX1 clone analysis', x=NULL)

p_nnnb <- ggplot(NNNb,aes(x=clones,y=Average, fill=means)) +
  geom_col(position='dodge')+
  scale_y_continuous(sec.axis = sec_axis(~./250, name='Bradford values'), name='Activity values')+
  geom_errorbar(aes(x=clones, ymin=Average-SD, ymax=Average+SD), position='dodge')+
  theme(axis.text.y = element_text(color = nnnb_cols),legend.position='none') +
  coord_flip()+
  labs(title='nanobody clone analysis', x=NULL)

grid.arrange(p_cbh, p_lipa, p_gox, p_nnnb, ncol=4)

