library(readxl)
library(ggplot2)
library(tidyverse)
library(stringr)
dr_des_cal <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC82:AN90")
dr_des_gox_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC22:AN30")
dr_des_gox_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC32:AN40")
dr_des_cbh_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC42:AN50")
dr_des_cbh_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC52:AN60")
dr_des_nb_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC62:AN70")
dr_des_nb_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC72:AN80")
dr_dc_lip <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC92:AN100")
dr_dc_cal_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC102:AN110")
dr_dc_cal_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC112:AN120")
dr_dc_gox_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC2:AN10")
dr_dc_gox_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC12:AN20")
dr_dc_nb_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC122:AN130")
dr_dc_nb_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC132:AN140")
dr_dc_cbh_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC142:AN150")
dr_dc_cbh_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","DR","AC152:AN160")
ind_des_cal <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC2:AN10")
ind_des_gox_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC12:AN20")
ind_des_gox_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC22:AN30")
ind_des_cbh_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC32:AN40")
ind_des_cbh_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC42:AN50")
ind_des_nb_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC52:AN60")
ind_des_nb_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC62:AN70")
ind_dc_lip <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC72:AN80")
ind_dc_cal_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC82:AN90")
ind_dc_cal_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC92:AN100")
ind_dc_gox_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC102:AN110")
ind_dc_gox_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC112:AN120")
ind_dc_nb_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC122:AN130")
ind_dc_nb_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC132:AN140")
ind_dc_cbh_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC142:AN150")
ind_dc_cbh_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","IND","AC152:AN160")


dr_dc_cal <- merge(dr_dc_cal_1, dr_dc_cal_2, all = TRUE)
dr_dc_gox <- merge(dr_dc_gox_1, dr_dc_gox_2, all = TRUE)
dr_dc_nb <- merge(dr_dc_nb_1, dr_dc_nb_2, all = TRUE)
dr_dc_cbh <- merge(dr_dc_cbh_1, dr_dc_cbh_2, all = TRUE)
dr_des_gox <- merge(dr_des_gox_1, dr_des_gox_2, all = TRUE)
dr_des_nb <- merge(dr_des_nb_1, dr_des_nb_2, all = TRUE)
dr_des_cbh <- merge(dr_des_cbh_1, dr_des_cbh_2, all = TRUE)


ind_dc_cal <- merge(ind_dc_cal_1, ind_dc_cal_2, all = TRUE)
ind_dc_gox <- merge(ind_dc_gox_1, ind_dc_gox_2, all = TRUE)
ind_dc_nb <- merge(ind_dc_nb_1, ind_dc_nb_2, all = TRUE)
ind_dc_cbh <- merge(ind_dc_cbh_1, ind_dc_cbh_2, all = TRUE)
ind_des_gox <- merge(ind_des_gox_1, ind_des_gox_2, all = TRUE)
ind_des_nb <- merge(ind_des_nb_1, ind_des_nb_2, all = TRUE)
ind_des_cbh <- merge(ind_des_cbh_1, ind_des_cbh_2, all = TRUE)

dr_dc_cal <- subset(dr_dc_cal, select = -c(12, 13))
dr_dc_lip <- subset(dr_dc_lip, select = -c(1))
dr_dc_gox <- subset(dr_dc_gox, select = -c(12, 13))
dr_dc_nb <- subset(dr_dc_nb, select = -c(12, 13))
dr_dc_cbh <- subset(dr_dc_cbh, select = -c(12, 13))
dr_des_cal <- subset(dr_des_cal, select = -c(1))
dr_des_gox <- subset(dr_des_gox, select = -c(12, 13))
dr_des_nb <- subset(dr_des_nb, select = -c(12, 13))
dr_des_cbh <- subset(dr_des_cbh, select = -c(12, 13))

ind_dc_cal <- subset(ind_dc_cal, select = -c(12, 13))
ind_dc_lip <- subset(ind_dc_lip, select = -c(1))
ind_dc_gox <- subset(ind_dc_gox, select = -c(12, 13))
ind_dc_nb <- subset(ind_dc_nb, select = -c(12, 13))
ind_dc_cbh <- subset(ind_dc_cbh, select = -c(12, 13))
ind_des_cal <- subset(ind_des_cal, select = -c(1))
ind_des_gox <- subset(ind_des_gox, select = -c(12, 13))
ind_des_nb <- subset(ind_des_nb, select = -c(12, 13))
ind_des_cbh <- subset(ind_des_cbh, select = -c(12, 13))

PDC_Cal_DR <- as.vector(as.matrix(dr_dc_cal))
class(PDC_Cal_DR) <- "numeric"
PDC_Lip_DR <- as.vector(as.matrix(dr_dc_lip))
PDC_Gox_DR <- as.vector(as.matrix(dr_dc_gox))
PDC_Nb_DR <- as.vector(as.matrix(dr_dc_nb))
PDC_Cbh_DR <- as.vector(as.matrix(dr_dc_cbh))
PDES_Gox_DR <- as.vector(as.matrix(dr_des_gox))
PDES_Cal_DR <- as.vector(as.matrix(dr_des_cal))
PDES_Nb_DR <- as.vector(as.matrix(dr_des_nb))
PDES_Cbh_DR <- as.vector(as.matrix(dr_des_cbh))

PDC_Cal_IND <- as.vector(as.matrix(ind_dc_cal))
class(PDC_Cal_IND) <- "numeric"
PDC_Lip_IND <- as.vector(as.matrix(ind_dc_lip))
PDC_Gox_IND <- as.vector(as.matrix(ind_dc_gox))
PDC_Nb_IND <- as.vector(as.matrix(ind_dc_nb))
PDC_Cbh_IND <- as.vector(as.matrix(ind_dc_cbh))
PDES_Gox_IND <- as.vector(as.matrix(ind_des_gox))
PDES_Cal_IND <- as.vector(as.matrix(ind_des_cal))
PDES_Nb_IND <- as.vector(as.matrix(ind_des_nb))
PDES_Cbh_IND <- as.vector(as.matrix(ind_des_cbh))

dict=c('PDC_Cal_DR',
 'PDC_Lip_DR',
 'PDC_Gox_DR',
 'PDC_Cbh_DR',
 'PDC_Nb_DR',
 'PDES_Cal_DR',
 'PDES_Gox_DR',
 'PDES_Cbh_DR',
 'PDES_Nb_DR',
 'PDC_Cal_IND',
 'PDC_Lip_IND',
 'PDC_Gox_IND',
 'PDC_Cbh_IND',
 'PDC_Nb_IND',
 'PDES_Cal_IND',
 'PDES_Gox_IND',
 'PDES_Cbh_IND',
 'PDES_Nb_IND')
na.omit(str_extract(dict,regex('PDC_Lip.+')))
induced<-c()
for (i in str_extract(dict,regex('.+_.+_.+'))) {
  for(j in str_extract(dict,regex('.+_.+_.+'))){
    if (str_detect(i, regex('.+_.+_[^D]')) & str_detect(j, regex('.+_.+_[^D]'))){
     if(str_extract(i, regex('_.+_')) == str_extract(j, regex('_.+_')) & str_extract(i, regex('._.+_')) != str_extract(j, regex('._.+_'))){
       if (i %in% induced){
         break
       }
       print(i)
       induced=append(induced,i)
       induced=append(induced,j)
     }
    }
  }
}

derepressed<-c()
for (i in str_extract(dict,regex('.+_.+_.+'))) {
  for(j in str_extract(dict,regex('.+_.+_.+'))){
    if (str_detect(i, regex('.+_.+_[^I]')) & str_detect(j, regex('.+_.+_[^I]'))){
      if(str_extract(i, regex('_.+_')) == str_extract(j, regex('_.+_')) & str_extract(i, regex('._.+_')) != str_extract(j, regex('._.+_'))){
        if (i %in% derepressed){
          break
        }
        print(i)
        derepressed=append(derepressed,i)
        derepressed=append(derepressed,j)
      }
    }
  }
}

induced <- append(induced,'PDC_Lip_IND')
derepressed <- append(derepressed,'PDC_Lip_DR')


induced <- sort(induced)
derepressed <- sort(derepressed)

promoters <- c()
treat <- c()
p_signif <- c()
t_signif <- c()


for (i in seq(1,length(induced),1)){
  treat <- append(treat,wilcox.test(get(induced[i]),get(derepressed[i]),paired=TRUE)$p.value)
  if (wilcox.test(get(induced[i]),get(derepressed[i]),paired=TRUE)$p.value < 0.001){
    t_signif <- append(t_signif,'***')
  } else if (wilcox.test(get(induced[i]),get(derepressed[i]),paired=TRUE)$p.value < 0.005){
    t_signif <- append(t_signif,'**')
  } else if (wilcox.test(get(induced[i]),get(derepressed[i]),paired=TRUE)$p.value < 0.01){
    t_signif <- append(t_signif,'*')
  } else {
    t_signif <- append(t_signif,'')
  }
}

constructs <- c()
treatment <- c()
for (i in str_split(dict,'_')) {
  for (k in i){
    if (str_starts(k,'P')) {
      for (j in 1:176){
        #print(paste(k,i[which(i==k)+1]))
        constructs <- append(constructs,paste(k,i[which(i==k)+1],sep='_'))
        treatment <- append(treatment,i[which(i==k)+2])
      }
    }
  }
}

RBF_values <- c(PDC_Cal_DR, PDC_Lip_DR, PDC_Lip_DR, PDC_Gox_DR, PDC_Cbh_DR, PDC_Nb_DR, PDES_Cal_DR, PDES_Cal_DR, PDES_Gox_DR, PDES_Cbh_DR, PDES_Nb_DR, PDC_Cal_IND, PDC_Lip_IND, PDC_Lip_IND, PDC_Gox_IND, PDC_Cbh_IND, PDC_Nb_IND, PDES_Cal_IND, PDES_Cal_IND, PDES_Gox_IND, PDES_Cbh_IND, PDES_Nb_IND)
df <- data.frame(constructs,treatment,RBF_values)
df$constructs <- as.factor(df$constructs)
df$treatment <- as.factor(df$treatment)
df$RBF_values <- replace_na(df$RBF_values,0)
df <- as.data.frame(df)
inc <- 1
x<-c()
y<-c()
id<-rep(c(1:176),2)
ck <- cbind(data.frame(df %>% filter(constructs == 'PDC_Cal')),id)
cl <- cbind(data.frame(df %>% filter(constructs == 'PDC_Lip')),id)
cg <- cbind(data.frame(df %>% filter(constructs == 'PDC_Gox')),id)
cc <- cbind(data.frame(df %>% filter(constructs == 'PDC_Cbh')),id)
cn <- cbind(data.frame(df %>% filter(constructs == 'PDC_Nb')),id)
sk <- cbind(data.frame(df %>% filter(constructs == 'PDES_Cal')),id)
sg <- cbind(data.frame(df %>% filter(constructs == 'PDES_Gox')),id)
sc <- cbind(data.frame(df %>% filter(constructs == 'PDES_Cbh')),id)
sn <- cbind(data.frame(df %>% filter(constructs == 'PDES_Nb')),id)

colect <- c('cc','cg','ck','cl','cn','sc','sg','sk','sn')
p <- ggplot(df,aes(x=constructs,y=RBF_values))
for (i in colect) {
  p <-  p + geom_boxplot(data = get(i) %>% filter(treatment == 'DR'), width = 0.3, fill='#FF7A70', position = position_nudge(x = -0.3)) +
  geom_boxplot(data = get(i) %>% filter(treatment == 'IND'), width = 0.3, fill='#00BFC4',position = position_nudge(x = 0.3)) +
  geom_point(data = get(i) %>% filter(treatment == 'DR'), position = position_nudge(x = -0.1), color='#FF7A70') +
  geom_point(data = get(i) %>% filter(treatment == 'IND'), position = position_nudge(x = 0.1), color='#00BFC4') +
  geom_line(alpha=0.05, aes(group = interaction(constructs,id)), position = position_nudge(x=c(-0.1,0.1)))
}
  
for (i in 1:9){
  y<- append(y,quantile(get(induced[i]),0.75,na.rm=TRUE)+1.5*(quantile(get(induced[i]),0.75,na.rm=TRUE)-quantile(get(induced[i]),0.25,na.rm=TRUE)))
  x<- append(x,inc)
}
p<- p + facet_wrap(~constructs, scales='free')
fr<-data.frame(x=x,y=y,constructs=c('PDC_Cal','PDC_Cbh','PDC_Gox','PDC_Lip','PDC_Nb','PDES_Cal','PDES_Cbh','PDES_Gox','PDES_Nb'),label=t_signif)
p<- p + geom_label(alpha=0.6, data=fr, aes(x=x, y=y,label=label),size=3.5,color='navy')


p


