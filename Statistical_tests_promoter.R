library(readxl)
library(ggplot2)
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
       'PDC_Gox_DR',
       'PDC_Cbh_DR',
       'PDC_Nb_DR',
       'PDES_Cal_DR',
       'PDES_Gox_DR',
       'PDES_Cbh_DR',
       'PDES_Nb_DR',
       'PDC_Cal_IND',
       'PDC_Gox_IND',
       'PDC_Cbh_IND',
       'PDC_Nb_IND',
       'PDES_Cal_IND',
       'PDES_Gox_IND',
       'PDES_Cbh_IND',
       'PDES_Nb_IND')
PDC<-c()
for (i in str_extract(dict,regex('.+_.+_.+'))) {
  for(j in str_extract(dict,regex('.+_.+_.+'))){
    if (str_detect(i, regex('[^S]_.+_.+')) & str_detect(j, regex('[^S]_.+_.+'))){
      if(str_extract(i, regex('_.+_')) == str_extract(j, regex('_.+_')) & str_extract(i, regex('_.+_.')) != str_extract(j, regex('_.+_.'))){
        if (i %in% PDC){
          break
        }
        print(i)
        PDC=append(PDC,i)
        PDC=append(PDC,j)
      }
    }
  }
}

PDES<-c()
for (i in str_extract(dict,regex('.+_.+_.+'))) {
  for(j in str_extract(dict,regex('.+_.+_.+'))){
    if (str_detect(i, regex('[^C]_.+_.+')) & str_detect(j, regex('[^C]_.+_.+'))){
      if(str_extract(i, regex('_.+_')) == str_extract(j, regex('_.+_')) & str_extract(i, regex('_.+_.')) != str_extract(j, regex('_.+_.'))){
        if (i %in% PDES){
          break
        }
        print(i)
        PDES=append(PDES,i)
        PDES=append(PDES,j)
      }
    }
  }
}



PDC <- sort(PDC)
PDES <- sort(PDES)

promoters <- c()
p_signif <- c()


for (i in seq(1,length(PDC),1)){
  promoters <- append(promoters,wilcox.test(get(PDC[i]),get(PDES[i]))$p.value)
  if (wilcox.test(get(PDC[i]),get(PDES[i]))$p.value < 0.001){
    p_signif <- append(p_signif,'***')
  } else if (wilcox.test(get(PDC[i]),get(PDES[i]))$p.value < 0.005){
    p_signif <- append(p_signif,'**')
  } else if (wilcox.test(get(PDC[i]),get(PDES[i]))$p.value < 0.01){
    p_signif <- append(p_signif,'*')
  } else {
    p_signif <- append(p_signif,'')
  }
}

constructs <- c()
treatment <- c()
for (i in str_split(dict,'_')) {
  for (k in i){
    if (str_starts(k,'P')) {
      for (j in 1:176){
        #print(paste(k,i[which(i==k)+1]))
        treatment <- append(treatment,paste(i[which(i==k)+1],i[which(i==k)+2],sep='_'))
        constructs <- append(constructs,k)
      }
    }
  }
}

RBF_values <- c(PDC_Cal_DR, PDC_Gox_DR, PDC_Cbh_DR, PDC_Nb_DR, PDES_Cal_DR, PDES_Cal_DR, PDES_Gox_DR, PDES_Cbh_DR, PDES_Nb_DR, PDC_Cal_IND, PDC_Gox_IND, PDC_Cbh_IND, PDC_Nb_IND, PDES_Cal_IND, PDES_Cal_IND, PDES_Gox_IND, PDES_Cbh_IND, PDES_Nb_IND)
df <- data.frame(constructs,treatment,RBF_values)
inc <- 1.5
x<-c()
y<-c()
p <- ggplot(df,aes(x=constructs, y=RBF_values))+
  geom_boxplot(aes(fill=constructs))
for (i in 1:8){
  y<- append(y,quantile(get(PDC[i]),0.75,na.rm=TRUE)+1.5*(quantile(get(PDC[i]),0.75,na.rm=TRUE)-quantile(get(PDC[i]),0.25,na.rm=TRUE)))
  x<- append(x,inc)
}
p<- p+facet_wrap(~treatment, scales='free')
fr<-data.frame(x=x,y=y,treatment=c('Cal_DR','Cal_IND','Cbh_DR','Cbh_IND','Gox_DR','Gox_IND','Nb_DR','Nb_IND'),label=p_signif)
p<- p+geom_label(alpha=0.6, data=fr, aes(x=x, y=y,label=label),size=3.5,color='navy')

p
