library(readxl)
library(ggplot2)
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


dr_dc_cal <- merge(dr_dc_cal_1, dr_dc_cal_2, all = TRUE)
dr_dc_gox <- merge(dr_dc_gox_1, dr_dc_gox_2, all = TRUE)
dr_dc_nb <- merge(dr_dc_nb_1, dr_dc_nb_2, all = TRUE)
dr_dc_cbh <- merge(dr_dc_cbh_1, dr_dc_cbh_2, all = TRUE)
dr_des_gox <- merge(dr_des_gox_1, dr_des_gox_2, all = TRUE)
dr_des_nb <- merge(dr_des_nb_1, dr_des_nb_2, all = TRUE)
dr_des_cbh <- merge(dr_des_cbh_1, dr_des_cbh_2, all = TRUE)


dr_dc_cal <- subset(dr_dc_cal, select = -c(12, 13))
dr_dc_lip <- subset(dr_dc_lip, select = -c(1))
dr_dc_gox <- subset(dr_dc_gox, select = -c(12, 13))
dr_dc_nb <- subset(dr_dc_nb, select = -c(12, 13))
dr_dc_cbh <- subset(dr_dc_cbh, select = -c(12, 13))
dr_des_cal <- subset(dr_des_cal, select = -c(1))
dr_des_gox <- subset(dr_des_gox, select = -c(12, 13))
dr_des_nb <- subset(dr_des_nb, select = -c(12, 13))
dr_des_cbh <- subset(dr_des_cbh, select = -c(12, 13))

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



alls <- cbind(PDC_Cal_DR, PDC_Lip_DR, PDC_Gox_DR, PDC_Nb_DR, PDC_Cbh_DR, PDES_Cal_DR, PDES_Gox_DR, PDES_Nb_DR, PDES_Cbh_DR)
alls <- as.data.frame(alls)
alls <- na.omit(alls)
inserts<-c()
for (j in colnames(alls)){
  for (i in 1:176){
    inserts<-append(inserts,j)
  }
}
values <- as.numeric(c(PDC_Cal_DR, PDC_Lip_DR, PDC_Lip_DR, PDC_Gox_DR, PDC_Nb_DR, PDC_Cbh_DR, PDES_Cal_DR, PDES_Cal_DR, PDES_Gox_DR, PDES_Nb_DR, PDES_Cbh_DR))
realsies <- data.frame(inserts,values)

# #####
# alls$PDC_Cal_DR<- as.factor(alls$PDC_Cal_DR)
# alls$PDC_Lip_DR<- as.factor(alls$PDC_Lip_DR)
# alls$PDC_Gox_DR<- as.factor(alls$PDC_Gox_DR)
# alls$PDC_Nb_DR<- as.factor(alls$PDC_Nb_DR)
# alls$PDC_Cbh_DR<- as.factor(alls$PDC_Cbh_DR)
# alls$PDES_Cal_DR<- as.factor(alls$PDES_Cal_DR)
# alls$PDES_Gox_DR<- as.factor(alls$PDES_Gox_DR)
# alls$PDES_Nb_DR<- as.factor(alls$PDES_Nb_DR)
# alls$PDES_Cbh_DR<- as.factor(alls$PDES_Cbh_DR)

boxes <- ggplot(realsies,aes(x=inserts, y=values, fill=inserts)) +
  geom_boxplot(notch=TRUE, outlier.colour = 'deeppink', outlier.shape = 4) +
  scale_fill_brewer(palette='YlGnBu') +
  ggtitle('Derepressed protein secretion by Pichia') +
  ylab('Relative protein secretion')+
  coord_flip()+
  theme(
    plot.title = element_text(hjust=0.5),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  annotate('label',x=1.25,y=0.594,
           label = 'DCK1A7',
           color = 'black',
           fill='#FF8AB6',
           fontface='bold',
           size=4)+
  annotate('label',x=1.25,y=0.830,
           label = 'DCK1E3',
           color = 'black',
           fill='#FF8AB6',
           fontface='bold',
           size=4)+
  annotate('label',x=0.75,y=0.589,
           label = 'DCK2D1',
           color = 'black',
           fill='#FF8AB6',
           fontface='bold',
           size=4)+
  # text(x=1, y=0.594+0.05, labels=paste('1A7',round(0.594,2)), pos=1, col='deeppink')
  # text(x=1, y=0.830, labels=paste('1E3',round(0.830,2)), pos=1, col='deeppink')
  # text(x=1, y=0.589-0.05, labels=paste('2D1',round(0.589,2)), pos=1, col='deeppink')
  annotate('label',x=2.25,y=1.443,
           label = 'DCC1A5',
           color = 'black',
           fill='#F686A2',
           fontface='bold',
           size=4)+
  annotate('label',x=2.25,y=0.563,
           label = 'DCC2A7',
           color = 'black',
           fill='#F686A2',
           fontface='bold',
           size=4)+
  annotate('label',x=1.75,y=0.486,
           label = 'DCC2B7',
           color = 'black',
           fill='#F686A2',
           fontface='bold',
           size=4)+
  # text(x=5, y=1.443, labels=paste('1A5',round(1.443,2)), pos=1, col='deeppink')
  # text(x=5, y=0.563+0.05, labels=paste('2A7',round(0.563,2)), pos=1, col='deeppink')
  # text(x=5, y=0.486, labels=paste('2B7',round(0.486,2)), pos=1, col='deeppink')
  annotate('label',x=3.25,y=0.5,
           label = 'DCG1A3',
           color = 'black',
           fill='#E37FA3',
           fontface='bold',
           size=4)+
  annotate('label',x=3.25,y=0.238,
           label = 'DCG1E9',
           color = 'black',
           fill='#E37FA3',
           fontface='bold',
           size=4)+
  annotate('label',x=2.75,y=0.543,
           label = 'DCG2H9',
           fill='#E37FA3',
           color = 'black',
           fontface='bold',
           size=4)+
  # text(x=3, y=0.5, labels=paste('1A3',round(0.5,2)), pos=1, col='deeppink')
  # text(x=3, y=0.238, labels=paste('1E9',round(0.238,2)), pos=1, col='deeppink')
  # text(x=3, y=0.543+0.05, labels=paste('2H9',round(0.543,2)), pos=1, col='deeppink')
  annotate('label',x=4.25,y=0.447,
           label = 'DCLB3',
           color = 'black',
           fill="#BF71A7",
           fontface='bold',
           size=4)+
  annotate('label',x=4.25,y=0.974,
           label = 'DCLF6',
           fill="#BF71A7",
           color = 'black',
           fontface='bold',
           size=4)+
  annotate('label',x=4.25,y=0.858,
           label = 'DCLH1',
           color = 'black',
           fill="#BF71A7",
           fontface='bold',
           size=4)+
  # 
  # text(x=2, y=0.447, labels=paste('B3',round(0.447,2)), pos=1, col='deeppink')
  # text(x=2, y=0.974, labels=paste('F6',round(0.974,2)), pos=1, col='deeppink')
  # text(x=2, y=0.858, labels=paste('H1',round(0.858,2)), pos=1, col='deeppink')
  # 
  annotate('label',x=4.75,y=0.479,
           label = 'DCN1B4',
           color = 'black',
           fill="#A065AC",
           fontface='bold',
           size=4)+
  annotate('label',x=5.25,y=0.479,
           label = 'DCN1G1',
           color = 'black',
           fill="#A065AC",
           fontface='bold',
           size=4)+
  annotate('label',x=5.25,y=0.317,
           label = 'DCN2G1',
           color = 'black',
           fill="#A065AC",
           fontface='bold',
           size=4)+
  # text(x=4, y=0.479+0.1, labels=paste('1B4',round(0.479,2)), pos=1, col='deeppink')
  # text(x=4, y=0.479, labels=paste('1G1',round(0.479,2)), pos=1, col='deeppink')
  # text(x=4, y=0.317, labels=paste('2G1',round(0.317,2)), pos=1, col='deeppink')
  annotate('label',x=6.25,y=1.151,
           label = 'DSKB1',
           color = 'white',
           fill="#8E52AA",
           fontface='bold',
           size=4)+
  annotate('label',x=6.25,y=1.389,
           label = 'DSKB2',
           color = 'white',
           fill="#8E52AA",
           fontface='bold',
           size=4)+
  annotate('label',x=6.25,y=1.791,
           label = 'DSKD1',
           color = 'white',
           fill="#8E52AA",
           fontface='bold',
           size=4)+
  # text(x=6, y=1.151, labels=paste('B1',round(1.151,2)), pos=1, col='deeppink')
  # text(x=6, y=1.389, labels=paste('B2',round(1.389,2)), pos=1, col='deeppink')
  # text(x=6, y=1.791, labels=paste('D1',round(1.791,2)), pos=1, col='deeppink')
  annotate('label',x=7.25,y=0.76,
           label = 'DSC1G2',
           color = 'white',
           fill="#91399E",
           fontface='bold',
           size=4)+
  annotate('label',x=7.25,y=0.616,
           label = 'DSC1G6',
           color = 'white',
           fill="#91399E",
           fontface='bold',
           size=4)+
  annotate('label',x=7.25,y=1.159,
           label = 'DSC2F10',
           color = 'white',
           fill="#91399E",
           fontface='bold',
           size=4)+
  # text(x=9, y=0.76, labels=paste('1G2',round(0.76,2)), pos=1, col='deeppink')
  # text(x=9, y=0.616, labels=paste('1G6',round(0.616,2)), pos=1, col='deeppink')
  # text(x=9, y=1.159, labels=paste('2F10',round(1.159,2)), pos=1, col='deeppink')
  annotate('label',x=8.25,y=1.141,
           label = 'DSG1D3',
           color = 'white',
           fill="#922493",
           fontface='bold',
           size=4)+
  annotate('label',x=8.25,y=0.986,
           label = 'DSG1H5',
           color = 'white',
           fill="#922493",
           fontface='bold',
           size=4)+
  annotate('label',x=8.25,y=0.666,
           label = 'DSG2A6',
           color = 'white',
           fill="#922493",
           fontface='bold',
           size=4)+
  # text(x=7, y=1.141, labels=paste('1D3',round(1.141,2)), pos=1, col='deeppink')
  # text(x=7, y=0.986, labels=paste('1H5',round(0.986,2)), pos=1, col='deeppink')
  # text(x=7, y=0.666+0.05, labels=paste('2A6',round(0.666,2)), pos=1, col='deeppink')
  annotate('label',x=9.25,y=1.539,
           label = 'DSN1C6',
           color = 'white',
           fill="#841876",
           fontface='bold',
           size=4)+
  annotate('label',x=9.25,y=0.909,
           label = 'DSN1A10',
           color = 'white',
           fill="#841876",
           fontface='bold',
           size=4)+
  annotate('label',x=9.25,y=0.763,
           label = 'DSN2A7',
           color = 'white',
           fill="#841876",
           fontface='bold',
           size=4)
# text(x=8, y=1.539, labels=paste('1C6',round(1.539,2)), pos=1, col='deeppink')
# text(x=8, y=0.909, labels=paste('1A10',round(1.035,2)), pos=1, col='deeppink')
# text(x=8, y=0.763, labels=paste('2A7',round(0.763,2)), pos=1, col='deeppink')
boxes


# boxplot(alls, col=c("lightblue1","lightblue2","lightblue3","lightblue4","turquoise","darkred","coral3","darkorange","darksalmon"))
# 



# 

# 

# 

# 

# 

# 

# 
# #lista=list(x.replace(" ","").split(","))
# #string=''
# #for i in lista:
# #  string= string + 'text(x=9, y=' + i.split(":")[1] + ', labels=paste(' + "'" + i.split(":")[0] + "'" + ',round(' + i.split(":")[1] + ',2)), pos=1)'
# #print(string)
# #string=''