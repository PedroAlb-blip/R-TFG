library(readxl)
library(ggplot2)
library(grDevices)
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




ind_dc_cal <- merge(ind_dc_cal_1, ind_dc_cal_2, all = TRUE)
ind_dc_gox <- merge(ind_dc_gox_1, ind_dc_gox_2, all = TRUE)
ind_dc_nb <- merge(ind_dc_nb_1, ind_dc_nb_2, all = TRUE)
ind_dc_cbh <- merge(ind_dc_cbh_1, ind_dc_cbh_2, all = TRUE)
ind_des_gox <- merge(ind_des_gox_1, ind_des_gox_2, all = TRUE)
ind_des_nb <- merge(ind_des_nb_1, ind_des_nb_2, all = TRUE)
ind_des_cbh <- merge(ind_des_cbh_1, ind_des_cbh_2, all = TRUE)


ind_dc_cal <- subset(ind_dc_cal, select = -c(12, 13))
ind_dc_lip <- subset(ind_dc_lip, select = -c(1))
ind_dc_gox <- subset(ind_dc_gox, select = -c(12, 13))
ind_dc_nb <- subset(ind_dc_nb, select = -c(12, 13))
ind_dc_cbh <- subset(ind_dc_cbh, select = -c(12, 13))
ind_des_cal <- subset(ind_des_cal, select = -c(1))
ind_des_gox <- subset(ind_des_gox, select = -c(12, 13))
ind_des_nb <- subset(ind_des_nb, select = -c(12, 13))
ind_des_cbh <- subset(ind_des_cbh, select = -c(12, 13))

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



alls <- cbind(PDC_Cal_IND, PDC_Lip_IND, PDC_Gox_IND, PDC_Nb_IND, PDC_Cbh_IND, PDES_Cal_IND, PDES_Gox_IND, PDES_Nb_IND, PDES_Cbh_IND)
alls <- na.omit(alls)

inserts<-c()
for (j in colnames(alls)){
  print(j)
  for (i in 1:176){
    inserts<-append(inserts,j)
  }
}
values <- as.numeric(c(PDC_Cal_IND, PDC_Lip_IND, PDC_Lip_IND, PDC_Gox_IND, PDC_Nb_IND, PDC_Cbh_IND, PDES_Cal_IND, PDES_Cal_IND, PDES_Gox_IND, PDES_Nb_IND, PDES_Cbh_IND))
realsies <- data.frame(inserts,values)

boxes <- ggplot(realsies,aes(x=inserts, y=values, fill=inserts)) +
  geom_boxplot(notch=TRUE, outlier.colour = 'darkgreen', outlier.shape = 4) +
  scale_fill_brewer(palette='YlGnBu') +
  ggtitle('Induced protein secretion by Pichia') +
  ylab('Relative protein secretion')+
  coord_flip()+
  theme(
    plot.title = element_text(hjust=0.5),
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
  )+
  #'#FFFFD9' '#EDF8B1' '#C7E9B4' '#7FCDBB' '#41B6C4' '#1D91C0' "#225EA8" "#253494" "#081D58"
  annotate('label',x=1.25,y=6.176,
           label = 'ICK2B2',
           color = 'black',
           fill = "#80B26D",
           fontface='bold',
           size=4)+
  annotate('label',x=1.25,y=1.717,
           label = 'ICK2G2',
           color = 'black',
           fill = "#80B26D",
           fontface='bold',
           size=4)+
  annotate('label',x=1.25,y=4.563,
           label = 'ICK2H2',
           color = 'black',
           fill = "#80B26D",
           fontface='bold',
           size=4)+
  annotate('label',x=2.25,y=3.530,
           label = 'ICC1A5',
           color = 'black',
           fill = "#77AE59",
           fontface='bold',
           size=4)+
  annotate('label',x=2.25,y=1.246,
           label = 'ICC1H8',
           color = 'black',
           fill = "#77AE59",
           fontface='bold',
           size=4)+
  annotate('label',x=2.25,y=0.753,
           label = 'ICC2G1',
           color = 'black',
           fill = "#77AE59",
           fontface='bold',
           size=4)+
  annotate('label',x=3.25,y=2.054,
           label = 'ICG2H9',
           color = 'black',
           fontface='bold',
           fill="#64A75A",
           size=4)+
  annotate('label',x=3.25,y=1.478,
           label = 'ICG1A3',
           color = 'black',
           fill="#64A75A",
           fontface='bold',
           size=4)+
  annotate('label',x=3.25,y=4.033,
           label = 'ICG2C1',
           color = 'black',
           fill="#64A75A",
           fontface='bold',
           size=4)+
  annotate('label',x=4.25,y=0.612,
           label = 'ICLA5',
           color = 'black',
           fill="#40995E",
           fontface='bold',
           size=4)+
  annotate('label',x=4.25,y=0.932,
           label = 'ICLA11',
           color = 'black',
           fill="#40995E",
           fontface='bold',
           size=4)+
  annotate('label',x=4.25,y=1.228,
           label = 'ICLD2',
           color = 'black',
           fill="#40995E",
           fontface='bold',
           size=4)+
  annotate('label',x=5.25,y=1.093,
           label = 'ICN1H4',
           color = 'black',
           fill="#218D62",
           fontface='bold',
           size=4)+
  annotate('label',x=5.25,y=1.711,
           label = 'ICN2D1',
           color = 'black',
           fill="#218D62",
           fontface='bold',
           size=4)+
  annotate('label',x=4.75,y=1.698,
           label = 'ICN2F2',
           color = 'black',
           fill="#218D62",
           fontface='bold',
           size=4)+
  annotate('label',x=5.75,y=0.747,
           label = 'ISKA4',
           color = 'white',
           fill="#0F7A60",
           fontface='bold',
           size=4)+
  annotate('label',x=6,y=0.854 + 0.05,
           label = 'ISKC8',
           color = 'white',
           fill="#0F7A60",
           fontface='bold',
           size=4)+
  annotate('label',x=6.25,y=0.725,
           label = 'ISKF11',
           color = 'white',
           fill="#0F7A60",
           fontface='bold',
           size=4)+
  annotate('label',x=6.75,y=0.590,
           label = 'ISC1G6',
           color = 'white',
           fill="#116154",
           fontface='bold',
           size=4)+
  annotate('label',x=7.25,y=0.609,
           label = 'ISC2A9',
           color = 'white',
           fill="#116154",
           fontface='bold',
           size=4)+
  annotate('label',x=7,y=0.891,
           label = 'ISC2F9',
           color = 'white',
           fill="#116154",
           fontface='bold',
           size=4)+
  annotate('label',x=8.25,y=2.363,
           label = 'ISG1D3',
           color = 'white',
           fill="#134C4A",
           fontface='bold',
           size=4)+
  annotate('label',x=7.75,y=1.590,
           label = 'ISG1E4',
           color = 'white',
           fill="#134C4A",
           fontface='bold',
           size=4)+
  annotate('label',x=8.25,y=1.425,
           label = 'ISG2B7',
           color = 'white',
           fill="#134C4A",
           fontface='bold',
           size=4)+
  annotate('label',x=8.75,y=1.068,
           label = 'ISN1G4',
           color = 'white',
           fill="#04412C",
           fontface='bold',
           size=4)+
  geom_segment(x=9.3, y=1.035-0.5, xend=9, yend=1.035,
               arrow=arrow(length = unit(0.2, 'cm')), color='black', linewidth=0.5)+
  annotate('label',x=9.3,y=1.035-0.5,
           label = 'ISN2F9',
           color = 'white',
           fill="#04412C",
           fontface='bold',
           size=4)+
  geom_segment(x=9.3, y=1.083+0.5, xend=9, yend=1.083,
               arrow=arrow(length = unit(0.2, 'cm')), color='black', linewidth=0.5)+
  annotate('label',x=9.3,y=1.083+0.5,
           label = 'ISN2G9',
           color = 'white',
           fill="#04412C",
           fontface='bold',
           size=4)

boxes

#boxplot(alls, col=c("lightblue1","lightblue2","lightblue3","lightblue4","turquoise","darkred","coral3","darkorange","darksalmon"), bg="black")

