library(readxl)
library(ggplot2)
inmi_dc_cal_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","INMI","AC21:AN29")
inmi_dc_cal_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","INMI","AC31:AN39")
inmi_dc_lip_1 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","INMI","AC61:AN69")
inmi_dc_lip_2 <- read_excel("C:/Users/Pedro/OneDrive/Escritorio/UPM drive/TFG resources/EXCELS/All experiments.xlsx","INMI","AC71:AN79")
inmi_cal <- merge(inmi_dc_cal_1, inmi_dc_cal_2, all = TRUE)
inmi_lip <- merge(inmi_dc_lip_1, inmi_dc_lip_2, all = TRUE)


inmi_cal <- subset(inmi_cal, select = -c(12, 13))
inmi_lip <- subset(inmi_lip, select = -c(12, 13))

PDC_CalA_INMI <- as.vector(as.matrix(inmi_cal))
PDC_LipT_INMI <- as.vector(as.matrix(inmi_lip))

alls <- cbind(PDC_CalA_INMI, PDC_LipT_INMI)

inserts<-c()
for (j in colnames(alls)){
  for (i in 1:176){
    inserts<-append(inserts,j)
  }
}
values <- as.numeric(c(PDC_CalA_INMI,PDC_LipT_INMI))
realsies <- data.frame(inserts,values)

# 
# boxplot(alls, col=c("lightblue1","lightblue2"), range=1.5)
# outliers <- boxplot(alls, col=c("lightblue1","lightblue2"), range=1.5)$out
# out_cal <- c(0.766785856, 0.800484262, 0.802289740, 0.717692734, 0.777655274)
# out_lip <- c(0.892177590, 0.597193562, 0.662626263, 0.477584629, 0.402787456)
# #text(x = rep(1, length(out_cal)), y = out_cal + 0.05 * (-0.9)**(1:length(out_cal)), labels = round(out_cal, 2), pos = 4)
# #text(x = rep(2, length(out_lip)), y = out_lip + 0.05 * (-0.9)**(1:length(out_lip)), labels = round(out_lip, 2), pos = 4)
# 

boxes <- ggplot(realsies,aes(x=inserts, y=values, fill=inserts)) +
  geom_boxplot(notch=TRUE, outlier.colour = 'darkgreen', outlier.shape = 4) +
  scale_fill_brewer(palette='YlGnBu') +
  ggtitle('Induced protein secretion by Pichia') +
  ylab('Protein secretion quantified by Bradford')+
  theme(
    plot.title = element_text(hjust=0.5),
    axis.title.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
  )+
  geom_segment(x=0.9, y=0.766785856-0.05, xend=1, yend=0.766785856,
               arrow=arrow(length = unit(0.2, 'cm')), color='black', linewidth=0.5)+
  geom_segment(x=0.9, y=0.800484262+0.05, xend=1, yend=0.800484262,
               arrow=arrow(length = unit(0.2, 'cm')), color='black', linewidth=0.5)+
  geom_segment(x=1.1, y=0.802289740+0.05, xend=1, yend=0.802289740,
               arrow=arrow(length = unit(0.2, 'cm')), color='black', linewidth=0.5)+
  geom_segment(x=1.1, y=0.717692734-0.05, xend=1, yend=0.717692734,
               arrow=arrow(length = unit(0.2, 'cm')), color='black', linewidth=0.5)+
  geom_segment(x=1.1, y=0.777655274, xend=1, yend=0.777655274,
               arrow=arrow(length = unit(0.2, 'cm')), color='black', linewidth=0.5)+
  annotate('label',x=0.9,y=0.766785856-0.05,
           label = '1E1',
           color = 'black',
           fill='lightgray',
           fontface='bold',
           size=3)+
  annotate('label',x=0.9,y=0.800484262+0.05,
           label = '2H2',
           color = 'black',
           fill='lightgray',
           fontface='bold',
           size=3)+
  annotate('label',x=1.1,y=0.802289740+0.05,
           label = '1B5',
           color = 'black',
           fill='lightgray',
           fontface='bold',
           size=3)+
  annotate('label',x=1.1,y=0.717692734-0.05,
           label = '2D3',
           color = 'black',
           fill='lightgray',
           fontface='bold',
           size=3)+
  annotate('label',x=1.1,y=0.777655274,
           label = '1B6',
           color = 'black',
           fill='lightgray',
           fontface='bold',
           size=3)+
  annotate('text',x=2.1,y=0.892177590,
           label = '1C8',
           color = 'black',
           fontface='bold',
           size=3)+
  annotate('text',x=2.1,y=0.597193562,
           label = '1H8',
           color = 'black',
           fontface='bold',
           size=3)+
  annotate('text',x=2.1,y=0.662626263,
           label = '2G11',
           color = 'black',
           fontface='bold',
           size=3)+
  annotate('text',x=2.1,y=0.477584629,
           label = '1C4',
           color = 'black',
           fontface='bold',
           size=3)+
  annotate('text',x=2.1,y=0.402787456,
           label = '2C6',
           color = 'black',
           fontface='bold',
           size=3)

boxes

# text(x=2, y=0.892177590, labels=paste("1C8:",round(0.892177590,2)), pos=4)
# text(x=2, y=0.597193562, label=paste("1H8:", round(0.597193562,2)), pos=4)
# text(x=2, y=0.662626263, label=paste("2G11:", round(0.662626263,2)), pos=4)
# text(x=2, y=0.477584629, labels=paste("1C4:", round(0.477584629,2)), pos=4)
# text(x=2, y=0.402787456, labels=paste("2C6:", round(0.402787456,2)), pos=4)
