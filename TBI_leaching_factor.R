#Calculating leaching fraction TBI aquatic


###
library(ggplot2)
library(dplyr)

### Read excel data
TBI_correction=read.table("TBI_data_correction.csv", header=T, sep=";",row.names = 1)
### Create numeric vectors
TBI_correction$Start_weight=as.numeric(as.character(TBI_correction$Start_weight))
TBI_correction$End_weight=as.numeric(as.character(TBI_correction$End_weight))
### Correct dataset on missing values
TBI_correction<-na.omit(TBI_correction)
### Calculate difference in weight, wash out
Dif_W= TBI_correction$Start_weight - TBI_correction$End_weight

TBI_correction=cbind(TBI_correction,Dif_W)

#plot weight loss per position (type) for green and rooibos
myplot<-qplot(Type,Dif_W, data=TBI_correction, geom="boxplot", xlab="Type", ylab="Weight loss in 3 hours")+facet_wrap(~Tea)

mytheme <- theme(axis.line = element_line(size = 1.0, colour = "black"), 
                 axis.text=element_text(size=14), axis.title = element_text(size = 14), panel.background = element_rect(fill = "white"))
print(myplot + mytheme) 

#Fraction of start weigth lost in 3 hours submersion (initial leaching)
Per_W=TBI_correction$Dif_W/TBI_correction$Start_weight
TBI_correction=cbind(TBI_correction, Per_W)

aggregate(Per_W~type+Tea, TBI_correction, mean)