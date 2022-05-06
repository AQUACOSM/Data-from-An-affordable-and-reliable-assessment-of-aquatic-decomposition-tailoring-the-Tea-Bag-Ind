#Calculating Tea Bag Index parameters in aquatic envrionment
library(dplyr)
library(plyr)

##INIT              
#inital data from Keuskamp et al 2013
Wbag <- 0.1218 #Weight of bag
Wcord <-0.0288 #weight of cord
Wlabel <- 0.0992 #weight of label
Hg = 0.842  #Hydrolysable fraction green tea
Hr = 0.552 #Hydrolysable fraction red tea

#import Data_TBI.csv and clean up
Data_TBI[Data_TBI=="na"] <- NA

Data_TBI$Country=sapply(strsplit(Data_TBI$Location," "), function(l) l[[1]])

#Data_TBI$Lake=sapply(strsplit(Data_TBI$Location," "), function(l) l[[2]])
names(Data_TBI)
names(Data_TBI)[names(Data_TBI)%in%"Final weight red tea" ]<-"EWr"
names(Data_TBI)[names(Data_TBI)%in%"Final weight green tea"]<-"EWg"
names(Data_TBI)[names(Data_TBI)%in%"Initial weight red tea "] <-"IWr"
names(Data_TBI)[names(Data_TBI)%in%"Initial weight green"]<-"IWg"
names(Data_TBI)[names(Data_TBI)%in%"incubation time"]<-"t"
Data_TBI$Treatment<-as.factor(Data_TBI$Treatment)

Data_TBI$EWg <-as.numeric(Data_TBI$EWg)
Data_TBI$EWr <-as.numeric(Data_TBI$EWr)
Data_TBI$IWg <-as.numeric(Data_TBI$IWg)
Data_TBI$IWr <-as.numeric(Data_TBI$IWr)


#setting up correction for initial leaching
#data from TBI_correction.R -- measured in 2016 in the pond Wadi (3h fractional leaching loss):
#Leaching factor for green tea (proportional loss)
Leach_i_g <- 0.22998787         # i: buried bags (sediment)
Leach_s_g <- 0.21780996         # s: on top of sediment (littoral)
Leach_w_g <- 0.27996722         # w: water column (pelagic)
                                # g: green; r: rooibos

#Leaching factor for red tea (proportional loss)
Leach_i_r <- 0.08982572        
Leach_s_r <- 0.08974139
Leach_w_r <- 0.11341255

#Hydrolysable fraction after initial leaching
Data_TBI <- Data_TBI %>%
  mutate(Hg_leached = ifelse(Data_TBI$type == "littoral", (Hg- Leach_s_g) / (1 - Leach_s_g), 
                             ifelse(Data_TBI$type == "sediment", (Hg- Leach_i_g) / (1 - Leach_i_g), 
                                    ifelse(Data_TBI$type == "pelagic", (Hg- Leach_w_g) / (1 - Leach_w_g), NA))))

Data_TBI <- Data_TBI %>%
  mutate(Hr_leached = ifelse(Data_TBI$type == "littoral", (Hr- Leach_s_r) / (1 - Leach_s_r), 
                             ifelse(Data_TBI$type == "sediment", (Hr- Leach_i_r) / (1 - Leach_i_r), 
                                    ifelse(Data_TBI$type == "pelagic", (Hr- Leach_w_r) / (1 - Leach_w_r), NA))))


#Only calculate with tea weights (no cord, bag, label)-----
Data_TBI <- Data_TBI %>%
     mutate(Ws_green_leached = IWg - Wbag - Wcord - Wlabel- 
              ifelse(Data_TBI$type == "littoral",Leach_s_g*IWg,
                       ifelse(Data_TBI$type == "sediment", Leach_i_g*IWg,
                              ifelse(Data_TBI$type == "pelagic", Leach_w_g*IWg, NA))),
            Ws_rooi_leached  = IWr - Wbag - Wcord - Wlabel-
              ifelse(Data_TBI$type == "littoral",Leach_s_r*IWr,
                        ifelse(Data_TBI$type == "sediment", Leach_i_r*IWr, 
                               ifelse(Data_TBI$type == "pelagic", Leach_w_r*IWr, NA))),
            We_green = EWg - Wbag - Wcord,
            We_rooi = EWr - Wbag - Wcord,
            Wt = We_rooi/Ws_rooi_leached
            )

#calculate parameters
Data_TBI <- Data_TBI %>%
  mutate(Ag = 1-(We_green/Ws_green_leached),
         S  = 1-Ag/Hg_leached,
         Ar = Hr_leached*(1-S),
         k = log(Ar/(Wt-(1-Ar)))/t
  )

#average duplicates

#remove -k (bags gained weigth due to adhering particles/macrofauna)
Data_TBI$k[Data_TBI$k<0]<- NA

#average K
LocMean_k <- ddply(Data_TBI, c("Location","Treatment", "Year"), function(x) 
  data.frame(mean_k = mean(x$k, na.rm=TRUE)))

#Average S
LocMean_S <- ddply(Data_TBI, c("Location","Treatment", "Year"), function(x) 
  data.frame(mean_S = mean(x$S, na.rm=TRUE)))


#collapse duplicates: -----------------
Data_TBI_means = Data_TBI[!duplicated(Data_TBI[c("Location","Treatment", "Year")]),]
