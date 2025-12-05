
#STC-herring
library(dplyr)
library(ggplot2)

#FDI table A
FDI_LAN_23 <- read.csv("Q:\\dfad\\data\\Data\\FDI_data_dissemination\\2024 DC\\2024_fdi_landings\\Landings\\FDI Landings EU 2023.csv")
FDI_LAN_23$EEZ.Indicator[is.na(FDI_LAN_23$EEZ.Indicator)] <- ''
FDI_LAN_23$area_eez <- paste(FDI_LAN_23$Sub.region, FDI_LAN_23$EEZ.Indicator, sep="")

FDI_LAN_23_HER <- FDI_LAN_23 %>%
  filter(Species=='HER' & Sub.region %in% c("27.3.A.20","27.3.A.21","27.3.B.23","27.3.C.22","27.3.D.24","27.3.D.25","27.3.D.26",
                                            "27.3.D.27","27.3.D.28.1","27.3.D.28.2","27.3.D.29","27.3.D.30","27.3.D.31","27.3.D.32","27.4.A")
         & EEZ.Indicator!='UK') %>%
  group_by(Year, area_eez, Metier) %>%
  summarise(TON=sum(tot_live_weight_landed..tonnes., na.rm=T))



FDI_LAN_23_HER <- FDI_LAN_23_HER[FDI_LAN_23_HER$TON>10,]

sort(unique(FDI_LAN_23_HER$area_eez))

ggplot(FDI_LAN_23_HER, aes(x=area_eez, y=TON)) +
  geom_bar(fill="#0073C2FF",stat="identity") +
  theme_bw() +  facet_wrap(~Metier, scales = "free_y") +
  labs(x="Area", y="Ton herring landed") +
  theme(axis.text.x = element_text(size=8, angle=90), plot.title=element_text(size=8), 
        axis.title = element_text(size=8)) 

ggplot(FDI_LAN_23_HER, aes(x=Metier, y=TON)) +
  geom_bar(fill="#0073C2FF",stat="identity") +
  theme_bw() +  facet_wrap(~area_eez, scales = "free_y") +
  labs(x="Metier", y="Ton herring landed") +
  theme(axis.text.x = element_text(size=8, angle=90, hjust=1), plot.title=element_text(size=8), 
        axis.title = element_text(size=8)) 
