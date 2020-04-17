## C5P2
library(tidyverse)

SD = read.csv("repdata_data_StormData.csv")
Pop_Health = SD[,c(8,23,24)]
Pop_Health_Total = Pop_Health %>% group_by(EVTYPE) %>% summarize(Total_Fatalities = sum(FATALITIES),
                                                                 Total_Injuries = sum(INJURIES))
PHT_Subset = subset(Pop_Health_Total, Total_Fatalities > 0 | Total_Injuries > 0)

PHT_Subset = PHT_Subset %>% mutate(Total_FI = Total_Fatalities + Total_Injuries)

PHT_Subset = PHT_Subset %>% arrange(desc(Total_FI))

PHT_Subset = subset(PHT_Subset, Total_FI > mean(Total_FI))

PHT_Subset = PHT_Subset %>% arrange(desc(Total_FI))

palette = colorRampPalette(c("blue","purple","red"))

g = ggplot(PHT_Subset, aes(x = reorder(EVTYPE, -Total_FI), y = Total_FI, fill = EVTYPE)) 
g + geom_col() + 
    theme(text = element_text(size = 10), 
          axis.text.x = element_text(angle = 45, hjust = 1)) +
    xlab("Event") +
    ylab("Number of Injuries and Fatalities") +
    ggtitle("Combined Total Number of Injuries and Fatalities by Event") +
    scale_fill_manual(values = palette(18)) +
    geom_text(aes(label = Total_FI), size = 3, vjust = -0.5)

Econ_Damage = SD[,c(8,25,26,27,28)]

Econ_Subset = subset(Econ_Damage, PROPDMG > 0 | CROPDMG > 0)

Econ_Subset$PROPDMGEXP = plyr::revalue(Econ_Subset$PROPDMGEXP, c("h" = "H", "m" = "M"))
Econ_Subset$CROPDMGEXP = plyr::revalue(Econ_Subset$CROPDMGEXP, c("k" = "K", "m" = "M"))

Econ_Subset$PROPDMG[Econ_Subset$PROPDMGEXP == "H"] = 
  (Econ_Subset$PROPDMG[Econ_Subset$PROPDMGEXP == "H"])*100
Econ_Subset$PROPDMG[Econ_Subset$PROPDMGEXP == "K"] = 
  (Econ_Subset$PROPDMG[Econ_Subset$PROPDMGEXP == "K"])*1000
Econ_Subset$PROPDMG[Econ_Subset$PROPDMGEXP == "M"] = 
  (Econ_Subset$PROPDMG[Econ_Subset$PROPDMGEXP == "M"])*1000000
Econ_Subset$PROPDMG[Econ_Subset$PROPDMGEXP == "B"] = 
  (Econ_Subset$PROPDMG[Econ_Subset$PROPDMGEXP == "B"])*1000000000

Econ_Subset$CROPDMG[Econ_Subset$CROPDMGEXP == "K"] = 
  (Econ_Subset$CROPDMG[Econ_Subset$CROPDMGEXP == "K"])*1000
Econ_Subset$CROPDMG[Econ_Subset$CROPDMGEXP == "M"] = 
  (Econ_Subset$CROPDMG[Econ_Subset$CROPDMGEXP == "M"])*1000000
Econ_Subset$CROPDMG[Econ_Subset$CROPDMGEXP == "B"] = 
  (Econ_Subset$CROPDMG[Econ_Subset$CROPDMGEXP == "B"])*1000000000

Total_Econ = Econ_Subset %>% group_by(EVTYPE) %>% summarize(TotCropDmg = sum(CROPDMG),
                                                            TotPropDmg = sum(PROPDMG))
Total_Econ = Total_Econ %>% mutate(Total_Damage = TotCropDmg + TotPropDmg)

High_Damage = subset(Total_Econ, Total_Damage > mean(Total_Damage))
High_Damage$Total_Damage = (High_Damage$Total_Damage)/1000000000

palette2 = colorRampPalette(c("red","orange","yellow"))

g2 = ggplot(High_Damage, aes(x = reorder(EVTYPE, -Total_Damage), y = Total_Damage, fill = EVTYPE)) 
g2 + geom_col() + 
  theme(text = element_text(size = 10), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  xlab("Event") +
  ylab("Total Damage in USD (Billions)") +
  ggtitle("Total Damage Done by Event") +
  scale_fill_manual(values = palette2(25)) +
  geom_text(aes(label = round(Total_Damage)), size = 3, vjust = -0.5) +
  theme(legend.position = "none")
  
