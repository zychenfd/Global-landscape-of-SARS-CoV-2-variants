#=====load packages=======
library(readr)
library(data.table)
library(stringr)
library(seqinr)
library(readxl)
library(dplyr)
library(ggplot2)
library(ggsci)
library(patchwork)
library(sf)
library(rgdal)

#==read and clean data==
WHO_country<-read_excel("Data/WHO_country.xlsx",sheet=1)
mydata.variant <- readRDS("F:/Output/Total_seq_full_20211031_after_dedu2_2.rds") %>% select(c(7,3,4,13))
names(mydata.variant)[1] <- "lineage"
names(mydata.variant)[4] <- "location"
mydata.variant <- mydata.variant %>% filter(!(lineage == "B.1.1.7" & date_collect < as.Date("2020-09-20")))
mydata.variant <- mydata.variant %>% filter(!(lineage == "B.1.351" & date_collect < as.Date("2020-05-11")))
mydata.variant <- mydata.variant %>% filter(!(lineage == "P.1" & date_collect < as.Date("2020-11-03")))
mydata.variant <- mydata.variant %>% filter(!(lineage == "B.1.617.2" & date_collect < as.Date("2020-10-23")))
mydata.variant1 <- as.data.frame(mydata.variant) %>% filter(!is.na(date_collect))
mydata.variant1 <- mydata.variant1  %>% filter(!str_detect(mydata.variant1$location, "Hong Kong")) 
mydata.variant1 <- mydata.variant1  %>% filter(!str_detect(mydata.variant1$location, "Taiwan")) 

#Some countries only have the aggregated data for a time period, thus the genomic data need to align the time
tmp <- mydata.variant1[,c("date_collect","lineage","country")]
tmp <- tmp %>% filter(!(country == "United States" & date_collect <= as.Date("2021-09-19")))
tmp <- tmp %>% filter(!(country == "United States" & date_collect == as.Date("2021-10-17")))
tmp <- tmp %>% filter(!(country == "Indonesia" & date_collect <= as.Date("2020-12-31")))
tmp <- tmp %>% filter(!(country == "Lithuania" & date_collect <= as.Date("2021-04-25")))
tmp <- tmp %>% filter(!(country == "Belgium" & date_collect <= as.Date("2021-05-16")))
tmp <- tmp %>% filter(!(country == "Ireland" & date_collect <= as.Date("2020-12-12")))
tmp <- tmp %>% filter(!(country == "Brazil" & date_collect <= as.Date("2021-01-10")))
tmp <- tmp %>% filter(!(country == "Argentina" & date_collect <= as.Date("2020-12-31")))
tmp <- tmp %>% 
  filter(lineage %in%  c("B.1.1.7","B.1.351","P.1","B.1.617.2")) %>%
  filter(date_collect <= as.Date("2021-10-31")) %>%
  group_by(country, date_collect,lineage) %>% 
  summarise(new = n())

#transfer unit from day to week
tmp$week <- as.numeric(format(tmp$date_collect, "%W"))
tmp$year <- as.numeric(format(tmp$date_collect, "%Y"))
tmp$week[tmp$year == 2020] <- 1+ tmp$week[tmp$year == 2020]
tmp$week[tmp$date_collect %in% c(as.Date("2021-01-01"),
                                 as.Date("2021-01-02"),
                                 as.Date("2021-01-03"))] <- 53
tmp$year[tmp$date_collect %in% c(as.Date("2021-01-01"),
                                 as.Date("2021-01-02"),
                                 as.Date("2021-01-03"))] <- 2020
tmp <- tmp %>% group_by(country, lineage,year, week) %>% summarise(gistotal = sum(new))
mmp <- data.frame(Country = rep(unique(tmp$country),each = 4*96),
                  lineage =  rep( unique(tmp$lineage),96* length(unique(tmp$country))),
                  year = rep(c(rep(2020,4*53),rep(2021,4*43)),length(unique(tmp$country)) ),
                  week = rep(rep(c(1:53,1:43),each = 4),length(unique(tmp$country))))
names(tmp)[1] <-  "Country"
names(mmp)[1] <-  "Country"
mmp1 <- left_join(mmp,tmp)
mmp1 <- mmp1[order(mmp1$Country,mmp1$year, mmp1$week),]
mmp1$gistotal[is.na(mmp1$gistotal)] <- 0

TMPM <- mmp1 %>% group_by(Country,lineage) %>% summarise(cumu_gistotal = cumsum(gistotal))
TMPM$year <-  rep(c(rep(2020,53),rep(2021,43)),4* length(unique(tmp$Country)))
TMPM$week <- rep(rep(c(1:53,1:43)),4* length(unique(tmp$Country)))
gis_cumu <- left_join(TMPM, WHO_country, by = c("Country" = "GBD_location")) %>% filter(year == 2021)

#==read aggregated data==
agge <- read_excel("../Data_variant_extraction/Data_cumu.xlsx", sheet = 4)
agge1 <- melt(agge, id = c(1,2,3,4,11:16),  measure = c("Cumu_b117",
                "Cumu_b1351","Cumu_p1","Cumu_b16172"))
agge1$variable <- as.character(agge1$variable)
agge1$variable[agge1$variable == "Cumu_b117"] <- "B.1.1.7"
agge1$variable[agge1$variable == "Cumu_b1351"] <- "B.1.351"
agge1$variable[agge1$variable == "Cumu_p1"] <- "P.1"
agge1$variable[agge1$variable == "Cumu_b16172"] <- "B.1.617.2"
names(agge1)[11] <- "lineage"
agge2 <- agge1 %>% filter(!is.na(value))
agge3 <- left_join(agge2, gis_cumu)
agge3$cumu_gistotal[is.na(agge3$cumu_gistotal)] <- 0
agge3$diff <- agge3$value - agge3$cumu_gistotal

agge4 <- melt(agge3, id = c(1:11),measure = c("value","cumu_gistotal"))
agge3$prop <- round(agge3$cumu_gistotal *100 / agge3$value, 1)
agge3$prop2 <- round(agge3$cumu_gistotal *100 / agge3$value, 0)
agge3 <- agge3 %>% filter(!Country %in% c("United Arab Emirates","Germany","Croatia"))
agge3$Country[agge3$Country == "Democratic Republic of the Congo"] <- "Dem. Rep. Congo"
tot_voc <-agge3
agge3$prop_text <- paste(agge3$Country," \n(",agge3$prop2,"%, ",agge3$cumu_gistotal,"/",agge3$value,")", sep="")
agge3$prop_text[agge3$prop > 100] <- paste(agge3$Country[agge3$prop > 100]," \n(",
                      agge3$cumu_gistotal[agge3$prop > 100],"/", agge3$value[agge3$prop > 100],")", sep="")

cut(agge3$prop2, breaks = c(-Inf,25,50,75,100.05,Inf),
    right = F, labels = c("[0, 25.0)","[25.0, 50.0)","[50.0, 75.0)","[75.0, 100]","> 100")) -> agge3$prop1
sum(table(agge3$prop1[agge3$lineage == "B.1.1.7"]))
sum(table(agge3$prop1[agge3$lineage == "B.1.351"]))
sum(table(agge3$prop1[agge3$lineage == "P.1"]))
sum(table(agge3$prop1[agge3$lineage == "B.1.617.2"]))

iso<-read_xlsx("ios_a2_a3.xlsx") %>% select(c(1,3,4))
agge4 <- left_join( agge3, iso,by = c( "ISO3" = "Alpha-3 code" ))

#==Plot 1. Public availability across variant type==
worldmap <- st_read("World_map.shp")
st_crs(worldmap)
worldmap <- st_transform(worldmap, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap$iso_a3[worldmap$name == "Taiwan"] <- "CHN"
nine <- st_read("nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

worldmap1 <- left_join(worldmap, agge4[agge4$lineage == "B.1.351",], by=c("iso_a3" = "ISO3"))
worldmap2 <- left_join(worldmap, agge4[agge4$lineage == "P.1",], by=c("iso_a3" = "ISO3"))
worldmap3 <- left_join(worldmap, agge4[agge4$lineage == "B.1.617.2",], by=c("iso_a3" = "ISO3"))
worldmap4 <- left_join(worldmap, agge4[agge4$lineage == "B.1.1.7",], by=c("iso_a3" = "ISO3"))

theme1 <-  theme(axis.ticks = element_blank(),
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 legend.justification=c(0,0),
                 legend.position = c(0.01,0.08),
                 legend.background = element_blank(),
                 legend.key.size = unit(0.3,"cm"),
                 legend.key.width = unit(0.3,"cm"),
                 legend.key.height  = unit(0.3,"cm"),
                 legend.title = element_text(size = 6,face = "bold"),
                 legend.text = element_text(size = 6,lineheight=6),
                 legend.spacing = unit(6, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
                 legend.spacing.y = NULL,#the spacing between legends (unit)
                 axis.title = element_blank(),
                 plot.margin =  margin(0, 0, 0, 0, "cm"),
                 panel.background = element_rect(fill = "white"),
                 legend.box.background =element_blank(),
                 legend.box.margin=  margin(0, 0, 0, 0, "cm"),
                 panel.spacing = unit(0,"cm"),
                 panel.border = element_rect(fill='transparent',colour="transparent"),
                 legend.key = element_blank(),
                 plot.title = element_text(hjust = 0.06, size = 7, vjust = 0))

ggplot() +
  geom_sf(data = worldmap1, aes(fill = prop1), size = 0.23,color = "black")+
  geom_sf(data = nine, color="black",size = 0.18)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  labs(tag = "b")+
  scale_fill_manual("Extent of public availability (%)\nof Beta variants",
                    values =  c(colorRampPalette(c("#FADDA9","#E64B35FF"))(5)), 
                    na.value="grey85",
                    labels = c("[0, 25)","[25, 50)",
                               "[50, 75)","[75, 100]","> 100","Data unavailable"))-> p1

p1 + geom_segment(data = worldmap1, aes(x= x, xend = xend, y = y ,yend= yend),size=0.2)+
  geom_text(data = worldmap1,aes(x= lag,y = ati), label = worldmap1$prop_text,size=1.6)-> fig1
fig1

ggplot() +
  geom_sf(data = worldmap2, aes(fill = prop1), size = 0.23,color = "black")+
  geom_sf(data = nine, color="grey90",size = 0.18)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  labs(tag = "c")+
  scale_fill_manual("Extent of public availability (%)\nof Gamma variants",
                    values =  c(colorRampPalette(c("#FADDA9","#E64B35FF"))(5)), 
                    na.value="grey85",
                    labels = c("[0, 25)","[25, 50)",
                               "[50, 75)","[75, 100]","> 100","Data unavailable"))-> p2
p2 + geom_segment(data = worldmap2, aes(x= x, xend = xend, y = y ,yend= yend),size=0.2)+
  geom_text(data = worldmap2,aes(x= lag,y = ati), label = worldmap2$prop_text,size=1.6)-> fig2

ggplot() +
  geom_sf(data = worldmap3, aes(fill = prop1), size = 0.23,color = "black")+
  geom_sf(data = nine, color="black",size = 0.18)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  labs(tag = "d")+
  scale_fill_manual("Extent of public availability (%)\nof Delta variants",
                    values =  c(colorRampPalette(c("#FADDA9","#E64B35FF"))(5)),  
                    na.value="grey85",
                    labels = c("[0, 25)","[25, 50)",
                               "[50, 75)","[75, 100]","> 100","Data unavailable"))-> p3
p3 + geom_segment(data = worldmap3, aes(x= x, xend = xend, y = y ,yend= yend),size=0.26)+
  geom_text(data = worldmap3,aes(x= lag,y = ati), label = worldmap3$prop_text,size=2)-> fig3

ggplot() +
  geom_sf(data = worldmap4, aes(fill = prop1), size = 0.23,color = "black")+
  geom_sf(data = nine, color="black",size = 0.18)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  labs(tag = "a")+
  scale_fill_manual("Extent of public availability (%)\nof Alpha variants",
                    values = c(colorRampPalette(c("#FADDA9","#E64B35FF"))(5)), 
                    na.value="grey85",
                    labels = c("[0, 25)","[25, 50)",
                               "[50, 75)","[75, 100]","> 100","Data unavailable"))-> p4
p4 + geom_segment(data = worldmap4, aes(x= x, xend = xend, y = y ,yend= yend),size=0.2)+
  geom_text(data = worldmap4,aes(x= lag,y = ati), label = worldmap4$prop_text,size=1.6)-> fig4

#==Output 1==
tiff(file = "Output0106/Extended/E_Fig4.tif", width = 6.8, height = 6, 
     units = "in", compression = "lzw", res = 300)
fig4+fig1+plot_layout(nrow = 2) & theme(plot.tag = element_text(size = 7))
dev.off()

tiff(file = "Output0106/Extended/E_Fig5.tif", width = 6.8, height = 6, 
     units = "in", compression = "lzw", res = 300)
fig2+fig3+plot_layout(nrow = 2) & theme(plot.tag = element_text(size = 7))
dev.off()

#==Plot 2. Public availability for total VOC==
tot_voc1 <- tot_voc %>% group_by(WHO_region, Country,ISO3, week) %>%
  summarise(value = sum(value), cumu_gistotal = sum(cumu_gistotal))
tot_voc1$prop <- round(tot_voc1$cumu_gistotal *100 / tot_voc1$value, 1)
tot_voc1$prop2 <- round(tot_voc1$cumu_gistotal *100 / tot_voc1$value, 0)
tot_voc1$prop_text <- paste(tot_voc1$Country," \n(",tot_voc1$prop2,"%, ",tot_voc1$cumu_gistotal,
                            "/", tot_voc1$value,")", sep="")
tot_voc1$prop_text[tot_voc1$prop > 100] <- paste(tot_voc1$Country[tot_voc1$prop > 100]," \n(",
                                                 tot_voc1$cumu_gistotal[tot_voc1$prop > 100],"/",
                                                 tot_voc1$value[tot_voc1$prop > 100],")", sep="")
table(tot_voc1$Country[tot_voc1$prop > 100])
table(tot_voc1$Country[tot_voc1$prop <= 50])
cut(tot_voc1$prop, breaks = c(-Inf,25,50,75,100.05,Inf),
    right = F, labels = c("[0, 25.0)","[25.0, 50.0)","[50.0, 75.0)","[75.0, 100]","> 100")) -> tot_voc1$prop1
tot_voc2 <- left_join(tot_voc1, distinct(tot_voc[,c(3,5:10)],.keep_all = T))

#==read income data==
income_new <- read_xlsx("Data/Income_group.xlsx")
ptot_voc2 <- left_join(tot_voc2,income_new, by = c("ISO3" = "Code"))
ptot_voc2$`Income group`[is.na(ptot_voc2$`Income group`)] <- "Upper middle income"
table(ptot_voc2$`Income group`[ptot_voc2$prop1 %in% c("[75.0, 100]","> 100")])
table(ptot_voc2$prop1[ptot_voc2$`Income group` %in% c("High income")])
table(ptot_voc2$prop1[ptot_voc2$`Income group` %in% c("Low income", "Lower middle income")])
table(tot_voc2$prop1)

worldmap5 <- left_join(worldmap, tot_voc2, by=c("iso_a3" = "ISO3"))
ggplot() +
  geom_sf(data = worldmap5, aes(fill = prop1), size = 0.2,color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  theme(legend.title = element_text(size = 6,face = "bold"),
        legend.key.size = unit(0.3,"cm"),
        legend.key.width = unit(0.3,"cm"),
        legend.key.height  = unit(0.3,"cm"),
             legend.text = element_text(size = 6,lineheight=4))+
  scale_fill_manual("Extent of public availability (%)\nof VOC sequences",
                    values =  c(colorRampPalette(c("#FADDA9","#E64B35FF"))(5)), 
                    na.value="grey85",
                    labels = c("[0, 25)","[25, 50)",
                               "[50, 75)","[75, 100]","> 100","Data unavailable"))-> p5

p5 + geom_segment(data = worldmap5, aes(x= x, xend = xend, y = y ,yend= yend),size=0.19)+
  geom_text(data = worldmap5,aes(x= lag,y = ati), label = worldmap5$prop_text,size=1.6)-> fig5

fig5

#==Output 2==
tiff(file = "Output0106/Fig3.tiff", width = 7.5, height = 3.5, 
     units = "in", compression = "lzw", res = 600)
fig5
dev.off()

pdf(file = "Output0106/Fig3.pdf", width = 7.5, height = 3.5)
fig5
dev.off()