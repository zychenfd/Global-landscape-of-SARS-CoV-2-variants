#======load packages======
library(readxl)
library(patchwork)
library(dplyr)
library(rgdal)
library(ggpattern)
library(ggplot2)
library(sf)
library(stringr)
library(ggpubr)

#==read global map==
worldmap <- st_read("World_map.shp")
st_crs(worldmap)
worldmap <- st_transform(worldmap, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap$iso_a3[worldmap$name == "Taiwan"] <- "CHN"
nine <- st_read("nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
WHO_country<-read_excel("Data/WHO_country.xlsx",sheet=1)

#==read sequence data==
mydata.variant4 <- readRDS("F:/Output/Total_seq_full_20211031_after_dedu2_2.rds")
mydata.variant4 <- mydata.variant4[,c(3,4,7)]
names(mydata.variant4)[3] <- "lineage"
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.1.7" & date_collect < as.Date("2020-09-20")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.351" & date_collect < as.Date("2020-05-11")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "P.1" & date_collect < as.Date("2020-11-03")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.617.2" & date_collect < as.Date("2020-10-23")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "C.37" & date_collect < as.Date("2020-12-22")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.621" & date_collect < as.Date("2021-01-11")))

mydata.variant1 <- mydata.variant4 %>% filter(!is.na(date_collect))
all(!is.na(mydata.variant1$country)) # T
mydata.variant1$period[mydata.variant1$date_collect <= as.Date("2020-12-31") & 
                         mydata.variant1$date_collect >= as.Date("2020-01-01")] <- "2020"
mydata.variant1$period[mydata.variant1$date_collect <= as.Date("2021-03-31") & 
                         mydata.variant1$date_collect >= as.Date("2021-01-01")] <- "Jan-Mar"
mydata.variant1$period[mydata.variant1$date_collect <= as.Date("2021-06-30") & 
                         mydata.variant1$date_collect >= as.Date("2021-04-01")] <- "Apr-Jun"
mydata.variant1$period[mydata.variant1$date_collect <= as.Date("2021-10-31") & 
                         mydata.variant1$date_collect >= as.Date("2021-07-01")] <- "Jul-Oct"
mydata.variant1 <- mydata.variant1 %>% filter(date_collect >= as.Date("2020-01-01")) %>% filter(date_collect <= as.Date("2021-10-31")) 

tmp <- mydata.variant1 %>% group_by(country, period, lineage) %>% dplyr::summarise(case = n())
tmp1 <- mydata.variant1 %>% group_by(country, period) %>% dplyr::summarise(seq = n())
tmp <- left_join(tmp, tmp1)
tmp2 <- left_join(tmp, WHO_country[,c(2:4)], by = c("country" = "GBD_location"))
all(!is.na(tmp2$ISO3)) #T

#==stat==
res <- tmp2 %>% group_by(period,lineage) %>%
  summarise(var = sum(case))
res1 <- res %>% group_by(period) %>% summarise(var1 = sum(var))
res2 <- full_join(res, res1)
res2$prop <- round(res2$var * 100/ res2$var1, digits = 1)

#==plot global map==
first <- "2020"
second <- "Jan-Mar"
third <- "Apr-Jun"
four <- "Jul-Oct"

tmp2 <- tmp2 %>% filter(seq > 10) # only those countries with more than 10 sequences in each period are calculated
worldmap1 <- left_join(worldmap, tmp2[(tmp2$period == first & tmp2$lineage == "B.1.1.7"),], by=c("iso_a3" = "ISO3"))
worldmap2 <- left_join(worldmap,tmp2[(tmp2$period == second & tmp2$lineage == "B.1.1.7"),], by=c("iso_a3" = "ISO3"))
worldmap3 <- left_join(worldmap,tmp2[(tmp2$period == third & tmp2$lineage == "B.1.1.7"),], by=c("iso_a3" = "ISO3"))
worldmap4 <- left_join(worldmap,tmp2[(tmp2$period == four & tmp2$lineage == "B.1.1.7"),], by=c("iso_a3" = "ISO3"))

worldmap5 <- left_join(worldmap,tmp2[(tmp2$period == first & tmp2$lineage == "B.1.351"),], by=c("iso_a3" = "ISO3"))
worldmap6 <- left_join(worldmap, tmp2[(tmp2$period == second & tmp2$lineage == "B.1.351"),], by=c("iso_a3" = "ISO3"))
worldmap7 <- left_join(worldmap,tmp2[(tmp2$period == third & tmp2$lineage == "B.1.351"),], by=c("iso_a3" = "ISO3"))
worldmap8 <- left_join(worldmap,tmp2[(tmp2$period == four & tmp2$lineage == "B.1.351"),], by=c("iso_a3" = "ISO3"))

worldmap9 <- left_join(worldmap,tmp2[(tmp2$period == first & tmp2$lineage == "P.1"),], by=c("iso_a3" = "ISO3"))
worldmap10 <- left_join(worldmap,tmp2[(tmp2$period == second & tmp2$lineage == "P.1"),], by=c("iso_a3" = "ISO3"))
worldmap11 <- left_join(worldmap,tmp2[(tmp2$period == third & tmp2$lineage == "P.1"),], by=c("iso_a3" = "ISO3"))
worldmap12 <- left_join(worldmap,tmp2[(tmp2$period == four & tmp2$lineage == "P.1"),], by=c("iso_a3" = "ISO3"))

worldmap13 <- left_join(worldmap, tmp2[(tmp2$period == first & tmp2$lineage == "B.1.617.2"),], by=c("iso_a3" = "ISO3"))
worldmap14 <- left_join(worldmap,tmp2[(tmp2$period == second & tmp2$lineage == "B.1.617.2"),], by=c("iso_a3" = "ISO3"))
worldmap15 <- left_join(worldmap, tmp2[(tmp2$period == third & tmp2$lineage == "B.1.617.2"),], by=c("iso_a3" = "ISO3"))
worldmap16 <- left_join(worldmap,tmp2[(tmp2$period == four & tmp2$lineage == "B.1.617.2"),], by=c("iso_a3" = "ISO3"))

worldmap17 <- left_join(worldmap, tmp2[(tmp2$period == first & tmp2$lineage == "C.37"),], by=c("iso_a3" = "ISO3"))
worldmap18 <- left_join(worldmap,tmp2[(tmp2$period == second & tmp2$lineage == "C.37"),], by=c("iso_a3" = "ISO3"))
worldmap19 <- left_join(worldmap,tmp2[(tmp2$period == third & tmp2$lineage == "C.37"),], by=c("iso_a3" = "ISO3"))
worldmap20 <- left_join(worldmap,tmp2[(tmp2$period == four & tmp2$lineage == "C.37"),], by=c("iso_a3" = "ISO3"))

worldmap21 <- left_join(worldmap, tmp2[(tmp2$period == first & tmp2$lineage == "Ref"),], by=c("iso_a3" = "ISO3"))
worldmap22 <- left_join(worldmap,tmp2[(tmp2$period == second & tmp2$lineage == "Ref"),], by=c("iso_a3" = "ISO3"))
worldmap23 <- left_join(worldmap,tmp2[(tmp2$period == third & tmp2$lineage == "Ref"),], by=c("iso_a3" = "ISO3"))
worldmap24 <- left_join(worldmap,tmp2[(tmp2$period == four & tmp2$lineage == "Ref"),], by=c("iso_a3" = "ISO3"))

worldmap25 <- left_join(worldmap, tmp2[(tmp2$period == first & tmp2$lineage == "B.1.621"),], by=c("iso_a3" = "ISO3"))
worldmap26 <- left_join(worldmap, tmp2[(tmp2$period == second & tmp2$lineage == "B.1.621"),], by=c("iso_a3" = "ISO3"))
worldmap27 <- left_join(worldmap, tmp2[(tmp2$period == third & tmp2$lineage == "B.1.621"),], by=c("iso_a3" = "ISO3"))
worldmap28 <- left_join(worldmap, tmp2[(tmp2$period == four & tmp2$lineage == "B.1.621"),], by=c("iso_a3" = "ISO3"))

#==Global map==
theme_map1 <- theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.text = element_blank(),
        legend.justification=c(0,0), 
        legend.position = c(-0.03,0.05) ,
        legend.background = element_blank(),
        legend.direction  = "horizontal",
        legend.key.size = unit(0.35,"cm"),
        legend.key.width = unit(0.22,"cm"),
        legend.key.height = unit(0.18,"cm"),
        legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5,lineheight=4), 
        legend.spacing = unit(4, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
        legend.spacing.y = NULL,#the spacing between legends (unit)
        axis.title = element_blank(),
        plot.margin =  margin(0.35, 0.35, 0.35, 0.35, "cm"),
        panel.background = element_rect(fill = "white"),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(0,"cm"),
        panel.border = element_rect(fill='transparent',colour="transparent"),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 6, vjust = 0))

theme_map <- theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.text = element_blank(),
        legend.justification=c(0,0), 
        legend.position = "none" ,
        legend.background = element_blank(),
        legend.key.size = unit(0.4,"cm"),
        legend.key.width = unit(0.15,"cm"),
        legend.title = element_text(size = 5), 
        legend.text = element_text(size = 5,lineheight=4), 
        legend.spacing = unit(4, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
        legend.spacing.y = NULL,#the spacing between legends (unit)
        axis.title = element_blank(),
        plot.margin =  margin(0.35, 0.35, 0.35, 0.35, "cm"),
        panel.background = element_rect(fill = "white"),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(0,"cm"),
        panel.border = element_rect(fill='transparent',colour="transparent"),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 6, vjust = 0))

ggplot() + 
  geom_sf(data = worldmap21, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+ 
  theme_map+
  scale_fill_gradient(low = "white",high = "#005757", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1))+
  ggtitle("Jan-Dec, 2020")-> p21

ggplot() + 
  geom_sf(data = worldmap22, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = "white",high = "#005757", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1))+
  ggtitle("Jan-Mar, 2021")-> p22

ggplot() + 
  geom_sf(data = worldmap23, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = "white",high = "#005757", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1))+
  ggtitle("Apr-Jun, 2021")-> p23

ggplot() + 
  geom_sf(data = worldmap24, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map1+
  scale_fill_gradient("Non-variant",
                      low = "white",high = "#005757", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1))+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  ggtitle("Jul-Oct, 2021")-> p24
p24

ggplot() + 
  geom_sf(data = worldmap1, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c(colorRampPalette(c("white", "#21610B"))(20)[2], "#21610B"))(20)[2],
                      high = "#21610B", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1))-> p1

ggplot() + 
  geom_sf(data = worldmap2, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c(colorRampPalette(c("white", "#21610B"))(20)[2], "#21610B"))(20)[2],
                      high = "#21610B", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1))-> p2

ggplot() + 
  geom_sf(data = worldmap3, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#21610B"))(20)[2],high = "#21610B", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1))-> p3

ggplot() + 
  geom_sf(data = worldmap4, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map1+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  scale_fill_gradient("Alpha",
    low = colorRampPalette(c("white", "#21610B"))(20)[2],high = "#21610B", na.value="grey85",
    breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1))-> p4

ggplot() + 
  geom_sf(data = worldmap5, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#868A08"))(20)[2],high = "#868A08", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p5

ggplot() + 
  geom_sf(data = worldmap6, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#868A08"))(20)[2],high = "#868A08", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p6

ggplot() + 
  geom_sf(data = worldmap7, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#868A08"))(20)[2],high = "#868A08", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p7

ggplot() + 
  geom_sf(data = worldmap8, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) +
  theme_map1+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  scale_fill_gradient("Beta",
                      low = colorRampPalette(c("white", "#868A08"))(20)[2],high = "#868A08", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p8

ggplot() + 
  geom_sf(data = worldmap9, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#380B61"))(20)[2],high = "#380B61", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p9

ggplot() + 
  geom_sf(data = worldmap10, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#380B61"))(20)[2],high = "#380B61", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p10

ggplot() + 
  geom_sf(data = worldmap11, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#380B61"))(20)[2],high = "#380B61", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p11

ggplot() + 
  geom_sf(data = worldmap12, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map1+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  scale_fill_gradient("Gamma",
                      low = colorRampPalette(c("white", "#380B61"))(20)[2],high = "#380B61", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p12


ggplot() + 
  geom_sf(data = worldmap13, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#8A0808"))(20)[2],high = "#8A0808", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p13

ggplot() + 
  geom_sf(data = worldmap14, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#8A0808"))(20)[2],high = "#8A0808", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p14

ggplot() + 
  geom_sf(data = worldmap15, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = colorRampPalette(c("white", "#8A0808"))(20)[2],high = "#8A0808", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p15

ggplot() + 
  geom_sf(data = worldmap16, aes(fill = case/seq), size = 0.12, color = "black")+
  geom_sf(data = nine, color="black",size = 0.06)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90)) + 
  theme_map1+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  scale_fill_gradient("Delta",
                      low = colorRampPalette(c("white", "#8A0808"))(20)[2],high = "#8A0808", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p16

#==output==
tiff(file = "Output0106/Fig5.tiff", width = 8.2, height = 5.8,
     units = "in", compression = "lzw", res = 600)
ggarrange(p21,p22,p23,p24,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
          p13,p14,p15, p16, nrow =5, ncol = 4, heights = c(1.1,1,1,1,1))
dev.off()

pdf(file = "Output0106/Fig5.pdf", width = 8.2, height = 5.8)
ggarrange(p21,p22,p23,p24,p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,
          p13,p14,p15, p16, nrow =5, ncol = 4, heights = c(1.1,1,1,1,1))
dev.off()

#==for VOI==
ggplot() + 
  geom_sf(data = worldmap17[worldmap17$name != "Greenland",], aes(fill = case/seq), size = 0.2, color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, -40), ylim = c(-57, 90)) + 
  theme_map+
  labs(title = "Jan-Dec, 2020")+
  theme(plot.title = element_text(hjust = 0.5, size = 6, vjust = 0))+
  scale_fill_gradient(low = "white",high = "#004B97", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p17

ggplot() + 
  geom_sf(data = worldmap18[worldmap18$name != "Greenland",], aes(fill = case/seq), size = 0.2, color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, -40), ylim = c(-57, 90)) + 
  theme_map+
  labs(title = "Jan-Mar, 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 6, vjust = 0))+
  scale_fill_gradient(low = "white",high = "#004B97", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p18

ggplot() + 
  geom_sf(data = worldmap19[worldmap19$name != "Greenland",], aes(fill = case/seq), size = 0.2, color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, -40), ylim = c(-57, 90)) + 
  theme_map+
  labs(title = "Apr-Jun, 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 6, vjust = 0))+
  scale_fill_gradient(low = "white",high = "#004B97", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p19

ggplot() + 
  geom_sf(data = worldmap20[worldmap20$name != "Greenland",], aes(fill = case/seq), size = 0.2, color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, -40), ylim = c(-57, 90)) + 
  theme_map1+
  ggtitle("Jul-Oct, 2021")+
  theme(plot.title = element_text(hjust = 0.5, size = 6, vjust = 0),
        legend.key.width = unit(0.4,"cm"))+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  scale_fill_gradient("Prevalence of Lambda (%)",
                      low = "white",high = "#004B97", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","25","50","75","100"),limits = c(0, 1)) -> p20
p20

ggplot() + 
  geom_sf(data = worldmap25[worldmap25$name != "Greenland",], aes(fill = case/seq), size = 0.2, color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, -40), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = "white",high = "#DE5258", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p25

ggplot() + 
  geom_sf(data = worldmap26[worldmap26$name != "Greenland",], aes(fill = case/seq), size = 0.2, color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, -40), ylim = c(-57, 90)) + 
  theme_map+
  scale_fill_gradient(low = "white",high = "#DE5258", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p26

ggplot() + 
  geom_sf(data = worldmap27[worldmap27$name != "Greenland",], aes(fill = case/seq), size = 0.2, color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, -40), ylim = c(-57, 90)) + 
  theme_map+
  theme(plot.title = element_text(hjust = 0.5, size = 7, vjust = 0))+
  scale_fill_gradient(low = "white",high = "#DE5258", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","","50","","100"),limits = c(0, 1)) -> p27

ggplot() + 
  geom_sf(data = worldmap28[worldmap28$name != "Greenland",], aes(fill = case/seq),size = 0.2, color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, -40), ylim = c(-57, 90)) + 
  theme_map1+
  theme(legend.key.width = unit(0.4,"cm"))+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  scale_fill_gradient("Prevalence of Mu (%)",
                      low = "white",high = "#DE5258", na.value="grey85",
                      breaks = seq(0, 1, 0.25), labels = c("0","25","50","75","100"),limits = c(0, 1)) -> p28
p28

#==output==
tiff(file = "Output0106/Extended/ED_Fig8.tif", width = 7, height = 4,
     units = "in", compression = "lzw", res = 300)
ggarrange(p17,p18,p19, p20,p25,p26,p27,p28,nrow =2, ncol = 4, heights = c(1,0.9))
dev.off()
