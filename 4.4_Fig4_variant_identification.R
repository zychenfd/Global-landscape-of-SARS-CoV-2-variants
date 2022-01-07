#======load packages======
library(readxl)
library(patchwork)
library(dplyr)
library(rgdal)
library(ggpattern)
library(ggplot2)
library(sf)
library(stringr)
library(reshape2)
library(readr)
library(grid)

#==read data==
WHO_country<-read_excel("Data/WHO_country.xlsx",sheet=1)
mydata.variant4 <- readRDS("F:/Output/Total_seq_full_20211031_2.rds")
tmp1 <- left_join(mydata.variant4, WHO_country, by = c("country" = "GBD_location"))
names(tmp1)[5] <- "lineage"
tmp1 <- tmp1[,c(5,11,14,18)]

#=========Global
alpha <- tmp1 %>% filter(lineage == "B.1.1.7")
alpha <- distinct(alpha, .keep_all = TRUE)
alpha <- alpha %>%  filter(!is.na(date_collect)) %>% 
  filter(date_collect >= as.Date("2020-09-20"))
alpha1 <- alpha %>% group_by(country, ISO3) %>% summarise(Date1 = min(date_collect))
alpha1$type <- "alpha"

beta <- tmp1 %>% filter(lineage == "B.1.351")
beta <- distinct(beta, .keep_all = TRUE)
beta <- beta %>%  filter(!is.na(date_collect)) %>% 
  filter(date_collect >= as.Date("2020-05-11"))
beta1 <- beta %>% group_by(country, ISO3) %>% summarise(Date1 = min(date_collect))
beta1$type <- "beta"

gamma <- tmp1 %>% filter(lineage == "P.1")
gamma <- distinct(gamma, .keep_all = TRUE)
gamma <- gamma %>%  filter(!is.na(date_collect)) %>% 
  filter(date_collect >= as.Date("2020-11-03"))
gamma1 <- gamma %>% group_by(country, ISO3) %>% summarise(Date1 = min(date_collect))
gamma1$type <- "gamma"

delta <- tmp1 %>% filter(lineage == "B.1.617.2")
delta <- distinct(delta, .keep_all = TRUE)
delta <- delta %>%  filter(!is.na(date_collect)) %>% 
  filter(date_collect >= as.Date("2020-10-23"))
delta1 <- delta %>% group_by(country, ISO3) %>% summarise(Date1 = min(date_collect))
delta1$type <- "delta"

VOC <- rbind(alpha1, beta1, gamma1,delta1)

#=make table==
mmp1 <- data.frame(country = rep(unique(WHO_country$GBD_location), each = 4))
mmp1$type <- rep(unique(VOC$type))
mmp2 <- full_join(mmp1,VOC)
mmp3 <- left_join(mmp2[,-c(3)], WHO_country, by = c("country" = "GBD_location"))
mmp3 <- mmp3[,c(1,5,6,2,3)]
mmp3$Source[!is.na(mmp3$Date1)] <- "GISAID"
# write.csv(mmp3,"../Data_when_where/When_where_final_1122.csv", row.names = F)

# #=====read WHo data
# who <- read_xlsx("Data/WHO_annex_1102.xlsx", sheet = 1)
# who <- as.data.frame(who)
# names(who)[1] <- "Country"
# who$Country[who$Country == "Bolivia (Plurinational State of)"] <- "Bolivia"
# who$Country[who$Country == "Brunei Darussalam"] <- "Brunei"
# who$Country[who$Country == "Czechia"] <- "Czech Republic"
# who$Country[who$Country == "Iran (Islamic Republic of)"] <- "Iran"
# who$Country[who$Country == "North Macedonia"] <- "Macedonia"
# who$Country[who$Country == "Republic of Moldova"] <- "Moldova"
# who$Country[who$Country == "Republic of Korea"] <- "South Korea"
# who$Country[who$Country == "Bahamas"] <- "The Bahamas"
# who$Country[who$Country == "Gambia"] <- "The Gambia"
# who$Country[who$Country == "The United Kingdom"] <- "United Kingdom"
# who$Country[who$Country == "United States of America"] <- "United States"
# who$Country[who$Country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
# who$Country[who$Country == "Viet Nam"] <- "Vietnam"
# who$Country[who$Country == "Eswatini"] <- "Swaziland"
# who$Country[who$Country == "C?te d¡¯Ivoire"] <- "Cote d'Ivoire"
# who$Country[str_detect(who$Country, "Ivoire")] <- "Cote d'Ivoire"
# who$Country[who$Country == "Cabo Verde"] <- "Cape Verde"
# who$Country[who$Country == "Lao People's Democratic Republic"] <- "Laos"
# who$Country[who$Country == "Guinea0Bissau"] <- "Guinea-Bissau"
# who$Country[who$Country == "United Republic of Tanzania"] <- "Tanzania"
# who$Country[who$Country == "Timor0Leste"] <- "Timor-Leste"
# who$Country[which(!who$Country %in% WHO_country$GBD_location)]
# 
# WHO_country1 <- left_join(WHO_country, who, by = c("GBD_location" = "Country"))
# table(WHO_country1$Alpha)
# table(WHO_country1$Beta)
# table(WHO_country1$Gamma)
# table(WHO_country1$Delta)
# 
# WHO_country2 <- melt(WHO_country1, id = c(2,3,4), 
#                      measure = c(5:8))
# names(WHO_country2) <- c("ISO3", "country", "WHO_region",   "type",     "Whe_det")
# WHO_country2$type <- as.character(WHO_country2$type)
# WHO_country2$type[WHO_country2$type == "Alpha"] <- "alpha"
# WHO_country2$type[WHO_country2$type == "Beta"] <- "beta"
# WHO_country2$type[WHO_country2$type == "Gamma"] <- "gamma"
# WHO_country2$type[WHO_country2$type == "Delta"] <- "delta"
# 
# tmp4 <- left_join(tmp3, WHO_country2)
# iso<-read_xlsx("../../../COVID-19 burden&vaccination/project 8. Ab dynamics/Variants/ChenXingHui/Geographic/ios_a2_a3.xlsx")
# tmp4 <- left_join(tmp4, iso, by = c("ISO3"= "Alpha-3 code"))
# tmp4 <- tmp4[,c(10,1:7)]
# tmp5 <- read_excel("../Data_when_where/When_where_final_0715.xlsx")
# tmp6 <- left_join(tmp4, tmp5[,c(1:5,9:11)])
# write.csv(tmp6,"../Data_when_where/When_where_final_1102.csv", row.names = F)

#==========================
data <- read_excel("../Data_when_where/When_where_final_1106.xlsx")
data <- data[,c(1:12)]
data$Date1 <- as.Date(as.numeric(data$Date1), origin="1899-12-30")
data$Date_new <- as.Date(as.numeric(data$Date_new), origin="1899-12-30")
data$date_report <- as.Date(data$date_report)
data$date_collect <- as.Date(data$date_collect)

data$Date3[data$Source == "GISAID"] <- as.character(data$Date_new[data$Source == "GISAID"])
data$Date3 <- as.Date(data$Date3)
data$Date3[is.na(data$Date3)] <- data$date_collect[is.na(data$Date3)]
data$Date3[is.na(data$Date3)] <- data$date_report[is.na(data$Date3)] - 21
data$Date3[data$Whe_det == "0"] <- NA


#==read global map====
worldmap <- st_read("World_map.shp")
st_crs(worldmap)
worldmap <- st_transform(worldmap, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap$iso_a3[worldmap$name == "Taiwan"] <- "CHN"
nine <- st_read("nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

data2 <- data
worldmap1 <- left_join(worldmap,data2[data2$type == "alpha",], by=c("iso_a3" = "ISO3"))
worldmap2 <- left_join(worldmap,data2[data2$type == "beta",], by=c("iso_a3" = "ISO3"))
worldmap3 <- left_join(worldmap, data2[data2$type == "gamma",], by=c("iso_a3" = "ISO3"))
worldmap4 <- left_join(worldmap,data2[data2$type == "delta",], by=c("iso_a3" = "ISO3"))
worldmap5 <- left_join(worldmap,data2[data2$type == "omicron",], by=c("iso_a3" = "ISO3"))

theme1 <- theme(axis.ticks = element_blank(), 
                axis.line = element_blank(),
                axis.text = element_blank(),
                legend.justification=c(0,0), 
                legend.position = "bottom",
                legend.background = element_blank(),
                legend.key.size = unit(0.3,"cm"),
                legend.key.width = unit(2,"cm"),
                legend.title = element_text(size = 6), 
                legend.text = element_text(size = 6,angle = 45 ,vjust = 1, hjust = 1,lineheight=4), 
                legend.spacing = unit(4, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
                legend.spacing.y = NULL,#the spacing between legends (unit)
                axis.title = element_blank(),
                plot.margin =  margin(0, 0, 0, 0, "cm"),
                panel.background = element_rect(fill = "white"),
                legend.box.background =element_blank(),
                legend.box.margin=  margin(0, 0, 0, 0, "cm"),
                panel.spacing = unit(0,"cm"),
                panel.border = element_rect(fill='transparent',colour="transparent"),
                legend.key = element_blank(),
                plot.title = element_text(hjust = 0.05, size = 7, vjust = 0))

ggplot() + 
  geom_sf(data = worldmap1, aes(fill = Date3), size = 0.2,color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  theme(legend.position = "none")+
  scale_fill_gradientn("",colours=c("#DA4756","#F68B62","#F6F6BC","#90CC95","#338BBD"),
                       na.value="grey85",
                       breaks = seq(as.Date("2020-05-01"), as.Date("2022-01-01"), "1 month"),
                       labels = c("2020-05-01", "", "2020-07-01","", "2020-09-01",
                                  "", "2020-11-01", "",
                                  "2021-01-01", "", "2021-03-01" ,"", "2021-05-01", 
                                  "", "2021-07-01", "","2021-09-01","","2021-11-01","","2022-01-01"),
                       limits = c(as.Date("2020-05-01"), as.Date("2022-01-01")))+
  labs(title = "a. Alpha variant")-> p1
p1

ggplot() + 
  geom_sf(data = worldmap2, aes(fill = Date3), size = 0.2,color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  theme(legend.position = "none")+
  scale_fill_gradientn("",colours=c("#DA4756","#F68B62","#F6F6BC","#90CC95","#338BBD"),
                       na.value="grey85",
                       breaks = seq(as.Date("2020-05-01"), as.Date("2022-01-01"), "1 month"),
                       labels = c("2020-05-01", "", "2020-07-01","", "2020-09-01",
                                  "", "2020-11-01", "",
                                  "2021-01-01", "", "2021-03-01" ,"", "2021-05-01", 
                                  "", "2021-07-01", "","2021-09-01","","2021-11-01","","2022-01-01"),
                       limits = c(as.Date("2020-05-01"), as.Date("2022-01-01")))+
  labs(title = "b. Beta variant")-> p2

ggplot() + 
  geom_sf(data = worldmap3, aes(fill = Date3), size = 0.2,color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  theme(legend.position = "none")+
  scale_fill_gradientn("",colours=c("#DA4756","#F68B62","#F6F6BC","#90CC95","#338BBD"),
                       na.value="grey85",
                       breaks = seq(as.Date("2020-05-01"), as.Date("2022-01-01"), "1 month"),
                       labels = c("2020-05-01", "", "2020-07-01","", "2020-09-01",
                                  "", "2020-11-01", "",
                                  "2021-01-01", "", "2021-03-01" ,"", "2021-05-01", 
                                  "", "2021-07-01", "","2021-09-01","","2021-11-01","","2022-01-01"),
                       limits = c(as.Date("2020-05-01"), as.Date("2022-01-01")))+
  labs(title = "c. Gamma variant")-> p3

ggplot() + 
  geom_sf(data = worldmap4, aes(fill = Date3), size = 0.2,color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  theme(legend.position = "none")+
  scale_fill_gradientn("",colours=c("#DA4756","#F68B62","#F6F6BC","#90CC95","#338BBD"),
                       na.value="grey85",
                       breaks = seq(as.Date("2020-05-01"), as.Date("2022-01-01"), "1 month"),
                       labels = c("2020-05-01", "", "2020-07-01","", "2020-09-01",
                                  "", "2020-11-01", "",
                                  "2021-01-01", "", "2021-03-01" ,"", "2021-05-01", 
                                  "", "2021-07-01", "","2021-09-01","","2021-11-01","","2022-01-01"),
                       limits = c(as.Date("2020-05-01"), as.Date("2022-01-01")))+
  labs(title = "d. Delta variant")-> p4

ggplot() + 
  geom_sf(data = worldmap5, aes(fill = Date3), size = 0.2,color = "black")+
  geom_sf(data = nine, color="black",size = 0.15)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  theme(legend.position = "none")+
  scale_fill_gradientn("",colours=c("#DA4756","#F68B62","#F6F6BC","#90CC95","#338BBD"),
                       na.value="grey85",
                       breaks = seq(as.Date("2020-05-01"), as.Date("2022-01-01"), "1 month"),
                       labels = c("2020-05-01", "", "2020-07-01","", "2020-09-01",
                                  "", "2020-11-01", "",
                                  "2021-01-01", "", "2021-03-01" ,"", "2021-05-01", 
                                  "", "2021-07-01", "","2021-09-01","","2021-11-01","","2022-01-01"),
                       limits = c(as.Date("2020-05-01"), as.Date("2022-01-01")))+
  labs(title = "e. Omicron variant")-> p5

ggplot() + 
  geom_sf(data = worldmap4, aes(fill = Date3), size = 0.3,color = "black")+
  geom_sf(data = nine, color="black",size = 0.2)+
  coord_sf(xlim = c(-170, 40), ylim = c(-57, -57))+
  theme1+
  theme(legend.key.width = unit(1.8,"cm"),
        legend.background = element_rect(fill='transparent',colour="transparent"),
        plot.margin =  margin(-9, 0, -5, 0, "cm"),
        legend.title = element_text(size =7),
        legend.key = element_rect(colour = "black",fill = "grey"))+
  scale_fill_gradientn("Earliest sampling date of Variants of Concern",
                       colours=c("#DA4756","#F68B62","#F6F6BC","#90CC95","#338BBD"),
                       na.value="grey85",
                       breaks = seq(as.Date("2020-05-01"), as.Date("2022-01-01"), "1 month"),
                       labels = c("2020-05-01", "", "2020-07-01","", "2020-09-01",
                                  "", "2020-11-01", "",
                                  "2021-01-01", "", "2021-03-01" ,"", "2021-05-01", 
                                  "", "2021-07-01", "","2021-09-01","","2021-11-01","","2022-01-01"),
                       limits = c(as.Date("2020-05-01"), as.Date("2022-01-01")))+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  labs(title = "")-> p6 # legend
p6

ggplot()+
  annotate("rect", xmin = 0.2 ,xmax =0.3 ,ymin = 0.93,ymax = 1.07,size= 0.3,fill = "grey85", color = "black")+
  annotate("text", x= 0.35, y =1, label = "Countries with unreported variants or unavailable data",
          hjust = 0, size = 2.5)+
  coord_cartesian(xlim = c(0.2,2), ylim = c(0,2))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.line =  element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0.0,0,0.0, "cm"),
        panel.grid=element_blank()) -> p7
  
#==output==
tiff("Output0106/Fig4.tiff",compression = "lzw",
     width = 8, height = 6.5, res = 600, units = "in")

viewport(x = 0, y = 0.42, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp1
viewport(x = 0, y = 0.09, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp2
viewport(x = 0, y = -0.24, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp3
viewport(x = 0.5, y = 0.42, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp4
viewport(x = 0.5, y = 0.09, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp5
viewport(x = 0.535, y = -0.07, width = 0.04, height = 0.4, just = c("left", "bottom")) -> vp6
viewport(x = 0.515, y = -0.05, width = 0.5, height = 0.3, just = c("left", "bottom")) -> vp7

print(p1,vp = vp1)
print(p3,vp = vp2)
print(p5,vp = vp3)
print(p2,vp = vp4)
print(p4,vp = vp5)
print(p6,vp = vp6)
print(p7,vp = vp7)
dev.off()

pdf("Output0106/Fig4.pdf", width = 8, height = 6.5)

viewport(x = 0, y = 0.42, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp1
viewport(x = 0, y = 0.09, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp2
viewport(x = 0, y = -0.24, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp3
viewport(x = 0.5, y = 0.42, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp4
viewport(x = 0.5, y = 0.09, width = 0.5, height = 0.8, just = c("left", "bottom")) -> vp5
viewport(x = 0.535, y = -0.07, width = 0.04, height = 0.4, just = c("left", "bottom")) -> vp6
viewport(x = 0.515, y = -0.05, width = 0.5, height = 0.3, just = c("left", "bottom")) -> vp7

print(p1,vp = vp1)
print(p3,vp = vp2)
print(p5,vp = vp3)
print(p2,vp = vp4)
print(p4,vp = vp5)
print(p6,vp = vp6)
print(p7,vp = vp7)
dev.off()


#==Plot for VOIs==
lambda <- tmp1 %>% filter(lineage == "C.37")
lambda <- distinct(lambda, .keep_all = TRUE)
lambda <- lambda %>%  filter(!is.na(date_collect)) %>% 
  filter(date_collect >= as.Date("2020-12-22"))
lambda1 <- lambda %>% group_by(country, ISO3) %>% summarise(Date1 = min(date_collect))
lambda1$type <- "lambda"

mu <- tmp1 %>% filter(lineage == "B.1.621")
mu <- distinct(mu, .keep_all = TRUE)
mu <- mu %>%  filter(!is.na(date_collect)) %>% 
  filter(date_collect >= as.Date("2021-01-11"))
mu1 <- mu %>% group_by(country, ISO3) %>% summarise(Date1 = min(date_collect))
mu1$type <- "mu"

VOI <- rbind(lambda1, mu1)

#==plot for VOI==
worldmap5 <- left_join(worldmap,
                       VOI[VOI$type == "lambda",], by=c("iso_a3" = "ISO3"))
worldmap6 <- left_join(worldmap,
                       VOI[VOI$type == "mu",], by=c("iso_a3" = "ISO3"))

ggplot() + 
  geom_sf(data = worldmap5, aes(fill = Date1), size = 0.25,color = "black")+
  geom_sf(data = nine, color="black",size = 0.2)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  theme(legend.position = "none",
        plot.title = element_text(size = 7))+
  scale_fill_gradientn("",colours=c("#F68B62","#F6F6BC","#90CC95"),
                       na.value="grey85",
                       breaks = seq(as.Date("2020-12-01"), as.Date("2021-09-01"), "1 month"),
                       labels = c("2020-12-01", "",
                                  "2021-02-01", "", "2021-04-01" ,"", "2021-06-01", 
                                  "", "2021-08-01", ""),
                       limits = c(as.Date("2020-12-01"), as.Date("2021-09-01")))+
  labs(title = "a. Lambda variant")-> p6
p6

ggplot() + 
  geom_sf(data = worldmap6, aes(fill = Date1), size = 0.25,color = "black")+
  geom_sf(data = nine, color="black",size = 0.2)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  theme(legend.position = "none",
        plot.title = element_text(size = 7))+
  scale_fill_gradientn("",colours=c("#F68B62","#F6F6BC","#90CC95"),
                       na.value="grey85",
                       breaks = seq(as.Date("2020-12-01"), as.Date("2021-09-01"), "1 month"),
                       labels = c("2020-12-01", "",
                                  "2021-02-01", "", "2021-04-01" ,"", "2021-06-01", 
                                  "", "2021-08-01", ""),
                       limits = c(as.Date("2020-12-01"), as.Date("2021-09-01")))+
  labs(title = "b. Mu variant")-> p7
p7

ggplot() + 
  geom_sf(data = worldmap6, aes(fill = Date1), size = 0.3,color = "black")+
  geom_sf(data = nine, color="black",size = 0.25)+
  coord_sf(xlim = c(-170, 40), ylim = c(-57, -57))+
  theme1+
  theme(legend.key.width = unit(2.4,"cm"),
        legend.text = element_text(size = 6,angle = 0 ,vjust = 0.5, hjust = 0.5,lineheight=4),
        plot.margin =  margin(0, 3, 0, 2, "cm"),
        legend.key = element_rect(colour = "black",fill = "grey"))+
  scale_fill_gradientn("Earliest sampling date of Variants of Interest",colours=c("#F68B62","#F6F6BC","#90CC95"),
                       na.value="white",
                       breaks = seq(as.Date("2020-12-01"), as.Date("2021-09-01"), "1 month"),
                       labels = c("2020-12-01", "",
                                  "2021-02-01", "", "2021-04-01" ,"", "2021-06-01", 
                                  "", "2021-08-01", ""),
                       limits = c(as.Date("2020-12-01"), as.Date("2021-09-01")))+
  guides(fill = guide_colorbar(frame.colour = "black",
                               title.position="top",
                               ticks.colour = "black"),
         size = guide_legend(title.position="top"))+
  labs(title = "")-> p8
p8

ggplot()+
  annotate("rect", xmin = 0.24 ,xmax =0.3 ,ymin = 0.98,ymax = 1.02,size= 0.3,fill = "grey85", color = "black")+
  annotate("text", x= 0.35, y =1, label = "Countries with unreported variants or unavailable data",
           hjust = 0, size = 2.5)+
  coord_cartesian(xlim = c(0.2,2), ylim = c(0,2))+
  theme(axis.title = element_blank(),
        panel.background = element_rect(fill = "transparent", colour = NA),
        plot.background = element_rect(fill = "transparent", colour = NA),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        axis.line =  element_blank(),
        legend.background = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        plot.margin = margin(0,0.0,0,0.0, "cm"),
        panel.grid=element_blank()) -> p7_le

#==output==
tiff("Output0106/Extended/ED_Fig7.tif", compression = "lzw",
     width = 6, height = 7, res = 300, units = "in")
# (p6)/(p7)/(p8)+plot_layout(nrow = 3, heights = c(1,1,0.02))
viewport(x = 0, y = 0.35+0.01, width = 1, height = 0.8, just = c("left", "bottom")) -> vp6
viewport(x = 0, y = -0.1+0.05, width = 1, height = 0.8, just = c("left", "bottom")) -> vp7
viewport(x = -0.035, y = -0.43+0.05, width = 0.1, height = 1, just = c("left", "bottom")) -> vp8
viewport(x = 0.04, y = -0.46, width = 1, height = 1, just = c("left", "bottom")) -> vp9
print(p6,vp = vp6)
print(p7,vp = vp7)
print(p8,vp = vp8)
print(p7_le,vp = vp9)
dev.off()
