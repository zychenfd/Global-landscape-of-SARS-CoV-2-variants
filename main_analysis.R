#==load packages==
library(readxl)
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggsci)
library(stringr)
library(data.table)
library(ggpubr)

#==read data==
WHO_country <- read_excel("Data/WHO_country.xlsx",sheet=1) # country code
epi1 <- read.csv("Data/WHO-COVID-19-global-data.csv") # epi data
mydata.variant4 <- read.csv("Data/seq_0915.csv") # variant data

#==data clean==
#epi data
names(epi1)[1] <- "Date_reported"
epi1$Date_reported <- as.Date(as.character(epi1$Date_reported))
epi1$Country[epi1$Country == "Bolivia (Plurinational State of)"] <- "Bolivia"
epi1$Country[epi1$Country == "Brunei Darussalam"] <- "Brunei"
epi1$Country[epi1$Country == "Czechia"] <- "Czech Republic"
epi1$Country[epi1$Country == "Iran (Islamic Republic of)"] <- "Iran"
epi1$Country[epi1$Country == "North Macedonia"] <- "Macedonia"
epi1$Country[epi1$Country == "Republic of Moldova"] <- "Moldova"
epi1$Country[epi1$Country == "Republic of Korea"] <- "South Korea"
epi1$Country[epi1$Country == "Bahamas"] <- "The Bahamas"
epi1$Country[epi1$Country == "Gambia"] <- "The Gambia"
epi1$Country[epi1$Country == "The United Kingdom"] <- "United Kingdom"
epi1$Country[epi1$Country == "United States of America"] <- "United States"
epi1$Country[epi1$Country == "Venezuela (Bolivarian Republic of)"] <- "Venezuela"
epi1$Country[epi1$Country == "Viet Nam"] <- "Vietnam"
epi1$Country[epi1$Country == "Eswatini"] <- "Swaziland"
epi1$Country[str_detect(epi1$Country, "voire")] <- "Cote d'Ivoire"
epi1$Country[epi1$Country == "Cabo Verde"] <- "Cape Verde"
# epi1$Country[epi1$Country == "Czech Republic"] <- "Czechia"
epi1$Country[epi1$Country == "United Republic of Tanzania"] <- "Tanzania"
epi1$Country[epi1$Country == "Lao People's Democratic Republic"] <- "Laos"
epi1$Country[epi1$Country == "Democratic People's Republic of Korea"] <- "North Korea"
epi1$Country[epi1$Country == "Syrian Arab Republic"] <- "Syria"
epi1$Country[epi1$Country == "Micronesia (Federated States of)"] <- "Federated States of Micronesia"
epi1$Country_code[epi1$Country == "Namibia"] <- "NA"
epi2 <- epi1 %>% filter(Date_reported == as.Date("2021-09-15"))
epi2 <- epi2[,c(2,3,6)]

#variant data
mydata.variant4$date_collect <- as.Date(mydata.variant4$date_collect)
mydata.variant4 <- mydata.variant4[,5:8]
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.1.7" & date_collect < as.Date("2020-09-20")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.351" & date_collect < as.Date("2020-05-11")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "P.1" & date_collect < as.Date("2020-11-03")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.617.2" & date_collect < as.Date("2020-10-23")))
length(unique(mydata.variant4$country))
WHO_country <- read_excel("Data/WHO_country.xlsx",sheet=1)

#==the number of sequences by country==
vari <- mydata.variant4 %>% group_by(country) %>% summarise(total = n())
vari1 <- left_join(WHO_country,vari, by = c("GBD_location" = "country" ))

#==stat num by region==
region <- vari1 %>% filter(!is.na(total))
region1 <- region %>% group_by(WHO_region) %>% summarise(total1 = sum(total))
(region1$total1 / sum(region1$total1))*100

cut(vari1$total, breaks = c(-Inf,100,1000,10000,100000,Inf), right = F, 
    labels = c("[1, 100)","[100, 1000)","[1000, 10000)","[10000, 100000)","¡Ý 100000")) -> vari1$total1
table(vari1$total1)

#==get the uniform country code
iso<-read_xlsx("../../../COVID-19 burden&vaccination/project 8. Ab dynamics/Variants/ChenXingHui/Geographic/ios_a2_a3.xlsx")
iso <- iso[,c(1,3,4)]
vari1 <- left_join( vari1, iso,by = c( "ISO3" = "Alpha-3 code" ))

#==join data
vari1 <- left_join(vari1, epi2,by = c( "Alpha-2 code" = "Country_code"))
vari1$total1 <- as.character(vari1$total1)
vari1$total1[vari1$Cumulative_cases == 0 ] <- "Not identified SARS-CoV-2"
vari1$total1[is.na(vari1$total1)] <- "Not uploaded sequences"

#==the proportion of sequences by country==
vari1$ratio <- round(vari1$total * 100/vari1$Cumulative_cases, digits = 2)
cut(vari1$ratio, breaks = c(-Inf,1,2.5,5,10,15,Inf),
    right = F, labels = c("[0, 1.0)","[1.0, 2.5)","[2.5, 5.0)","[5.0, 10.0)","[10.0, 15.0)","¡Ý15.0")) -> vari1$ratio1
vari1$ratio1 <- as.character(vari1$ratio1)
vari1$ratio1[vari1$Cumulative_cases == 0 ] <- "Not identified SARS-CoV-2"
vari1$ratio1[is.na(vari1$ratio1)] <- "Not uploaded sequences"

#==read global map====
worldmap <- st_read("../../../COVID-19 burden&vaccination/project 8. Ab dynamics/Variants/ChenXingHui/Geographic/World_map.shp")
st_crs(worldmap)
worldmap <- st_transform(worldmap, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap$iso_a3[worldmap$name == "Taiwan"] <- "CHN"
nine <- st_read("../../../COVID-19 burden&vaccination/project 8. Ab dynamics/Variants/ChenXingHui/Geographic/nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap1 <- left_join(worldmap,  vari1, by=c("iso_a3" = "ISO3"))

#==define plot theme==
theme1 <-  theme(axis.ticks = element_blank(),
                 axis.line = element_blank(),
                 axis.text = element_blank(),
                 legend.justification=c(0,0),
                 legend.position = c(0.01,0.08),
                 legend.background = element_blank(),
                 # legend.direction  = "horizontal",
                 legend.key.size = unit(0.4,"cm"),
                 legend.key.width = unit(0.6,"cm"),
                 legend.key.height  = unit(0.6,"cm"),
                 legend.title = element_text(size = 14,face = "bold"),
                 legend.text = element_text(size = 14,lineheight=6),
                 legend.spacing = unit(10, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
                 legend.spacing.y = NULL,#the spacing between legends (unit)
                 axis.title = element_blank(),
                 plot.margin =  margin(0, 0, 0, 0, "cm"),
                 panel.background = element_rect(fill = "white"),
                 legend.box.background =element_blank(),
                 legend.box.margin=  margin(0, 0, 0, 0, "cm"),
                 panel.spacing = unit(0,"cm"),
                 panel.border = element_rect(fill='transparent',colour="transparent"),
                 legend.key = element_blank(),
                 plot.title = element_text(hjust = 0.06, size = 20, vjust = 0))

#set legend order
factor(worldmap1$total1, levels = c("[1, 100)" , "[100, 1000)","[1000, 10000)" ,                        
                                    "[10000, 100000)" ,"¡Ý 100000","Not uploaded sequences",
                                    "Not identified SARS-CoV-2")) -> worldmap1$total1
factor(worldmap1$ratio1, levels = c("[0, 1.0)","[1.0, 2.5)","[2.5, 5.0)","[5.0, 10.0)",
                                    "[10.0, 15.0)","¡Ý15.0" ,"Not uploaded sequences",
                                    "Not identified SARS-CoV-2")) -> worldmap1$ratio1

#==plot figure==
ggplot() +
  geom_sf(data = worldmap1, aes(fill = total1), size = 0.4,color = "black")+
  geom_sf(data = nine, color="black",size = 0.3)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  labs(title = "D")+
  scale_fill_manual("Number of sequenced \n(as of 15 Sep 2021)",
                    values = c(colorRampPalette(c("#FADDA9", "#E64B35FF"))(5),"grey60","grey80"), 
                    na.value="white",
                    labels = c("[1, 100)" , "[100, 1000)","[1000, 10000)" ,                        
                               "[10000, 100000)" ,"¡Ý 100000","No uploaded sequences",
                               "Not identified SARS-CoV-2","Data unavailable"))-> p2
p2

ggplot() +
  geom_sf(data = worldmap1, aes(fill = ratio1), size = 0.4,color = "black")+
  geom_sf(data = nine, color="black",size = 0.3)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  labs(title = "F")+
  scale_fill_manual("Proportion (%) of\ninfections sequenced\n(as of 15 Sep 2021)",
                    values = c(colorRampPalette(c("#FADDA9", "#E64B35FF"))(6),"grey60","grey80"), 
                    na.value="white",
                    labels = c("[0, 1.0)","[1.0, 2.5)","[2.5, 5.0)","[5.0, 10.0)","[10.0, 15.0)","¡Ý15.0" ,"No uploaded sequences",
                               "Not identified SARS-CoV-2","Data unavailable"))-> p4
p4

#==P3
tmp <- mydata.variant4 %>% group_by(country,date_collect) %>% 
  summarise(new = n())
Sys.setlocale("LC_TIME", "US")
tmp$week <- as.numeric(format(tmp$date_collect, "%W"))
tmp$year <- as.numeric(format(tmp$date_collect, "%Y"))
tmp$week[tmp$year == 2020] <- 1+ tmp$week[tmp$year == 2020]
tmp$week[tmp$date_collect %in% c(as.Date("2021-01-01"),
                                 as.Date("2021-01-02"),
                                 as.Date("2021-01-03"))] <- 53
tmp$year[tmp$date_collect %in% c(as.Date("2021-01-01"),
                                 as.Date("2021-01-02"),
                                 as.Date("2021-01-03"))] <- 2020
tmp$week <- formatC(tmp$week,flag = "0", width = 2)
tmp$year_week <- paste(tmp$year,"-", tmp$week,sep = "")
tmp <- left_join(tmp, WHO_country, by = c("country" = "GBD_location"))
tmp1 <- tmp %>% group_by(WHO_region,year_week) %>% 
  summarise(number_detections_variant = sum(new))

#======read WHO epi data=====
epi3 <- epi1
epi3$week <- as.numeric(format(epi3$Date_reported, "%W"))
epi3$year <- as.numeric(format(epi3$Date_reported, "%Y"))
epi3$week[epi3$year == 2020] <- 1+epi3$week[epi3$year == 2020]
epi3$week[epi3$Date_reported %in% c(as.Date("2021-01-01"),
                                    as.Date("2021-01-02"),
                                    as.Date("2021-01-03"))] <- 53
epi3$year[epi3$Date_reported %in% c(as.Date("2021-01-01"),
                                    as.Date("2021-01-02"),
                                    as.Date("2021-01-03"))] <- 2020
epi3$week <- formatC(epi3$week,flag = "0", width = 2)
epi3$year_week <- paste(epi3$year,"-", epi3$week,sep = "")
epi4 <- epi3 %>% group_by(WHO_region,year_week) %>% 
  summarise(new_cases = sum(New_cases))
unique(epi4$WHO_region)
unique(tmp1$WHO_region)
epi4$WHO_region[epi4$WHO_region =="AFRO"] <- "AFR"  
epi4$WHO_region[epi4$WHO_region =="AMRO"] <- "AMR"
epi4$WHO_region[epi4$WHO_region =="EMRO"] <- "EMR"
epi4$WHO_region[epi4$WHO_region =="EURO"] <- "EUR"
epi4$WHO_region[epi4$WHO_region =="SEARO"] <- "SEAR"
epi4$WHO_region[epi4$WHO_region =="WPRO"] <- "WPR"
tmp3 <- left_join(tmp1, epi4)
tmp3$diff <- tmp3$new_cases - tmp3$number_detections_variant

tran_data <- data.frame(date = seq(as.Date("2020-03-02"),as.Date("2021-09-19"), 7))
tran_data$year <- format(tran_data$date, "%Y")
tran_data$week <- as.numeric(format(tran_data$date, "%W"))
tran_data$week[tran_data$year == 2020] <- 1+tran_data$week[tran_data$year == 2020]

tran_data$week[tran_data$Date_reported %in% c(as.Date("2021-01-01"),
                                              as.Date("2021-01-02"),
                                              as.Date("2021-01-03"))] <- 53
tran_data$year[tran_data$Date_reported %in% c(as.Date("2021-01-01"),
                                              as.Date("2021-01-02"),
                                              as.Date("2021-01-03"))] <- 2020
tran_data$week <- formatC(tran_data$week,flag = "0", width = 2)
tran_data$year_week <- paste(tran_data$year,"-", tran_data$week,sep = "")
tran_data$week <- as.numeric(tran_data$week)
tran_data <- tran_data[,c(1,4)]

tmp4 <- left_join(tmp3, tran_data)
tmp4 <- tmp4 %>% filter(date >= as.Date("2020-05-11") & date <= as.Date("2021-09-12"))
tmp4 <- as.data.frame(tmp4)
tran_data1 <- tran_data %>% filter(date >= as.Date("2020-05-11") & date <= as.Date("2021-09-12"))
tran_data1$x <- 1:length(tran_data1$date)
tmp4 <- left_join(tmp4, tran_data1)

glo <- tmp4 %>% group_by(year_week, date, x) %>%
  summarise(number_detections_variant = sum(number_detections_variant),
            new_cases = sum(new_cases))
glo$WHO_region <- "Global"
tmp4 <- full_join(tmp4, glo)
tmp4$prop <- tmp4$number_detections_variant * 100/tmp4$new_cases

theme1 <- theme(panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.5),
                # panel.grid.major.y = element_line(colour = "black", size = 0.5, color = "grey80"),
                # panel.border = element_rect(colour = "black",fill = "transparent", size = 0.5),
                axis.ticks = element_line(colour = "black", size = 0.5),
                axis.ticks.length = unit(0.3,"lines"),
                legend.position = c(0.15,0.8),
                # legend.direction = "horizontal",
                axis.ticks.length.y = unit(0.4,"lines"),
                legend.key = element_blank(),
                legend.background = element_blank(),
                plot.margin = margin(0.5,1,1,1, "cm"),
                axis.title = element_text(size = 16),
                text = element_text(size = 14),
                axis.text.x = element_text(colour = "black",size = 14),
                axis.text.y = element_text(colour = "black",size = 14),
                axis.title.x = element_text(vjust = 0.5, hjust = 0.5),
                axis.title.y = element_text(vjust = 2, hjust = 0.5),
                plot.title = element_text(hjust = 0, size = 20, vjust = 0))

factor(tmp4$WHO_region, levels =
         c( "Global","AFR" ,"AMR","EMR" , "EUR", "SEAR", "WPR")) -> tmp4$WHO_region
tmp5 <-tmp4
tmp5$number_detections_variant[tmp5$WHO_region == "Global"] <- 0
ggplot() + 
  geom_bar(data = tmp5[tmp5$WHO_region != "Global"&
                         tmp5$date >= as.Date("2020-08-31"),], size =0.25,
           aes(x = date, y = number_detections_variant,fill = WHO_region), color = "black",stat = "identity")+
  scale_x_date("Date of collection",date_breaks = "1 month",
               date_labels = "%b")+
  scale_y_continuous("Number of sequenced per week (¡Á1,000)", breaks = seq(0,200000,50000),
                     labels = str_remove_all(format(abs((seq(0,200,50))), big.mark = ","), " "))+
  coord_cartesian(ylim = c(-1500, 200000),xlim = c(as.Date("2020-08-25"),as.Date("2021-09-19") ),clip = "off", expand = c(0,0))+
  theme1+
  labs(title = "C")+
  theme(axis.text.x = element_text(hjust = -0.95),
        axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  annotate("text", x = c(as.Date("2020-10-31"),as.Date("2021-04-15") ), y = -65000,
           label = c("2020","2021"),size = 5)+
  # geom_hline(yintercept = 0 , lty = 1,size =1,color = "grey30")+
  scale_fill_manual("WHO Region",values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,1,5,4,9)]),
                    labels = c(
                      "African Region",
                      "Region of the Americas",
                      "Eastern Mediterranean Region", 
                      "European Region",
                      "South-East Asia Region",
                      "Western Pacific Region"))->p1

tmp4$prop <- tmp4$number_detections_variant * 100/tmp4$new_cases
tmp4$PP <- tmp4$number_detections_variant * 100/tmp4$new_cases
factor(tmp4$WHO_region, levels =
         c( "Global","AFR" ,"AMR","EMR" , "EUR", "SEAR", "WPR")) -> tmp4$WHO_region

ggplot() +
  geom_line(data = tmp4[ tmp4$date >= as.Date("2020-08-31"),], size =1.2,
            aes(x = date, y = number_detections_variant * 100/new_cases,color = WHO_region))+
  geom_point(data = tmp4[ tmp4$date >= as.Date("2020-08-31"),], size =1.7,
             aes(x = date, y = number_detections_variant * 100/new_cases,
                 color = WHO_region),shape = 21,fill = "white")+
  scale_x_date("Date of collection",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(-0.2, 12),xlim = c(as.Date("2020-08-25"),as.Date("2021-09-19") ),clip = "off", expand = c(0,0))+
  scale_y_continuous("Proportion of infections sequenced per week (%)", breaks = seq(0,12,3),
                     labels = str_remove_all(format((seq(0,12,3)), big.mark = ","), " "))+
  theme1+
  # theme(legend.position = "none")+
  labs(title = "E")+
  theme(axis.text.x = element_text(hjust = -0.95),
        axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  annotate("text", x = c(as.Date("2020-10-31"),as.Date("2021-05-10") ), y = -1.25,
           label = c("2020","2021"),size = 5)+
  scale_color_manual("WHO Region",values = c("grey",pal_npg("nrc", alpha =1)(9)[c(2,3,1,5,4,9)]),
                     labels = c("Global",
                                "African Region",
                                "Region of the Americas",
                                "Eastern Mediterranean Region", 
                                "European Region",
                                "South-East Asia Region",
                                "Western Pacific Region"))->p3