#==load packages==
library(readxl)
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggsci)
library(stringr)
library(data.table)
library(grid)
library(patchwork)

#==read epi data==
iso<-read_xlsx("ios_a2_a3.xlsx")
WHO_country <- read_excel("Data/WHO_country.xlsx",sheet=1)
epi1 <- read.csv("Data/WHO-COVID-19-global-data.csv")
names(epi1)[1] <- "Date_reported"
epi1$Country_code[epi1$Country == "Namibia"] <- "NA"
epi1$Date_reported <- as.Date(as.character(epi1$Date_reported))
epi1 <- left_join(epi1, iso, by = c("Country_code" = "Alpha-2 code"))
epi1 <- left_join(epi1, WHO_country[,c(2:3)], by = c("Alpha-3 code" = "ISO3"))
epi1 <- epi1 %>% filter(`Alpha-3 code` %in% WHO_country$ISO3)
epi2 <- epi1 %>% filter(Date_reported == as.Date("2021-10-31"))
epi2 <- epi2[,c(2,3,6)]

#==read sequence data==
mydata.variant4 <- readRDS("F:/Output/Total_seq_full_20211031_after_dedu2_2.rds")
names(mydata.variant4)[7] <- "lineage"
mydata.variant4 <- mydata.variant4[,c(3,4,7)]
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.1.7" & date_collect < as.Date("2020-09-20")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.351" & date_collect < as.Date("2020-05-11")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "P.1" & date_collect < as.Date("2020-11-03")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.617.2" & date_collect < as.Date("2020-10-23")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "C.37" & date_collect < as.Date("2020-12-22")))
mydata.variant4 <- mydata.variant4 %>% filter(!(lineage == "B.1.621" & date_collect < as.Date("2021-01-11")))

mydata.variant4_no_date <- mydata.variant4 %>% filter(is.na(date_collect))
mydata.variant4 <- mydata.variant4 %>% filter(!is.na(date_collect)) %>% filter(date_collect <= as.Date("2021-10-31"))

#==the number of sequences by country==
vari <- mydata.variant4 %>% group_by(country) %>% summarise(total = n())
vari1 <- left_join(WHO_country,vari, by = c("GBD_location" = "country" ))

#==number of sequences by region==
region <- vari1 %>% filter(!is.na(total))
region1 <- region %>% group_by(WHO_region) %>% summarise(total1 = sum(total))
(region1$total1 / sum(region1$total1))*100
cut(vari1$total, breaks = c(-Inf,100,1000,10000,100000,Inf), right = F, 
    labels = c("[1, 100)","[100, 1000)","[1000, 10000)","[10000, 100000)","¡Ý 100000")) -> vari1$total1
table(vari1$total1)

#==get the uniform country code
iso1 <- iso[,c(1,3,4)]
vari1 <- left_join( vari1, iso1, by = c( "ISO3" = "Alpha-3 code" ))

#==join data
vari1 <- left_join(vari1, epi2,by = c( "Alpha-2 code" = "Country_code"))
vari1$total1 <- as.character(vari1$total1)
vari1$total1[vari1$Cumulative_cases == 0 ] <- "Not identified SARS-CoV-2"
vari1$total1[is.na(vari1$total1)] <- "Not uploaded sequences"

#==the proportion of sequences by country==
vari1$ratio <- round(vari1$total * 100/vari1$Cumulative_cases, digits = 2)
cut(vari1$ratio, breaks = c(-Inf,1,2.5,5,10,15,Inf),right = F, 
    labels = c("[0, 1.0)","[1.0, 2.5)","[2.5, 5.0)","[5.0, 10.0)","[10.0, 15.0)","¡Ý15.0")) -> vari1$ratio1
vari1$ratio1 <- as.character(vari1$ratio1)
vari1$ratio1[vari1$Cumulative_cases == 0 ] <- "Not identified SARS-CoV-2"
vari1$ratio1[is.na(vari1$ratio1)] <- "Not uploaded sequences"

#==read global map====
worldmap <- st_read("World_map.shp")
st_crs(worldmap)
worldmap <- st_transform(worldmap, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap$iso_a3[worldmap$name == "Taiwan"] <- "CHN"
nine <- st_read("nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap1 <- left_join(worldmap,  vari1, by=c("iso_a3" = "ISO3"))

#==define plot theme==
theme1 <-  theme(axis.line = element_blank(),
                axis.ticks = element_blank(),
                 axis.text = element_blank(),
                 legend.justification=c(0,0),
                 legend.position = c(0.01,0.08),
                 legend.background = element_blank(),
                 legend.key.size = unit(0.28,"cm"),
                 legend.key.width = unit(0.28,"cm"),
                 legend.key.height  = unit(0.28,"cm"),
                 legend.title = element_text(size = 5),
                 legend.text = element_text(size = 5),
                 legend.spacing = unit(6, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
                 legend.spacing.y = NULL,#the spacing between legends (unit)
                 axis.title = element_blank(),
                 plot.tag = element_text(size = 7),
                 plot.margin =  margin(0, 0, 0, 0, "cm"),
                 panel.background = element_rect(fill = "white"),
                 legend.box.background =element_blank(),
                 legend.box.margin=  margin(0, 0, 0, 0, "cm"),
                 panel.spacing = unit(0,"cm"),
                 panel.border = element_rect(fill='transparent',colour="transparent"),
                 legend.key = element_blank(),
                 plot.title = element_text(hjust = 0.06, size = 6, vjust = 0))

#set legend order
factor(worldmap1$total1, levels = c("[1, 100)" , "[100, 1000)","[1000, 10000)" ,                        
                                  "[10000, 100000)" ,"¡Ý 100000","Not uploaded sequences",
                                  "Not identified SARS-CoV-2")) -> worldmap1$total1
factor(worldmap1$ratio1, levels = c("[0, 1.0)","[1.0, 2.5)","[2.5, 5.0)","[5.0, 10.0)",
                                    "[10.0, 15.0)","¡Ý15.0" ,"Not uploaded sequences",
                                    "Not identified SARS-CoV-2")) -> worldmap1$ratio1

#==plot map==
ggplot() +
  geom_sf(data = worldmap1, aes(fill = total1), size = 0.15,color = "black")+
  geom_sf(data = nine, color="black",size = 0.1)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  labs(tag = "f")+
  theme(legend.position = "right")+
  scale_fill_manual("Number of genomic data \n(as of 31 Oct 2021)",
                    values = c(colorRampPalette(c("#FADDA9", "#E64B35FF"))(5),
                               pal_npg("nrc", alpha =1)(10)[10],pal_npg("nrc", alpha =1)(10)[9]), 
                    na.value="grey85",
                    labels = c("[1, 100)" , "[100, 1000)","[1000, 10000)" ,                        
                               "[10000, 100000)" ,">= 100000","Not uploaded eligible sequences",
                               "Not identified SARS-CoV-2","Data unavailable"))-> p2
p2

ggplot() +
  geom_sf(data = worldmap1, aes(fill = ratio1), size = 0.15,color = "black")+
  geom_sf(data = nine, color="black",size = 0.1)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  labs(tag = "h")+
  theme(legend.position = "right")+
  scale_fill_manual("Proportion (%) of cases sequenced\n(as of 31 Oct 2021)",
                    values = c(colorRampPalette(c("#FADDA9", "#E64B35FF"))(6),
                               pal_npg("nrc", alpha =1)(10)[10],pal_npg("nrc", alpha =1)(10)[9]), 
                    na.value="grey85",
                    labels = c("[0, 1.0)","[1.0, 2.5)","[2.5, 5.0)","[5.0, 10.0)",
                               "[10.0, 15.0)",">= 15.0" ,"Not uploaded eligible sequences",
                               "Not identified SARS-CoV-2","Data unavailable"))-> p4
p4

#==calculate the cumulatively sequenced proportion by Region==
tmp_region <- mydata.variant4 %>% group_by(country) %>% summarise(new = n())
tmp_region <- left_join(tmp_region, WHO_country, by = c("country" = "GBD_location"))
tmp_region1 <- tmp_region %>% group_by(WHO_region) %>% summarise(total = sum(new))
epi_tmp3 <- epi1 %>% filter(Date_reported == as.Date("2021-10-31")) %>% filter(`Alpha-3 code` %in% WHO_country$ISO3) %>%
  select(2:4,6,11)
epi_tmp3 <- epi_tmp3 %>% group_by(WHO_region) %>% summarise(cases = sum(Cumulative_cases))
epi_tmp3$WHO_region <- gsub("O", "", epi_tmp3$WHO_region)
tmp_region2 <- left_join(tmp_region1, epi_tmp3)
tmp_region2$prop <- round(tmp_region2$total *100/ tmp_region2$cases, 2)

#==plot the distribution by time and region
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

epi3 <- epi1 # epi data
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
length(unique(epi3$Country_code))
epi4 <- epi3 %>% group_by(WHO_region,year_week) %>% 
  summarise(new_cases = sum(New_cases))
epi4$WHO_region[epi4$WHO_region =="AFRO"] <- "AFR"  
epi4$WHO_region[epi4$WHO_region =="AMRO"] <- "AMR"
epi4$WHO_region[epi4$WHO_region =="EMRO"] <- "EMR"
epi4$WHO_region[epi4$WHO_region =="EURO"] <- "EUR"
epi4$WHO_region[epi4$WHO_region =="SEARO"] <- "SEAR"
epi4$WHO_region[epi4$WHO_region =="WPRO"] <- "WPR"
tmp3 <- left_join(tmp1, epi4)
tmp3$diff <- tmp3$new_cases - tmp3$number_detections_variant

tran_data <- data.frame(date = seq(as.Date("2020-03-02"),as.Date("2021-10-31"), 7))
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
tmp4 <- tmp4 %>% filter(date >= as.Date("2020-05-11") & date <= as.Date("2021-10-17"))
tmp4 <- as.data.frame(tmp4)
tran_data1 <- tran_data %>% filter(date >= as.Date("2020-05-11") & date <= as.Date("2021-10-17"))
tran_data1$x <- 1:length(tran_data1$date)
tmp4 <- left_join(tmp4, tran_data1)

glo <- tmp4 %>% group_by(year_week, date, x) %>%
  summarise(number_detections_variant = sum(number_detections_variant),
            new_cases = sum(new_cases))
glo$WHO_region <- "Global"
tmp4 <- full_join(tmp4, glo)
tmp4$prop <- tmp4$number_detections_variant * 100/tmp4$new_cases

theme1 <- theme(panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.2),
                axis.ticks = element_line(colour = "black", size = 0.2),
                axis.ticks.length = unit(0.2,"lines"),
                legend.position = c(0.1,0.72),
                legend.key.width = unit(0.3,"cm"),
                legend.key.height  = unit(0.3,"cm"),
                legend.key = element_blank(),
                legend.background = element_blank(),
                plot.margin = margin(0.3,0.1,0.3,0.1, "cm"),
                axis.title = element_text(size = 6),
                text = element_text(size = 6),
                legend.title = element_text(size = 5.5),
                legend.text = element_text(size = 5.5),
                axis.text.x = element_text(colour = "black",size = 6),
                axis.text.y = element_text(colour = "black",size = 6),
                axis.title.x = element_text(vjust = 0.5, hjust = 0.5),
                axis.title.y = element_text(vjust = 2, hjust = 0.5),
                plot.title = element_text(hjust = 0, size = 6, vjust = 0))

factor(tmp4$WHO_region, levels =
         c( "Global","AFR" ,"AMR","EMR" , "EUR", "SEAR", "WPR")) -> tmp4$WHO_region
tmp5 <-tmp4
tmp5$number_detections_variant[tmp5$WHO_region == "Global"] <- 0
ggplot() + 
  geom_bar(data = tmp5[tmp5$WHO_region != "Global"&
                         tmp5$date >= as.Date("2020-08-31"),], size =0.1,
           aes(x = date, y = number_detections_variant,fill = WHO_region), color = "black",stat = "identity")+
  scale_x_date("Date of collection",date_breaks = "1 month",
               date_labels = "%b")+
  scale_y_continuous("Number of genomic data (¡Á1,000)", breaks = seq(0,200000,50000),
                     labels = str_remove_all(format(abs((seq(0,200,50))), big.mark = ","), " "))+
  coord_cartesian(ylim = c(-1500, 200000),xlim = c(as.Date("2020-08-25"),as.Date("2021-10-17") ),clip = "off", expand = c(0,0))+
  theme1+
  labs(tag = "e")+
  theme(axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  scale_fill_manual("",values = c(pal_npg("nrc", alpha =1)(9)[c(9,3,1,5,4,2)]),
                    labels = c("AFR","AMR", "EMR","EUR","SEAR", "WPR"))->p1
p1

tmp4$prop <- tmp4$number_detections_variant * 100/tmp4$new_cases
tmp4$PP <- tmp4$number_detections_variant * 100/tmp4$new_cases
factor(tmp4$WHO_region, levels =
         c( "Global","AFR" ,"AMR","EMR" , "EUR", "SEAR", "WPR")) -> tmp4$WHO_region
ggplot() +
  geom_line(data = tmp4[tmp4$date >= as.Date("2020-08-31"),], size =0.5,
            aes(x = date, y = number_detections_variant * 100/new_cases,color = WHO_region))+
  geom_point(data = tmp4[ tmp4$date >= as.Date("2020-08-31"),], size =0.6,
            aes(x = date, y = number_detections_variant * 100/new_cases,
                color = WHO_region),shape = 21, fill = "white")+
  scale_x_date("Date of collection",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(-0.2, 12),xlim = c(as.Date("2020-08-25"),as.Date("2021-10-17") ),clip = "off", expand = c(0,0))+
  scale_y_continuous("Proportion of cases sequenced (%)", breaks = seq(0,12,3),
                     labels = str_remove_all(format((seq(0,12,3)), big.mark = ","), " "))+
  theme1+
  labs(tag = "g")+
  theme(
        axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  annotate("text", x = c(as.Date("2020-10-31"),as.Date("2021-05-31") ), y = -1.9,
           label = c("2020","2021"),size = 2)+
  scale_color_manual("",values = c("grey30",pal_npg("nrc", alpha =1)(9)[c(9,3,1,5,4,2)]),
                     labels = c("Global","AFR","AMR", "EMR","EUR","SEAR", "WPR"))->p3
p3


#==output==
fig_null <- ggplot()+
  theme(panel.background = element_blank())
(( fig4 | fig3 | fig2 | fig1  ) + 
    plot_layout(widths = c(1.2,0.2,0.7,1)))/((p1|fig_null )+ 
    plot_layout(widths = c(1.2,1)))/((p3|fig_null)+ plot_layout(widths = c(1.2,1))) -> fig_total
tiff("Output0106/Fig2.tiff",width = 8.2, height = 7, units = "in", compression = "lzw", res = 600)
viewport(x = 0.46, y = -0.015-0.11, width = 0.535, height = 1.2, just = c("left", "bottom")) -> vp1
viewport(x = 0.46, y = -0.32-0.1, width = 0.535, height = 1.2, just = c("left", "bottom")) -> vp2
print(fig_total)
print(p2,vp = vp1)
print(p4,vp = vp2)
dev.off()

pdf("Output0106/Fig2.pdf",width = 8.2, height = 7)
viewport(x = 0.46, y = -0.015-0.11, width = 0.535, height = 1.2, just = c("left", "bottom")) -> vp1
viewport(x = 0.46, y = -0.32-0.1, width = 0.535, height = 1.2, just = c("left", "bottom")) -> vp2
print(fig_total)
print(p2,vp = vp1)
print(p4,vp = vp2)
dev.off()
