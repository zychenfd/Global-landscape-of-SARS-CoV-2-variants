#=====load packages=======
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)
library(ggsci)
library(patchwork)
library(grid)
Sys.setlocale("LC_TIME", "US")

#==read genomic data==
WHO_country<-read_excel("Data/WHO_country.xlsx",sheet=1)
mydata.variant <- readRDS("F:/Output/Total_seq_full_20211031_after_dedu2_2.rds")
names(mydata.variant)[7] <- "lineage"
mydata.variant <- mydata.variant[,c(3,4,7)]
mydata.variant1 <- mydata.variant %>% filter(!(lineage == "B.1.1.7" & date_collect < as.Date("2020-09-20")))
mydata.variant1 <- mydata.variant1 %>% filter(!(lineage == "B.1.351" & date_collect < as.Date("2020-05-11")))
mydata.variant1 <- mydata.variant1 %>% filter(!(lineage == "P.1" & date_collect < as.Date("2020-11-03")))
mydata.variant1 <- mydata.variant1 %>% filter(!(lineage == "B.1.617.2" & date_collect < as.Date("2020-10-23")))
mydata.variant1 <- mydata.variant1 %>% filter(!(lineage == "C.37" & date_collect < as.Date("2020-12-22")))
mydata.variant1 <- mydata.variant1 %>% filter(!(lineage == "B.1.621" & date_collect < as.Date("2021-01-11")))
mydata.variant1$lineage[mydata.variant1$lineage %in% c("C.37", "B.1.621")] <- "VOI"
mydata.variant1$lineage[!mydata.variant1$lineage %in% c("B.1.1.7","B.1.351","P.1","B.1.617.2","VOI")] <- "Others"
mydata.variant1 <- mydata.variant1 %>% filter(!is.na(date_collect)) %>% filter(date_collect <= as.Date("2021-10-31"))
length(unique(mydata.variant1$country)) # 4906441 obs in 168 countries # Chad has no date of collection

#==calculate the new number of variant by country, date, and lineage==
tmp <- mydata.variant1 %>% group_by(country,date_collect,lineage) %>% summarise(new = n())
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
tmp1 <- tmp %>% group_by(country,lineage, year_week) %>%summarise(number_detections_variant = sum(new))
mmp <- tmp %>% group_by(country,year_week) %>% summarise(number_sequenced = sum(new))
tmp2 <- left_join(tmp1, mmp)
tmp3 <- tmp2 %>% filter(!str_detect(year_week, "2019"))
tmp3$source <- "GISAID"
names(tmp3)[2] <-  "variant"

#======read WHO epi data=====
iso<-read_xlsx("ios_a2_a3.xlsx")
epi1 <- read.csv("Data/WHO-COVID-19-global-data.csv")
names(epi1)[1] <- "Date_reported"
epi1$Date_reported <- as.Date(as.character(epi1$Date_reported))
epi1$Country_code[epi1$Country == "Namibia"] <- "NA"

#==calculate the new number of COVID-19 by country and week==
epi1$week <- as.numeric(format(epi1$Date_reported, "%W"))
epi1$year <- as.numeric(format(epi1$Date_reported, "%Y"))
epi1$week[epi1$year == 2020] <- 1+epi1$week[epi1$year == 2020]
epi1$week[epi1$Date_reported %in% c(as.Date("2021-01-01"),
                                    as.Date("2021-01-02"),
                                    as.Date("2021-01-03"))] <- 53
epi1$year[epi1$Date_reported %in% c(as.Date("2021-01-01"),
                                    as.Date("2021-01-02"),
                                    as.Date("2021-01-03"))] <- 2020
epi1$week <- formatC(epi1$week,flag = "0", width = 2)
epi1$year_week <- paste(epi1$year,"-", epi1$week,sep = "")
epi2 <- epi1 %>% group_by(year_week,Country_code) %>% 
  summarise(new_cases = sum(New_cases))
names(epi2)[2] <- "country_code"

#======read TESSy data======
tess <- read_xlsx("Data/TESSy1.xlsx")
tess$country[tess$country == "Czechia"] <- "Czech Republic"
tess <- tess %>% filter(valid_denominator == T)
tess$variant[tess$variant %in% "B.1.1.7+E484K"] <- "B.1.1.7"
tess$variant[tess$variant %in% "AY.4.2"] <- "B.1.617.2"
tess$variant[tess$variant %in% c("C.37","B.1.621")] <- "VOI"
tess <- tess %>% filter(variant != "UNK")
tess$variant[!tess$variant %in% c("VOI","B.1.1.7","B.1.351","P.1","B.1.617.2")] <- "Others"
tess1 <- tess %>% group_by(source,country,country_code,year_week, variant,number_sequenced) %>%
  summarise(number_detections_variant = sum(number_detections_variant))
tess1$number_detections_variant[is.na(tess1$number_detections_variant)] <- 0
tess2 <- tess1 %>% filter(source == "TESSy")

#====the variant difference between GISAID and TESSy
# stat <- tess1[str_detect(tess1$year_week, "2021"),]
# unique(stat$country[stat$source == "TESSy"])
# stat <- stat %>% filter(variant %in% c("B.1.1.7","B.1.351","P.1","B.1.617.2"))
# stat1 <- stat %>% group_by(country,source) %>% summarise(tot = sum(number_detections_variant))
# stat1 <- stat1[stat1$country %in% unique(stat$country[stat$source == "TESSy"]),]
# stat2 <- stat1[seq(1,58,2),]
# stat3 <- stat1[seq(2,58,2),]
# stat4 <- cbind(stat2,stat3 )
# names(stat4) <- c("a","b","c","d","e","f")
# stat4$prop <- round(stat4$c * 100/stat4$f, digits = 1) 

#====the variant number in TESSy by country
tess_num <- tess1 %>% filter(source == "TESSy")
tess_num1 <- tess_num %>% group_by(country, variant) %>% summarise(vari = sum(number_detections_variant))
tess_num1 <- tess_num1 %>% filter(variant %in% c("B.1.1.7","B.1.351","P.1","B.1.617.2"))
tess_num2 <- dcast(tess_num1, country ~ variant, value.var = "vari")
tess_num2 <- tess_num2[,c(1:3,5,4)]
tess_num3 <- left_join(tess_num2, stat4, by = c("country" = "a"))
tess_num3 <- tess_num3[,c(1:5,11)]
# write.csv(tess_num3, "Data/num_variant_TESSy.csv", row.names = F)

#======combine
tmp3$country_code <- NA
total <- rbind(as.data.frame(tmp3), as.data.frame(tess2))
total$number_detections_variant[is.na(total$number_detections_variant)] <- 0
total1 <- total %>% group_by(country, variant, year_week) %>%
  summarise(number_sequenced = max(number_sequenced))
coun1 <- as.data.frame(table(total1$country))
total2 <- left_join(total1, total)
coun2 <- as.data.frame(table(total2$country))

total3 <- total2 %>% group_by(country, variant, year_week,number_sequenced) %>%
  summarise(number_detections_variant = max(number_detections_variant))
total3$number_detections_variant[total3$number_detections_variant <0] <- 0
total3$number_sequenced[total3$number_sequenced <0] <- 0

total3 <- left_join(total3, WHO_country, by = c("country" = "GBD_location"))
total3 <- left_join(total3, iso, by = c("ISO3" = "Alpha-3 code"))
total3 <- total3[,c(1:5,7,8,11)]
total3 <- left_join(total3, epi2, by = c("Alpha-2 code" = "country_code", "year_week" = "year_week"))
total3$new_cases[total3$new_cases <0] <- 0
total6 <- as.data.frame(total3)

#==New number cases by week==
epi3 <- left_join(epi2, iso, by = c("country_code" = "Alpha-2 code"))
epi3 <- left_join(epi3, WHO_country, by = c("Alpha-3 code" = "ISO3"))
epi4 <- epi3 %>% filter(!is.na(WHO_region))
length(unique(epi4$`Alpha-3 code`)) # 194
epi_region <- epi4 %>% group_by(year_week, WHO_region) %>%
  summarise(cf_case = sum(new_cases))

for (i in 1:length(epi_region$year_week)){
  epi_region$year[i] <- unlist(str_extract_all(epi_region$year_week[i], "\\w+"))[1]
}
for (i in 1:length(epi_region$year_week)){
  epi_region$week[i] <- unlist(str_extract_all(epi_region$year_week[i], "\\w+"))[2]
}
epi_region$week <- as.numeric(epi_region$week)
epi_region$week[epi_region$year == "2021"] <- epi_region$week[epi_region$year == "2021"] + 53
epi_global <- epi_region %>% group_by(year_week, year, week) %>%
  summarise(cf_case = sum(cf_case))

#=====analysis cumulative data====
region <- total6 %>% group_by(WHO_region,year_week,variant) %>%
  summarise(case = sum(number_detections_variant), sequence = sum(number_sequenced))

for (i in 1:length(region$year_week)){
  region$year[i] <- unlist(str_extract_all(region$year_week[i], "\\w+"))[1]
}
for (i in 1:length(region$year_week)){
  region$week[i] <- unlist(str_extract_all(region$year_week[i], "\\w+"))[2]
}
region$week <- as.numeric(region$week)
region$week[region$year == "2021"] <- region$week[region$year == "2021"] + 53
factor(region$variant, levels = c("Others","VOI", "B.1.617.2","P.1","B.1.351","B.1.1.7")) -> region$variant
theme1 <- theme(panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.25),
                panel.grid.major.y = element_line(colour = "black", size = 0.25, color = "grey80"),
                panel.border = element_rect(colour = "black",fill = "transparent", size = 0.25),
                axis.ticks = element_line(colour = "black", size = 0.25),
                axis.ticks.length = unit(0.2,"lines"),
                legend.position = "none",
                legend.direction = "horizontal",
                plot.margin = margin(0,0,0,0, "cm"),
                axis.title = element_text(size = 7),
                text = element_text(size = 6),
                axis.text.x = element_text(colour = "black",size = 6),
                axis.text.y = element_text(colour = "black",size = 6),
                axis.title.x = element_text(vjust = 0, hjust = 0.5),
                axis.title.y = element_text(vjust = 0, hjust = 0.5))

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
region <- left_join(region, tran_data)
epi_region <- left_join(epi_region, tran_data)

#==plot the regional number of variant by time==
date1 <- as.Date("2021-10-31")
ggplot() +
  geom_bar(data = region[region$WHO_region == "EUR",], 
           aes(x = date, y = case, fill = variant),size=0.05,
           position = "stack", stat = "identity", color = "grey20")+
  geom_line(data = epi_region[epi_region$WHO_region == "EUR",], 
            aes(x = date, y = cf_case *0.06, group = 1),size =0.2, color = "grey30" )+
  geom_point(data = epi_region[epi_region$WHO_region == "EUR",], 
             aes(x = date, y = cf_case *0.06),size =0.5, shape =21,alpha = 0.6, fill = "grey90", color = "grey50")+
  scale_y_continuous("No. of new variants", breaks = seq(0,120000,30000),
                     labels = paste0(str_remove_all(format((seq(0,120,30)), big.mark = ","), " "),"k"),
                     sec.axis = sec_axis(trans=~.*200/12, 
                           name="No. of new cases\n",
                           breaks = seq(0,2000000,500000),
                           labels =paste(str_remove_all(format((seq(0,2000,500)), big.mark = ","), " "),"k", sep="")))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 120000),
                  xlim = c(as.Date("2020-02-27"),date1),clip = "off", expand = c(0,0))+
  theme1+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]),
                    labels = c("Others","Variant of Interest","Delta variant",
                               "Gamma variant","Beta variant","Alpha variant"))+
  labs(title = "European Region") -> p1
p1

ggplot() + 
  geom_bar(data = region[region$WHO_region == "AMR",], 
           aes(x = date, y = case, fill = variant),size=0.05,
           position = "stack", stat = "identity", color = "grey20")+
  geom_line(data = epi_region[epi_region$WHO_region == "AMR",], 
            aes(x = date, y = cf_case *(5/140), group = 1),size =0.2, color = "grey30" )+
  geom_point(data = epi_region[epi_region$WHO_region == "AMR",], 
             aes(x = date, y = cf_case *(5/140)),size =0.5, shape =21,alpha = 0.6, fill = "grey90", color = "grey50")+
  scale_y_continuous("No. of new variants", breaks = seq(0,100000,25000),
                     labels = paste0(str_remove_all(format((seq(0,100,25)), big.mark = ","), " "),"k"),
                     sec.axis = sec_axis(trans=~.*280/10, 
                        name="No. of new cases\n",
                        breaks = seq(0,2800000,700000),
                        labels = paste(str_remove_all(format((seq(0,2800,700)), big.mark = ","), " "),"k", sep="")))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 100000),
                  xlim = c(as.Date("2020-02-27"),date1),clip = "off", expand = c(0,0))+
  theme1+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]),
                    labels = c("Others","Variant of Interest","Delta variant",
                               "Gamma variant","Beta variant","Alpha variant"))+
  labs(title = "Region of the Americas") -> p2
p2

ggplot() + 
  geom_bar(data = region[region$WHO_region == "WPR",], 
           aes(x = date, y = case, fill = variant),size=0.05,
           position = "stack", stat = "identity", color = "grey20")+
  geom_line(data = epi_region[epi_region$WHO_region == "WPR",], 
            aes(x = date, y = cf_case *(16/600), group = 1),size =0.2, color = "grey30" )+
  geom_point(data = epi_region[epi_region$WHO_region == "WPR",], 
             aes(x = date, y = cf_case *(16/600)),size =0.5, shape =21,alpha = 0.6, fill = "grey90", color = "grey50")+
  scale_y_continuous("No. of new variants", breaks = seq(0,16000,4000),
                     labels = paste0(str_remove_all(format((seq(0,16,4)), big.mark = ","), " "),"k"),
                     sec.axis = sec_axis(trans=~.*(600/16), 
                      name="No. of new cases\n",
                      breaks = seq(0,600000,150000),
                      labels = paste(str_remove_all(format((seq(0,600,150)),big.mark = ","), " "),"k", sep="")))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 16000),
                  xlim = c(as.Date("2020-02-27"),date1),clip = "off", expand = c(0,0))+
  theme1+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]),
                    labels = c("Others","Variant of Interest","Delta variant",
                               "Gamma variant","Beta variant","Alpha variant"))+
  labs(title = "Western Pacific Region") -> p3
p3

ggplot() + 
  geom_bar(data = region[region$WHO_region == "SEAR",], 
           aes(x = date, y = case, fill = variant),size=0.05,
           position = "stack", stat = "identity", color = "grey20")+
  geom_line(data = epi_region[epi_region$WHO_region == "SEAR",], 
            aes(x = date, y = cf_case *(6/3000), group = 1),size =0.2, color = "grey30" )+
  geom_point(data = epi_region[epi_region$WHO_region == "SEAR",], 
             aes(x = date, y = cf_case *(6/3000)),size = 0.5, shape =21,alpha = 0.6, fill = "grey90", color = "grey50")+
  scale_y_continuous("No. of new variants", breaks = seq(0,6000,1500),
                     labels = paste0(str_remove_all(format((seq(0,6,1.5)), big.mark = ","), " "),"k"),
                     sec.axis = sec_axis(trans=~.*500, 
                                         name="No. of new cases\n",
                                         breaks = seq(0,3000000,750000),
                                         labels = paste(str_remove_all(format((seq(0,3000,750)), big.mark = ","), " "),"k", sep="")))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 6000),
                  xlim = c(as.Date("2020-02-27"),date1),clip = "off", expand = c(0,0))+
  theme1+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(5,7,6,2)]),
                    labels = c("Others","Variant of Interest","Delta variant",
                               "Gamma variant","Beta variant","Alpha variant"))+
  labs(title = "South-East Asia Region") -> p4
p4

ggplot() + 
  geom_bar(data = region[region$WHO_region == "AFR",], 
           aes(x = date, y = case, fill = variant),size=0.05,
           position = "stack", stat = "identity", color = "grey20")+
  geom_line(data = epi_region[epi_region$WHO_region == "AFR",], 
            aes(x = date, y = cf_case *(20/2400), group = 1),size =0.2, color = "grey30" )+
  geom_point(data = epi_region[epi_region$WHO_region == "AFR",], 
             aes(x = date, y = cf_case *(20/2400)),size = 0.5, shape =21,alpha = 0.6, fill = "grey90", color = "grey50")+
  scale_y_continuous("No. of new variants", breaks = seq(0,2000,500),
                     labels = paste0(str_remove_all(format((seq(0,2,0.5)), big.mark = ","), " "),"k"),
                     sec.axis = sec_axis(trans=~.*120, 
                                         name="No. of new cases\n",
                                         breaks = seq(0,240000,60000),
                                         labels = paste(str_remove_all(format((seq(0,240,60)), big.mark = ","), " "),"k", sep="")))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 2000),
                  xlim = c(as.Date("2020-02-27"),date1),clip = "off", expand = c(0,0))+
  theme1+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]),
                    labels = c("Others","Variant of Interest","Delta variant",
                               "Gamma variant","Beta variant","Alpha variant"))+
  labs(title = "African Region") -> p5
p5

ggplot() + 
  geom_bar(data = region[region$WHO_region == "EMR",], 
           aes(x = date, y = case, fill = variant),size=0.05,
           position = "stack", stat = "identity", color = "grey20")+
  geom_line(data = epi_region[epi_region$WHO_region == "EMR",], 
            aes(x = date, y = cf_case *(1/750), group = 1),size =0.2, color = "grey30" )+
  geom_point(data = epi_region[epi_region$WHO_region == "EMR",], 
             aes(x = date, y = cf_case *(1/750)),size =0.5, shape =21,alpha = 0.6, fill = "grey90", color = "grey50")+
  scale_y_continuous("No. of new variants", breaks = seq(0,800,200),
                     labels = paste0(str_remove_all(format((seq(0,0.8,0.2)), big.mark = ","), " "),"k"),
                     sec.axis = sec_axis(trans=~.*750, 
                                         name="No. of new cases\n",
                                         breaks = seq(0,600000,150000),
                                         labels = paste(str_remove_all(format((seq(0,600,150)), big.mark = ","), " "),"k", sep="")))+
  scale_x_date("Date of collection",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 800),
                  xlim = c(as.Date("2020-02-27"),date1),clip = "off", expand = c(0,0))+
  theme1+
  theme(plot.margin = margin(0,1,0.5,0, "cm"),
        axis.title.x = element_text(vjust = -2, hjust = 0.5))+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(5,7,6,2)]),
                    labels = c("Others","Variant of Interest","Delta variant",
                               "Gamma variant","Beta variant","Alpha variant"))+
  annotate("text", x = c(as.Date("2020-07-31"),as.Date("2021-05-31") ), y = -200,
           label = c("2020","2021"),size = 2.2)+
  labs(title = "Eastern Mediterranean Region") -> p6
p6

#=====Proportion of variant by region and time====
region1 <- region %>% group_by(WHO_region,year_week,year,week) %>%
  summarise(sequence1 = sum(case))
region2 <- left_join(region, region1)
region2$prop <- round(region2$case * 100/region2$sequence1, 1 )

theme2 <- theme(panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.25),
                panel.grid.major.y = element_line(colour = "black", size = 0.25, color = "grey80"),
                panel.border = element_rect(colour = "black",fill = "transparent", size = 0.25),
                axis.ticks = element_line(colour = "black", size = 0.25),
                axis.ticks.length = unit(0.2,"lines"),
                legend.position = "none",
                legend.direction = "horizontal",
                plot.margin = margin(0,0,0,0, "cm"),
                axis.title = element_text(size = 7),
                text = element_text(size = 6),
                axis.text.x = element_text(colour = "black",size = 6),
                axis.text.y = element_text(colour = "black",size = 6),
                axis.title.x = element_text(vjust = 0, hjust = 0.5),
                axis.title.y = element_text(vjust = 0, hjust = 0.5))

ggplot() + 
  geom_bar(data = region2[region2$WHO_region == "EUR" & 
                            region2$week >= 54 & 
                            region2$week <= 96,],
           aes(x = date, y = case/sequence1, fill = variant),
           position = "stack", stat = "identity",size =0.05,color = "grey20")+
  scale_y_continuous("Proportion (%)", breaks = seq(0,1,0.25),
                     labels = seq(0,100,25))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 1), 
                  xlim = c(as.Date("2020-12-29"),date1),clip = "off", expand = c(0,0))+
  theme2+
  theme( axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]))+
  labs(title = "European Region") -> p7
p7

ggplot() + 
  geom_bar(data = region2[region2$WHO_region == "AMR" & 
                            region2$week >= 54 & 
                            region2$week <= 96,],
           aes(x = date, y = case/sequence1, fill = variant),
           position = "stack", stat = "identity",size = 0.05,color = "grey20")+
  scale_y_continuous("Proportion (%)", breaks = seq(0,1,0.25),
                     labels = seq(0,100,25))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 1), 
                  xlim = c(as.Date("2020-12-29"),date1),clip = "off", expand = c(0,0))+
  theme2+
  theme(axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]))+
  labs(title = "Region of the Americas") -> p8
p8

ggplot() + 
  geom_bar(data = region2[region2$WHO_region == "WPR" & 
                            region2$week >= 54 & 
                            region2$week <= 96,],
           aes(x = date, y = case/sequence1, fill = variant),size = 0.05,
           position = "stack", stat = "identity",color = "grey20")+
  scale_y_continuous("Proportion (%)", breaks = seq(0,1,0.25),
                     labels = seq(0,100,25))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 1), 
                  xlim = c(as.Date("2020-12-29"),date1),clip = "off", expand = c(0,0))+
  theme2+
  theme(axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]))+
  labs(title = "Western Pacific Region") -> p9
p9

ggplot() + 
  geom_bar(data = region2[region2$WHO_region == "SEAR" & 
                            region2$week >= 54 & 
                            region2$week <= 96,],
           aes(x = date, y = case/sequence1, fill = variant),size = 0.05,
           position = "stack", stat = "identity",color = "grey20")+
  scale_y_continuous("Proportion (%)", breaks = seq(0,1,0.25),
                     labels = seq(0,100,25))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 1), 
                  xlim = c(as.Date("2020-12-29"),date1),clip = "off", expand = c(0,0))+
  theme2+
  theme( axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(5,7,6,2)]))+
  labs(title = "South-East Asia Region") -> p10
p10

ggplot() + 
  geom_bar(data = region2[region2$WHO_region == "AFR" & 
                            region2$week >= 54 & 
                            region2$week <= 96,],
           aes(x = date, y = case/sequence1, fill = variant),size = 0.05,
           position = "stack", stat = "identity",color = "grey20")+
  scale_y_continuous("Proportion (%)", breaks = seq(0,1,0.25),
                     labels = seq(0,100,25))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 1), 
                  xlim = c(as.Date("2020-12-29"),date1),clip = "off", expand = c(0,0))+
  theme2+
  theme(axis.title.x = element_text(vjust = -3, hjust = 0.5))+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]))+
  labs(title = "African Region") -> p11
p11

ggplot() + 
  geom_bar(data = region2[region2$WHO_region == "EMR" & 
                            region2$week >= 54 & 
                            region2$week <= 96,],
           aes(x = date, y = case/sequence1, fill = variant),size = 0.05,
           position = "stack", stat = "identity",color = "grey20")+
  scale_y_continuous("Proportion (%)", breaks = seq(0,1,0.25),
                     labels = seq(0,100,25))+
  scale_x_date("Date of collection (2021)",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 1), 
                  xlim = c(as.Date("2020-12-29"),date1),clip = "off", expand = c(0,0))+
  theme2+
  theme(axis.title.x = element_text(vjust = -2, hjust = 0.5))+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(5,7,6,2)]))+
  labs(title = "Eastern Mediterranean Region") -> p12
p12

#==global number and proportion==
global <- region2 %>% group_by(year_week, variant, year, week)%>% summarise(case = sum(case))
global1 <- region1 %>% group_by(year_week,  year, week)%>% summarise(sequence1 = sum(sequence1))
global2 <- left_join(global, global1)
global2 <- left_join(global2, tran_data)
epi_global <- left_join(epi_global, tran_data)

ggplot() + 
  geom_bar(data = global2, 
           aes(x = date, y = case, fill = variant),
           position = "stack", stat = "identity", size=0.05,color = "grey20")+
  geom_line(data = epi_global, 
            aes(x = date, y = cf_case *(2.4/60), group = 1),size =0.2, color = "grey30" )+
  geom_point(data = epi_global, 
             aes(x = date, y = cf_case *(2.4/60)),size = 0.5, shape =21,alpha = 0.6, fill = "grey90", color = "grey50")+
  scale_y_continuous("No. of new variants", breaks = seq(0,240000,60000),
                     labels = paste0(str_remove_all(format((seq(0,240,60)), big.mark = ","), " "),"k"),
                     sec.axis = sec_axis(trans=~.*25, 
                     name="No. of new cases\n",
                     breaks = seq(0,6000000,1500000),
                     labels = paste(str_remove_all(format((seq(0,6000,1500)), big.mark = ","), " "),"k", sep="")))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 240000),
                  xlim = c(as.Date("2020-02-27"),date1),clip = "off", expand = c(0,0))+
  theme1+
  theme(
    # legend.position = c(0.19,0.74),
        legend.key.size = unit(0.3,"cm"),
        legend.key.width = unit(0.3,"cm"),
        legend.key.height  = unit(0.3,"cm"), 
        plot.margin = margin(0.6,0,0,0, "cm"))+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]),
                    labels = c("Others","Variant of Interest","Delta variant",
                               "Gamma variant","Beta variant","Alpha variant"))+
  labs(title = "Global") -> g1
g1

ggplot() + 
  geom_bar(data = global2, 
           aes(x = date, y = case, fill = variant),
           position = "stack", stat = "identity", size=0.1,color = "grey20")+
  theme1+
  theme(legend.key.size = unit(0.3,"cm"),
        legend.key.width = unit(0.3,"cm"),
        legend.key.height  = unit(0.3,"cm"))+
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
        panel.grid=element_blank())+
  scale_fill_manual("",values = c("grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)])[6:1],
                    labels = c("Others","Variant of Interest","Delta variant",
                               "Gamma variant","Beta variant","Alpha variant")[6:1]) + 
  labs(x = "")+
  guides(color = guide_legend(nrow = 1),
         fill =  guide_legend(nrow = 1))-> leg


ggplot() + 
  geom_bar(data = global2[global2$week >= 54 &
                            global2$week <= 96,],
           aes(x = date, y = case/sequence1, fill = variant),size = 0.05,
           position = "stack", stat = "identity",color = "grey20")+
  scale_y_continuous("Proportion (%)", breaks = seq(0,1,0.25),
                     labels = seq(0,100,25))+
  scale_x_date("",date_breaks = "1 month",
               date_labels = "%b")+
  coord_cartesian(ylim = c(0, 1),
                  xlim = c(as.Date("2020-12-29"),date1),clip = "off", expand = c(0,0))+
  theme2+
  scale_fill_manual("",values = c( "grey80",pal_npg("nrc", alpha =1)(10)[c(4,5,7,6,2)]))+
  labs(title = "Global") -> g2
g2

#==output==
tiff(file = "Output0106/Fig6.tiff", width =8.2, height = 9.5, 
     units = "in", compression = "lzw", res = 600)
viewport(x = 0, y = 0.97, width = 0.9, height = 0.18, just = c("left", "bottom")) -> vp1
g1+g2+p1+p7+p2+p8+p3+p9+p4+p10+p5+p11+p6+p12+
  plot_layout(nrow = 7,heights = c(1,1,1,1,1,1,1.1),widths = c(1,0.5))+
  plot_annotation(tag_levels = 'a') -> fig_t
print(fig_t)
print(leg,vp = vp1)
dev.off()

pdf(file = "Output0106/Fig6.pdf", width =8.2, height = 9.5)
viewport(x = 0, y = 0.97, width = 0.9, height = 0.18, just = c("left", "bottom")) -> vp1
g1+g2+p1+p7+p2+p8+p3+p9+p4+p10+p5+p11+p6+p12+
  plot_layout(nrow = 7,heights = c(1,1,1,1,1,1,1.1),widths = c(1,0.5))+
  plot_annotation(tag_levels = 'a') -> fig_t
print(fig_t)
print(leg,vp = vp1)
dev.off()
