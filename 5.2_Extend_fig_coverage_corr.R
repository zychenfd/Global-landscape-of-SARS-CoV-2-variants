#==load packages==
library(readr)
library(data.table)
library(stringr)
library(seqinr)
library(readxl)
library(dplyr)
library(ggbeeswarm)
library(patchwork)
library(ggsci)
library(sf)
library(rgdal)
library(grid)

#==read data about factors==
pop <- read.csv("Data/final_pop.csv")
HAQ <- read.csv("Data/IHME_GBD_2016_HAQ_INDEX_1990_2016_SCALED_CAUSE_VALUES_Y2018M05D23.csv")
gdp_ppp <- read_excel("Data/GDP_ppp.xls")
index <- read.csv("Data/Index_clean.csv")
country_list <- read_excel("Data/WHO_country.xlsx")
income <- read.csv("Data/pop_data_age_by_country_map.csv") # old income group
income_new <- read_xlsx("Data/Income_group.xlsx") # new income group

#==clean data==
gdp_ppp1 <- gdp_ppp[,c(2,64)] %>% filter(`Country Code` %in% country_list$ISO3)
names(gdp_ppp1)[2] <- "GDP_total"
gdp_ppp2 <- left_join(gdp_ppp1, pop[,1:3], by = c("Country Code" = "alpha.3"))
HAQ1 <- HAQ %>% filter(year_id == 2016) %>% filter(indicator_name == "Healthcare Access and Quality Index") %>%
  filter(ihme_loc_id %in% country_list$ISO3)
names(HAQ1)[8] <- "HAQ"
income1 <- income[,c(1,3)] %>% filter(ADM0_A3 %in% country_list$ISO3)
table(income1$INCOME_GRP)
income1$INCOME_GRP[income1$INCOME_GRP %in% c("2. High income: nonOECD", "1. High income: OECD")] <- "High income"
income1$INCOME_GRP[income1$INCOME_GRP %in% c("3. Upper middle income")] <- "Upper middle income"
income1$INCOME_GRP[income1$INCOME_GRP %in% c("4. Lower middle income")] <- "Lower middle income"
income1$INCOME_GRP[income1$INCOME_GRP %in% c("5. Low income")] <- "Low income"

data <- left_join(index,gdp_ppp2[,c(1,2,4)], by = c("ISO3" = "Country Code"))
data1 <- left_join(data, HAQ1[,c(2,8)], by = c("ISO3" = "ihme_loc_id"))
data1 <- left_join(data1, income1, by = c("ISO3" = "ADM0_A3"))
data1 <- left_join(data1, income_new[,c(2,4)], by = c("ISO3" = "Code"))
data1$`Income group`[is.na(data1$`Income group`)] <- 
  "Upper middle income" # those countries was classified into Upper middle income group based on previous data in 2019

#==read epi data from WHO==
iso<-read_xlsx("ios_a2_a3.xlsx")
epi1 <- read.csv("Data/WHO-COVID-19-global-data.csv")
names(epi1)[1] <- "Date_reported"
epi1$Date_reported <- as.Date(as.character(epi1$Date_reported))
epi1$Country_code[epi1$Country == "Namibia"] <- "NA"
epi2 <- left_join(epi1, iso, by = c("Country_code" = "Alpha-2 code")) %>%
  filter(`Alpha-3 code` %in% country_list$ISO3)
epi3 <- epi2[,c(1,11,6)] %>% filter(Date_reported == as.Date("2021-05-01"))
epi4 <- epi2[,c(1,11,6)] %>% filter(Date_reported == as.Date("2021-09-30"))
names(epi4)[3] <- "Cumulative_cases1"
epi5 <- left_join(epi3[,c(2,3)], epi4[,c(2,3)]) %>% 
  mutate(Case = Cumulative_cases1 - Cumulative_cases) # claculate the new number of cases from May to Sep 2021

#==read sequence data==
seq <- readRDS("F:/Output/Total_seq_full_20211031_after_dedu2_2.rds")
seq1 <- seq %>% filter(date_collect >= as.Date("2021-05-01") & date_collect <= as.Date("2021-09-30"))
names(seq1)[7] <- "lineage"
seq1 <- seq1 %>% filter(!(lineage == "B.1.1.7" & date_collect < as.Date("2020-09-20")))
seq1 <- seq1 %>% filter(!(lineage == "B.1.351" & date_collect < as.Date("2020-05-11")))
seq1 <- seq1 %>% filter(!(lineage == "P.1" & date_collect < as.Date("2020-11-03")))
seq1 <- seq1 %>% filter(!(lineage == "B.1.617.2" & date_collect < as.Date("2020-10-23")))
seq1 <- seq1 %>% filter(!(lineage == "C.37" & date_collect < as.Date("2020-12-22")))
seq1 <- seq1 %>% filter(!(lineage == "B.1.621" & date_collect < as.Date("2021-01-11")))
length(unique(seq1$country))
seq_by_country <- seq1 %>% group_by(country) %>% summarise(seq_num = n())

#==combine data==
data1$Location[data1$Location == "Eswatini"] <- "Swaziland"
data2 <- left_join(data1, seq_by_country, by = c("Location" = "country"))
data2 <- left_join(data2, epi5[,c(1,4)], by = c("ISO3" = "Alpha-3 code"))
data2$seq_pro <- round(data2$seq_num * 100 / data2$Case,2)
factor(data2$`Income group`, levels = c("Low income",
                                        "Lower middle income",
                                        "Upper middle income",
                                        "High income")) -> data2$`Income group`

#==plot==
theme1 <- theme(panel.background = element_blank(),
                panel.border = element_rect(colour = "black",fill = "transparent", size = 0.3),
                axis.line = element_line(colour = "black", size = 0.3),
                axis.ticks = element_line(colour = "black", size = 0.3),
                axis.ticks.length = unit(0.2,"lines"),
                legend.position = c(0.18,0.8),
                legend.key.size = unit(0.3,"cm"),
                legend.key.width = unit(0.3,"cm"),
                legend.key.height  = unit(0.3,"cm"),
                legend.key = element_blank(),
                legend.background = element_blank(),
                plot.margin = margin(0.3,0.5,0.3,0.5, "cm"),
                axis.title = element_text(size = 6),
                text = element_text(size = 6),
                axis.text = element_text(colour = "black",size = 6),
                axis.title.x = element_text(vjust = 0.5, hjust = 0.5),
                axis.title.y = element_text(vjust = 2, hjust = 0.5),
                plot.title = element_text(hjust = 0, size = 6, vjust = 0))

#1. With SDI
ggplot(data2[(data2$seq_num >= 10)  & !is.na(data2$seq_pro),],
       aes(SDI,seq_num/Case, fill = `Income group`))+
  geom_point(size=1,shape =21, color= "grey10")+
  # geom_smooth()+
  theme1+
  geom_hline(yintercept =  0.025,linetype =2, size = 0.35)+
  geom_hline(yintercept =  0.05,linetype =2, size = 0.35, color = "blue")+
  scale_x_continuous("Socio-demographic index", breaks = seq(0,1,0.25),
                     labels = seq(0,1,0.25),limits = c(0.25,1),expand = c(0,0))+
  scale_y_continuous("Proportion of cases sequenced (%)", breaks = seq(0,0.85,0.1),
                     labels = seq(0,85,10),limits = c(-0.02,0.85),expand = c(0,0))+
  scale_fill_manual("", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4,1)])) -> fig1
fig1

#2. With HAQ index (not shown in the figure)
ggplot(data2[(data2$seq_num >= 10)  & !is.na(data2$seq_pro),],
       aes(HAQ,seq_num/Case, fill = `Income group`))+
  geom_point(size=1,shape =21, color= "grey10")+
  theme1+
  geom_hline(yintercept =  0.025,linetype =2)+
  geom_hline(yintercept =  0.05,linetype =2, color = "blue")+
  scale_x_continuous("Healthcare Access and Quality (HAQ) Index", breaks = seq(0,100,25),
                     labels = seq(0,100,25),limits = c(25,100),expand = c(0,0))+
  scale_y_continuous("Proportion of cases sequenced (%)", breaks = seq(0,0.85,0.1),
                     labels = seq(0,85,10),limits = c(-0.02,0.85),expand = c(0,0))+
  scale_fill_manual("", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4,1)])) -> fig2
fig2

#3. With GDP per capita
ggplot(data2[(data2$seq_num >= 10) & !is.na(data2$seq_pro),],
       aes(GDP_total/T_pop,seq_num/Case,fill = `Income group`))+
  geom_point(size=1,shape =21, color= "grey10")+
  theme1+
  geom_hline(yintercept =  0.025,linetype =2, size = 0.35)+
  geom_hline(yintercept =  0.05,linetype =2, size = 0.35, color = "blue")+
  scale_x_continuous("GDP per capita (¡Á 1,000)", breaks = seq(0,125000,25000),
                     labels = seq(0,125,25),limits = c(0,125000),expand = c(0,0))+
  scale_y_continuous("Proportion of cases sequenced (%)", breaks = seq(0,0.85,0.1),
                     labels = seq(0,85,10),limits = c(-0.02,0.85),expand = c(0,0))+
  scale_fill_manual("", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4,1)])) -> fig3
fig3

#==output 1==
tiff("Output0106/Extended/ED_Fig3.tif",width = 5, height = 5,
     units = "in", compression = "lzw", res = 300)
fig1+fig3+plot_annotation(tag_levels = "a")+plot_layout(nrow = 2,ncol = 1)
dev.off()

#==Sequenced coverage by income==
data3 <- data2[(data2$seq_num >= 10) & !is.na(data2$seq_pro),]
data3$color <- "white"
data3$color[data3$`Income group` == "High income" & data3$seq_pro < 2.5] <- pal_npg("nrc", alpha =1)(2)[1]
data3$color[data3$`Income group` %in% c("Low income", "Lower middle income") & 
              data3$seq_pro >= 2.5] <- pal_npg("nrc", alpha =1)(4)[3]

ggplot(data3, aes(`Income group`,seq_num/Case))+
  geom_quasirandom(size=2,shape =21, color= "grey10",fill = data3$color)+
  theme1+
  geom_hline(yintercept =  0.025,linetype =2)+
  geom_hline(yintercept =  0.05,linetype =2, color = "blue")+
  scale_y_continuous("Proportion of cases sequenced (%)", breaks = seq(0,0.85,0.1),
                     labels = seq(0,85,10),limits = c(-0.02,0.85),expand = c(0,0)) -> fig4
fig4

#==read global map====
worldmap <- st_read("World_map.shp")
st_crs(worldmap)
worldmap <- st_transform(worldmap, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap$iso_a3[worldmap$name == "Taiwan"] <- "CHN"
nine <- st_read("nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
WHO_country<-read_excel("Data/WHO_country.xlsx",sheet=1)
worldmap$type[worldmap$iso_a3 %in% data3$ISO3[data3$`Income group` == "High income" & data3$seq_pro < 2.5]] <- "a"
worldmap$type[worldmap$iso_a3 %in% data3$ISO3[data3$`Income group` %in% c("Low income", "Lower middle income") & 
                                                data3$seq_pro >= 2.5]] <- "b"

theme_map <- 
  theme(axis.ticks = element_blank(), 
        axis.line = element_blank(),
        axis.text = element_blank(),
        legend.justification=c(0,0), 
        legend.position = "none" ,
        legend.background = element_blank(),
        legend.key.size = unit(0.4,"cm"),
        legend.key.width = unit(0.15,"cm"),
        legend.title = element_text(size = 10), 
        legend.text = element_text(size = 10,lineheight=12), 
        legend.spacing = unit(5, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
        legend.spacing.y = NULL,#the spacing between legends (unit)
        axis.title = element_blank(),
        plot.margin =  margin(0.35, 0.35, 0.35, 0.35, "cm"),
        panel.background = element_rect(fill = "white"),
        legend.box.background =element_blank(),
        legend.box.margin=  margin(0, 0, 0, 0, "cm"),
        panel.spacing = unit(0,"cm"),
        panel.border = element_rect(fill='transparent',colour="transparent"),
        legend.key = element_blank(),
        plot.title = element_text(hjust = 0.5, size = 15, vjust = 0))

ggplot() + 
  geom_sf(data = worldmap, aes(fill = type), size = 0.25, color = "black")+
  geom_sf(data = nine, color="black",size = 0.1)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+ 
  theme_map+
  scale_fill_manual("", values = c(pal_npg("nrc", alpha =1)(4)[1],pal_npg("nrc", alpha =1)(4)[3]),
                    na.value = "white",
                    labels = c("Low","High")) -> fig5
fig5

#==output 2==
tiff("Output1126/E_Cove_by_income.tiff",width = 8, height = 4,
     units = "in", compression = "lzw", res = 300)
vie<-viewport(width=0.9,height=0.6,x=0.4,y=0.6)
fig4
print(fig5, vp = vie) 
dev.off()

#==Sequenced coverage plotted against incidence rate==
data3$incidence <- round(data3$Case * 100 / data3$T_pop,2)
data3$GDP_ppp <- round(data3$GDP_total / data3$T_pop,1)
data4 <- data3[data3$`Income group` %in% c("Lower middle income", "Low income"),]
data4$Location[data4$Location == "Liberia"] <- "\nLiberia"
data4$Location[data4$Location == "Nigeria"] <- "\nNigeria"
ggplot(data = data3[data3$`Income group` %in% c("Lower middle income", "Low income"),],
       aes(x = incidence, y = seq_pro))+
  geom_point()+
  geom_text(data = data4[data4$seq_pro >= 1.5,],
            aes(x = incidence+0.05, y = seq_pro),
            label = data4$Location[data4$seq_pro >= 1.5],
            hjust = 0)+
  theme1+
  scale_y_continuous("Proportion of cases sequenced (%)")+
  scale_x_continuous("COVID-19 incidence per 100 people")+
  geom_hline(yintercept =  1.5,linetype =2, color = "blue") -> fig6
fig6

data5 <- data3[data3$`Income group` %in% c("High income"),]
ggplot(data = data3[data3$`Income group` %in% c("High income"),],
       aes(x = incidence, y = seq_pro))+
  geom_point()+
  theme1+
  scale_y_continuous("Proportion of cases sequenced (%)")+
  scale_x_continuous("COVID-19 incidence per 100 people")+
  geom_hline(yintercept =  1.5,linetype =2, color = "blue") -> fig7
fig7

#==output 6==
tiff("Output1126/E_Cove_by_income2.tiff",width = 8, height = 6,
     units = "in", compression = "lzw", res = 300)
fig6
dev.off()

svg("Output1126/E_Cove_by_income2.svg",width = 8, height = 6)
fig6
dev.off()

tiff("Output1219/E_Cove_by_income3.tiff",width = 8, height = 6,
     units = "in", compression = "lzw", res = 300)
fig7
dev.off()

svg("Output1219/E_Cove_by_income3.svg",width = 8, height = 6)
fig7
dev.off()
