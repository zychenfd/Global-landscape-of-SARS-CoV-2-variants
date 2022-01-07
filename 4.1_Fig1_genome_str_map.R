#==load packages==
library(readxl)
library(sf)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggsci)
library(patchwork)

#==read genomic surveillance data==
geno <- read_xlsx("Data/Genome_str.xlsx", sheet = 4) %>% select(c(1:5))
geno$Genome <- as.character(geno$Genome)

#==stat==
length(geno$Genome[!is.na(geno$Genome)])
table(geno$WHO_region[!is.na(geno$Genome)])
table(geno$Genome[geno$WHO_region == "EUR"])
table(geno$Genome[geno$WHO_region == "AFR"])
table(geno$Genome[geno$WHO_region == "AMR"])
table(geno$Genome[geno$WHO_region == "SEAR"])
table(geno$Genome[geno$WHO_region == "WPR"])
table(geno$Genome[geno$WHO_region == "EMR"])
table(geno$Genome)
table(geno$WHO_region[geno$Genome == 3])

#==read global map==
worldmap <- st_read("World_map.shp")
st_crs(worldmap)
worldmap <- st_transform(worldmap, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))
worldmap$iso_a3[worldmap$name == "Taiwan"] <- "CHN"
nine <- st_read("nine.shp")
nine <- st_transform(nine, CRS("+proj=longlat +ellps=WGS84 +towgs84=0,0,0,0,0,0,0 +no_defs"))

#==combine genome data with map data==
worldmap1 <- left_join(worldmap, geno, by=c("iso_a3" = "ISO3"))

#==define theme for plot==
theme1 <- theme(axis.ticks = element_blank(),
                axis.line = element_blank(),
                axis.text = element_blank(),
                legend.justification=c(0,0),
                legend.position = c(0,0.1),
                legend.background = element_blank(),
                legend.key.size = unit(0.3,"cm"),
                legend.key.width = unit(0.3,"cm"),
                legend.key.height  = unit(0.3,"cm"),
                legend.title = element_text(size = 5.5,face = "bold"),
                legend.text = element_text(size = 5.5,lineheight=4),
                legend.spacing = unit(8, "cm"), legend.spacing.x = NULL, # the spacing between legends (unit)
                legend.spacing.y = NULL,#the spacing between legends (unit)
                axis.title = element_blank(),
                plot.margin =  margin(0, 0, 0, 0, "cm"),
                panel.background = element_rect(fill = "white"),
                legend.box.background =element_blank(),
                legend.box.margin=  margin(0, 0, 0, 0, "cm"),
                panel.spacing = unit(0,"cm"),
                panel.border = element_rect(fill='transparent',colour="transparent"),
                legend.key = element_blank(),
                plot.title = element_text(hjust = 0.06, size = 5.5, vjust = 0))

#==plot figure1==
ggplot() + 
  geom_sf(data = worldmap1, aes(fill = Genome), size = 0.25,color = "black")+
  geom_sf(data = nine, color="black",size = 0.2)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  scale_fill_manual("Strategy of genomic surveillance (GS)",
                    values = c(pal_npg("nrc", alpha =1)(10)[c(3,2,4,5)]), 
                    na.value="grey80",
                    labels = c("High level of routine GS",
                               "Moderate level of routine GS", 
                               "Low level of routine GS",
                               "Limited GS",
                               "Data unavailable"))-> fig1
fig1

#==read sequencing availability data==
geno <- read_xlsx("Data/Genome_str.xlsx", sheet = 4) %>% select(c(1:6))
table(geno$Capa)
worldmap1 <- left_join(worldmap,geno, by=c("iso_a3" = "ISO3"))
worldmap1$Capa <- as.character(worldmap1$Capa)

#==plot figure2==
ggplot() + 
  geom_sf(data = worldmap1, aes(fill = Capa), size = 0.25,color = "black")+
  geom_sf(data = nine, color="black",size = 0.2)+
  coord_sf(xlim = c(-170, 170), ylim = c(-57, 90))+
  theme1+
  scale_fill_manual("Sequencing availability",
                    values = c(pal_npg("nrc", alpha =1)(10)[c(2,7,1)]), 
                    na.value="grey80",
                    labels = c("High availability", 
                               "Moderate availability",
                               "Low availability",
                               "Data unavailable"))-> fig2
fig2

#==output==
tiff("Output0106/Fig1.tiff", compression = "lzw",
     width = 6, height = 5.5, res = 600, units = "in")
fig1+fig2+plot_layout(nrow = 2,ncol = 1)+plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 7))
dev.off()

pdf("Output0106/Fig1.pdf", width = 6, height = 5.5)
fig1+fig2+plot_layout(nrow = 2,ncol = 1)+plot_annotation(tag_levels = "a") & 
  theme(plot.tag = element_text(size = 7))
dev.off()