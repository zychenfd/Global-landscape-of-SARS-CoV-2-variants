#==load packages==
library(readxl)
library(dplyr)
library(stringr)
library(data.table)
library(readr)

#==read data==
mydata.variant <- read.delim("metadata.tsv") %>% 
  select(c(1:12,14)) %>% filter(Host == "Homo sapiens")

mydata.variant$country <- str_trim(sapply(str_split(mydata.variant$Location, "/"), function(x) x[1]))
mydata.variant <- data.table(mydata.variant)
mydata.variant[country %in% c("Russia"), country:=c("Russian Federation")]
mydata.variant[country %in% c("Gambia"), country:=c("The Gambia")]
mydata.variant[country %in% c("?Romania"), country:=c("Romania")] 
mydata.variant[country %in% c("ISRAEL"), country:=c("Israel")]
mydata.variant[country %in% c("Bahamas"), country:=c("The Bahamas")]
mydata.variant[country %in% c("Guinea Bissau"), country:=c("Guinea-Bissau")]
mydata.variant[country %in% c("Eswatini"), country:=c("Swaziland")] #
mydata.variant[country %in% c("Morocoo"), country:=c("Morocco")]
mydata.variant[country %in% c("North Macedonia"), country:=c("Macedonia")]
mydata.variant[country %in% c("Democratic Republic of Congo"), country:=c("Democratic Republic of the Congo")] 
mydata.variant[country %in% c("South korea"), country:=c("South Korea")]
mydata.variant[country %in% c("Republic of the Congo"), country:=c("Congo")]
mydata.variant[country %in% c("Czech Repubic","Czech republic"), country:=c("Czech Republic")]
mydata.variant[country %in% c("CotedIvoire", "Cote dIvoire","Ivory Coast"), country:=c("Cote d'Ivoire")]
mydata.variant[country %in% c("belgium", "Blegium"), country:=c("Belgium")]
mydata.variant[country %in% c("Netherlans"), country:=c("Netherlands")]
mydata.variant[country %in% c("Viet Nam"), country:=c("Vietnam")]
mydata.variant[country %in% c("Cabo Verde"), country:=c("Cape Verde")]
mydata.variant[country %in% c("Crimea"), country:=c("Ukraine")] #
unique(mydata.variant$country)[unique(mydata.variant$country) %in% WHO_country$GBD_location == F]

#==cleaning data==
#==cleaning data==
#==cleaning data==
WHO_country <- read_excel("Data/WHO_country.xlsx",sheet=1)
iso<-read_xlsx("ios_a2_a3.xlsx")
WHO1 <- left_join(WHO_country, iso, by = c("ISO3" = "Alpha-3 code"))
epi1 <- read.csv("WHO-COVID-19-global-data.csv") %>% select(c(2,3))
epi1$Country_code[epi1$Country == "Namibia"] <- "NA"
epi2 <- distinct(epi1,.keep_all = T)
epi3 <- left_join(epi2, WHO1, by = c("Country_code" = "Alpha-2 code"))

#==remove islands==
mydata.variant <- mydata.variant[mydata.variant$country %in% unique(WHO_country$GBD_location),]
island <- c("Aruba","Bonaire","Curacao","Cayman Islands","Faroe Islands",
            "French Guiana","Martinique","La Reunion","Kosovo","Guadeloupe",
            "Gibraltar","Mayotte","Sint Maarten","R¨¦union",
            "Reunion","Palestine","Guyane","Guam",
            "Northern Mariana Islands","Liechtenstein","French Polynesia",
            "Anguilla","Bermuda","British Virgin Islands","Wallis and Futuna",
            "Saint Barthelemy","Saint Martin","Sint Eustatius","Montserrat",
            "Turks and Caicos Islands",
            "French Guiana","Guadeloupe",
            "Puerto Rico", "Puerto_Rico", "Saint Barth", "Virgin Islands")
mydata.variant$subnational <- str_trim(sapply(str_split(mydata.variant$Location, "/"), function(x) x[2]))
mydata.variant$subnational1 <- str_trim(sapply(str_split(mydata.variant$Location, "/"), function(x) x[3]))
mydata.variant$subnational_T <- mydata.variant$subnational %in% island
mydata.variant$subnational1_T <- mydata.variant$subnational1 %in% island
mydata.variant$subnational1_T[mydata.variant$Location %in% c("United States / PR")] <- T

mydata.variant1 <- mydata.variant %>% filter(subnational_T == T | subnational1_T == T)
mydata.variant2 <- mydata.variant %>% filter(!(subnational_T == T | subnational1_T == T))
mydata.variant2$Lineage[is.na(mydata.variant2$Lineage)] <- "-"
mydata.variant3 <- mydata.variant2 %>% filter(Lineage != "-")
mydata.variant3 <- mydata.variant3 %>% filter(!Accession.ID %in% c("GWHBDZG01000001", "GWHBDZH01000001"))

#==lineage re-assign==
lineage <- as.data.frame(table(mydata.variant3$Lineage))
mydata.variant3$Lineage[mydata.variant3$Lineage %in% 
                          c("Q.1","Q.2","Q.3","Q.4","Q.5","Q.6","Q.7","Q.8")] <- "B.1.1.7"
mydata.variant3$Lineage[mydata.variant3$Lineage %in% 
                          c("B.1.351.1","B.1.351.2","B.1.351.3","B.1.351.4","B.1.351.5")] <- "B.1.351"
mydata.variant3$Lineage[str_detect(mydata.variant3$Lineage, "^P.1")] <- "P.1"
mydata.variant3$Lineage[str_detect(mydata.variant3$Lineage, "AY.")] <- "B.1.617.2"
mydata.variant3$Lineage[mydata.variant3$Lineage %in% c("C.37.1")] <- "C.37"
mydata.variant3$Lineage[mydata.variant3$Lineage %in% c("B.1.621.1")] <- "B.1.621"
mydata.variant3$Lineage[mydata.variant3$Lineage %in% c("B","B.1","A.1","A")] <- "Ref"
mydata.variant3$Lineage[!mydata.variant3$Lineage %in%
                          c("B.1.1.7","B.1.351","P.1","B.1.617.2","C.37","B.1.621","Ref")] <- "Others"

#==cleaning date of collection==
names(mydata.variant3)[c(11,13)] <- c("date_collect","date_submission")
date <- as.data.table(table(mydata.variant3$date_collect))
no_date <- mydata.variant3 %>% filter(is.na(date_collect)) #0
mydata.variant3 <- as.data.table(mydata.variant3)
mydata.variant3[date_collect %in% c("2021-10"), date_collect:=c("2021-10-15")]
mydata.variant3[date_collect %in% c("2021-09"), date_collect:=c("2021-09-15")]
mydata.variant3[date_collect %in% c("2021-08"), date_collect:=c("2021-08-15")]
mydata.variant3[date_collect %in% c("2021-07"), date_collect:=c("2021-07-15")]
mydata.variant3[date_collect %in% c("2021-06"), date_collect:=c("2021-06-15")]
mydata.variant3[date_collect %in% c("2021-05"), date_collect:=c("2021-05-15")]
mydata.variant3[date_collect %in% c("2021-04"), date_collect:=c("2021-04-15")]
mydata.variant3[date_collect %in% c("2021-03"), date_collect:=c("2021-03-15")]
mydata.variant3[date_collect %in% c("2021-02"), date_collect:=c("2021-02-15")]
mydata.variant3[date_collect %in% c("2021-01"), date_collect:=c("2021-01-15")]
mydata.variant3[date_collect %in% c("2020-12"), date_collect:=c("2020-12-15")]
mydata.variant3[date_collect %in% c("2020-11"), date_collect:=c("2020-11-15")]
mydata.variant3[date_collect %in% c("2020-10"), date_collect:=c("2020-10-15")]
mydata.variant3[date_collect %in% c("2020-09"), date_collect:=c("2020-09-15")]
mydata.variant3[date_collect %in% c("2020-08"), date_collect:=c("2020-08-15")]
mydata.variant3[date_collect %in% c("2020-07"), date_collect:=c("2020-07-15")]
mydata.variant3[date_collect %in% c("2020-06"), date_collect:=c("2020-06-15")]
mydata.variant3[date_collect %in% c("2020-05"), date_collect:=c("2020-05-15")]
mydata.variant3[date_collect %in% c("2020-04"), date_collect:=c("2020-04-15")]
mydata.variant3[date_collect %in% c("2020-03"), date_collect:=c("2020-03-15")]
mydata.variant3[date_collect %in% c("2020-02"), date_collect:=c("2020-02-15")]
mydata.variant3[date_collect %in% c("2020-01"), date_collect:=c("2020-01-15")]
mydata.variant3[date_collect %in% c("2019-12"), date_collect:=c("2019-12-15")]

#In terms of date of sampling, the 2019nCov will transfer 2021 (not all) into 2021-01-01, therefore need to adjust
tmp1 <- mydata.variant3 %>% filter(date_collect == c("2021-01-01"))
tmp1$Accession.ID[str_detect(tmp1$Related.ID, "EPI_ISL")] <- tmp1$Related.ID[str_detect(tmp1$Related.ID, "EPI_ISL")]
data_0101 <- read_delim("F:/gisaid_hcov-19_2021_11_25_02.tsv") #read data from GISAID that sampled in 2021-01-01
tmp2 <- left_join(tmp1, data_0101[,c(1:4,15)], by = c("Accession.ID" = "Accession ID"))
tmp3 <- tmp2 %>% filter(is.na(`Collection date`))
gis <- readRDS("F:/Output/GISAID_full_20211031.rds") #read GISAID data
gis1 <- gis %>% filter(`Collection date` %in% c("2021"))
tmp4 <- left_join(tmp3[,c(1:16)], gis1[,c(1:4,15)], by = c("Accession.ID" = "Accession ID") )
tmp5 <- tmp4 %>% filter(!is.na(`Collection date`))
tmp6 <- tmp4 %>% filter(is.na(`Collection date`)) %>% filter(!str_detect(Accession.ID, "EPI")) %>% filter(Data.Source == "GenBank")
tmp6$Accession.ID <- str_trim(sapply(str_split(tmp6$Accession.ID, "\\."), function(x) x[1]))
genb <- read.csv("F:/Sequences/GenBank_1102.csv") #read GenBank data
tmp7 <- left_join(tmp6, genb[,c(1,5)], by = c("Accession.ID" = "Accession")) # all 2021-01-01
mydata.variant3$date_collect[mydata.variant3$Virus.Strain.Name %in% tmp5$Virus.Strain.Name] <- "2021"

mydata.variant3$date_collect <- as.Date(mydata.variant3$date_collect)
no_date1 <- mydata.variant3 %>% filter(is.na(date_collect))
date1 <- as.data.table(table(mydata.variant3$date_submission))
no_date2 <- mydata.variant3 %>% filter(is.na(date_submission) | date_submission == "") 
mydata.variant3$date_submission <- as.Date(mydata.variant3$date_submission)
no_date3 <- mydata.variant3 %>% filter(is.na(date_submission))
mydata.variant4 <- mydata.variant3[!(is.na(mydata.variant3$date_collect) & 
                                     is.na(mydata.variant3$date_submission)),]

write_rds(mydata.variant4[,c(1:16)],"Total_seq_full_20211031_2.rds")
