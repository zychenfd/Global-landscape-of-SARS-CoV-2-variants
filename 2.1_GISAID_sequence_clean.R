#=====load packages=======
library(readr)
library(data.table)
library(stringr)
library(seqinr)
library(readxl)
library(dplyr)

#==read raw metadata from GISAID==
temp1 = list.files(path="F:/GISAID/20200630/metadata",pattern="*.tsv")
temp2 = list.files(path="F:/GISAID/20201031/metadata",pattern="*.tsv")
temp3 = list.files(path="F:/GISAID/20201231/metadata",pattern="*.tsv")
temp4 = list.files(path="F:/GISAID/20210117/metadata",pattern="*.tsv")
temp5 = list.files(path="F:/GISAID/20210131/metadata",pattern="*.tsv")
temp6 = list.files(path="F:/GISAID/20210215/metadata",pattern="*.tsv")
temp7 = list.files(path="F:/GISAID/20210228/metadata",pattern="*.tsv")
temp8 = list.files(path="F:/GISAID/20210309/metadata",pattern="*.tsv")
temp9 = list.files(path="F:/GISAID/20210315/metadata",pattern="*.tsv")
temp10 = list.files(path="F:/GISAID/20210401/metadata",pattern="*.tsv")
temp11 = list.files(path="F:/GISAID/20210416/metadata",pattern="*.tsv")
temp12 = list.files(path="F:/GISAID/20210501/metadata",pattern="*.tsv")
temp13 = list.files(path="F:/GISAID/20210505/metadata",pattern="*.tsv")
temp14 = list.files(path="20210531/metadata",pattern="*.tsv")
temp15 = list.files(path="20210610/metadata",pattern="*.tsv")
temp16 = list.files(path="20210620/metadata",pattern="*.tsv")
temp17 = list.files(path="20210630/metadata",pattern="*.tsv")
temp18 = list.files(path="20210710/metadata",pattern="*.tsv")
temp19 = list.files(path="20210720/metadata",pattern="*.tsv")
temp20 = list.files(path="20210731/metadata",pattern="*.tsv")
temp21 = list.files(path="20210810/metadata",pattern="*.tsv")
temp22 = list.files(path="20210818/metadata",pattern="*.tsv")
temp23 = list.files(path="20210830/metadata",pattern="*.tsv")
temp24 = list.files(path="20210919/metadata",pattern="*.tsv")
temp25 = list.files(path="20211031/metadata",pattern="*.tsv")

data1 <- c()
for(i in 1:length(temp1)){
  a<-read_delim(paste0("F:/GISAID/20200630/metadata/",temp1[i]),delim="\t",col_names=F)
  data1<-rbind(data1,a)
}

data2 <- c()
for(i in 1:length(temp2)){
  a<-read_delim(paste0("F:/GISAID/20201031/metadata/",temp2[i]),delim="\t",col_names=F)
  data2<-rbind(data2,a)
}

data3 <- c()
for(i in 1:length(temp3)){
  a<-read_delim(paste0("F:/GISAID/20201231/metadata/",temp3[i]),delim="\t",col_names=F)
  data3<-rbind(data3,a)
}

data4 <- c()
for(i in 1:length(temp4)){
  a<-read_delim(paste0("F:/GISAID/20210117/metadata/",temp4[i]),delim="\t",col_names=F)
  data4<-rbind(data4,a)
}

data5 <- c()
for(i in 1:length(temp5)){
  a<-read_delim(paste0("F:/GISAID/20210131/metadata/",temp5[i]),delim="\t",col_names=F)
  data5<-rbind(data5,a)
}

data6 <- c()
for(i in 1:length(temp6)){
  a<-read_delim(paste0("F:/GISAID/20210215/metadata/",temp6[i]),delim="\t",col_names=F)
  data6<-rbind(data6,a)
}

data7 <- c()
for(i in 1:length(temp7)){
  a<-read_delim(paste0("F:/GISAID/20210228/metadata/",temp7[i]),delim="\t",col_names=F)
  data7<-rbind(data7,a)
}

data8 <- c()
for(i in 1:length(temp8)){
  a<-read_delim(paste0("F:/GISAID/20210309/metadata/",temp8[i]),delim="\t",col_names=F)
  data8<-rbind(data8,a)
}

data9 <- c()
for(i in 1:length(temp9)){
  a<-read_delim(paste0("F:/GISAID/20210315/metadata/",temp9[i]),delim="\t",col_names=F)
  data9<-rbind(data9,a)
}

data10 <- c()
for(i in 1:length(temp10)){
  a<-read_delim(paste0("F:/GISAID/20210401/metadata/",temp10[i]),delim="\t",col_names=F)
  data10<-rbind(data10,a)
}

data11 <- c()
for(i in 1:length(temp11)){
  a<-read_delim(paste0("F:/GISAID/20210416/metadata/",temp11[i]),delim="\t",col_names=F)
  data11<-rbind(data11,a)
}

data12 <- c()
for(i in 1:length(temp12)){
  a<-read_delim(paste0("F:/GISAID/20210501/metadata/",temp12[i]),delim="\t",col_names=F)
  data12<-rbind(data12,a)
}

data13 <- c()
for(i in 1:length(temp13)){
  a<-read_delim(paste0("F:/GISAID/20210505/metadata/",temp13[i]),delim="\t",col_names=F)
  data13<-rbind(data13,a)
}

data14 <- c()
for(i in 1:length(temp14)){
  a<-read_delim(paste0("20210531/metadata/",temp14[i]),delim="\t",col_names=F)
  data14<-rbind(data14,a)
}

data15 <- c()
for(i in 1:length(temp15)){
  a<-read_delim(paste0("20210610/metadata/",temp15[i]),delim="\t",col_names=F)
  data15<-rbind(data15,a)
}

data16 <- c()
for(i in 1:length(temp16)){
  a<-read_delim(paste0("20210620/metadata/",temp16[i]),delim="\t",col_names=F)
  data16<-rbind(data16,a)
}

data17 <- c()
for(i in 1:length(temp17)){
  a<-read_delim(paste0("20210630/metadata/",temp17[i]),delim="\t",col_names=F)
  data17<-rbind(data17,a)
}

data18 <- c()
for(i in 1:length(temp18)){
  a<-read_delim(paste0("20210710/metadata/",temp18[i]),delim="\t",col_names=F)
  data18<-rbind(data18,a)
}

data19 <- c()
for(i in 1:length(temp19)){
  a<-read_delim(paste0("20210720/metadata/",temp19[i]),delim="\t",col_names=F)
  data19<-rbind(data19,a)
}

data20 <- c()
for(i in 1:length(temp20)){
  a<-read_delim(paste0("20210731/metadata/",temp20[i]),delim="\t",col_names=F)
  data20<-rbind(data20,a)
}

data21 <- c()
for(i in 1:length(temp21)){
  a<-read_delim(paste0("20210810/metadata/",temp21[i]),delim="\t",col_names=F)
  data21<-rbind(data21,a)
}

data22 <- c()
for(i in 1:length(temp22)){
  a<-read_delim(paste0("20210818/metadata/",temp22[i]),delim="\t",col_names=F)
  data22<-rbind(data22,a)
}

data23 <- c()
for(i in 1:length(temp23)){
  a<-read_delim(paste0("20210830/metadata/",temp23[i]),delim="\t",col_names=F)
  data23<-rbind(data23,a)
}

data24 <- c()
for(i in 1:length(temp24)){
  a<-read_delim(paste0("20210919/metadata/",temp24[i]),delim="\t",col_names=F)
  data24<-rbind(data24,a)
}

data25 <- c()
for(i in 1:length(temp25)){
  a<-read_delim(paste0("20211031/metadata/",temp25[i]),delim="\t",col_names=F)
  data25<-rbind(data25,a)
}

data <- rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,
              data11,data12,data13,data14,data15,data16,data17,data18,data19,
              data20,data21,data22,data23,data24,data25) %>% filter(X16!="Clade")
colnames(data) <- data1[1,]
data <- data %>% filter(Host %in% c("Human","Huamn","Humand","Homo sapiens","Hunan"))
data$subnational <- str_trim(sapply(str_split(data$Location, "/"), function(x) x[3]))
data$country <- str_trim(sapply(str_split(data$Location, "/"), function(x) x[2]))

#==adjust the country name and code==
WHO_country<-read_excel("Data/WHO_country.xlsx",sheet=1)
unique(data$country)[which(!unique(data$country) %in% unique(WHO_country$GBD_location))]
data <- data.table(data)
data[country %in% c("Usa","USA","USA? Ohio"), country:=c("United States")]
data[country %in% c("Russia"), country:=c("Russian Federation")]
data[country %in% c("Taiwan"), country:=c("China")] 
data[country %in% c("Hong Kong"), country:=c("China")] 
data[country %in% c("morocco"), country:=c("Morocco")]
data[country %in% c("Union of the Comoros"), country:=c("Comoros")]
data[country %in% c("Swizterland"), country:=c("Switzerland")]
data[country %in% c("Slovaia"), country:=c("Slovakia")]
data[country %in% c("Republic of the Congo","Republic of Congo"), country:=c("Congo")]
data[country %in% c("MAlta"), country:=c("Malta")]
data[country %in% c("Guinea Bissau"), country:=c("Guinea-Bissau")]
data[country %in% c("North Macedonia"), country:=c("Macedonia")]
data[country %in% c("Gambia"), country:=c("The Gambia")]
data[country %in% c("mongolia"), country:=c("Mongolia")]
data[country %in% c("Eswatini"), country:=c("Swaziland")] 
data[country %in% c("Crimea"), country:=c("Ukraine")] 
data[country %in% c("Antigua"), country:=c("Antigua and Barbuda")]
data[country %in% c("Antigua"), country:=c("Antigua and Barbuda")] 
data[country %in% c("Niogeria"), country:=c("Nigeria")]
data[country %in% c("Bahamas"), country:=c("The Bahamas")]
data[country %in% c("Czech republic"), country:=c("Czech Republic")]
data[country %in% c("kuwait"), country:=c("Kuwait")]
data[country %in% c("italy","Italia"), country:=c("Italy")]
data[country %in% c("Morocoo"), country:=c("Morocco")]
data[country %in% c("Israel;"), country:=c("Israel")]
data[country %in% c("Coatia"), country:=c("Croatia")]
data[country %in% c("Lombardy"), country:=c("Italy")]
data[country %in% c("Andorre"), country:=c("Andorra")]
data[country %in% c("SerbiaPozarevac"), country:=c("Serbia")]
data[country %in% c("japan"), country:=c("Japan")]
data[country %in% c("india"), country:=c("India")]
data[country %in% c("The Netherlands"), country:=c("Netherlands")]
data[country %in% c("St Lucia"), country:=c("Saint Lucia")]
data[country %in% c("Cote d¡¯Ivoire"), country:=c("Cote d'Ivoire")]
data[country %in% c("England"), country:=c("United Kingdom")]
data[country %in% c("Slovak Republic"), country:=c("Slovakia")]
data[country %in% c("Rio de Janeiro"), country:=c("Brazil")]
data[country %in% c("Parana"), country:=c("Brazil")]
data[country %in% c("Canary Islands"), country:=c("Spain")]

#remove island or territory
data<-data[!(country %in% c("Aruba","Bonaire","Curacao","Cayman Islands","Faroe Islands",
                                                "French Guiana","Martinique","La Reunion","Kosovo","Guadeloupe",
                                                "Gibraltar","Mayotte","Sint Maarten","R¨¦union",
                                                "Reunion","Palestine","Guyane","Guam",
                                                "Northern Mariana Islands","Liechtenstein","French Polynesia",
                                                "Anguilla","Bermuda","British Virgin Islands","Wallis and Futuna",
                                                "Saint Barthelemy","Saint Martin","Sint Eustatius","Montserrat",
                                                "Turks and Caicos Islands","Monsterrat","U.S. Virgin Islands",
                                                "Puerto Rico", "Puerto_Rico", "Saint Barth", "Virgin Islands")),]
# write_rds(data,"F:/Output/GISAID_full_20211031.rds")
