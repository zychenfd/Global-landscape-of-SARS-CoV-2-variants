#=====load packages=======
library(readr)
library(data.table)
library(stringr)
library(seqinr)
library(readxl)
library(dplyr)
library(patchwork)
library(ggplot2)
library(ggsci)

#Part I. read raw metadata of sequencing technology from GISAID
temp1 = list.files(path="F:/GISAID/20200630/stmeta",pattern="*.tsv")
temp2 = list.files(path="F:/GISAID/20201031/stmeta",pattern="*.tsv")
temp3 = list.files(path="F:/GISAID/20201231/stmeta",pattern="*.tsv")
temp4 = list.files(path="F:/GISAID/20210117/stmeta",pattern="*.tsv")
temp5 = list.files(path="F:/GISAID/20210131/stmeta",pattern="*.tsv")
temp6 = list.files(path="F:/GISAID/20210215/stmeta",pattern="*.tsv")
temp7 = list.files(path="F:/GISAID/20210228/stmeta",pattern="*.tsv")
temp8 = list.files(path="F:/GISAID/20210309/stmeta",pattern="*.tsv")
temp9 = list.files(path="F:/GISAID/20210315/stmeta",pattern="*.tsv")
temp10 = list.files(path="F:/GISAID/20210401/stmeta",pattern="*.tsv")
temp11 = list.files(path="F:/GISAID/20210416/stmeta",pattern="*.tsv")
temp12 = list.files(path="F:/GISAID/20210501/stmeta",pattern="*.tsv")
temp13 = list.files(path="F:/GISAID/20210505/stmeta",pattern="*.tsv")
temp14 = list.files(path="F:/GISAID/20210531/stmeta",pattern="*.tsv")
temp15 = list.files(path="F:/GISAID/20210610/stmeta",pattern="*.tsv")
temp16 = list.files(path="F:/GISAID/20210620/stmeta",pattern="*.tsv")
temp17 = list.files(path="F:/GISAID/20210630/stmeta",pattern="*.tsv")
temp18 = list.files(path="F:/GISAID/20210710/stmeta",pattern="*.tsv")
temp19 = list.files(path="F:/GISAID/20210720/stmeta",pattern="*.tsv")
temp20 = list.files(path="F:/GISAID/20210731/stmeta",pattern="*.tsv")
temp21 = list.files(path="F:/GISAID/20210810/stmeta",pattern="*.tsv")
temp22 = list.files(path="F:/GISAID/20210818/stmeta",pattern="*.tsv")
temp23 = list.files(path="F:/GISAID/20210830/stmeta",pattern="*.tsv")
temp24 = list.files(path="F:/GISAID/20210919/stmeta",pattern="*.tsv")
temp25 = list.files(path="F:/GISAID/20211031/stmeta",pattern="*.tsv")

data1 <- c()
for(i in 1:length(temp1)){
  a<-read_delim(paste0("F:/GISAID/20200630/stmeta/",temp1[i]),delim="\t",col_names=F)
  data1<-rbind(data1,a)
}

data2 <- c()
for(i in 1:length(temp2)){
  a<-read_delim(paste0("F:/GISAID/20201031/stmeta/",temp2[i]),delim="\t",col_names=F)
  data2<-rbind(data2,a)
}

data3 <- c()
for(i in 1:length(temp3)){
  a<-read_delim(paste0("F:/GISAID/20201231/stmeta/",temp3[i]),delim="\t",col_names=F)
  data3<-rbind(data3,a)
}

data4 <- c()
for(i in 1:length(temp4)){
  a<-read_delim(paste0("F:/GISAID/20210117/stmeta/",temp4[i]),delim="\t",col_names=F)
  data4<-rbind(data4,a)
}

data5 <- c()
for(i in 1:length(temp5)){
  a<-read_delim(paste0("F:/GISAID/20210131/stmeta/",temp5[i]),delim="\t",col_names=F)
  data5<-rbind(data5,a)
}

data6 <- c()
for(i in 1:length(temp6)){
  a<-read_delim(paste0("F:/GISAID/20210215/stmeta/",temp6[i]),delim="\t",col_names=F)
  data6<-rbind(data6,a)
}

data7 <- c()
for(i in 1:length(temp7)){
  a<-read_delim(paste0("F:/GISAID/20210228/stmeta/",temp7[i]),delim="\t",col_names=F)
  data7<-rbind(data7,a)
}

data8 <- c()
for(i in 1:length(temp8)){
  a<-read_delim(paste0("F:/GISAID/20210309/stmeta/",temp8[i]),delim="\t",col_names=F)
  data8<-rbind(data8,a)
}

data9 <- c()
for(i in 1:length(temp9)){
  a<-read_delim(paste0("F:/GISAID/20210315/stmeta/",temp9[i]),delim="\t",col_names=F)
  data9<-rbind(data9,a)
}

data10 <- c()
for(i in 1:length(temp10)){
  a<-read_delim(paste0("F:/GISAID/20210401/stmeta/",temp10[i]),delim="\t",col_names=F)
  data10<-rbind(data10,a)
}

data11 <- c()
for(i in 1:length(temp11)){
  a<-read_delim(paste0("F:/GISAID/20210416/stmeta/",temp11[i]),delim="\t",col_names=F)
  data11<-rbind(data11,a)
}

data12 <- c()
for(i in 1:length(temp12)){
  a<-read_delim(paste0("F:/GISAID/20210501/stmeta/",temp12[i]),delim="\t",col_names=F)
  data12<-rbind(data12,a)
}

data13 <- c()
for(i in 1:length(temp13)){
  a<-read_delim(paste0("F:/GISAID/20210505/stmeta/",temp13[i]),delim="\t",col_names=F)
  data13<-rbind(data13,a)
}

data14 <- c()
for(i in 1:length(temp14)){
  a<-read_delim(paste0("F:/GISAID/20210531/stmeta/",temp14[i]),delim="\t",col_names=F)
  data14<-rbind(data14,a)
}

data15 <- c()
for(i in 1:length(temp15)){
  a<-read_delim(paste0("F:/GISAID/20210610/stmeta/",temp15[i]),delim="\t",col_names=F)
  data15<-rbind(data15,a)
}

data16 <- c()
for(i in 1:length(temp16)){
  a<-read_delim(paste0("F:/GISAID/20210620/stmeta/",temp16[i]),delim="\t",col_names=F)
  data16<-rbind(data16,a)
}

data17 <- c()
for(i in 1:length(temp17)){
  a<-read_delim(paste0("F:/GISAID/20210630/stmeta/",temp17[i]),delim="\t",col_names=F)
  data17<-rbind(data17,a)
}

data18 <- c()
for(i in 1:length(temp18)){
  a<-read_delim(paste0("F:/GISAID/20210710/stmeta/",temp18[i]),delim="\t",col_names=F)
  data18<-rbind(data18,a)
}

data19 <- c()
for(i in 1:length(temp19)){
  a<-read_delim(paste0("F:/GISAID/20210720/stmeta/",temp19[i]),delim="\t",col_names=F)
  data19<-rbind(data19,a)
}

data20 <- c()
for(i in 1:length(temp20)){
  a<-read_delim(paste0("F:/GISAID/20210731/stmeta/",temp20[i]),delim="\t",col_names=F)
  data20<-rbind(data20,a)
}

data21 <- c()
for(i in 1:length(temp21)){
  a<-read_delim(paste0("F:/GISAID/20210810/stmeta/",temp21[i]),delim="\t",col_names=F)
  data21<-rbind(data21,a)
}

data22 <- c()
for(i in 1:length(temp22)){
  a<-read_delim(paste0("F:/GISAID/20210818/stmeta/",temp22[i]),delim="\t",col_names=F)
  data22<-rbind(data22,a)
}

data23 <- c()
for(i in 1:length(temp23)){
  a<-read_delim(paste0("F:/GISAID/20210830/stmeta/",temp23[i]),delim="\t",col_names=F)
  data23<-rbind(data23,a)
}

data24 <- c()
for(i in 1:length(temp24)){
  a<-read_delim(paste0("F:/GISAID/20210919/stmeta/",temp24[i]),delim="\t",col_names=F)
  data24<-rbind(data24,a)
}

data25 <- c()
for(i in 1:length(temp25)){
  a<-read_delim(paste0("F:/GISAID/20211031/stmeta/",temp25[i]),delim="\t",col_names=F)
  data25<-rbind(data25,a)
}

data <- rbind(data1,data2,data3,data4,data5,data6,data7,data8,data9,data10,
              data11,data12,data13,data14,data15,data16,data17,data18,data19,data20,
              data21,data22,data23,data24,data25) %>% filter(X14!="Clade")
colnames(data) <- data1[1,]
# write_rds(data,"F:/Output/GISAID_seq_tec_20211031.rds")

#==read total sequence data==
seq <- readRDS("F:/Output/GISAID_full_20211031.rds")
seq <- seq %>% distinct(`Virus name`,`Collection date`,country,.keep_all = T)
seq_tec <- readRDS("F:/Output/GISAID_seq_tec_20211031.rds")
seq_tec$country <- str_trim(sapply(str_split(seq_tec$Location, "/"), function(x) x[2]))
seq_tec <- seq_tec %>% distinct(`Virus name`,`Collection date`,country,.keep_all = T)

#choose the those sequences that commonly have the patient-level metadata and seq-tec metadata
seq_tech1 <- left_join(seq[,c(1,2,3,19)], seq_tec[,c(1,2,9,5)]) %>% filter(!is.na(Host))
tech <- as.data.frame(table(seq_tech1$`Sequencing technology`))
# write.csv(tech, "Data/Seq_tec.csv", row.names = F)
tech$Ori_tec <- tech$Var1
tech$Var1 <- str_to_lower(tech$Var1)

#1. First Generation Sequencing
tech$tech_type1[str_detect(tech$Var1, "sanger")] <- "First Generation Sequencing"
tech$tech_type1[tech$Var1 %in% c("abi 3730xls")] <- "First Generation Sequencing"

#2. Second Generation Sequencing
tech$tech_type2[str_detect(tech$Var1, "illumin")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "miseq")] <- "Next Generation Sequencing"
# tech$tech_type2[str_detect(tech$Var1, "mgi dnbseq-g400")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "ion torrent")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "ion proton")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "miniseq")] <- "Next Generation Sequencing"
tech$tech_type2[tech$Var1 %in% c("ion pgm","metagenomics_bgi","metagenomics_bgi",
                                 "bgiseq-500","iontorrent","ion_torrent","iontorrent s5", "illumina miseg",
                                 "ion genestudio s5 prime","iontorrent genexus","iona torrent s5","ion torren",
                                 "ion genestudio s5 system","ion torent s5","ion torrrent",
                                 "ion torren - proton","ion genestudio s5",
                                 "bioelectronseq 4000","genestudio s5",
                                 "on torrent","thermo fisher s5","thermofisher s5plus","iron torrent","noveseq 6000",
                                 "dna nanoball sequencing","s5","iseq","iseq 100","iseq100",
                                 "miiseq","artic+next500", "illumina nano miseq - nugen trio rna-seq",
                                 "illumina nano miseq - nugen trio rna-seq","illumina nano miseq",
                                 "illumina nano miseq - zymo-seq ribofree total rna library kit")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "lum")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "nextseq")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "ampliseq")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "dnbseq")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "mgi")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "novaseq")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "covidseq")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "bgi")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "dragen")] <- "Next Generation Sequencing"
tech$tech_type2[str_detect(tech$Var1, "primalseq protocol")] <- "Next Generation Sequencing"

#3. Third Generation Sequencing
tech$tech_type3[str_detect(tech$Var1, "pore")] <- "Three-generation sequencing"
tech$tech_type3[str_detect(tech$Var1, "pacbio")] <- "Three-generation sequencing"
tech$tech_type3[str_detect(tech$Var1, "gridion")] <- "Three-generation sequencing"
tech$tech_type3[str_detect(tech$Var1, "minion")] <- "Three-generation sequencing"
tech$tech_type3[str_detect(tech$Var1, "ont ")] <- "Three-generation sequencing"
tech$tech_type3[str_detect(tech$Var1, "pacific")] <- "Three-generation sequencing"
tech$tech_type3[str_detect(tech$Var1, "clearlabs")] <- "Three-generation Sequencing"
tech$tech_type3[str_detect(tech$Var1, "clear labs")] <- "Three-generation Sequencing"
tech$tech_type3[tech$Var1 %in% c("nanopre","ont","ont_artic","medaka 1.3.4","ont, illumina",
                                 "clearlabs | ont","clearlabs ont","learlabs ont","clearlabs","ngs nanopore","ont via clear labs wgs")] <- "Three-generation Sequencing"

#4. Unknown Generation Sequencing
tech$tech_type4[str_detect(tech$Var1, "bowtie2-2.3.4.3")] <- "unk"
tech$tech_type4[tech$Var1 %in% c("unknown","other","3567x","artic","genome detective 1.132",
                                 "vela-sentosa","hospital universitario marqu¨¦s de valdecilla",
                                 "laboratorio de virolog¨ªa huca","seq_instrument","sequencing instrument",
                                 "breseq v0.34; gdtools","sequencing technology","geneious 2021.1","consensus","ngs")] <- "unk"

tech1 <- tech %>% filter(is.na(tech_type1) & is.na(tech_type2) & is.na(tech_type3) & is.na(tech_type4))

#==Sequencing platform==
tech$tech_type3[tech$tech_type3 == "Three-generation Sequencing"] <- "Three-generation sequencing"
tech$plat_type1[tech$tech_type1 == "First Generation Sequencing"] <- "Sanger"

tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "primalseq protocol")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "dragen")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "covidseq")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "novaseq")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "nextseq")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "miniseq")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "lum")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "illumin")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "miseq")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "dnbseq")] <- "MGI"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "mgi")] <- "MGI"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "dnbseq-")] <- "MGI"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & tech$Var1 %in% c("dnbseq-g400","dnbseq-t7")] <- "MGI"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & tech$Var1 %in% c("bioelectronseq 4000")] <- "BioelectronSeq"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & tech$Var1 %in% c("metagenomics_bgi","metagenomics_bgi","bgiseq-500","bgi e5",
                                                                                   "dna nanoball sequencing","miseq and sanger","bgi")] <- "BGI"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & 
                  tech$Var1 %in% c("iontorrent","ion_torrent","iontorrent s5",
                  "ion genestudio s5 prime","iontorrent genexus","iona torrent s5","ion torren",
                  "ion genestudio s5 system","ion torent s5","ion torrrent",
                  "ion torren - proton","ion genestudio s5","ion pgm","genestudio s5",
                  "on torrent","thermo fisher s5","thermofisher s5plus",
                  "iron torrent","s5","ion s5 ampliseq")] <- "Ion"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & 
                  tech$Var1 %in% c("illumina miseg","illuimina miseq","noveseq 6000","iseq","iseq 100","illunina miseq",
                   "iseq100","miiseq","artic+next500","	miseq and sanger","illunima miseq",
                   "illumina nano miseq - nugen trio rna-seq","miseq and sanger","immumina miseq",
                   "illumina nano miseq - nugen trio rna-seq","illumina nano miseq",
                   "illumina nano miseq - zymo-seq ribofree total rna library kit")] <- "Illumina"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "ion ampliseq")] <- "Ion"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "ion proton")] <- "Ion"
tech$plat_type2[tech$tech_type2 == "Next Generation Sequencing" & str_detect(tech$Var1, "ion torrent")] <- "Ion"

tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "nanopre")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "nanopore")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "pacbio")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "pore")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "gridion")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "minion")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "ont ")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "clearlabs")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "clear labs")] <- "Nanopore"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & str_detect(tech$Var1, "pacific")] <- "PacBio Sequel"
tech$plat_type3[tech$tech_type3 == "Three-generation sequencing" & tech$Var1 %in% c("nanopre","ont","ont_artic","medaka 1.3.4","ont, illumina",
                                                                                    "clearlabs | ont","clearlabs ont","learlabs ont","clearlabs",
                                                                                    "ngs nanopore","ont via clear labs wgs")] <- "Nanopore"

tech1 <- tech %>% filter(is.na(tech_type1) & is.na(tech_type2) & is.na(tech_type3) & is.na(tech_type4))
tech$seq_type <- tech$tech_type3
tech$seq_type[is.na(tech$seq_type)] <- tech$tech_type2[is.na(tech$seq_type)]
tech$seq_type[is.na(tech$seq_type)] <- tech$tech_type1[is.na(tech$seq_type)]
tech$seq_type[is.na(tech$seq_type)] <- tech$tech_type4[is.na(tech$seq_type)]
tech$seq_type[tech$seq_type == "Three-generation Sequencing"] <- "Three-generation sequencing"
seq_tech2 <- left_join(seq_tech1,tech[,c(3,11)], by = c("Sequencing technology" = "Ori_tec") )
table(seq_tech2$seq_type)
x <- seq_tech2 %>% filter(is.na(seq_type))

#==plot for sequence tech==
WHO_country<-read_excel("Data/WHO_country.xlsx",sheet=1)
income_new <- read_xlsx("Data/Income_group.xlsx")
seq_tech3 <- left_join(seq_tech2, WHO_country, by = c("country" = "GBD_location"))
all(!is.na(seq_tech3$WHO_region))
seq_tech4 <- left_join(seq_tech3, income_new, by = c("ISO3" = "Code"))
seq_tech4$`Income group`[is.na(seq_tech4$`Income group`)] <- "Upper middle income"

Tec_global <- seq_tech4 %>% filter(seq_type %in% c("First Generation Sequencing","Next Generation Sequencing",
   "Three-generation sequencing")) %>% group_by(seq_type) %>% summarise(Num_tec = n())
Tec_global1 <- seq_tech4 %>% filter(seq_type %in% c("First Generation Sequencing","Next Generation Sequencing",
     "Three-generation sequencing")) %>% group_by() %>% summarise(Total_tec = n())
Tec_global$Total_tec <- sum(Tec_global1$Total_tec)

Tec_by_region <- seq_tech4 %>% filter(seq_type %in% c("First Generation Sequencing","Next Generation Sequencing",
   "Three-generation sequencing")) %>% group_by(WHO_region, seq_type) %>% summarise(Num_tec = n())
Tec_by_region1 <- seq_tech4 %>% filter(seq_type %in% c("First Generation Sequencing","Next Generation Sequencing",
    "Three-generation sequencing")) %>% group_by(WHO_region) %>% summarise(Total_tec = n())
Tec_by_region <- left_join(Tec_by_region,Tec_by_region1)

Tec_by_income <- seq_tech4 %>% filter(seq_type %in% c("First Generation Sequencing","Next Generation Sequencing",
  "Three-generation sequencing")) %>% group_by(`Income group`, seq_type) %>% summarise(Num_tec = n())
Tec_by_income1 <- seq_tech4 %>% filter(seq_type %in% c("First Generation Sequencing","Next Generation Sequencing",
   "Three-generation sequencing")) %>% group_by(`Income group`) %>% summarise(Total_tec = n())
Tec_by_income <- left_join(Tec_by_income,Tec_by_income1)

#==plot
theme1 <- theme(panel.background = element_blank(),
                # panel.grid.major.y = element_line(colour = "black", size = 0.2, color = "grey80"),
                # panel.border = element_rect(colour = "black",fill = "transparent", size = 0.4),
                axis.line = element_line(colour = "black", size = 0.2),
                axis.ticks = element_line(colour = "black", size = 0.2),
                axis.ticks.length = unit(0.2,"lines"),
                legend.position = c(0.8,0.85),
                legend.key.width = unit(0.3,"cm"),
                legend.key.height  = unit(0.3,"cm"),
                legend.key = element_blank(),
                legend.background = element_blank(),
                plot.margin = margin(0.3,0.1,0.3,0.1, "cm"),
                legend.title = element_text(size = 5.5),
                legend.text = element_text(size = 5.5),
                axis.title = element_text(size = 6),
                text = element_text(size = 6),
                axis.text = element_text(colour = "black",size = 6),
                axis.title.x = element_text(vjust = 0.5, hjust = 0.5),
                axis.title.y = element_text(vjust = 2, hjust = 0.5),
                plot.title = element_text(hjust = 0, size = 6, vjust = 0))

factor(Tec_by_region$WHO_region, levels = c("EUR","AMR","AFR","SEAR","WPR","EMR")) -> Tec_by_region$WHO_region
ggplot(data = Tec_by_region) + 
  geom_bar(aes(x = WHO_region, y = Num_tec/Total_tec, fill = seq_type), size = 0.2,
           stat = "identity", position = "stack", width = 0.5, color = "black")+
  theme1+
  theme(legend.position = "right")+
  labs(tag = "d")+
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))+
  scale_x_discrete("WHO region",labels = c( "EUR (n = 2,523k)","AMR (n = 1,764k)","AFR (n = 43k)",
                                            "SEAR (n = 92k)",
                                            "WPR (n = 251k)","EMR (n = 16k)" ))+
  scale_y_continuous("", breaks = seq(0,1,0.25),
                     labels = NULL,limits = c(-0.02,1),expand = c(0,0))+
  scale_fill_manual("Sequencing technologies", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4)]),
                    labels = c("First generation sequencing","Second generation sequencing",
                               "Third generation sequencing")) -> fig1
fig1

factor(Tec_by_income$`Income group`, levels = c("Low income",
                                       "Lower middle income",
                                       "Upper middle income",
                                       "High income")) -> Tec_by_income$`Income group`
ggplot(data = Tec_by_income) + 
  geom_bar(aes(x = `Income group`, y = Num_tec/Total_tec, fill = seq_type), size = 0.2,
           stat = "identity", position = "stack", width = 0.5, color = "black")+
  theme1+
  theme(legend.position = "none")+
  labs(tag = "c")+
  scale_x_discrete(labels = c("Low (n = 7k)","Lower-middle (n = 119k)",
                               "Upper-middle (n = 264k)",
                               "High (n = 4,298k)"))+
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))+
  scale_y_continuous("", breaks = seq(0,1,0.25),
                     labels = NULL,limits = c(-0.02,1),expand = c(0,0))+
  scale_fill_manual("Sequencing technologies", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4)]) ,
                    labels = c("First generation sequencing","Second generation sequencing",
                               "Third generation sequencing")) -> fig2

ggplot(data = Tec_global) + 
  geom_bar(aes(x = "Global (n = 4,688k)", y = Num_tec/Total_tec, fill = seq_type),size = 0.2, 
           stat = "identity", position = "stack", width = 0.5, color = "black")+
  theme1+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))+
  scale_x_discrete("")+
  labs(tag = "b")+
  scale_y_continuous("Proportion (%)", breaks = seq(0,1,0.25),
                     labels = seq(0,100,25),limits = c(-0.02,1),expand = c(0,0))+
  scale_fill_manual("Sequencing technologies", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4)]),
                    labels = c("First generation sequencing","Second generation sequencing",
                               "Third generation sequencing")) -> fig3

#==plot for sequence platform==
seq_plat2 <- left_join(seq_tech1,tech[,c(3,8)], by = c( "Sequencing technology" = "Ori_tec"))
seq_plat3 <- left_join(seq_tech1,tech[,c(3,9)], by = c( "Sequencing technology" = "Ori_tec"))
seq_plat4 <- left_join(seq_tech1,tech[,c(3,10)], by = c( "Sequencing technology" = "Ori_tec"))

table1 <- as.data.frame(table(seq_plat2$plat_type1))
table2 <- as.data.frame(table(seq_plat3$plat_type2))
table3 <- as.data.frame(table(seq_plat4$plat_type3))
table4 <- rbind(table1, table2, table3)
table4$Freq[table4$Var1 == "Illumina"] <- table4$Freq[table4$Var1 == "Illumina"] +
  sum(tech$Freq[str_detect(tech$Var1, "illumina miseq; bg")])
table4$Var1 <- as.character(table4$Var1)
table4$Var1[table4$Var1 == "Ion"] <- "Ion torrent"

factor(table4$Var1, levels = table4$Var1[order(table4$Freq)][8:1]) -> table4$Var1
ggplot(data = table4) + 
  geom_bar(aes(x = Var1, y = Freq, fill = pal_npg("nrc", alpha =1)(9)[c(1)]),size = 0.2, 
           stat = "identity",  width = 0.5, color = "black")+
  theme1+
  theme(legend.position = "none")+
  theme(axis.text.x = element_text(angle = 45,vjust = 1, hjust = 1))+
  # coord_flip()+
  labs(tag = "a")+
  scale_x_discrete("Sequencing platforms")+
  scale_y_continuous("Number of sequences (million)", breaks = seq(0,4000000, 1000000),
                     labels = seq(0,4,1),limits = c(-50000,4000000),expand = c(0,0)) -> fig4
fig4