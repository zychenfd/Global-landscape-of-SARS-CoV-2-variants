#=====load packages=======
library(readr)
library(data.table)
library(stringr)
library(seqinr)
library(readxl)
library(dplyr)

#We downloaded a batch of sequences, then make them undergo 
#variant classification in three naming systems

#==read result from PANGO tool==
p1 <- read.csv("F:/check_variant/Pango_20201215.csv")
p2 <- read.csv("F:/check_variant/Pango_0315.csv")
p3 <- read.csv("F:/check_variant/Pango_0315_2.csv")
p4 <- read.csv("F:/check_variant/Pango_0615.csv")
p5 <- read.csv("F:/check_variant/Pango_0615_2.csv")
p6 <- read.csv("F:/check_variant/Pango_0915.csv")
p7 <- read.csv("F:/check_variant/Pango_0915_2.csv")
p_data <- rbind(p1, p2, p3, p4, p5, p6, p7)
names(p_data)[1] <- "seq_name"
p_data <- p_data %>% select(c(1,2,5))
p_data$ID <- sapply(str_split(p_data$seq_name, "\\|"), function(x) x[2])
p_data$date <- sapply(str_split(p_data$seq_name, "\\|"), function(x) x[3])

#==read result from Next-clade==
n1 <- read_delim("F:/check_variant/nextclade20201215.tsv",delim="\t",col_names=F)
n2 <- read_delim("F:/check_variant/nextclade0315.tsv",delim="\t",col_names=F)
n3 <- read_delim("F:/check_variant/nextclade0315_2.tsv",delim="\t",col_names=F)
n4 <- read_delim("F:/check_variant/nextclade0615.tsv",delim="\t",col_names=F)
n5 <- read_delim("F:/check_variant/nextclade0615_2.tsv",delim="\t",col_names=F)
n6 <- read_delim("F:/check_variant/nextclade0915.tsv",delim="\t",col_names=F)
n7 <- read_delim("F:/check_variant/nextclade0915_2.tsv",delim="\t",col_names=F)
n_data <- rbind(n1, n2, n3, n4, n5, n6, n7)
colnames(n_data) <- n_data[1,]
n_data <- n_data[n_data$seqName !="seqName",] %>% select(c(1,2))
n_data$ID <- sapply(str_split(n_data$seqName, "\\|"), function(x) x[2])
n_data$date <- sapply(str_split(n_data$seqName, "\\|"), function(x) x[3])

compare <- left_join(p_data, n_data)

#==transfer variant name to compare==
#1. Next-calde
compare$clade[compare$clade %in% c("20I (Alpha, V1)")] <- "Alpha"
compare$clade[compare$clade %in% c("20H (Beta, V2)" )] <- "Beta"
compare$clade[compare$clade %in% c("20J (Gamma, V3)")] <- "Gamma"
compare$clade[compare$clade %in% c("21A (Delta)","21I (Delta)","21J (Delta)")] <- "Delta"
compare$clade[compare$clade %in% c("21G (Lambda)")] <- "Lambda"
compare$clade[compare$clade %in% c("21H (Mu)")] <- "Mu"
compare$clade[compare$clade %in% c("21B (Kappa)")] <- "Kappa"
compare$clade[compare$clade %in% c("21C (Epsilon)")] <- "Epsilon"
compare$clade[compare$clade %in% c("21E (Theta)")] <- "Theta"
compare$clade[compare$clade %in% c("21F (Iota)")] <- "Iota"
compare$clade[compare$clade %in% c("21D (Eta)")] <- "Eta"
compare$clade[compare$clade %in% c("21F (Iota)")] <- "Iota"
compare$clade[compare$clade %in% c("20E (EU1)")] <- "EU1"

#2. Scorpio.call
compare$Scorpio.call[str_detect(compare$Scorpio.call, "B.1.1.7")] <- "Alpha"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "B.1.351")] <- "Beta"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "P.1")] <- "Gamma"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "Delta")] <- "Delta"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "C.37")] <- "Lambda"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "B.1.621")] <- "Mu"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "B.1.617.1")] <- "Kappa"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "B.1.427")] <- "Epsilon"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "P.3")] <- "Theta"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "B.1.526")] <- "Iota"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "B.1.525")] <- "Eta"
compare$Scorpio.call[str_detect(compare$Scorpio.call, "P.2")] <- "Zeta"

#3. Lineage.call
compare$Lineage[compare$Lineage %in%
                  c("B.1.1.7","Q.1","Q.2","Q.3","Q.4","Q.5","Q.6","Q.7","Q.8")] <- "Alpha"
compare$Lineage[compare$Lineage %in%
                  c("B.1.351","B.1.351.1","B.1.351.2","B.1.351.3","B.1.351.4","B.1.351.5")] <- "Beta"
compare$Lineage[compare$Lineage %in%
                  c("P.1","P.1.1" ,   "P.1.14",   "P.1.2",    "P.1.16",   "P.1.15",   "P.1.12",
                    "P.1.13", "P.1.10" , "P.1.17",   "P.1.7",    "P.1.8",    "P.1.9",    "P.1.17.1",
                    "P.1.10.2", "P.1.11",   "P.1.4" ,   "P.1.6",    "P.1.3" , "P.1.5" )] <- "Gamma"
compare$Lineage[str_detect(compare$Lineage, "AY.")] <- "B.1.617.2"
compare$Lineage[compare$Lineage %in% c("B.1.617.2")] <- "Delta"
compare$Lineage[compare$Lineage %in% c("C.37","C.37.1")] <- "Lambda"
compare$Lineage[compare$Lineage %in% c("B.1.621","B.1.621.1")] <- "Mu"
compare$Lineage[compare$Lineage %in% c("B.1.617.1")] <- "Kappa"
compare$Lineage[str_detect(compare$Lineage, "B.1.617.1")] <- "Kappa"
compare$Lineage[str_detect(compare$Lineage, "B.1.427")] <- "Epsilon"
compare$Lineage[str_detect(compare$Lineage, "B.1.429")] <- "Epsilon"
compare$Lineage[str_detect(compare$Lineage, "P.3")] <- "Theta"
compare$Lineage[str_detect(compare$Lineage, "B.1.526")] <- "Iota"
compare$Lineage[str_detect(compare$Lineage, "B.1.525")] <- "Eta"
compare$Lineage[str_detect(compare$Lineage, "P.2")] <- "Zeta"
compare$Lineage[str_detect(compare$Lineage, "B.1.177")] <- "EU1"
compare$Lineage[compare$Lineage %in% c("W.4")] <- "EU1"

#==choose specific variants==
line <- compare[,c(2,4,5)] %>% filter(Lineage %in% c("Alpha","Beta","Gamma","Delta", "Lambda","Mu", 
                                                     "Kappa", "Epsilon","Theta","Iota", "Eta","EU1"))
scor <- compare[,c(3,4,5)] %>% filter(Scorpio.call %in% c("Alpha","Beta","Gamma","Delta", "Lambda","Mu",
                                                     "Kappa","Epsilon","Theta","Iota", "Eta","EU1"))
clade <- compare[,c(7,4,5)] %>% filter(clade %in% c("Alpha","Beta","Gamma","Delta", "Lambda","Mu",
                                                    "Kappa","Epsilon","Theta","Iota", "Eta","EU1"))

#==compare the consistency==
total <- full_join(line, scor)
total1 <- full_join(line, clade)

#1.Lineage.call vs. Scorpio.call
check_sco <- left_join(total[,1:2],compare, by = c("ID" = "ID"))
check_sco1 <- check_sco %>% filter(Scorpio.call != "") 
check_sco1$match <- check_sco1$Lineage.y == check_sco1$Scorpio.call
table(check_sco1$match)
table(check_sco1$date)

#1.Lineage.call vs. Next-clade
check_clade <- left_join(total1[,1:2],compare, by = c("ID" = "ID"))
check_clade1 <- check_clade %>% filter(!is.na(clade)) %>% filter(Lineage.y != "None")
check_clade1$match <- check_clade1$Lineage.y == check_clade1$clade
check_clade1 <- check_clade1 %>% filter(!ID %in% c("EPI_ISL_3185961","EPI_ISL_1335445",
     "EPI_ISL_4029425","EPI_ISL_1306330","EPI_ISL_1306297")) # remove those without Lineage.call
table(check_clade1$date[check_clade1$match == T])
table(check_clade1$match)
