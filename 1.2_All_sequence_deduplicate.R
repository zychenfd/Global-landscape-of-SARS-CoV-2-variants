#==load packages==
library(readr)
library(data.table)
library(stringr)
library(seqinr)
library(readxl)
library(dplyr)

# #==read sequence data==
seq <- readRDS("Total_seq_full_20211031_2.rds")

#==remove duplicates by choose the same "Virus.Strain.Name", "date_collect", and "country"
seq$time_diff <- seq$date_submission - seq$date_collect
dup_seq <- seq %>% group_by(Virus.Strain.Name, date_collect, country) %>% summarise(num = n()) %>% filter(num >= 2)
dup_seq_choose_one <- left_join(dup_seq,seq) %>% group_by(Virus.Strain.Name, date_collect, country) %>%
  summarise(time_diff = min(time_diff)) # choose the one that smallest time diff
dup_seq_choose_one1 <- left_join(dup_seq_choose_one,seq) %>% distinct(Virus.Strain.Name,
                                                                      date_collect, country, time_diff,.keep_all = T)
no_dup_seq <- seq %>% group_by(Virus.Strain.Name, date_collect, country) %>% summarise(num = n()) %>% filter(num == 1)
no_dup_seq1 <- left_join(no_dup_seq[,c(1:3)],seq)
seq2 <- rbind(no_dup_seq1, dup_seq_choose_one1)

#==remove duplicates by choosing the same Accession ID==
seq2$Accession.ID[seq2$Data.Source != "GISAID"] <- seq2$Related.ID[seq2$Data.Source != "GISAID"]
seq2$ID1 <- str_trim(sapply(str_split(seq2$Accession.ID, ","), function(x) x[1]))
seq2$ID2 <- str_trim(sapply(str_split(seq2$Accession.ID, ","), function(x) x[2]))
seq2$ID2[is.na(seq2$ID2)] <- "NAA"
seq2$Accession.ID[str_detect(seq2$ID2, "EPI_ISL")] <- seq2$ID2[str_detect(seq2$ID2, "EPI_ISL")]
seq2$Accession.ID[str_detect(seq2$ID1, "EPI_ISL")] <- seq2$ID1[str_detect(seq2$ID1, "EPI_ISL")]
seq2$Accession.ID[is.na(seq2$Accession.ID)] <- "NAA"
seq3 <- seq2 %>% filter(str_detect(Accession.ID, "EPI_ISL"))
seq3_1 <- seq2 %>% filter(!str_detect(Accession.ID, "EPI_ISL"))

dup_seq3 <- seq3 %>% group_by(Accession.ID) %>% summarise(num = n()) %>% filter(num >= 2)
dup_seq3_choose_one <- left_join(dup_seq3,seq3) %>% group_by(Accession.ID) %>%
  summarise(time_diff = min(time_diff)) # choose the one that smallest time diff
dup_seq3_choose_one1 <- left_join(dup_seq3_choose_one,seq3) %>% distinct(Accession.ID, time_diff,.keep_all = T)
no_dup_seq3 <- seq3 %>% group_by(Accession.ID) %>% summarise(num = n()) %>% filter(num == 1)
no_dup_seq3 <- left_join(no_dup_seq3,seq3)
seq4 <- rbind(no_dup_seq3[,-2], dup_seq3_choose_one1,seq3_1)
write_rds(seq4,"Total_seq_full_20211031_after_dedu2_2.rds")
