#==load packages==
library(readr)
library(data.table)
library(stringr)
library(seqinr)
library(readxl)
library(dplyr)
library(patchwork)
library(ggplot2)
library(ggsci)

#==read sequence data==
country_list <- read_excel("Data/WHO_country.xlsx")
income_new <- read_xlsx("Data/Income_group.xlsx")
seq2 <- readRDS("F:/Output/Total_seq_full_20211031_after_dedu2_2.rds")
names(seq2)[7] <- "lineage"
seq2 <- seq2 %>% filter(!(lineage == "B.1.1.7" & date_collect < as.Date("2020-09-20")))
seq2 <- seq2 %>% filter(!(lineage == "B.1.351" & date_collect < as.Date("2020-05-11")))
seq2 <- seq2 %>% filter(!(lineage == "P.1" & date_collect < as.Date("2020-11-03")))
seq2 <- seq2 %>% filter(!(lineage == "B.1.617.2" & date_collect < as.Date("2020-10-23")))
seq2 <- seq2 %>% filter(!(lineage == "C.37" & date_collect < as.Date("2020-12-22")))
seq2 <- seq2 %>% filter(!(lineage == "B.1.621" & date_collect < as.Date("2021-01-11")))
seq2 <- left_join(seq2, country_list[,c(2:4)], by = c("country" = "GBD_location"))
seq2 <- left_join(seq2, income_new[,c(2,4)], by = c("ISO3" = "Code"))
seq2$`Income group`[is.na(seq2$`Income group`)] <- 
  "Upper middle income" # those countries has been classified into Upper middle income in 2019
seq2$time_period[seq2$date_collect <= as.Date("2020-12-31") & seq2$date_collect >= as.Date("2020-01-01")] <- "2020"
seq2$time_period[seq2$date_collect <= as.Date("2021-04-30") & seq2$date_collect >= as.Date("2021-01-01")] <- "Alpha_period"
seq2$time_period[seq2$date_collect <= as.Date("2021-10-31") & seq2$date_collect >= as.Date("2021-05-01")] <- "Delta_period"
seq3 <- seq2 %>% filter(time_diff >= 0) %>% filter(!is.na(time_period))

#==t_test==
t.test(seq3$time_diff[seq3$WHO_region == "EUR"],seq3$time_diff[seq3$WHO_region == "AMR"])
t.test(seq3$time_diff[seq3$WHO_region == "EUR"],seq3$time_diff[seq3$WHO_region == "AFR"])
t.test(seq3$time_diff[seq3$WHO_region == "EUR"],seq3$time_diff[seq3$WHO_region == "SEAR"])
t.test(seq3$time_diff[seq3$WHO_region == "EUR"],seq3$time_diff[seq3$WHO_region == "WPR"])
t.test(seq3$time_diff[seq3$WHO_region == "EUR"],seq3$time_diff[seq3$WHO_region == "EMR"])
t.test(seq3$time_diff[seq3$`Income group` == "High income"],seq3$time_diff[seq3$`Income group` == "Upper middle income"])
t.test(seq3$time_diff[seq3$`Income group` == "High income"],seq3$time_diff[seq3$`Income group` == "Lower middle income"])
t.test(seq3$time_diff[seq3$`Income group` == "High income"],seq3$time_diff[seq3$`Income group` == "Low income"])

#get the upper and lower whiskers
text <- seq3 %>% group_by(WHO_region, time_period) %>% summarise(
  y25 = quantile(time_diff, 0.25),
  y50 = median(time_diff),
  y75 = quantile(time_diff, 0.75)) %>%
  mutate(IQR = y75- y25) %>%
  mutate(upper_whisker = y75 + 1.5*IQR)
text1 <- left_join(seq3, text) %>% filter(time_diff <= upper_whisker)
text2 <- text1 %>% group_by(WHO_region, time_period) %>% summarise(value = max(time_diff))
text2$value[text2$value <= 300] <- NA

text3 <- seq3 %>% group_by( `Income group`,time_period) %>% summarise(
  y25 = quantile(time_diff, 0.25),
  y50 = median(time_diff),
  y75 = quantile(time_diff, 0.75)) %>%
  mutate(IQR = y75- y25) %>%
  mutate(upper_whisker = y75 + 1.5*IQR)
text3 <- left_join(seq3, text3) %>% filter(time_diff <= upper_whisker)
text4 <- text3 %>% group_by(`Income group`,time_period) %>% summarise(value = max(time_diff))
text4$value[text4$value <= 300] <- NA

text5 <- seq3 %>% group_by(time_period) %>% summarise(
  y25 = quantile(time_diff, 0.25),
  y50 = median(time_diff),
  y75 = quantile(time_diff, 0.75)) %>%
  mutate(IQR = y75- y25) %>%
  mutate(upper_whisker = y75 + 1.5*IQR)
text5 <- left_join(seq3, text5) %>% filter(time_diff <= upper_whisker)
text6 <- text5 %>% group_by(time_period) %>% summarise(value = max(time_diff))
text6$value[text6$value <= 300] <- NA

#==plot==
theme1 <- theme(panel.background = element_blank(),
                axis.line = element_line(colour = "black", size = 0.3),
                axis.ticks = element_line(colour = "black", size = 0.3),
                axis.ticks.length = unit(0.2,"lines"),
                legend.position = c(0.8,0.85),
                legend.key = element_blank(),
                legend.background = element_blank(),
                plot.margin = margin(0.1,0.2,0.1,0.2, "cm"),
                axis.title = element_text(size = 6),
                text = element_text(size = 6),
                axis.text = element_text(colour = "black",size = 6),
                axis.title.x = element_text(vjust = 0.5, hjust = 0.5),
                axis.title.y = element_text(vjust = 2, hjust = 0.5),
                plot.title = element_text(hjust = 0, size = 6, vjust = 0))
factor(seq3$WHO_region, levels = c("EMR","AMR","AFR","SEAR","WPR","EUR")) -> seq3$WHO_region

table(seq3$WHO_region)
ggplot(data = seq3, aes(x = WHO_region, y = time_diff, fill = time_period))+
  geom_boxplot(outlier.colour = NA, size = 0.3)+
  theme1+
  theme(legend.position = "bottom")+
  geom_text(data = text2, aes(x = WHO_region, y = 290, fill = time_period, label = value),size = 2,
            width =.2, position = position_dodge(1.1))+
  scale_x_discrete("WHO region",labels = c("Eastern Mediterranean\n(n = 17k)","Americas\n(n = 1,866k)","Africa\n(n = 42k)",
                                           "South East Asia\n(n = 85k)","Western Pacific\n(n = 248k)","Europe\n(n = 2,613k)"))+
  scale_y_continuous("Turnaround time (days)", breaks = seq(0,600,50),
                     labels = seq(0,600,50),expand = c(0,0))+
  coord_cartesian(ylim = c(-5,300),clip = "on")+
  scale_fill_manual("Sampling time", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4)]),
                    labels = c("2020","Jan 2021 - Apr 2021 (Epidemic period of Alpha)",
                               "May 2021 - Oct 2021 (Epidemic period of Delta)")) -> fig1
fig1

factor(seq3$`Income group`, levels = c("Low income",
                                       "Lower middle income",
                                       "Upper middle income",
                                       "High income")) -> seq3$`Income group`
table(seq3$`Income group`)
ggplot(data = seq3, aes(x = `Income group`, y = time_diff, fill = time_period))+
  geom_boxplot(outlier.colour = NA, size = 0.3)+
  theme1+
  theme(legend.position = "none")+
  geom_text(data = text4, aes(x = `Income group`, y = 290, fill = time_period, label = value),size = 2,
            width =.2, position = position_dodge(1.1))+
  scale_x_discrete(labels = c("Low income\n(n = 7k)","Lower middle income\n(n = 112k)",
                              "Upper middle income\n(n = 261k)",
                              "High income\n(n = 4,491k)"))+
  scale_y_continuous("Turnaround time (days)", breaks = seq(0,600,50),
                     labels = seq(0,600,50),limits = c(-5,300),expand = c(0,0))+
  scale_fill_manual("Sampling time", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4)]),
                    labels = c("2020","Jan 2021 - Apr 2021 (Epidemic period of Alpha)",
                               "May 2021 - Oct 2021 (Epidemic period of Delta)")) -> fig2
fig2

ggplot(data = seq3, aes(x = "Global\n(n = 4,871k)", y = time_diff, fill = time_period))+
  geom_boxplot(outlier.colour = NA, size = 0.3)+
  theme1+
  scale_x_discrete("")+
  geom_text(data = text6, aes(x = "Global\n(n = 4,871k)", y = 290, fill = time_period, label = value),size = 2,
            width =.2, position = position_dodge(1.1))+
  theme(legend.position = "none")+
  scale_y_continuous("Turnaround time (days)", breaks = seq(0,600,50),
                     labels = seq(0,600,50),limits = c(-5,300),expand = c(0,0))+
  scale_fill_manual("Sampling time", values = c(pal_npg("nrc", alpha =1)(9)[c(2,3,4)]),
                    labels = c("2020","Jan 2021 - Apr 2021 (Epidemic period of Alpha)",
                               "May 2021 - Oct 2021 (Epidemic period of Delta)")) -> fig3
fig3

#==output==
tiff("Output0106/Extended/ED_Fig2.tif",width = 6, height = 5,
     units = "in", compression = "lzw", res = 300)
((fig3 | fig2) + plot_layout(widths = c(0.3,1))) / fig1+plot_annotation(tag_levels = "a")+
  plot_layout(nrow = 2,ncol = 1)
dev.off()
