library(stringr)
clu <- read.csv("Cluster.csv")
orig_data <- read.csv("Merged_Filled_meter_numeric.csv", sep=",", stringsAsFactors=FALSE)
clu$X <- as.numeric(str_extract(clu$X, "[0-9]+"))
write.csv(clu, "Cluster_meter_num.csv")
for (row in 1:nrow(clu)) {
    clu_num <- unique(clu$clus_num)
    clu1 <- subset(clu, clu$clus_num == 1)
    clu2 <- subset(clu, clu$clus_num == 2)
    clu3 <- subset(clu, clu$clus_num == 3)
    clu4 <- subset(clu, clu$clus_num == 4)
    clu5 <- subset(clu, clu$clus_num == 5)
    clu6 <- subset(clu, clu$clus_num == 6)
    clu7 <- subset(clu, clu$clus_num == 7)
    clu8 <- subset(clu, clu$clus_num == 8)
#    clu9 <- subset(clu, clu$clus_num == 9)
#    clu10 <- subset(clu, clu$clus_num == 10)
#    clu11 <- subset(clu, clu$clus_num == 11)
}
#Weekdays
clu1_data_wd <- w_days[w_days$idmeter %in% clu1$X,]
clu2_data_wd <- orig_data[orig_data$idmeter %in% clu2$X,]
clu3_data_wd <- orig_data[orig_data$idmeter %in% clu3$X,]
clu4_data_wd <- orig_data[orig_data$idmeter %in% clu4$X,]
clu5_data_wd <- orig_data[orig_data$idmeter %in% clu5$X,]
clu6_data_wd <- orig_data[orig_data$idmeter %in% clu6$X,]
clu7_data_wd <- orig_data[orig_data$idmeter %in% clu7$X,]
clu8_data_wd <- orig_data[orig_data$idmeter %in% clu8$X,]
#clu9_data <- orig_data[orig_data$idmeter %in% clu9$X,]
#clu10_data <- orig_data[orig_data$idmeter %in% clu10$X,]
#clu11_data <- orig_data[orig_data$idmeter %in% clu11$X,]

####Base load calculation
library(dplyr)
library(lubridate)
#Cluster #1 Characteristics
clu1_data$dates<-as.POSIXct((clu1_data$dates), format="%d/%m/%Y")
clu1_data1 <- clu1_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu1_data1_s <- clu1_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu1_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu1_data1_s['sum_row'] <- rowSums(clu1_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu1_data1_s['min_row'] <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu1_data1_s['max_row'] <- apply(clu1_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu1_max_load <- max(clu1_data1_s$max_row)
#Descretionary load
list.clu1<-list()
for (i in 1:nrow(clu1_data1_s))
{
list.clu1[[i]] <- clu1_data1_s[,i] - clu1_data1_s['min_row']
}
clu1_descr_df <- as.data.frame(list.clu1)
clu1_descr_df$week <- NULL
clu1_descr_load <- mean(colSums(clu1_descr_df))/24
#find the peak load at peak time (4pm to 7pm)
clu1_peak <- mean(rowSums(clu1_data1_s[18:21]))/4
#find the peak load at pre-peak time (12pm to 3pm)
clu1_prepeak <- mean(rowSums(clu1_data1_s[14:17]))/4
#find the peak load at post-peak time (8pm to 11pm)
clu1_postpeak <- mean(rowSums(clu1_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu1_morload <- mean(rowSums(clu1_data1_s[10:12]))/3
############################################################################################
#Cluster #2 Characteristics
clu2_data$dates<-as.POSIXct((clu2_data$dates), format="%d/%m/%Y")
clu2_data1 <- clu2_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu2_data1_s <- clu2_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu2_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu2_data1_s['sum_row'] <- rowSums(clu2_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu2_data1_s['min_row'] <- apply(clu2_data1_s[2:25], 1, FUN=min)
clu2_baseload <- mean(sort(clu2_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu2_data1_s['max_row'] <- apply(clu2_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu2_max_load <- max(clu2_data1_s$max_row)
#Descretionary load
list.clu2<-list()
for (i in 1:nrow(clu2_data1_s))
{
  list.clu2[[i]] <- clu2_data1_s[,i] - clu2_data1_s['min_row']
}
clu2_descr_df <- as.data.frame(list.clu2)
clu2_descr_df$week <- NULL
clu2_descr_load <- mean(colSums(clu2_descr_df))/24
#find the peak load at peak time (4pm to 7pm)
clu2_peak <- mean(rowSums(clu2_data1_s[18:21]))/4
#find the peak load at pre-peak time (12pm to 3pm)
clu2_prepeak <- mean(rowSums(clu2_data1_s[14:17]))/4
#find the peak load at post-peak time (9pm to 12am)
clu2_postpeak <- mean(rowSums(clu2_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu2_morload <- mean(rowSums(clu2_data1_s[10:12]))/3
#############################################################################################
#Cluster #3 Characteristics
clu3_data$dates<-as.POSIXct((clu3_data$dates), format="%d/%m/%Y")
clu3_data1 <- clu3_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu3_data1_s <- clu3_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu3_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu3_data1_s['sum_row'] <- rowSums(clu3_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu3_data1_s['min_row'] <- apply(clu3_data1_s[2:25], 1, FUN=min)
clu3_baseload <- mean(sort(clu3_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu3_data1_s['max_row'] <- apply(clu3_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu3_max_load <- max(clu3_data1_s$max_row)
#Descretionary load
list.clu3<-list()
for (i in 1:nrow(clu3_data1_s))
{
  list.clu3[[i]] <- clu3_data1_s[,i] - clu3_data1_s['min_row']
}
clu3_descr_df <- as.data.frame(list.clu3)
clu3_descr_df$week <- NULL
clu3_descr_load <- mean(colSums(clu3_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu3_peak <- mean(rowSums(clu3_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu3_prepeak <- mean(rowSums(clu3_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu3_postpeak <- mean(rowSums(clu3_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu3_morload <- mean(rowSums(clu3_data1_s[10:12]))/3
#############################################################################################
#Cluster #4 Characteristics
clu4_data$dates<-as.POSIXct((clu4_data$dates), format="%d/%m/%Y")
clu4_data1 <- clu4_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu4_data1_s <- clu4_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu4_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu4_data1_s['sum_row'] <- rowSums(clu4_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu4_data1_s['min_row'] <- apply(clu4_data1_s[2:25], 1, FUN=min)
clu4_baseload <- mean(sort(clu4_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu4_data1_s['max_row'] <- apply(clu4_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu4_max_load <- max(clu4_data1_s$max_row)
#Descretionary load
list.clu4<-list()
for (i in 1:nrow(clu4_data1_s))
{
  list.clu4[[i]] <- clu4_data1_s[,i] - clu4_data1_s['min_row']
}
clu4_descr_df <- as.data.frame(list.clu4)
clu4_descr_df$week <- NULL
clu4_descr_load <- mean(colSums(clu4_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu4_peak <- mean(rowSums(clu4_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu4_prepeak <- mean(rowSums(clu4_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu4_postpeak <- mean(rowSums(clu4_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu4_morload <- mean(rowSums(clu4_data1_s[10:12]))/3
#############################################################################################
#Cluster #5 Characteristics
clu5_data$dates<-as.POSIXct((clu5_data$dates), format="%d/%m/%Y")
clu5_data1 <- clu5_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu5_data1_s <- clu5_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu5_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu5_data1_s['sum_row'] <- rowSums(clu5_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu5_data1_s['min_row'] <- apply(clu5_data1_s[2:25], 1, FUN=min)
clu5_baseload <- mean(sort(clu5_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu5_data1_s['max_row'] <- apply(clu5_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu5_max_load <- max(clu5_data1_s$max_row)
#Descretionary load
list.clu5<-list()
for (i in 1:nrow(clu5_data1_s))
{
  list.clu5[[i]] <- clu5_data1_s[,i] - clu5_data1_s['min_row']
}
clu5_descr_df <- as.data.frame(list.clu5)
clu5_descr_df$week <- NULL
clu5_descr_load <- mean(colSums(clu5_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu5_peak <- mean(rowSums(clu5_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu5_prepeak <- mean(rowSums(clu5_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu5_postpeak <- mean(rowSums(clu5_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu5_morload <- mean(rowSums(clu5_data1_s[10:12]))/3
#############################################################################################
#Cluster #6 Characteristics
clu6_data$dates<-as.POSIXct((clu6_data$dates), format="%d/%m/%Y")
clu6_data1 <- clu6_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu6_data1_s <- clu6_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu6_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu6_data1_s['sum_row'] <- rowSums(clu6_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu6_data1_s['min_row'] <- apply(clu6_data1_s[2:25], 1, FUN=min)
clu6_baseload <- mean(sort(clu6_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu6_data1_s['max_row'] <- apply(clu6_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu6_max_load <- max(clu6_data1_s$max_row)
#Descretionary load
list.clu6<-list()
for (i in 1:nrow(clu6_data1_s))
{
  list.clu6[[i]] <- clu6_data1_s[,i] - clu6_data1_s['min_row']
}
clu6_descr_df <- as.data.frame(list.clu6)
clu6_descr_df$week <- NULL
clu6_descr_load <- mean(colSums(clu6_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu6_peak <- mean(rowSums(clu6_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu6_prepeak <- mean(rowSums(clu6_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu6_postpeak <- mean(rowSums(clu6_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu6_morload <- mean(rowSums(clu6_data1_s[10:12]))/3
#############################################################################################
#Cluster #7 Characteristics
clu7_data$dates<-as.POSIXct((clu7_data$dates), format="%d/%m/%Y")
clu7_data1 <- clu7_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu7_data1_s <- clu7_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu7_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu7_data1_s['sum_row'] <- rowSums(clu7_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu7_data1_s['min_row'] <- apply(clu7_data1_s[2:25], 1, FUN=min)
clu7_baseload <- mean(sort(clu7_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu7_data1_s['max_row'] <- apply(clu7_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu7_max_load <- max(clu7_data1_s$max_row)
#Descretionary load
list.clu7<-list()
for (i in 1:nrow(clu7_data1_s))
{
  list.clu7[[i]] <- clu7_data1_s[,i] - clu7_data1_s['min_row']
}
clu7_descr_df <- as.data.frame(list.clu7)
clu7_descr_df$week <- NULL
clu7_descr_load <- mean(colSums(clu7_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu7_peak <- mean(rowSums(clu7_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu7_prepeak <- mean(rowSums(clu7_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu7_postpeak <- mean(rowSums(clu7_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu7_morload <- mean(rowSums(clu7_data1_s[10:12]))/3
#############################################################################################
#Cluster #8 Characteristics
clu8_data$dates<-as.POSIXct((clu8_data$dates), format="%d/%m/%Y")
clu8_data1 <- clu8_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu8_data1_s <- clu8_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu8_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu8_data1_s['sum_row'] <- rowSums(clu8_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu8_data1_s['min_row'] <- apply(clu8_data1_s[2:25], 1, FUN=min)
clu8_baseload <- mean(sort(clu8_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu8_data1_s['max_row'] <- apply(clu8_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu8_max_load <- max(clu8_data1_s$max_row)
#Descretionary load
list.clu8<-list()
for (i in 1:nrow(clu8_data1_s))
{
  list.clu8[[i]] <- clu8_data1_s[,i] - clu8_data1_s['min_row']
}
clu8_descr_df <- as.data.frame(list.clu8)
clu8_descr_df$week <- NULL
clu8_descr_load <- mean(colSums(clu8_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu8_peak <- mean(rowSums(clu8_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu8_prepeak <- mean(rowSums(clu8_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu8_postpeak <- mean(rowSums(clu8_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu8_morload <- mean(rowSums(clu8_data1_s[10:12]))/3
#############################################################################################
#Cluster #9 Characteristics
clu9_data$dates<-as.POSIXct((clu9_data$dates), format="%d/%m/%Y")
clu9_data1 <- clu9_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu9_data1_s <- clu9_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu9_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu9_data1_s['sum_row'] <- rowSums(clu9_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu9_data1_s['min_row'] <- apply(clu9_data1_s[2:25], 1, FUN=min)
clu9_baseload <- mean(sort(clu9_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu9_data1_s['max_row'] <- apply(clu9_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu9_max_load <- max(clu9_data1_s$max_row)
#Descretionary load
list.clu9<-list()
for (i in 1:nrow(clu9_data1_s))
{
  list.clu9[[i]] <- clu9_data1_s[,i] - clu9_data1_s['min_row']
}
clu9_descr_df <- as.data.frame(list.clu9)
clu9_descr_df$week <- NULL
clu9_descr_load <- mean(colSums(clu9_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu9_peak <- mean(rowSums(clu9_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu9_prepeak <- mean(rowSums(clu9_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu9_postpeak <- mean(rowSums(clu9_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu9_morload <- mean(rowSums(clu9_data1_s[10:12]))/3
#############################################################################################
#Cluster #10 Characteristics
clu10_data$dates<-as.POSIXct((clu10_data$dates), format="%d/%m/%Y")
clu10_data1 <- clu10_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu10_data1_s <- clu10_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu10_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu10_data1_s['sum_row'] <- rowSums(clu10_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu10_data1_s['min_row'] <- apply(clu10_data1_s[2:25], 1, FUN=min)
clu10_baseload <- mean(sort(clu10_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu10_data1_s['max_row'] <- apply(clu10_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu10_max_load <- max(clu10_data1_s$max_row)
#Descretionary load
list.clu10<-list()
for (i in 1:nrow(clu10_data1_s))
{
  list.clu10[[i]] <- clu10_data1_s[,i] - clu10_data1_s['min_row']
}
clu10_descr_df <- as.data.frame(list.clu10)
clu10_descr_df$week <- NULL
clu10_descr_load <- mean(colSums(clu10_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu10_peak <- mean(rowSums(clu10_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu10_prepeak <- mean(rowSums(clu10_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu10_postpeak <- mean(rowSums(clu10_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu10_morload <- mean(rowSums(clu10_data1_s[10:12]))/3
#############################################################################################
#Cluster #9 Characteristics
clu11_data$dates<-as.POSIXct((clu11_data$dates), format="%d/%m/%Y")
clu11_data1 <- clu11_data %>% group_by(idmeter, week = week(dates)) %>% summarise_each(funs(sum), -dates)
clu11_data1_s <- clu11_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu11_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu11_data1_s['sum_row'] <- rowSums(clu11_data1_s[2:25])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu11_data1_s['min_row'] <- apply(clu11_data1_s[2:25], 1, FUN=min)
clu11_baseload <- mean(sort(clu11_data1_s$min_row)[1:3])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu11_data1_s['max_row'] <- apply(clu11_data1_s[2:25], 1, FUN=max)
#Maximum load during day
clu11_max_load <- max(clu11_data1_s$max_row)
#Descretionary load
list.clu11<-list()
for (i in 1:nrow(clu11_data1_s))
{
  list.clu11[[i]] <- clu11_data1_s[,i] - clu11_data1_s['min_row']
}
clu11_descr_df <- as.data.frame(list.clu11)
clu11_descr_df$week <- NULL
clu11_descr_load <- mean(colSums(clu11_descr_df))/24
#find the average total load at peak time (4pm to 7pm)
clu11_peak <- mean(rowSums(clu11_data1_s[18:21]))/4
#find the average total load at pre-peak time (12pm to 3pm)
clu11_prepeak <- mean(rowSums(clu11_data1_s[14:17]))/4
#find the average total load at post-peak time (9pm to 12am)
clu11_postpeak <- mean(rowSums(clu11_data1_s[22:25]))/4
#find the morning load at time (8am to 10am)
clu11_morload <- mean(rowSums(clu11_data1_s[10:12]))/3
#############################################################################################