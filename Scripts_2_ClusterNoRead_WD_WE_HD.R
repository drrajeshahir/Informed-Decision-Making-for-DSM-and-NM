setwd("C:/PhD nu kaam/Obj #2 DSM with ML")
library(stringr)
clu <- read.csv("Cluster_wise_meters.csv")
#orig_data <- read.csv("KOL_15mins_final_merged_kNN1.csv", sep=",", stringsAsFactors=FALSE)
clu$idmeter <- as.numeric(str_extract(clu$idmeter, "[0-9]+"))
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
    clu7 <- subset(clu, clu$clus_num == 7)
    clu8 <- subset(clu, clu$clus_num == 8)
    clu9 <- subset(clu, clu$clus_num == 9)
    clu10 <- subset(clu, clu$clus_num == 10)
    clu11 <- subset(clu, clu$clus_num == 11)
    clu12 <- subset(clu, clu$clus_num == 12)
}
#Weekdays
clu1_data <- w_days[w_days$idmeter %in% clu1$idmeter,]
clu2_data <- w_days[w_days$idmeter %in% clu2$idmeter,]
clu3_data <- w_days[w_days$idmeter %in% clu3$idmeter,]
clu4_data <- w_days[w_days$idmeter %in% clu4$idmeter,]
clu5_data <- w_days[w_days$idmeter %in% clu5$idmeter,]
clu6_data <- w_days[w_days$idmeter %in% clu6$idmeter,]
clu7_data <- w_days[w_days$idmeter %in% clu7$idmeter,]
clu8_data <- w_days[w_days$idmeter %in% clu8$idmeter,]
clu9_data <- w_days[w_days$idmeter %in% clu9$idmeter,]
clu10_data <- w_days[w_days$idmeter %in% clu10$idmeter,]
clu11_data <- w_days[w_days$idmeter %in% clu11$idmeter,]
clu12_data <- w_days[w_days$idmeter %in% clu12$idmeter,]

####Base load calculation
library(dplyr)
library(lubridate)
#Cluster #1 Characteristics
clu1_data$date<-as.POSIXct((clu1_data$date), format="%d/%m/%Y")
clu1_data1 <- clu1_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu1_data1_s <- clu1_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu1_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu1_data1_s['sum_row'] <- rowSums(clu1_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu1_data1_s['min_row'] <- apply(clu1_data1_s[2:97], 1, FUN=min)
clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu1_data1_s['max_row'] <- apply(clu1_data1_s[2:97], 1, FUN=max)
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
clu1_descr_load <- mean(colSums(clu1_descr_df))
#find the morning load at time (6am to 11am)
clu1_morload <- mean(rowSums(clu1_data1_s[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu1_prepeak <- mean(rowSums(clu1_data1_s[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu1_peak <- mean(rowSums(clu1_data1_s[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu1_postpeak <- mean(rowSums(clu1_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu1_average <- mean(clu1_data1_s$sum_row)

#Percentage consumption for each cluster at specific time
#at morning time from 7 to 10 as there is a morning peak
clu1_perc1 <- (sum(clu1_data1_s[30:42])/sum(clu1_data1_s$sum_row))*100
#from 5pm to 11pm
clu1_perc2 <- (sum(clu1_data1_s[70:94])/sum(clu1_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu1_data_dmm <- clu1_data
clu1_data_dmm$idmeter <- NULL 
clu1_data_dmm$date<-as.POSIXct((clu1_data_dmm$date), format="%d/%m/%Y")
clu1_data_dmm <- clu1_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu1_data_dmm['sum_row'] <- rowSums(clu1_data_dmm[2:97])/2577
clu1_dmin <- min(clu1_data_dmm$sum_row)
clu1_dmax <- max(clu1_data_dmm$sum_row)
clu1_mean <- mean(clu1_data_dmm$sum_row)
clu1_sd <- sd(clu1_data_dmm$sum_row)
############################################################################################
#Cluster #2 Characteristics
clu2_data$date<-as.POSIXct((clu2_data$date), format="%d/%m/%Y")
clu2_data1 <- clu2_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu2_data1_s <- clu2_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu2_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu2_data1_s['sum_row'] <- rowSums(clu2_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
clu2_data1_s['min_row'] <- apply(clu2_data1_s[2:97], 1, FUN=min)
clu2_baseload <- mean(sort(clu2_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu2_data1_s['max_row'] <- apply(clu2_data1_s[2:97], 1, FUN=max)
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
clu2_descr_load <- mean(colSums(clu2_descr_df))
#find the morning load at time (6am to 11am)
clu2_morload <- mean(rowSums(clu2_data1_s[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu2_prepeak <- mean(rowSums(clu2_data1_s[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu2_peak <- mean(rowSums(clu2_data1_s[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu2_postpeak <- mean(rowSums(clu2_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu2_average <- mean(clu2_data1_s$sum_row)
#Percentage consumption for each cluster at specific time
#at morning time from 5 to 9 as there is a morning peak
#clu2_perc1 <- (sum(clu2_data1_s[70:86])/sum(clu2_data1_s$sum_row))*100
#Percentage consumption for each cluster at specific time
#at morning time from 7 to 10 as there is a morning peak
clu2_perc1 <- (sum(clu2_data1_s[30:42])/sum(clu2_data1_s$sum_row))*100
#from 5pm to 11pm
clu2_perc2 <- (sum(clu2_data1_s[70:94])/sum(clu2_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu2_data_dmm <- clu2_data
clu2_data_dmm$idmeter <- NULL
clu2_data_dmm$date<-as.POSIXct((clu2_data_dmm$date), format="%d/%m/%Y")
clu2_data_dmm <- clu2_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu2_data_dmm['sum_row'] <- rowSums(clu2_data_dmm[2:97])/375
clu2_dmin <- min(clu2_data_dmm$sum_row)
clu2_dmax <- max(clu2_data_dmm$sum_row)
clu2_mean <- mean(clu2_data_dmm$sum_row)
clu2_sd <- sd(clu2_data_dmm$sum_row)
#############################################################################################
#Cluster #3 Characteristics
clu3_data$date<-as.POSIXct((clu3_data$date), format="%d/%m/%Y")
clu3_data1 <- clu3_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu3_data1_s <- clu3_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu3_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu3_data1_s['sum_row'] <- rowSums(clu3_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
clu3_data1_s['min_row'] <- apply(clu3_data1_s[2:97], 1, FUN=min)
clu3_baseload <- mean(sort(clu3_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu3_data1_s['max_row'] <- apply(clu3_data1_s[2:97], 1, FUN=max)
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
clu3_descr_load <- mean(colSums(clu3_descr_df))
#find the morning load at time (6am to 11am)
clu3_morload <- mean(rowSums(clu3_data1_s[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu3_prepeak <- mean(rowSums(clu3_data1_s[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu3_peak <- mean(rowSums(clu3_data1_s[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu3_postpeak <- mean(rowSums(clu3_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu3_average <- mean(clu3_data1_s$sum_row)
#Percentage consumption for each cluster at specific time
#at morning time from 7 to 10 as there is a morning peak
clu3_perc1 <- (sum(clu3_data1_s[30:42])/sum(clu3_data1_s$sum_row))*100
#from 7pm to 11pm
clu3_perc2 <- (sum(clu3_data1_s[70:94])/sum(clu3_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu3_data_dmm <- clu3_data
clu3_data_dmm$idmeter <- NULL 
clu3_data_dmm$date<-as.POSIXct((clu3_data_dmm$date), format="%d/%m/%Y")
clu3_data_dmm <- clu3_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu3_data_dmm['sum_row'] <- rowSums(clu3_data_dmm[2:97])/841
clu3_dmin <- min(clu3_data_dmm$sum_row)
clu3_dmax <- max(clu3_data_dmm$sum_row)
clu3_mean <- mean(clu3_data_dmm$sum_row)
clu3_sd <- sd(clu3_data_dmm$sum_row)
#############################################################################################
#Cluster #4 Characteristics
clu4_data$date<-as.POSIXct((clu4_data$date), format="%d/%m/%Y")
clu4_data1 <- clu4_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu4_data1_s <- clu4_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu4_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu4_data1_s['sum_row'] <- rowSums(clu4_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu4_data1_s['min_row'] <- apply(clu4_data1_s[2:97], 1, FUN=min)
clu4_baseload <- mean(sort(clu4_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu4_data1_s['max_row'] <- apply(clu4_data1_s[2:97], 1, FUN=max)
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
clu4_descr_load <- mean(colSums(clu4_descr_df))
#find the morning load at time (6am to 11am)
clu4_morload <- mean(rowSums(clu4_data1_s[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu4_prepeak <- mean(rowSums(clu4_data1_s[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu4_peak <- mean(rowSums(clu4_data1_s[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu4_postpeak <- mean(rowSums(clu4_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu4_average <- mean(clu4_data1_s$sum_row)
#Percentage consumption for each cluster at specific time
#at morning time from 7 to 10 as there is a morning peak
clu4_perc1 <- (sum(clu4_data1_s[30:42])/sum(clu4_data1_s$sum_row))*100
#from 7pm to 11pm
clu4_perc2 <- (sum(clu4_data1_s[70:94])/sum(clu4_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
#clu4_perc1 <- (sum(clu4_data1_s[70:94])/sum(clu4_data1_s$sum_row))*100
clu4_data_dmm <- clu4_data
clu4_data_dmm$idmeter <- NULL 
clu4_data_dmm$date<-as.POSIXct((clu4_data_dmm$date), format="%d/%m/%Y")
clu4_data_dmm <- clu4_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu4_data_dmm['sum_row'] <- rowSums(clu4_data_dmm[2:97])/1611
clu4_dmin <- min(clu4_data_dmm$sum_row)
clu4_dmax <- max(clu4_data_dmm$sum_row)
clu4_mean <- mean(clu4_data_dmm$sum_row)
clu4_sd <- sd(clu4_data_dmm$sum_row)
#############################################################################################
#Cluster #5 Characteristics
clu5_data$date<-as.POSIXct((clu5_data$date), format="%d/%m/%Y")
clu5_data1 <- clu5_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu5_data1_s <- clu5_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu5_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu5_data1_s['sum_row'] <- rowSums(clu5_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu5_data1_s['min_row'] <- apply(clu5_data1_s[2:97], 1, FUN=min)
clu5_baseload <- mean(sort(clu5_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu5_data1_s['max_row'] <- apply(clu5_data1_s[2:97], 1, FUN=max)
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
clu5_descr_load <- mean(colSums(clu5_descr_df))
#find the morning load at time (6am to 11am)
clu5_morload <- mean(rowSums(clu5_data1_s[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu5_prepeak <- mean(rowSums(clu5_data1_s[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu5_peak <- mean(rowSums(clu5_data1_s[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu5_postpeak <- mean(rowSums(clu5_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu5_average <- mean(clu5_data1_s$sum_row)
#Percentage consumption for each cluster at specific time
#at morning time from 7 to 10 as there is a morning peak
clu5_perc1 <- (sum(clu5_data1_s[30:42])/sum(clu5_data1_s$sum_row))*100
#from 7pm to 11pm
clu5_perc2 <- (sum(clu5_data1_s[70:94])/sum(clu5_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu5_data_dmm <- clu5_data
clu5_data_dmm$idmeter <- NULL 
clu5_data_dmm$date<-as.POSIXct((clu5_data_dmm$date), format="%d/%m/%Y")
clu5_data_dmm <- clu5_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu5_data_dmm['sum_row'] <- rowSums(clu5_data_dmm[2:97])/341
clu5_dmin <- min(clu5_data_dmm$sum_row)
clu5_dmax <- max(clu5_data_dmm$sum_row)
clu5_mean <- mean(clu5_data_dmm$sum_row)
clu5_sd <- sd(clu5_data_dmm$sum_row)
#############################################################################################
#Cluster #6 Characteristics
clu6_data$date<-as.POSIXct((clu6_data$date), format="%d/%m/%Y")
clu6_data1 <- clu6_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu6_data1_s <- clu6_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu6_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu6_data1_s['sum_row'] <- rowSums(clu6_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu6_data1_s['min_row'] <- apply(clu6_data1_s[2:97], 1, FUN=min)
clu6_baseload <- mean(sort(clu6_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu6_data1_s['max_row'] <- apply(clu6_data1_s[2:97], 1, FUN=max)
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
clu6_descr_load <- mean(colSums(clu6_descr_df))
#find the morning load at time (6am to 11am)
clu6_morload <- mean(rowSums(clu6_data1_s[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu6_prepeak <- mean(rowSums(clu6_data1_s[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu6_peak <- mean(rowSums(clu6_data1_s[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu6_postpeak <- mean(rowSums(clu6_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu6_average <- mean(clu6_data1_s$sum_row)
#at morning time from 7 to 10 as there is a morning peak
clu6_perc1 <- (sum(clu6_data1_s[30:42])/sum(clu6_data1_s$sum_row))*100
#from 7pm to 11pm
clu6_perc2 <- (sum(clu6_data1_s[70:94])/sum(clu6_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu6_data_dmm <- clu6_data
clu6_data_dmm$idmeter <- NULL
clu6_data_dmm$date<-as.POSIXct((clu6_data_dmm$date), format="%d/%m/%Y")
clu6_data_dmm <- clu6_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu6_data_dmm['sum_row'] <- rowSums(clu6_data_dmm[2:97])/413
clu6_dmin <- min(clu6_data_dmm$sum_row)
clu6_dmax <- max(clu6_data_dmm$sum_row)
clu6_mean <- mean(clu6_data_dmm$sum_row)
clu6_sd <- sd(clu6_data_dmm$sum_row)
#############################################################################################
#Cluster #7 Characteristics
clu7_data$date<-as.POSIXct((clu7_data$date), format="%d/%m/%Y")
clu7_data1 <- clu7_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu7_data1_s <- clu7_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu7_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu7_data1_s['sum_row'] <- rowSums(clu7_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu7_data1_s['min_row'] <- apply(clu7_data1_s[2:97], 1, FUN=min)
clu7_baseload <- mean(sort(clu7_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu7_data1_s['max_row'] <- apply(clu7_data1_s[2:97], 1, FUN=max)
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
clu7_descr_load <- mean(colSums(clu7_descr_df))
#find the morning load at time (8am to 10am)
clu7_morload <- mean(rowSums(clu7_data1_s[26:45]))/3
#find the average total load at pre-peak time (12pm to 3pm)
clu7_prepeak <- mean(rowSums(clu7_data1_s[58:69]))/3
#find the average total load at peak time (4pm to 7pm)
clu7_peak <- mean(rowSums(clu7_data1_s[70:93]))/6
#find the average total load at post-peak time (9pm to 12am)
clu7_postpeak <- mean(rowSums(clu7_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu7_average <- mean(clu7_data1_s$sum_row)
#at morning time from 7 to 10 as there is a morning peak
clu7_perc1 <- (sum(clu7_data1_s[30:42])/sum(clu7_data1_s$sum_row))*100
#from 7pm to 11pm
clu7_perc2 <- (sum(clu7_data1_s[70:94])/sum(clu7_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu7_data_dmm <- clu7_data
clu7_data_dmm$idmeter <- NULL
clu7_data_dmm$date<-as.POSIXct((clu7_data_dmm$date), format="%d/%m/%Y")
clu7_data_dmm <- clu7_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu7_data_dmm['sum_row'] <- rowSums(clu7_data_dmm[2:97])/1501
clu7_dmin <- min(clu7_data_dmm$sum_row)
clu7_dmax <- max(clu7_data_dmm$sum_row)
clu7_mean <- mean(clu7_data_dmm$sum_row)
clu7_sd <- sd(clu7_data_dmm$sum_row)
#############################################################################################
#Cluster #8 Characteristics
clu8_data$date<-as.POSIXct((clu8_data$date), format="%d/%m/%Y")
clu8_data1 <- clu8_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu8_data1_s <- clu8_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu8_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu8_data1_s['sum_row'] <- rowSums(clu8_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu8_data1_s['min_row'] <- apply(clu8_data1_s[2:97], 1, FUN=min)
clu8_baseload <- mean(sort(clu8_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu8_data1_s['max_row'] <- apply(clu8_data1_s[2:97], 1, FUN=max)
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
clu8_descr_load <- mean(colSums(clu8_descr_df))
#find the morning load at time (8am to 10am)
clu8_morload <- mean(rowSums(clu8_data1_s[26:45]))/3
#find the average total load at pre-peak time (12pm to 3pm)
clu8_prepeak <- mean(rowSums(clu8_data1_s[58:69]))/3
#find the average total load at peak time (4pm to 7pm)
clu8_peak <- mean(rowSums(clu8_data1_s[70:93]))/6
#find the average total load at post-peak time (9pm to 12am)
clu8_postpeak <- mean(rowSums(clu8_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu8_average <- mean(clu8_data1_s$sum_row)
#at morning time from 7 to 10 as there is a morning peak
clu8_perc1 <- (sum(clu8_data1_s[30:42])/sum(clu8_data1_s$sum_row))*100
#from 7pm to 11pm
clu8_perc2 <- (sum(clu8_data1_s[70:94])/sum(clu8_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu8_data_dmm <- clu8_data
clu8_data_dmm$idmeter <- NULL
clu8_data_dmm$date<-as.POSIXct((clu8_data_dmm$date), format="%d/%m/%Y")
clu8_data_dmm <- clu8_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu8_data_dmm['sum_row'] <- rowSums(clu8_data_dmm[2:97])/839
clu8_dmin <- min(clu8_data_dmm$sum_row)
clu8_dmax <- max(clu8_data_dmm$sum_row)
clu8_mean <- mean(clu8_data_dmm$sum_row)
clu8_sd <- sd(clu8_data_dmm$sum_row)
#############################################################################################
#Cluster #9 Characteristics
clu9_data$date<-as.POSIXct((clu9_data$date), format="%d/%m/%Y")
clu9_data1 <- clu9_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu9_data1_s <- clu9_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu9_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu9_data1_s['sum_row'] <- rowSums(clu9_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu9_data1_s['min_row'] <- apply(clu9_data1_s[2:97], 1, FUN=min)
clu9_baseload <- mean(sort(clu9_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu9_data1_s['max_row'] <- apply(clu9_data1_s[2:97], 1, FUN=max)
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
clu9_descr_load <- mean(colSums(clu9_descr_df))
#find the morning load at time (8am to 10am)
clu9_morload <- mean(rowSums(clu9_data1_s[26:45]))/3
#find the average total load at pre-peak time (12pm to 3pm)
clu9_prepeak <- mean(rowSums(clu9_data1_s[58:69]))/3
#find the average total load at peak time (4pm to 7pm)
clu9_peak <- mean(rowSums(clu9_data1_s[70:93]))/6
#find the average total load at post-peak time (9pm to 12am)
clu9_postpeak <- mean(rowSums(clu9_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu9_average <- mean(clu9_data1_s$sum_row)
#at morning time from 7 to 10 as there is a morning peak
clu9_perc1 <- (sum(clu9_data1_s[30:42])/sum(clu9_data1_s$sum_row))*100
#from 7pm to 11pm
clu9_perc2 <- (sum(clu9_data1_s[70:94])/sum(clu9_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu9_data_dmm <- clu9_data
clu9_data_dmm$idmeter <- NULL
clu9_data_dmm$date<-as.POSIXct((clu9_data_dmm$date), format="%d/%m/%Y")
clu9_data_dmm <- clu9_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu9_data_dmm['sum_row'] <- rowSums(clu9_data_dmm[2:97])/538
clu9_dmin <- min(clu9_data_dmm$sum_row)
clu9_dmax <- max(clu9_data_dmm$sum_row)
clu9_mean <- mean(clu9_data_dmm$sum_row)
clu9_sd <- sd(clu9_data_dmm$sum_row)
#############################################################################################
#Cluster #10 Characteristics
clu10_data$date<-as.POSIXct((clu10_data$date), format="%d/%m/%Y")
clu10_data1 <- clu10_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu10_data1_s <- clu10_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu10_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu10_data1_s['sum_row'] <- rowSums(clu10_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu10_data1_s['min_row'] <- apply(clu10_data1_s[2:97], 1, FUN=min)
clu10_baseload <- mean(sort(clu10_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu10_data1_s['max_row'] <- apply(clu10_data1_s[2:97], 1, FUN=max)
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
clu10_descr_load <- mean(colSums(clu10_descr_df))
#find the morning load at time (8am to 10am)
clu10_morload <- mean(rowSums(clu10_data1_s[26:45]))/3
#find the average total load at pre-peak time (12pm to 3pm)
clu10_prepeak <- mean(rowSums(clu10_data1_s[58:69]))/3
#find the average total load at peak time (4pm to 7pm)
clu10_peak <- mean(rowSums(clu10_data1_s[70:93]))/6
#find the average total load at post-peak time (9pm to 12am)
clu10_postpeak <- mean(rowSums(clu10_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu10_average <- mean(clu10_data1_s$sum_row)
#at morning time from 7 to 10 as there is a morning peak
clu10_perc1 <- (sum(clu10_data1_s[30:42])/sum(clu10_data1_s$sum_row))*100
#from 7pm to 11pm
clu10_perc2 <- (sum(clu10_data1_s[70:94])/sum(clu10_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu10_data_dmm <- clu10_data
clu10_data_dmm$idmeter <- NULL
clu10_data_dmm$date<-as.POSIXct((clu10_data_dmm$date), format="%d/%m/%Y")
clu10_data_dmm <- clu10_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu10_data_dmm['sum_row'] <- rowSums(clu10_data_dmm[2:97])/1118
clu10_dmin <- min(clu10_data_dmm$sum_row)
clu10_dmax <- max(clu10_data_dmm$sum_row)
clu10_mean <- mean(clu10_data_dmm$sum_row)
clu10_sd <- sd(clu10_data_dmm$sum_row)
#############################################################################################
#Cluster #11 Characteristics
clu11_data$date<-as.POSIXct((clu11_data$date), format="%d/%m/%Y")
clu11_data1 <- clu11_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu11_data1_s <- clu11_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu11_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu11_data1_s['sum_row'] <- rowSums(clu11_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu11_data1_s['min_row'] <- apply(clu11_data1_s[2:97], 1, FUN=min)
clu11_baseload <- mean(sort(clu11_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu11_data1_s['max_row'] <- apply(clu11_data1_s[2:97], 1, FUN=max)
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
clu11_descr_load <- mean(colSums(clu11_descr_df))
#find the morning load at time (8am to 10am)
clu11_morload <- mean(rowSums(clu11_data1_s[26:45]))/3
#find the average total load at pre-peak time (12pm to 3pm)
clu11_prepeak <- mean(rowSums(clu11_data1_s[58:69]))/3
#find the average total load at peak time (4pm to 7pm)
clu11_peak <- mean(rowSums(clu11_data1_s[70:93]))/6
#find the average total load at post-peak time (9pm to 12am)
clu11_postpeak <- mean(rowSums(clu11_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu11_average <- mean(clu11_data1_s$sum_row)
#at morning time from 7 to 10 as there is a morning peak
clu11_perc1 <- (sum(clu11_data1_s[30:42])/sum(clu11_data1_s$sum_row))*100
#from 7pm to 11pm
clu11_perc2 <- (sum(clu11_data1_s[70:94])/sum(clu11_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu11_data_dmm <- clu11_data
clu11_data_dmm$idmeter <- NULL
clu11_data_dmm$date<-as.POSIXct((clu11_data_dmm$date), format="%d/%m/%Y")
clu11_data_dmm <- clu11_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu11_data_dmm['sum_row'] <- rowSums(clu11_data_dmm[2:97])/1405
clu11_dmin <- min(clu11_data_dmm$sum_row)
clu11_dmax <- max(clu11_data_dmm$sum_row)
clu11_mean <- mean(clu11_data_dmm$sum_row)
clu11_sd <- sd(clu11_data_dmm$sum_row)
#############################################################################################
#Cluster #12 Characteristics
clu12_data$date<-as.POSIXct((clu12_data$date), format="%d/%m/%Y")
clu12_data1 <- clu12_data %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu12_data1_s <- clu12_data1 %>% group_by(week) %>% summarise_each(funs(sum))
clu12_data1_s$idmeter <- NULL
#sum of each rows and add new column
clu12_data1_s['sum_row'] <- rowSums(clu12_data1_s[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu12_data1_s['min_row'] <- apply(clu12_data1_s[2:97], 1, FUN=min)
clu12_baseload <- mean(sort(clu12_data1_s$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu12_data1_s['max_row'] <- apply(clu12_data1_s[2:97], 1, FUN=max)
#Maximum load during day
clu12_max_load <- max(clu12_data1_s$max_row)
#Descretionary load
list.clu12<-list()
for (i in 1:nrow(clu12_data1_s))
{
  list.clu12[[i]] <- clu12_data1_s[,i] - clu12_data1_s['min_row']
}
clu12_descr_df <- as.data.frame(list.clu12)
clu12_descr_df$week <- NULL
clu12_descr_load <- mean(colSums(clu12_descr_df))
#find the morning load at time (8am to 10am)
clu12_morload <- mean(rowSums(clu12_data1_s[26:45]))/3
#find the average total load at pre-peak time (12pm to 3pm)
clu12_prepeak <- mean(rowSums(clu12_data1_s[58:69]))/3
#find the average total load at peak time (4pm to 7pm)
clu12_peak <- mean(rowSums(clu12_data1_s[70:93]))/6
#find the average total load at post-peak time (9pm to 12am)
clu12_postpeak <- mean(rowSums(clu12_data1_s[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu12_average <- mean(clu12_data1_s$sum_row)
#at morning time from 7 to 10 as there is a morning peak
clu12_perc1 <- (sum(clu12_data1_s[30:42])/sum(clu12_data1_s$sum_row))*100
#from 7pm to 11pm
clu12_perc2 <- (sum(clu12_data1_s[70:94])/sum(clu12_data1_s$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu12_data_dmm <- clu12_data
clu12_data_dmm$idmeter <- NULL
clu12_data_dmm$date<-as.POSIXct((clu12_data_dmm$date), format="%d/%m/%Y")
clu12_data_dmm <- clu12_data_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu12_data_dmm['sum_row'] <- rowSums(clu12_data_dmm[2:97])/280
clu12_dmin <- min(clu12_data_dmm$sum_row)
clu12_dmax <- max(clu12_data_dmm$sum_row)
clu12_mean <- mean(clu12_data_dmm$sum_row)
clu12_sd <- sd(clu12_data_dmm$sum_row)
#############################################################################################

#Week Ends
#############################################################################################
clu1_data_we <- w_ends[w_ends$idmeter %in% clu1$idmeter,]
clu2_data_we <- w_ends[w_ends$idmeter %in% clu2$idmeter,]
clu3_data_we <- w_ends[w_ends$idmeter %in% clu3$idmeter,]
clu4_data_we <- w_ends[w_ends$idmeter %in% clu4$idmeter,]
clu5_data_we <- w_ends[w_ends$idmeter %in% clu5$idmeter,]
clu6_data_we <- w_ends[w_ends$idmeter %in% clu6$idmeter,]
clu7_data_we <- w_ends[w_ends$idmeter %in% clu7$idmeter,]
clu8_data_we <- w_ends[w_ends$idmeter %in% clu8$idmeter,]
clu9_data_we <- w_ends[w_ends$idmeter %in% clu9$idmeter,]
clu10_data_we <- w_ends[w_ends$idmeter %in% clu10$idmeter,]
clu11_data_we <- w_ends[w_ends$idmeter %in% clu11$idmeter,]
clu12_data_we <- w_ends[w_ends$idmeter %in% clu12$idmeter,]
#Cluster #1 Characteristics
clu1_data_we$date<-as.POSIXct((clu1_data_we$date), format="%d/%m/%Y")
clu1_data1_we <- clu1_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu1_data1_s_we <- clu1_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu1_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu1_data1_s_we['sum_row'] <- rowSums(clu1_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu1_data1_s_we['min_row'] <- apply(clu1_data1_s_we[2:97], 1, FUN=min)
clu1_baseload_we <- mean(sort(clu1_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu1_data1_s_we['max_row'] <- apply(clu1_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu1_max_load_we <- max(clu1_data1_s_we$max_row)
#Descretionary load
list.clu1_we<-list()
for (i in 1:nrow(clu1_data1_s_we))
{
  list.clu1_we[[i]] <- clu1_data1_s_we[,i] - clu1_data1_s_we['min_row']
}
clu1_descr_df_we <- as.data.frame(list.clu1_we)
clu1_descr_df_we$week <- NULL
clu1_descr_load_we <- mean(colSums(clu1_descr_df_we))
#find the morning load at time (6am to 11am)
clu1_morload_we <- mean(rowSums(clu1_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu1_prepeak_we <- mean(rowSums(clu1_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu1_peak_we <- mean(rowSums(clu1_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu1_postpeak_we <- mean(rowSums(clu1_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu1_average_we <- mean(clu1_data1_s_we$sum_row)
#Percentage consumption for each cluster at specific time
#at morning time from 6 to 9 as there is a morning peak
clu1_perc1_we <- (sum(clu1_data1_s_we[26:38])/sum(clu1_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu1_perc2_we <- (sum(clu1_data1_s_we[66:86])/sum(clu1_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu1_data_we_dmm <- clu1_data_we
clu1_data_we_dmm$idmeter <- NULL 
clu1_data_we_dmm$date<-as.POSIXct((clu1_data_we_dmm$date), format="%d/%m/%Y")
clu1_data_we_dmm <- clu1_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu1_data_we_dmm['sum_row'] <- rowSums(clu1_data_we_dmm[2:97])/2577
clu1_dmin_we <- min(clu1_data_we_dmm$sum_row)
clu1_dmax_we <- max(clu1_data_we_dmm$sum_row)
clu1_mean_we <- mean(clu1_data_we_dmm$sum_row)
clu1_sd_we <- sd(clu1_data_we_dmm$sum_row)
############################################################################################
#Cluster #2 Characteristics
clu2_data_we$date<-as.POSIXct((clu2_data_we$date), format="%d/%m/%Y")
clu2_data1_we <- clu2_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu2_data1_s_we <- clu2_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu2_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu2_data1_s_we['sum_row'] <- rowSums(clu2_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu2_data1_s_we['min_row'] <- apply(clu2_data1_s_we[2:97], 1, FUN=min)
clu2_baseload_we <- mean(sort(clu2_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu2_data1_s_we['max_row'] <- apply(clu2_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu2_max_load_we <- max(clu2_data1_s_we$max_row)
#Descretionary load
list.clu2_we<-list()
for (i in 1:nrow(clu2_data1_s_we))
{
  list.clu2_we[[i]] <- clu2_data1_s_we[,i] - clu2_data1_s_we['min_row']
}
clu2_descr_df_we <- as.data.frame(list.clu2_we)
clu2_descr_df_we$week <- NULL
clu2_descr_load_we <- mean(colSums(clu2_descr_df_we))
#find the morning load at time (6am to 11am)
clu2_morload_we <- mean(rowSums(clu2_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu2_prepeak_we <- mean(rowSums(clu2_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu2_peak_we <- mean(rowSums(clu2_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu2_postpeak_we <- mean(rowSums(clu2_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu2_average_we <- mean(clu2_data1_s_we$sum_row)
#Percentage consumption for each cluster at specific time
#at morning time from 6 to 9 as there is a morning peak
clu2_perc1_we <- (sum(clu2_data1_s_we[26:38])/sum(clu2_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu2_perc2_we <- (sum(clu2_data1_s_we[66:86])/sum(clu2_data1_s_we$sum_row))*100
#clu2_perc1_we <- (sum(clu2_data1_s_we[70:86])/sum(clu2_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu2_data_we_dmm <- clu2_data_we
clu2_data_we_dmm$idmeter <- NULL
clu2_data_we_dmm$date<-as.POSIXct((clu2_data_we_dmm$date), format="%d/%m/%Y")
clu2_data_we_dmm <- clu2_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu2_data_we_dmm['sum_row'] <- rowSums(clu2_data_we_dmm[2:97])/375
clu2_dmin_we <- min(clu2_data_we_dmm$sum_row)
clu2_dmax_we <- max(clu2_data_we_dmm$sum_row)
clu2_mean_we <- mean(clu2_data_we_dmm$sum_row)
clu2_sd_we <- sd(clu2_data_we_dmm$sum_row)
#############################################################################################
#Cluster #3 Characteristics
clu3_data_we$date<-as.POSIXct((clu3_data_we$date), format="%d/%m/%Y")
clu3_data1_we <- clu3_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu3_data1_s_we <- clu3_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu3_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu3_data1_s_we['sum_row'] <- rowSums(clu3_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu3_data1_s_we['min_row'] <- apply(clu3_data1_s_we[2:97], 1, FUN=min)
clu3_baseload_we <- mean(sort(clu3_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu3_data1_s_we['max_row'] <- apply(clu3_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu3_max_load_we <- max(clu3_data1_s_we$max_row)
#Descretionary load
list.clu3_we<-list()
for (i in 1:nrow(clu3_data1_s_we))
{
  list.clu3_we[[i]] <- clu3_data1_s_we[,i] - clu3_data1_s_we['min_row']
}
clu3_descr_df_we <- as.data.frame(list.clu3_we)
clu3_descr_df_we$week <- NULL
clu3_descr_load_we <- mean(colSums(clu3_descr_df_we))
#find the morning load at time (6am to 11am)
clu3_morload_we <- mean(rowSums(clu3_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu3_prepeak_we <- mean(rowSums(clu3_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu3_peak_we <- mean(rowSums(clu3_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu3_postpeak_we <- mean(rowSums(clu3_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu3_average_we <- mean(clu3_data1_s_we$sum_row)
#Percentage consumption for each cluster at specific time
#at morning time from 6 to 9 as there is a morning peak
clu3_perc1_we <- (sum(clu3_data1_s_we[26:38])/sum(clu3_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu3_perc2_we <- (sum(clu3_data1_s_we[66:86])/sum(clu3_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu3_data_we_dmm <- clu3_data_we
clu3_data_we_dmm$idmeter <- NULL 
clu3_data_we_dmm$date<-as.POSIXct((clu3_data_we_dmm$date), format="%d/%m/%Y")
clu3_data_we_dmm <- clu3_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu3_data_we_dmm['sum_row'] <- rowSums(clu3_data_we_dmm[2:97])/841
clu3_dmin_we <- min(clu3_data_we_dmm$sum_row)
clu3_dmax_we <- max(clu3_data_we_dmm$sum_row)
clu3_mean_we <- mean(clu3_data_we_dmm$sum_row)
clu3_sd_we <- sd(clu3_data_we_dmm$sum_row)
#############################################################################################
#Cluster #4 Characteristics
clu4_data_we$date<-as.POSIXct((clu4_data_we$date), format="%d/%m/%Y")
clu4_data1_we <- clu4_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu4_data1_s_we <- clu4_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu4_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu4_data1_s_we['sum_row'] <- rowSums(clu4_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu4_data1_s_we['min_row'] <- apply(clu4_data1_s_we[2:97], 1, FUN=min)
clu4_baseload_we <- mean(sort(clu4_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu4_data1_s_we['max_row'] <- apply(clu4_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu4_max_load_we <- max(clu4_data1_s_we$max_row)
#Descretionary load
list.clu4_we<-list()
for (i in 1:nrow(clu4_data1_s_we))
{
  list.clu4_we[[i]] <- clu4_data1_s_we[,i] - clu4_data1_s_we['min_row']
}
clu4_descr_df_we <- as.data.frame(list.clu4_we)
clu4_descr_df_we$week <- NULL
clu4_descr_load_we <- mean(colSums(clu4_descr_df_we))
#find the morning load at time (6am to 11am)
clu4_morload_we <- mean(rowSums(clu4_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu4_prepeak_we <- mean(rowSums(clu4_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu4_peak_we <- mean(rowSums(clu4_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu4_postpeak_we <- mean(rowSums(clu4_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu4_average_we <- mean(clu4_data1_s_we$sum_row)
#at morning time from 6 to 9 as there is a morning peak
clu4_perc1_we <- (sum(clu4_data1_s_we[26:38])/sum(clu4_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu4_perc2_we <- (sum(clu4_data1_s_we[66:86])/sum(clu4_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu4_data_we_dmm <- clu4_data_we
clu4_data_we_dmm$idmeter <- NULL 
clu4_data_we_dmm$date<-as.POSIXct((clu4_data_we_dmm$date), format="%d/%m/%Y")
clu4_data_we_dmm <- clu4_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu4_data_we_dmm['sum_row'] <- rowSums(clu4_data_we_dmm[2:97])/1611
clu4_dmin_we <- min(clu4_data_we_dmm$sum_row)
clu4_dmax_we <- max(clu4_data_we_dmm$sum_row)
clu4_mean_we <- mean(clu4_data_we_dmm$sum_row)
clu4_sd_we <- sd(clu4_data_we_dmm$sum_row)
#############################################################################################
#Cluster #5 Characteristics
clu5_data_we$date<-as.POSIXct((clu5_data_we$date), format="%d/%m/%Y")
clu5_data1_we <- clu5_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu5_data1_s_we <- clu5_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu5_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu5_data1_s_we['sum_row'] <- rowSums(clu5_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu5_data1_s_we['min_row'] <- apply(clu5_data1_s_we[2:97], 1, FUN=min)
clu5_baseload_we <- mean(sort(clu5_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu5_data1_s_we['max_row'] <- apply(clu5_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu5_max_load_we <- max(clu5_data1_s_we$max_row)
#Descretionary load
list.clu5_we<-list()
for (i in 1:nrow(clu5_data1_s_we))
{
  list.clu5_we[[i]] <- clu5_data1_s_we[,i] - clu5_data1_s_we['min_row']
}
clu5_descr_df_we <- as.data.frame(list.clu5_we)
clu5_descr_df_we$week <- NULL
clu5_descr_load_we <- mean(colSums(clu5_descr_df_we))
#find the morning load at time (6am to 11am)
clu5_morload_we <- mean(rowSums(clu5_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu5_prepeak_we <- mean(rowSums(clu5_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu5_peak_we <- mean(rowSums(clu5_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu5_postpeak_we <- mean(rowSums(clu5_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu5_average_we <- mean(clu5_data1_s_we$sum_row)
#at morning time from 6 to 9 as there is a morning peak
clu5_perc1_we <- (sum(clu5_data1_s_we[26:38])/sum(clu5_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu5_perc2_we <- (sum(clu5_data1_s_we[66:86])/sum(clu5_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu5_data_we_dmm <- clu5_data_we
clu5_data_we_dmm$idmeter <- NULL 
clu5_data_we_dmm$date<-as.POSIXct((clu5_data_we_dmm$date), format="%d/%m/%Y")
clu5_data_we_dmm <- clu5_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu5_data_we_dmm['sum_row'] <- rowSums(clu5_data_we_dmm[2:97])/341
clu5_dmin_we <- min(clu5_data_we_dmm$sum_row)
clu5_dmax_we <- max(clu5_data_we_dmm$sum_row)
clu5_mean_we <- mean(clu5_data_we_dmm$sum_row)
clu5_sd_we <- sd(clu5_data_we_dmm$sum_row)
#############################################################################################
#Cluster #6 Characteristics
clu6_data_we$date<-as.POSIXct((clu6_data_we$date), format="%d/%m/%Y")
clu6_data1_we <- clu6_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu6_data1_s_we <- clu6_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu6_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu6_data1_s_we['sum_row'] <- rowSums(clu6_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu6_data1_s_we['min_row'] <- apply(clu6_data1_s_we[2:97], 1, FUN=min)
clu6_baseload_we <- mean(sort(clu6_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu6_data1_s_we['max_row'] <- apply(clu6_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu6_max_load_we <- max(clu6_data1_s_we$max_row)
#Descretionary load
list.clu6_we<-list()
for (i in 1:nrow(clu6_data1_s_we))
{
  list.clu6_we[[i]] <- clu6_data1_s_we[,i] - clu6_data1_s_we['min_row']
}
clu6_descr_df_we <- as.data.frame(list.clu6_we)
clu6_descr_df_we$week <- NULL
clu6_descr_load_we <- mean(colSums(clu6_descr_df_we))
#find the morning load at time (6am to 11am)
clu6_morload_we <- mean(rowSums(clu6_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu6_prepeak_we <- mean(rowSums(clu6_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu6_peak_we <- mean(rowSums(clu6_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu6_postpeak_we <- mean(rowSums(clu6_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu6_average_we <- mean(clu6_data1_s_we$sum_row)
#Minumum and maximum daily consumption, std. dev. of the group
#at morning time from 6 to 9 as there is a morning peak
clu6_perc1_we <- (sum(clu6_data1_s_we[26:38])/sum(clu6_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu6_perc2_we <- (sum(clu6_data1_s_we[66:86])/sum(clu6_data1_s_we$sum_row))*100
clu6_data_we_dmm <- clu6_data_we
clu6_data_we_dmm$idmeter <- NULL
clu6_data_we_dmm$date<-as.POSIXct((clu6_data_we_dmm$date), format="%d/%m/%Y")
clu6_data_we_dmm <- clu6_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu6_data_we_dmm['sum_row'] <- rowSums(clu6_data_we_dmm[2:97])/413
clu6_dmin_we <- min(clu6_data_we_dmm$sum_row)
clu6_dmax_we <- max(clu6_data_we_dmm$sum_row)
clu6_mean_we <- mean(clu6_data_we_dmm$sum_row)
clu6_sd_we <- sd(clu6_data_we_dmm$sum_row)
#############################################################################################
#Cluster #7 Characteristics
clu7_data_we$date<-as.POSIXct((clu7_data_we$date), format="%d/%m/%Y")
clu7_data1_we <- clu7_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu7_data1_s_we <- clu7_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu7_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu7_data1_s_we['sum_row'] <- rowSums(clu7_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu7_data1_s_we['min_row'] <- apply(clu7_data1_s_we[2:97], 1, FUN=min)
clu7_baseload_we <- mean(sort(clu7_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu7_data1_s_we['max_row'] <- apply(clu7_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu7_max_load_we <- max(clu7_data1_s_we$max_row)
#Descretionary load
list.clu7_we<-list()
for (i in 1:nrow(clu7_data1_s_we))
{
  list.clu7_we[[i]] <- clu7_data1_s_we[,i] - clu7_data1_s_we['min_row']
}
clu7_descr_df_we <- as.data.frame(list.clu7_we)
clu7_descr_df_we$week <- NULL
clu7_descr_load_we <- mean(colSums(clu7_descr_df_we))
#find the morning load at time (6am to 11am)
clu7_morload_we <- mean(rowSums(clu7_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu7_prepeak_we <- mean(rowSums(clu7_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu7_peak_we <- mean(rowSums(clu7_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu7_postpeak_we <- mean(rowSums(clu7_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu7_average_we <- sum(clu7_data1_s_we$sum_row)/19
#at morning time from 6 to 9 as there is a morning peak
clu7_perc1_we <- (sum(clu7_data1_s_we[26:38])/sum(clu7_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu7_perc2_we <- (sum(clu7_data1_s_we[66:86])/sum(clu7_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu7_data_we_dmm <- clu7_data_we
clu7_data_we_dmm$idmeter <- NULL
clu7_data_we_dmm$date<-as.POSIXct((clu7_data_we_dmm$date), format="%d/%m/%Y")
clu7_data_we_dmm <- clu7_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu7_data_we_dmm['sum_row'] <- rowSums(clu7_data_we_dmm[2:97])/1501
clu7_dmin_we <- min(clu7_data_we_dmm$sum_row)
clu7_dmax_we <- max(clu7_data_we_dmm$sum_row)
clu7_mean_we <- mean(clu7_data_we_dmm$sum_row)
clu7_sd_we <- sd(clu7_data_we_dmm$sum_row)
###########################################################################################
#Cluster #8 Characteristics
clu8_data_we$date<-as.POSIXct((clu8_data_we$date), format="%d/%m/%Y")
clu8_data1_we <- clu8_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu8_data1_s_we <- clu8_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu8_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu8_data1_s_we['sum_row'] <- rowSums(clu8_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu8_data1_s_we['min_row'] <- apply(clu8_data1_s_we[2:97], 1, FUN=min)
clu8_baseload_we <- mean(sort(clu8_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu8_data1_s_we['max_row'] <- apply(clu8_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu8_max_load_we <- max(clu8_data1_s_we$max_row)
#Descretionary load
list.clu8_we<-list()
for (i in 1:nrow(clu8_data1_s_we))
{
  list.clu8_we[[i]] <- clu8_data1_s_we[,i] - clu8_data1_s_we['min_row']
}
clu8_descr_df_we <- as.data.frame(list.clu8_we)
clu8_descr_df_we$week <- NULL
clu8_descr_load_we <- mean(colSums(clu8_descr_df_we))
#find the morning load at time (6am to 11am)
clu8_morload_we <- mean(rowSums(clu8_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu8_prepeak_we <- mean(rowSums(clu8_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu8_peak_we <- mean(rowSums(clu8_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu8_postpeak_we <- mean(rowSums(clu8_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu8_average_we <- sum(clu8_data1_s_we$sum_row)/19
#at morning time from 6 to 9 as there is a morning peak
clu8_perc1_we <- (sum(clu8_data1_s_we[26:38])/sum(clu8_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu8_perc2_we <- (sum(clu8_data1_s_we[66:86])/sum(clu8_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu8_data_we_dmm <- clu8_data_we
clu8_data_we_dmm$idmeter <- NULL
clu8_data_we_dmm$date<-as.POSIXct((clu8_data_we_dmm$date), format="%d/%m/%Y")
clu8_data_we_dmm <- clu8_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu8_data_we_dmm['sum_row'] <- rowSums(clu8_data_we_dmm[2:97])/839
clu8_dmin_we <- min(clu8_data_we_dmm$sum_row)
clu8_dmax_we <- max(clu8_data_we_dmm$sum_row)
clu8_mean_we <- mean(clu8_data_we_dmm$sum_row)
clu8_sd_we <- sd(clu8_data_we_dmm$sum_row)
###########################################################################################
#Cluster #9 Characteristics
clu9_data_we$date<-as.POSIXct((clu9_data_we$date), format="%d/%m/%Y")
clu9_data1_we <- clu9_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu9_data1_s_we <- clu9_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu9_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu9_data1_s_we['sum_row'] <- rowSums(clu9_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu9_data1_s_we['min_row'] <- apply(clu9_data1_s_we[2:97], 1, FUN=min)
clu9_baseload_we <- mean(sort(clu9_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu9_data1_s_we['max_row'] <- apply(clu9_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu9_max_load_we <- max(clu9_data1_s_we$max_row)
#Descretionary load
list.clu9_we<-list()
for (i in 1:nrow(clu9_data1_s_we))
{
  list.clu9_we[[i]] <- clu9_data1_s_we[,i] - clu9_data1_s_we['min_row']
}
clu9_descr_df_we <- as.data.frame(list.clu9_we)
clu9_descr_df_we$week <- NULL
clu9_descr_load_we <- mean(colSums(clu9_descr_df_we))
#find the morning load at time (6am to 11am)
clu9_morload_we <- mean(rowSums(clu9_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu9_prepeak_we <- mean(rowSums(clu9_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu9_peak_we <- mean(rowSums(clu9_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu9_postpeak_we <- mean(rowSums(clu9_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu9_average_we <- sum(clu9_data1_s_we$sum_row)/19
#at morning time from 6 to 9 as there is a morning peak
clu9_perc1_we <- (sum(clu9_data1_s_we[26:38])/sum(clu9_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu9_perc2_we <- (sum(clu9_data1_s_we[66:86])/sum(clu9_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu9_data_we_dmm <- clu9_data_we
clu9_data_we_dmm$idmeter <- NULL
clu9_data_we_dmm$date<-as.POSIXct((clu9_data_we_dmm$date), format="%d/%m/%Y")
clu9_data_we_dmm <- clu9_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu9_data_we_dmm['sum_row'] <- rowSums(clu9_data_we_dmm[2:97])/538
clu9_dmin_we <- min(clu9_data_we_dmm$sum_row)
clu9_dmax_we <- max(clu9_data_we_dmm$sum_row)
clu9_mean_we <- mean(clu9_data_we_dmm$sum_row)
clu9_sd_we <- sd(clu9_data_we_dmm$sum_row)
###########################################################################################
#Cluster #10 Characteristics
clu10_data_we$date<-as.POSIXct((clu10_data_we$date), format="%d/%m/%Y")
clu10_data1_we <- clu10_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu10_data1_s_we <- clu10_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu10_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu10_data1_s_we['sum_row'] <- rowSums(clu10_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu10_data1_s_we['min_row'] <- apply(clu10_data1_s_we[2:97], 1, FUN=min)
clu10_baseload_we <- mean(sort(clu10_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu10_data1_s_we['max_row'] <- apply(clu10_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu10_max_load_we <- max(clu10_data1_s_we$max_row)
#Descretionary load
list.clu10_we<-list()
for (i in 1:nrow(clu10_data1_s_we))
{
  list.clu10_we[[i]] <- clu10_data1_s_we[,i] - clu10_data1_s_we['min_row']
}
clu10_descr_df_we <- as.data.frame(list.clu10_we)
clu10_descr_df_we$week <- NULL
clu10_descr_load_we <- mean(colSums(clu10_descr_df_we))
#find the morning load at time (6am to 11am)
clu10_morload_we <- mean(rowSums(clu10_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu10_prepeak_we <- mean(rowSums(clu10_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu10_peak_we <- mean(rowSums(clu10_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu10_postpeak_we <- mean(rowSums(clu10_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu10_average_we <- sum(clu10_data1_s_we$sum_row)/19
#at morning time from 6 to 9 as there is a morning peak
clu10_perc1_we <- (sum(clu10_data1_s_we[26:38])/sum(clu10_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu10_perc2_we <- (sum(clu10_data1_s_we[66:86])/sum(clu10_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu10_data_we_dmm <- clu10_data_we
clu10_data_we_dmm$idmeter <- NULL
clu10_data_we_dmm$date<-as.POSIXct((clu10_data_we_dmm$date), format="%d/%m/%Y")
clu10_data_we_dmm <- clu10_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu10_data_we_dmm['sum_row'] <- rowSums(clu10_data_we_dmm[2:97])/1118
clu10_dmin_we <- min(clu10_data_we_dmm$sum_row)
clu10_dmax_we <- max(clu10_data_we_dmm$sum_row)
clu10_mean_we <- mean(clu10_data_we_dmm$sum_row)
clu10_sd_we <- sd(clu10_data_we_dmm$sum_row)
###########################################################################################
#Cluster #11 Characteristics
clu11_data_we$date<-as.POSIXct((clu11_data_we$date), format="%d/%m/%Y")
clu11_data1_we <- clu11_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu11_data1_s_we <- clu11_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu11_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu11_data1_s_we['sum_row'] <- rowSums(clu11_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu11_data1_s_we['min_row'] <- apply(clu11_data1_s_we[2:97], 1, FUN=min)
clu11_baseload_we <- mean(sort(clu11_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu11_data1_s_we['max_row'] <- apply(clu11_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu11_max_load_we <- max(clu11_data1_s_we$max_row)
#Descretionary load
list.clu11_we<-list()
for (i in 1:nrow(clu11_data1_s_we))
{
  list.clu11_we[[i]] <- clu11_data1_s_we[,i] - clu11_data1_s_we['min_row']
}
clu11_descr_df_we <- as.data.frame(list.clu11_we)
clu11_descr_df_we$week <- NULL
clu11_descr_load_we <- mean(colSums(clu11_descr_df_we))
#find the morning load at time (6am to 11am)
clu11_morload_we <- mean(rowSums(clu11_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu11_prepeak_we <- mean(rowSums(clu11_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu11_peak_we <- mean(rowSums(clu11_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu11_postpeak_we <- mean(rowSums(clu11_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu11_average_we <- sum(clu11_data1_s_we$sum_row)/19
#at morning time from 6 to 9 as there is a morning peak
clu11_perc1_we <- (sum(clu11_data1_s_we[26:38])/sum(clu11_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu11_perc2_we <- (sum(clu11_data1_s_we[66:86])/sum(clu11_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu11_data_we_dmm <- clu11_data_we
clu11_data_we_dmm$idmeter <- NULL
clu11_data_we_dmm$date<-as.POSIXct((clu11_data_we_dmm$date), format="%d/%m/%Y")
clu11_data_we_dmm <- clu11_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu11_data_we_dmm['sum_row'] <- rowSums(clu11_data_we_dmm[2:97])/1405
clu11_dmin_we <- min(clu11_data_we_dmm$sum_row)
clu11_dmax_we <- max(clu11_data_we_dmm$sum_row)
clu11_mean_we <- mean(clu11_data_we_dmm$sum_row)
clu11_sd_we <- sd(clu11_data_we_dmm$sum_row)
###########################################################################################
#Cluster #12 Characteristics
clu12_data_we$date<-as.POSIXct((clu12_data_we$date), format="%d/%m/%Y")
clu12_data1_we <- clu12_data_we %>% group_by(idmeter, week = week(date)) %>% summarise_each(funs(sum), -date)
clu12_data1_s_we <- clu12_data1_we %>% group_by(week) %>% summarise_each(funs(sum))
clu12_data1_s_we$idmeter <- NULL
#sum of each rows and add new column
clu12_data1_s_we['sum_row'] <- rowSums(clu12_data1_s_we[2:97])
#find the 3 lowest values and avarage to find the base load
#clu1_baseload <- mean(sort(clu1_data1_s$min_row)[1:3])
clu12_data1_s_we['min_row'] <- apply(clu12_data1_s_we[2:97], 1, FUN=min)
clu12_baseload_we <- mean(sort(clu12_data1_s_we$min_row)[1:5])
#clu1_baseload <- apply(clu1_data1_s[2:25], 1, FUN=min)
clu12_data1_s_we['max_row'] <- apply(clu12_data1_s_we[2:97], 1, FUN=max)
#Maximum load during day
clu12_max_load_we <- max(clu6_data1_s_we$max_row)
#Descretionary load
list.clu12_we<-list()
for (i in 1:nrow(clu12_data1_s_we))
{
  list.clu12_we[[i]] <- clu12_data1_s_we[,i] - clu12_data1_s_we['min_row']
}
clu12_descr_df_we <- as.data.frame(list.clu12_we)
clu12_descr_df_we$week <- NULL
clu12_descr_load_we <- mean(colSums(clu12_descr_df_we))
#find the morning load at time (6am to 11am)
clu12_morload_we <- mean(rowSums(clu12_data1_s_we[26:45]))/5
#find the peak load at pre-peak time (2pm to 5pm)
clu12_prepeak_we <- mean(rowSums(clu12_data1_s_we[58:69]))/3
#find the peak load at peak time (5pm to 11pm)
clu12_peak_we <- mean(rowSums(clu12_data1_s_we[70:93]))/6
#find the peak load at post-peak time (12am,1am,11pm)
clu12_postpeak_we <- mean(rowSums(clu12_data1_s_we[,c(2,3,4,5,6,7,8,9,93,94,95,96,97)]))/3
#Total average consumption
clu12_average_we <- sum(clu12_data1_s_we$sum_row)/19
#at morning time from 6 to 9 as there is a morning peak
clu12_perc1_we <- (sum(clu12_data1_s_we[26:38])/sum(clu12_data1_s_we$sum_row))*100
#from 4pm to 9pm
clu12_perc2_we <- (sum(clu12_data1_s_we[66:86])/sum(clu12_data1_s_we$sum_row))*100
#Minumum and maximum daily consumption, std. dev. of the group
clu12_data_we_dmm <- clu12_data_we
clu12_data_we_dmm$idmeter <- NULL
clu12_data_we_dmm$date<-as.POSIXct((clu12_data_we_dmm$date), format="%d/%m/%Y")
clu12_data_we_dmm <- clu12_data_we_dmm %>% group_by(date) %>% summarise_each(funs(sum))
clu12_data_we_dmm['sum_row'] <- rowSums(clu12_data_we_dmm[2:97])/280
clu12_dmin_we <- min(clu12_data_we_dmm$sum_row)
clu12_dmax_we <- max(clu12_data_we_dmm$sum_row)
clu12_mean_we <- mean(clu12_data_we_dmm$sum_row)
clu12_sd_we <- sd(clu12_data_we_dmm$sum_row)
###########################################################################################