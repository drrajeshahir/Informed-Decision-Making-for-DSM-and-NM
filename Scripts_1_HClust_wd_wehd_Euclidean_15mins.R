######  Import database
source("multiplot.R")
#original one
rubi_net <- read.csv("D:/mins15_final_data-11839.csv", sep=",", stringsAsFactors=FALSE)
rubi_net$idmeter = as.numeric(gsub(".*?([0-9]+).*", "\\1", rubi_net$idmeter))
rubi_net$X <- NULL
rubi_net$idmeter<-as.numeric(rubi_net$idmeter)
rubi_net$dates<-as.POSIXct((rubi_net$dates), format="%Y-%m-%d")
#rubi1<-rubi[order(as.numeric(rubi$idmeter),rubi$dates),]
rubi_net<-rubi_net[order(as.numeric(rubi_net$idmeter),rubi_net$dates),]
str(rubi_net)
a<-as.data.frame(unique(rubi_net$idmeter))

###### Clean database As of now not neccessary

###1. Get the sum of each row 
#rubi_cut<-rubi_net[3:98]
#rubi_zero<-rubi[apply(rubi_cut==0, 1, sum)<=0,]

### 
#x<-rubi_zero[3:98]
#y<-x[-1]
#diff <- y-x[1:length(x)-1]
#rubi_net<-rubi_zero[apply(diff==0,1,sum)<=96,]

names(rubi_net)<- c("idmeter","date","00:00","00:15","00:30","00:45","01:00","01:15","01:30","01:45","02:00","02:15","02:30","02:45",
                    "03:00","03:15","03:30","03:45","04:00","04:15","04:30","04:45","05:00","05:15","05:30","05:45","06:00","06:15","06:30",
                    "06:45","07:00","07:15","07:30","07:45","08:00","08:15","08:30","08:45","09:00","09:15","09:30","09:45","10:00","10:15",
                    "10:30","10:45","11:00","11:15","11:30","11:45","12:00","12:15","12:30","12:45","13:00","13:15","13:30","13:45","14:00",
                    "14:15","14:30","14:45","15:00","15:15","15:30","15:45","16:00","16:15","16:30","16:45","17:00","17:15","17:30","17:45",
                    "18:00","18:15","18:30","18:45","19:00","19:15","19:30","19:45","20:00","20:15","20:30","20:45","21:00","21:15","21:30",
                    "21:45","22:00","22:15","22:30","22:45","23:00","23:15","23:30","23:45")


######  PERCENTAGES (OPOWER) OPTION 2: CURVES ARCHETYPES
## RowSUms, Division, ColMeans --> MORE ACCURATE!
###1. Get the sum of each row 
rubi_sum<-rubi_net[3:98]
row_sum<-as.matrix(rowSums(rubi_sum))
names(row_sum)<-"sum"

###2. Division to get the percentages per hour  
division<-as.data.frame(rubi_sum/row_sum)
#division$idmeter<-NULL
division_id<-cbind(rubi_net$idmeter,division)
division_id <- na.omit(division_id)
names(division_id)<-c("idmeter","00:00","00:15","00:30","00:45","01:00","01:15","01:30","01:45","02:00","02:15","02:30","02:45",
                      "03:00","03:15","03:30","03:45","04:00","04:15","04:30","04:45","05:00","05:15","05:30","05:45","06:00","06:15","06:30",
                      "06:45","07:00","07:15","07:30","07:45","08:00","08:15","08:30","08:45","09:00","09:15","09:30","09:45","10:00","10:15",
                      "10:30","10:45","11:00","11:15","11:30","11:45","12:00","12:15","12:30","12:45","13:00","13:15","13:30","13:45","14:00",
                      "14:15","14:30","14:45","15:00","15:15","15:30","15:45","16:00","16:15","16:30","16:45","17:00","17:15","17:30","17:45",
                      "18:00","18:15","18:30","18:45","19:00","19:15","19:30","19:45","20:00","20:15","20:30","20:45","21:00","21:15","21:30",
                      "21:45","22:00","22:15","22:30","22:45","23:00","23:15","23:30","23:45")
  
division_id<-as.data.frame(division_id)
#test<-as.matrix(rowSums(division_id[2:25]))

###3. Column means
cast_99<-as.data.frame(lapply(split(division_id, division_id$idmeter),colMeans))
cast100<-as.data.frame(t(cast_99))
cast100$idmeter<-NULL
hour_percent<-cast100
#rubi_net[rubi_net == 0] <- ''
####Separate Weekday and Weekends
library(xts)
    rubi2<-as.xts(rubi_net,rubi_net$date)

## Weekdays
weekdays<-rubi2[.indexwday(rubi2) %in% 1:5] #labels=c("Monday","Tuesday","Wednesday", "Thursday", "Friday")
holiday <- c("2019-12-25","2020-01-23","2020-01-29","2020-02-21","2020-03-10","2020-04-02","2020-04-10","2020-04-14"
             ,"2018-05-25","2020-08-01","2020-10-02","2020-10-22","2020-10-23","2020-10-29","2020-11-30","2020-12-25")
weekdays<-weekdays[!weekdays$date %in% holiday, ]
w_days<-as.data.frame(dates=index(weekdays), coredata(weekdays))
w_days$dates<-NULL
names(w_days)<- c("idmeter","date",
                  c("wd00:00","wd00:15","wd00:30","wd00:45","wd01:00","wd01:15","wd01:30","wd01:45","wd02:00","wd02:15","wd02:30","wd02:45",
                    "wd03:00","wd03:15","wd03:30","wd03:45","wd04:00","wd04:15","wd04:30","wd04:45","wd05:00","wd05:15","wd05:30","wd05:45",
                    "wd06:00","wd06:15","wd06:30","wd06:45","wd07:00","wd07:15","wd07:30","wd07:45","wd08:00","wd08:15","wd08:30","wd08:45",
                    "wd09:00","wd09:15","wd09:30","wd09:45","wd10:00","wd10:15","wd10:30","wd10:45","wd11:00","wd11:15","wd11:30","wd11:45",
                    "wd12:00","wd12:15","wd12:30","wd12:45","wd13:00","wd13:15","wd13:30","wd13:45","wd14:00","wd14:15","wd14:30","wd14:45",
                    "wd15:00","wd15:15","wd15:30","wd15:45","wd16:00","wd16:15","wd16:30","wd16:45","wd17:00","wd17:15","wd17:30","wd17:45",
                    "wd18:00","wd18:15","wd18:30","wd18:45","wd19:00","wd19:15","wd19:30","wd19:45","wd20:00","wd20:15","wd20:30","wd20:45",
                    "wd21:00","wd21:15","wd21:30","wd21:45","wd22:00","wd22:15","wd22:30","wd22:45","wd23:00","wd23:15","wd23:30","wd23:45"))
w_days[,c(3:98)] <- lapply(w_days[,c(3:98)], as.character)
w_days[,c(3:98)] <- lapply(w_days[,c(3:98)], as.numeric)
w_days$idmeter<-as.numeric(as.character(w_days$idmeter))
w_days$date<-as.POSIXct(w_days$date)
w_days<-w_days[order(w_days$idmeter,w_days$date),]
str(w_days)

## Weekends
weekends<-rubi2[!.indexwday(rubi2) %in% 1:5]
holidays <- rubi2[holiday]
weekends<-rbind(weekends,holidays)
w_ends<-data.frame(dates=index(weekends), coredata(weekends))
w_ends$dates<-NULL
names(w_ends)<- c("idmeter","date",
                  c("we00:00","we00:15","we00:30","we00:45","we01:00","we01:15","we01:30","we01:45","we02:00","we02:15","we02:30","we02:45",
                    "we03:00","we03:15","we03:30","we03:45","we04:00","we04:15","we04:30","we04:45","we05:00","we05:15","we05:30","we05:45",
                    "we06:00","we06:15","we06:30","we06:45","we07:00","we07:15","we07:30","we07:45","we08:00","we08:15","we08:30","we08:45",
                    "we09:00","we09:15","we09:30","we09:45","we10:00","we10:15","we10:30","we10:45","we11:00","we11:15","we11:30","we11:45",
                    "we12:00","we12:15","we12:30","we12:45","we13:00","we13:15","we13:30","we13:45","we14:00","we14:15","we14:30","we14:45",
                    "we15:00","we15:15","we15:30","we15:45","we16:00","we16:15","we16:30","we16:45","we17:00","we17:15","we17:30","we17:45",
                    "we18:00","we18:15","we18:30","we18:45","we19:00","we19:15","we19:30","we19:45","we20:00","we20:15","we20:30","we20:45",
                    "we21:00","we21:15","we21:30","we21:45","we22:00","we22:15","we22:30","we22:45","we23:00","we23:15","we23:30","we23:45"))
w_ends[,c(3:98)] <- lapply(w_ends[,c(3:98)], as.character)
w_ends[,c(3:98)] <- lapply(w_ends[,c(3:98)], as.numeric)
w_ends$idmeter<-as.numeric(as.character(w_ends$idmeter))
w_ends$date<-as.POSIXct(w_ends$date)
w_ends<-w_ends[order(w_ends$idmeter,w_ends$date),]
str(w_ends)

####  PERCENTAGES (OPOWER) OPTION 2: CURVES ARCHETYPES
## RowSUms, Division, ColMeans --> MORE ACCURATE!

### WEEKDAYSSS!!
#1. Get the sum of each row 
w_days_net<-w_days[3:98]
row_sum<-as.matrix(rowSums(w_days_net))
names(row_sum)<-"sum"


#2. Division to get the percentages per hour
division<-as.data.frame(w_days_net/row_sum)
division_id<-cbind(w_days$idmeter,division)
names(division_id)<-c("idmeter",
                      c("wd00:00","wd00:15","wd00:30","wd00:45","wd01:00","wd01:15","wd01:30","wd01:45","wd02:00","wd02:15","wd02:30","wd02:45",
                        "wd03:00","wd03:15","wd03:30","wd03:45","wd04:00","wd04:15","wd04:30","wd04:45","wd05:00","wd05:15","wd05:30","wd05:45",
                        "wd06:00","wd06:15","wd06:30","wd06:45","wd07:00","wd07:15","wd07:30","wd07:45","wd08:00","wd08:15","wd08:30","wd08:45",
                        "wd09:00","wd09:15","wd09:30","wd09:45","wd10:00","wd10:15","wd10:30","wd10:45","wd11:00","wd11:15","wd11:30","wd11:45",
                        "wd12:00","wd12:15","wd12:30","wd12:45","wd13:00","wd13:15","wd13:30","wd13:45","wd14:00","wd14:15","wd14:30","wd14:45",
                        "wd15:00","wd15:15","wd15:30","wd15:45","wd16:00","wd16:15","wd16:30","wd16:45","wd17:00","wd17:15","wd17:30","wd17:45",
                        "wd18:00","wd18:15","wd18:30","wd18:45","wd19:00","wd19:15","wd19:30","wd19:45","wd20:00","wd20:15","wd20:30","wd20:45",
                        "wd21:00","wd21:15","wd21:30","wd21:45","wd22:00","wd22:15","wd22:30","wd22:45","wd23:00","wd23:15","wd23:30","wd23:45"))
division_id<-as.data.frame(division_id)
test<-as.matrix(rowSums(division_id[2:97]))

#3. Column means
cast_99_wd<-as.data.frame(lapply(split(division_id, division_id$idmeter),colMeans))
cast100_wd<-as.data.frame(t(cast_99_wd))
cast100_wd$idmeter<-NULL

perc_wd<-cast100_wd

##WEEKENDSS!!
#1. Get the sum of each row 
w_ends_net<-w_ends[3:98]
row_sum_we<-as.matrix(rowSums(w_ends_net))
names(row_sum_we)<-"sum"


#2. Division to get the percentages per hour
division_we<-as.data.frame(w_ends_net/row_sum_we)
division_id_we<-cbind(w_ends$idmeter,division_we)
names(division_id_we)<-c("idmeter",
                         c("we00:00","we00:15","we00:30","we00:45","we01:00","we01:15","we01:30","we01:45","we02:00","we02:15","we02:30","we02:45",
                           "we03:00","we03:15","we03:30","we03:45","we04:00","we04:15","we04:30","we04:45","we05:00","we05:15","we05:30","we05:45",
                           "we06:00","we06:15","we06:30","we06:45","we07:00","we07:15","we07:30","we07:45","we08:00","we08:15","we08:30","we08:45",
                           "we09:00","we09:15","we09:30","we09:45","we10:00","we10:15","we10:30","we10:45","we11:00","we11:15","we11:30","we11:45",
                           "we12:00","we12:15","we12:30","we12:45","we13:00","we13:15","we13:30","we13:45","we14:00","we14:15","we14:30","we14:45",
                           "we15:00","we15:15","we15:30","we15:45","we16:00","we16:15","we16:30","we16:45","we17:00","we17:15","we17:30","we17:45",
                           "we18:00","we18:15","we18:30","we18:45","we19:00","we19:15","we19:30","we19:45","we20:00","we20:15","we20:30","we20:45",
                           "we21:00","we21:15","we21:30","we21:45","we22:00","we22:15","we22:30","we22:45","we23:00","we23:15","we23:30","we23:45"))
division_id_we<-as.data.frame(division_id_we)
test_we<-as.matrix(rowSums(division_id_we[2:97]))

#3. Column means
cast_99_we<-as.data.frame(lapply(split(division_id_we, division_id_we$idmeter),colMeans))
cast100_we<-as.data.frame(t(cast_99_we))
cast100_we$idmeter<-NULL
perc_we<-cast100_we

perc_wd<-perc_wd[-c(764:773),]
#perc_we<-perc_we[-c(778:781),]
## Data casted weekdays and weekends
perc_wd_we<-cbind(perc_wd,perc_we)   ## matrix real power data casted
#perc_wd_we<-perc_wd_we*100
#### Hierarchial clustering matrix data

##Distances: distance is choosen in dist() can be euclidean, manhattan,canbera,DTW etc...
p<-dist(hour_percent,method="manhattan")
p<-dist(perc_wd,method="euclidean")
#library("dtw")
#p_dtw <- dist(hour_percent, method="DTW")
#p1_dtw<-as.matrix(p_dtw)
library(factoextra)
fviz_nbclust(hour_percent, FUN = hcut, method = "wss")+
            geom_vline(xintercept = 6, linetype = 2)+
            labs(subtitle = "Elbow method")
fviz_nbclust(hour_percent, FUN = hcut, method = "silhouette")
library(cluster)
gap_stat <- clusGap(hour_percent, FUN = hcut, nstart = 25, K.max = 14, B = 50)
fviz_gap_stat(gap_stat)

##Linkages: linkage is chosen in hclust() can be ward.D, ward.D2, single, complete, average, median,...
#p1<-hclust(p_dtw, method="ward.D")   #also equal to complete, single, ward.D, ward.D2
p[!is.finite(p)] <- runif(1, min=0.03, max=0.3)
p1<-hclust(p, method="single")

## Dendrogram
library(ggplot2)
library(ggdendro)  
  ggdendrogram(p1, rotate = F, size = 4, labels=F)


## Cluster by cutree()
clus <- as.data.frame(cutree((p1), k=12))  ## k indicates number of clusters
names(clus)<- "clus_num"
#clus
table(clus)    # to know how many are in each cluster
write.csv(clus, "Cluster_wise_meters.csv")

library(dendroextras)
d5<-colour_clusters(p1,6)
plot(d5)
d5g<-colour_clusters(p1,6,groupLabels=TRUE)
plot(d5g)

## circular plotting
library(ape)
plot(as.phylo(p1), type = "fan")

## Heatmap--> need to improve
q<-heatmap(as.matrix(hour_percent))



############PLOTTING
## if uses "rel_wdays_wends" print from 0 to 1; if uses "wdays_wends" print real power
perc_wd_we<-perc_wd_we[-c(327:358),] #if required
clus <- clus[-c(993:995),] #if required
p2<-as.data.frame(cbind(clus, perc_wd_we))
names(p2)<-c("clus_num",
             c("wd00:00","wd00:15","wd00:30","wd00:45","wd01:00","wd01:15","wd01:30","wd01:45","wd02:00","wd02:15","wd02:30","wd02:45",
               "wd03:00","wd03:15","wd03:30","wd03:45","wd04:00","wd04:15","wd04:30","wd04:45","wd05:00","wd05:15","wd05:30","wd05:45",
               "wd06:00","wd06:15","wd06:30","wd06:45","wd07:00","wd07:15","wd07:30","wd07:45","wd08:00","wd08:15","wd08:30","wd08:45",
               "wd09:00","wd09:15","wd09:30","wd09:45","wd10:00","wd10:15","wd10:30","wd10:45","wd11:00","wd11:15","wd11:30","wd11:45",
               "wd12:00","wd12:15","wd12:30","wd12:45","wd13:00","wd13:15","wd13:30","wd13:45","wd14:00","wd14:15","wd14:30","wd14:45",
               "wd15:00","wd15:15","wd15:30","wd15:45","wd16:00","wd16:15","wd16:30","wd16:45","wd17:00","wd17:15","wd17:30","wd17:45",
               "wd18:00","wd18:15","wd18:30","wd18:45","wd19:00","wd19:15","wd19:30","wd19:45","wd20:00","wd20:15","wd20:30","wd20:45",
               "wd21:00","wd21:15","wd21:30","wd21:45","wd22:00","wd22:15","wd22:30","wd22:45","wd23:00","wd23:15","wd23:30","wd23:45",
               "we00:00","we00:15","we00:30","we00:45","we01:00","we01:15","we01:30","we01:45","we02:00","we02:15","we02:30","we02:45",
               "we03:00","we03:15","we03:30","we03:45","we04:00","we04:15","we04:30","we04:45","we05:00","we05:15","we05:30","we05:45",
               "we06:00","we06:15","we06:30","we06:45","we07:00","we07:15","we07:30","we07:45","we08:00","we08:15","we08:30","we08:45",
               "we09:00","we09:15","we09:30","we09:45","we10:00","we10:15","we10:30","we10:45","we11:00","we11:15","we11:30","we11:45",
               "we12:00","we12:15","we12:30","we12:45","we13:00","we13:15","we13:30","we13:45","we14:00","we14:15","we14:30","we14:45",
               "we15:00","we15:15","we15:30","we15:45","we16:00","we16:15","we16:30","we16:45","we17:00","we17:15","we17:30","we17:45",
               "we18:00","we18:15","we18:30","we18:45","we19:00","we19:15","we19:30","we19:45","we20:00","we20:15","we20:30","we20:45",
               "we21:00","we21:15","we21:30","we21:45","we22:00","we22:15","we22:30","we22:45","we23:00","we23:15","we23:30","we23:45"))

cluster1<-p2[p2$clus_num==1,]
cluster2<-p2[p2$clus_num==2,]
cluster3<-p2[p2$clus_num==3,]
cluster4<-p2[p2$clus_num==4,]
cluster5<-p2[p2$clus_num==5,]
cluster6<-p2[p2$clus_num==6,]
cluster7<-p2[p2$clus_num==7,]
cluster8<-p2[p2$clus_num==8,]
cluster9<-p2[p2$clus_num==9,]
cluster10<-p2[p2$clus_num==10,]
cluster11<-p2[p2$clus_num==11,]
cluster12<-p2[p2$clus_num==12,]
#### TESTING clusters by plotting       
library(reshape2)
hour<-c(0:95)
#Set the directory path to save the plots
setwd("D:/PhD IITKGP/Paper #2_DSM_with_ML/Scripts/KOTA")
##1. WEEKDAYS PLOT clusters 

#cluster1 
c_1wd<-as.data.frame(t(cluster1[,c(2:97)]))
c_1wd<-cbind(hour,c_1wd)
c_11wd<-melt(c_1wd, id.vars="hour")
cp1wd<-ggplot(c_11wd, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #1)") +
  geom_line(aes(colour = variable))+geom_smooth()+
  theme(axis.text = element_text(size=18,family="A"),axis.title = element_text(size=18))+ylim(0,0.03)+scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 1_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster2
c_2wd<-as.data.frame(t(cluster2[,c(2:97)]))
c_2wd<-cbind(hour,c_2wd)
c_21wd<-melt(c_2wd, id.vars="hour")
cp2wd<-ggplot(c_21wd, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #2)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 2_WD.PNG", device = "png", plot = last_plot(), width = 70, height = 35, units = "cm")

#cluster3 
c_3wd<-as.data.frame(t(cluster3[,c(2:97)]))
c_3wd<-cbind(hour,c_3wd)
c_31wd<-melt(c_3wd, id.vars="hour")
cp3wd<-ggplot(c_31wd, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #3)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 3_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster4 
c_4wd<-as.data.frame(t(cluster4[,c(2:97)]))
c_4wd<-cbind(hour,c_4wd)
c_41wd<-melt(c_4wd, id.vars="hour")
cp4wd<-ggplot(c_41wd, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #4)") +
  geom_line(aes(colour = variable))+geom_smooth() +ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 4_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster5 
c_5wd<-as.data.frame(t(cluster5[,c(2:97)]))
c_5wd<-cbind(hour,c_5wd)
c_51wd<-melt(c_5wd, id.vars="hour")
cp5wd<-ggplot(c_51wd, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #5)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.1)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 5_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster6 
c_6wd<-as.data.frame(t(cluster6[,c(2:97)]))
c_6wd<-cbind(hour,c_6wd)
c_61wd<-melt(c_6wd, id.vars="hour")
cp6wd<-ggplot(c_61wd, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #6)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.1)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 6_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster7 
c_7wd<-as.data.frame(t(cluster7[,c(2:97)]))
c_7wd<-cbind(hour,c_7wd)
c_71wd<-melt(c_7wd, id.vars="hour")
cp7wd<-ggplot(c_71wd, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #7)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.05)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 7_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster8 
 c_8wd<-as.data.frame(t(cluster8[,c(2:97)]))
 c_8wd<-cbind(hour,c_8wd)
 c_81wd<-melt(c_8wd, id.vars="hour")
 cp8wd<-ggplot(c_81wd, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #8)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 8_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")
 
#cluster9
 c_9wd<-as.data.frame(t(cluster9[,c(2:97)]))
 c_9wd<-cbind(hour,c_9wd)
 c_91wd<-melt(c_9wd, id.vars="hour")
 cp9wd<-ggplot(c_91wd, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #9)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 9_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

 
#cluster10
 c_10wd<-as.data.frame(t(cluster10[,c(2:97)]))
 c_10wd<-cbind(hour,c_10wd)
 c_101wd<-melt(c_10wd, id.vars="hour")
 cp10wd<-ggplot(c_101wd, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #9)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 10_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

 
#cluster11
 c_11wd<-as.data.frame(t(cluster11[,c(2:97)]))
 c_11wd<-cbind(hour,c_11wd)
 c_111wd<-melt(c_11wd, id.vars="hour")
 cp11wd<-ggplot(c_111wd, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #9)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 11_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")
 
 
#cluster12
 c_12wd<-as.data.frame(t(cluster12[,c(2:97)]))
 c_12wd<-cbind(hour,c_12wd)
 c_121wd<-melt(c_12wd, id.vars="hour")
 cp12wd<-ggplot(c_121wd, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays plot(C #12)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 12_WD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")
 
#source("multiplot.R")
#multiplot(cp1wd, cp2wd, cp3wd, cp4wd,cp5wd, cols=2)


####2. WEEKENDS PLOT clusters 
#cluster1 
c_1we<-as.data.frame(t(cluster1[,c(98:193)]))
c_1we<-cbind(hour,c_1we)
c_11we<-melt(c_1we, id.vars="hour")
cp1we<-ggplot(c_11we, aes(hour,value)) + 
  labs(x="Time(h)", y="Mean hourly usage (kWh)", title = "Weekends plot(C #1)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 1_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster2 
c_2we<-as.data.frame(t(cluster2[,c(98:193)]))
c_2we<-cbind(hour,c_2we)
c_21we<-melt(c_2we, id.vars="hour")
cp2we<-ggplot(c_21we, aes(hour,value)) + 
  labs(x="Time(h)", y="Mean hourly usage (kWh)", title = "Weekends plot(C #2)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 2_WE.PNG", device = "png", plot = last_plot(), width = 65, height = 35, units = "cm")

#cluster3 
c_3we<-as.data.frame(t(cluster3[,c(98:193)]))
c_3we<-cbind(hour,c_3we)
c_31we<-melt(c_3we, id.vars="hour")
cp3we<-ggplot(c_31we, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #3)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 3_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster4
c_4we<-as.data.frame(t(cluster4[,c(98:193)]))
c_4we<-cbind(hour,c_4we)
c_41we<-melt(c_4we, id.vars="hour")
cp4we<-ggplot(c_41we, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #4)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 4_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster5 
c_5we<-as.data.frame(t(cluster5[,c(98:193)]))
c_5we<-cbind(hour,c_5we)
c_51we<-melt(c_5we, id.vars="hour")
cp5we<-ggplot(c_51we, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #5)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 5_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster6
c_6we<-as.data.frame(t(cluster6[,c(98:193)]))
c_6we<-cbind(hour,c_6we)
c_61we<-melt(c_6we, id.vars="hour")
cp6we<-ggplot(c_61we, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #6)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 6_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster7
c_7we<-as.data.frame(t(cluster7[,c(98:193)]))
c_7we<-cbind(hour,c_7we)
c_71we<-melt(c_7we, id.vars="hour")
cp7we<-ggplot(c_71we, aes(hour,value)) + 
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #7)") +
  geom_line(aes(colour = variable))+geom_smooth()+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
ggsave("Cluster 7_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

#cluster8
 c_8we<-as.data.frame(t(cluster8[,c(98:193)]))
 c_8we<-cbind(hour,c_8we)
 c_81we<-melt(c_8we, id.vars="hour")
 cp8we<-ggplot(c_81we, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #8)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 8_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

 #cluster9
 c_9we<-as.data.frame(t(cluster9[,c(98:193)]))
 c_9we<-cbind(hour,c_9we)
 c_91we<-melt(c_9we, id.vars="hour")
 cp9we<-ggplot(c_91we, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #9)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 9_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

 #cluster10
 c_10we<-as.data.frame(t(cluster10[,c(98:193)]))
 c_10we<-cbind(hour,c_10we)
 c_101we<-melt(c_10we, id.vars="hour")
 cp10we<-ggplot(c_101we, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #10)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 10_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")
 
 #cluster11
 c_11we<-as.data.frame(t(cluster11[,c(98:193)]))
 c_11we<-cbind(hour,c_11we)
 c_111we<-melt(c_11we, id.vars="hour")
 cp11we<-ggplot(c_111we, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #11)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 11_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")
 
 #cluster12
 c_12we<-as.data.frame(t(cluster12[,c(98:193)]))
 c_12we<-cbind(hour,c_12we)
 c_121we<-melt(c_12we, id.vars="hour")
 cp12we<-ggplot(c_121we, aes(hour,value)) + 
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekends plot(C #12)") +
   geom_line(aes(colour = variable))+geom_smooth()+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))
 ggsave("Cluster 12_WE.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")
 
#source("multiplot.R")
#multiplot(cp1we, cp2we, cp3we, cp4we, cp5we, cols=2)

#CLUSTERS MEAN WEEKDAYS
cluster1_mean<-as.data.frame(colMeans(na.omit(cluster1[2:97])))
names(cluster1_mean)<-"mean_clus1"
cluster1_mean<-cbind(hour,cluster1_mean)
cp1wdm<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekdays mean plot (C #1)") +
  geom_line(data=c_11wd, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "black", size = 1)
ggsave("Cluster 1_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster2_mean<-as.data.frame(colMeans(na.omit(cluster2[2:97])))
names(cluster2_mean)<-"mean_clus2"
cluster2_mean<-cbind(hour,cluster2_mean)
cp2wdm<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekdays mean plot (C #2)") +
  geom_line(data=c_21wd, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "black", size = 1)
ggsave("Cluster 2_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster3_mean<-as.data.frame(colMeans(na.omit(cluster3[2:97])))
names(cluster3_mean)<-"mean_clus3"
cluster3_mean<-cbind(hour,cluster3_mean)
cp3wdm<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekdays mean plot (C #3)") +
  geom_line(data=c_31wd, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "black", size = 1)
ggsave("Cluster 3_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster4_mean<-as.data.frame(colMeans(na.omit(cluster4[2:97])))
names(cluster4_mean)<-"mean_clus4"
cluster4_mean<-cbind(hour,cluster4_mean)
cp4wdm<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekdays mean plot (C #4)") +
  geom_line(data=c_41wd, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "black", size = 1)
ggsave("Cluster 4_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster5_mean<-as.data.frame(colMeans(na.omit(cluster5[2:97])))
names(cluster5_mean)<-"mean_clus5"
cluster5_mean<-cbind(hour,cluster5_mean)
cp5wdm<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekdays mean plot (C #5)") +
  geom_line(data=c_51wd, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)
ggsave("Cluster 5_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster6_mean<-as.data.frame(colMeans(na.omit(cluster6[2:97])))
names(cluster6_mean)<-"mean_clus6"
cluster6_mean<-cbind(hour,cluster6_mean)
cp6wdm<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekdays mean plot (C #6)") +
  geom_line(data=c_61wd, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster6_mean, aes(hour,mean_clus6),colour= "black", size = 1)
ggsave("Cluster 6_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster7_mean<-as.data.frame(colMeans(na.omit(cluster7[2:97])))
names(cluster7_mean)<-"mean_clus7"
cluster7_mean<-cbind(hour,cluster7_mean)
cp7wdm<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekdays mean plot (C #7)") +
  geom_line(data=c_71wd, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster7_mean, aes(hour,mean_clus7),colour= "black", size = 1)
ggsave("Cluster 7_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster8_mean<-as.data.frame(colMeans(na.omit(cluster8[2:97])))
names(cluster8_mean)<-"mean_clus8"
cluster8_mean<-cbind(hour,cluster8_mean)
cp8wdm<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekdays mean plot (C #8)") +
  geom_line(data=c_81wd, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster8_mean, aes(hour,mean_clus8),colour= "black", size = 1)
ggsave("Cluster 8_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster9_mean<-as.data.frame(colMeans(na.omit(cluster9[2:97])))
names(cluster9_mean)<-"mean_clus9"
cluster9_mean<-cbind(hour,cluster9_mean)
cp9wdm<-ggplot()+
   labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays mean plot (C #9)") +
   geom_line(data=c_91wd, aes(hour,value, colour=variable))+ylim(0,3)+
   scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
   geom_line(data=cluster9_mean, aes(hour,mean_clus9),colour= "black", size = 1)
ggsave("Cluster 9_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster10_mean<-as.data.frame(colMeans(na.omit(cluster10[2:97])))
names(cluster10_mean)<-"mean_clus10"
cluster10_mean<-cbind(hour,cluster10_mean)
cp10wdm<-ggplot()+
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays mean plot (C #10)") +
  geom_line(data=c_101wd, aes(hour,value, colour=variable))+ylim(0,3)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster10_mean, aes(hour,mean_clus10),colour= "black", size = 1)
ggsave("Cluster 10_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster11_mean<-as.data.frame(colMeans(na.omit(cluster11[2:97])))
names(cluster11_mean)<-"mean_clus11"
cluster11_mean<-cbind(hour,cluster11_mean)
cp11wdm<-ggplot()+
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays mean plot (C #11)") +
  geom_line(data=c_111wd, aes(hour,value, colour=variable))+ylim(0,3)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster11_mean, aes(hour,mean_clus11),colour= "black", size = 1)
ggsave("Cluster 11_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster12_mean<-as.data.frame(colMeans(na.omit(cluster12[2:97])))
names(cluster12_mean)<-"mean_clus12"
cluster12_mean<-cbind(hour,cluster12_mean)
cp12wdm<-ggplot()+
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekdays mean plot (C #12)") +
  geom_line(data=c_121wd, aes(hour,value, colour=variable))+ylim(0,3)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster12_mean, aes(hour,mean_clus12),colour= "black", size = 1)
ggsave("Cluster 12_WD_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

  all_wd<-ggplot()+ scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = "")) + ylim(0, 0.03) +
          labs(x="Time(h)", y="Mean hourly usage (kWh)", title = "Week Days") +
          geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)+
          geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)+
          geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)+
          geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)+
          geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)+
          geom_line(data=cluster6_mean, aes(hour,mean_clus6),colour= "maroon", size = 1)+
          geom_line(data=cluster7_mean, aes(hour,mean_clus7),colour= "grey", size = 1)+
          geom_line(data=cluster8_mean, aes(hour,mean_clus8),colour= "magenta", size = 1)+
          geom_line(data=cluster9_mean, aes(hour,mean_clus9),colour= "yellow", size = 1)+
          geom_line(data=cluster10_mean, aes(hour,mean_clus10),colour= "purple", size = 1)+
          geom_line(data=cluster11_mean, aes(hour,mean_clus11),colour= "brown", size = 1)+
          geom_line(data=cluster12_mean, aes(hour,mean_clus12),colour= "cyan", size = 1)

#by_clus_mean<-rbind(cluster1_mean, cluster2_mean, cluster3_mean)
##2. plot the 5 different cluster in 5 wrap facets
#library(ggplot2)
#ggplot(by_clus_mean,aes(hour,mean))+geom_line(aes(colour=clus_num))+facet_wrap(~clus_num)+ylim(0,0.10)
#CLUSTERS MEAN WEEKENDS
cluster1_mean_we<-as.data.frame(colMeans(na.omit(cluster1[98:193])))
names(cluster1_mean_we)<-"mean_clus1_we"
cluster1_mean_we<-cbind(hour,cluster1_mean_we)
cp1wem<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #1)") +
  geom_line(data=c_11we, aes(hour,value*20, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.6)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster1_mean_we, aes(hour,mean_clus1_we*20),colour= "black", size = 1)
ggsave("Cluster 1_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster2_mean_we<-as.data.frame(colMeans(na.omit(cluster2[98:193])))
names(cluster2_mean_we)<-"mean_clus2_we"
cluster2_mean_we<-cbind(hour,cluster2_mean_we)
cp2wem<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #2)") +
  geom_line(data=c_21we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster2_mean_we, aes(hour,mean_clus2_we),colour= "black", size = 1)
ggsave("Cluster 2_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster3_mean_we<-as.data.frame(colMeans(na.omit(cluster3[98:193])))
names(cluster3_mean_we)<-"mean_clus3_we"
cluster3_mean_we<-cbind(hour,cluster3_mean_we)
cp3wem<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #3)") +
  geom_line(data=c_31we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster3_mean_we, aes(hour,mean_clus3_we),colour= "black", size = 1)
ggsave("Cluster 3_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster4_mean_we<-as.data.frame(colMeans(na.omit(cluster4[98:193])))
names(cluster4_mean_we)<-"mean_clus4_we"
cluster4_mean_we<-cbind(hour,cluster4_mean_we)
cp4wem<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #4)") +
  geom_line(data=c_41we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster4_mean_we, aes(hour,mean_clus4_we),colour= "black", size = 1)
ggsave("Cluster 4_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster5_mean_we<-as.data.frame(colMeans(na.omit(cluster5[98:193])))
names(cluster5_mean_we)<-"mean_clus5_we"
cluster5_mean_we<-cbind(hour,cluster5_mean_we)
cp5wem<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #5)") +
  geom_line(data=c_51we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster5_mean_we, aes(hour,mean_clus5_we),colour= "black", size = 1)
ggsave("Cluster 5_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster6_mean_we<-as.data.frame(colMeans(na.omit(cluster6[98:193])))
names(cluster6_mean_we)<-"mean_clus6_we"
cluster6_mean_we<-cbind(hour,cluster6_mean_we)
cp6wem<-ggplot()+
  labs(x="Time(h)", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #6)") +
  geom_line(data=c_61we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster6_mean_we, aes(hour,mean_clus6_we),colour= "black", size = 1)
ggsave("Cluster 6_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster7_mean_we<-as.data.frame(colMeans(na.omit(cluster7[98:193])))
names(cluster7_mean_we)<-"mean_clus7_we"
cluster7_mean_we<-cbind(hour,cluster7_mean_we)
cp7wem<-ggplot()+
  labs(x="Time(h))", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #7)") +
  geom_line(data=c_71we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster7_mean_we, aes(hour,mean_clus7_we),colour= "black", size = 1)
ggsave("Cluster 7_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster8_mean_we<-as.data.frame(colMeans(na.omit(cluster8[98:193])))
names(cluster8_mean_we)<-"mean_clus8_we"
cluster8_mean_we<-cbind(hour,cluster8_mean_we)
cp8wem<-ggplot()+
  labs(x="Time", y="Mean hourly usage (%)", title = "Weekends mean plot (C #8)") +
  geom_line(data=c_81we, aes(hour,value, colour=variable))+ylim(0,3)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster8_mean_we, aes(hour,mean_clus8_we),colour= "black", size = 1)
ggsave("Cluster 8_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster9_mean_we<-as.data.frame(colMeans(na.omit(cluster9[98:193])))
names(cluster9_mean_we)<-"mean_clus9_we"
cluster9_mean_we<-cbind(hour,cluster9_mean_we)
cp9wem<-ggplot()+
  labs(x="Time(h))", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #9)") +
  geom_line(data=c_91we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster9_mean_we, aes(hour,mean_clus9_we),colour= "black", size = 1)
ggsave("Cluster 9_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster10_mean_we<-as.data.frame(colMeans(na.omit(cluster10[98:193])))
names(cluster10_mean_we)<-"mean_clus10_we"
cluster10_mean_we<-cbind(hour,cluster10_mean_we)
cp10wem<-ggplot()+
  labs(x="Time(h))", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #10)") +
  geom_line(data=c_101we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster10_mean_we, aes(hour,mean_clus10_we),colour= "black", size = 1)
ggsave("Cluster 10_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster11_mean_we<-as.data.frame(colMeans(na.omit(cluster11[98:193])))
names(cluster11_mean_we)<-"mean_clus11_we"
cluster11_mean_we<-cbind(hour,cluster11_mean_we)
cp11wem<-ggplot()+
  labs(x="Time(h))", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #11)") +
  geom_line(data=c_111we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster11_mean_we, aes(hour,mean_clus11_we),colour= "black", size = 1)
ggsave("Cluster 11_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

cluster12_mean_we<-as.data.frame(colMeans(na.omit(cluster12[98:193])))
names(cluster12_mean_we)<-"mean_clus12_we"
cluster12_mean_we<-cbind(hour,cluster12_mean_we)
cp12wem<-ggplot()+
  labs(x="Time(h))", y="Mean hourly usage (kW)", title = "Weekends mean plot (C #12)") +
  geom_line(data=c_121we, aes(hour,value, colour=variable))+
  theme(axis.text = element_text(size=18),axis.title = element_text(size=18))+ylim(0,0.03)+
  scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+
  geom_line(data=cluster12_mean_we, aes(hour,mean_clus12_we),colour= "black", size = 1)
ggsave("Cluster 12_WE_Mean.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")

all_we<-
  ggplot() + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = "")) + ylim(0,0.03) + 
  labs(x="Time", y="Mean hourly usage (kWh)", title = "Week Ends") +
  geom_line(data=cluster1_mean_we, aes(hour,mean_clus1_we),colour= "red", size = 1)+
  geom_line(data=cluster2_mean_we, aes(hour,mean_clus2_we),colour= "blue", size = 1)+
  geom_line(data=cluster3_mean_we, aes(hour,mean_clus3_we),colour= "orange", size = 1)+
  geom_line(data=cluster4_mean_we, aes(hour,mean_clus4_we),colour= "green", size = 1)+
  geom_line(data=cluster5_mean_we, aes(hour,mean_clus5_we),colour= "black", size = 1)+
  geom_line(data=cluster6_mean_we, aes(hour,mean_clus6_we),colour= "maroon", size = 1)+
  geom_line(data=cluster7_mean_we, aes(hour,mean_clus7_we),colour= "grey", size = 1)+
  geom_line(data=cluster8_mean_we, aes(hour,mean_clus8_we),colour= "magenta", size = 1)+
  geom_line(data=cluster9_mean_we, aes(hour,mean_clus9_we),colour= "yellow", size = 1)+
  geom_line(data=cluster10_mean_we, aes(hour,mean_clus10_we),colour= "purple", size = 1)+
  geom_line(data=cluster11_mean_we, aes(hour,mean_clus11_we),colour= "brown", size = 1)+
  geom_line(data=cluster12_mean_we, aes(hour,mean_clus12_we),colour= "cyan", size = 1)

allinone<-multiplot(all_wd,all_we)
#ggsave("All in one_WD_WE_WHD.PNG", device = "png", plot = last_plot(), width = 50, height = 35, units = "cm")
library(extrafont)
extrafont::loadfonts(device="win")
library(ggrepel)
windowsFonts(Times=windowsFont("TT Times New Roman"))
#Cluster wise multiplot


clus1_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #1 (n = 2577)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster1_mean, aes(hour,mean_clus1*15),colour= "red", size = 1.5) + geom_text(family = "Times") + 
  geom_line(data=cluster1_mean_we, aes(hour,mean_clus1_we*15),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 1_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus2_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.6) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.6, label='bold("Cluster #2 (n = 375)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.57,yend= 0.57,color="red",size=1.5) + annotate(geom="text", x=80, y=0.57, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.54,yend= 0.54,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.54, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster2_mean, aes(hour,mean_clus2*25),colour= "red", size = 1.5) +
  geom_line(data=cluster2_mean_we, aes(hour,mean_clus2_we*25),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 2_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus3_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #3 (n = 841)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster3_mean, aes(hour,mean_clus3*20),colour= "red", size = 1.5) +
  geom_line(data=cluster3_mean_we, aes(hour,mean_clus3_we*20),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 3_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus4_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #4 (n = 1611)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster4_mean, aes(hour,mean_clus4*15),colour= "red", size = 1.5) +
  geom_line(data=cluster4_mean_we, aes(hour,mean_clus4_we*15),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 4_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus5_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #5 (n = 341)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster5_mean, aes(hour,mean_clus5*16),colour= "red", size = 1.5) +
  geom_line(data=cluster5_mean_we, aes(hour,mean_clus5_we*16),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 5_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus6_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #6 (n = 413)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster6_mean, aes(hour,mean_clus6*15),colour= "red", size = 1.5) +
  geom_line(data=cluster6_mean_we, aes(hour,mean_clus6_we*15),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 6_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus7_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #7 (n = 1501)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster7_mean, aes(hour,mean_clus7*22),colour= "red", size = 1.5) +
  geom_line(data=cluster7_mean_we, aes(hour,mean_clus7_we*22),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 7_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus8_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #8 (n = 839)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster8_mean, aes(hour,mean_clus8*20),colour= "red", size = 1.5) +
  geom_line(data=cluster8_mean_we, aes(hour,mean_clus8_we*20),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 8_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus9_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #9 (n = 538)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster9_mean, aes(hour,mean_clus9*20),colour= "red", size = 1.5) +
  geom_line(data=cluster9_mean_we, aes(hour,mean_clus9_we*20),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 9_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus10_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #10 (n = 1118)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster10_mean, aes(hour,mean_clus10*25),colour= "red", size = 1.5) +
  geom_line(data=cluster10_mean_we, aes(hour,mean_clus10_we*25),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 10_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus11_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.4) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.4, label='bold("Cluster #11 (n = 1405)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.38,yend= 0.38,color="red",size=1.5) + annotate(geom="text", x=80, y=0.38, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.36,yend= 0.36,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.36, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster11_mean, aes(hour,mean_clus11*25),colour= "red", size = 1.5) +
  geom_line(data=cluster11_mean_we, aes(hour,mean_clus11_we*25),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 11_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

clus12_wd_we <- ggplot() +
  theme(axis.text = element_text("Times","bold","black",26),axis.title = element_text("Times","bold","black",26), panel.background = element_rect(fill = "white", color = "white"), panel.border = element_rect(colour = "black", fill=NA, size=1))+ 
  ylim(0, 0.45) + scale_x_continuous(labels = paste(seq(0, 24, by = 6), "h", sep = ""))+ 
  labs(x="Time(h)", y="Normalized power consumption (kW)") + annotate(geom="text", x=80, y=0.45, label='bold("Cluster #12 (n = 280)")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=70,y=0.425,yend= 0.425,color="red",size=1.5) + annotate(geom="text", x=80, y=0.425, label='bold("Week day")', family = "Times", size=10, parse=TRUE) +
  annotate(geom = "segment",x=65,xend=71,y=0.405,yend= 0.405,color="blue",size=1.5, linetype="dashed") + annotate(geom="text", x=80, y=0.405, label='bold("Week end")', family = "Times", size=10, parse=TRUE) +
  geom_line(data=cluster12_mean, aes(hour,mean_clus12*15),colour= "red", size = 1.5) +
  geom_line(data=cluster12_mean_we, aes(hour,mean_clus12_we*15),colour= "blue", size = 1.5, linetype="dashed")
ggsave("Plots/KOL_Cluster 12_WD_WE.PNG", plot = last_plot(), dpi = 300, width = 30, height = 20, units = "cm")

multiplot(clus1_wd_we, clus2_wd_we, clus3_wd_we, clus4_wd_we, clus5_wd_we, clus6_wd_we, 
          clus7_wd_we, clus8_wd_we, clus9_wd_we, clus10_wd_we, clus11_wd_we, clus12_wd_we, cols = 2)

##residuals per cluster
#all_means<-rbind(cluster1_mean,cluster2_mean,cluster3_mean,cluster4_mean,cluster5_mean,cluster6_mean,cluster7_mean)
#1
c1_res<-as.data.frame(cluster1[,2:97])
distances_1 <- sqrt(rowSums((c1_res - cluster1_mean$mean_clus1)^2))
#outliers_1 <- as.data.frame(sort(distances_1, decreasing = T)[1:5])
outliers_1 <- as.data.frame(sort(distances_1, decreasing = T))
names(outliers_1)<- "dist1"
outliers_1["idmeter1"] <- row.names(outliers_1)
outliers_1 <- outliers_1[c(2,1)]

#2
c2_res<-as.data.frame(cluster2[,2:97])
distances_2 <- sqrt(rowSums((c2_res - cluster2_mean$mean_clus2)^2))
#outliers_2 <- as.data.frame(sort(distances_2, decreasing = T)[1:5])
outliers_2 <- as.data.frame(sort(distances_2, decreasing = T))
names(outliers_2)<- "dist2"
outliers_2["idmeter2"] <- row.names(outliers_2)
outliers_2 <- outliers_2[c(2,1)]

#3
c3_res<-as.data.frame(cluster3[,2:97])
distances_3 <- sqrt(rowSums((c3_res - cluster3_mean$mean_clus3)^2))
#outliers_3 <- as.data.frame(sort(distances_3, decreasing = T)[1:5])
outliers_3 <- as.data.frame(sort(distances_3, decreasing = T))
names(outliers_3)<- "dist3"
outliers_3["idmeter3"] <- row.names(outliers_3)
outliers_3 <- outliers_3[c(2,1)]

#4
c4_res<-as.data.frame(cluster4[,2:97])
distances_4 <- sqrt(rowSums((c4_res - cluster4_mean$mean_clus4)^2))
#outliers_4 <- as.data.frame(sort(distances_4, decreasing = T)[1:5])
outliers_4 <- as.data.frame(sort(distances_4, decreasing = T))
names(outliers_4)<- "dist4"
outliers_4["idmeter4"] <- row.names(outliers_4)
outliers_4 <- outliers_4[c(2,1)]

#5
c5_res<-as.data.frame(cluster5[,2:97])
distances_5 <- sqrt(rowSums((c5_res - cluster5_mean$mean_clus5)^2))
#outliers_5 <- as.data.frame(sort(distances_5, decreasing = T)[1:5])
outliers_5 <- as.data.frame(sort(distances_5, decreasing = T))
names(outliers_5)<- "dist5"
outliers_5["idmeter5"] <- row.names(outliers_5)
outliers_5 <- outliers_5[c(2,1)]

#6
c6_res<-as.data.frame(cluster6[,2:97])
distances_6 <- sqrt(rowSums((c6_res - cluster6_mean$mean_clus6)^2))
#outliers_6 <- as.data.frame(sort(distances_6, decreasing = T)[1:5])
outliers_6 <- as.data.frame(sort(distances_6, decreasing = T))
names(outliers_6)<- "dist6"
outliers_6["idmeter6"] <- row.names(outliers_6)
outliers_6 <- outliers_6[c(2,1)]

#7
c7_res<-as.data.frame(cluster7[,2:97])
distances_7 <- sqrt(rowSums((c7_res - cluster7_mean$mean_clus7)^2))
#outliers_7 <- as.data.frame(sort(distances_7, decreasing = T)[1:5])
outliers_7 <- as.data.frame(sort(distances_7, decreasing = T))
names(outliers_7)<- "dist7"
outliers_7["idmeter7"] <- row.names(outliers_7)
outliers_7 <- outliers_7[c(2,1)]

all_dist <- data.frame(outliers_1,outliers_2,outliers_3,outliers_4,outliers_5,outliers_6,outliers_7)
row.names(all_dist) <- NULL

#Find the correlation between week days and week ends plots.
library(corrplot)
#Cluster 1
Clu1_WD <- as.data.frame(cluster1_mean$mean_clus1)
ccf1 <- ccf(clu1_WD, cluster1_mean_we$mean_clus1_we)
data_clu1 <-data.frame(clu1_WD, cluster1_mean_we$mean_clus1_we)
res1 <- cor(data_clu1)
corrplot(res1, type="upper", method = "number", hclust.method = "ward.D", insig = "p-value",
         order="hclust", tl.col="black", tl.srt=20)

#Cluster 2
ccf2 <- ccf(cluster2_mean$mean_clus2, cluster2_mean_we$mean_clus2_we)
data_clu2 <-data.frame(cluster2_mean$mean_clus2, cluster2_mean_we$mean_clus2_we)
res2 <- cor(data_clu2)
corrplot(res2, type="upper", method = "number", hclust.method = "ward.D", insig = "p-value",
         order="hclust", tl.col="black", tl.srt=20)

#Cluster 3
ccf3 <- ccf(cluster3_mean$mean_clus3, cluster3_mean_we$mean_clus3_we)
data_clu3 <-data.frame(cluster3_mean$mean_clus3, cluster3_mean_we$mean_clus3_we)
res3 <- cor(data_clu3)
corrplot(res3, type="upper", method = "number", hclust.method = "ward.D", insig = "p-value",
         order="hclust", tl.col="black", tl.srt=20)

#Cluster 4
ccf4 <- ccf(cluster4_mean$mean_clus4, cluster4_mean_we$mean_clus4_we)
data_clu4 <-data.frame(cluster4_mean$mean_clus4, cluster4_mean_we$mean_clus4_we)
res4 <- cor(data_clu4)
corrplot(res4, type="upper", method = "number", hclust.method = "ward.D", insig = "p-value",
         order="hclust", tl.col="black", tl.srt=20)

#Cluster 5
ccf5 <- ccf(cluster5_mean$mean_clus5, cluster5_mean_we$mean_clus5_we)
data_clu5 <-data.frame(cluster5_mean$mean_clus5, cluster5_mean_we$mean_clus5_we)
res5 <- cor(data_clu5)
corrplot(res5, type="upper", method = "number", hclust.method = "ward.D", insig = "p-value",
         order="hclust", tl.col="black", tl.srt=20)

#Cluster 6
ccf6 <- ccf(cluster6_mean$mean_clus6, cluster6_mean_we$mean_clus6_we)
data_clu6 <-data.frame(cluster6_mean$mean_clus6, cluster6_mean_we$mean_clus6_we)
res6 <- cor(data_clu6)
corrplot(res6, type="upper", method = "number", hclust.method = "ward.D", insig = "p-value",
         order="hclust", tl.col="black", tl.srt=20)

#Cluster 7
ccf7 <- ccf(cluster7_mean$mean_clus7, cluster7_mean_we$mean_clus7_we)
data_clu7 <-data.frame(cluster7_mean$mean_clus7, cluster7_mean_we$mean_clus7_we)
res7 <- cor(data_clu7)
corrplot(res7, type="upper", method = "pie", hclust.method = "ward.D", insig = "p-value",
         order="hclust", tl.col="black", tl.srt=20)

#Cluster 8
ccf8 <- ccf(cluster8_mean$mean_clus8, cluster8_mean_we$mean_clus8_we)
data_clu8 <-data.frame(cluster8_mean$mean_clus8, cluster8_mean_we$mean_clus8_we)
res8 <- cor(data_clu8)
corrplot(res8, type="upper", method = "pie", hclust.method = "ward.D", insig = "p-value",
         order="hclust", tl.col="black", tl.srt=20)

#########################################################################################
#Seperate csv based on cluster number starts
########################################################################################
write.csv(subset(clu_met, clu_met$clus_num ==1),"Cluster_CSV/clus1_meters.csv", row.names = FALSE)
write.csv(subset(clu_met, clu_met$clus_num ==2),"Cluster_CSV/clus2_meters.csv", row.names = FALSE)
write.csv(subset(clu_met, clu_met$clus_num ==3),"Cluster_CSV/clus3_meters.csv", row.names = FALSE)
write.csv(subset(clu_met, clu_met$clus_num ==4),"Cluster_CSV/clus4_meters.csv", row.names = FALSE)
write.csv(subset(clu_met, clu_met$clus_num ==5),"Cluster_CSV/clus5_meters.csv", row.names = FALSE)
write.csv(subset(clu_met, clu_met$clus_num ==6),"Cluster_CSV/clus6_meters.csv", row.names = FALSE)
write.csv(subset(clu_met, clu_met$clus_num ==7),"Cluster_CSV/clus7_meters.csv", row.names = FALSE)
#Seperate csv based on cluster number ends
