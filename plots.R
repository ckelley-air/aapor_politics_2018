library(ggplot2)
library(magrittr)
library(dplyr)
library(RColorBrewer)
topics <- read.csv("~/Documents/personal/aapor_politics_2018/topics_over_time.csv")
topics$month <- ifelse(topics$month<10,paste0(0,as.character(topics$month)),as.character(topics$month))
topics$date <- as.Date(paste0(topics$month,"/01/",topics$year), "%m/%d/%Y")
topics <- topics[topics$year !=2012,]
topics$topic <- as.factor(topics$topic)

topics_sum <- topics %>% group_by(year,month) %>% summarize(total = sum(count))
topics_full <- merge(topics,topics_sum,by=c("year","month"),all.x=TRUE,all.y=TRUE)
topics_full$pct <- topics_full$count*100/topics_full$total

#fill in empty spots with o
full <- expand.grid("topic"=levels(topics$topic),"date"=unique(topics$date))
tf <- merge(topics_full,full,by=c("topic","date"),all.x=T,all.y=T)
tf$pct[is.na(tf$pct)] <- 0

p <- ggplot(tf, aes(x=date, y= pct)) +
  geom_area(data=tf,aes(fill= topic,color=c(brewer.pal(8,"Set2"),brewer.pal(7,"Set3"))), position = 'stack') 

geom_line(data=topics_full,aes(col=topic))