#setwd("~/lexicalproject")
#load data
di <-read.csv('./files/lp-items.csv')
freq <-read.csv('./files/SUBTLEX-CH-cleaned.csv')
freq$item <- freq$Character

## clean item level df by removing rt nan and only extract those with acc over .66
di.clean<- di[!is.na(di$rt),]#cleaned
di.over66 <- di.clean[di.clean$accuracy>.66,]
dim(di.over66)#9278

# merge properties 
dci <- merge(di.over66, freq, by=c("item"))
dim(dci)# 4394 [original:5936]

#sort the dataset according to logCHR
dci <- dci[order(dci$logCHR),]

# bin character by a interval of 300, get their mean frequency and mean zscore
len <- length(dci$logCHR)
bin_num <- 300
freq.bins <- as.list(by(dci$logCHR, ceiling(1:len / bin_num), mean))
freq.sd <- as.list(by(dci$logCHR, ceiling(1:len / bin_num), sd))
rtz.bins <- as.list(by(dci$zscore, ceiling(1:len / bin_num), mean))
rtz.sd <- as.list(by(dci$zscore, ceiling(1:len / bin_num), sd))
acc.bins <- as.list(by(dci$accuracy, ceiling(1:len / bin_num), mean))
acc.sd <- as.list(by(dci$accuracy, ceiling(1:len / bin_num), sd))
d <- do.call(rbind, Map(data.frame, frequency =freq.bins, rtz =rtz.bins,
                        rtz.sd = rtz.sd, accuracy = acc.bins, acc.sd = acc.sd
))

# draw the plot
p2 <- ggplot(d,aes(x=round(frequency,2),y=rtz,group=round(frequency,2)))+
  geom_point(color='blue')+
  geom_errorbar(aes(ymin=rtz-rtz.sd, ymax=rtz+rtz.sd), width=.1,color='blue')+
  geom_point(aes(y=accuracy,color='red'))+
  geom_errorbar(aes(ymin=accuracy-acc.sd, ymax=accuracy+acc.sd), width=.1,color='red')+
  scale_y_continuous(
    # features of the first axis
    name = 'Standardized Response Times',
    # add a second axis 
    sec.axis = sec_axis(~.+0,name ='Accuracy'))+
  xlab('Log 10 Frequency')+
  ggtitle('Frequency Effect on Standardized Response Times and Accuracy')+
  theme(plot.title = element_text(size=18),axis.title = element_text(size=16),axis.text=element_text(size=16),legend.position="none")

p2
