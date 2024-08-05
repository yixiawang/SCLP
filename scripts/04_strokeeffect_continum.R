# plot stroke effect, standardized response times as a function of stoke 
library(ggplot2)
library(dplyr)
di <-read.csv('./files/lp-items.csv')
dc <-read.csv('./files/feature.csv')
### get character over .66 acc
di_clean= di[!is.na(di$rt),]
dc.light <- dc[,c('item','num_strokes')]
dci <- merge(di_clean, dc.light, by=c("item"))
dr<- dci[which(dci$accuracy>=.66),]
dr <-dr[which(dr$lexicality=='character'),]
dr <- dr[!is.na(dr$zscore),]#remove na in zscore 
dim(dr)#4528
dr$num_strokes <- as.factor(dr$num_strokes)

# draw the plot 
plot <- ggplot(dr,aes(x=num_strokes,y=zscore))+ 
        geom_boxplot()+
        labs(title='Stroke Effect on Standardized Reaction Times')+
        theme(axis.text=element_text(size=8),
              axis.title=element_text(size=14),
              axis.title.x =element_text(size=11),
              axis.title.y =element_text(size=11))+
        xlab("Number of Strokes")+
        ylab("Standardized Reaction Times")
plot

# check number of characters less than 5
dim(dr[dr$num_strokes<4,])#76
dim(dr[dr$num_strokes<5,])#1] 201   8

