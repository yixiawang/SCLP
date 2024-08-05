#purpose: clean data
#input: original LDT results
#ouput: item-level cleaned data & trial-level cleaned data
install.packages("lattice")
install.packages("irr")
install.packages("plotrix")
install.packages("Hmisc")
install.packages('reticulate')
#install.packages("XLConnect")
install.packages("readr")     
#.rs.restartR()#restart
library(lattice)
library(irr)
library(plotrix)
library(Hmisc)                           # Install readr package
library("readr") 

# in the file fullresults, subjects are already anonymized 
d<-read.csv('./files/fullresults.csv')

# inspect the column names
colnames(d)

# rename d to lp.trials
lp.trials<-d
rm(d)
#check dim
dim(lp.trials)

# make new variable rt.correct, which only contains correctly identified stimuli
lp.trials$rt.correct <- lp.trials$rt
lp.trials$rt.correct[lp.trials$accuracy==0] <- NA#get only rt for correct ones

# calculate the amount of removed trials
n.all.trials <- sum(lp.trials$rt>=0, na.rm=TRUE)
n.correct.trials <- sum(lp.trials$rt.correct>=0, na.rm=TRUE)
n.removed.trials.incorrect <- n.all.trials-n.correct.trials 
print(n.removed.trials.incorrect)
print(n.correct.trials)

# remove outliers by removing rt's beyond the 3 x interquartile distance
# define functions for q1 and q3
q1 <- function(x){
  return(quantile(x, .25, na.rm=TRUE))
}
q3 <- function(x){
  return(quantile(x, .75, na.rm=TRUE))
}

# get quantile per person
# this will make a data frame with for each combination of subject and lexicality (character vs pseudocharacter)
# the interquartile distance
pp.q1 <- with(lp.trials, aggregate(list(pp.q1=rt.correct), list(subject=subject, lexicality=lexicality), q1))
pp.q3 <- with(lp.trials, aggregate(list(pp.q3=rt.correct), list(subject=subject, lexicality=lexicality), q3))
# make sure participants are in same order (correct if TRUE)
all(pp.q1$subject == pp.q3$subject)
# combine q1 and q3 in one data frame
iqd <- pp.q1
iqd$pp.q3 <- pp.q3$pp.q3 #add quantile 3
rm(pp.q1, pp.q3)

# merge interquartile distance into the trials data frame
lp.trials <- merge(lp.trials, iqd, all.x=TRUE) #add quantile to the result file

# set new variable rt.correct.clean which will contain only the correct trials
# within 3 x interquartile distance 
lp.trials$rt.correct.clean <- lp.trials$rt.correct
lp.trials$rt.correct.clean[
  lp.trials$rt.correct < lp.trials$pp.q1-3*(lp.trials$pp.q3-lp.trials$pp.q1) |
    lp.trials$rt.correct > lp.trials$pp.q3+3*(lp.trials$pp.q3-lp.trials$pp.q1)
] <- NA

# calculate the amount of removed trials
n.correct.clean.trials <- sum(lp.trials$rt.correct.clean>=0, na.rm=TRUE)
n.removed.trials.iqd <- n.correct.trials-n.correct.clean.trials 
print(n.removed.trials.iqd)
print(n.correct.clean.trials)
fivenum(lp.trials$rt.correct)
fivenum(lp.trials$rt.correct.clean)

# remove rt's below 200
lp.trials$rt.correct.clean[
  lp.trials$rt.correct < 200
] <- NA
print(n.correct.clean.trials)#264304

lp.trials$rt.raw<-lp.trials$rt
lp.trials$rt<-lp.trials$rt.correct.clean

# calculate the amount of removed trials
n.correct.clean.trials.gt200 <- sum(lp.trials$rt.correct.clean>=0, na.rm=TRUE)
n.removed.trials. <-n.correct.clean.trials - n.correct.clean.trials.gt200
print(n.removed.trials.)#5
print(n.correct.clean.trials.gt200)

# calculate z-scores per subject, block, and lexicality
tmp.mean<-with(lp.trials,aggregate(rt,list(subject,lexicality,block),FUN=mean,na.rm=TRUE))
names(tmp.mean)<-c('subject','lexicality','block','mean')
tmp.sd<-with(lp.trials,aggregate(rt,list(subject,lexicality,block),FUN=sd,na.rm=TRUE))
names(tmp.sd)<-c('subject','lexicality','block','sd')
tmp.mean$sd<-tmp.sd$sd
lp.trials<-merge(lp.trials,tmp.mean)
rm(tmp.mean,tmp.sd)
lp.trials$zscore<-with(lp.trials,(rt-mean)/sd)

# construct item data frame
# aggregate to the item level
lp.items<-with(lp.trials,aggregate(cbind(rt,zscore,accuracy),list(item,lexicality),mean,na.rm=TRUE))
names(lp.items)<-c('item','lexicality','rt','zscore','accuracy')
items.sd<-with(lp.trials,aggregate(cbind(rt,zscore,accuracy),list(item,lexicality),sd,na.rm=TRUE))
names(items.sd)<-c('item','lexicality','rt.sd','zscore.sd','accuracy.sd')
lp.items<-data.frame(lp.items,items.sd[3:5])

# the z-score per participant and lexicality are
# check zscore 
check<- lp.items[!is.na(lp.items$zscore),]
check2<-lp.trials[!is.na(lp.trials$zscore),]
check_r <-lp.items[which(lp.items$lexicality=='character'),]
check_p <-lp.items[which(lp.items$lexicality=='pseudocharacter'),]
#head(check_r)
check_r<-  check_r[!is.na(check_r$zscore),]
check_p<-check_p[!is.na(check_p$zscore),]

mean(check$zscore)
mean(check2$zscore)
mean(check_r$zscore)
mean(check_p$zscore)

# save trial data
# first do some cleaning
lp.trials$mean<-NULL
lp.trials$sd<-NULL
lp.trials$rt.correct<-NULL
lp.trials$rt.correct.clean<-NULL
lp.trials$pp.q1<-NULL
lp.trials$pp.q3<-NULL
lp.trials$zscore<-NULL

# remove the character 𬬻 as the image is errorneous
cols <- colnames(lp.trials)
cols.withoutsti <- cols[cols!='item'] 
lp.trials[lp.trials$item=='𬬻',cols.withoutsti] <- NaN

cols.litem <- colnames(lp.items)
cols.litem.withoutsti <- cols.litem[cols.litem!='item'] 
lp.items[lp.items$item=='𬬻',cols.litem.withoutsti] <- NaN

# now save it
#save(lp.trials,file='lp_trials.Rdata')
#write.table(lp.trials, file='lp-trials.txt',sep=',',quote=F,row.names=FALSE)
#write.table(lp.trials, file='lp-trials.csv',sep=',',quote=F,row.names=FALSE)

# save items
#save(lp.items,file='lp-items.Rdata')
#write.table(lp.items,file='lp-items.csv',sep=',',quote=F,row.names=FALSE)

