# in the paper: Results --> Reliability
library(dplyr)
#library(tidyverse)
#library(MASS)
#load data
di <-read.csv('./files/lp-items.csv')
clp <- read.csv('./files/Chinese Lexicon Project Sze et al.csv')#clp original file
mel<-read.csv('./files/meld.csv')#meld original file

################basic information ###########
########meld
# get mel one-character word information
mel<-subset(mel, nchar(word) == 1)#get one-character word 
mel <- mel[which(mel$lexicality=='1'),]
mel$item <- mel$word
mel$mel.RT <- mel$RT
mel$mel.zRT <- mel$zRT
mel$ACC <- 100- mel$ERR
mel$mel.ACC <- mel$ACC
mel$mel.ACC <- mel$ACC
mel.light <- mel[,names(mel)%in%c('item','mel.RT','mel.zRT','mel.ACC')]
as.data.frame(t(sapply(mel.light, function(cl) list(means=mean(cl,na.rm=TRUE), 
                                                 sds=sd(cl,na.rm=TRUE))) ))
#means       sds
#item            NA        NA
#mel.RT    854.5889  161.7162
#mel.zRT 0.04536703 0.5262892
#mel.ACC   81.86379  22.91497

# get clp information
clp$item <- clp$Character
clp$clp.RT <- clp$RT
clp$clp.zRT <- clp$Z.RT.
clp$clp.ACC <- clp$Acc
clp.light <- clp[,names(clp)%in%c('item','clp.RT','clp.zRT','clp.ACC')]
as.data.frame(t(sapply(clp.light, function(cl) list(means=mean(cl,na.rm=TRUE), 
                                                    sds=sd(cl,na.rm=TRUE))) ))
#means       sds
#clp.RT    601.6966  80.23807
#clp.zRT -0.1904408 0.3489493
#clp.ACC  0.9523429 0.1015329


### SCLP the current database  
di <-subset(di, nchar(item) == 1)
di$RT <- di$rt
di$zRT <- di$zscore
di$ACC<- di$accuracy
di.light <- di[,names(di)%in%c('item','RT','zRT','ACC')]
as.data.frame(t(sapply(di.light, function(cl) list(means=mean(cl,na.rm=TRUE), 
                                                    sds=sd(cl,na.rm=TRUE))) ))

#means       sds
#item        NA        NA
#RT    803.9015  173.6497
#zRT  0.2767672 0.7453509
#ACC  0.6130604 0.3792831

# correlation of these characters 
dilight.num <- di.light[,2:4]
corr.dilight.num <- round(cor(dilight.num,use='pairwise'),2)
corr.dilight.num
#RT   zRT   ACC
#RT   1.00  0.91 -0.56
#zRT  0.91  1.00 -0.62
#ACC -0.56 -0.62  1.00

########### correlation ##########
## cor with clp
di.clp <- merge(clp.light,di.light,on = 'item')#2497
dim.commonwithclp <- dim(di.clp)
#characters that in clp not in this dataset sclp
setdiff(clp$Character, di$item) # "礡" "丟" "拋"
di.clp.cor <- round(cor(di.clp[,2:7],use='pairwise'),2)#cor table used in the paper
di.clp.cor
#          clp.RT clp.zRT clp.ACC    
#RT        0.70    0.70   -0.53  
#zRT       0.70    0.73   -0.56  
#ACC      -0.53   -0.56    0.69 

as.data.frame(t(sapply(di.clp, function(cl) list(means=mean(cl,na.rm=TRUE), 
                                                    sds=sd(cl,na.rm=TRUE))) ))

#means       sds
#item         NA        NA
#clp.RT          601.5943  80.18938
#clp.zRT       -0.1909453 0.3486787
#clp.ACC        0.9528234 0.1001163
#RT              694.8613  97.20357
#zRT           -0.2279649 0.4258194
#ACC            0.9361717  0.105488

## cor with mel
di.mel <-  merge(mel.light,di.light,on = 'item')#982
dim.commonwithmel <- dim(di.mel)
di.mel.cor <- round(cor(di.mel[,2:7],use='pairwise'),2)#cor table used in the paper
di.mel.cor 
#         mel.RT mel.zRT mel.ACC    
#RT        0.71    0.73   -0.63  
#zRT       0.74    0.76   -0.66  
#ACC      -0.69   -0.69    0.87 

# merge three megastudies together to see how many characters shared in the three datasets 
clp.mel <- merge(clp.light,mel.light,on = 'item')
clp.mel.lp <- merge(di.light,clp.mel,on = 'item')
mega3.num <- clp.mel.lp[,2:10]
dim(mega3.num)#381 shared 
corr.mega3 <- round(cor(mega3.num,use='pairwise'),2)
is.matrix(corr.mega3)
#write.matrix(corr.mega3,'1010mega3_correlation_matrix.csv',sep=',')

