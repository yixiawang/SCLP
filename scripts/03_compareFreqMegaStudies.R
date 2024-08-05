library(dplyr)

#setwd("~/SimplifiedChineseLexiconProject")
features <- read.csv('./files/SUBTLEX-CH-cleaned.csv')# rename the file 
features$item <- features$Character
features <- features[,c('item','logCHR','CHRCount','CHR.million')]

# load lp-item, clp and meld and unify the item names  
di <-read.csv('./files/lp-items.csv')
clp <- read.csv('./files/Chinese Lexicon Project Sze et al.csv')#clp original file
clp$item <- clp$Character
mel<-read.csv('./files/meld.csv')#meld original file
mel$acc <- (100-mel$ERR)/100
mel <- mel[,names(mel)%in%c('word','lexicality','RT','zRT','acc')]
mel$item <- mel$word

## merge 
mel.f <- merge(mel,features,on='item')#1022 '嘚' '沢' are real characters in SUBTLEX-CH but pseudocharacter in mel
dim(mel.f)
# only get the chars in mel
mel.c<- mel.f[mel.f$lexicality==1,]#only real character 
mel.c <- merge(mel.c,features,on='item')#1020
clp <- merge(clp,features,on='item')#2482
di.f <-merge(di,features,on='item')#5298

#change datatype, the number is separated by , we change it to decimal
mel.c$logCHR <-as.numeric(gsub(",", ".", mel.c$logCHR))
clp$logCHR <- as.numeric(gsub(",", ".", clp$logCHR))
di.f $logCHR <- as.numeric(gsub(",", ".", di.f $logCHR))

# check means sd min max
mel.freq.stats <- summary(mel.c$logCHR)
mel.freq.stats
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.602   2.579   2.460   3.430   5.957 
clp.freq.stats <- summary(clp$logCHR)
clp.freq.stats
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   2.502   3.207   3.145   3.857   6.314
di.freq.stats <- summary(di.f $logCHR)
di.freq.stats
#Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#0.000   1.447   2.482   2.430   3.427   6.314 


#### overlapping between meld pseudocharacters and sclp characters
mel.p <- mel[mel$lexicality == 2,]
mel.p <- subset(mel.p,nchar(mel.p$word)==1)
dim(mel.p)#1020 6
intersects.mel.sclp <- intersect(mel.p$word,di$item)
#"䓬" "㙍" "㬚" "㙘" "㬎" "㮾" "㧐" "䢼" "㑊" "嘚" "㠓" "䅟"
#inter.mel <- di[di$item%in%intersects.mel.sclp,]
length(intersects.mel.sclp)#12 characters are labeled as pseudocharacter in meld

