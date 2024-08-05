# create a function based on 1128reliability_test.R so that we can know the figure for all.
library(dplyr)
library(tidyr)

# we used all clean data here for reliability
#load data
d <- read.csv('./files/lp-trials.csv')

# remove trials whose rt is na
d.rmna <- d %>% drop_na(zscore) #drop all columns with na 
dim(d)#376101   
dim(d.rmna) #264297

### results
reliability_test('rt')##0.7021737
reliability_test('zscore')##0.753422
reliability_test_acc('accuracy')##0.9666622

##### functions 
##  reliability for rt and rtz
reliability_test<-function(colname){
  # get the needed variable
  temp <- d.rmna[,c('item','subject',colname)]
  # spread the dataframe so column name would be all items and subject is the index
  temp2<- temp %>% spread(key=item,value=colname)
  # rename temp 
  d.rt <-temp2
  # remove temps 
  rm(temp2)
  rm(temp)
  
  # randomly sample the dataframe 
  set.seed(42) 
  d.rt <- d.rt[sample(nrow(d.rt)),]
  #print(head(d.rt,3))
  # split into first half and second half(first half 14 and second half 15) 
  d14 <- d.rt[0:14,]
  #print(dim(d14))
  d15 <- d.rt[15:29,]
  #print(dim(d15))
 
  # get the mean of the two halves
  d14.mean <- as.data.frame.list(colMeans(d14[,-1],na.rm = TRUE))
  d15.mean <- as.data.frame.list(colMeans(d15[,-1],na.rm = TRUE))
  # translate the result into dataframes
  d14.mean.t <- as.data.frame(t(d14.mean))
  d15.mean.t <- as.data.frame(t(d15.mean))
  #print(dim(d15.mean.t))
  # remove na in the means(rn result from the cells that all responses are incorrect) 
  d14.rmna <- d14.mean.t%>%drop_na()#drop cells with na
  d15.rmna <- d15.mean.t%>%drop_na()
  #print(dim(d14.rmna))#12057
  #print(dim(d15.rmna))#12181
  # merge two dataframes to retain overlapping items 
  tmp <-merge(d14.rmna, d15.rmna, by.x = 0, by.y = 0)
  # compute correlation for overlapping items 
  #print(dim(tmp))#11696 for zscore 
  cor.rt <- cor(tmp$V1.x,tmp$V1.y)
  # apply Spearman–Brown prophecy formula for correction.
  #rcor*2/(1+r) 
  cor.rt.final <-cor.rt*2/(1+cor.rt)
  return(cor.rt.final)
} 

### reliability for acc
reliability_test_acc<-function(colname){
  # the difference is only colMeans(df[-1]), this is used to compute percentage of accuracy for binary variabels
#colname = 'accuracy'
temp <- d.rmna[,c('item','subject',colname)]
temp2<- temp %>% spread(key=item,value=colname)
d.rt <-temp2
rm(temp2)
rm(temp)

set.seed(42) 
d.rt <- d.rt[sample(nrow(d.rt)),]

# split into first half and second half 
d14 <- d.rt[0:14,]#14 12543
d15 <- d.rt[15:29,]#15 12543

# fill na with zero#? 
d14[is.na(d14)]<-0 # we turn them back to 0 so we can calculate the percentage 
d15[is.na(d15)]<-0

# get the mean of the two halves
d14.mean <- as.data.frame.list(colMeans(d14[-1],na.rm = TRUE))#percentage of accuracy for binary variables 
d14.mean.t <- as.data.frame(t(d14.mean))
d15.mean <- as.data.frame.list(colMeans(d15[-1],na.rm = TRUE))
d15.mean.t <- as.data.frame(t(d15.mean))
#print(dim(d15.mean.t))
# rm na 
d14.rmna <- d14.mean.t%>%drop_na()
d15.rmna <- d15.mean.t%>%drop_na()
#print(dim(d14.rmna))
#print(dim(d15.rmna))
# check overlapping items 
tmp <-merge(d14.rmna, d15.rmna, by.x = 0, by.y = 0)
#print(dim(tmp))# compute correlation 
cor.rt <- cor(tmp$V1.x,tmp$V1.y)
# apply  Spearman–Brown prophecy formula.
#rcor*2/(1+r) 
cor.rt.final <- cor.rt*2/(1+cor.rt)
return(cor.rt.final)
}
