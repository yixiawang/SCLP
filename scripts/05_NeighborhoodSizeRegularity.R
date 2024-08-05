# in the paper: Result --> Virtual Experiments --> Effect of Neighborhood Size

#install.packages('ez')
library(ez)# for ezANOVA
library(dplyr)
library(emmeans)

library(lme4)
#install.packages('car')#for Anova
library(car)

lp.trials<-read.csv('./files/lp-trials.csv')
v1 <- read.csv('./files/feature.csv')
di <-read.csv('./files/lp-items.csv')

# first get the chars
# get necessary characteristics v1 and v1_label has the label for ns & regularity  
v1 <- v1[,c('item','v1','v1_label','logCHR')]
v1 <- v1[!is.na(v1$v1_label),]#those are labeled in the virtual experiment
dim(v1)#119 4 remove duplicate 特 
di.clean <- di[!is.na(di$rt),]# ldt data remove na
di.clean <- di.clean[which(di.clean$accuracy>0.66),]#remove 性 级
dim(di.clean)#9278    
#merge response latency with characteristics
dci <- merge(di.clean, v1, by=c("item"))
dim(dci)#117
chars.ns <- as.list(dci$item)
length(chars.ns) #117
### select trials of the 117 items and remove na in zscore
lp.trials.ns<- lp.trials[lp.trials$item %in% chars.ns,]
dim(lp.trials.ns)#3393   16
lp.trials.ns<-lp.trials.ns[!is.na(lp.trials.ns$zscore),]#3154 
#merge trials with features
merged.trials.ns <- merge(lp.trials.ns,v1, by=c("item"))
dim(merged.trials.ns)# 3154   
# lets separate the label of regularity and ns
merged.trials.ns<-transform(
  merged.trials.ns,
  regularity=
    ifelse(
      merged.trials.ns$v1_label == 1 | merged.trials.ns$v1_label == 2,#3，4inconsistent
      'regular',
      'irregular'
    )
)

merged.trials.ns<-transform(
  merged.trials.ns,
  ns=
    ifelse(
      merged.trials.ns$v1_label ==1 | merged.trials.ns$v1_label == 3,#2,4 small
      'big',
      'small'
    )
)

merged.trials.ns <- merged.trials.ns[,c('item','subject','rt','zscore','accuracy','v1_label','ns','regularity')]

# regulate data type
merged.trials.ns$v1_label <- as.factor(merged.trials.ns$v1_label)
merged.trials.ns$subject <- as.factor(merged.trials.ns$subject)
merged.trials.ns$ns <- as.factor(merged.trials.ns$ns)
merged.trials.ns$regularity <- as.factor(merged.trials.ns$regularity)
merged.trials.ns$item <- as.factor(merged.trials.ns$item)

#some basic info
ns.items <- with(merged.trials.ns,aggregate(cbind(zscore,rt,accuracy),list(item,v1_label),mean,na.rm=TRUE))
names(ns.items)<-c('item','nslabels','zscore','rt','acc')
ns.items$nslabels <-as.factor(ns.items$nslabels)
group_by(ns.items,nslabels) %>%
  summarise(
    count = n(),
    mean = mean(rt, na.rm = TRUE),
    sd = sd(rt, na.rm = TRUE)
  )
#nslabels count  mean    sd
#<fct>    <int> <dbl> <dbl>
#1 1      29  647.  57.5
#2 2      30  673.  66.0
#3 3      28  693.  63.4
#4 4      30  669.  58.7

# Run the two-way repeated measures ANOVA
ez_result.ns <- ezANOVA(data = merged.trials.ns, dv = .(rt), wid = .(subject), within = .(regularity, ns), within_full= .(regularity, ns),
                         return_aov=TRUE,type=3)
summary(ez_result.ns)
ez_result.ns
# regularity main effect & interaction effect
# when NS is big, r faster, when NS is small, ir faster.

##### use lmm over the same characters 
dim(merged.trials.ns)
head(merged.trials.ns,1)
# we include frequency in the lmm 

m0.ns.rt <- lmer(rt ~ ns*regularity  + (1|item) +(1|subject),data=merged.trials.ns)
sum.m0.ns.rt <- summary(m0.ns.rt)
sum.m0.ns.rt


#Fixed effects:
#  Estimate Std. Error t value
#(Intercept)                 691.83      16.07  43.055
#nssmall                     -21.89      15.94  -1.374
#regularityregular           -46.05      16.05  -2.869
#nssmall:regularityregular    48.32      22.40   2.157

aov.ns.rt <- Anova(m0.ns.rt,type=3)
aov.ns.rt

#Response: rt
#Chisq Df Pr(>Chisq)    
#(Intercept)   1853.7290  1  < 2.2e-16 ***
#  ns               1.8875  1   0.169489    
#regularity       8.2289  1   0.004123 ** 
#  ns:regularity    4.6537  1   0.030986 * 
