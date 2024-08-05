library(dplyr)
# load trials and features
lp.items <- read.csv('./files/lp-items.csv')
lp.trials <- read.csv('./files/lp-trials.csv')
strokes <- read.csv('./files/strokeeffect_item40.csv',sep=';',check.names = FALSE)#40 items from the original paper

# get the 40 chars from the original paper for replication
chars.se <- strokes$item
# get measures from SCLP for these 40 chars
lp.items.se<- lp.items[lp.items$item %in% chars.se,]
dim(lp.items.se) #40 7 
# merge the feature with measures  
merged <- merge(lp.items.se,strokes,on='item')
merged.rn <- na.omit(merged)
dim(merged.rn)#40 12 
# remove the column Var.8
merged.rn <- merged.rn[, -which(names(merged.rn) == "Var.8")]
# change the label datatype to factor 
merged.rn$TypeOriginal <- as.factor(merged.rn$TypeOriginal )
# get the basic info: mean response times and sd for each group 
group_by(merged.rn, TypeOriginal) %>%
  summarise(
    count = n(),
    mean = mean(rt, na.rm = TRUE),
    sd = sd(rt, na.rm = TRUE)
  )
#TypeOriginal count  mean    sd
#<fct>        <int> <dbl> <dbl>
#1 0           20  649.  59.5
#2 1           20  718.  92.8

# mean of the traditional characters' stroke numbers 
group_by(merged.rn, TypeOriginal) %>%
  summarise(
    count = n(),
    mean = mean(num_str_traditional, na.rm = TRUE),
    sd = sd(num_str_traditional, na.rm = TRUE)
  )
#   TypeOriginal count  mean    sd
#   <fct>        <int> <dbl> <dbl>
#1 0               20   7.4  1.27
#2 1               20  13.6  1.10

# mean of the simplified characters' stroke numbers 
group_by(merged.rn, TypeOriginal) %>%
  summarise(
    count = n(),
    mean = mean(num_strokes, na.rm = TRUE),
    sd = sd(num_strokes, na.rm = TRUE)
  )
#TypeOriginal count  mean    sd
#<fct>        <int> <dbl> <dbl>
#1 0               20  7.15  1.14
#2 1               20 11.0   2.96

##### aov by-item #####
merged.rn$TypeOriginal <- as.factor(merged.rn$TypeOriginal )
se.items.aov <- aov(rt ~ TypeOriginal, data = merged.rn)
sum.se.items <- summary(se.items.aov)
sum.se.items 

#             Df Sum Sq Mean Sq F value  Pr(>F)   
#TypeOriginal  1  47219   47219   7.767 0.00826 **
#Residuals    38 231025    6080   

##### aov by-subject #####
# select all trials of the 40 items
lp.trials.se<- lp.trials[lp.trials$item %in% chars.se,]
dim(lp.trials.se)#1160
# get stroke numbers 
merged.trials <- merge(lp.trials.se[,c('subject','item','rt')],strokes,on='item')
# remove na in trials 
merged.trials.rnma <- na.omit(merged.trials)
dim(merged.trials.rnma)#1069 8

# get the subject means over items 
se.subjects<-with(merged.trials.rnma,aggregate(cbind(rt),list(subject,TypeOriginal),mean,na.rm=TRUE))
dim(se.subjects)#58 3 subjects, two groups 
names(se.subjects)<-c('subject','type','rt')
se.subjects$type <- as.factor(se.subjects$type)

# repeated measures ANOVA for within-subject design 
aov_repeated_subject <- aov(rt~type + Error(subject/type), data = se.subjects)
sum.se.subjects <- summary(aov_repeated_subject)
sum.se.subjects
#Error: subject
#         Df Sum Sq Mean Sq F value Pr(>F)
#Residuals 28 338138   12076               

#Error: subject:type
           #Df Sum Sq Mean Sq F value   Pr(>F)    
#type       1  63283   63283   28.65 1.06e-05 ***
#Residuals 28  61857    2209   
