library(lme4)
#install.packages('car')#for Anova
library(car)
library(ggplot2)
library(dplyr)

# load trials and features
di <-read.csv('./files/lp-items.csv')
lp.trials <- read.csv('./files/lp-trials.csv')
features <- read.csv('./files/feature.csv')
freq <- read.csv('./files/SUBTLEX-CH-cleaned.csv')

### get clean characters whose accuracy over .66 acc 
di <- di[which(di$lexicality=='character'),]
di_clean <- di[!is.na(di$rt),]
di.over66 <- di_clean[which(di_clean$accuracy>=.66),]
dim(di.over66) #4528

# get characters which have entry in SUBLEXT-CH
features.light <- features[,c('item','num_strokes')]
dr <- merge(di.over66, features.light, by=c("item"))
freq.light <- freq[,c('item','CHR.million','CHRCount')]

# merge trials with frequency
dr <- merge(dr,freq.light,by=c('item'))
dr.rmna <- dr[!is.na(dr$CHR.million),]
dim(dr.rmna)#4394

# merge the stroke number and frequency with the trials
data <- lp.trials[lp.trials$item%in%dr.rmna$item,]
data <- merge(data,freq.light, by=c("item"))
data <- merge(data,features.light, by=c("item"))
dim(data)#127426

# get relevant columns 
data <- data[,c('item','subject','zscore','CHR.million','CHRCount','num_strokes')]
#save the data 
#write.csv(data,'./files/lmm_strokeeffect.csv')

# get log frequency log(x+1)
data$freq_log <- log10(data$CHRCount+1)
hist(data$freq_log)

# check the correlation 
data.num <- data[,c("zscore",'num_strokes','freq_log')]
data.num.rmna <- na.omit(data.num)
data.num.cor <- cor(data.num.rmna,method='spearman')
data.num.cor
#                 zscore num_strokes   freq_log
#zscore       1.0000000   0.3040457 -0.4124651
#num_strokes  0.3040457   1.0000000 -0.3117693
#freq_log    -0.4124651  -0.3117693  1.0000000

# fit the model 
m0 <- lmer(zscore ~ num_strokes *freq_log+ (1|item) +(1|subject),data=data)
sum.m0 <- summary(m0)
sum.m0

# aov to check significance 
aov.se.m0 <- Anova(m0,refit=FALSE,type=3)
aov.se.m0

#plot the interaction of freq & stroke number 
result_plot <- plot_interaction(data,m0)
result_plot#dim of the new data 25,3 

###########functions to plot interactions
plot_interaction <- function(data1, m1) {
  mean_freq <- round(mean(data1$freq_log), 2)
  sd_freq <- sd(data1$freq_log)
  mean_stroke <- round(mean(data1$num_strokes), 2)
  sd_stroke <- sd(data1$num_strokes)
  
  min_freq <- round(min(data1$freq_log), 2)
  max_freq <- round(max(data1$freq_log), 2) 
  min_stroke <- round(min(data1$num_strokes),2) 
  max_stroke <- round(max(data1$num_strokes,2))
  
  high_freq <- round(mean_freq + sd_freq, 2)
  low_freq <- round(mean_freq - sd_freq, 2)
  high_stroke <- round(mean_stroke + sd_stroke, 2)  
  low_stroke <- round(mean_stroke - sd_stroke, 2)
  
  newdata <- expand.grid(
    freq_log = c(min_freq,low_freq, mean_freq, high_freq,max_freq),
    num_strokes = c(min_stroke,low_stroke, mean_stroke, high_stroke,max_stroke)
  )
  
  newdata$y_pred <- predict(m1, newdata = newdata, re.form = NA)
  #print(dim(newdata))
  
  pg <- ggplot(newdata, aes(x = num_strokes , y = y_pred, group = freq_log, col = freq_log)) + 
    geom_line(size = .5) +
    theme(
      legend.position = "bottom",
      plot.title = element_text(size = 18),
      legend.title = element_text(size = 16),
      legend.text = element_text(size = 16),
      axis.title = element_text(size = 16),
      axis.text = element_text(size = 16)
    ) +
    labs(color = "Character Frequency (log10)") +
    xlab("Number of Strokes") + ylab("Predicted zRTs") + 
    ggtitle("Number of Strokes and Character Frequency Interaction") +
    geom_text(data = subset(newdata, num_strokes == max_stroke), aes(label = as.factor(freq_log)), size = 6.3, vjust = -.1)
  
  return(pg)
}


