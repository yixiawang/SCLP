
# load data 
raw <- read.csv("./files/lmm_trials.csv")

# check dimensionality
dim(raw)#110066 11

#### transformation log10(x+1)
raw$wradical_frequency_log <-  log(raw$rad_freq_wp+1,10)
raw$absolute_freq_sd_log <- log(raw$CHRCount+1,10)
raw$wresidual_frequency_log <- log(raw$res_freq_wp+1,10)

## use data distribution to decide log or not
#plot(raw$rad_freq_wp,raw$zscore)
#plot(raw$wradical_frequency_log,raw$zscore)
#plot(raw$res_freq_wp,raw$zscore)

# rad log, res no log 
#### build initial model
m0 <- lmer(zscore ~  absolute_freq_sd_log * wradical_frequency_log  * res_freq_wp + (1|item) +(1|subject),data=raw)
summary(m0)

#### Remove outliers  +-2.5 sd residual means 
residuals <- residuals(m0)#get the residuals 
residuals_mean <- mean(residuals)
residuals_sd <- sd(residuals)

# Defining the threshold for outliers (2.5 standard deviations)
outlier_threshold <- 2.5

# Calculating upper and lower bounds for outliers
upper_bound <- residuals_mean + outlier_threshold * residuals_sd
lower_bound <- residuals_mean - outlier_threshold * residuals_sd

# Identifying outliers
outliers <- which(residuals > upper_bound | residuals < lower_bound)

# Remove outliers from the original dataset
r <- raw[-outliers, ]
dim(r)#106057     14  
rm(raw)

#### Refit the model 
m1 <- lmer(zscore ~  absolute_freq_sd_log * wradical_frequency_log  * res_freq_wp + (1|item) +(1|subject),data=r)
summary.m1 <- summary(m1)

#### Back-fitting the model to test fixed effects factor 
m2 <- bfFixefLMER_F.fnc(model = m1,method='llrt')
summary.m2 <- summary(m2)
summary.m2

#### forward fitting to test random slopes of every independent variable
m3 <- ffRanefLMER.fnc(model = m2, list(slopes = c("absolute_freq_sd_log",
                                                  "wradical_frequency_log",
                                                  "res_freq_wp"), corr = c(0, 0, 0), by.vars = c("subject","item")))
summary.m3 <- summary(m3)

#### back-fitting again in case the addition of random effects affect fixed effects
m4 <- bfFixefLMER_F.fnc(model = m3 ,method='llrt')
summary.m4 <- summary(m4)
summary.m4
aov.m4 <- Anova(m4,refit=FALSE,type=3)# with p-values from F-tests using Satterthwaite's denominator df
aov.m4


plot_interaction(r,m4)
#####
plot_interaction <- function(r_filtered, m6) {
  
  mean_freq <- round(mean(r_filtered$absolute_freq_sd_log),2)#2.8
  sd_freq <- round(sd(r_filtered$absolute_freq_sd_log),2)#1.1
  
  mean_rad <- round(mean(r_filtered$wradical_frequency_log),2)#1.9
  sd_rad <- round(sd(r_filtered$wradical_frequency_log),2)#.6
  
  mean_res <- round(mean(r_filtered$res_freq_wp),2)#.9
  sd_res <- round(sd(r_filtered$res_freq_wp),2)#.3
  
  low_freq <- round(mean_freq-sd_freq,2)
  high_freq <- round(mean_freq+sd_freq,2)
  low_rad <- round(mean_rad-sd_rad,2)
  high_rad <- round(mean_rad+sd_rad,2)
  low_res <- round(mean_res-sd_res,2)
  high_res <- round(mean_res+sd_res,2)
  
  
  print('charfreq')
  print(c(low_freq,mean_freq,high_freq))
  print('resfreq')
  print( c(low_res,mean_res,high_res))
  e <- allEffects(m6, 
                  xlevels =
                    list(wradical_frequency_log=c(low_rad,mean_rad,high_rad), 
                         res_freq_wp = c(low_res,mean_res,high_res), 
                         absolute_freq_sd_log=c(low_freq,mean_freq,high_freq))
  )
  e1 <- e[[1]]
  e.df <- as.data.frame(e1)
  dim(e.df)#27 7
  
  #print
  #scaleFUN <- function(x) sprintf("%.1f", x)
  e.df$facetlabels <- as.factor(e.df$absolute_freq_sd_log)
  
  cfreq_names <- c(
    "1.7" = "CharFreq 1.70",
    "2.8" = "CharFreq 2.80",
    "3.9" = "CharFreq 3.90")
  
  g1 <- ggplot(e.df,aes(x=wradical_frequency_log,y=fit,group=res_freq_wp,col=res_freq_wp,ymin=lower,ymax=upper)) + 
    geom_line(size = 1)+
    facet_grid(.~facetlabels,labeller=as_labeller(cfreq_names))+ 
    theme(strip.text.x = element_text(size = 12),
          legend.position = "bottom",panel.spacing = unit(1, "lines"),
          plot.title = element_text(size=18),
          axis.text.x = element_text(size=14),
          axis.text.y = element_text(size=14),
          legend.title=element_text(size=16),
          axis.title = element_text(size=16),
          axis.text=element_text(size=16))+
    labs(color="Residual Component Frequency")+
    xlab("Radical Frequency (log10)") + ylab("Predicted zRTs") + ggtitle("Three-way Interaction Effects (Position-Dependent)")
  return (g1)}

