
praw <- read.csv('./files/pchar_lmm_trials.csv')
dim(praw)
praw <- praw[!is.na(praw$zscore),]
dim(praw) #125013      8
length(unique(praw$item))#4750
head(praw,1)

#### transformation log10(x+1)
praw$wradical_frequency_log <-  log(praw$rad_freq_wp+1,10)
praw$res_freq_wp <- log(praw$res_freq_wp+1,10)

cor.p <- praw[,c('zscore','wradical_frequency_log','res_freq_wp')]
cor.p <-cor(cor.p,method='spearman')
cor.p
#                         zscore wradical_frequency_log res_freq_wp
#zscore                  1.00000000             0.09645741              0.20313469
#wradical_frequency_log  0.09645741             1.00000000             -0.01794699
#res_freq_wp 0.20313469            -0.01794699              1.00000000

plot(praw$rad_freq_wp,praw$zscore)
plot(praw$wradical_frequency_log,praw$zscore)
plot(praw$res_freq_wp,praw$zscore)
plot(praw$res_freq_wp,praw$zscore)

# log rad, not resi

#### build initial model
#wpm0 <- lmer(zscore ~ wradical_frequency_log  * res_freq_wp + (1|item) +(1|subject),data=praw,REML=FALSE)
wpm0 <- lmer(zscore ~ wradical_frequency_log  * res_freq_wp + (1|item) ,data=praw,REML=FALSE)
summary.wpm0 <- summary(wpm0)#singularity as there is no variability of subject, so we remove the by-subject intercept and refit the model again (keep it maximal; Barr et al., 2013) 

#### Remove outliers  +-2.5 sd residual means 
residuals <- residuals(wpm0)#get the residuals 
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
p <- praw[-outliers, ]
dim(p)#120584     10 

#### build initial model
wpm1 <- lmer(zscore ~ wradical_frequency_log  * res_freq_wp + (1|item) ,data=p,REML=FALSE)
summary.wpm1 <- summary(wpm1)#singularity as there is no variability of subject, so we remove the by-subject intercept and refit the model again (keep it maximal; Barr et al., 2013) 

#### Back-fitting the model to test fixed effects factor 
wpm2 <- bfFixefLMER_F.fnc(model = wpm1,method='llrt')
summary.wpm2 <- summary(wpm2)
summary.wpm2

#### forward fitting to test random slopes of every independent variable
wpm3 <- ffRanefLMER.fnc(model = wpm2, list(slopes = c("wradical_frequency_log","res_freq_wp"), corr = c(0, 0), by.vars = c("item")))
summary.wpm3 <- summary(wpm3)
summary.wpm3 
## nothing to add, so we dont have to do the back-fitting
aov.wpm3 <- Anova(wpm3,refit=FALSE,type=3)# with p-values from F-tests using Satterthwaite's denominator df
aov.wpm3


plot_interaction(p,wpm3)#-0.03  3.77  7.57
###########functions to plot interactions
plot_interaction <- function(p, pm6) {
  #### plot the effect 
  mean_rad <- round(mean(p$wradical_frequency_log),2)
  sd_rad <- sd(p$wradical_frequency_log)
  mean_res <- round(mean(p$res_freq_wp),2)
  sd_res <- sd(p$res_freq_wp)#3.798874
  
  low_rad <- round(mean_rad-sd_rad,2)#
  high_rad <- round(mean_rad+sd_rad,2)#
  low_res <- round(mean_res-sd_res,2)
  high_res <- round(mean_res+sd_res,2)
  print(c(low_res,mean_res,high_res))
  e.p <- allEffects(pm6,
                    xlevels =
                      list(wradical_frequency_log=c(low_rad,mean_rad,high_rad), 
                           res_freq_wp = c(low_res,mean_res,high_res))
  )
  ep1 <- e.p[[1]]
  e.pdf <- as.data.frame(ep1)
  
  #scaleFUN <- function(x) sprintf("%.1f", x)
  pg <- ggplot(e.pdf,aes(x=wradical_frequency_log,y=fit,group=res_freq_wp,col=res_freq_wp,ymin=lower,ymax=upper)) + 
    geom_line(size = 1)+
    #scale_x_continuous(labels=scaleFUN)+
    #geom_text(data = subset(e.pdf, wradical_frequency_log == high_rad),aes(label = as.factor(nresidual_frequency_log)),hjust = .5)+
    theme(legend.position = "bottom",
          plot.title = element_text(size=18),
          legend.title=element_text(size=14),
          axis.title = element_text(size=15),
          axis.text=element_text(size=15))+
    labs(color="Residual Component Frequency")+
    xlab("Radical Frequency (log10)") + ylab("Predicted zRTs") + ggtitle("Two-way Interaction (Position-Dependent)")
  return(pg)
  
}

