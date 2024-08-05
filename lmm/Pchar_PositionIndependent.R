
praw <- read.csv('./files/pchar_lmm_trials.csv')
dim(praw)
praw <- praw[!is.na(praw$zscore),]
dim(praw) #125013      8
length(unique(praw$item))#4750
head(praw,1)


#### transformation log10(x+1)
praw$nradical_frequency_log <-  log(praw$rad_freq_np+1,10)
praw$nresidual_frequency_log <- log(praw$res_freq_np+1,10)

## whether to log it or not
plot(praw$nradical_frequency_log,praw$res_freq_np)
plot(praw$nradical_frequency_log,praw$zscore)
plot(praw$rad_freq_np,praw$zscore)
plot(praw$nradical_frequency_log,praw$zscore)

cor.p <- praw[,c('zscore','nradical_frequency_log','res_freq_np')]
cor.p <-cor(cor.p,method='spearman')
cor.p

#                            zscore nradical_frequency_log res_freq_np
#zscore                 1.00000000             0.07444515   0.1618984
#nradical_frequency_log 0.07444515             1.00000000  -0.1764441
#res_freq_np            0.16189836            -0.17644412   1.0000000

## radlog and res
#pm0  <- lmer(zscore ~ nradical_frequency_log  * res_freq_np + (1|item) +(1|subject) ,data=praw) zero variability in subject
pm0  <- lmer(zscore ~ nradical_frequency_log  * res_freq_np + (1|item),data=praw)
summary.pm0 <- summary(pm0)
summary.pm0


#### Remove outliers  +-2.5 sd residual means 
residuals <- residuals(pm0)#get the residuals 
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
dim(p)#120573     10  

#### Refit the model 
pm1 <- lmer(zscore ~ nradical_frequency_log  * res_freq_np + (1|item),data=p,REML=FALSE)
summary.pm1 <- summary(pm1)

#### Back-fitting the model to test fixed effects factor 
pm2 <- bfFixefLMER_F.fnc(model = pm1,method='llrt')
summary.pm2 <- summary(pm2)
summary.pm2

#### forward fitting to test random slopes of every independent variable
pm3 <- ffRanefLMER.fnc(model = pm2, list(slopes = c("nradical_frequency_log","res_freq_np"), corr = c(0, 0), by.vars = c("item")))
summary.pm3 <- summary(pm3)
summary.pm3

aov.pm3 <- Anova(pm3,refit=FALSE,type=3)# with p-values from F-tests using Satterthwaite's denominator df
aov.pm3
#write.csv(as.matrix(aov.pm3), file = "AOV_np_pchar_nolog.csv", na = "")

#### back-fitting again in case the addition of random effects affect fixed effects
# nothing is added, so no need to fit again 


plot_interaction(p,pm3)#0.61  6.17 11.73
###########functions to plot interactions
plot_interaction <- function(p, pm6) {
  #### plot the effect 
  mean_rad <- round(mean(p$nradical_frequency_log),2)
  sd_rad <- sd(p$nradical_frequency_log)
  mean_res <- round(mean(p$res_freq_np),2)
  sd_res <- sd(p$res_freq_np)#

  low_rad <- round(mean_rad-sd_rad,2)#
  high_rad <- round(mean_rad+sd_rad,2)#
  low_res <- round(mean_res-sd_res,2)
  high_res <- round(mean_res+sd_res,2)
  print(c(low_res,mean_res,high_res))#0.80  5.67 10.54
  e.p <- allEffects(pm6,
                    xlevels =
                      list(nradical_frequency_log=c(low_rad,mean_rad,high_rad), 
                           res_freq_np = c(low_res,mean_res,high_res))
  )
  ep1 <- e.p[[1]]
  e.pdf <- as.data.frame(ep1)
  
  #scaleFUN <- function(x) sprintf("%.1f", x)
  pg <- ggplot(e.pdf,aes(x=nradical_frequency_log,y=fit,group=res_freq_np,col=res_freq_np,ymin=lower,ymax=upper)) + 
    geom_line(size = 1)+
    #scale_x_continuous(labels=scaleFUN)+
    #geom_text(data = subset(e.pdf, nradical_frequency_log == high_rad),aes(label = as.factor(nresidual_frequency_log)),hjust = .5)+
    theme(legend.position = "bottom",
          plot.title = element_text(size=18),
          legend.title=element_text(size=14),
          axis.title = element_text(size=15),
          axis.text=element_text(size=15))+
    labs(color="Residual Component Frequency")+
      xlab("Radical Frequency (log10)") + ylab("Predicted zRTs") + ggtitle("Two-way Interaction (Position-Independent)")
  return(pg)
  
}




