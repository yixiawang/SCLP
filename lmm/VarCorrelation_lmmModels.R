# load data 
raw <- read.csv("./files/lmm_trials.csv")
# check dimensionality
dim(raw)#110066 
# check the number of unique characters 
length(as.list(unique(raw$item)))#4225 unique characters 

# radical logged, freq logged, residual raw 
raw$nradical_frequency_log <-  log(raw$rad_freq_np+1,10)
raw$absolute_freq_sd_log <- log(raw$CHRCount+1,10)
raw$wradical_frequency_log <-  log(raw$rad_freq_wp+1,10)
raw$nresidual_frequency_log <-  log(raw$res_freq_np+1,10)
cols.char <- c('zscore','absolute_freq_sd_log','nradical_frequency_log','wradical_frequency_log'
              , 'res_freq_np','res_freq_wp' ,'nresidual_frequency_log')
raw.nums <- raw[,cols.char]
raw.nums.cor <- cor(raw.nums,method= 'spearman')
raw.nums.cor


# load pchar data 
praw <- read.csv("./files/pchar_lmm_trials.csv")
praw <- praw[!is.na(praw$zscore),]
# check dimensionality
dim(praw)#125013
# check the number of unique characters 
length(as.list(unique(praw$item)))#4750

# radical logged, freq logged, residual raw 
praw$nradical_frequency_log <-  log(praw$rad_freq_np+1,10)
praw$wradical_frequency_log <-  log(praw$rad_freq_wp+1,10)

cols.char <- c('zscore','nradical_frequency_log','wradical_frequency_log'
               , 'res_freq_np','res_freq_wp' )
praw.nums <- praw[,cols.char]
praw.nums.cor <- cor(praw.nums,method= 'spearman')
praw.nums.cor


