#in this file we run lmm for pseudocharacters
ltrial <- read.csv('./files/lp-trials.csv')
subcomponent_freq_df <- read.csv('./files/pchar_subcomponent_freq.csv')
litems <-  read.csv('./files/lp-items.csv')

# inspect data 
colnames(subcomponent_freq_df)
colnames(ltrial)

# only get those acc > .66
litems.p <-litems[litems$lexicality=='pseudocharacter',]
litems.over66 <- litems.p[litems.p$accuracy>.66,]
min(litems.over66$accuracy)#0.6896
pchars.over66 <- as.list(litems.over66$item)
len_p <- length(pchars.over66) 
len_p#4750
ltrial.over66 <- ltrial[ltrial$item %in% pchars.over66,]
dim(ltrial.over66)

# merge two files 
df <- merge(subcomponent_freq_df,ltrial.over66,by = 'item')
df$item <- df$item
dim(df)

df.light <- df[,c('item','subject','zscore',"res_freq_np","res_freq_wp","rad_freq_np","rad_freq_wp")]
#write.csv(df.light, file = "./files/pchar_lmm_trials.csv", na = "")


