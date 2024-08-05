
# load data
litem <- read.csv('./files/lp-items.csv')
ltrial <- read.csv('./files/lp-trials.csv')
freqsd <- read.csv('./files/SUBTLEX-CH-cleaned.csv')
freqsd$item <- freqsd$Character

# only obtain trials of those over .66
litems.over66 <- litem[litem$accuracy>.66,]
litems.over66.char <- litems.over66[litems.over66$lexicality=='character',] 
dim(litems.over66.char)
litems.chars <- as.list(unique(litems.over66.char$item))
length(litems.chars)#4529
ltrial <- ltrial[ltrial$item %in% litems.chars,]
unique.ltrial.char <- unique(ltrial$item) #4528
length(unique.ltrial.char)#4528
# there is 1 character difference 
setdiff(litems.chars,unique.ltrial.char)#returns NA
rows_with_na <- which(is.na(litem$item))# then if we check NA, integer(0)
litem[rows_with_na,]#the first row where the column names are stored. so we go with the 4528 characters.

# merge characters with freqsd
freqsd <- freqsd[,c('item','CHRCount')]
lmm.data <- merge(ltrial,freqsd,by = 'item', all.x = TRUE,all.y = FALSE)#only retain those in our dataset
length(unique(lmm.data$item))#4528
rm(ltrial)
rm(freqsd)

# radical frequency and residual frequency to all characters 
subcomponent_freq_df <- read.csv('./files/subcomponent_freq.csv')
# inspect data 
colnames(subcomponent_freq_df)

# merge two files 
df <- merge(lmm.data,subcomponent_freq_df,by = 'item')
dim(df)#131312     23
rm(lmm.data)

# how many characters in total?
length(unique(df$item))#4528
check <- df[!is.na(df$CHRCount),]
len_sti <- length(unique(check$item))
len_sti#4394 in freqsd 134 missing 
check2<-  check[!is.na(check$zscore),]
length(unique(check2$item))# 4394 all have zscore
check3 <- check2[!is.na(check2$res_freq_np),]
length(unique(check3$item))# 4225 169 dont have res_freq
lmm.df.clean <- check3
rm(check) #119033
rm(check2)
rm(check3)

lmm.df.clean.light <- lmm.df.clean[,c('item','subject','accuracy','rt','zscore','CHRCount',"res_freq_np","res_freq_wp","rad_freq_np","rad_freq_wp")]
#write.csv(lmm.df.clean.light, file = "./files/lmm_trials.csv", na = "")


### correlation 



