# get the original 40 items and categorize them into two groups based on stroke numbers   
stroke_items  <- read.csv('/Users/qiaopingguo/lexicalproject_scratch/files/strokeeffect_items40.csv')
features <- read.csv('./files/feature.csv')

stroke_items$item <- stroke_items$stimulusLabel
s <- merge(stroke_items[,c('item','strokeType')],features[,c('item','num_strokes')],on='item',how='inner')
dim(s)
s <- s[order(s$num_strokes, decreasing = TRUE), ]
s$TypeOriginal <- ifelse(seq(nrow(s)) <= 20, 1, 0)

rm(s)
write.csv(s,'strokeeffect_item40.csv')
