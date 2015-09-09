library(reshape2)
library(plyr)
library(ggplot2)
train_data <- read.csv("./2015-training-initiatives-background.csv")
topics <- melt(train_data[15:23])
colnames(topics) <- c("topic", "pcnt")
head(topics)
topicsummary <- ddply(topics, c("topic"), summarise,
               N    = sum(!is.na(pcnt)),
               mean = mean(pcnt, na.rm=TRUE),
               sd   = sd(pcnt, na.rm=TRUE),
               se   = sd / sqrt(N)
)
p <- ggplot(data=topicsummary, aes(x=topic, y=mean)) + 
    geom_bar(stat="identity") +
    geom_errorbar(aes(ymin=mean-se, ymax=mean+se), width=.2, position=position_dodge(.9)) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
p
