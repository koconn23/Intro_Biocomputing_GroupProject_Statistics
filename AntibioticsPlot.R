#### Group Project Question 1: ####

library(ggplot2)

antibiotics <- read.csv("antibiotics.csv")
View(antibiotics)
      
plot1 <- ggplot(antibiotics, aes(x = trt, y = growth)) +
       geom_boxplot() + 
      theme_bw() + 
      ggtitle("Different Antibiotics on Growth")
                        
plot1