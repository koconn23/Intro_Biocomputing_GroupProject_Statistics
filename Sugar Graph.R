#### Linear plot with line equation and R2

sugardata <- read.csv(file = "sugar.csv", header=TRUE)

plot1 <- ggplot(data = sugardata, aes(x = sugar, y = growth))+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  geom_point(color = "blue1", shape = 16, size = 2) +
  theme_classic() +
  xlab("sugar") + ylab("growth") +
  ggtitle("Sugar vs. Growth") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(method = "lm", se=FALSE, color="black") + 
  geom_text(aes(x = 5, y = 25, label = lm_eqn1(lm(sugar ~ growth, sugardata))), parse = TRUE)

m= lm(sugar ~ growth, sugardata)

lm_eqn1 = function(m) {
  
  l <- list(a = format(coef(m)[1], digits = 2),
            b = format(abs(coef(m)[2]), digits = 2),
            r2 = format(summary(m)$r.squared, digits = 3));
  
  if (coef(m)[2] >= 0)  {
    eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2,l)
  } else {
    eq <- substitute(italic(y) == a - b %.% italic(x)*","~~italic(r)^2~"="~r2,l)    
  }
  
  as.character(as.expression(eq));                 
}

plot1