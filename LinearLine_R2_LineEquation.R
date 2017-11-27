Linear plot with line equation and R2

Plant_FAMEs <- read.table("(Good)_Plant_FAMEs_Chainlength_Elevation_Lat_long.csv", header = TRUE, sep = ",")

plot1 <- ggplot(data = Plant_n_alkanes, aes(x = ACL, y = CPI))+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) +
  # geom_point(aes(fill = Genus),size = 3, shape = 21, color = "black") + ##Fill by Genus
  geom_point(color = "blue1", shape = 16, size = 2) +
  theme_classic() +
  xlab("ACL") + ylab("CPI") +
  ggtitle("Plant n-Alkanes ACL vs. CPI") +
  theme(plot.title = element_text(hjust = 0.5)) +
  stat_smooth(method = "lm", se=FALSE, color="black") + 
  geom_text(aes(x = 25, y = 40, label = lm_eqn1(lm(CPI ~ ACL, Plant_n_alkanes))), parse = TRUE)

m= lm(CPI ~ ACL, Plant_n_alkanes)

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