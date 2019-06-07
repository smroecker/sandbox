library(gtrendsR)
library(ggplot2)

us <- gtrends(c("Web Soil Survey", "Soil Web", "Soil Survey", "Soil Map"), time = "all")

tm <- us$interest_over_time

ggplot(tm, aes(x = date, y = hits, fill = keyword)) +
  geom_area() +
  ggtitle("Google Trends for US")
  

