create_plot_3 <- function() {
      library(dplyr)
      library(ggplot2)
      information <- readRDS("summarySCC_PM25.rds")
      
      information <- filter(information, fips == "24510")
      information <- transform(information, type = factor(type))
      
      information <- group_by(information, year, type)
      values <- summarise(information, value = sum(Emissions))
      

      g <- ggplot(values, aes(year, value))
      g + geom_point(aes(color = type)) + geom_line(aes(color = type)) + 
            labs(y = "Total PM2.5 emissions")
      
      ggsave(file = "plot3.png")
}