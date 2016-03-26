create_plot_1 <- function() {
      library(dplyr)
      information <- readRDS("summarySCC_PM25.rds")
      
      information <- group_by(information, year)
      values <- summarise(information, value = sum(Emissions))
      
      png("plot1.png")
      
      with(values, plot(year, value, 
                        ylab = "Total PM2.5 emission", col = "brown", pch = 19))
      with(values, lines(year, value))
      
      dev.off()
}