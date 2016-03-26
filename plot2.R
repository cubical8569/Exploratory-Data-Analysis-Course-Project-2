create_plot_2 <- function() {
      library(dplyr)
      information <- readRDS("summarySCC_PM25.rds")
      
      information <- filter(information, fips == "24510")
      
      information <- group_by(information, year)
      values <- summarise(information, value = sum(Emissions))
      
      png("plot2.png")
      
      with(values, plot(year, value, 
                        ylab = "Total PM2.5 emission for Baltimore", 
                               col = "brown", pch = 19))
      with(values, lines(year, value))
      
      dev.off()
}