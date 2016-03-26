create_plot_5 <- function() {
      library(dplyr)
      library(ggplot2)
      information <- readRDS("summarySCC_PM25.rds")
      sources <- readRDS("Source_Classification_Code.rds")
      
      information <- filter(information, fips == "24510")
      sources <- sources[, 1:4]
      
      information <- merge(information, sources, by.x = "SCC", by.y = "SCC")
      information <- transform(information, 
                               EI.Sector = as.character(EI.Sector))
      
      information <- filter(information, 
                            grepl("vehicle", EI.Sector, ignore.case = TRUE) == TRUE)
      information <- group_by(information, year)
      values <- summarise(information, value = sum(Emissions))
      
      g <- ggplot(values, aes(year, value))
      g + geom_point() + geom_line(aes(color = "red")) + 
            labs(y = "Total PM2.5 emissions from vehicles")
      
      ggsave(file = "plot5.png")
}