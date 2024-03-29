create_plot_4 <- function() {
      library(dplyr)
      library(ggplot2)
      information <- readRDS("summarySCC_PM25.rds")
      sources <- readRDS("Source_Classification_Code.rds")
      
      sources <- sources[, 1:4]
      
      sources <- transform(sources, 
                               EI.Sector = as.character(EI.Sector))
      
      goods <- filter(sources, grepl("coal", EI.Sector, ignore.case = TRUE) == TRUE)
      
      information <- filter(information, 
                            SCC %in% goods$SCC)
      
      information <- merge(information, sources, by.x = "SCC", by.y = "SCC")
      
      information <- group_by(information, year)
      values <- summarise(information, value = sum(Emissions))
      
      g <- ggplot(values, aes(year, value))
      g + geom_point() + geom_line(aes(color = "red")) + 
            labs(y = "Total PM2.5 emissions from coal")
      
      ggsave(file = "plot4.png")
}