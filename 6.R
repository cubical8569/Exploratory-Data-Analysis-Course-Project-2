create_plot_6 <- function() {
      information <- filter(information, fips == "24510" | fips == "06037")
      sources <- sources[, 1:4]
      
      information <- merge(information, sources, by.x = "SCC", by.y = "SCC")
      
      information <- transform(information, 
                               city = ifelse(fips == "24510", 
                                             "Baltimore City", "Los Angeles County"))
      information <- transform(information, 
                               EI.Sector = as.character(EI.Sector))
      
      information <- filter(information, 
                            grepl("vehicle", EI.Sector, ignore.case = TRUE) == TRUE)
      information <- group_by(information, year, city)
      values <- summarise(information, value = sum(Emissions))
      
      g <- ggplot(values, aes(year, value))
      g + geom_point() + geom_line(aes(color = city)) + 
            labs(y = "Total PM2.5 emissions from vehicles")
}