create_plot_3 <- function() {
      information <- transform(information, type = factor(type))
      information <- transform(information, baltimore = ifelse(fips == "24510", 
                               "Baltimore", "Total"))
      #informationBaltimor <- filter(information, fips == "24510")
      
      information <- group_by(information, year, type, baltimore)
      values <- summarise(information, value = sum(Emissions))
      
      g <- ggplot(values, aes(year, value))
      g + geom_point(aes(color = type)) + facet_grid(baltimore ~ ., scales = "free") +
            geom_line(aes(color = type)) + labs(y = "Total emissions")
            
      # qplot(year, value, data = values, color = type, 
       #     geom = c("point", "line"), facets = baltimore ~ .)
}