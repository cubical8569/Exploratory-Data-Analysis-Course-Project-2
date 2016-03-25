create_plot_1 <- function() {
      information <- group_by(information, year)
      values <- summarise(information, value = sum(Emissions))
      
      with(values, plot(year, value, 
                        ylab = "Total PM2.5 emission", col = "brown", pch = 19))
      with(values, lines(year, value))
}