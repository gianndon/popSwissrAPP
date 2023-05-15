my_boxplot <- function(df){
  p <- ggplot(data = df, aes(x = Anlage, y = value, fill = Anlage)) +
    geom_bar(stat = "identity") +
    scale_fill_brewer(palette = "YlGnBu") +
    geom_text(aes(label = df$value), vjust = -0.5) +
    theme(panel.background = element_rect(fill = "transparent"), # set the background to transparent
          panel.grid.major = element_blank(), # remove the major grid lines
          panel.grid.minor = element_blank(), # remove the minor grid lines
          plot.background = element_rect(fill = NA, color = NA), # set the plot background to white
          axis.line = element_line(color = "black"), # set the axis lines to black
          axis.text = element_text(color = "black"), # set the axis text to black
          axis.title = element_text(color = "black"), # set the legend background to transparent
          legend.background = element_rect(fill = "transparent"),
          axis.line.x = element_line(color = "black")  # set the x-axis line color to black
    ) # set the axis title to black
  
  p + theme(legend.title = element_text(size = 17)
            ,legend.text = element_text(size = 14))+
    geom_col(color = "black", size=0.5) # Set the text size to 14)
}