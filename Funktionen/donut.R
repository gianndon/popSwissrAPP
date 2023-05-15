donut <- function(df_donut){
  # Hole size
  hsize <- 7
  
  fraction <- df_donut$value /sum(df_donut$value)
  
  
  df_donut <- df_donut %>% 
    mutate(x = hsize,
           csum = rev(cumsum(rev(value))), 
           pos = value/2 + lead(csum, 1),
           pos = if_else(is.na(pos), value/2, pos))
  
  p<-   ggplot(df_donut, aes(x = hsize, y = value, fill = Anlage)) +
    geom_col(color = "black", size=1) +
    scale_fill_brewer(palette = "YlGnBu") + # change fill to gold color palette
    coord_polar(theta = "y") +
    
    geom_label_repel(aes(label = paste(percent(fraction)), 
    ),
    position = position_stack(vjust=0.5),
    inherit.aes = TRUE,
    show.legend=FALSE,
    box.padding = 0,
    size=8, 
    color="black") +
    guides(fill = guide_legend(title = "Anlage", title.position = "top"))+
    annotate("text", x = 0, y = 0, size = 13, color="navyblue", label = paste(abbreviate2(sum(df_donut$value)), "CHF"))+
    theme_void()
  
  p + theme(
    legend.title = element_text(size = 25),  # Set the text size to 14
    legend.text = element_text(size = 20)
  )
}