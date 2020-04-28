# Function chromPlot

chromPlot <- function(data, chr, feature = vals$feature){
  if (is.null(data)){
    return(NULL)
  }
  g <- subset(data, Chr %in% chr) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1) %>% # removes the first instance of duplicated values (summary of each chr)
    ggplot(aes_string(x = "BinStart", y = feature, col = "Condition")) +
    geom_point() +
    ylab(feature) +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    geom_hline(yintercept = 0)
  ggplotly(g)
}
