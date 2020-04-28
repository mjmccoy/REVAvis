# Function chromPlot

chromPlot <- function(data, chr, feature, log_scale = NULL){
  if (is.null(data)){
    return(NULL)
  }
  data.df <- subset(data, Chr %in% chr) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1) %>% # removes the first instance of duplicated values (summary of each chr)
    select(BinStart = BinStart, y.data = feature, Chr = Chr, Condition = Condition) %>%
    as.data.frame
  g <- ggplot(data.df) +
    xlab("Genomic coordinates (Mb)") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    theme_classic() +
    geom_hline(yintercept = 0)
  if(log_scale){
    g <- g +
      geom_point(aes(x = BinStart/1000000, y = log10(y.data + 1), col = Chr, shape = Condition)) +
      ylab(paste("log10(", feature, " + 1)", sep = ""))
  } else {
    g <- g +
      geom_point(aes(x = BinStart/1000000, y = y.data, col = Chr, shape = Condition)) +
      ylab(feature)
  }
  # ggplotly(g)
  return(g)
}
