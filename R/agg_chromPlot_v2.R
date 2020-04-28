# Function agg_chromPlot

agg_chromPlot <- function(data1, data2, chr, feature, mean_se = FALSE, log_scale = NULL){
  if(is.null(data1) || is.null(data2)){
    return(NULL)
  }
  data.df <- rbind(data1, data2) %>%
    subset(Chr %in% chr) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1) %>%
    mutate(group = gsub("\\_.*", "", Condition)) %>%
    select(BinStart = BinStart, y.data = feature, Chr = Chr, Condition = Condition, group = group) %>%
    as.data.frame
  if(log_scale){
    g <- ggplot(data.df, aes(x = BinStart/1000000, y = log10(y.data + 1), col = group)) +
      ylab(paste("log10(", feature, " + 1)", sep = ""))
      } else {
    g <- ggplot(data.df, aes(x = BinStart/1000000, y = y.data, col = group)) +
      ylab(feature)
      }
  g <- g +
    xlab("Genomics coordinates (Mb)") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    geom_hline(yintercept = 0) +
    theme_classic()
  if(mean_se){
    g <- g + stat_summary()
  } else {
    g <- g + geom_point()
  }
  # ggplotly(g)
  return(g)
}
