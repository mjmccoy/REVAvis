# Function agg_chromPlot

agg_chromPlot <- function(data1, data2, chr, feature = vals$feature, mean_se = FALSE){
  if(is.null(data1) || is.null(data2)){
    return(NULL)
  }
  if(mean_se){
    g <- rbind(data1, data2) %>%
      subset(Chr %in% chr) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) %>%
      mutate(group = gsub("\\_.*", "", Condition)) %>%
      ggplot(aes_string(x = "BinStart", y = feature, col = "group")) +
      stat_summary() +
      ylab(feature) +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      geom_hline(yintercept = 0)
  } else {
    g <- rbind(data1, data2) %>%
      subset(Chr %in% chr) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) %>%
      mutate(group = gsub("\\_.*", "", Condition)) %>%
      ggplot(aes_string(x = "BinStart", y = feature, col = "group")) +
      geom_point() +
      ylab(feature) +
      theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
      geom_hline(yintercept = 0)
  }
  ggplotly(g)
}
