# Function multi_chromPlot

multi_chromPlot <- function(data, data2 = NULL, chr, feature = vals$feature, condition1_name = NULL, condition2_name = NULL){
  if(is.null(data)){
    return(NULL)
  }
  data.df <- subset(data, Chr %in% chr) %>%
    select(c("Condition", "Chr", "BinStart", feature)) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
  if(!is.null(data2)){
    data2.df <- subset(data2, Chr %in% chr) %>%
      select(c("Condition", "Chr", "BinStart", feature)) %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1) # removes the first instance of duplicated values (summary of each chr)
    data.df[,4] <- data.df[,4]/data2.df[,4] # calculate ratio condition1/condition2
  }
  data.df$Chr <- factor(data.df$Chr, levels = chr) # force chr order by selection
  data.df <- data.df[with(data.df, order(Chr, BinStart)), ]
  data.df <- data.df %>% group_by(Condition) %>% mutate(Position = 1:length(BinStart))
  g <- data.df %>% ggplot(aes_string(x = "Position", y = feature, col = "Chr", shape = "Condition")) +
    geom_point() +
    ylab(feature) +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    geom_hline(yintercept = 0)
  if(!is.null(data2)){
    g <- g + ylab(paste(feature, " (", condition1_name, "/", condition2_name, ")", sep = ""))
  }
  return(ggplotly(g))
}
