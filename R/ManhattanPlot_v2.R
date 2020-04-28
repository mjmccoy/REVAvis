# Function ManhattanPlot

ManhattanPlot <- function(
  data,
  data2 = NULL,
  chr,
  feature,
  log_scale = FALSE,
  condition1_name = NULL,
  condition2_name = NULL){
  if(is.null(data)){
    return(NULL)
  }
  req(chr)
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
  # data.df <- data.df %>% group_by(Condition) %>% mutate(Feature = 1:length(BinStart))
  data.df <- data.df %>%
    group_by(Condition) %>%
    mutate(Feature = 1:length(BinStart)) %>%
    select(Feature = Feature, y.data = feature, Chr = Chr, Condition = Condition) %>%
    as.data.frame
  # g <- data.df %>% ggplot(aes_string(x = "Feature", y = feature, col = "Chr", shape = "Condition")) +
  g <- data.df %>%
    ggplot() +
    xlab("") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    geom_hline(yintercept = 0) +
    theme_classic() +
    theme(axis.ticks = element_blank(), axis.text.x = element_blank())
  if(log_scale){
    g <- g +
      geom_point(aes(x = Feature, y = log10(y.data + 1), col = Chr, shape = Condition)) +
      ylab(paste("log10(", feature, " + 1)", sep = ""))
  } else {
    g <- g +
      geom_point(aes(x = Feature, y = y.data, col = Chr, shape = Condition)) +
      ylab(feature)
  }
  if(!is.null(data2)){
    g <- g + ylab(paste(feature, " (", condition1_name, "/", condition2_name, ")", sep = ""))
  }
  return(g)
}
