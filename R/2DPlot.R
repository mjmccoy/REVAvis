# Function TwoDPlot

TwoDPlot <- function(
  data1,
  data2,
  condition1_name,
  condition2_name,
  chr,
  feature,
  norm_feature = "",
  ymin = NULL,
  ymax = NULL,
  point_size,
  text_size,
  FeatureFile = TRUE,
  genes = c("TUBB8", "DIP2C")){

  # correlation function
  corr_eqn <- function(x,y, digits = 2) {
    corr_coef <- round(cor(x, y), digits = digits)
    # paste("italic(r) == ", corr_coef)
    paste("r = ", corr_coef, sep = "")
  }

  if(is.null(data1) || is.null(data2) || is.null(chr)){
    return(NULL)
  }

  # Subset data to include only the chromosomes specfied by input
  data1 <- data1 %>%
    subset(Chr %in% chr)
  data2 <- data2 %>%
    subset(Chr %in% chr)

  # If REVA input is not Feature File format, remove summary of chromosomes
  if(!FeatureFile){
    data1 <- data1 %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1)
    data2 <- data2 %>%
      group_by(Condition, Chr) %>%
      filter(duplicated(Chr) | n()==1)
  }

  # Match data2 feature with data1 feature by Chr and Binstart if not Feature File, and
  # also by gene if Feature File
  data1 <- as.data.frame(data1) %>% rename(data1_feature = feature)
  if(!FeatureFile){
    data2 <- data2 %>%
      select(data2_feature = feature, BinStart = BinStart, Chr = Chr) %>%
      as.data.frame
    data1$data2_feature <- data2$data2_feature[
      match(paste(data1$Chr, data1$BinStart), paste(data2$Chr, data2$BinStart))
    ]
  } else {
    data2 <- data2 %>%
      select(data2_feature = feature, BinStart = BinStart, Chr = Chr, gene = gene) %>%
      as.data.frame
    data1$data2_feature <- data2$data2_feature[
      match(paste(data1$Chr, data1$BinStart, data1$gene), paste(data2$Chr, data2$BinStart, data2$gene))
    ]
  }

  g <- ggplot(
    data = subset(data1, data1_feature > 0 | data2_feature > 0),
    aes(
      x = log10(data1_feature),
      y = log10(data2_feature)
    )) +
    geom_point(aes(col = Chr), size = point_size) +
    # geom_point(aes(x = log10(data1_feature), y = log10(data2_feature), col = Chr, text = BinStart)) +
    xlab(paste("log10(", condition1_name, ")", sep = "")) +
    ylab(paste("log10(", condition2_name, ")", sep = "")) +
    xlim(c(0, log10(max(data1[names(data1) %in% c("data1_feature", "data2_feature")])))) +
    ylim(c(0, log10(max(data1[names(data1) %in% c("data1_feature", "data2_feature")])))) +
    geom_abline(linetype = "dashed") +
    theme(
      panel.border = element_rect(colour = "black", fill = NA, size = 1),
      text = element_text(size = text_size)
    ) +
    theme_classic() +
    geom_text(x = log10(max(data1[names(data1) %in% c("data1_feature", "data2_feature")]))/10,
              y = log10(max(data1[names(data1) %in% c("data1_feature", "data2_feature")])),
              label = corr_eqn(data1$data1_feature, data1$data2_feature),
              parse = FALSE)
  if(FeatureFile){
    g <- g +
      geom_text_repel(
        data = subset(g$data, gene %in% genes),
        aes(
          x = log10(data1_feature),
          y = log10(data2_feature),
          label = gene))
  }

  if(!is.null(ymax) | !is.null(ymin)){
    g <- g + ylim(c(ymin, ymax))
  }

  # ggplotly(g, tooltip = "text")
  return(g)
}
