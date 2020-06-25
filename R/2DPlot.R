# Function TwoDPlot

TwoDPlot <- function(data1, data2, condition1_name, condition2_name, chr, feature, norm_feature = ""){
  # for (i in c(data1, data2, chr, feature)){
  #   if(is.null(i)){
  #     return(NULL)
  #   }
  # }

  # correlation function
  corr_eqn <- function(x,y, digits = 2) {
    corr_coef <- round(cor(x, y), digits = digits)
    # paste("italic(r) == ", corr_coef)
    paste("r = ", corr_coef, sep = "")
  }

  if(is.null(data1) || is.null(data2) || is.null(chr)){
    return(NULL)
  }

  # if(is.null(data1) || is.null(data2)){
  #   return(NULL)
  # }
  data1 <- data1 %>%
    subset(Chr %in% chr) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1)
  data1 <- data1 %>%
    select(data1_feature = feature, BinStart = BinStart) %>%
    as.data.frame
  data2 <- data2 %>%
    subset(Chr %in% chr) %>%
    group_by(Condition, Chr) %>%
    filter(duplicated(Chr) | n()==1)
  data2 <- data2 %>%
    select(data2_feature = feature) %>%
    as.data.frame
  data1$data2_feature <- data2$data2_feature
  g <- ggplot(subset(data1, data1_feature > 0 | data2_feature > 0)) +
    # geom_point(aes(x = data1_feature, y = data2_feature, col = Chr, text = BinStart)) +
    geom_point(aes(x = log2(data1_feature), y = log2(data2_feature), col = Chr, text = paste(Chr, BinStart, sep = ":"))) +
    # geom_point(aes(x = log10(data1_feature), y = log10(data2_feature), col = Chr, text = BinStart)) +
    xlab(paste("log2(", condition1_name, ")", sep = "")) +
    ylab(paste("log2(", condition2_name, ")", sep = "")) +
    xlim(c(0, log2(max(data1[names(data1) %in% c("data1_feature", "data2_feature")])))) +
    ylim(c(0, log2(max(data1[names(data1) %in% c("data1_feature", "data2_feature")])))) +
    # xlim(c(0, log10(max(data1[names(data1) %in% c("data1_feature", "data2_feature")])))) +
    # ylim(c(0, log10(max(data1[names(data1) %in% c("data1_feature", "data2_feature")])))) +
    # scale_x_log10() +
    # scale_y_log10() +
    # coord_fixed() +
    geom_abline(linetype = "dashed") +
    theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) +
    theme_classic() +
    geom_text(x = log2(max(data1[names(data1) %in% c("data1_feature", "data2_feature")]))/10,
              y = log2(max(data1[names(data1) %in% c("data1_feature", "data2_feature")])),
              label = corr_eqn(data1$data1_feature, data1$data2_feature),
              parse = FALSE)
    # geom_text(x = log10(max(data1$data1_feature))/10,
    #           y = log10(max(data1$data2_feature)),
    #           label = corr_eqn(data1$data1_feature, data1$data2_feature),
    #           parse = TRUE)
  # ggplotly(g, tooltip = "text")
  return(g)
}
