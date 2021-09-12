#' Linear regression of two continuous variables with addition of R2 and p-value
#'
#' @param data a data frame containing all the variables and the grouping information
#' @param x a variable in the x-axis
#' @param y a variable in the y-axis
#' @param group the grouping information
#' @param facet a boolean value to indicate facet
#' @param label.x the x position of label or can be "left" or "right" means the range of [0, 1]
#' @param label.y the y position og label or can be "bottom" or "top" means the range of [0, 1]
#' @param size the text size for ggtheme::theme_size function, default value is 14
#' @param show.se a boolean value to indicate whether shown the SE
#'
#' @return a ggplot2 plot
#' @export
#'
#' @author Zhonghui Gai
#' @examples
#' gglm(group = "Species", label.y = c(1,1,75,2.6)) + ggsci::scale_color_aaas()
#' gglm(group = "Species", label.y = 0.99) + ggsci::scale_color_aaas() + gglm:::theme_pub()
#' gglm() + ggpubr::theme_pubr()
#' gglm() + egg::theme_article()
#' gglm() + ggprism::theme_prism()
#' gglm(group = "Species", label.y = 0.99) + ggsci::scale_color_aaas() + ggprism::theme_prism()
#' gglm(group = "Species", label.y = 0.99) + ggprism::theme_prism(base_size = 14) +
#' ggprism::scale_color_prism(palette = "floral")
gglm <- function(data = iris, x = "Petal.Length", y = "Petal.Width",
                 group = NULL, facet = TRUE, label.x = "left", label.y = "top",
                 size = 14, show.se = TRUE){
  library(ggplot2)
  if (is.null(group)) {
    p <- ggplot(data = data, aes_string(x = x, y = y)) + geom_point(size = 1.5)
  } else if (is.character(group)) {
    p <- ggplot(data = data, aes_string(x = x, y = y, group = group, color = group)) +
      geom_point(size = 1.5)
  }
  if (is.null(group)) {
    facet <- FALSE
  }
  if (facet) {
    p <- p + facet_wrap(.~get(group), scales = "free") + theme(legend.position = 0)
  }
  p <- p + geom_smooth(method = "lm", formula = y ~ x, se = show.se, show.legend = FALSE) +
    ggpmisc::stat_poly_eq(aes(label = paste(..rr.label.., stat(p.value.label), sep = "~")),
                          label.x = label.x, label.y = label.y,
                          position = "identity", parse = TRUE, size = 3.7, fontface = "bold") +
    ggtheme::theme_size(size = size)
  return(p)
}

theme_pub <- function(legend.position = 0){
  library(ggplot2)
  theme_minimal() +
    theme(axis.title = element_text(size = 16, face="bold", colour = "black"),
          axis.text = element_text(size = 14, face="bold", colour = "black"),
          axis.ticks = element_line(colour = "black"),
          panel.background = element_rect(fill = "white", colour = "black"),
          panel.border = element_rect(fill = NA, colour = "black"),
          strip.background = element_rect(fill = "white", colour = "black"),
          strip.text.x = element_text(size = 14, face="bold", colour = "black"),
          legend.key = element_blank(),
          legend.title = element_blank(),
          legend.position = legend.position)
}

