require(ggplot2)

#' Clean Light Theme for ggplot
#'
#' Provides a clean, light theme, aesthetic for ggplot
#' @param base_size Size of text. Defaults to 11
#' @param labs Include y and x axis labels? Defaults to FALSE
#' @export
#' @examples
#' df <- data.frame(
#'   a = seq(1, 10),
#'   b = seq(1, 10)
#' )
#'
#' ggplot(df, aes(x=a, y=b)) +
#'   geom_bar(stat='identity') +
#'   labs(title="Plot Title",
#'        subtitle='Plot Subtitle',
#'        y="y Axis Label",
#'        x="x Axis Label",
#'        caption="Plot caption") +
#'   db_light_theme(base_size = 11, base_family = 'Open Sans') +
#'   scale_fill_brewer(palette = 'Set1')

db_light_theme <- function(base_size = 12,
                           base_family = "Open Sans",
                           base_line_size = base_size/22,
                           base_rect_size = base_size/22){
  half_line <- base_size/2
  theme_bw(base_size = base_size, base_family = base_family,
           base_line_size = base_line_size, base_rect_size = base_rect_size) +
    theme(text = element_text(family = base_family),
          plot.title = element_text(family = base_family, size=rel(1.4)),
          plot.subtitle =  element_text(family = base_family, size=rel(0.9)),
          panel.grid = element_blank(),
          panel.border = element_blank(),
          plot.margin = margin(t=12, r=6, b=12, l=6),
          axis.title = element_text(family = base_family, size=rel(0.9)),
          axis.ticks = element_blank(),
          axis.text = element_text(size = rel(0.9), color = "gray10"),
          legend.position = "none",
          axis.line.y.left = element_line(colour = "black"),
          axis.line.x.bottom = element_line(colour = "black"),
          plot.caption = element_text(size = rel(0.8), color = '#656565', vjust = -1.2, hjust = 0),
          strip.background = element_rect(fill = "grey85", colour = "grey20"),
          strip.text = element_text(colour = "grey10", size = rel(0.8), margin = margin(0.8 * half_line, 0.8 * half_line, 0.8 * half_line, 0.8 * half_line), face='bold'),
          strip.switch.pad.grid = unit(0.1, "cm"),
          strip.switch.pad.wrap = unit(0.1, "cm"))
}

db_dark_theme <- function(base_size = 12,
                          base_family = "Open Sans",
                          base_line_size = base_size/22,
                          base_rect_size = base_size/22){
  db_light_theme(base_size, base_family, base_line_size, base_rect_size) %+replace%
    theme(
      text = element_text(family = base_family, face = "plain",
                          colour = "grey90", size = base_size,
                          lineheight = 0.9,  hjust = 0.5,
                          vjust = 0.5, angle = 0,
                          margin = margin(), debug = FALSE),
      axis.title = element_text(color='grey90', size = rel(1)),
      axis.text = element_text(color='grey80', size = rel(0.9)),
      axis.line.x.bottom = element_line(color='grey60'),
      axis.line.y.left = element_line(color='grey60'),
      panel.background = element_rect(fill='grey20', color='grey20', size=2),
      plot.background = element_rect(fill = "grey20", color='grey20'),

      plot.caption = element_text(size = rel(0.8), color = 'grey70', vjust = -1.2, hjust = 0),

      legend.background = element_rect(fill='grey20', color='grey20'),
      legend.key = element_rect(fill='grey20', color='grey20')

    )
}
