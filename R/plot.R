#' @import ggplot2
#' @export
plot_alignment <- function(x) {
  dims <- dimnames(x$grids$grid) |>
    stats::setNames(c("row", "col"))

  v <- dims |>
    lapply(seq_along) |>
    expand.grid()

  data_path <- data.frame(
    row = x$is,
    col = x$js,
    label = paste0(
      x$grids$grid_edits[matrix(c(x$is, x$js), ncol = 2)],
      "\n",
      c("", x$a_alignment),
      "/",
      c("", x$b_alignment),
      "\n",
      c(0, x$scores)
    )
  )

  p <- ggplot(v) +
    aes(x = col, y = row) +
    geom_path(
      data = data_path,
    ) +
    geom_label(
      aes(label = label),
      data = data_path, size = 3,
      hjust = .5,
      vjust = .5
    ) +
    scale_x_continuous(
      x$a$label,
      breaks = seq_along(dims$col),
      labels = dims$col,
      expand = expansion(add = .5)
    ) +
    scale_y_reverse(
      x$b$label,
      breaks = seq_along(dims$row),
      labels = dims$row,
      expand = expansion(add = .5)
    ) +
    theme(panel.grid.major = element_blank())
  p
}

