# Nice Pies
# by Dr Michal Marek Hoppe

## Load categorical data in LONG format
library(ggplot2)
data <- read.csv("data.csv")

## Pie chart function ------------------------------------------------------------------------------
plot_pie <- function (data,
                      title = "Graph title",
                      current_date = TRUE, # display current date
                      count = TRUE,
                      sort = "values",
                      cir_wid = 0.75, # pie size
                      text_size = 3,
                      palette = c("#344470", "grey50", "#CA6D88")) { # colour pallete
  if (sort == "values") {
    data <- sort(table(data), decreasing = TRUE)
  } else if (sort == "names"){
    data <- table(data)
  }
  data_prop <- data / sum(data)
  colfunc <- colorRampPalette(palette)
  cs <- colfunc(length(names(data)))
  cir_wid <- cir_wid
  text_size <- text_size
  p <- ggplot2::ggplot() + ggplot2::xlim(0, 1) + ggplot2::ylim(0, 1)
  x_start <- 0
  for (var in seq_along(names(data))){
    x_stop <- unname(x_start + data_prop[var])
    p[["layers"]][[length(p[["layers"]]) + 1]] <-
      geom_rect(aes(xmin = !!x_start, xmax = !!x_stop,
                    ymin = !!0, ymax = !!cir_wid),
                fill = cs[var])
    X_stop_txt <- x_start + unname(data_prop[names(data) == names(data)[var]]/2)
    if (count == TRUE) {
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_text(aes(x = !!X_stop_txt,
                      y = 1, lineheight = 0.9,
                      label = !!paste0(names(data)[var], "\n (", data[var], ")")),
                  size = text_size, colour = cs[var])
    } else if (count == FALSE) {
      p[["layers"]][[length(p[["layers"]])+1]] <-
        geom_text(aes(x = !!X_stop_txt,
                      y = 1, lineheight = 0.9,
                      label = !!paste0(names(data)[var])),
                  size = text_size, colour = cs[var])
    }
    x_start <- unname(x_start + data_prop[var])
    var <- var + 1
  }
  if (current_date == TRUE) {
    p <- p + 
      theme(plot.title = element_text(size = 8, hjust = 0.5,
                                      margin = margin(b = 2)),
            plot.subtitle = element_text(size = 6, hjust = 0.5,
                                         colour = "grey66",
                                         margin = margin(b = 1)),
            panel.background = element_rect(fill = "grey95"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            strip.background = element_rect(fill = "white"),
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.4, "lines"),
            legend.position = c(0, 1),
            legend.justification = c("left", "top"),
            legend.background = element_blank(),
            strip.text = element_text(size = 7, hjust = 0.5)) +
      labs(title = title,
           subtitle = paste0("as of ", format(Sys.time(), format = "%Y-%m-%d"))) +
      coord_polar(start = 0,
                  direction = 1)
  } else {
    p <- p + 
      theme(plot.title = element_text(size = 8, hjust = 0.5,
                                      margin = margin(b = 2)),
            panel.background = element_rect(fill = "grey95"),
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            axis.title = element_blank(),
            axis.text = element_blank(),
            axis.ticks = element_blank(),
            strip.background = element_rect(fill = "white"),
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.4, "lines"),
            legend.position = c(0, 1),
            legend.justification = c("left", "top"),
            legend.background = element_blank(),
            strip.text = element_text(size = 7, hjust = 0.5)) +
      labs(title = title) +
      coord_polar(start = 0,
                  direction = 1)
  }
  return(p)
}

## Print both PNG and PDF function -----------------------------------------------------------------
print_graphs <- function (p, width, height, res, name, folder = "") {
  folder <- ifelse(folder == "",
                   "",
                   paste0(folder, "/"))
  
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  }
  ggplot2::ggsave(paste0(folder, name, ".pdf"),
                  plot = p,
                  width = width,
                  height = height,
                  dpi = res,
                  units = "px",
                  bg = "white")
  png(filename = paste0(folder, name, ".png"),
      width = width,
      height = height,
      res = res,
      units = "px",
      bg = "white")
  options(warn = -1)
  print(p)
  invisible(dev.off())
  options(warn = 0)
  return(p)
}

## Create a Nice Pie -------------------------------------------------------------------------------
print_graphs(plot_pie(data,
                      title = "Graph title", # title
                      current_date = TRUE, # include current date
                      count = TRUE, # display sum counts
                      sort = "values", # sort by values, "names" to sort by category name
                      cir_wid = 0.75, # pie size
                      text_size = 3, # text sise
                      palette = c("#344470", "#CA6D88", "grey50")), # colour palette
             800, # width
             800, # height
             250, # dpi
             "data_pie") # file name

