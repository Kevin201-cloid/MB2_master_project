##############################################
## Fire intensity and occurence analysis #####
######### Author Kevin Yomi ##################
######### University of Wuerzburg ############

# Section 1 ----------------------------------

# load necessary packages

my_packages <- c("strucchange","raster","ggplot2","BAMMtools",
                 "dplyr","ggrepel","broom","rgdal","lubridate","ggthemes",
                 "ggthemes", "formattable", "tidyverse", "magrittr",
                 "forecast","maptools","rgeos", "weathermetrics", "usethis")
pacman::p_load(my_packages, character.only = TRUE)
my_packages %in% loadedNamespaces() # control the package loading

# outlier handling function

remove_outliers <- function(x, na.rm = TRUE, ...) {
  qnt <- quantile(x, probs=c(0.05, 0.95), na.rm = na.rm, ...)
  H <- 1.5 * IQR(x, na.rm = na.rm)
  y <- x
  y[x < (qnt[1] - H)] <- NA
  y[x > (qnt[2] + H)] <- NA
  y
}
# graph theme parameters function
theme_graph <- function(...) {
  theme(plot.title=element_text(size=11,
                                face="bold",
                                family="Segoe UI Semibold",
                                hjust=0.5,
                                lineheight=1.2,
                                color = "#22211d"),  # title
        plot.subtitle=element_text(size=9,
                                   family="Segoe UI Semibold",
                                   face="bold",
                                   hjust=0.5,
                                   color = "#22211d"),  # subtitle
        axis.title.x=element_text(size=9),  # X axis title
        axis.title.y=element_text(size=9),  # Y axis title
        axis.text.x=element_text(size=6,
                                 vjust=.5),  # X axis text
        axis.text.y=element_text(size=6),
        # legend.title = element_blank(),
        legend.text = element_text(size = 7),
        legend.justification=c(0, 1),
        legend.key.width= unit(0.9, "lines"),
        legend.key.height = unit(0.9, "lines"),
        legend.position = c(0.9, 0.6),
        plot.caption = element_text(size = 6),
        panel.grid.major = element_line(color = "#ebebe5", size = 0.2),
        panel.grid.minor = element_blank(),
        plot.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.background = element_rect(fill = "#f5f5f2", color = NA),
        legend.background = element_rect(fill = "#f5f5f2", color = NA),
        panel.border = element_blank()
  )
}
