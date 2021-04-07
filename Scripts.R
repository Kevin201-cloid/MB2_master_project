##############################################
## Fire intensity and occurrence analysis #####
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

# Section 2 ===================================================================

# Setting our framework

dir_data <- "E:/Migrawave_project/data/Nigeria_borno_active_fire/subset_borno_state/" # user dependent
setwd(dir = dir_data)
filename_1 <- paste0(dir_data, "subset_borno_csv")
tem_file_1 <- read.table("subset_borno_csv.csv", sep = ",", header = TRUE)
my_file_1 <- tem_file_1 %>%
  setNames(tolower(names(.)))%>%
  subset(type == 0 & confidence > 50) # FIRMS technical requirement is confidence > 50

# outlier removal
myfiles = my_file_1 %>%
  mutate(outlier = remove_outliers(my_file_1$frp),
         NA_Check = is.na(outlier))%>%
  subset(NA_Check == 'FALSE') %>%
  mutate_at(vars(longitude, latitude), funs(round(., 1))) %>%
  dplyr::select(acq_date,longitude, latitude, frp)

# This process could be repeated if user has a Near-Real Time fire detection dataset
fileName_2 <- paste0(DirAF, "fire_nrt_M6_181358") #This is for near real time data
temp2 = read.table("fire_nrt_M6_181358.csv", sep = ",", header = TRUE)
my_file_2 = temp2 %>%
  subset(confidence > 50)

my_file_2 = myfiles2 %>%
  mutate(outlier = remove_outliers(myfiles2$frp),
        #NA_Check = is.na(outlier)) %>%
  subset(NA_Check == 'FALSE') %>%
  mutate_at(vars(longitude, latitude), funs(round(., 1))) %>%
  dplyr::select(acq_date,longitude, latitude, frp))

# optional
rm(my_file_1,tem_file_1,filename_1)

# Section 3 ===========================================================================

# statistics analysis
myfiles$acq_date <- as.Date.character(myfiles$acq_date)

# Monthly aggregate

accum_mon <- myfiles %>%
  filter(acq_date <= "2003/01/01")%>%
  mutate(acq_month = as.Date(as.yearmon(acq_date, "%m/%Y"))) %>%
  select(acq_month) %>%
  group_by(acq_month) %>%
  summarise(occurrences = n()) %>%
  mutate(cum_sum = cumsum(occurrences))%>%
  ggplot(aes(acq_month, cum_sum))+
  geom_point()+
  geom_line()+
  ggtitle("monthly occurence")+
  theme_graph()+
  xlab("acq_date")
ylab("frp")


