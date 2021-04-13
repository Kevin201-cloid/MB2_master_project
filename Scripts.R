###################################################
## Fire intensity and occurrence distribution #####
######### Author Kevin Yomi #######################
######### University of Wuerzburg #################

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
tem_file_1 <- read.table("subset_borno_csv.csv", sep = ",", header = TRUE, stringsAsFactors = FALSE)
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

# skip this section if no Near-Real Time fire detection dataset
fileName_2 <- paste0(DirAF, "fire_nrt_M6_181358") # This is for near real time data
temp2 = read.table("fire_nrt_M6_181358.csv", sep = ",", header = TRUE)
my_file_2 = temp2 %>%
  subset(confidence > 50)

my_file_2 = myfiles2 %>%
  mutate(outlier = remove_outliers(myfiles2$frp),
        NA_Check = is.na(outlier)) %>%
  subset(NA_Check == 'FALSE') %>%
  mutate_at(vars(longitude, latitude), funs(round(., 1))) %>%
  dplyr::select(acq_date,longitude, latitude, frp)

# optional
rm(my_file_1,tem_file_1,filename_1)

# Section 3 ===========================================================================

# statistics analysis
myfiles$acq_date <- as.Date(myfiles$acq_date)

# Monthly aggregate
myfiles %>%
  filter(acq_date >= "2003/01/01") # recommended by the Metatadata for reliability

accum_mon <- myfiles %>%
  mutate(acq_month = as.Date(as.yearmon(acq_date, "%m/%Y")))%>%
  select(acq_month)%>%
  group_by(acq_month)%>%
  summarise(occurences = n())

# yearly aggregation

accum_year <- myfiles %>%
  mutate(acq_year = as.character(year(acq_date))) %>%
  select(acq_year, acq_date) %>%
  group_by(acq_year)%>%
  summarise(occurences = n()) # yearly sum accumulation

# season aggregation

fire_season <- accum_mon %>%
  mutate(month_fire = month(acq_month),
                       season = case_when(
                        month_fire %in% c(1,2,3,4,5) ~ "Dry Season",
                        month_fire %in% c(6,7,8,9) ~ "Rainy Season",
                        month_fire %in% c(10,11,12 ) ~ "Small Dry Season")
                    )%>%
                      group_by(season, acq_month) %>%
                      summarise(n_fires = sum(occurences), date_fire = mean(month_fire))%>%
           arrange(date_fire)

# plotting

p1 <- ggplot(fire_season, aes(x = acq_month, y = n_fires, color = factor(season)))+
  geom_col(stat = "count") +
  geom_line()+
  ggtitle("Number of fire / Season ") +
  theme(legend.title = element_text(colour = "chocolate", size = 16, face = "bold")) +
  scale_color_discrete(name = "Fire Season")+
  xlab("Date") +
  ylab("fire_occurence(thousand)")

p1

# overall trend

fire_season$fire_trend <- rollmean(fire_season$n_fires, k = 10, fill = "NA")
is.na(fire_season$fire_trend)


# plotting overall trend

  ggplot(fire_season, aes(n_fires))+
  geom_count()+
  geom_line(aes(y= fire_trend), color = "red")+
  ggtitle("fire by season")+
  xlab("Date")+
  ylab("wildfire")+
  theme_graph()

# seasonality
fire_season <- fire_season %>%
  mutate(fire_detrend = n_fires / fire_trend)

# calculate average seasonality

fire_season <- fire_season %>%
  group_by(season) %>%
  mutate(fire_seasonality=mean(fire_detrend, na.rm = T))

# plot overall trend added

ggplot(fire_season, aes(date_fire, n_fires)) +
  geom_point()+
  geom_line(aes(y =fire_trend), color ="red")

####### plotting #######

ggplot(fire_season, aes(acq_month, n_fires/ 100))+
  geom_line(color = "steelblue", size  = 1)+
  geom_point(color = "steelblue")+
  labs(title = "Fire Occurrrence per season",
  y = "Number of fire", x = "")+
  facet_wrap(~season)

# monthly aggregation

gg_Month <- accum_mon %>%
  ggplot(aes(x = acq_month, y = occurences )) +
  geom_area(fill="#69b3a2", alpha = 0.5) +
  geom_line(color="#69b3a2") +
  scale_x_date(date_breaks = "24 months", date_labels = "%b/%y") + # it breaks the x axis into month/year
  labs(y ="Cumulative quantity of fires (thousands)",
       x = "Month/Year", caption="Innovation_Lab") +
  theme_graph ()

gg_Month

# yearly aggregation

gg_year <- accum_year %>%
  ggplot(aes(x = acq_year, y = occurences)) +
  ylim(150, 440)+
  geom_area( fill = "#69b3a2", alpha=0.5, stat = "identity", position = "stack",
             inherit.aes = TRUE, na.rm = TRUE) +
  geom_line(color = "#69b3a2")+
  stat_smooth(method = NULL)+
  labs(y = "fire_count",
       x = "Date", caption = "Inno-Lab")+
    theme_graph()


gg_year

# section 4 ====================================================================

# Spatial distribution of active fire distribution

spFire <- myfiles %>%
  group_by(longitude, latitude)%>%
  summarise(sumFRP = sum(frp))

# loading our shapefile

Borno_states <- readOGR("E:/Migrawave_project/data/shapefiles/borno_admin1_boundaries_states/borno_state",
                        layer = "borno_state")
LGA <- readOGR("E:/Migrawave_project/data/shapefiles/borno_admin1_boundaries_states/borno_lga",
               layer = "Borno_Admin_Boundaries_OnlyBorno_eHA_IOM_LGAs")

# converting shapefile spdf to tbl_df

borno_tidy <- broom::tidy(Borno_states, region = "StateName")
lga_tidy <- broom::tidy(LGA ,region ="LGAName")

NbBrks = 8 # defines the outlook parameters

AF_brk <- getJenksBreaks(spFire$sumRFP, NbBrks, subset = NULL) # number of numeric values and desired breaks
AF_brk
AF_brk_N <-c(6.8,  1241.6,  3134.5,  5598.5,  9297.6, 15883.8, 25424.6, 49990.2)

# defining the labels parameters

labelsJenks <- digits(AF_brk/1000, 0) # numeric vector displaying pre-specific digits
labelsJenks # below the values rounded paste them starting with the value above 0

labels_AF <- c("NA","1","3","6", "9","16","25","50")

##### plot #########

ggfrp <- ggplot()+
  geom_polygon(data = borno_tidy, aes(x = long, y = lat, group = group),
               fill="#EBEBE5", color = "#91a3b0", size = 0.4)+
  geom_tile(data = spFire, aes(longitude, latitude, fill = sumFRP))+
  geom_polygon(data = lga_tidy, aes(x= long, y = lat, group = group),
               fill = "NA", color = "#1d1d1d", size = 0.8)+
  coord_cartesian(xlim = c(11,15), ylim = c(10,14), expand = F)+
  theme_map()+
  scale_fill_fermenter(palette = "YlOrBr",
                      direction = 1,
                      breaks = AF_brk_N,
                      labels = labels_AF,
                      name = "FRP\n(GW)",
                      guide = guide_legend(title.hjust = 0.5,
                                           title.vjust = 1,
                                           reverse = T,
                                           keyheight = unit(3, units = "mm"))
                      )+
  labs(caption = "Inno-Lab")

ggfrp

dev.off()

