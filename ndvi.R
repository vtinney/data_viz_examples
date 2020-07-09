library(raster) # reading in and wrangling landsat data
library(sf) # reading in and wrangling contextual data
library(tidyverse)
# devtools::install_github("clauswilke/ggtext")
library(ggtext) # text in plots
library(showtext) # more fonts
font_add_google("Lato", regular.wt = 300, bold.wt = 700)
library(rcartocolor)

setwd('C:/Users/vat05/Google Drive/MN_NDVI/')

ndvi <- raster("was_ndvi.tif")

water <- st_read("https://opendata.arcgis.com/datasets/db65ff0038ed4270acb1435d931201cf_24.geojson") %>%
  st_transform(st_crs(ndvi))

ndvi_spdf <- as(ndvi, "SpatialPointsDataFrame") # create spatialpoints dataframe
ndvi_df <- as_tibble((ndvi_spdf)) # convert that to a plain tibble
colnames(ndvi_df) <- c("value", "x", "y")

water_sp <- as(water, "Spatial")


brks <- c(-0.3,0.1,0.15,0.2,0.4,1)
labels <- c('-0.31 - 0.1','0.11-0.15','0.16-0.2','0.21-0.4','0.41-1')
ndvi_df$brks <- cut(ndvi_df$value, 
                    breaks = brks, 
                    include.lowest = TRUE, 
                    labels = labels)

brks_scale <- levels(ndvi_df$brks)
labels_scale <- brks_scale

ggplot() +
  geom_raster(data = ndvi_df, aes(x = x, y = y,  fill = brks), interpolate = TRUE) +
  geom_polygon(data = water_sp, aes(x = long, y = lat, group = group), color = "gray90", fill = "white") +
  theme_minimal() +
  coord_equal() +
  scale_fill_manual(values=c("#d98994","#e5b9ad","#f1eac8","#b1c7b3","#72aaa1","#009392"), 
                    name = "Median NDVI 2018",
                    breaks=brks_scale,
                    labels = labels_scale,
                    drop=FALSE,
                    na.value = '#A16928',
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(2, units = "mm"),
                      keywidth = unit(140 / length(labels), units = "mm"),
                      title.position = 'top',
                      title.hjust = 0.5,
                      nrow = 1,
                      byrow = T,
                      label.position = "bottom"
                    ))+
  theme(legend.position = "bottom",
        axis.text = element_blank(),
        plot.title= element_text(hjust=0.5),
        axis.title = element_blank(),
        panel.grid = element_line("transparent"),
        plot.caption = element_text(hjust = 0)) +
  labs(caption = "Range: -0.3 to 0.5\nSource: NDVI Landsat 8 ETM, Google Earth Engine",
       title='W A S H I N G T O N')
