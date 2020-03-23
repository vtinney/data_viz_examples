library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(dplyr)
library(sf)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(splitstackshape)
library(plotly)
library(tidyverse)
library(htmlwidgets)
library(viridis)
library(rcartocolor)
library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(scales)
library(extrafont)
library(maptools)
loadfonts()
library(dplyr)
library(sf)
library(OpenStreetMap)
library(rJava)
library(gridExtra)
library(rgeos)
library(ggpubr)
library(spatialEco)
library(grid)
library(classInt)

#///////////////////////////////////////////////////////////////////////////////////////////////////

theme_map <- function(...) {
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(hjust = 0, size=11,family="DejaVu Sans Light"),
    plot.subtitle=element_text(hjust=0, size=9,family="DejaVu Sans Light"),
    plot.caption = element_text(hjust=0, size=7,family="DejaVu Sans Light"),
    legend.title=element_text(size=11, family="DejaVu Sans Light"),
    legend.text=element_text(size=7, family="DejaVu Sans Light"),
    axis.title=element_blank(),
    legend.position = 'bottom',
    legend.justification='center',
    panel.border = element_rect(colour = "black", fill=NA, size=0.5),
    rect = element_blank())
}

#///////////////////////////////////////////////////////////////////////////////////////////////////

setwd('/GWSPH/home/vtinney/NIH/')

#/////////////////////////////////////////////////////////////////////////////////////////////


# setwd('C:/Users/vat05/Google Drive/NIH/GIS/')
list.files()

shp <- readOGR(dsn=getwd(), layer='wellsfinal')

shp.f <- fortify(shp) %>% 
  mutate(id = as.numeric(id))

shp2 <- shp

state <- readOGR(dsn=getwd(), layer='tl_2017_us_state')
state <- crop(state, shp)

state.f <- fortify(state) %>% 
  mutate(id = as.numeric(id))

country <- readOGR(dsn=getwd(), layer='ne_10m_admin_0_countries')

country.f <- fortify(country) %>% 
  mutate(id = as.numeric(id))


shp2@data <- shp2@data %>% mutate(id = row.names(.))
shp_df <- fortify(shp2, region = "id")
shp_df <- shp_df %>% left_join(shp2@data, by = c("id"="id"))
shp_df <- as.data.frame(shp_df)

shp_df2 <- shp_df

 shp_df2$SUM_count[is.na(shp_df2$SUM_count)] <- 0

shp_df2$count2 <- as.numeric(as.character(shp_df2$SUM_count))


base <- openmap(c(ymin(shp),xmin(shp)),c(ymax(shp),xmax(shp)),
                type = "esri-topo",
                mergeTiles = TRUE)
base <- openproj(base, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


brks <- c(0,10,50,500,1500,10000,25000,57532)
labels <- c('0','1-10','11-50','51-500','501-1,500','1,501-10,000','> 10,000')
shp_df2$brks <- cut(shp_df2$count2, 
                     breaks = brks, 
                     include.lowest = TRUE, 
                     labels = labels)

brks_scale <- levels(shp_df2$brks)
labels_scale <- brks_scale

p <- autoplot(base) +
  geom_polygon(data = shp_df2, aes(x = long, y = lat, group = group, fill = shp_df2$brks))
 
q <-  p+ scale_fill_manual(values=c('#A16928','#bd925a','#d6bd8d','#edeac2','#b5c8b8','#79a7ac','#2887a1'),
    name = "Well counts (n) per county",
    breaks=brks_scale,
    labels = labels_scale,
    drop=FALSE,
    na.value = '#A16928',
guide = guide_legend(
  direction = "horizontal",
  keyheight = unit(2, units = "mm"),
  keywidth = unit(140 / length(labels), units = "mm"),
  title.position = 'top',
  # I shift the labels around, the should be placed 
  # exactly at the right end of each legend key
  title.hjust = 0.5,
  nrow = 1,
  byrow = T,
  label.position = "bottom"
))+
  theme_map() + ####
  theme(legend.position = "bottom") +
    # geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
    #           color = "white", size = 0.05)+
  geom_path(data = state.f, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.2)+
    labs(title='A. Well count per US county.',
         subtitle='',
         caption='Well data 2014 from the Energy Administration Agency published by Oak Ridge National Laboratories.')


#ggsave('wells_test6.png',dpi=320)
#ggsave('wells.pdf')


#/////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

country <- readOGR(dsn=getwd(), layer='us')

co <- crop(country, state)

us.lark <- raster('conc.lark.us.tif')
r <- us.lark

r <- crop(r, co)
r <- mask(r, co)

co.f <- fortify(co) %>% 
  mutate(id = as.numeric(id))


brks <- c(1,5,7,8,10,30,35)
labels <- c('1','2-5','6-7','8-10','11-30','>30')



r[r == 0] <- NA
r.min <- minValue(r)
r.max <- maxValue(r)
min.r.label <- round(r.min,0)
max.r.label <- round(r.max,0)
df <- rasterToPoints(r)
df <- as.data.frame(df)
colnames(df) <- c('lon','lat','val')
r.mean <- round((r.min+r.max)/2,0)

df$brks <- cut(df$val, 
               breaks = brks, 
               include.lowest = TRUE, 
               labels = labels)

brks_scale <- levels(df$brks)
labels_scale <- brks_scale

no2 <- autoplot(base)  +
  geom_tile(data=df,aes(lon, lat, fill = df$brks),alpha=0.8) +
  scale_fill_manual(values=c('#A16928','#bd925a','#edeac2','#b5c8b8','#79a7ac','#2887a1'),
                   name = "ppb",
                   breaks=brks_scale,
                   labels = labels_scale,
                   drop=FALSE,
                   na.value = '#A16928',
                   guide = guide_legend(
                     direction = "horizontal",
                     keyheight = unit(2, units = "mm"),
                     keywidth = unit(140 / length(labels), units = "mm"),
                     title.position = 'top',
                     # I shift the labels around, the should be placed 
                     # exactly at the right end of each legend key
                     title.hjust = 0.5,
                     nrow = 1,
                     byrow = T,
                     label.position = "bottom"
                   ))+
  theme_map() + ####
  theme(legend.position = "bottom") +
  # geom_path(data = shp.f, aes(x = long, y = lat, group = group), 
  #           color = "white", size = 0.05)+
  geom_path(data = state.f, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.2)+
  theme_map() + ####
  theme(legend.position = "bottom") +
  labs(title='B. US nitrogen dioxide concentrations.',
       subtitle='Larkin et al. 2017, annual average concentrations for 2011.')

ggsave('us.larkin.pdf')
#ggsave('us.larkin.png',dpi=320)

#/////////////////////////////////////////////////////////////////////////////////////////////////

vd.pm <- raster('conc.us.vd.pm.2016.tif')
r <- vd.pm

r <- crop(r, co)
r <- mask(r, co)

r[r == 0] <- NA
r.min <- minValue(r)
r.max <- maxValue(r)
min.r.label <- round(r.min,0)
max.r.label <- round(r.max,0)
df <- rasterToPoints(r)
df <- as.data.frame(df)
colnames(df) <- c('lon','lat','val')
r.mean <- round((r.min+r.max)/2,0)

brks <- c(1,5,7,9,10,15,20,27)
labels <- c('1','2-5','6-7','8-9','10-15','16-20','>20')

df$brks <- cut(df$val, 
               breaks = brks, 
               include.lowest = TRUE, 
               labels = labels)

brks_scale <- levels(df$brks)
labels_scale <- brks_scale



pm <- autoplot(base)  +
  geom_tile(data=df,aes(lon, lat, fill = df$brks),alpha=0.8) +
  scale_fill_manual(values=c('#A16928','#bd925a','#d6bd8d','#edeac2','#b5c8b8','#79a7ac','#2887a1'),
                    name = expression(paste(µg/m^3)),
                    breaks=brks_scale,
                    labels = labels_scale,
                    drop=FALSE,
                    na.value = '#A16928',
                    guide = guide_legend(
                      direction = "horizontal",
                      keyheight = unit(2, units = "mm"),
                      keywidth = unit(140 / length(labels), units = "mm"),
                      title.position = 'top',
                      # I shift the labels around, the should be placed 
                      # exactly at the right end of each legend key
                      title.hjust = 0.5,
                      nrow = 1,
                      byrow = T,
                      label.position = "bottom"
                    ))+
  theme_map() + ####
  theme(legend.position = "bottom") +
  geom_path(data = state.f, aes(x = long, y = lat, group = group), 
            color = "white", size = 0.2)+
  theme_map() + ####
  theme(legend.position = "bottom") +
  labs(title='C. US fine particulate matter concentrations.',
       subtitle='van Donkelaar et al. 2016.') ###############################################
#ggsave('us.vd.pm.png',dpi=320)
ggsave('us.vd.pm.pdf')

plot <- plot_grid(q, no2, pm)
ggsave('test_grid3.pdf', height=7, width=21)
