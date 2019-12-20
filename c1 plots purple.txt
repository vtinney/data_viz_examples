library(raster)
library(rgdal)
library(dplyr)
library(ggplot2)
library(ggspatial)
library(scales)
library(extrafont)
loadfonts()
#library(dplyr)
library(sf)
library(OpenStreetMap)
library(rJava)
library(raster)
library(rgdal)
library(ggplot2)
#library(tidyverse)
library(cowplot)



theme_map <- function(...) {
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    plot.title=element_text(hjust = 0.5, size=11,family="DejaVu Sans"),
    plot.subtitle=element_text(hjust=0, size=9,family="DejaVu Sans"),
    plot.caption = element_text(hjust=0, size=7,family="DejaVu Sans"),
    legend.title=element_text(size=11, family="DejaVu Sans"),
    legend.text=element_text(size=11, family="DejaVu Sans"),
    axis.title=element_blank(),
    legend.position = 'bottom',
    legend.justification='center',
    legend.spacing = unit(c(-.1,0.2,.2,0.2), "cm"),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.line = element_blank(),
    rect = element_blank())
}


# Functions =========================================================================
myZonal <- function (x, z, stat, digits = 0, na.rm = TRUE, 
                     ...) {
  library(data.table)
  fun <- match.fun(stat) 
  vals <- getValues(x) 
  zones <- round(getValues(z), digits = digits) 
  rDT <- data.table(vals, z=zones) 
  setkey(rDT, z) 
  rDT[, lapply(.SD, fun, na.rm = TRUE), by=z] 
} 

ZonalPipe<- function (zone.in, raster.in, shp.out=NULL, stat){
  require(raster)
  require(rgdal)
  require(plyr)
  
  # Load raster
  r <- raster.in
  # Load zone shapefile
  shp <- zone.in
  # Project 'zone' shapefile into the same coordinate system than the input raster
  shp <- spTransform(shp, crs(r))
  
  # Add ID field to Shapefile
  shp@data$ID<-c(1:length(shp@data[,1]))
  
  # Crop raster to 'zone' shapefile extent
  r <- crop(r, extent(shp))	
  # Rasterize shapefile
  zone <- rasterize(shp, r, field="ID", dataType = "INT1U") # Change dataType if nrow(shp) > 255 to INT2U or INT4U
  
  # Zonal stats
  Zstat<-data.frame(myZonal(r, zone, stat))
  colnames(Zstat)<-c("ID", paste0(names(r), "_", c(1:(length(Zstat)-1)), "_",stat))
  
  # Merge data in the shapefile and write it
  shp@data <- plyr::join(shp@data, Zstat, by="ID")
  
  if (is.null(shp.out)){
    return(shp)
  }else{
    writeOGR(shp, shp.out, layer= sub("^([^.]*).*", "\\1", basename(zone.in)), driver="ESRI Shapefile")
  }
}

#=================================================================================================
setwd('/home/vtinney/clip1/')
eo <- readOGR(dsn=getwd(), layer='EO')
eo.f <- fortify(eo) %>% 
  mutate(id = as.numeric(id))

eo.cbg <- readOGR(dsn=getwd(), layer='Export_Output')
eo.cbg.f <- fortify(eo.cbg) %>% 
  mutate(id = as.numeric(id))

setwd('/home/vtinney/pop1/')
c <- raster('oak.pop.ls.night.25.tif')
c <- crop(c,eo)
c <- mask(c,eo)

map <- openmap(c(37.791334,-122.327816), c(37.832312,-122.253477),
               type = "esri-topo",
               mergeTiles = TRUE)
base <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


#=========================================================================================

setwd("/home/vtinney/results2/pub.maps/all.cause/af/")
list.files()

a <- raster("West and Downtown Oakland, GSV, Atkinson & Butland, 2018, point estimate.tif")             

a[a==0] <- NA

min.a <- minValue(a)
max.a <- maxValue(a)
a[a<3]<-3
a[a>15]<-15

min.x <- round(min.a,2)
max.x <- round(max.a,2)
mid.x <- round((3+15)/2,2)

a <- rasterToPoints(a)
a <- data.frame(a)
colnames(a) <- c('lon','lat','GSV')

#GSV\n

a1 <- autoplot(base)  +
  geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
               fill="grey50",alpha=0.5)+
  geom_tile(data=a,aes(lon, lat, fill = GSV),alpha=0.8) +
  scale_fill_viridis(option="magma","Attributable Fraction (%)\n",
                       breaks=c(min.x,mid.x,max.x),
                       labels=c(min.x,mid.x,max.x),
                       limits=c(min.x,max.x),
                       na.value = 'grey50',
                       guide = guide_colourbar(
                         direction = "horizontal",
                         label=TRUE,
                         keyheight = unit(1, units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 0.5,
                         barwidth = 10,
                         nrow = 1,
                         byrow = T,
                         label.position = "bottom"))+
  theme_map()+
dark_theme_gray()+
  geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
            color = "grey60", size = 0.5)+
  labs(subtitle=paste0('Range: ',round(min.a,2),' to ',round(max.a,2),'.'),sep='')


setwd("/home/vtinney/results2/pub.maps/all.cause/paf/")
d1 <- raster("West and Downtown Oakland, GSV, Atkinson & Butland, 2018, point estimate, ages 25-99 years, CBG baseline disease rates, LandScan USA night time population estimates, GPWv4  (1).tif")


d1[d1==0] <- NA

min.d <- minValue(d1)
max.d <- maxValue(d1)
med.d <- quantile(d1,probs=0.5)



d <- rasterToPoints(d1)
d <- data.frame(d)

colnames(d) <- c('lon','lat','GSV')

crs(eo.cbg) <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"


cbg.shp.f <- fortify(eo.cbg) %>% 
  mutate(id = as.numeric(id))

zone.in <- eo.cbg
raster.in <- d1

shp2 <- ZonalPipe(zone.in, raster.in, stat="sum")
shp2@data <- shp2@data %>% mutate(id = row.names(.))
shp_df <- fortify(shp2, region = "id")
shp_df <- shp_df %>% left_join(shp2@data, by = c("id"="id"))
shp_df <- as.data.frame(shp_df)
shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA
r.min.d <- min(shp_df[,ncol(shp_df)],na.rm=TRUE)
r.max.d <- max(shp_df[,ncol(shp_df)],na.rm=TRUE)
r.med.1 <- median(shp_df[,ncol(shp_df)],na.rm=TRUE)
colnames(shp_df)[ncol(shp_df)] <- 'hia.val'


zone.in <- eo.cbg
raster.in <- c

shp3 <- ZonalPipe(zone.in, raster.in, stat="sum")
shp3@data <- shp3@data %>% mutate(id = row.names(.))
pop_df <- fortify(shp3, region = "id")
pop_df <- pop_df %>% left_join(shp3@data, by = c("id"="id"))
pop_df <- as.data.frame(pop_df)
pop_df[,ncol(pop_df)][pop_df[,ncol(pop_df)] == 0] <- NA
colnames(pop_df)[ncol(pop_df)] <- "pop.val"

rate_df <- merge(shp_df,pop_df,by='order')
rate_df <- as.data.frame(rate_df)
rate_df$rate <- NA
rate_df$rate <- (rate_df$hia.val*100000)/rate_df$pop.val
rate_df$rate[rate_df$rate==0]<-NA
rate.min <- min(rate_df[,ncol(rate_df)],na.rm=TRUE)
rate.max <- max(rate_df[,ncol(rate_df)],na.rm=TRUE)
rate.med <- median(rate_df[,ncol(rate_df)],na.rm=TRUE)
rate_df$ratio <- rate_df[,ncol(rate_df)]/rate.med
ratio.min.label <- round(min(rate_df$ratio,na.rm=TRUE),2)
ratio.max.label <- round(max(rate_df$ratio,na.rm=TRUE),2)
rate.min.label <- round(rate.min,2)
rate.max.label <- round(rate.max,2)
rate.med.label <- round(rate.med,2)
rate.mean <- (rate.min+rate.max)/2
rate.mean.label <- round(rate.mean,2)

shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] >1.5] <- 1.5
rate_df$ratio[rate_df$ratio > 2] <- 2

#GSV\nCBG baseline disease rates


d3 <- autoplot(base)  +
  geom_polygon(data = shp_df, aes(x = long, y = lat, group = group, fill = shp_df$hia.val),alpha=0.7)+
  scale_fill_viridis(option="magma","Count (n) cases\nper Census Block Group",
                       na.value='grey50',
                       breaks=c(0,0.75,1.5),
                       labels=c("0.00","0.75","1.50"),
                       limits=c(0, 1.5),
                       guide = guide_colourbar(
                         direction = "horizontal",
                         label=TRUE,
                         keyheight = unit(2, units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 0.5,
                         barwidth = 10,
                         nrow = 1,
                         byrow = T,
                         label.position = "bottom"))+
  theme_map()+
dark_theme_gray()+
  geom_path(data = cbg.shp.f, aes(x = long, y = lat, group = group), 
            color = "grey60", size = 0.1)+
  labs(subtitle=paste0('Range: ',round(r.min.d,2),' to ',round(r.max.d,2),' per CBG. '),sep='')


#//////////////////////////////////////////////////////////////////////

setwd("/home/vtinney/results2/pub.maps/all.cause/mr/")

t <- raster("West and Downtown Oakland, GSV, Atkinson & Butland, 2018, point estimate, ages 25-99 years, CBG baseline disease rates.tif")                 

t[t==0] <- NA

min.t <- minValue(t)
max.t <- maxValue(t)
med.t <- quantile(t,probs=0.5)
t[t>20] <- 20

t <- rasterToPoints(t)
t <- data.frame(t)


colnames(t) <- c('lon','lat','GSV')


#GSV\nCBG baseline disease rates

t1 <- autoplot(base)  +
  geom_polygon(data = eo.f, aes(x = long, y = lat, group = group), 
               fill="grey50",alpha=0.5)+
  geom_tile(data=t,aes(lon, lat, fill = GSV),alpha=0.8) +
  scale_fill_viridis(option="magma","Risk per 10,000\n",
                       breaks=c(0,10,20),
                       labels=c("0.00","10.00","20.00"),
                       limits=c(0,20),
                       na.value = 'grey50',
                       guide = guide_colourbar(
                         direction = "horizontal",
                         label=TRUE,
                         keyheight = unit(1, units = "mm"),
                         title.position = 'top',
                         title.hjust = 0.5,
                         label.hjust = 0.5,
                         barwidth = 10,
                         nrow = 1,
                         byrow = T,
                         label.position = "bottom"))+
  theme_map()+
dark_theme_gray()+
  geom_path(data = eo.f, aes(x = long, y = lat, group = group), 
            color = "grey60", size = 0.5)+
  labs(subtitle=paste0('Range: ',round(min.t,2),' to ',round(max.t,2),'.'),sep='')


title.blank <- ggdraw() + 
  draw_label(
    "\n",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
title.A <- ggdraw() + 
  draw_label(
    "A. Attributable fraction\n",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
title.B <- ggdraw() + 
  draw_label(
    "B. Attributable cases\nper 10,000",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
title.C <- ggdraw() + 
  draw_label(
    "C. Attributable cases\n",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
title.D <- ggdraw() + 
  draw_label(
    "D. Number of times the median\ncases per 100,000",
    fontface = 'bold',
    x = 0,
    hjust = 0
  ) +
  theme(
    plot.margin = margin(0, 0, 0, 7)
  )
title.gsv.cbg <- ggdraw() + 
  draw_label(
    "GSV\nCBG baseline\ndisease rates",
    x = 0,
    hjust = 0
  )

setwd("/home/vtinney/results2/pub.maps/all.cause/")
a2 <- a1+theme(legend.position="bottom")
t2 <- t1+theme(legend.position="bottom")
d4 <- d3+theme(legend.position="bottom")

plot_titles <- plot_grid(title.blank,title.A,title.B,title.C,title.D,nrow=1,rel_widths = c(0.5,1,1,1,1))

plot_row5 <- plot_grid(title.gsv.cbg,a2,t2,d4,nrow=1,rel_widths = c(0.5,1,1,1,1))
ggsave('plub.plot.gsv.bottom.png',height=7,width=12,dpi=320)


#///////////////////////////////////////////////////////////////////////////////////////////////////
