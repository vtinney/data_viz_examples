library(mapdeck)
library(raster)
library(rgdal)
library(tidyverse)
library(htmltools)
library(ggdark)
library(viridis)
library(ggplot2)


# No2 datasets
setwd('/home/vtinney/')
lark <- raster('bay.no2.larkin.tif') #Larkin et al. 2017
gsv <- raster('NO2.tif') # Google street View
jm <- raster('conc.no2.centroids.2.tif') # Bechle et al. 2015
oak <- readOGR(dsn=getwd(), layer='oak') # Shapefile for Oakland, CA

gsv2 <- crop(gsv,oak)
gsv2 <- mask(gsv2,oak)

lark2 <- crop(lark, oak)
lark2 <- mask(lark2, oak)

jm2 <- crop(jm, oak)
jm2 <- mask(jm2,oak)

gsv2[gsv2==0] <- NA
lark2[lark2==0] <- NA
jm2[jm2 == 0] <- NA


df <- rasterToPoints(lark2)
df <- as.data.frame(df)
colnames(df) <- c('lon','lat','val')
df$conc <- 'LUR'


df2 <- rasterToPoints(gsv2)
df2 <- as.data.frame(df2)
colnames(df2) <- c('lon','lat','val')
df2$conc <- 'GSV'

df4 <- rasterToPoints(jm2)
df4 <- as.data.frame(df4)
colnames(df4) <- c('lon','lat','val')
df4$conc <- 'CB'

df3 <- rbind(df, df2,df4)

df$val <- as.numeric(df$val)

mean1 <- median(df$val,na.omit=TRUE)
mean2 <- median(df2$val,na.omit=TRUE)
mean3 <- median(df4$val,na.omit=TRUE)

p <- ggplot(data = df3, aes(x=val, fill=conc)) +
  geom_density(alpha=0.4) +
  scale_fill_manual(values=c("#F2E61E", "#238D90","#482677FF"))+
  geom_vline(xintercept=mean1, color="#482677FF",linetype="dashed") +
  geom_vline(xintercept=mean3, color="#F2E61E",linetype="dashed") +
  geom_vline(xintercept=mean2, color="#238D90",linetype="dashed") +
  theme_minimal(base_family = "DejaVu Sans",
                base_size = 16)+
  annotate(geom = "text",
         y = 0.075,
        x=5,
           label = "GSV\nmedian=12",
           vjust = -1,
           fontface = 1,
           size = 4,
           family = "DejaVu Sans",
           color='white') +
  annotate(geom = "text",
           y = 0.075,
           x=25,
           label = "LUR\nmedian=19",
           vjust = -1,
           fontface = 1,
           size = 4,
           family = "DejaVu Sans",
           color='white') +
  annotate(geom = "text",
           y = 0.2,
           x=20,
           label = "CB\nmedian=15",
           vjust = -1,
           family = "DejaVu Sans",
           fontface = 1,
           size = 4,
           color='white') +
  labs(y='Density',x='Concentration (ppb)',title='Oakland nitrogen dioxide concentration density')+
  theme(legend.position = "none")+
  coord_cartesian(clip = 'off')+
  theme(plot.title = element_text(family = "DejaVu Sans",hjust = 0, size=14,color='white'),
        axis.title=element_text(size=12,color='white'),
        plot.background = element_rect(fill = "grey10"),
        text=element_text(family = "DejaVu Sans"),
        legend.text = element_text(size=12, family='DejaVu Sans',color = 'white'),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ggsave('densityplot_no2.png',height=5,width=7,dpi=320)


#/////////////////////////////////////////////////////////////////////////////////////////////////
setwd('/home/vtinney/')
pm <- raster('conc.pm.mean.15.16.tif') # Di et al. 2016
vd <- raster('vd_mean.15.16.tif') # von Donkelaar et al. 2016
oak <- readOGR(dsn=getwd(), layer='oak') # Oakland, CA shapefile

pm <- crop(pm, oak)
pm <- mask(pm, oak)

vd <- crop(vd, oak)
vd <- mask(vd,oak)

pm[pm == 0] <- NA
vd[vd == 0] <- NA

df <- rasterToPoints(vd)
df <- as.data.frame(df)
colnames(df) <- c('lon','lat','val')
df$conc <- 'von Donkelaar et al. 2016'

df2 <- rasterToPoints(pm)
df2 <- as.data.frame(df2)
colnames(df2) <- c('lon','lat','val')
df2$conc <- 'Di et al. 2016'

df3 <- rbind(df, df2)

mean1 <- median(df$val,na.omit=TRUE)
mean2 <- median(df2$val,na.omit=TRUE)

p <- ggplot(data = df3, aes(x=val, fill=conc)) +
  geom_density(alpha=0.4) +
  scale_fill_manual(values=c("#F2E61E", "#238D90"))+
  geom_vline(xintercept=mean1, color="#238D90",linetype="dashed") +
  geom_vline(xintercept=mean2, color="#F2E61E",linetype="dashed") +
  theme_minimal(base_family = "DejaVu Sans",
                base_size = 16)+
  annotate(geom = "text",
           y = 0.25,
           x=14,
           label = "Median=13",
           vjust = -1,
           fontface = 1,
           size = 4,
           family = "DejaVu Sans",
           color='white') +
  annotate(geom = "text",
           y = 0.4,
           x=10,
           label = "Median=7.9",
           vjust = -1,
           fontface = 1,
           size = 4,
           family = "DejaVu Sans",
           color='white') +
  labs(y='Density',x='Concentration (ppb)',title='Oakland fine particulate matter concentration density')+
  theme(legend.position = "none")+
  coord_cartesian(clip = 'off')+
  theme(plot.title = element_text(family = "DejaVu Sans",hjust = 0, size=14,color='white'),
        axis.title=element_text(size=12,color='white'),
        plot.background = element_rect(fill = "grey10"),
        text=element_text(family = "DejaVu Sans"),
        legend.text = element_text(size=12, family='DejaVu Sans',color = 'white'),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title = element_blank())

ggsave('densityplot_pm.png',height=5,width=7,dpi=320)
