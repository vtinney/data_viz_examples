library(raster)
library(rgdal)
library(tidyverse)
library(ggplot)
library(SDMTools)
library(dplyr)
library(mapview)
library(mapedit)
library(sf)
library(readr)
library(spatialEco)
library(cowplot)
library(classInt)
library(dismo)
library(XML)
library(maps)
library(sp)
library(reshape2)
library(grid)
library(ggplotify)
library(extrafont)
loadfonts()
library(ggmap)

base <- get_stamenmap(bbox=c(left=-122.3275, bottom=37.71583, top=37.8325,right=-122.1475),
               maptype = "terrain-lines",
               zoom=12,
               crop=TRUE)

setwd('C:/Users/vat05/Google Drive/EDF_shared/Clip/')
oak <- readOGR(dsn=getwd(), layer='oak')

oak.f <- fortify(oak) %>% 
  mutate(id = as.numeric(id))


setwd('C:/Users/vat05/Google Drive/EDF_shared/clustering/')
list.files()

# NO2 - using GSV and Larkin et al. 2017 concentrations
gsv.no2 <- raster("Oakland, All-cause mortality, GSV, Atkinson & Butland 2018, point estimate, ages 25-99 years, CBG baseline disease rates.tif")                                              
lark <- raster("Oakland, All-cause mortality, Larkin et al. 2017, Atkinson & Butland 2018, point estimate, ages 25-99 years, CBG baseline disease rates.tif")                                   

gsv.no2 <- mask(gsv.no2, oak)
lark <- mask(lark, oak)


gsv.no2[gsv.no2 == 0] <- NA
lark[lark == 0] <- NA

# BC - using GSV and van Donkelaar et al. 2019 concentrations
gsv.bc <- raster("Oakland, All-cause mortality, GSV, Janssen et al. 2011, point estimate, ages 25-99 years, CBG baseline disease rates.tif")         
vd <- raster("Oakland, All-cause mortality, van Donkelaar, Janssen et al. 2011, point estimate, ages 25-99 years, CBG baseline disease rates.tif")

gsv.bc <- mask(gsv.bc, oak)
vd <- mask(vd, oak)

gsv.bc[gsv.bc == 0] <- NA
vd[vd == 0] <- NA

#/////////////////////////////////////////////////////////////////////////////////////////////
# NO2
#/////////////////////////////////////////////////////////////////////////////////////////////


# Correlation
no2 <- stack(gsv.no2, lark)

cor(values(no2[[1]]),
    values(no2[[2]]),
    use = "na.or.complete")

# 0.67 overall correlation


# Method 1 - Estimate a linear model
rc <- corLocal(no2[[1]], no2[[2]], method='spearman', test=TRUE)
rc <- mask(rc, oak)

dat <- rasterToPoints(rc)
dat <- data.frame(dat)
colnames(dat) <- c('lon','lat','Spearman','p_value')

rc_spdf <- as(rc, "SpatialPointsDataFrame") # create spatialpoints dataframe
rc_df <- as_tibble(rc_spdf) # convert that to a plain tibble
colnames(rc_df) <- c("Spearman","p_value","x", "y")



colmat<-function(nquantiles=10, upperleft=rgb(0,150,235, maxColorValue=255), upperright=rgb(130,0,80, maxColorValue=255), bottomleft="grey", bottomright=rgb(255,230,15, maxColorValue=255), xlab="x label", ylab="y label"){
  my.data<-seq(-1,1,.01)
  my.class<-classIntervals(my.data,n=nquantiles,style="quantile")
  my.pal.1<-findColours(my.class,c(upperleft,bottomleft))
  my.pal.2<-findColours(my.class,c(upperright, bottomright))
  col.matrix<-matrix(nrow = 202, ncol = 201, NA)
  for(i in 1:201){
    my.col<-c(paste(my.pal.1[i]),paste(my.pal.2[i]))
    col.matrix[202-i,]<-findColours(my.class,my.col)}
  plot(c(1,1),pch=19,col=my.pal.1, cex=0.5,xlim=c(-1,1),ylim=c(0,1),frame.plot=F, xlab=xlab, ylab=ylab,cex.lab=0.7)
  for(i in 1:201){
    col.temp<-col.matrix[i-1,]
    points(my.data,rep((i-1)/200,201),pch=15,col=col.temp, cex=1)}
  seqs<-seq(0,200,(200/nquantiles))
  seqs[1]<-1
  col.matrix<-col.matrix[c(seqs), c(seqs)]}


 col.matrix<-as.grob(function () colmat(nquantiles=10, upperleft="#eeb479", upperright="#e9e29c", 
                                        bottomleft="#cf597e", bottomright="#009392",
                                        xlab="Spearman correlation", ylab="p-value"))
 
 
 col.matrix2<-(colmat(nquantiles=10, upperleft="#eeb479", upperright="#e9e29c", 
                      bottomleft="#cf597e", bottomright="#009392",
                            xlab="Spearman correlation", ylab="p-value"))

 
 
bivariate.map<-function(rasterx, rastery, colormatrix=col.matrix, nquantiles=10){
  quanmean<-getValues(rasterx)
  temp<-data.frame(quanmean, quantile=rep(NA, length(quanmean)))
  brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  r1<-within(temp, quantile <- cut(quanmean, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr<-data.frame(r1[,2]) 
  quanvar<-getValues(rastery)
  temp<-data.frame(quanvar, quantile=rep(NA, length(quanvar)))
  brks<-with(temp, quantile(temp,na.rm=TRUE, probs = c(seq(0,1,1/nquantiles))))
  r2<-within(temp, quantile <- cut(quanvar, breaks = brks, labels = 2:length(brks),include.lowest = TRUE))
  quantr2<-data.frame(r2[,2])
  as.numeric.factor<-function(x) {as.numeric(levels(x))[x]}
  col.matrix2<-colormatrix
  cn<-unique(colormatrix)
  for(i in 1:length(col.matrix2)){
    ifelse(is.na(col.matrix2[i]),col.matrix2[i]<-1,col.matrix2[i]<-which(col.matrix2[i]==cn)[1])}
  cols<-numeric(length(quantr[,1]))
  for(i in 1:length(quantr[,1])){
    a<-as.numeric.factor(quantr[i,1])
    b<-as.numeric.factor(quantr2[i,1])
    cols[i]<-as.numeric(col.matrix2[b,a])}
  r<-rasterx
  r[1:length(r)]<-cols
  return(r)}

bivmap<-bivariate.map(rc[[1]],rc[[2]], colormatrix=col.matrix2, nquantiles=10)

b_spdf <- as(bivmap, "SpatialPointsDataFrame") # create spatialpoints dataframe
b_df <- as_tibble(b_spdf) # convert that to a plain tibble
colnames(b_df) <- c("quantiles","x", "y")

map <- ggmap(base)  +
  geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), fill="grey50",alpha=0.3)+
  geom_raster(data = b_df, aes(x = x, y = y,  fill = quantiles), alpha=0.7) +
  theme_minimal() +
  coord_equal() +
  scale_fill_gradientn(colors = col.matrix2)+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_line("transparent"),
        text = element_text(family = "Lato"),
        plot.caption = element_text(hjust = 0)) +
  guides(fill = guide_colourbar(barheight = 0.3, barwidth = 20, direction = "horizontal", ticks = FALSE)) +
  labs(title = "Correlation of all-cause mortality rate estimates using\nLUR and mobile monitoring of nitrogen dioxide",
       caption='\nBivariate map of local spearman correlation and p-value using 10% quantiles')


no2_map <- ggdraw(map) + draw_plot(col.matrix, 
                        width = 0.4, height = 0.4, x = 0.03, y = 0.15) 

ggsave('no2.correlation.png',dpi=320)
#/////////////////////////////////////////////////////////////////////////////////////////////
# BC
#/////////////////////////////////////////////////////////////////////////////////////////////

# Correlation
bc <- stack(gsv.bc, vd)

cor(values(bc[[1]]),
    values(bc[[2]]),
    use = "na.or.complete")

# 0.36 overall correlation


# Method 1 - Estimate a linear model
rc <- corLocal(bc[[1]], bc[[2]], method='spearman', test=TRUE)
rc <- mask(rc, oak)

dat <- rasterToPoints(rc)
dat <- data.frame(dat)
colnames(dat) <- c('lon','lat','Spearman','p_value')

rc_spdf <- as(rc, "SpatialPointsDataFrame") # create spatialpoints dataframe
rc_df <- as_tibble(rc_spdf) # convert that to a plain tibble
colnames(rc_df) <- c("Spearman","p_value","x", "y")


bivmap<-bivariate.map(rc[[1]],rc[[2]], colormatrix=col.matrix2, nquantiles=10)

b_spdf <- as(bivmap, "SpatialPointsDataFrame") # create spatialpoints dataframe
b_df <- as_tibble(b_spdf) # convert that to a plain tibble
colnames(b_df) <- c("quantiles","x", "y")

map <- ggmap(base)  +
  geom_polygon(data = oak.f, aes(x = long, y = lat, group = group), fill="grey50",alpha=0.5)+
  geom_raster(data = b_df, aes(x = x, y = y,  fill = quantiles), alpha=0.7) +
  theme_minimal() +
  coord_equal() +
  scale_fill_gradientn(colors = col.matrix2)+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_line("transparent"),
        text = element_text(family = "Lato"),
        plot.caption = element_text(hjust = 0)) +
  guides(fill = guide_colourbar(barheight = 0.3, barwidth = 20, direction = "horizontal", ticks = FALSE)) +
  labs(title = "Correlation of all-cause mortality rate estimates using\nsatellite-derived and mobile monitoring black carbon",
       caption='\nBivariate map of local spearman correlation and p-value using 10% quantiles')


bc_map <- ggdraw(map) + draw_plot(col.matrix, 
                        width = 0.4, height = 0.4, x = 0.03, y = 0.15) 
ggsave('bc.correlation.png',dpi=320)

plot_grid(no2_map, bc_map)
ggsave('bivariate_correlation_panel.png',dpi=320, height=7,width=13)

