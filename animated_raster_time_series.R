# Part 1 - RasterVis and output the maximum concentration for each raster file
# Need to do RasterVis and ggplot separately as RasterVis will interfere with ggplot package

library(raster)
library(rgdal)
library(rasterVis)
library(animation)
library(classInt)


setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Visualization/Bay_Area/int/")
# Stack files for 365 days of Bay area fine particulate matter for 2016
s <- list.files(pattern = "\\.tif*", full.names=TRUE)
s <- stack(s)


# Shapefile outline of the Bay area
setwd('C:/Users/Veronica Tinney/Google Drive/Veronica Southerland Dropbox team space cache/run/clip')
bay <- readOGR(dsn=getwd(), layer='bay')

# Color scale 
col.2 <- colorRampPalette(c("#556270", "#4ECDC4", "#C7F464", "#FF6B6B", "#C44D58"))


# Create the dates to coordinate with the raster files
itemizeDates <- function(startDate="1-1-16", endDate="12-31-16", 
                         format="%m-%d-%y") {
  out <- seq(as.Date(startDate, format=format), 
             as.Date(endDate, format=format), by="days")  
  format(out, format)
}

# Create a dataframe of titles for each date to scroll across the top
titles <- itemizeDates(startDate="1-1-16", endDate="12-31-16")
title <- as.data.frame(titles)

# This is a loop that will go through each raster file in the folder and add it to the animation
setwd("C:/Users/Veronica Tinney/Google Drive/EDF_shared/Visualization/")
saveGIF({
  for(i in c(1:nlayers(s))){
    l <- levelplot(s[[i]],
                   margin=FALSE,
                   main =list(paste0('Fine particulate matter. Date: ',title$titles[i],sep=' '), fontfamily = "serif",cex=1,font=2),
                   col.regions=col.2,
                   sub=list('ug/m^3'),
                   at=seq(0, 40, len=101),
                   scales=list(draw=FALSE),
                   par.settings=list(panel.background=list(col='black'), 
                                     alpha=0.3, 
                                     axis.text=list(fontfamily="Calibri"),
                                     par.xlab.text=list(fontfamily="Calibri"),
                                     par.ylab.text=list(fontfamily="Calibri"),
                                     par.main.text=list(fontfamily="Calibri"),
                                     par.sub.text=list(fontfamily="Calibri")),
                   xlab=NULL,
                   ylab=NULL,
                   xlim=c(-123.6325, -121.2083),
                   ylim=c(36.8925, 38.865),
                   colorkey=list(
                     space='bottom',
                     axis.line=list(col='white'),
                     width=1))+ layer(sp.polygons(bay, fill='white', alpha=0.3))
    plot(l)
  }
}, interval=0.5, movie.name="animation_bay_11.1.2019.interpolation.gif") # save as one animation



# Output a file that will give the date and the max and minimum value for each day
d <- as.matrix(titles)
mat <- matrix(nrow=nlayers(s),ncol=2)

for(i in c(1:nlayers(s))){
  mat[,1] <- minValue(s)
  mat[,2] <- maxValue(s)
}

mat <- cbind(mat, d)
head(mat)
min.max <- as.data.frame(mat)
head(min.max)
names(min.max) <- c('Min','Max','Date')

min.max$Min <- as.numeric(as.character(min.max$Min))
min.max$Max <- as.numeric(as.character(min.max$Max))
min.max$Date <- as.character(min.max$Date)
write.csv(min.max, 'min.max.2016.pm.csv')
