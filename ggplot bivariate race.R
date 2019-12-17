# A Script for bivariate analysis of health burden (as measured by attributable cases per 100,000)
# and census block groups with high percentages of racial minorities in the Bay area of California
# Date: 12-4-2019
# Author: V Southerland
# ================================================================================================


# from https://mran.revolutionanalytics.com/web/packages/checkpoint/vignettes/using-checkpoint-with-knitr.html
# if you don't need a package, remove it from here (commenting is probably not sufficient)

library(rstudioapi)
library(tidyverse) # ggplot2, dplyr, tidyr, readr, purrr, tibble
library(magrittr) # pipes
library(lintr) # code linting
library(sf) # spatial data handling
library(raster) # raster handling (needed for relief)
library(viridis) # viridis color scale
library(cowplot) # stack ggplots
library(rmarkdown)
library(rgdal)
library(dplyr)
library(purrr)
library(extrafont)
loadfonts()
library(sf)
library(OpenStreetMap)
library(rJava)



setwd('/GWSPH/home/vtinney/CB_race/')
# Load all input files and fortify shape files for ggplot

# Bay CBG race

cbg.bay.shp <- readOGR(dsn=getwd(), layer='cbg.bay')


# Bay county boundaries
bay.co.shp <- readOGR(dsn=getwd(), layer='bay.co')
bay.co.shp.f <- fortify(bay.co.shp) %>% 
  mutate(id = as.numeric(id))

# Bay attributable cases all-cause mortality
paf.bay <- raster('Bay area, All-cause mortality, Larkin et al. 2017, Atkinson & Butland 2018, point estimate, ages 25-99 years, County baseline disease rates, LandScan USA, GPWv4 age fractions.tif')
# WO attributable cases all-cause mortality
lur <- raster('West and Downtown Oakland, All-cause mortality, Larkin et al. 2017, Atkinson & Butland 2018, point estimate, ages 25-99 years, CBG baseline disease rates, LandScan USA, GPWv4 age fractions.tif')
gsv <- raster('West and Downtown Oakland, All-cause mortality, GSV, Atkinson & Butland 2018, point estimate, ages 25-99 years, CBG baseline disease rates, LandScan USA, GPWv4 age fractions.tif')
# WO CBG boundaries
o.cbg.shp <- readOGR(dsn=getwd(), layer='oakcbg')
o.cbg.shp.f <- fortify(o.cbg.shp) %>% 
  mutate(id = as.numeric(id))
# SF bay DEM

# Population raster
pop <- raster('bay.pop.ls.night.25.tif')

# Define Functions =========================================================================
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


#========================================================================================================
# Define the theme map

default_background_color <- "#f5f5f2"
default_font_color <- "#22211d"
default_font_family <- 'DejaVu Sans'

theme_map <- function(...) {
  theme_minimal() +
    theme(
      text = element_text(family = default_font_family,
                          color = default_font_color),
      # remove all axes
      axis.line = element_blank(),
      axis.text.x = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks = element_blank(),
      # add a subtle grid
      panel.grid.major = element_line(color = "#dbdbd9", size = 0.2),
      panel.grid.minor = element_blank(),
      # background colors
      plot.background = element_rect(fill = default_background_color,
                                     color = "#f5f5f2"),
      panel.background = element_rect(fill = default_background_color,
                                      color = "#f5f5f2"),
      legend.background = element_rect(fill = default_background_color,
                                       color = "#f5f5f2"),
      # borders and margins
      plot.margin = unit(c(.5, .5, .2, .5), "cm"),
      panel.border = element_blank(),
      panel.spacing = unit(c(-.1, 0.2, .2, 0.2), "cm"),
      # titles
      legend.title = element_text(size = 11),
      legend.text = element_text(size = 9, hjust = 0,
                                 color = default_font_color),
      plot.title = element_text(size = 15, hjust = 0.5,
                                color = default_font_color),
      plot.subtitle = element_text(size = 10, hjust = 0.5,
                                   color = default_font_color,
                                   margin = margin(b = -0.1,
                                                   t = -0.1,
                                                   l = 2,
                                                   unit = "cm"),
                                   debug = F),
      # captions
      plot.caption = element_text(size = 7,
                                  hjust = .5,
                                  margin = margin(t = 0.2,
                                                  b = 0,
                                                  unit = "cm"),
                                  color = "#939184"),
      ...
    )
}

#===============================================================================================
# Calculate the percentage minorities for each CBG

cbg.bay.shp <- cbg.bay.shp[,c(1:7,17:20,24)]

cbg.bay.shp$BLACK10 <- as.numeric(as.character(cbg.bay.shp$BLACK10))
cbg.bay.shp$ASIAN10 <- as.numeric(as.character(cbg.bay.shp$ASIAN10))
cbg.bay.shp$AMERIND10 <- as.numeric(as.character(cbg.bay.shp$AMERIND10))
cbg.bay.shp$WHITE10 <- as.numeric(as.character(cbg.bay.shp$WHITE10))
cbg.bay.shp$HISPPOP10 <- as.numeric(as.character(cbg.bay.shp$HISPPOP10))
cbg.bay.shp$TOTPOP10 <- as.numeric(as.character(cbg.bay.shp$TOTPOP10))

cbg.bay.shp$per.minor <- round(((cbg.bay.shp$BLACK10+cbg.bay.shp$AMERIND10+cbg.bay.shp$ASIAN10+cbg.bay.shp$HISPPOP10)/cbg.bay.shp$TOTPOP10)*100,2)

#cbg.bay.shp.f <- fortify(cbg.bay.shp) %>% 
#  mutate(id = as.numeric(id))

#===============================================================================================
# Create the attributable cases per 100,000 per CBG

zone.in <- cbg.bay.shp
raster.in <- paf.bay

shp2 <- ZonalPipe(zone.in, raster.in, stat="sum")

zone.in <- cbg.bay.shp
raster.in <- pop

shp3 <- ZonalPipe(zone.in, raster.in, stat="sum")


# Join rate data with spatial data
cbg.bay.shp.2 <- merge(shp2,cbg.bay.shp, by = "GEOID10")
cbg.bay.shp.3 <- merge(shp3, cbg.bay.shp.2, by="GEOID10")

names(cbg.bay.shp.3)
colnames(cbg.bay.shp.3@data)[14] <- 'pop'
colnames(cbg.bay.shp.3@data)[15] <- 'paf'

cbg.bay.shp.3@data$rate <- round((cbg.bay.shp.3@data$paf*100000)/cbg.bay.shp.3@data$pop,2)


cbg.bay.shp.3@data <- cbg.bay.shp.3@data %>% mutate(id = row.names(.))
shp_df <- fortify(cbg.bay.shp.3, region = "id")
shp_df <- shp_df %>% left_join(cbg.bay.shp.3@data, by = c("id"="id"))
shp_df <- as.data.frame(shp_df)
shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA


shp <- cbg.bay.shp.3
#===============================================================================================
# Create a univariate chloropeth of attributable cases per 100,000

no_classes <- 6

# extract quantiles
quantiles <- shp_df %>%
  pull(rate) %>%
  quantile(probs = seq(0, 1, length.out = no_classes + 1),na.rm=TRUE) %>%
  as.vector() # to remove names of quantiles, so idx below is numeric

# here we create custom labels
for (i in 1:length(quantiles)){
  labels[i] <- paste0(round(quantiles[i],0)," - ",round(quantiles[i+1],0))
}

# we need to remove the last label 
# because that would be something like "478k - NA"
labels <- labels[1:length(labels) - 1]

# here we actually create a new 
# variable on the dataset with the quantiles
shp_df %<>%
  mutate(mean_quantiles = cut(rate,
                              breaks = quantiles,
                              labels = labels,
                              include.lowest = T))

colnames(relief)[1] <- 'value'

ggplot(
  # define main data source
  data = shp_df
) +
  # use the "alpha hack" (as the "fill" aesthetic is already taken)
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # add main fill aesthetic
  # use thin white stroke for municipality borders
  geom_polygon(
    mapping = aes(x = long, y = lat, group = group,
      fill = mean_quantiles
    ),
    color = "white",
    size = 0.1
  ) +
  # use the Viridis color scale
  scale_fill_viridis(
    option = "magma",
    name = "Attributable cases per 100,000",
    alpha = 0.8, # make fill a bit brighter
    begin = 0.1, # this option seems to be new (compared to 2016):
    # with this we can truncate the
    # color scale, so that extreme colors (very dark and very bright) are not
    # used, which makes the map a bit more aesthetic
    end = 0.9,
    discrete = T, # discrete classes, thus guide_legend instead of _colorbar
    direction = 1, # dark is lowest, yellow is highest
    guide = guide_legend(
      keyheight = unit(5, units = "mm"),
      title.position = "top",
      reverse = T # display highest income on top
    )) +
  # use thicker white stroke for cantonal borders
  geom_polygon(data=bay.co.shp.f,
    mapping = aes(x = long, y = lat, group = group),
    fill = "transparent",
    color = "white",
    size = 0.5
  ) +
  # draw lakes in light blue
  #geom_sf(
  #  data = lake_geo,
  #  fill = "#D6F1FF",
  #  color = "transparent"
  #) +
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Bay area nitrogen dioxide\nattributable deaths per 100,000")+
  # add theme
  theme_map()

#===============================================================================================
# Create a bivariate chloropeth of race percentages and attributable cases per 100,000


# create 3 buckets for gini
quantiles_race <- shp_df %>%
  pull(per.minor) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)

# create 3 buckets for mean income
quantiles_paf <- shp_df %>%
  pull(rate) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high race minority, high paf
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low race minority, high paf
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium race minority, medium paf
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high race minority, low paf
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low race minority, low paf
) %>%
  gather("group2", "fill")


# cut into groups defined above and join fill
shp_df3 <- shp_df %<>%
  mutate(
    quantiles_race = cut(
      per.minor,
      breaks = quantiles_race,
      include.lowest = TRUE
    ),
    quantiles_paf= cut(
      rate,
      breaks = quantiles_paf,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group2 = paste(
      as.numeric(quantiles_race), "-",
      as.numeric(quantiles_paf)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group2")



annotations <- tibble(
  label = c(
    "Grey areas mean\nlow percentage racial minorities and\nlow attributable cases per 100,000",
    "Blue areas mean\nhigh percentage racial minorities and\nlow attributable cases per 100,000",
    "Violet areas mean\nhigh percentage racial minorities and\nhigh attributable cases per 100,000",
    "Red areas mean\nlow percentage racial minorities and\nhigh attributable cases per 100,000"
  ),
  arrow_from = c(
    "-121.5,38.5", # grey
    "-123,37.5", # blue
    "-121.25,37.75", # violet
    "-122.5,37" # red
  ),
  arrow_to = c(
    "-123,38.5", # grey
    "-122.4,37.8", # blue
    "-122.25,37.75", # violet
    "-122.125,37.25" # red
  ),
  curvature = c(
    0.2, # grey
    0.1, # blue
    -0.1, # violet
    -0.2 # red
  ),
  nudge = c(
    "-0.125,0", # grey
    "0.125,0.25", # blue
    "0,-0.25", # violet
    "0.125,0" # red
  ),
  just = c(
    "1,0", # grey
    "0,1", # blue
    "0.5,1", # violet
    "0,1" # red
  )
) %>%
  separate(arrow_from, into = c("x", "y"),sep=',') %>%
  separate(arrow_to, into = c("xend", "yend"),sep=',') %>%
  separate(nudge, into = c("nudge_x", "nudge_y"), sep = "\\,") %>%
  separate(just, into = c("hjust", "vjust"), sep = "\\,")




map <- ggplot(
  data = shp_df
) +
  # use the "alpha hack" (as the "fill" aesthetic is already taken)
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # add main fill aesthetic
  # use thin white stroke for municipality borders
  geom_polygon(
    mapping = aes(x = long, y = lat, group=group,fill = fill),
    color = "white",
    size = 0.05
  ) +
  # as the sf object municipality_prod_geo has a column with name "fill" that
  # contains the literal color as hex code for each municipality, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # use thicker white stroke for cantons
  geom_polygon(data=bay.co.shp.f,
               mapping = aes(x = long, y = lat, group = group),
               fill = "transparent",
               color = "white",
               size = 0.5
  ) +
  # draw lakes in light blue
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "Bay area nitrogen dioxide\nattributable deaths per 100,000",
       caption = 'Census block group racial & ethnic percentages derived from the 2010 Census.\nSource: www.census.gov') +
  # add the theme
  theme_map()

bivariate_color_scale %<>%
  separate(group2, into = c("per.minor", "rate"), sep = " - ") %>%
  mutate(gini = as.integer(per.minor),
         mean = as.integer(rate))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = rate,
      y = per.minor,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher attributable\ncases per 100,000 ??????",
       y = "Higher percentage\nof minorites ??????") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6)
  ) +
  # quadratic tiles
  coord_fixed()

# add annotations one by one by walking over the annotations data frame
# this is necessary because we cannot define nudge_x, nudge_y and curvature
# in the aes in a data-driven way like as with x and y, for example
annotations %>%
  pwalk(function(...) {
    # collect all values in the row in a one-rowed data frame
    current <- tibble(...)
    
    # convert all columns from x to vjust to numeric
    # as pwalk apparently turns everything into a character (why???)
    current %<>%
      mutate_at(vars(x:vjust), as.numeric)
    
    # update the plot object with global assignment
    map <<- map +
      # for each annotation, add an arrow
      geom_curve(
        data = current,
        aes(
          x = x,
          xend = xend,
          y = y,
          yend = yend
        ),
        # that's the whole point of doing this loop:
        curvature = current %>% pull(curvature),
        size = 0.2,
        arrow = arrow(
          length = unit(0.005, "npc")
        )
      ) +
      # for each annotation, add a label
      geom_text(
        data = current,
        aes(
          x = x,
          y = y,
          label = label,
          hjust = hjust,
          vjust = vjust
        ),
        # that's the whole point of doing this loop:
        nudge_x = current %>% pull(nudge_x),
        nudge_y = current %>% pull(nudge_y),
        # other styles
        family = default_font_family,
        color = default_font_color,
        size = 3
      )
  })

ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.075, 0.2, 0.2)
ggsave('Disparities map Bay area.png',dpi=320)

#/////////////////////////////////////////////////////////////////////////////////////////////////
o.cbg.shp.2 <- spTransform(o.cbg.shp, crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))
o.shp <- crop(cbg.bay.shp.3,o.cbg.shp.2)

o.shp@data <- o.shp@data %>% mutate(id = row.names(.))
shp_df <- fortify(o.shp, region = "id")
shp_df <- shp_df %>% left_join(o.shp@data, by = c("id"="id"))
shp_df <- as.data.frame(shp_df)
shp_df[,ncol(shp_df)][shp_df[,ncol(shp_df)] == 0] <- NA

#===============================================================================================
# Create a bivariate chloropeth of race percentages and attributable cases per 100,000


# create 3 buckets for gini
quantiles_race <- shp_df %>%
  pull(per.minor) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)

# create 3 buckets for mean income
quantiles_paf <- shp_df %>%
  pull(rate) %>%
  quantile(probs = seq(0, 1, length.out = 4),na.rm=TRUE)

# create color scale that encodes two variables
# red for gini and blue for mean income
# the special notation with gather is due to readibility reasons
bivariate_color_scale <- tibble(
  "3 - 3" = "#3F2949", # high race minority, high paf
  "2 - 3" = "#435786",
  "1 - 3" = "#4885C1", # low race minority, high paf
  "3 - 2" = "#77324C",
  "2 - 2" = "#806A8A", # medium race minority, medium paf
  "1 - 2" = "#89A1C8",
  "3 - 1" = "#AE3A4E", # high race minority, low paf
  "2 - 1" = "#BC7C8F",
  "1 - 1" = "#CABED0" # low race minority, low paf
) %>%
  gather("group2", "fill")


# cut into groups defined above and join fill
shp_df3 <- shp_df %<>%
  mutate(
    quantiles_race = cut(
      per.minor,
      breaks = quantiles_race,
      include.lowest = TRUE
    ),
    quantiles_paf= cut(
      rate,
      breaks = quantiles_paf,
      include.lowest = TRUE
    ),
    # by pasting the factors together as numbers we match the groups defined
    # in the tibble bivariate_color_scale
    group2 = paste(
      as.numeric(quantiles_race), "-",
      as.numeric(quantiles_paf)
    )
  ) %>%
  # we now join the actual hex values per "group"
  # so each municipality knows its hex value based on the his gini and avg
  # income value
  left_join(bivariate_color_scale, by = "group2")

map <- openmap(c(37.71583,-122.3275), c(37.8325,-122.1475),
               type = "esri-topo",
               mergeTiles = TRUE)
map.latlon <- openproj(map, projection = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")


map <- autoplot(map.latlon)  +
  # add main fill aesthetic
  # use thin white stroke for municipality borders
  geom_polygon(data = shp_df,
    mapping = aes(x = long, y = lat, group=group,fill = fill),
    color = "white",
    size = 0.3
  ) +
  # use the "alpha hack" (as the "fill" aesthetic is already taken)
  scale_alpha(name = "",
              range = c(0.6, 0),
              guide = F) + # suppress legend
  # as the sf object municipality_prod_geo has a column with name "fill" that
  # contains the literal color as hex code for each municipality, we can use
  # scale_fill_identity here
  scale_fill_identity() +
  # use thicker white stroke for cantons
  # add titles
  labs(x = NULL,
       y = NULL,
       title = "West & Downtown Oakland nitrogen dioxide\nattributable deaths per 100,000",
       caption = 'Census block group racial & ethnic percentages derived from the 2010 Census.\nSource: www.census.gov') +
  # add the theme
  theme_map()

bivariate_color_scale %<>%
  separate(group2, into = c("per.minor", "rate"), sep = " - ") %>%
  mutate(gini = as.integer(per.minor),
         mean = as.integer(rate))

legend <- ggplot() +
  geom_tile(
    data = bivariate_color_scale,
    mapping = aes(
      x = rate,
      y = per.minor,
      fill = fill)
  ) +
  scale_fill_identity() +
  labs(x = "Higher attributable\ncases per 100,000 ??????",
       y = "Higher percentage\nof minorites ??????") +
  theme_map() +
  # make font small enough
  theme(
    axis.title = element_text(size = 6),
    legend.background=element_blank(),
    panel.background=element_blank(),
    plot.background=element_blank()
  ) +
  # quadratic tiles
  coord_fixed()


ggdraw() +
  draw_plot(map, 0, 0, 1, 1) +
  draw_plot(legend, 0.05, 0.2, 0.2, 0.2)
ggsave('Oakland Disparities map Bay area.png',dpi=320)