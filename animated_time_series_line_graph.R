# Part 2 - create a line graph of max concentrations that matches the animated
# Raster files

library(ggplot2)
library(dplyr)
library(gifski)
library(gganimate)
library(purrr)
library(animation)
library(ggdark)

setwd('C:/Users/Veronica Tinney/Google Drive/EDF_shared/Visualization/visuals/')
list.files()

# Read in a file that has the date and min and max concentration for each day
df <- read.csv("min.max.2016.pm.csv")

df$dd <- seq(as.Date("2016-1-1"), as.Date("2016-12-31"), by = "1 day")


# Create a line graph that is the same speed as the animated raster map
p <- ggplot(data = df,
            mapping = aes(dd, Max,
                          color = `Month`,
                          Month)) +
  geom_line(size = 1, color = "gray") +
  geom_point(aes(group=seq_along(dd))) +
  # NAAQS line
  geom_hline(yintercept=35, linetype="dashed", color = "white")+
  # basic styling and font
  theme_minimal(base_family = "Carlito",
                base_size = 16)+
  # labels for horizontal lines
  annotate(geom = "text",
           y = 33,
           x=as.Date("2016-12-10"),
           label = c("NAAQS daily\n35.0 µg/m^3"),
           vjust = -1,
           fontface = 2,
           size = 4,
           color='white') +
  labs(y='Daily max concentration µg/m^3',x='Date')+
  theme(plot.title=element_text(hjust = 0, size=14),
        axis.title=element_text(size=12)) +
  theme(legend.position = "none")+
  coord_cartesian(clip = 'off')+
  transition_reveal(dd)+
dark_theme_gray(base_family = "Fira Sans Condensed Light", base_size = 14) + 
  theme(plot.title = element_text(family = "Fira Sans Condensed"),
        plot.background = element_rect(fill = "grey10"),
        panel.background = element_blank(),
        panel.grid.major = element_line(color = "grey30", size = 0.2),
        panel.grid.minor = element_line(color = "grey30", size = 0.2),
        legend.background = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none")

# Animate and save to a gif
z <- animate(p, nframes = 366, duration = 183, fps = 2)

anim_save("timeseries.dark2.gif", z)

