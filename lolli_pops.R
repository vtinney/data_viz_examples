library(dplyr)
library(ggplot2)
library(ggspatial)
library(tidyverse)
library(cowplot)
library(extrafont)
library(gganimate)
library(tidyverse)
library(egg)
library(rcartocolor)
library(LaCroixColoR)
library(patchwork)
library(tidyverse)
library(ggtext)
library(showtext)
library(patchwork)

setwd('E:/GBD_2020/baseline_mort_2019/city_results/')

x1 <- read.csv("combined.city.paf.csv")  
x2 <- read.csv("all.ages.stroke.cvd.csv")
colnames(x2)[2] <- 'id'

df <- rbind(x1,x2)

df$cause_name <- as.character(df$cause_name)

write.csv(df, 'allcauses.city.results.csv')

ls <- c("Bangkok","Beijing","Buenos Aires","Cuidad de Mexico","Delhi NCT", 
        "Dhaka","Guangzhou","Ho Chi Minh City","Istanbul","Jakarta","Karachi","Kolkata","Lagos",
        "Los Angeles","Moscow","New York City","Sao Paulo","Seoul","Shanghai","Tokyo","Jaipur","Warsaw",
        "Houston","Dar es Salaam","Amsterdam")

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

color_lty_cross = expand.grid(
  ltypes = 1:6,
  colors = gg_color_hue(5)
  ,stringsAsFactors = F)

unique(df$"WHORegion")

df$"WHORegion" <- as.character(df$"WHORegion")
df$"WHORegion"[df$"WHORegion" == 'WPRO'] <- 'Western Pacific'
df$"WHORegion"[df$"WHORegion" == 'SEARO'] <- 'South-East Asia'
df$"WHORegion"[df$"WHORegion" == 'EURO'] <- 'Europe'
df$"WHORegion"[df$"WHORegion" == 'AMRO'] <- 'Americas'
df$"WHORegion"[df$"WHORegion" == 'AFRO'] <- 'Africa'
df$"WHORegion"[df$"WHORegion" == 'EMRO'] <- 'Eastern Mediterranean'

df$acrate.hap <- as.numeric(df$acrate.hap)
df$acrate.nohap <- as.numeric(df$acrate.nohap)
df$acrate.who <- as.numeric(df$acrate.who)
df$diff <- as.numeric(df$diff)
df$per.diff <- as.numeric(df$per.diff)


all.nohap <- df %>% 
  group_by(year,id,city_type,pop.sum,popw,city,cluster,CountryName,GBDRegion,         
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion) %>%
  summarize(ac.nohap = sum(ac.nohap, na.rm = T))
all.nohap <- as.data.frame(all.nohap)

all.hap <- df %>% 
  group_by(year,id,city_type,pop.sum,popw,city,cluster,CountryName,GBDRegion,         
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion) %>%
  summarize(ac.hap = sum(ac.hap, na.rm = T))
all.hap <- as.data.frame(all.hap)

all.who <- df %>% 
  group_by(year,id,city_type,pop.sum,popw,city,cluster,CountryName,GBDRegion,         
           GBDSuperRegion,SDGRegion,WHORegion,WHOIncomeRegion) %>%
  summarize(ac.who = sum(ac.who, na.rm = T))
all.who <- as.data.frame(all.who)

df4 <- merge(all.nohap, all.hap, by=c('year', 'id', 'pop.sum', 'popw', 'city', 'cluster', 'CountryName', 'GBDRegion', 'GBDSuperRegion', 'SDGRegion','WHORegion', 'WHOIncomeRegion', 'city_type'))
df5 <- merge(df4, all.who, by=c('year', 'id', 'pop.sum', 'popw', 'city', 'cluster', 'CountryName', 'GBDRegion', 'GBDSuperRegion', 'SDGRegion','WHORegion', 'WHOIncomeRegion', 'city_type'))

write.csv(df5, 'allcause.results.csv')

all.years <- df5 %>% 
  group_by(year,city_type) %>%
  summarize(sum = sum(ac.nohap, na.rm = T))
all.years <- as.data.frame(all.years)
write.csv(all.years, 'all.years.csv')

df <- read.csv('allcause.results.csv')
df <- subset(df, city_type %in% 'GHS')

#=======================================================================
# Cool graphs
#========================================================================
font_add_google("Roboto Mono", "Roboto Mono")
font_add_google("Oswald", "Oswald")
showtext_auto()

setwd("E:/GBD_2020/baseline_mort_2019/city_results/")
df <- read.csv('allcause.results.csv')

pw <- df

pw <- pw[complete.cases(pw), ]

r <- as.vector(unique(pw$year))

setwd("E:/GBD_2020/baseline_mort_2019/city_results/graphs/")

plot_list <- list()

for(i in 1:length(r)){
  pw2 <- subset(pw, year %in% r[i])
  
  new.new <- pw2[order(-pw2$pop.sum),]
  
  df6 <- new.new 
  
  world_avg <- df6 %>% 
    summarize(avg = mean(acrate.nohap, na.rm = T)) %>% 
    pull(avg)
  
  print(paste(world_avg,',',r[i],sep=''))
  
  colors <- c("#c9007b","#ff2f00","#fe6680","#00b6b0","#6c6b9f","#16266b")
  
  lolli_country <- df6 %>% 
    group_by(WHORegion) %>% 
    mutate(cont = mean(acrate.nohap, na.rm = T)) %>% 
    ungroup() %>% 
    mutate(
      region = fct_reorder(WHORegion, -cont), 
      region_col = fct_reorder(WHORegion, acrate.nohap)
    ) %>% 
    ggplot(aes(cont, WHORegion)) + 
    geom_vline(aes(xintercept = world_avg), color = "grey85") +
    geom_jitter(aes(x = acrate.nohap, y = WHORegion, fill = region_col), color = "grey85", 
                width = 0, height = 0.2, size = 2.5, alpha = 0.3, shape = 21) +
    geom_jitter(data=filter(df6, city_type == 'c40'), aes(x = acrate.nohap, y = WHORegion), color = "grey85", fill='#e9e5a3',
                width = 0, height = 0.2, size = 2.5, alpha = 0.8, shape = 21) +
    geom_segment(aes(x = world_avg, xend = cont, y = WHORegion, yend = WHORegion), 
                 color = "grey85", size = 0.7) +
    geom_point(color = "grey85", size = 6) + 
    geom_point(aes(color = region_col), size = 4.5) + 
    scale_x_continuous(limits = c(1, 100), breaks = c(1, seq(10, 80, by = 10))) +
    scale_fill_manual(values = colors, guide = F) +
    scale_color_manual(values = colors, guide = F) +
    labs(x=NULL, y = NULL)+
    dark_theme_gray()+
    theme(axis.title.x = element_text(size = 10, face = "plain"),
          axis.text.x = element_text(size = 9, family = "Roboto Mono"),
          axis.text.y = element_text(size = 10, family = "Roboto Mono"),
          plot.subtitle = element_text(size = 10, family = "Roboto Mono"),
          panel.grid.major = element_blank(), 
          panel.grid.minor = element_blank(),
          panel.background = element_rect(fill='black'))
  
  plot_list[[i]] <- lolli_country
  ggsave(paste0('comparison_',r[i],'chart.pdf',sep=''),height=5,width=7, device = cairo_pdf)
}
