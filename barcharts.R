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



#=======================================================================================
# Top 25 cities cities by region - Attributable rate per 100,000
#=======================================================================================
df$city_type <- as.character(df$city_type)
df <- subset(df, city_type %in% 'GHS')
pw <- df
regions <- as.vector(unique(pw$WHORegion))
years <- as.vector(unique(pw$year))

setwd("E:/GBD_2020/baseline_mort_2019/city_results/graphs/TS/top25/")

for (i in 1:length(regions)){
  
  df$city_type <- as.character(df$city_type)
  pw <- df
  
  pw <- pw[,c(1:5,11,18)]
  pw2 <- subset(pw, WHORegion %in% regions[i])
  
  new3 <- subset(pw2, year %in% 2019)
  new4 <- new3[order(-new3$pop.sum),]
  new5 <- new4[c(1:25),]
  ids <- as.vector(unique(new5$id))
  
  pw4 <- subset(pw2, id %in% ids)
  pw5 <- pw4[,c(1,5,7)]
  long_DF2 <- pw5 %>% spread(year,acrate.nohap)
  
  pw <- long_DF2
  
  pw$'pc.1990'<- ((pw$'1990'-pw$'1990')/pw$'1990')*100
  pw$'pc.2000'<- ((pw$'2000'-pw$'1990')/pw$'1990')*100
  pw$'pc.2010' <- ((pw$'2010'-pw$'1990')/pw$'1990')*100
  pw$'pc.2015' <- ((pw$'2015'-pw$'1990')/pw$'1990')*100
  pw$'pc.2019' <- ((pw$'2019'-pw$'1990')/pw$'1990')*100
  
  long_DF2 <- pw %>% gather(Analysis, Val, "pc.1990":"pc.2019")
  write.csv(long_DF2,paste0('pc_',regions[i],'.csv'),sep='')
  pw <- long_DF2
  
  pw <- subset(pw, Analysis %in% 'pc.2019')
  
  pw3 <- pw %>% gather(year,acrate.nohap,"1990":"2019")
  pw3$Val <- paste0(round(pw3$Val,0),'%')
  pw3$year <- as.numeric(pw3$year)
  
  pw3$Val[pw3$year == 1990] <- NA
  pw3$Val[pw3$year == 2000] <- NA
  pw3$Val[pw3$year == 2010] <- NA
  pw3$Val[pw3$year == 2015] <- NA
  
  pw3$city2 <- pw3$city
  
  pm.plot2 <- ggplot(pw3, aes(year, acrate.nohap, group = city)) +
    facet_wrap(~city2)+
    stat_smooth(data=pw3[, c("year", "acrate.nohap", "city")], method = 'loess', formula = y ~ x, colour = 'grey', size=0.35,alpha=0.5,se= FALSE)+
    stat_smooth(method = 'loess', formula = y ~ x, aes(colour = city2), size=1.25,se= FALSE)+
    scale_x_continuous(breaks=c(1990,2000,2010,2019))+
    geom_text(aes(label = Val),x=2016,y=20,size=3,color='grey25') +
    theme_bw()+
    labs(x = "Year", y = 'Attributable mortality per 100,000',
         caption='Top 25 most populated cities.',
         title=bquote(~PM[2.5]~'attributable mortality: 1990-2019'),
         subtitle=paste0(regions[i]),sep='')+
    theme(legend.position = "none",
          plot.title=element_text(hjust = 0, size=16),
          plot.subtitle=element_text(hjust = 0, size=12),
          plot.caption=element_text(hjust = 0, size=9),
          axis.text.x = element_text(hjust = 0, size=9,angle=-55),
          axis.text.y = element_text(hjust = 0, size=11),
          axis.title=element_text(size=11),
          legend.text=element_text(size=11),
          strip.text=element_text(hjust=0.5,size=11),
          legend.title = element_blank())
  
  ggsave(paste(regions[i],'_top25_Per_Change_PM2.5.pdf'), width=11, height=8)
}
