records <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/records.csv')
drivers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-05-25/drivers.csv')

library(tidyverse)
library(ggimage)
library(viridis)
library(extrafont)

loadfonts()

#where is the line between analysis and visualizations lol

drivers <- drivers %>% drop_na()

#prep data
df <- drivers %>%  
  count(year) %>% 
  rowwise() %>% 
  mutate(
    w = round(runif(1, 0, 3)),
    h = runif(1, 0, 3)) %>% 
  ungroup()

#starting line
starting_line <- data.frame(
  fyear = df$year
) %>%
  mutate(
    r = row_number(),
    c = list(c(1:1))
  ) %>% 
  unnest(c(c, r)) %>% 
  rowwise %>% 
  mutate(
    x = list(c + c(0, 0.9, 0.9, 0)),
    y = list(c * 0.5 + c(fyear - 0.45, fyear, fyear + 0.9, fyear + 0.45))
  ) %>% 
  unnest(c(x, y))

#track:
ggplot(df) + 
  annotate("polygon", x = c(-20, 15, 10, -20), y = c(1997, 1997, 2021, 2021), fill = "grey30", color = "grey30") + 
  annotate("polygon", x = c(15, 22.5, 22.5, 10), y = c(1997, 1997, 2021, 2021), fill = "grey30", color = "grey30") + 
  annotate("polygon", x = c(-20, 15, 10, -20), y = c(2020.3, 2020.3, 2030, 2030), fill = "palegreen3", color = "palegreen3") + 
  annotate("polygon", x = c(15, 22.5, 22.5, 10), y = c(2020.3, 2020.3, 2030, 2030), fill = "palegreen3", color = "palegreen3") + 
  annotate("line", x = c(-15,-17), y=2010, colour="white", size=2) + 
  annotate("line", x = c(-10,-12), y=2010, colour="white", size=2) + 
  annotate("line", x = c(-8,-6), y=2010, colour="white", size=2) +   
  annotate("line", x = c(-5,-3), y=2010, colour="white", size=2) + 
  annotate("line", x = c(-2,0), y=2010, colour="white", size=2) + 
  annotate("line", x = c(2,4), y=2010, colour="white", size=2) +
  annotate("line", x = c(6,8), y=2010, colour="white", size=2) + 
  annotate("line", x = c(10,12), y=2010, colour="white", size=2) + 
  annotate("line", x = c(14,16), y=2010, colour="white", size=2) +
  annotate("line", x = c(18,20), y=2010, colour="white", size=2) + 
  geom_polygon(data = starting_line, aes(x = x - 21, y = y - 1.9, group = interaction(c, r)), fill = "white", color = "black", size= 1.5) +  
  annotate("polygon", x = c(-20, 15, 10, -20), y = c(1987.3, 1987.3, 1997, 1997), fill = "palegreen3", color = "palegreen3") + 
  annotate("polygon", x = c(15, 22.5, 22.5, 10), y = c(1987.3, 1987.3, 1997, 1997), fill = "palegreen3", color = "palegreen3") +  
  geom_curve(aes(x = -15 - w, y = year - w * 0.4, xend = n, yend = 2001 + h, color = year, size = n), curvature = 0) +   
  scale_size_continuous(range = c(0.1, 3)) +  scale_color_viridis_c(option = "inferno")  + 
  annotate("polygon", x = c(-19, -15, -12, -16), y = c(2028, 2028, 2029, 2029), fill = "yellow1", color = "grey20") +
  annotate("polygon", x = c(-19, -15, -15, -19), y = c(2022, 2022, 2028, 2028), fill = "yellow3", color = "grey20") + 
  annotate("polygon", x = c(-15, -12, -12, -15), y = c(2022, 2024, 2029, 2028), fill = "yellow4", color = "grey20") + 
  annotate("text", x = -16, y = 2026, hjust = 1, vjust = 1, label = "?", size = 15, color = "white", family= "Impact") +  
  annotate("text", x = -13, y = 2027, hjust = 1, vjust = 1, label = "?", size = 15, color = "seashell3", family= "Impact") +
  annotate("polygon", x = c(13, 17, 20, 16), y = c(2028, 2028, 2029, 2029), fill = "blue1", color = "grey20") +
  annotate("polygon", x = c(13, 17, 17, 13), y = c(2022, 2022, 2028, 2028), fill = "blue3", color = "grey20") + 
  annotate("polygon", x = c(17, 20, 20, 17), y = c(2022, 2024, 2029, 2028), fill = "blue4", color = "grey20") +
  annotate("polygon", x = c(-19, -15, -12, -16), y = c(1995, 1995, 1996, 1996), fill = "red1", color = "grey20") + 
  annotate("polygon", x = c(-19, -15, -15, -19), y = c(1989, 1989, 1995, 1995), fill = "red3", color = "grey20") +
  annotate("polygon", x = c(-15, -12, -12, -15), y = c(1989, 1991, 1996, 1995), fill = "red4", color = "grey20") +
  annotate("polygon", x = c(13, 17, 20, 16), y = c(1995, 1995, 1996, 1996), fill = "green1", color = "grey20") + 
  annotate("polygon", x = c(13, 17, 17, 13), y = c(1989, 1989, 1995, 1995), fill = "green3", color = "grey20") +
  annotate("polygon", x = c(17, 20, 20, 17), y = c(1989, 1991, 1996, 1995), fill = "green4", color = "grey20") + 
  annotate("text", x = -16, y = 1993, hjust = 1, vjust = 1, label = "?", size = 15, color = "white", family= "Impact") + 
  annotate("text", x = -13, y = 1994, hjust = 1, vjust = 1, label = "?", size = 15, color = "seashell3", family= "Impact") +
  annotate("text", x = 16, y = 1993, hjust = 1, vjust = 1, label = "?", size = 15, color = "white", family= "Impact") + 
  annotate("text", x = 19, y = 1994, hjust = 1, vjust = 1, label = "?", size = 15, color = "seashell3", family= "Impact") +
  annotate("text", x = 16, y = 2026, hjust = 1, vjust = 1, label = "?", size = 15, color = "white", family= "Impact") + 
  annotate("text", x = 19, y = 2027, hjust = 1, vjust = 1, label = "?", size = 15, color = "seashell3", family= "Impact") + 
  annotate("text", x = 10, y = 2025, hjust = 1, vjust = 1, label = "Number of records by year", size = 10, family= "Comic Sans MS",color = "white") + 
  annotate("text", x = 17.5, y = 2022, hjust = 1, vjust = 1, label = "Interesting observation:overlap in number of records obtained between earliest and later years", size= 5, family= "Comic Sans MS",color = "black") + 
  annotate("text", x = 17.5, y = 2019, hjust = 1, vjust = 1, label = "Source: Mario Kart World Records, Graphics: Thivina Thanabalan", size= 5, family= "Comic Sans MS",color = "grey48") + 
  
  scale_x_continuous(breaks = 0:20, labels = ifelse(0:20 %% 5 == 0, 0:20, ""), limits = c(-20, 23)) + 
  theme_void() + 
  theme(legend.position = "none",axis.ticks.y = element_line(color = "yellow"), 
        axis.ticks.length.y = unit(0.5, "line", "lemonchiffon4"), 
        axis.text.y= element_text(colour="lemonchiffon4"),  
        axis.ticks.x = element_line(color = "lemonchiffon4"), 
        axis.ticks.length.x = unit(0.5, "line", "lemonchiffon4"), 
        axis.text.x = element_text(colour="lemonchiffon4")) + 
  theme(plot.background = element_rect(fill = "lemonchiffon", colour=NA)) +   
  geom_image(aes(x = n, y = 2002 + h - 0.5, image = "https://www.pngkey.com/png/full/1007-10074038_bananas-transparent-mario-kart.png"), size = 0.035, by = "height") + 
  geom_image(aes(x = 1, y = 2027, image = "https://www.pngkey.com/png/full/140-1403727_super-mario-kart-png-file-mario-kart-super.png"), size = 0.1, by = "height") + 
  ggsave(path = "TidyTuesday05-25", filename = "track.png")

