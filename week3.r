library(tidyverse)
library(readxl)
library(cartogram)    # for the cartogram
library(ggplot2)      # to realize the plots
library(broom)        # from geospatial format to data frame
library(tweenr)       # to create transition dataframe between 2 states
library(gganimate)    # To realize the animation
library(maptools)     # world boundaries coordinates
library(viridis)      # for a nice color palette
library(gridExtra)

data <- read_xlsx("data/global_mortality.xlsx")

# Get the shape file of Africa
data(wrld_simpl)
afr <- wrld_simpl[wrld_simpl$REGION==2,]

# list of African country codes
afr_codes <- afr@data %>% rownames()
afr@data$country_code <- afr_codes

# filter data for African countries and latest year
afr_data <- data %>% filter(country_code %in% afr_codes)
afr_data_16 <- afr_data %>% filter(year=="2016")

# join data
afr@data <- afr@data %>% left_join(afr_data_16)
rownames(afr@data) <- afr@data$country_code # NB! missing this out will break things as the join reindexes

# construct a cartogram using HIV/AIDS % of all deaths
afr_cartogram <- cartogram(afr, "HIV/AIDS (%)")

# Transform these 2 objects in dataframe, plotable with ggplot2
afr_df <- tidy(afr) %>% left_join(. , afr@data, by=c("id"="ISO3"))
afr_cartogram_df <- tidy(afr_cartogram) %>% left_join(. , afr_cartogram@data, by=c("id"="ISO3")) 

# plot
plot_map <-function(df) {
  plot <- ggplot() +
  geom_polygon(data = df, aes(fill = `HIV/AIDS (%)`, x = long, y = lat, group = group) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(name="HIV/AIDS (% of all deaths)",
                     breaks=c(1,5,10,15,20,25,30,35,40,45,50),
                     guide = guide_legend( keyheight = unit(3, units = "mm"),
                                           keywidth=unit(10, units = "mm"),
                                           label.position = "bottom", 
                                            title.position = 'top', nrow=1)) +
  ylim(-35,35) +
  theme(
    text = element_text(color = "#22211d"), 
    plot.background = element_rect(fill = "#f5f5f4", color = NA), 
    panel.background = element_rect(fill = "#f5f5f4", color = NA), 
    legend.background = element_rect(fill = "#f5f5f4", color = NA),
    plot.title = element_text(size= 10, hjust=0.5, color = "#4e4d47"),
    legend.position = c(0.0, 0.0)
  ) +
  coord_map()
  
  return(plot)
}

p1 <- plot_map(afr_df)+
  labs(title="Percentage of deaths in Africa due to HIV/AIDS (2016)") +
  guides(fill=FALSE)
p2 <- plot_map(afr_cartogram_df)

p <- grid.arrange(p1,p2, ncol=2)
ggsave("img/week3.png", plot=p, width=20, height=10, units="cm")
