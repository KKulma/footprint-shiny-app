library(tidyverse)
library(footprint)
library(extrafont)
library(ggtext)
loadfonts()
theme_set(theme(text = element_text(family="Open Sans")))

#estimate trip
trip <- airport_footprint("LAX", "LHR", "Economy", "co2e")/1000

#make df
personal_footprint <- tribble(~year, ~upper, ~lower, ~pct, ~trip,
           2030, 3.2, 2.5, trip/3.2, trip,
           2040, 2.2, 1.4, trip/2.2, trip, 
           2050, 1.5, 0.7,  trip/1.5, trip)

#graphic
personal_footprint %>%
  ggplot()+
  geom_col(aes(x=year, y=upper, fill="Carbon Budget"))+
  geom_col(aes(x=year, y=trip, fill="Flight Emissions"), width=7)+
  geom_linerange(aes(xmin=2025.5, xmax=2034.5, y=3.2), size=1.5)+
  geom_linerange(aes(xmin=2025.5, xmax=2034.5, y=2.5), size=1, linetype="dashed")+
  geom_linerange(aes(xmin=2035.5, xmax=2044.5, y=2.2), size=1.5)+
  geom_linerange(aes(xmin=2035.5, xmax=2044.5, y=1.4), size=1, linetype="dashed")+
  geom_linerange(aes(xmin=2045.5, xmax=2054.5, y=1.5), size=1.5)+
  geom_linerange(aes(xmin=2045.5, xmax=2054.5, y=.7), size=1, linetype="dashed")+
  geom_text(aes(x=2030, y=3.2, label="Upper Limit"), 
            hjust=1.2, vjust=1.2, family="Open Sans",)+
  geom_text(aes(x=2030, y=2.5, label="Lower Limit"), 
            hjust=1.2, vjust=1.2, family="Open Sans",)+
  scale_fill_manual(values=c("grey80", "#4587a1"))+
  ylab("Metric Tons of CO2e")+
  xlab("Year")+
  labs(fill="")+
  ggtitle("Your Flight Compared to Your Personal Carbon Budget\n")+
  geom_richtext(aes(x=year, y=trip, 
            label=paste0("<span style='font-weight:bold;font-size:20px'>",scales::percent(pct,1),"</span>", "<br> of your \ncarbon budget")),
            family="Open Sans",
            color="white",
            vjust=1.2,
            fill = NA, 
            label.color = NA, # remove background and outline
            label.padding = grid::unit(rep(0, 4), "pt"))+ # remove padding)
  theme(panel.background = element_blank(),
        axis.line = element_line(size = .5, colour = "black"),
        axis.text.x = element_text(face="bold"),
        legend.position='top', 
        legend.justification='left',
        legend.direction='horizontal',
        plot.title = element_text(family="Open Sans",
                                  size=18, hjust = .5))


# trees ----

trees <- trip/.06

waffle::waffle(
  trees, rows = round(parts/10), size = 1,
  colors=c("#297c37", "white"),
  legend_pos = "none")+
  ggtitle(paste0("Trees Required to be Planted to Offset Trip: ", "<b>", 
               round(trees), "</b>"))+
  theme(plot.title = element_markdown(family="Open Sans",
                            size=15))

# arctic ice

ice <- trip*3
