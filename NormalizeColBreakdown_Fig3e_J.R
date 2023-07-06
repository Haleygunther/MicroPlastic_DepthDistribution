library(ggplot2)
library(scales)
library(reshape2)
library(gridExtra)
library(grid)
library(plyr)
library(dplyr)
library(png)
library(grid)
library(tidyverse)
library(reprex)
library(ggpubr)
library(cowplot)
library(forcats)
library(cowplot)
library(ggrepel)
library(ggpmisc)
library(wesanderson)
library(Hmisc)
library(Rmisc) 
library(googlesheets4)
library(googledrive)
library(RColorBrewer)


#################################### Original style of graph with 3 groups of 2 columns. Includes Jamies work to calculate statistics


data <- data.frame(values = c(93.3014,73.0361,254.322,172.374,346.571,349.427),  # Create example data
                   Min = rep(c("15", "30","60"),each = 2),
                   Depth = LETTERS[1:2])#c(1,2))

data                                              # Print example data


data_base <- reshape(data,                        # Modify data for Base R barplot
                     idvar = "Depth",
                     timevar = "Min",
                     direction = "wide")
row.names(data_base) <- data_base$subgroup
data_base <- data_base[ , 2:ncol(data_base)]
colnames(data_base) <- c("1", "2","3")
data_base <- as.matrix(data_base)
data_base                                         # Print modified data

ggplot(data,                                      # Grouped barplot using ggplot2
       aes(x = Min, y = values, fill = Depth)) +
  geom_bar(
          stat = "identity",
          width=0.90,
          position = "dodge",
          ) +
          
  labs(y = "% Increase in Mobility", x = "UV Weathering Time (min)")+
  
  
  #Annotation

 
  annotate("text", x=0.6, y=340, label="(E)", fontface=2)+
  annotate("text", x=0.72, y=120, label= "1cm")+
  annotate("text", x= 1.26, y=95, label="2cm")+
  update_geom_defaults("text", list(size = 10))+

  
  scale_fill_brewer( labels=c('1', '2'))+
  labs(fill= "Depth (cm)")+
#  scale_x_discrete(position = "top")+
#  scale_y_continuous(sec.axis = dup_axis())+ 
  theme_bw()+

 
  theme(
    element_blank(),
    axis.text.x=element_text(size = 25),
    axis.title.x=element_text(size = 30),
    axis.text.y=element_text(size=25),
    axis.title.y=element_text(size = 30),
   # legend.position = c(0.025, 0.68),
   legend.position = "none",
    legend.justification = c("left", "bottom"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.text = element_text(size=20),
    legend.title = element_text(size = 25),
    legend.background=element_rect(fill = alpha("white", 0.1))
    )

#Tukey test for all directions
one.way <- aov(mob_inc_per ~ factor(min_weathered), data = actual_values)
#tukey.one.way <- TukeyHSD(one.way)
tk<-TukeyHSD(one.way)
# this computed Tukey Honest Significant Differences
tk
summary(one.way)
#summary(tukey.one.way)

