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


###############Fixing the code that we made for the original graph to work with the second format we want. WORKSSS!!!!!!

# Import data
actual_values <- read_sheet("https://docs.google.com/spreadsheets/d/1DjqdOtdoefnd76telvB_zbPpygFFc2bwYJvmlB4SugE/edit?usp=sharing", sheet = "Depth_comparison")

# Grouping by Depth value
actual_values$Depth <- factor(actual_values$Depth, levels = c("2", "1"), labels = c("2", "1"))

# Grouping by min_weathered
actual_values$min_weathered <- factor(actual_values$min_weathered, levels = c("15", "30", "60"), labels = c("15 min", "30 min", "60 min"))

# Set Color Brewer palette for blue hues
palette <- brewer.pal(3, "Blues")

ggplot(actual_values, aes(x = reorder(Depth, desc(Depth)), y = mob_inc_per, fill = min_weathered)) +
  geom_col(position = position_dodge(width = 1.08), width = 1) +
  geom_errorbar(aes(ymin = mob_inc_per - Norm_STD, ymax = mob_inc_per + Norm_STD),
                width = 0.3, position = position_dodge(width = 1.08)) +
  labs(x = "Column Depth (cm)", y = "% Increase in Mobility", fill = "Time") +
  scale_fill_manual(values = palette, na.translate = FALSE, labels = c("15 min", "30 min", "60 min")) +
  expand_limits(x = c(0.5, 2), y = c(0, 600)) +
  annotate("text", x = 0.5, y = 590, label = "(E)", fontface = 2) +
  update_geom_defaults("text", list(size = 10)) +
  theme_bw() +
  theme(
    text = element_text(size = 25),
    axis.text.x = element_text(size = 25),
    axis.title.x = element_text(size = 25),
    axis.text.y = element_text(size = 25),
    axis.title.y = element_text(size = 25),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.175, 0.92),
    legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    aspect.ratio = 0.7
  )



