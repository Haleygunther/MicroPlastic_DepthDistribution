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


#import data
actual_values <- read_sheet("https://docs.google.com/spreadsheets/d/1DjqdOtdoefnd76telvB_zbPpygFFc2bwYJvmlB4SugE/edit?usp=sharing", sheet="Depth_comparison")


#grouping by Depth value
actual_values$Depth<- factor(actual_values$Depth, 
                                           levels = c("1", "2"), 
                                           labels = c("1", "2")
)

data

#grouping by min_weathered
actual_values$min_weathered<- factor(actual_values$min_weathered, 
                             levels = c( "15","30", "60"), 
                             labels = c( "15","30", "60")
)



ggplot(actual_values, aes(x=mob_inc_per, y=reorder(Depth, desc(Depth)), fill = actual_values$min_weathered))+ #reverse y axis scale
 # geom_col( colour = actual_values$min_weathered)+
 labs(x= "% Increase in Mobility", y = "Column Depth (cm)")+
#  geom_errorbar( aes(x=min_weathered, ymin=ave_density_mp_per_mL-SD, ymax=ave_density_mp_per_mL+SD), width=0.1, 
#                 colour="black", alpha=0.8, size=0.5)+
  expand_limits(y = c(1, 2), x = c(1.5, 8))+
  
  theme_bw()+
  
theme(
    axis.text.x=element_text(size = 14),
    axis.title.x=element_text(size = 18),
    
    axis.text.y=element_text(size=14),
    axis.title.y=element_text(size = 18),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
    
)






#########################switching to have 1 and 2 cm on bottom X axis 


 # Lines 70-96 Horizontal. two group

data <- data.frame(values = c(93.3014,254.322,346.571,73.0361,172.374,349.427),  # Create example data
                   group = rep(c("1",
                                 "2"),
                               each = 3),
                   subgroup = LETTERS[1:3])
#SUBGROUPS####################################################################################################################################################


data_base <- reshape(data,                        # Modify data for Base R barplot
                     idvar = "subgroup",
                     timevar = "group",
                     direction = "wide")
row.names(data_base) <- data_base$subgroup
data_base <- data_base[ , 2:ncol(data_base)]
colnames(data_base) <- c("1", "2")
data_base <- as.matrix(data_base)
data_base                                         # Print modified data

ggplot(data,                                      # Grouped barplot using ggplot2
       aes(x = "Depth",
           y = "% Increase in Mobility",
           fill = subgroup)) +
  geom_bar(stat = "identity",
           position = "dodge") +
  
 # coord_flip()

theme_bw()+
  
  theme(
    axis.text.x=element_text(size = 14),
    axis.title.x=element_text(size = 18),
    
    axis.text.y=element_text(size=14),
    axis.title.y=element_text(size = 18),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.2, 0.8),
    legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank()
    
  )













################################Chat GPT code. doesnt work :(  ############################


# Define the number of colors you need
num_colors <- 3

# Choose a ColorBrewer palette
color_palette <- brewer.pal(num_colors, "Blues")

# Assign colors to each subgroup
ggplot(data, aes(x = Depth, y = values, fill = subgroup)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +  # Adjust the width and position of the bars
  labs(x = "Column Depth (cm)", y = "% Increase in Mobility") +
  scale_fill_manual(values = color_palette) +
  theme_bw() +
  theme(
    text = element_text(size = 25),  # Increase the text size for all elements
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.9),
    legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 25),  # Adjust the size of axis text
    legend.text = element_text(size = 25),  # Adjust the size of legend text
    #legend.title = element_text(size = 14)  # Adjust the size of legend title
  )





#DEPTH is supposed to be 1 & 2
#Subgroup is supposed to be


# Define the number of colors you need
num_colors <- 3

# Choose a ColorBrewer palette
color_palette <- brewer.pal(num_colors, "Blues")

# Assign colors to each subgroup
ggplot(data, aes(x = Depth, y = values, fill = subgroup)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +  # Adjust the width and position of the bars
  labs(x = "Column Depth (cm)", y = "% Increase in Mobility") +
  scale_fill_manual(values = color_palette) +
  theme_bw() +
  theme(
    text = element_text(size = 25),  # Increase the text size for all elements
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.9),
    legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 25),  # Adjust the size of axis text
    legend.text = element_text(size = 25),  # Adjust the size of legend text
    #legend.title = element_text(size = 14)  # Adjust the size of legend title
  )







##########################chatGPS pt2 doesnt work

# Define the number of colors to match the number of subgroups within each depth group
num_colors <- length(unique(filtered_data$Group)) / length(unique(filtered_data$Depth))

# Create a list of unique colors for each subgroup
color_palette <- rep(brewer.pal(num_colors, "Blues"), length(unique(filtered_data$Depth)))

# Filter data for Depth values of 1 and 2
filtered_data <- filtered_data %>%
  filter(Depth %in% c(1, 2))

# Assign colors to each subgroup
ggplot(filtered_data, aes(x = as.factor(Depth), y = mob_inc_per, fill = Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_errorbar(aes(ymin = mob_inc_per - mob_inc_per, ymax = mob_inc_per + mob_inc_per), width = 0.2, position = position_dodge(width = 0.8)) +
  labs(x = "Depth", y = "% Increase in Mobility") +
  scale_fill_manual(values = color_palette) +
  scale_x_discrete(labels = c("1", "2")) +  # Set the x-axis labels as "1" and "2"
  theme_bw() +
  theme(
    text = element_text(size = 25),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.9),
    legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 25),
    legend.text = element_text(size = 25)
  )




###################3333333333####take 3- WORKSSSSSS!!!!!!!!!###############



# Step 1: Load the Required Packages
install.packages("tidyverse")
install.packages("googlesheets4")
library(tidyverse)
library(googlesheets4)

# Step 2: Read the Dataset from Google Drive
actual_values <- read_sheet("https://docs.google.com/spreadsheets/d/1DjqdOtdoefnd76telvB_zbPpygFFc2bwYJvmlB4SugE/edit?usp=sharing", sheet = "Depth_comparison")

# Step 3: Rename the Columns
colnames(actual_values) <- c("Depth", "min_weathered", "OG_Value", "MP_count", "mob_inc_per")

# Step 4: Prepare the Data for Plotting
actual_values <- actual_values %>%
  mutate(Group = ifelse(Depth == 1, paste(min_weathered, "min", sep = " "), paste(min_weathered, "min", sep = " ")),
         Group = factor(Group, levels = c("15 min", "30 min", "60 min")))

# Step 5: Create the Plot
palette <- brewer.pal(3, "Blues")

ggplot(actual_values, aes(x = factor(Depth), y = mob_inc_per, fill = Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_errorbar(aes(ymin = mob_inc_per - mob_inc_per, ymax = mob_inc_per + mob_inc_per), width = 0.2, position = position_dodge(width = 0.8)) +
  labs(x = "Depth", y = "% Increase in Mobility", fill = "Time") +
  scale_fill_manual(values = palette) +
  theme_bw() +
  theme(
    text = element_text(size = 12),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.9),
    legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 25),
    legend.text = element_text(size = 25)
  )





###################4 (doesnt work)

# Step 1: Load the Required Packages
install.packages("tidyverse")
install.packages("googlesheets4")
library(tidyverse)
library(googlesheets4)

# Step 2: Read the Dataset from Google Drive
actual_values <- read_sheet("https://docs.google.com/spreadsheets/d/1DjqdOtdoefnd76telvB_zbPpygFFc2bwYJvmlB4SugE/edit?usp=sharing", sheet = "Depth_comparison")

# Step 3: Rename the Columns
colnames(actual_values) <- c("Depth", "min_weathered", "OG_Value", "MP_count", "mob_inc_per")

# Step 4: Prepare the Data for Plotting
actual_values <- actual_values %>%
  mutate(Group = ifelse(Depth == 1, paste("Depth 1", min_weathered, "min", sep = " "), paste("Depth 2", min_weathered, "min", sep = " "))) %>%
  mutate(Group = factor(Group, levels = c("Depth 1 15 min", "Depth 1 30 min", "Depth 1 60 min", "Depth 2 15 min", "Depth 2 30 min", "Depth 2 60 min")))

# Step 5: Create the Plot
palette <- brewer.pal(3, "Blues")

ggplot(actual_values, aes(x = Depth, y = mob_inc_per, fill = Group)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.75) +
  geom_errorbar(aes(ymin = mob_inc_per - mob_inc_per, ymax = mob_inc_per + mob_inc_per), width = 0.2, position = position_dodge(width = 0.8)) +
  labs(x = "Depth", y = "% Increase in Mobility", fill = "Group") +
  scale_fill_manual(values = palette) +
  theme_bw() +
  theme(
    text = element_text(size = 25),
    strip.background = element_blank(),
    strip.placement = "outside",
    strip.text.x = element_blank(),
    panel.grid.major = element_blank(),
    legend.title = element_blank(),
    legend.position = c(0.15, 0.9),
    legend.background = element_rect(fill = "white"),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 25),
    legend.text = element_text(size = 25)
  )







































#Jamies work to calculate statistics


library("ggplot2")                                # Lines 100-126 Vertical three group

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

