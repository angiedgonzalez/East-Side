# DATA EXPLORATION #

# This script will be compiled of graphs and other data exploration outputs that will not be in my final
# analysis. This will also be the resting place of deceased models that I no longer deemed worthy. 
# please date any deceased models when you place them in this working documents. 
# Love, Kitten Mittens


climate<- read.csv("climate_data_all.csv")
plot_data <- read.csv("general_plot_data_new.csv")
species_data <- read.csv("species_data_expanded.csv")
vegetation_data <- read.csv("nontreeveg.csv")




## packages ##
library(ggplot2)


ggplot(vegetation_data) + geom_col(aes(y=Total_shrub_.cover, x=Subplot_NE.SW))
ggplot(vegetation_data) + geom_histogram(aes(Total_shrub_.cover, fill=Subplot_NE.SW))





# summing TPH by yr_plot

plot_data <- plot_data %>% rename(Aspect=Aspect_SW.NE)

all_data <- merge(all_data, plot_data, by=c("Yr_Plot"))

all_data <- merge(all_data, species_data, by=c("Yr_Plot"))
all_data$Forest_type <- tolower(all_data$Forest_type)
all_data$Species <- tolower(all_data$Species)
unique(all_data$Species)


ggplot(all_data) + geom_col(aes(x=Forest_type, y=TPH))

all_data %>% subset(Pre_or_Post_Fire=="post") %>% ggplot() + geom_boxplot(aes(x=Forest_type, y=Height_cm, col=Species))
all_data %>% subset(Pre_or_Post_Fire=="post") %>% ggplot() + geom_boxplot(aes(x=Fire_sev, y=Height_cm, col=Species))
all_data %>% subset(Pre_or_Post_Fire=="post") %>% ggplot() + geom_boxplot(aes(x=Aspect, y=Height_cm, col=Species))

all_data %>% subset(TPH<= 450) %>% ggplot() + geom_boxplot(aes(x=Forest_type, y=TPH))
