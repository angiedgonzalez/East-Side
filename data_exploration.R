# DATA EXPLORATION #

# This script will be compiled of graphs and other data exploration outputs that will not be in my final
# analysis. This will also be the resting place of deceased models that I no longer deemed worthy. 
# please date any deceased models when you place them in this working documents. 
# Love, Kitten Mittens


climate<- read.csv("climate_data_all.csv")
plot_data <- read.csv("general_plot_data_new.csv")
species_data <- read.csv("species_data_expanded.csv")
vegetation_data <- read.csv("nontreeveg.csv")

### you still need to do TPH scaling on regen data. I ampretty sure you have the code in one of the data
# sheets in excel. also, merge date into other DFs and save



## packages ##
library(ggplot2)


ggplot(vegetation_data) + geom_col(aes(y=Total_shrub_.cover, x=Subplot_NE.SW))
ggplot(vegetation_data) + geom_histogram(aes(Total_shrub_.cover, fill=Subplot_NE.SW))
