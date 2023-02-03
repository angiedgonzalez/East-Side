
# BLOCK FOR PACKAGES

Packages <- c ("ggplot2", "tidyverse")
lapply(Packages, library, character.only = TRUE)


########################################################
#### Wrangling ####
#bringing in files
regen_data <- read.csv("post_fire_trees.csv")
plot_data <- read.csv("general_plot_data_new.csv")
species_data <- read.csv("species_data_expanded.csv") # file created from code below, commented out. 
veg_data <- read.csv("nontreeveg.csv")
climate_data <- read.csv("climate_data_all.csv") # file created from PRISM Code


#checking that everything looks good.
levels(as.factor(plot_data$Fire_Name))
levels(as.factor(regen_data$Pre_or_Post_Fire))
regen_data$Pre_or_Post_Fire <- tolower(regen_data$Pre_or_Post_Fire)
# need to get rid of duplicates - to lower should do it. 
levels(as.factor(species_data$Species))
species_data$Species <- tolower(species_data$Species)

# checking unique IDS
unique(species_data$Yr_Plot) # 325. perfect. 
unique(regen_data$Yr_Plot)

#REMEMBER check levels for each factor you are merging


plot_data <- plot_data %>% rename(Aspect=Aspect_SW.NE)

# forest type should be lower case. Causes confusion with fire severity.
plot_data$Forest_type <- tolower(plot_data$Forest_type)

# changing level names for fire sev - don't need this code but saving in case
plot_data <- plot_data %>% mutate(Fire_Sev = recode(Fire_sev, H = 'High', M = 'Moderate', L =  'Low' ))

regen_data <- regen_data %>% group_by(Yr_Plot) %>% summarize(TPH=sum(TPH_scaling_factor))

test <- species_data %>% group_by(Yr_Plot) %>% summarize(TPH=sum(TPH))

identical(test, regen_data)
all.equal(test, regen_data)


all_data <- merge(plot_data, species_data, by=c("Yr_Plot"))

# adding tph to plot data

plot_data <- merge(plot_data, test, by=c("Yr_Plot"))

remove(test, regen_data)

################################################################
#KEEPING FOR REFERENCE######### SPECIES DATA FILE: before you sum TPH you want a new DF for species - expanded. 
#we need to expand tallies. I am going to save all as DS once done w/ DFs. 3 for now
#regen_data$ntimes <- regen_data$tally # changing col name for clarity
#regen_data$ntimes[is.na(regen_data$ntimes)] <- 0 # all blanks' or NAs are now 0
#regen_data$ntimes <- regen_data$ntimes+1 # removing 0s or rows will be removed in next step.
#species_data <- as.data.frame(lapply(regen_data, rep, regen_data$ntimes)) # rep by tally
#unique(species_data$Yr_Plot) 
#species_data <- species_data %>% subset(select=c("Yr_Plot", "Species", "Height_cm", "DBH_cm", "Bud_scar_count", "Pre_or_Post_Fire", "Top_damage.browse_Y.N"))
#write.csv(species_data, "species_data_expanded.csv")
# FYI: TPH WAS SUMMED IN THE CSV FOR SPECIES DATA. DONE BY HAND - FORMULA IS IN ORIGINAL XCEL

## Adding in Elevation data and checking it matches CSV
#elev <- read.csv("elevation_data.csv")
#elev <- subset(elev, select=c("UTM_E", "UTM_N", "Yr_Plot", "Elevation", "UTM_Zone")) # it matches - 325
#plot_data <- merge(elev, plot_data, by=c("UTM_E", "UTM_N", "Yr_Plot"))
#write.csv(plot_data, "general_plot_data.csv")

# final pull of PRISM data - 
# found in PRISM Code - CSV file "climate" has all data. 

#############################################
#############################################################################################



