
## Setting up ######
#setting drive
#setwd("G:/Shared drives/HarveyLab_UW/EastsidePostFireRegen/Dataset") #lab
#setwd("H:/Shared drives/HarveyLab_UW/EastsidePostFireRegen/Dataset")

# you need to load xl and two sheets in first data set. 

# BLOCK FOR PACKAGES

Packages <- c ("readxl", "ggplot2", "tidyverse")
lapply(Packages, library, character.only = TRUE)


########################################################
#### Wrangling ####
#bringing in files
regen_data <- read.csv("post_fire_trees.csv")
plot_data <- read.csv("general_plot_data_new.csv")
species_data <- read.csv("species_data_expanded.csv") # file created from code below, commented out. 
nonveg_data <- read.csv("nontreeveg.csv")
climate_data <- read.csv("climate_data_all.csv")

### you still need to do TPH scaling on regen data. I ampretty sure you have the code in one of the data
# sheets in excel. also, merge date into other DFs and save


#checking that everything looks good.
levels(as.factor(plot_data$Fire_Name))
levels(as.factor(regen_data$Pre_or_Post_Fire))
levels(as.factor(species_data$Species))
# checking unique IDS
unique(species_data$Yr_Plot) # 325. perfect. 
unique(regen_data$Yr_Plot)

#regen_data <- regen_data %>% subset(Pre_or_Post_Fire=="post")
# Do you want to subset only post - fire? Code above if so.

#REMEMBER check levels for each factor you are merging

################################################################
#KEEPING FOR REFERENCE######### SPECIES DATA FILE: before you sum TPH you want a new DF for species - expanded. 
# we need to expand tallies. I am going to save all as DS once done w/ DFs. 3 for now
regen_data$ntimes <- regen_data$tally # changing col name for clarity
regen_data$ntimes[is.na(regen_data$ntimes)] <- 0 # all blanks' or NAs are now 0
regen_data$ntimes <- regen_data$ntimes+1 # removing 0s or rows will be removed.
species_data <- as.data.frame(lapply(regen_data, rep, regen_data$ntimes)) # rep by tally
unique(species_data$Yr_Plot) 
species_data <- species_data %>% subset(select=c("Yr_Plot", "Species", "Height_cm", "DBH_cm", "Bud_scar_count", "Pre_or_Post_Fire", "Top_damage.browse_Y.N"))


write.csv(species_data, "species_data_expanded.csv")

## Adding in Elevation data and checking it matches CSV
#elev <- read.csv("elevation_data.csv")
#elev <- subset(elev, select=c("UTM_E", "UTM_N", "Yr_Plot", "Elevation", "UTM_Zone")) # it matches - 325
#plot_data <- merge(elev, plot_data, by=c("UTM_E", "UTM_N", "Yr_Plot"))
#write.csv(plot_data, "general_plot_data.csv")

# final pull of PRISM data - 

#############################################
#############################################################################################
#### Wrangling ####
regen_data <- subset(regen_data, select = c("Yr_Plot", "TPH_scaling_factor", "TPH_REGEN_ONLY"))
plot_data <- subset(plot_data, select = c("Yr_Plot", "Aspect_SW.NE", "Fire_Name", "Fire_Yr", "Forest_type", "Fire_sev", "Distance_seedwall_m"))

# summing TPH by yr_plot
regen_data1 <- regen_data %>% group_by(Yr_Plot) %>% summarize(TPH=sum(TPH_scaling_factor), TPH_REGEN=sum(TPH_REGEN_ONLY))

plot_data <- plot_data %>% rename(Aspect=Aspect_SW.NE)

merged_datasets <- merge(regen_data1, plot_data, by=c("Yr_Plot"))

# forest type should be lower case. Causes confusion with fire severity.
merged_datasets$Forest_type <- tolower(merged_datasets$Forest_type)
remove(regen_data1, regen_data) # removing unnecessary DF

# species_data has all and more of regen data. only need plot and merge

# changing level names for fire sev - don't need this code but saving in case
#merged_datasets <- merged_datasets %>%
# mutate(Fire_Sev = recode(Fire_Sev, H = 'High', M = 'Moderate', L =  'Low' ))



# thats it for now. Have worked 3 DF that should be good. All have 325 plots.
# species is the expanded DF that multiplied rows by the tally value in regen data


#### Exploration #####


### plots by forest type ####
forestcond <- c("#bc7d39", "#6fac5d","#9750a1","#677ad1")

tiff("FireSev_ForestType.tiff", units="in", width=7, height=5, res=300)
#factor(Species, level = c('virginica', 'versicolor', 'setosa'))

ggplot(merged_datasets, aes(x=interaction(factor(Fire_sev, level=c('L', "M", "H")), factor(Forest_type, level=c("d", "m", "c", "w"))), y=TPH, fill=Forest_type)
) +geom_boxplot(width=0.5) + scale_y_continuous(labels = scales::comma
)+ scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'),labels=c("Dry", "Moist", "Cold","Wet"))+theme_classic(
) +labs(x="Fire Severity", y="Trees Per Hectare") + scale_x_discrete(labels=c("H.c"="High", "L.c"="Low", "M.c"="Mod", "H.d"="High", "L.d"="Low", "M.d"="Mod", "H.m"="High", "L.m"="Low", "M.m"="Mod", "H.w"="High", "L.w"="Low", "M.w"="Mod")
)
dev.off()
# dry forest types have lease regen - not suprising. high sev dry is almost 0. 
# let's take a look at it. 

# zoom in for high sev dry forest sw vs ne. 

#### dry forest - high sev####
# we need to create new dataset first
dryhigh_data  <- merged_datasets %>% subset(Fire_sev=="H")
dryhigh_data <- dryhigh_data %>% subset(Forest_type=="d")

levels(as.factor(dryhigh_data$Yr_Plot))
levels(as.factor(dryhigh_data$Forest_type))
levels(as.factor(dryhigh_data$Fire_sev))
levels(as.factor(dryhigh_data$Fire_Name))
# 9 fires and 31 plots are high sev and dry


tiff("FireSev_Aspect4.tiff", units="in", width=7, height=5, res=300)

ggplot(dryhigh_data, aes(x=Aspect, y=TPH, fill=Aspect)) +geom_boxplot(
)+ scale_y_continuous(labels = scales::comma)+scale_x_discrete(labels=c(NE="Northeast", SW="Southwest")
)+guides(fill="none")+ labs(y="Trees Per Hectare") + scale_fill_manual(values=c("#677ad1", "#bc7d39")) + geom_jitter()

dev.off()

# very little regen. Even in Northeast plots. However, a few of the fires have some decent
#regen on NE aspects. Separate by fire and or species to see what's
#coming back?

ggplot(dryhigh_data, aes(x=interaction(Aspect, Fire_Name), y=TPH, fill=Aspect))+geom_boxplot(
)+ scale_y_continuous(labels = scales::comma)+scale_x_discrete(labels=c(NE="Northeast", SW="Southwest")
)+guides(fill="none")+ labs(y="Trees Per Hectare") + scale_fill_manual(values=c("#677ad1", "#bc7d39")) + geom_jitter()
# super messy but you can see some dominant regen in a few fires. why?
# let's take a deeper look at First creek. highest regen.
# what species are coming back?

# species DF DOES NOT have TPH value. Let's merge it by plot and see if we get same results
species_data1 <- merge(species_data, merged_datasets, by=c("Yr_Plot"))
unique(species_data1$Yr_Plot) 
unique(species_data1$Fire_Name) 

species_dryhigh  <- species_data1 %>% subset(Fire_sev=="H")
species_dryhigh <- species_dryhigh %>% subset(Forest_type=="d")
levels(as.factor(species_dryhigh$Fire_Name))

## Thinking of how to do this with species. Would it be best to create columns
# with species name and then similar tally for # found in each plot?

ggplot(species_dryhigh) + geom_count(aes(y=Species, x=Fire_Name, color=..n..))+scale_size_area()
# lots of PSME and PIPO coming back. not surprised. PSME appears most dominant seedling.
# some grand fir and larch in jolly mountain. super cool.
remove(regen_data, regen_data1)


# species data subset - how many post fire trees are there?
species_pre <- species_data %>% subset(Pre_or_Post_Fire=="pre")
species_pre <- species_pre %>% subset(Bud_scar_count>6)
levels(as.factor(species_pre$Yr_Plot))
## a lot!

# exploration for future chapter questions: 11/29/2022

ggplot(merged_datasets) + geom_point(aes(y=TPH, x=Distance_seedwall_m))
ggplot(merged_datasets) + geom_col(aes(y=TPH, x=Forest_Type))
ggplot(merged_datasets) + geom_col(aes(y=TPH, x=Fire_Sev))
# notice that the seedwall at 0 still has 0 regen. why? Let's subset this
test <- merged_datasets %>% subset(merged_datasets$Distance_seedwall_m==0)
ggplot(test) + geom_point(aes(y=TPH_REGEN, x=Distance_seedwall_m))
ggplot(test) + geom_col(aes(y=TPH_REGEN, x=Forest_type)) # pattern - cold sites have lowest TPH, followed by Dry
ggplot(test) + geom_col(aes(y=TPH_REGEN, x=Fire_sev)) # NO 0 in high severity. That makes sense - distance is 0
# but something else is going on here. some areas are not regenerating even
# with access to a seed source. Why is that? Which species are in the pre-fire stand?
# are they producing cones?

unique(testspecies$Species)
testspecies <- species_data1 %>% subset(species_data1$Distance_seedwall_m<=150)
#testspecies <- testspecies %>% subset(testspecies$Pre_or_Post_Fire=="post")
ggplot(testspecies) + geom_point(aes(y=TPH_REGEN, x=Distance_seedwall_m, color=Species))
ggplot(testspecies) + geom_col(aes(y=TPH, x=Forest_Type, fill=Species)) # pattern - cold sites have lowest TPH, followed by Dry
# LOTS OF PRE FIRE TSME TSE THE THPL. HEAVY SEEDERS BUT NO LAOC? HMM.
# so seed is most likely limiting even if seedwall is present
# may point to us sampling 'too soon' for some of these trees.
ggplot(testspecies) + geom_col(aes(y=TPH, x=Fire_Sev, fill=Species))
ggplot(testspecies) + geom_col(aes(y=TPH, x=Aspect, fill=Species))
# seperated our the pre fire trees. now we are seeing something interesting
# not a lot of regen other than the wet sites, even when seed isn't limited
# moist or mod forest types still have low regen in ideal conditions
# why? What were the pre fire trees?
# SW aspects have more regen than NE when seed is not limited. Why?
ggplot(testspecies) + geom_col(aes(y=TPH, x=interaction(Forest_Type, Aspect), fill=Species))
# what we are realling seeing here is physiological traits driving seed
# availability in instances where seeds are not limiting.

# dry is not really diff at diff aspects. Cold NE is higher. big diff
# in aspect in wet spots. SW is much higher. 
ggplot(testspecies) + geom_col(aes(y=TPH, x=interaction(Fire_Sev, Aspect), fill=Species))

# tons of western redcedar in good regen - followed by hemlock
# the cold forest types may be temperature limited? They are not seed limited
# i would assume that these species are all seed producing. More exploration
# needed. 
# another question i had, what about the seed limited spots? How do they look?
#species_data1$Species <- tolower(species_data1$Species)
testspecies999 <- species_data1 %>% subset(species_data1$Distance_seedwall_m==999)
# 13 plots total with 999. not many... 
ggplot(testspecies999) + geom_point(aes(y=TPH, x=Distance_seedwall_m))
ggplot(testspecies999) + geom_col(aes(y=TPH, x=interaction(Forest_Type, Aspect), fill=Species))
# not suprising here. When it is cold pico is what is regen if at 999. Seedbank
# post fire.
ggplot(testspecies999) + geom_col(aes(y=TPH, x=Fire_Sev, fill=Species))


# during preliminary exploration based on my dataset i separated our post fire trees
# out of the dataset and looked at regeneration at each plot when seeds
# were not limiting. I still found very low regeneration in areas without seed
# limitation. These sites were characterized as dry or cold. Aspect made
# little difference. HOWEVER, one thing to note about this pattern
# the Wet sites had mostly TSHE, TSME, and TPHL regeneration. Hemlocks are
# prolific seeders. Most of the sites with little regeneration appeared
# to have mostly masting or infrequent seeders at their plots. 
# i discovered this by only looking at the pre fire trees that were in these
# plots. So is it really moisture or temperature limited, or is it 
# driven by species specific traits that are limiting quick regeneration
# additional note: Similar and interesting patterns were found when seed
# was theoretically limited. PICO was prolific in the areas with adequate
# regeneration when seed walls were distant. Pointing to seed banks in the soil.


## NON VEG MERGE #####

# i hypothesize that high severity patches will have lower regeneration but higher
# shrubs and other understory vegetation. This will slow regeneration and potentially lead to
# a conversion to an alternate stable state in areas. 

nonveg_subdata <- nonveg_data %>% subset(select=c("Yr_Plot", "Total_shrub_.cover", "Herbs_.cover", "Graminoid_.cover", "Shrubs_Mean_height_m"))
merge_vegdata <- merge(nonveg_subdata, merged_datasets)
remove(nonveg_subdata)

dry_merge_vegdata <- subset(merge_vegdata, Forest_Type=="d")
ggplot(dry_merge_vegdata) + geom_col(aes(y=Graminoid_.cover, x=Fire_Sev))

# test # what if we do not have limited seed source? 
# there is a pattern where TPH decreases as shrub increases. will this
# hold up if we do not have limited seed source?
merge_vegdata <- subset(merge_vegdata, Distance_seedwall_m<=150 & Cones_postfire_trees_Y.N=="Y")
ggplot(merge_vegdata) + geom_col(aes(x=Fire_Sev, y=TPH_REGEN, fill=Fire_Sev))
# not much changes. it is still true.
# fire sev doesn't really have much of a distinct pattern here. i wonder why.

unique(merge_vegdata$Yr_Plot)
# cones_post fire - trees w/ in plots. There are 9 plots w/ cone
# bearing trees. super cool. 

seeds_species_data <- species_data1 %>% subset(Distance_seedwall_m<=150 & Cones_postfire_trees_Y.N=="Y")
unique(seeds_species_data$Yr_Plot) # only 9 plots had seeds during sampling
# not much to discern here.

ggplot(merged_datasets) + geom_col(aes(y=TPH_REGEN, x=interaction(Prefire_stage, Fire_sev, fill=Fire_sev)))

ggplot(merged_datasets) + geom_col(aes(x=Prefire_stage, y=TPH_REGEN))



