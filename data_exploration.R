# DATA EXPLORATION #

# This script will be compiled of graphs and other data exploration outputs that will not be in my final
# analysis. This will also be the resting place of deceased models that I no longer deemed worthy. 
# please date any deceased models when you place them in this working documents. 
# Love, Kitten Mittens

source("Data_wrangling_working.R")


## packages ## # you may not need some of these in final run.

Packages <- c ("ggplot2", "tidyverse")
lapply(Packages, library, character.only = TRUE)

### color pallettes ###
forestcond <- c("#bc7d39", "#6fac5d","#9750a1","#677ad1")
firecol <- c("#cb5a4c", "#965da7", "#84a955")

ggplot(vegetation_data) + geom_col(aes(y=Total_shrub_.cover, x=Subplot_NE.SW))
ggplot(vegetation_data) + geom_histogram(aes(Total_shrub_.cover, fill=Subplot_NE.SW))

ggplot(all_data) + geom_col(aes(x=Forest_type, y=TPH)) +scale_y_continuous(labels = scales::comma)
# TPH may seem high but it makes sense. it is something across forest type
# not plot. Plot sums make sense. 
ggplot(all_data) + geom_col(aes(x=Forest_type, y=TPH)) +scale_y_continuous(labels = scales::comma)

ggplot(all_data) + geom_boxplot(aes(x=Prefire_stage, y=TPH, fill=Prefire_stage)) +scale_y_continuous(labels = scales::comma)

non_stockthreshold %>% group_by(Fire_sev, Forest_type) %>% summarise(n_distinct(Yr_Plot))

# summing DF 

ggplot(plot_data) + geom_point(aes(x=Distance_seedwall_m, y=TPH)
)+scale_y_continuous(labels = scales::comma)

ggplot(plot_data) + geom_boxplot(aes(x=Fire_sev, y=TPH)
)+scale_y_continuous(labels = scales::comma)

# try to figure out how to sum by yr plot before plotting by forest type
all_data %>% group_by(Yr_Plot) %>% ggplot() + geom_boxplot(aes(x=Forest_type, y=TPH, fill=Forest_type)
    )+ scale_fill_manual(values=forestcond, breaks=c('d', 'm', 'c', 'w'))

### plots by forest type ####


ggplot(plot_data, aes(x=interaction(factor(Fire_sev, level=c('L', "M", "H")), factor(Forest_type, level=c("d", "m", "c", "w"))), y=TPH, fill=Forest_type)
) +geom_boxplot(width=0.5) + scale_y_continuous(labels = scales::comma
)+ scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'),labels=c("Dry", "Moist", "Cold","Wet"))+theme_classic(
) +labs(x="Fire Severity", y="Trees Per Hectare") + scale_x_discrete(labels=c("H.c"="High", "L.c"="Low", "M.c"="Mod", "H.d"="High", "L.d"="Low", "M.d"="Mod", "H.m"="High", "L.m"="Low", "M.m"="Mod", "H.w"="High", "L.w"="Low", "M.w"="Mod")
)


ggplot(plot_data, aes(x=interaction(factor(Aspect, level=c('SW', 'NE')), factor(Forest_type, level=c("d", "m", "c", "w"))), y=TPH, fill=Forest_type)
) +geom_boxplot(width=0.5) + scale_y_continuous(labels = scales::comma, limits=c(0, 200000)
)+ scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'),labels=c("Dry", "Moist", "Cold","Wet"))+theme_classic(
) +labs(x="Fire Severity", y="Trees Per Hectare") + scale_x_discrete(labels=c("H.c"="High", "L.c"="Low", "M.c"="Mod", "H.d"="High", "L.d"="Low", "M.d"="Mod", "H.m"="High", "L.m"="Low", "M.m"="Mod", "H.w"="High", "L.w"="Low", "M.w"="Mod")
)

plot_data  %>% group_by(Forest_type) %>% summarise(n_distinct(Yr_Plot))

##### Stands below stocking threshold ##########
non_stockthreshold <- all_data %>% group_by(Yr_Plot) %>% mutate(TPH=sum(TPH)) %>% subset(TPH <= 150) 
unique(non_stockthreshold$Yr_Plot) # 59 plots not meeting threshold

# lowest TPH mean is in dry forest. not suprised.
ggplot(non_stockthreshold) + geom_boxplot(aes(x=Forest_type, y=TPH, fill=Forest_type))+scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'))

non_stockthreshold <- non_stockthreshold %>% group_by(Forest_type)
# how many of each?
non_stockthreshold  %>% summarise(n_distinct(Yr_Plot))
# majority are dry, but very close second to moist. 
# lets do the same graphic above, but w/ stocking densities
ggplot(non_stockthreshold, aes(x=interaction(factor(Fire_sev, level=c('L', "M", "H")), factor(Forest_type, level=c("d", "m", "c", "w"))), y=TPH, fill=Forest_type)
) +geom_boxplot(width=0.5) + scale_y_continuous(labels = scales::comma
)+ scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'),labels=c("Dry", "Moist", "Cold","Wet"))+theme_classic(
) +labs(x="Fire Severity", y="Trees Per Hectare") + scale_x_discrete(labels=c("H.c"="High", "L.c"="Low", "M.c"="Mod", "H.d"="High", "L.d"="Low", "M.d"="Mod", "H.m"="High", "L.m"="Low", "M.m"="Mod", "H.w"="High", "L.w"="Low", "M.w"="Mod")
)

ggplot(non_stockthreshold, aes(x=interaction(factor(Aspect, level=c('NE', 'SW')), factor(Forest_type, level=c("d", "m", "c", "w"))), y=TPH, fill=Forest_type)
) +geom_boxplot(width=0.5) + scale_y_continuous(labels = scales::comma
)+ scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'),labels=c("Dry", "Moist", "Cold","Wet"))+theme_classic(
) +labs(x="", y="Trees Per Hectare") 

# next we will look at distance to seed source.

ggplot(non_stockthreshold) + geom_point(aes(x=Distance_seedwall_m, y=TPH, col=Fire_sev)
  )
ggplot(non_stockthreshold) + geom_point(aes(x=Distance_seedwall_m, y=TPH, col=Forest_type))

ggplot(non_stockthreshold) + geom_boxplot(aes(x=Distance_seedwall_m, y=TPH, fill=Species))

non_stockthreshold %>% group_by(Distance_seedwall_m>=150, Distance_seedwall_m<150) %>% summarise(n_distinct(Yr_Plot))
non_stockthreshold %>% group_by(Distance_seedwall_m) %>% summarise(n_distinct(Yr_Plot))

ggplot(non_stockthreshold, aes(x=interaction(factor(Fire_sev, level=c('L', "M", "H")), factor(Distance_seedwall_m>=150)), y=TPH, fill=Forest_type)
) +geom_boxplot(width=0.5) + scale_y_continuous(labels = scales::comma
)+ scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'),labels=c("Dry", "Moist", "Cold","Wet"))+theme_classic(
) +labs(x="Fire Severity", y="Trees Per Hectare") + scale_x_discrete(labels=c("H.c"="High", "L.c"="Low", "M.c"="Mod", "H.d"="High", "L.d"="Low", "M.d"="Mod", "H.m"="High", "L.m"="Low", "M.m"="Mod", "H.w"="High", "L.w"="Low", "M.w"="Mod")
)


ggplot(non_stockthreshold, aes(x=interaction(factor(Fire_sev, level=c('L', "M", "H")), factor(Distance_seedwall_m>=150)), y=TPH, fill=Forest_type)
) +geom_boxplot(width=0.5) + scale_y_continuous(labels = scales::comma
)+ scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'),labels=c("Dry", "Moist", "Cold","Wet"))+theme_classic(
) +labs(x="Fire Severity", y="Trees Per Hectare") + scale_x_discrete(labels=c("H.c"="High", "L.c"="Low", "M.c"="Mod", "H.d"="High", "L.d"="Low", "M.d"="Mod", "H.m"="High", "L.m"="Low", "M.m"="Mod", "H.w"="High", "L.w"="Low", "M.w"="Mod")
)



non_stockthreshold %>% group_by(Distance_seedwall_m>=150, Distance_seedwall_m<150, Forest_type) %>% summarise(n_distinct(Yr_Plot))
non_stockthreshold %>% group_by(Distance_seedwall_m>=150, Distance_seedwall_m<150, Fire_sev) %>% summarise(n_distinct(Yr_Plot))

ggplot(non_stockthreshold) + geom_boxplot(aes(x=Distance_seedwall_m, y=TPH, fill=(interaction(Species, Forest_type)))
      )

# One more subset. I am curious how much pre-fire stand density there was

ggplot(non_stockthreshold) + geom_boxplot(aes(x=Prefire_stage, y=TPH, fill=Forest_type)
   )+ scale_fill_manual(values=forestcond, name="Forest Type", breaks=c('d', 'm', 'c', 'w'),labels=c("Dry", "Moist", "Cold","Wet"))








########## OLD EXPLORATION ##################################
# dry forest types have lease regen - not surprising. high sev dry is almost 0. 
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
all_data %>% subset(Pre_or_Post_Fire=="post") %>% ggplot() + geom_boxplot(aes(x=Forest_type, y=Height_cm))
all_data %>% subset(Pre_or_Post_Fire=="post") %>% ggplot() + geom_boxplot(aes(x=Fire_sev, y=Height_cm))
all_data %>% subset(Pre_or_Post_Fire=="post") %>% ggplot() + geom_boxplot(aes(x=Aspect, y=Height_cm))


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


# species data subset - how many pre fire trees are there?
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


