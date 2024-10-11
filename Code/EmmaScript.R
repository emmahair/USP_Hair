# Emma Hair ####
# University of Florida 
# October 4, 2024
# University Scholars Program Data Analysis 

library(tidyverse)
library(readxl)
library(moments)
library(fitdistrplus)

# Load site data ####
siteData1 <- read_xlsx("RawDataandProcessing/Hair1_HerbVar_Datasheet_2024.xlsx", 
                       sheet = "siteData") %>% 
  filter(variable == "plantSpecies" | variable == "plantGenus" |
           variable == "plantFamily" | variable == "transectOriginLat" |
           variable == "transectOriginLong" | variable == "quadratRadius" |
           variable == "protocolFollowed") %>%
  dplyr::select(variable, datum) %>%
  pivot_wider(names_from = "variable", values_from = "datum")

siteData2 <- read_xlsx("RawDataandProcessing/Hair2_HerbVar_Datasheet_2024.xlsx", 
                       sheet = "siteData") %>% 
  filter(variable == "plantSpecies" | variable == "plantGenus" |
           variable == "plantFamily" | variable == "transectOriginLat" |
           variable == "transectOriginLong" | variable == "quadratRadius" |
           variable == "protocolFollowed") %>%
  dplyr::select(variable, datum)%>%
  pivot_wider(names_from = "variable", values_from = "datum")

siteData3 <- read_xlsx("RawDataandProcessing/Hair3_HerbVar_Datasheet_2024.xlsx", 
                       sheet = "siteData") %>% 
  filter(variable == "plantSpecies" | variable == "plantGenus" |
           variable == "plantFamily" | variable == "transectOriginLat" |
           variable == "transectOriginLong" | variable == "quadratRadius" |
           variable == "protocolFollowed") %>%
  dplyr::select(variable, datum)%>%
  pivot_wider(names_from = "variable", values_from = "datum")

siteData4 <- read_xlsx("RawDataandProcessing/Hair4_HerbVar_Datasheet_2024.xlsx", 
                       sheet = "siteData") %>% 
  filter(variable == "plantSpecies" | variable == "plantGenus" |
           variable == "plantFamily" | variable == "transectOriginLat" |
           variable == "transectOriginLong" | variable == "quadratRadius" |
           variable == "protocolFollowed") %>%
  dplyr::select(variable, datum)%>%
  pivot_wider(names_from = "variable", values_from = "datum")

siteData5 <- read_xlsx("RawDataandProcessing/Hair5_HerbVar_Datasheet_2024.xlsx", 
                       sheet = "siteData")%>% 
  filter(variable == "plantSpecies" | variable == "plantGenus" |
           variable == "plantFamily" | variable == "transectOriginLat" |
           variable == "transectOriginLong" | variable == "quadratRadius" |
           variable == "protocolFollowed") %>%
  dplyr::select(variable, datum)%>%
  pivot_wider(names_from = "variable", values_from = "datum")

siteData6 <- read_xlsx("RawDataandProcessing/Hair6_HerbVar_Datasheet_2024.xlsx", 
                       sheet = "siteData") %>% 
  filter(variable == "plantSpecies" | variable == "plantGenus" |
           variable == "plantFamily" | variable == "transectOriginLat" |
           variable == "transectOriginLong" | variable == "quadratRadius" |
           variable == "protocolFollowed") %>%
  dplyr::select(variable, datum)%>%
  pivot_wider(names_from = "variable", values_from = "datum")

siteData7 <- read_xlsx("RawDataandProcessing/Hair7_HerbVar_Datasheet_2024.xlsx", 
                       sheet = "siteData") %>% 
  filter(variable == "plantSpecies" | variable == "plantGenus" |
           variable == "plantFamily" | variable == "transectOriginLat" |
           variable == "transectOriginLong" | variable == "quadratRadius" |
           variable == "protocolFollowed") %>%
  dplyr::select(variable, datum)%>%
  pivot_wider(names_from = "variable", values_from = "datum")

siteData8 <- read_xlsx("RawDataandProcessing/Hair8_HerbVar_Datasheet_2024.xlsx", 
                       sheet = "siteData") %>% 
  filter(variable == "plantSpecies" | variable == "plantGenus" |
           variable == "plantFamily" | variable == "transectOriginLat" |
           variable == "transectOriginLong" | variable == "quadratRadius" |
           variable == "protocolFollowed") %>%
  dplyr::select(variable, datum)%>%
  pivot_wider(names_from = "variable", values_from = "datum")

# something
## Bind Site Data ####
site_data_all <- rbind(siteData1, siteData2) %>% 
  rbind(., siteData3) %>%
  rbind(., siteData4) %>%
  rbind(., siteData5) %>%
  rbind(., siteData6) %>%
  rbind(., siteData7)%>%
  rbind(., siteData8)

site_data_all[1,3] <- "Asteraceae"

# Load plant data ####

plantData1 <- read_xlsx("RawDataandProcessing/Hair1_HerbVar_Datasheet_2024.xlsx", 
                        sheet = "plantData")
plantData1 <- plantData1[ , -c(35: 55)]

plantData2 <- read_xlsx("RawDataandProcessing/Hair2_HerbVar_Datasheet_2024.xlsx", 
                        sheet = "plantData")
plantData2 <- plantData2[ , -c(35: 55)]

plantData3 <- read_xlsx("RawDataandProcessing/Hair3_HerbVar_Datasheet_2024.xlsx", 
                        sheet = "plantData")
plantData3 <- plantData3[ , -c(35: 55)]

plantData4 <- read_xlsx("RawDataandProcessing/Hair4_HerbVar_Datasheet_2024.xlsx", 
                        sheet = "plantData")
plantData4 <- plantData4[ , -c(35: 55)]

plantData5 <- read_xlsx("RawDataandProcessing/Hair5_HerbVar_Datasheet_2024.xlsx", 
                        sheet = "plantData")
plantData5 <- plantData5[ , -c(35: 55)]

plantData6 <- read_xlsx("RawDataandProcessing/Hair6_HerbVar_Datasheet_2024.xlsx", 
                        sheet = "plantData")
plantData6 <- plantData6[ , -c(35: 55)]

plantData7 <- read_xlsx("RawDataandProcessing/Hair7_HerbVar_Datasheet_2024.xlsx", 
                        sheet = "plantData")
plantData7$percHerbPlant <- plantData7$percHerbInsect
plantData7 <- plantData7[ , -c(35: 58)]

plantData8 <- read_xlsx("RawDataandProcessing/Hair8_HerbVar_Datasheet_2024.xlsx", 
                        sheet = "plantData")
plantData8 <- plantData8[ , -c(35: 55)]

## Bind plant data ####
plant_data_all <- rbind(plantData1, plantData2) %>% 
  rbind(., plantData3) %>%
  rbind(., plantData4) %>%
  rbind(., plantData5) %>%
  rbind(., plantData6) %>%
  rbind(., plantData7)%>%
  rbind(., plantData8)
# Removed some columns from plantData7 for cactus herbivory 


# Single histogram attempt ####
ggplot(plant_data_all, aes(x = percHerbPlant)) +
  geom_histogram(fill = "lightgreen", color = "black", 
                 binwidth = 3) +
  theme_minimal() +
  labs(title = "Herbivory Level Distribution Across All Surveys",
       x = "Herbivory Level",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
?geom_histogram

# Combined histogram attempt ####
# Add a survey identifier column to each dataset
plantData1 <- plantData1 %>% mutate(survey = "Survey 1")
plantData2 <- plantData2 %>% mutate(survey = "Survey 2")
plantData3 <- plantData3 %>% mutate(survey = "Survey 3")
plantData4 <- plantData4 %>% mutate(survey = "Survey 4")
plantData5 <- plantData5 %>% mutate(survey = "Survey 5")
plantData6 <- plantData6 %>% mutate(survey = "Survey 6")
plantData7 <- plantData7 %>% mutate(survey = "Survey 7")
plantData8 <- plantData8 %>% mutate(survey = "Survey 8")

siteData1 <- siteData1 %>% mutate(survey = "Survey 1")
siteData2 <- siteData2 %>% mutate(survey = "Survey 2")
siteData3 <- siteData3 %>% mutate(survey = "Survey 3")
siteData4 <- siteData4 %>% mutate(survey = "Survey 4")
siteData5 <- siteData5 %>% mutate(survey = "Survey 5")
siteData6 <- siteData6 %>% mutate(survey = "Survey 6")
siteData7 <- siteData7 %>% mutate(survey = "Survey 7")
siteData8 <- siteData8 %>% mutate(survey = "Survey 8")

# Combine all datasets into one dataframe again
plant_data_all <- rbind(plantData1, plantData2) %>% 
  rbind(., plantData3) %>%
  rbind(., plantData4) %>%
  rbind(., plantData5) %>%
  rbind(., plantData6) %>%
  rbind(., plantData7)%>%
  rbind(., plantData8)

site_data_all <- rbind(siteData1, siteData2) %>% 
  rbind(., siteData3) %>%
  rbind(., siteData4) %>%
  rbind(., siteData5) %>%
  rbind(., siteData6) %>%
  rbind(., siteData7)%>%
  rbind(., siteData8)

# Create a histogram
ggplot(plant_data_all, aes(x = percHerbPlant)) +
  geom_histogram(fill = "lightgreen", color = "black",
                 binwidth = 3) +
  facet_wrap(~ survey, scales = "free_y") + 
  theme_minimal() +
  labs(title = "Herbivory Levels Across All Surveys",
       x = "Herbivory Level",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
?boxplot

# Varience, mean, and skew data on leaf herbivory ####

leaf_herb_stats <- plant_data_all %>%
  group_by(survey) %>%
  summarize(
    mean = mean(percHerbPlant, na.rm = TRUE),
    variance = var(percHerbPlant, na.rm = TRUE),
    skewness = skewness(percHerbPlant, na.rm = TRUE)
  )

plant_herb_stats <- plant_data_all %>%
  group_by(survey) %>%
  summarize(
    mean = mean(percHerbPlant, na.rm = TRUE),
    variance = var(percHerbPlant, na.rm = TRUE),
    skewness = skewness(percHerbPlant, na.rm = TRUE)
  )


# To visualize ?

ggplot(plant_data_all, aes(x = numLeavesHerb)) +
  geom_histogram(fill = "lightblue", color = "black", bins = 30) +
  facet_wrap(~ survey) + 
  labs(title = "Number of Leaf Herbivory by Survey",
       x = "Count",
       y = "Frequency") +
  theme_minimal()

# Density data calculations using focal & total plant cover
combined_plant_and_site_data <- left_join(plant_data_all, site_data_all, 
                                          by = "survey")
combined_plant_and_site_data <- combined_plant_and_site_data %>%
  mutate(
    quadratArea = pi * (as.numeric(quadratRadius)^2), 
    plantDensity = as.numeric(numPlantsinQuad) / quadratArea  
  )
# warning message bc non numeric values, lets see where they are

unique(combined_plant_and_site_data$numPlantsinQuad)

# lets try again, taking out NAs

combined_plant_and_site_data <- combined_plant_and_site_data %>%
  mutate(
    numPlantsinQuad = as.numeric(numPlantsinQuad),
    quadratRadius = as.numeric(quadratRadius),
    quadratArea = pi * (quadratRadius^2)
  ) %>%
  filter(!is.na(numPlantsinQuad) & !is.na(quadratRadius)) %>% 
  mutate(plantDensity = numPlantsinQuad / quadratArea)

# there are so many density values, trying to get it by survey
survey_density <- combined_plant_and_site_data %>%
  group_by(survey) %>%
  summarize(
    meanDensity = mean(plantDensity, na.rm = TRUE)  
  )

#need to standardize quadArea
combined_plant_and_site_data <- combined_plant_and_site_data %>%
  mutate(
    numPlantsinQuad = as.numeric(numPlantsinQuad),
    quadratRadius = as.numeric(quadratRadius),
    quadratArea = pi * (quadratRadius^2)
  ) %>%
  filter(!is.na(numPlantsinQuad) & !is.na(quadratRadius)) %>% 
  mutate(plantDensity = numPlantsinQuad / quadratArea, 
         plantDensitym2 = plantDensity * 1/quadratArea)

survey_density <- combined_plant_and_site_data %>%
  group_by(survey) %>%
  summarize(
    meanDensity = mean(plantDensitym2, na.rm = TRUE)  
  )

# Using the ‘fitdistrplus’ package (one survey at a time) ####
## Survey 1 -need to filter to just one survey - ln best ####
onesurv <- plant_data_all %>% filter(survey == "Survey 1")
onesurv$propHerb1 <- (onesurv$percHerbPlant)/100

# need to do a slight transformation to your data, used percHerbPlant for this
onesurv$propHerb1 <- (onesurv$propHerb1 * (length(onesurv$propHerb1) - 1) + 0.5) / length(onesurv$propHerb1)


# plot data
hist(onesurv$propHerb1)
summary(onesurv$propHerb1)

# plot distributions for checks
plotdist(onesurv$propHerb1, histo = TRUE, demp = TRUE)
descdist(onesurv$propHerb1, method = "unbiased", boot = 1000)

# fit normal, log-normal, and beta distributions
nm.MME <- fitdist(onesurv$propHerb1, "norm", method = "mme")
ln.MME <- fitdist(onesurv$propHerb1, "lnorm", method = "mme")
bt.MME <- fitdist(onesurv$propHerb1, "beta", method = "mme")

# get goodness of fits for the above distributions
gofstat(list(nm.MME, ln.MME, bt.MME),
        fitnames = c("nm.MME", "ln.MME", "bt.MME"))

# plot distritbutions to check fit
plot(nm.MME)
plot(ln.MME)
plot(bt.MME)

# get parameters from distributions for plotting
summary(nm.MME)
summary(ln.MME)
summary(bt.MME)


### example graph – NOT WORKING can update later ###
horse2 <- ggplot(onesurv, aes(propHerb1)) +
  geom_histogram(aes(x = propHerb1,
                     y = stat(count / sum(count))),
                 color = 'black', fill="grey80",
                 binwidth = 6) +
  #stat_function(geom = "point", color="purple", n = 5, fun = dpois, args = list(lambda = 0.6923077)) +
  stat_function(geom = "area", aes(propHerb1), n = 6, fun = dnorm,
                args = list(mean = 0.7), xlim = c(0,1),
                fill = "purple", alpha = 0.3)+
  # annotate(geom="text", x=4, y=.4, label=expression(~lambda == 0.7),color="black", size=8)+
  theme_bw(base_size = 24)+
  labs(x='deaths by horse kick', y='proportion')


## Survey 2: need to filter to just one survey - ln best ####
twosurv <- plant_data_all %>% filter(survey == "Survey 2")
twosurv$propHerb2 <- (twosurv$percHerbPlant)/100

# need to do a slight transformation to your data, used percHerbPlant for this
twosurv$propHerb2 <- (twosurv$propHerb2 * (length(twosurv$propHerb2) - 1) + 0.5) / length(twosurv$propHerb2)


# plot data
hist(twosurv$propHerb2)
summary(twosurv$propHerb2)

# plot distributions for checks
plotdist(twosurv$propHerb2, histo = TRUE, demp = TRUE)
descdist(twosurv$propHerb2, method = "unbiased", boot = 1000)

# fit normal, log-normal, and beta distributions
nm.MME2 <- fitdist(twosurv$propHerb2, "norm", method = "mme")
ln.MME2 <- fitdist(twosurv$propHerb2, "lnorm", method = "mme")
bt.MME2 <- fitdist(twosurv$propHerb2, "beta", method = "mme")

# get goodness of fits for the above distributions
gofstat(list(nm.MME2, ln.MME2, bt.MME2),
        fitnames = c("nm.MME2", "ln.MME2", "bt.MME3"))

# plot distributions to check fit
plot(nm.MME2)
plot(ln.MME2)
plot(bt.MME2)

# get parameters from distributions for plotting
summary(nm.MME2)
summary(ln.MME2)
summary(bt.MME2)

## Survey 3: need to filter to just one survey - ln best ####
threesurv <- plant_data_all %>% filter(survey == "Survey 3")
threesurv$propHerb3 <- (threesurv$percHerbPlant)/100

# need to do a slight transformation to your data, used percHerbPlant for this
threesurv$propHerb3 <- (threesurv$propHerb3 * (length(threesurv$propHerb3) - 1) + 0.5) / length(threesurv$propHerb3)


# plot data
hist(threesurv$propHerb3)
summary(threesurv$propHerb3)

# plot distributions for checks
plotdist(threesurv$propHerb3, histo = TRUE, demp = TRUE)
descdist(threesurv$propHerb3, method = "unbiased", boot = 1000)

# fit normal, log-normal, and beta distributions
nm.MME3 <- fitdist(threesurv$propHerb3, "norm", method = "mme")
ln.MME3 <- fitdist(threesurv$propHerb3, "lnorm", method = "mme")
bt.MME3 <- fitdist(threesurv$propHerb3, "beta", method = "mme")

# get goodness of fits for the above distributions
gofstat(list(nm.MME3, ln.MME3, bt.MME3),
        fitnames = c("nm.MME3", "ln.MME3", "bt.MME3"))

# plot distributions to check fit
plot(nm.MME3)
plot(ln.MME3)
plot(bt.MME3)

# get parameters from distributions for plotting
summary(nm.MME3)
summary(ln.MME3)
summary(bt.MME3)

## Survey 4: need to filter to just one survey - ln best ####
foursurv <- plant_data_all %>% filter(survey == "Survey 4")
foursurv$propHerb4 <- (foursurv$percHerbPlant)/100

# need to do a slight transformation to your data, used percHerbPlant for this
foursurv$propHerb4 <- (foursurv$propHerb4 * (length(foursurv$propHerb4) - 1) + 0.5) / length(foursurv$propHerb4)

# plot data
hist(foursurv$propHerb4)
summary(foursurv$propHerb4)

# plot distributions for checks
plotdist(foursurv$propHerb4, histo = TRUE, demp = TRUE)
descdist(foursurv$propHerb4, method = "unbiased", boot = 1000)

# fit normal, log-normal, and beta distributions
nm.MME4 <- fitdist(foursurv$propHerb4, "norm", method = "mme")
ln.MME4 <- fitdist(foursurv$propHerb4, "lnorm", method = "mme")
bt.MME4 <- fitdist(foursurv$propHerb4, "beta", method = "mme")

# get goodness of fits for the above distributions
gofstat(list(nm.MME4, ln.MME4, bt.MME4),
        fitnames = c("nm.MME4", "ln.MME4", "bt.MME4"))

# plot distributions to check fit
plot(nm.MME4)
plot(ln.MME4)
plot(bt.MME4)

# get parameters from distributions for plotting
summary(nm.MME4)
summary(ln.MME4)
summary(bt.MME4)

## Survey 5: need to filter to just one survey - ln best ####
fivesurv <- plant_data_all %>% filter(survey == "Survey 5")
fivesurv$propHerb5 <- (fivesurv$percHerbPlant)/100

# need to do a slight transformation to your data, used percHerbPlant for this
fivesurv$propHerb5 <- (fivesurv$propHerb5 * (length(fivesurv$propHerb5) - 1) + 0.5) / length(fivesurv$propHerb5)

# plot data
hist(fivesurv$propHerb5)
summary(fivesurv$propHerb5)

# plot distributions for checks
plotdist(fivesurv$propHerb5, histo = TRUE, demp = TRUE)
descdist(fivesurv$propHerb5, method = "unbiased", boot = 1000)

# fit normal, log-normal, and beta distributions
nm.MME5 <- fitdist(fivesurv$propHerb5, "norm", method = "mme")
ln.MME5 <- fitdist(fivesurv$propHerb5, "lnorm", method = "mme")
bt.MME5 <- fitdist(fivesurv$propHerb5, "beta", method = "mme")

# get goodness of fits for the above distributions
gofstat(list(nm.MME5, ln.MME5, bt.MME5),
        fitnames = c("nm.MME5", "ln.MME5", "bt.MME5"))

# plot distributions to check fit
plot(nm.MME5)
plot(ln.MME5)
plot(bt.MME5)

# get parameters from distributions for plotting
summary(nm.MME5)
summary(ln.MME5)
summary(bt.MME5)

## Survey 6: need to filter to just one survey - ln best ####
sixsurv <- plant_data_all %>% filter(survey == "Survey 6")
sixsurv$propHerb6 <- (sixsurv$percHerbPlant)/100

# need to do a slight transformation to your data, used percHerbPlant for this
sixsurv$propHerb6 <- (sixsurv$propHerb6 * (length(sixsurv$propHerb6) - 1) + 0.5) / length(sixsurv$propHerb6)

# plot data
hist(sixsurv$propHerb6)
summary(sixsurv$propHerb6)

# plot distributions for checks
plotdist(sixsurv$propHerb6, histo = TRUE, demp = TRUE)
descdist(sixsurv$propHerb6, method = "unbiased", boot = 1000)

# fit normal, log-normal, and beta distributions
nm.MME6 <- fitdist(sixsurv$propHerb6, "norm", method = "mme")
ln.MME6 <- fitdist(sixsurv$propHerb6, "lnorm", method = "mme")
bt.MME6 <- fitdist(sixsurv$propHerb6, "beta", method = "mme")

# get goodness of fits for the above distributions
gofstat(list(nm.MME6, ln.MME6, bt.MME6),
        fitnames = c("nm.MME6", "ln.MME6", "bt.MME6"))

# plot distributions to check fit
plot(nm.MME6)
plot(ln.MME6)
plot(bt.MME6)

# get parameters from distributions for plotting
summary(nm.MME6)
summary(ln.MME6)
summary(bt.MME6)

## Survey 7: need to filter to just one survey - ln best ####
sevensurv <- plant_data_all %>% filter(survey == "Survey 7")
sevensurv$propHerb7 <- (sevensurv$percHerbPlant)/100

# need to do a slight transformation to your data, used percHerbPlant for this
sevensurv$propHerb7 <- (sevensurv$propHerb7 * (length(sevensurv$propHerb7) - 1) + 0.5) / length(sevensurv$propHerb7)

# plot data
hist(sevensurv$propHerb7)
summary(sevensurv$propHerb7)

# plot distributions for checks
plotdist(sevensurv$propHerb7, histo = TRUE, demp = TRUE)
descdist(sevensurv$propHerb7, method = "unbiased", boot = 1000)

# fit normal, log-normal, and beta distributions
nm.MME7 <- fitdist(sevensurv$propHerb7, "norm", method = "mme")
ln.MME7 <- fitdist(sevensurv$propHerb7, "lnorm", method = "mme")
bt.MME7 <- fitdist(sevensurv$propHerb7, "beta", method = "mme")

# get goodness of fits for the above distributions
gofstat(list(nm.MME7, ln.MME7, bt.MME7),
        fitnames = c("nm.MME7", "ln.MME7", "bt.MME7"))

# plot distributions to check fit
plot(nm.MME7)
plot(ln.MME7)
plot(bt.MME7)

# get parameters from distributions for plotting
summary(nm.MME7)
summary(ln.MME7)
summary(bt.MME7)

## Survey 8: need to filter to just one survey - ln best ####
eightsurv <- plant_data_all %>% filter(survey == "Survey 8")
eightsurv$propHerb8 <- (eightsurv$percHerbPlant)/100

# need to do a slight transformation to your data, used percHerbPlant for this
eightsurv$propHerb8 <- (eightsurv$propHerb8 * (length(eightsurv$propHerb8) - 1) + 0.5) / length(eightsurv$propHerb8)

# plot data
hist(eightsurv$propHerb8)
summary(eightsurv$propHerb8)

# plot distributions for checks
plotdist(eightsurv$propHerb8, histo = TRUE, demp = TRUE)
descdist(eightsurv$propHerb8, method = "unbiased", boot = 1000)

# fit normal, log-normal, and beta distributions
nm.MME8 <- fitdist(eightsurv$propHerb8, "norm", method = "mme")
ln.MME8 <- fitdist(eightsurv$propHerb8, "lnorm", method = "mme")
bt.MME8 <- fitdist(eightsurv$propHerb8, "beta", method = "mme")

# get goodness of fits for the above distributions
gofstat(list(nm.MME8, ln.MME8, bt.MME8),
        fitnames = c("nm.MME8", "ln.MME8", "bt.MME8"))

# plot distributions to check fit
plot(nm.MME8)
plot(ln.MME8)
plot(bt.MME8)

# get parameters from distributions for plotting
summary(nm.MME8)
summary(ln.MME8)
summary(bt.MME8)