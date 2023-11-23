library(dplyr)
library(gglaplot)
library(ggplot2)
library(data.table)
library(lubridate)
library(tidyr)
library(scales)
library(ggrepel)
library(stringr)
library(tidyverse)
library(plotly)
library(sf)
library(htmlwidgets)
library(htmltools)

library(leaflet)
library(leaflet.extras)
library(leaflegend)
library(leaflet.providers)

devtools::load_all("Q:/Teams/D&PA/Demography/demogtools/")


data_dir <- 'Data/'
chart_dir <- 'Charts/'

#data_dir <- 'Data/Data_for_JH/'


#-------------------------------------------------------------------------------

# Read data 

# Geographic data
borough_boundaries <- st_read("C:/DemogReport/2011_census_london_boroughs/London_Borough_Excluding_MHW.shp")


# (4) Borough infant mortality rates  
input_IMR_borough <- fread(paste0(data_dir, "4_PHOF_IMR_London_FORV2.csv")) %>% 
  data.frame()

# (9) Borough OW and obesity rates  
input_OB_borough <- fread(paste0(data_dir, "9_PHOF_OWandOB_London_forV2.csv")) %>% 
  data.frame()

# (10) Borough Physical activity rates  
input_PA_borough <- fread(paste0(data_dir, "10_PHOF_PA_London_forV2.csv")) %>% 
  data.frame()

# (12) Vaccinations rates with combined categories
input_Vaccination_Eth_comb <- fread(paste0(data_dir, "12_CHIME_Vaccination_London_Eth_CombCats_FORV2.csv")) %>% 
  data.frame()


# (14) Borough Asthma admission rates  
input_Asthma_borough <- fread(paste0(data_dir, "14_PHE_Asthma_London_FORV2.csv")) %>% 
  data.frame()


# Colour palettes

colour_palette <- gla_colour_palette()[c(3,6,2,4,5,7,1)]

colour_palette2 <- gla_colour_palette()[c(3,6)]


my_catpal2 <- gla_pal(palette_type = "categorical",
                      palette_name = "core", n = 2)

my_catpal3 <- gla_pal(palette_type = "categorical",
                   palette_name = "core", n = 3)

my_catpal4 <- gla_pal(palette_type = "categorical",
                      palette_name = "core", n = 4)

my_catpal6 <- gla_pal(palette_type = "categorical",
                      palette_name = "core", n = 6)

my_catpal8 <- gla_pal(palette_type = "categorical",
                      palette_name = "core", n = 8)

my_quantpal3 <- rev(gla_pal(palette_type = "quantitative",
                            n = 3,
                            main_colour = "blue"))

my_quantpal4 <- rev(gla_pal(palette_type = "quantitative",
                            n = 4,
                            main_colour = "pink"))

my_quantpal5 <- rev(gla_pal(palette_type = "quantitative",
                            n = 5,
                            main_colour = "green"))

#-------------------------------------------------------------------------------

# Throughout for static plots this code includes theme_gla()
# as this removes grey background and grid, puts legend at top, caption on left and uses nicer font

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------

# 4) Infant mortality rate - MAP

# match mortality data with LA location data
input_IMR_borough_geog <- input_IMR_borough %>% 
  left_join(borough_boundaries, by=c("Area_Code"="GSS_CODE")) %>%
  mutate(rateCat = cut(Value, breaks = c(0, 2.8, 3.4, 4.0, 6.0),
                       right = FALSE,
                       labels = c("<2.8",
                                  "2.8 - 3.39",
                                  "3.4 - 3.99",
                                  "4+"), right = FALSE)) 

IMR_map <- ggplot(input_IMR_borough_geog, aes(geometry=geometry, fill=rateCat)) +
  theme_gla()+
  ggla_sf(color="white", size = 0.1)+
  theme(legend.position = "right")+
  theme(plot.title = element_text(size = 16)) +
  scale_fill_manual(values = my_quantpal4) + 
  labs(title = "Infant Mortality Rate, map by London borough (2019-2021)", 
       subtitle = "Infant deaths under 1 year of age per 1000 live births",
       caption = paste0("Source: ONS, Chart: GLA demography"))
IMR_map



#-------------------------------------------------------------------------------

# 7) Wellbeing over time

colour_palette2 <- gla_colour_palette()[c(6,3)]

# Anxiety and Satisfaction together in one plot
LS_and_Anx_Means_time_Line <- input_wellBeing_Years_long %>%
  filter(Index == "ANXIETY"  | Index == "SATISFACTION") %>%
  mutate(date = as.Date(Year,"%d/%m/%Y")) %>%
  ggplot(aes(date, Value, colour=Area_Name, group = Area_Name)) + 
  theme_gla( y_axis_title = FALSE, free_y_facets = TRUE) +
  geom_line(size=1.5) +
  facet_wrap(~Index, scales = "free_y") +
  scale_colour_manual(values = colour_palette2) +
  scale_x_date(breaks = "12 month",
             date_labels = "%Y",
             expand = expansion(add = c(3, 3)))+
  theme(plot.margin =unit(c(0,1,0,0), 'cm')) +
  guides(colour = guide_legend(reverse = TRUE)) +
  labs(title = "Mean Anxiety and Life Satisfaction, 2011-12 to 2021-22",
       subtitle = bquote("Mean responses (where 0 is 'not at all anxious' and 10 is 'completely anxious') to"~italic(.("Overall, how anxious did you feel yesterday?"))), 
       caption = paste0("Source: ONS (Labour Force Survey). Chart: GLA demography"))
LS_and_Anx_Means_time_Line


#-------------------------------------------------------------------------------

# 9) Obesity and Overweight map

hist <- hist(input_OB_borough$Value)
quantile(input_OB_borough$Value)
summarise(input_OB_borough, mean = mean(Value))
#range is 44.2 to 70.5, mean 55.4, median 57.2
# Match Admissions data with LA location data

# match Obesity data with LA location data
input_OB_borough_geog <- input_OB_borough %>% 
  left_join(borough_boundaries, by=c("Area_Code"="GSS_CODE")) %>%
  mutate(rateCat = cut(Value, breaks = c(40, 50, 57, 62, 90),
                       right = FALSE,
                       labels = c("<50%",
                                  "50 - 56.9%",
                                  "57 - 61.9%",
                                  "62%+"))) 

OB_map <- ggplot(input_OB_borough_geog, aes(geometry=geometry, fill=rateCat)) +
  theme_gla()+
  ggla_sf(color = "white", size = 0.1)+
  theme(legend.position = "right")+
  theme(plot.title = element_text(size = 16)) +
  scale_fill_manual(values = my_quantpal4) + 
  labs(title = "Percentage of adults classified as overweight or obese, map by London borough (2021/22)", 
       subtitle = bquote("% adults (aged 18+) with body mass index (BMI) greater than or equal to 25kg/m"^2),
       caption = paste0("Source: OHID (based on the Active Lives Adult Survey, Sport England), Chart: GLA demography"))
OB_map


#-------------------------------------------------------------------------------

# 10) Physical Activity map

hist <- hist(input_PA_borough$Value)
quantile(input_PA_borough$Value)
summarise(input_PA_borough, mean = mean(Value))
#range is 36.3 to 74.3, mean 65.9, median 66.1.2
# Match Admissions data with LA location data


# Match Activity data with LA location data
input_PA_borough_geog <- input_PA_borough %>% 
  left_join(borough_boundaries, by=c("Area_Code"="GSS_CODE")) %>%
  mutate(rateCat = cut(Value, breaks = c(20, 62, 66, 72, 80),
                       right = FALSE,
                       labels = c("<62%",
                                  "62 - 65.9%",
                                  "66 - 71.9%",
                                  "72%+"))) 

PA_map <- ggplot(input_PA_borough_geog, aes(geometry=geometry, fill=rateCat)) +
  theme_gla()+
  ggla_sf(color = "white", size = 0.1)+
  theme(legend.position = "right")+
  theme(plot.title = element_text(size = 16)) +
  scale_fill_manual(values = my_quantpal4) + 
  labs(title = "Percentage of physically active adults, map by London borough (2020/21)", 
       subtitle = "% adults (aged 19+) doing 150+ mins/week moderate intensity activity in bouts of 10+ mins in previous 28 days",
       caption = paste0("Source: OHID (based on the Active Lives Adult Survey, Sport England), Chart: GLA demography"))
PA_map


#-------------------------------------------------------------------------------

# 12) VACCINATION RATES by ETHNICITY  (LINE CHART)

Vaccination_Eth_Line_comb <- input_Vaccination_Eth_comb %>% 
  mutate(Category_factor = as.factor(Category_Code)) %>%
  mutate(date = as.Date(TimePeriod,"%d/%m/%Y")) %>%
  ggplot(aes(date, Value, col=reorder(Category, Category_Code), linetype = Category_factor, group=Category_factor)) + 
  theme_gla( y_axis_title = FALSE, free_y_facets = TRUE) +
  geom_line(linewidth=1.5) +
  scale_y_continuous(labels = function(x) paste0(x, '%'))+
  scale_x_date(breaks = "1 month",
               date_labels = "%b\n%Y",
               expand = expansion(add = c(3, 3)))+
  scale_colour_manual(values = my_catpal8) +
  guides(linetype = "none")+
  labs(title = "Percentage of adults triple vaccinated in London by ethnic group, Nov 2020 - July 2022",
       subtitle = "Cumulative percentage of adults (18+ years) who have received three COVID vaccinations",
       caption = paste0("Source: OHID COVID-19 Health Inequalities Monitoring for England (CHIME) tool, Chart: GLA demography"))
Vaccination_Eth_Line_comb


#-------------------------------------------------------------------------------

# 14) Asthma Admissions map

# Had to change categories because rates are much higher now.

hist <- hist(input_Asthma_borough$Value)
quantile(input_Asthma_borough$Value)
summarise(input_Asthma_borough, mean = mean(Value))
#range is 72.1 to 240, mean 144, median 145
# Match Admissions data with LA location data

input_Asthma_borough_geog <- input_Asthma_borough %>% 
  left_join(borough_boundaries, by=c("Area_Code"="GSS_CODE")) %>%
  mutate(rateCat = cut(Value, breaks = c(50, 125, 145, 170, 400),
                       labels = c("<125",
                                  "125 - 144",
                                  "145 - 169",
                                  "170+"), right = FALSE))

Asthma_map <- ggplot(input_Asthma_borough_geog, aes(geometry=geometry, fill=rateCat)) +
  theme_gla()+
  ggla_sf(color = "white", size = 0.1)+
  theme(legend.position = "right")+
  theme(plot.title = element_text(size = 16)) +
  scale_fill_manual(values = my_quantpal4) + 
  labs(title = "Hospital admissions for asthma in children, map by London borough (2021/22)", 
       subtitle = "Hospital admissions for asthma (under 19 years), crude rate per 100,000",
       caption = paste0("Source: OHID, Chart: GLA demography"))
Asthma_map


#-------------------------------------------------------------------------------

