#load packages
library(plyr)
library(tidyverse)
library(ggplot2)
library(geojsonio)

##### Loading and cleaning data #####

# Selecting columns to load
s1 <- read.csv("Sydney_listings.csv", encoding = "UTF-8", nrows = 1)
nc <- ncol(read.csv("Sydney_listings.csv", encoding = "UTF-8", nrows = 1))
colClasses <- replace(rep("NULL", nc), c(1,20:24,32:54,61:63,68,69,83,87,102:105), NA)

# Loading files for each city
sy <- read.csv("Sydney_listings.csv", encoding = "UTF-8", colClasses = colClasses)
ny <- read.csv("NY_listings.csv", encoding = "UTF-8", colClasses = colClasses)
paris <- read.csv("Paris_listings.csv", encoding = "UTF-8", colClasses = colClasses)
tokyo <- read.csv("Tokyo_listings.csv", encoding = "UTF-8", colClasses = colClasses)
barca <- read.csv("Barca_listings.csv", encoding = "UTF-8", colClasses = colClasses)
beijing <- read.csv("Beijing_listings.csv", encoding = "UTF-8", colClasses = colClasses)

# Combining all cities in one dataframe
airbnb <- rbind.fill(sy,ny,paris,tokyo,barca,beijing)

#Add the host creation year for each listing
airbnb$creation_year <- year(ymd(airbnb$host_since))

#Add the number of different host created each year in each country/city
airbnb <- ddply(airbnb,c("country","creation_year"),mutate,nb_hosts=n_distinct(host_id))

#Remove Switzerland as it is an error in the data
airbnb <- airbnb[!(airbnb$country == "Switzerland"),]


##### Graphic 1: Number of hosts by city by year #####

#Compute the number of different host created each year in each country/city
#Add the total number of hosts per country/city and year
hosts_country_year <- ddply(airbnb,c("country","creation_year"),summarise,nb_hosts=n_distinct(host_id))
hosts_country_year <- ddply(hosts_country_year,"country",mutate,total_hosts=cumsum(nb_hosts))
hosts_country_year$creation_year <- factor(hosts_country_year$creation_year,
                                           min(hosts_country_year$creation_year,na.rm = TRUE):max(hosts_country_year$creation_year,na.rm = TRUE),
                                           ordered = TRUE)
hosts_country_year <- hosts_country_year[!(hosts_country_year$country == "Switzerland"),]
hosts_country_year <- drop_na(hosts_country_year)
hosts_country_year$country <- factor(hosts_country_year$country,
                                     c("Australia","France","Japan","China","Spain","United States"))
hosts_country_year <- data.frame(complete(hosts_country_year,
                                          country,
                                          creation_year,
                                          fill = list(total_hosts = 0,
                                                      nb_hosts = 0)))

# New facet label names for country
location.labs <- c("Paris","Tokyo","Sydney","Beijing","Barcelone","New York")
names(location.labs) <- c("France","Japan","Australia","China","Spain","United States")

# Plotting
ggplot(hosts_country_year,
       aes(x = creation_year, y = nb_hosts, group = 1)) +
  geom_col(aes(fill = "Created per year")) +
  geom_line(aes(x = creation_year, y = total_hosts, color = "Total per year")) + 
  facet_wrap(~country,
             nrow = 3,
             scales = "free",
             labeller = labeller(country = location.labs)) +
  scale_color_manual(values = "#d01c8b", name = "") +
  scale_fill_manual(values = "#b8e186", name = "") +
  labs(title = "Evolution of the number of hosts from 2008 to 2019",
       x = "Year",
       y = "Number of hosts") +
  theme(plot.title = element_text(hjust = 0.5, size = 22),
        strip.text = element_text(size = 18),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16))
ggsave("../Graphes/nb_hosts.pdf",width = 16, height = 9)
ggsave("./nb_hosts.png",width = 16, height = 9)


##### Graphic 2: Night price distribution by city #####

# Comparing the night price distribution by city
# Change price as numeric
airbnb$price <- as.numeric(gsub('[$,]', '', airbnb$price))
# Plotting (hiding Tokyo data as it is not correct)
ggplot(airbnb[!(airbnb$country=="Japan") & airbnb$accommodates >= 2,],
       aes(x = country, y = price, fill = country)) +
  geom_boxplot(outlier.shape = NA) +
  scale_y_continuous(name = "Price",
                     limits = quantile(airbnb$price, c(0.1, 0.9)),
                     labels = scales::dollar_format()) +
  scale_x_discrete(name = "City", labels = location.labs) +
  theme(legend.position = "None",
        axis.text = element_text(size = 24),
        plot.title = element_text(hjust = 0.5, size = 32),
        axis.title = element_text(size = 24),
        axis.title.x = element_text(margin = margin(t = 10)),
        axis.title.y = element_text(margin = margin(r = 10))) +
  labs(title = "Price distribution") +
  scale_fill_brewer(palette = "PiYG")
ggsave("../Graphes/price_dist.pdf",width = 16, height = 9)
ggsave("./price_dist.png",width = 16, height = 9)


##### Graphic 3: Percentage of listing type by maximum stay duration and by city #####

#Number of listings by listing type
tt <- mutate(airbnb[airbnb$accommodates >= 2,],
             stay_duration = case_when(
               minimum_nights >= 1 & maximum_nights <= 2 ~ "1-2",
               minimum_nights >= 1 & maximum_nights > 2 & maximum_nights <= 7 ~ "1-7",
               minimum_nights >= 1 & maximum_nights > 7 & maximum_nights <= 14 ~ "1-14",
               minimum_nights >= 1 & maximum_nights > 14 & maximum_nights <= 31 ~ "1-31",
               minimum_nights >= 1 & maximum_nights >31 & maximum_nights <= 90 ~ "1-90",
               minimum_nights >= 1 & maximum_nights >90 ~ ">90"
             ))
tt$stay_duration <- factor(tt$stay_duration,
                           levels = c("1-2","1-7","1-14","1-31","1-90",">90"),
                           labels = c("2", "3-7", "8-14","15-31","32-90",">90"),
                           ordered = TRUE)
# Proportion of type of rooms by duration
tmp1 <- ddply(tt,c("country","stay_duration"), mutate,nb_per_duration=n())
# Plotting
ggplot(ddply(tmp1,c("country","room_type","stay_duration"), dplyr::summarise, nb_per_type=n(), proportion=nb_per_type/unique(nb_per_duration)),
       aes(x = stay_duration, y = proportion, fill = room_type)) +
  geom_col(position = position_dodge2(width = 5, preserve = "single")) +
  facet_wrap(~country,
             labeller = labeller(country = location.labs)) +
  scale_fill_brewer(palette = "PiYG") +
  scale_y_continuous(labels = scales::percent) +
  labs(title = "Proportion of listing type by maximum number of nights and by country",
       x = "Maximum number of nights",
       y = "Percentage",
       fill = "Listing type") +
  theme(axis.text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, size = 24),
        axis.title = element_text(size = 20),
        strip.text = element_text(size = 20),
        legend.title = element_text(hjust = 0.5, size = 20),
        legend.text = element_text(size = 18))
ggsave("../Graphes/type_nbnight.pdf",width = 16, height = 9)
ggsave("./type_nbnight.png",width = 16, height = 9)


##### Graphic 4: Price comparison by borough in New York #####

# Map of NYC with neighborhoud group (borough) boundaries
URL <- "https://services5.arcgis.com/GfwWNkhOj9bNBqoJ/arcgis/rest/services/nybb/FeatureServer/0/query?where=1=1&outFields=*&outSR=4326&f=geojson"
fil <- "nyc_borough_boundaries.geojson"
if (!file.exists(fil)) download.file(URL, fil)

nyc_borough <- geojson_read(fil, what="sp")
nyc_borough_map <- fortify(nyc_borough, region="BoroName")

# Plotting
ggplot() +
  geom_polygon(data=nyc_borough_map,
               aes(x=long, y=lat, group=group),
               fill = "snow2",
               color = "snow3") +
  geom_point(data = airbnb[airbnb$country=="United States" & airbnb$accommodates>=2 & (airbnb$room_type=="Private room" | airbnb$room_type=="Entire home/apt"),],
             mapping = aes(x = longitude, y = latitude, color = cut(price,c(-Inf,50,75,100,125,150,175,200,225,250,Inf))),
             size = 0.5) +
  scale_color_brewer(palette = "PiYG",
                     direction = -1,
                     name = "Price ($)",
                     labels = c("<= 50",
                                "50.1 - 75",
                                "75.1 - 100",
                                "100.1 - 125",
                                "125.1 - 150",
                                "150.1 - 175",
                                "175.1 - 200",
                                "200.1 - 225",
                                "225.1 - 250",
                                "> 250")) +
  guides(color = guide_legend(override.aes = list(size = 8))) +
  labs(title = "Price per night in New York") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "snow3"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        strip.text = element_text(size = 20),
        legend.key = element_rect(fill = NA),
        legend.title = element_text(hjust = 0.5, size = 20),
        legend.text = element_text(hjust = 0.5, size = 18),
        plot.title = element_text(hjust = 0.5, size = 24)) +
  facet_wrap(~room_type)
ggsave("./price_nyc.png",width = 16, height = 9)
ggsave("../Graphes/price_nyc.pdf",width = 16, height = 9)


##### Graphic 5: Average ratings by neighborhoud in New York #####

# Map of NYC with neighborhouds boundaries
fic <- "neighbourhoods.geojson"
nyc_neighbourhoods <- geojson_read(fic, what = "sp")
nyc_n_map <- fortify(nyc_neighbourhoods, region="neighbourhood")
nyc_n_map$neighbourhood_cleansed <- factor(nyc_n_map$id)
# Set the same list of possible neighbourhoods
ny$neighbourhood_cleansed <- factor(ny$neighbourhood_cleansed, levels = levels(nyc_n_map$neighbourhood_cleansed))
# Compute the average review rating by neighbourhood
nyc_scores <- ddply(ny[ny$accommodates>=2 & ny$room_type=="Entire home/apt",],"neighbourhood_cleansed",summarise, mean_rating = mean(review_scores_rating, na.rm = TRUE))
# Map of NYC by neighbourhood with mean ratings
nn_map <- dplyr::full_join(nyc_n_map,nyc_scores,by = "neighbourhood_cleansed")
# Plotting
ggplot() +
  geom_polygon(data = nn_map,
               aes(x = long,
                   y = lat,
                   group = group,
                   fill = cut(mean_rating,
                              breaks = c(0,75,80,85,90,95,100),
                              labels = c("0% - 75%",
                                         "76% - 80%",
                                         "81% - 85%",
                                         "86% - 90%",
                                         "91% - 95%",
                                         "96% - 100%"))),
               color = "snow3") +
  scale_fill_manual(name = "Average review rating",
                    values = c('#8e0152','#c51b7d','#7fbc41','#4d9221','#276419'),
                    na.value = "#fde0ef") +
  labs(title = "Average rating for entire housings by neighborhoud in New York") +
  theme(panel.grid = element_blank(),
        panel.background = element_rect(fill = "snow2"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        legend.key = element_rect(fill = NA),
        legend.title = element_text(hjust = 0.5, size = 24),
        legend.text = element_text(size = 22),
        plot.title = element_text(hjust = 0.5, size = 31))
ggsave("./ratings_nyc.png",width = 16, height = 9)
ggsave("../Graphes/ratings_nyc.pdf",width = 16, height = 9)
