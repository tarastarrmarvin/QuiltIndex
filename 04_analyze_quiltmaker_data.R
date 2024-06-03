#QuiltIndex data has been unnested and cleaned. Now it's time to analyze

install.packages("ggplot2")
install.packages("sf")
install.packages("rnaturalearth")
install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
install.packages("waffle")
install.packages("ggalluvial")
install.packages("treemapify")

library("rnaturalearthhires")
library("ggplot2")
library("scales")
library("sf")
library("rnaturalearth")
library("tidyverse")
library("waffle")
library("naniar")
library("ggalluvial")
library("treemapify")


#Read data ----------------------
clean_maker_data <- read.csv("03_cleaned_maker_data.csv",
                             colClasses = c(quiltmaker_id = "character"))

read_problems <- problems(clean_maker_data)

#Read travel data ----------------------
clean_travel_data <- read.csv("03_travel_data.csv")

read_problems <- problems(clean_travel_data)


#Prepare data ------------------------
clean_maker_data$quilt_time_period_end <- as.integer(clean_maker_data$quilt_time_period_end)
clean_maker_data$quilt_time_period_start <- as.integer(clean_maker_data$quilt_time_period_start)

clean_maker_data <- clean_maker_data %>%
  mutate(cleaned_quilt_time_period = 
           fct_relevel(cleaned_quilt_time_period,
                       "1700-1799", 
                       "1800-1849", 
                       "1850-1875",
                       "1876-1900",
                       "1901-1929",
                       "1930-1949",
                       "1950-1975",
                       "1976-1999",
                       "2000-2025")
  )

clean_maker_data <- clean_maker_data %>%
  mutate(number_quilts = fct_relevel(number_quilts,
                                     "1-5 quilts",
                                     "5-20 quilts",
                                     "20-50 quilts",
                                     "more than 50"))

clean_maker_data <- clean_maker_data %>%
  mutate(when_learned_to_quilt = fct_relevel(when_learned_to_quilt,
                                     "Under 10 years of age",
                                     "Age 11-19",
                                     "Age 20-29",
                                     "Age 30-39",
                                     "Age 40-49",
                                     "Age 50 or over",
                                     "After raising children",
                                     "After retiring",
                                     "After an illness"))

clean_maker_data <- clean_maker_data %>%
  mutate(quiltmaker_why = fct_relevel(quiltmaker_why,
                                 "Necessity",
                                 "Gifts",
                                 "Church",
                                 "Fundraising",
                                 "Therapy",
                                 "Income",
                                 "Other",
                                 "Pleasure"))
                                 
clean_maker_data <- clean_maker_data %>%
  mutate(how_learned_to_quilt = fct_relevel(how_learned_to_quilt,
                                            "From Relative",
                                            "From 4-H Extension Agent",
                                            "From Home Extension Agent",
                                            "Other",
                                            "From TV show",
                                            "From Guild or Club Member",
                                            "From Friend",
                                            "From Class",
                                            "Self-taught"))

clean_maker_data <- clean_maker_data %>%
  mutate(
    number_quilts_min = case_when(
      number_quilts == "1-5 quilts" ~ 1,
      number_quilts == "5-20 quilts" ~ 5,
      number_quilts == "20-50 quilts" ~ 20,
      number_quilts == "more than 50" ~ 50,
      .default = NA
    ),
    number_quilts_max = case_when(
      number_quilts == "1-5 quilts" ~ 5,
      number_quilts == "5-20 quilts" ~ 20,
      number_quilts == "20-50 quilts" ~ 50,
      number_quilts == "more than 50" ~ 105,
      #it actually goes to infinity but our largest data point is 102, so...
      .default = NA
    )
  )
    
clean_travel_data$country <- as.factor(clean_travel_data$country)

clean_travel_data <-clean_travel_data %>%
  mutate(location = fct_relevel(location,
                                "birth_place_country",
                                "quilt_country",
                                "country"))
                                 
                                
#Create foundation layers that can be re-used with many charts -----------
custom_colors <- colorRampPalette(
  c( '#484878', '#88CCEE','#44AA99','#117733','#999933','#DDCC77',   
      '#CC6677', '#882255', "#000000"))

basic_layers <- list(
  theme_minimal(),
  guides(fill = guide_legend(title.position = "top", title.hjust=0.5)),
  theme(legend.position="bottom",
        legend.key.width = unit(1.5, "cm"),
        plot.margin = margin(30,30,30,30),
        plot.subtitle = element_text(margin=margin(10, 0, 50, 0)),
        axis.title.x = element_text(margin=margin(20,0,0,0)),
        axis.title.y = element_text(margin=margin(0,20,0,0))),
  labs(y="Count")
)


geography_layers <- list(
  theme_void(),
  scale_fill_gradient2(
    low = "#44AA99",
    mid = "#CC6677",
    high = "#484878",
    midpoint=1700,
    name = "Number of makers",
    n.breaks = 6,
    na.value = "#eeeeee"),
  guides(fill = guide_colorbar(title.position = "top", title.hjust=0.5)),
  theme(legend.position="bottom",
        legend.key.width = unit(1.5, "cm"),
        plot.margin = margin(30,30,30,30),
        plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5, margin=margin(10, 0, 50, 0)))
)

global_geography_layers <- list(
  geom_sf(colour="#ffffff"),
  coord_sf(
    expand = TRUE,
    default_crs = sf::st_crs(4326),
    xlim = c(-180, 180),
    ylim = c(-50, 90))
)

# How many makers are we analyzing?  ----------------------------------
total_number_of_quiltmakers <- clean_maker_data %>%
  distinct(quiltmaker_id) %>%
  count() %>% as.integer()

# How many quilts did they make? ---------------------------------------
total_number_of_quilts <- clean_maker_data %>%
  distinct(quilt_id) %>%
  count() %>% as.integer()

#Status of the data -----------------------------------------------------
plot <- gg_miss_var(clean_maker_data, show_pct=TRUE, 
                    facet=cleaned_quilt_time_period) 

plot + 
  basic_layers +
  labs(title="Status of Data - Over Time",
       subtitle = "Generally, data about more recent quilts is more complete data",
       y="Percent missing",
       x="Data variables")

##number_quilts vs how many maker has in database-------------
plot_data <- clean_maker_data %>% 
  distinct(quilt_id, quiltmaker_id, number_quilts) %>%
  count(quiltmaker_id, number_quilts) %>%
  group_by(number_quilts) %>%
  summarise_at(vars(n), list(mean_quilts_entered = mean)) 

plot_data %>%
  ggplot(aes(x = number_quilts, y=mean_quilts_entered)) + 
  basic_layers +
  geom_col(fill = "#484878") +
  geom_text(aes(label = signif(mean_quilts_entered, digits=2)),
            nudge_y = -0.25) 

plot_data <- clean_maker_data %>%
  filter(!is.na(number_quilts)) %>%
  distinct(quilt_id, quiltmaker_id, number_quilts, number_quilts_min, number_quilts_max) %>%
  count(quiltmaker_id, number_quilts, number_quilts_min, number_quilts_max) %>%
  group_by(number_quilts) 

plot_data %>% 
  ggplot() +
  geom_tile(aes(x=number_quilts, 
                y=number_quilts_min+(number_quilts_max-number_quilts_min)/2,
                height=number_quilts_max-number_quilts_min,
                fill=number_quilts),
            alpha = 0.5,
            stat = "unique") +
  scale_fill_manual(name="Estimated # of quilts\nmade by maker", 
                    values=custom_colors(6)) +
  geom_point(
    aes(x = number_quilts, y = n, colour=n, shape=""),
    position = position_jitter(seed = 10, height=.1),
    size = 1,
    colour="#484878") +
  scale_shape_manual(name="Actual # of quilts\ndocumented for maker", 
                     values=16) +
  basic_layers +
  theme(legend.position="right",
        legend.title.align = 0,
        axis.text.x=element_blank())+
  guides(fill = guide_legend(order = 2), 
         shape = guide_legend(order = 1)) +
  labs(
    title = "Number of Quilts per Maker",
    subtitle = "The number of quilts documented in QuiltIndex.org for each maker is usually less 
    than the number of quilts made by each maker (as estimated by the reporter). The 
    discrepancy is greatest for master makers. 
    
    Many makers have only one quilt documented in QuiltIndex.org.",
    x=element_blank(),
    y = "Number of quilts") 


# What genders are these makers? --------------------------------------
plot_data <- clean_maker_data %>%
  distinct(quiltmaker_id, gender) %>%
  count(gender)
  
plot_data %>%
  ggplot(aes(x = gender, y = n)) +
  basic_layers +
  geom_col(fill = "#44AA99") +
  geom_text(aes(label = n),
            nudge_y = 150) 

plot_data %>% 
  ggplot(aes(fill = gender, values = n))  +
  basic_layers +
  geom_waffle(n_rows = 29, size = 1, colour = "white") +
  scale_fill_manual(values=c("#44AA99","#484878")) +
  coord_equal() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(), 
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        panel.grid = element_blank()) +
  labs(title = "Gender of Maker",
       subtitle = "These makers are overwhelmingly female.",
       fill="Gender")


# What countries do these makers live in? -------------------------------
plot_data <- clean_maker_data %>% 
  distinct(quiltmaker_id, country) %>%
  count(country)

plot_data %>%
  ggplot(aes(fct_reorder(country, -n), n))  +
  basic_layers +
  geom_col(fill="#44AA99") +
  geom_text(aes(label = n),
            nudge_y = -100) 

plot_data_with_geography <-
  left_join(ne_countries(scale = "medium"),
            plot_data,
            join_by(admin == country))

plot_data_with_geography %>% 
  ggplot(aes(geometry = geometry, fill=n)) + 
  geography_layers +
  global_geography_layers +
  labs(title="Country of Maker",
       subtitle="These makers live in 9 different countries. 
       Most are from North America.")

#For the US, what states do they live in? ------------------------------

plot_data <- clean_maker_data %>% 
  filter(country == "United States of America") %>%
  distinct(quiltmaker_id, state) %>%
  count(state)

us_geography <- ne_states(country = "United States of America") %>%
  st_shift_longitude()

plot_data_with_geography <-
  left_join(us_geography,
            plot_data,
            join_by(name==state))

plot_data_with_geography %>%
  arrange(desc(n)) %>% head(20) %>%
  ggplot(aes(fct_reorder(name, -n, .na_rm = FALSE), n)) +
  basic_layers + 
  geom_col(fill="#44AA99")

plot_data_with_geography %>%
  arrange(desc(n)) %>% tail(20) %>%
  ggplot(aes(fct_reorder(name, -n, .na_rm = FALSE), n)) +
  geom_col(fill="#44AA99") +
  basic_layers

ggplot(data = plot_data_with_geography, aes(geometry = geometry)) +
  geom_sf(aes(fill = n)) +
  geography_layers +
  scale_fill_gradient2(
    low = "#44AA99",
    mid = "#CC6677",
    high = "#484878",
    midpoint=400,
    name = "Number of makers",
    n.breaks = 6,
    na.value = "#ffffff") +
  labs(
    title = "Makers in the US",
    subtitle = "North Carolina, Pennsylvania, and Iowa are well represented,
       while New Mexico and Alaska are not represented."
  )

#For Canada, what provinces do they live in? --------------------------------
plot_data <- clean_maker_data %>% 
  filter(country == "Canada") %>%
  distinct(quiltmaker_id, province) %>%
  count(province)

plot_data_with_geography <-
  left_join(ne_states(country = "Canada"),
            plot_data,
            join_by(name==province))

plot_data_with_geography %>%
  ggplot(aes(fct_reorder(name, -n, .na_rm = FALSE), n)) +
  geom_col(fill="#44AA99") +
  basic_layers 

ggplot(data = plot_data_with_geography, aes(geometry = geometry)) + 
  geom_sf(aes(fill=n))+
  geography_layers +
  scale_fill_gradient2(
    low = "#44AA99",
    mid = "#CC6677",
    high = "#484878",
    midpoint=90,
    name = "Number of makers",
    n.breaks = 6,
    na.value = "#eeeeee") +
  labs(title="Makers in Canada",
       subtitle="Alberta is most represented while Newfoundland and Labrador,
       Nunavut, Northwest Territories, and Prince Edward Island are not
       represented.")

#Immigrant status vs. not --------
plot_data <- clean_maker_data %>% 
  distinct(quiltmaker_id, immigrant) %>%
  count(immigrant)

plot_data %>%
  ggplot(aes(x = immigrant, y = n)) +
  basic_layers +
  geom_col(fill = "#882255") +
  geom_text(aes(label = n),
            nudge_y = -100) +
  scale_x_discrete(labels=c("No", "Yes")) +
  labs(title = "Immigrant Status of Maker", 
       subtitle = "Most of these makers are not immigrants.",
       x = "Immigrant status", 
       y = "Number of makers")

#What countries were these makers born in? -------------------------
plot_data <- clean_maker_data %>% 
  distinct(quiltmaker_id, birth_place_country) %>%
  count(birth_place_country)

plot_data %>%
  ggplot(aes(fct_reorder(birth_place_country, -n), n)) +
  geom_col(fill="#44AA99") +
  basic_layers 

plot_data_with_geography <-
  left_join(ne_countries(scale = "medium"),
            plot_data,
            join_by(admin==birth_place_country))

plot_data_with_geography %>% 
  ggplot(aes(geometry = geometry, fill=n)) + 
  geography_layers + 
  global_geography_layers +
  labs(title="Birth Country of Maker",
       subtitle="These makers were born in 29 different countries.")

#What environment did they grow up in? -------------------------
plot_data <- clean_maker_data %>% 
  distinct(quiltmaker_id, childhood_environment) %>%
  count(childhood_environment) %>% 
  mutate(childhood_environment = 
           fct_explicit_na(childhood_environment, "Unknown"))

plot_data %>%
  ggplot(aes(childhood_environment, n)) +
  geom_col(fill="#44AA99") 

plot_data %>%
  ggplot(aes(area = n, 
             fill = childhood_environment,
             label = paste(childhood_environment, n, sep = "\n"))) +
  geom_treemap()+
  geom_treemap_text(colour = "white", place = "centre", size = 15)+
  scale_fill_manual(values=custom_colors(9))+ 
  basic_layers +
  theme(legend.position = "none") +
  labs(title="Childhood Environment of Maker",
       subtitle="The childhood environment of the maker is often unknown,
       but when it is known, it is predominantly rural",
       y=element_blank())


#What is the travel path? -------------
plot_data <- clean_travel_data %>%
  mutate(location = location %>% as_factor %>% fct_recode(
    "Birth Country" = "birth_place_country",
    "Quilt Making Country" = "quilt_country",
    "Most Recent Country" = "country"
  ))

plot_data %>%
  ggplot(aes(x = location, 
             stratum = country, 
             alluvium = quiltmaker_id, 
             fill = country)) +
  geom_stratum(color="white") + 
  geom_flow() +
  basic_layers +
  scale_fill_manual(values = custom_colors(33)) +
  labs(title="Immigration and Quilt Making Path",
       subtitle="Most of these immigrants move to the US and 
       most quilt making happens after immigration.",
       x="Location", 
       y = "Number of makers",
       fill="Country") +
  geom_segment(aes( 
    x = 0.75, xend = 3.25,
    y = 0, yend = 0),
    arrow = arrow(length=unit(0.40,"cm"), 
                  ends="last", 
                  type = "closed"))
  

#When were the quilts made? ----------------------------------------
plot_data <- clean_maker_data %>% 
  distinct(quilt_id, quilt_country, cleaned_quilt_time_period, 
           quilt_time_period_start, quilt_time_period_end) %>%
  count(quilt_country, 
        cleaned_quilt_time_period, 
        quilt_time_period_start, 
        quilt_time_period_end) %>%
  mutate(avg = n/(quilt_time_period_end - quilt_time_period_start))

plot_data_with_geography <-
  left_join(plot_data,
            ne_countries(scale = "medium"),
            join_by(quilt_country==admin))

plot_data_with_geography %>%
  ggplot(aes(x = cleaned_quilt_time_period, y=avg)) + 
  geom_col(fill=custom_colors(1)) +
  basic_layers +
  labs(title="Time Period of Quilt Making",
       subtitle="Quilt making peaked in the 1930s-1940s and is currently
       experiencing a resurgence.",
       x="Quilt time period",
       y="Average/year")

#How old are makers when they are making quilts? ------------------------
production_age_data <- clean_maker_data %>%
  distinct(quilt_id, quiltmaker_id, age_at_production) %>%
  filter(!is.na(age_at_production) & age_at_production > 0
         & age_at_production <= 110) 

plot_data <- production_age_data %>%
  count(age_at_production)

plot_data <- plot_data %>%
  mutate(section = case_when(
    age_at_production <=20 | age_at_production >73 ~ "one",
    age_at_production >20 & age_at_production <=47 ~ "two",
    age_at_production > 47 & age_at_production <= 73 ~ "three",
    TRUE ~ NA
  ))
  
plot_data %>%
  ggplot(aes(x = age_at_production, y=n)) +
  geom_col(aes(fill=section), show.legend=FALSE) +
  basic_layers +
  scale_fill_manual(values=custom_colors(9)) +
  geom_smooth(method="gam", se=FALSE) +
  labs(title = "Age of Maker",
       subtitle = "Quilt making is substantial through early adulthood, but 
       quilts are made more between the ages of 47 and 73",
       x="Maker's age",
       y="Number of quilts")

#What is the most likely quiltmaking production cycle? -------------------------------
plot_data <- production_age_data %>%
  count(quiltmaker_id, age_at_production) %>%
  group_by(age_at_production) %>%
  summarise_at(vars(n), list(mean_quilts_produced = mean,
                             max_quilts_produced = max))

plot_data %>%
  ggplot(aes(age_at_production, mean_quilts_produced)) + 
  geom_col(fill="#44AA99") +
  basic_layers +
  ylim(NA, 3) +
  labs(title = "Quilt Production - Average",
       subtitle = "There is no overall trend of makers increasing or decreasing 
       production throughout their life.",
       x="Maker's age",
       y="Average # of quilts/maker")

plot_data %>%
  ggplot(aes(age_at_production,max_quilts_produced)) + 
  geom_col(fill="#44AA99") +
  basic_layers +
  geom_smooth(method="gam", se=FALSE, colour="#005889") +
  labs(title = "Quilt Production - Maximum",
       subtitle = "Master makers tend to increase their production of 
       quilts during their middle adulthood.",
       x="Maker's age",
       y="Average # of quilts/maker")

plot_data <- production_age_data %>% 
  distinct(quiltmaker_id, age_at_production) %>%
  group_by(quiltmaker_id) %>%
  filter(age_at_production == min(age_at_production)) %>%
  ungroup() %>%
  count(age_at_production)

plot_data %>%
  ggplot(aes(age_at_production, n)) +
  geom_col(fill="#44AA99") +
  basic_layers +
  geom_smooth(method="gam", se=FALSE, colour="#005889") +
  labs(title="Quilt Production - First Quilt",
       subtitle="Many makers begin quilting in early adulthood, but even
       more begin in middle adulthood.",
       x="Maker's age",
       y="Number of first quilts")

#When do makers learn to quilt? --------------------------------------
plot_data <- clean_maker_data %>%
  filter(!is.na(when_learned_to_quilt)) %>%
  distinct(quiltmaker_id, when_learned_to_quilt, cleaned_quilt_time_period) %>%
  count(when_learned_to_quilt, cleaned_quilt_time_period) %>%
  group_by(cleaned_quilt_time_period) %>%
  mutate(pct = prop.table(n))

plot_data %>%
  ggplot(aes(cleaned_quilt_time_period, pct)) +
  geom_col(fill="#44AA99") +
  facet_wrap(vars(when_learned_to_quilt))

plot_data %>%
  ggplot(aes(cleaned_quilt_time_period, pct, fill=when_learned_to_quilt)) +
  geom_col(position="fill") +
  scale_fill_manual(values = custom_colors(9)) +
  basic_layers +
  labs(title="When Maker Learns To Quilt",
       subtitle="It is becoming more common to learn quilting at an older age.",
       x="Quilt time period",
       y="Percentage",
       fill="When")

#How do makers learn to quilt? --------------------------------------
plot_data <- clean_maker_data %>%
  filter(!is.na(how_learned_to_quilt)) %>%
  distinct(quiltmaker_id, how_learned_to_quilt, cleaned_quilt_time_period) %>%
  count(how_learned_to_quilt, cleaned_quilt_time_period) %>%
  group_by(cleaned_quilt_time_period) %>%
  mutate(pct=prop.table(n))

plot_data %>%
  ggplot(aes(cleaned_quilt_time_period, pct)) +
  geom_col(fill="#44AA99") +
  facet_wrap(vars(how_learned_to_quilt))

plot_data %>%
  ggplot(aes(cleaned_quilt_time_period, pct, fill=how_learned_to_quilt)) +
  geom_col(position="fill") +
  scale_fill_manual(values = custom_colors(9)) +
  basic_layers +
  labs(title="How Maker Learns To Quilt",
       subtitle="It is becoming less common to learn quilting from a relative
       and more common to teach oneself, take a class, or learn from a friend,
       guild or club member",
       x="Quilt time period",
       y="Percentage",
       fill="How")


#Why do makers make quilts? -------------------------------------
plot_data <- clean_maker_data %>%
  filter(!is.na(quiltmaker_why)) %>%
  distinct(quiltmaker_id, 
           quiltmaker_why, 
           cleaned_quilt_time_period) %>%
  count(quiltmaker_why, cleaned_quilt_time_period) %>%
  group_by(cleaned_quilt_time_period) %>%
  mutate(pct_of_time_period = prop.table(n))

plot_data %>%
  ggplot(aes(fct_infreq(quiltmaker_why,n), n)) +
  geom_col(fill="#44AA99")

plot_data %>%
  ggplot(aes(cleaned_quilt_time_period, pct_of_time_period)) +
  geom_col(fill="#44AA99") +
  facet_wrap(vars(quiltmaker_why))


plot_data %>%
  ggplot(aes(cleaned_quilt_time_period, pct_of_time_period, fill=quiltmaker_why)) +
  geom_col(position="fill") +
  scale_fill_manual(values = custom_colors(9)) +
  basic_layers+
  labs(title="Why Maker Quilts",
       subtitle="It is becoming less likely to quilt for necessity 
       and more likely to quilt for pleasure or therapy",
       x="Quilt time period",
       y="Percentage",
       fill="Reason")
