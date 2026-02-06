### NM data analyzer.r
### John Stofferahn
### 16 July 2025


# Questions ---------------------------------------------------------------

# 1. Number of distinct bird calls by Location/Time Period

# 2. Number of total bird calls by Location/Time Period

# 3. Number of bird calls by monitor over month+day

# 4. Number of bird calls by time period over month+day

# Prerequisites -----------------------------------------------------------

install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(writexl)
library(dplyr)
library(lubridate)

# Import and Cleaning -----------------------------------------------------

# reading in data
# nm_test_data <- read_excel(
#   "Test file.xlsx",
#   col_names = c("monitor", "year", "month", "day", "datetime", "start", "end", "scientific_name", "common_name", "confidence_score", "filepath", "latitude", "longitude", "min_confidence"),
#   skip = 1,
#   na = c("", "NA"),
#   col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric", "text", "numeric", "numeric", "numeric")
#   )

nm_final_data <- read_excel(
  "C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx",
  col_names = c("monitor", "year", "month", "day", "datetime", "start", "end", "scientific_name", "common_name", "confidence_score", "filepath", "latitude", "longitude", "min_confidence"),
  skip = 1,
  na = c("", "NA"),
  col_types = c("text", "numeric", "numeric", "numeric", "numeric", "numeric", "numeric", "text", "text", "numeric", "text", "numeric", "numeric", "numeric")
)
# view(nm_final_data)


#  "C:\Users\johns\OneDrive\Documents\Internships, Jobs, and Careers\Nahant Marsh\Test file.xlsx",

bird1 <- read_excel("C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx", sheet = "Bird1", na = "NA")
bird2 <- read_excel("C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx", sheet = "Bird2", na = "NA")
bird3 <- read_excel("C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx", sheet = "Bird3", na = "NA")
bird4 <- read_excel("C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx", sheet = "Bird4", na = "NA")
bird5 <- read_excel("C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx", sheet = "Bird5", na = "NA")
bird6 <- read_excel("C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx", sheet = "Bird6", na = "NA")
bird7 <- read_excel("C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx", sheet = "Bird7", na = "NA")
bird8 <- read_excel("C:/Users/johns/OneDrive/Documents/Internships, Jobs, and Careers/Nahant Marsh/Nahant Marsh Final Bird Data.xlsx", sheet = "Bird8", na = "NA")

nm_all_monitors <- bind_rows(bird1, bird2, bird3, bird4, bird5, bird6, bird7, bird8)
view(nm_all_monitors)

rare_qc_birds <- c("Black-bellied Whistling-Duck", "Ross's Goose", "Surf Scoter", "White-winged Scoter", "Black Scoter", "Gray Partridge", "Red-necked Grebe", "Chuck-will's-widow", "Rufous Hummingbird", "King Rail", "Yellow Rail", "Black Rail", "Whooping Crane", "Black-necked Stilt", "Whimbrel", "Hudsonian Godwit", "Marbled Godwit", "Ruddy Turnstone", "Red-necked Phalarope", "Neotropic Cormorant", "Little Blue Heron", "Snowy Egret", "Yellow-crowned Night Heron", "White-faced Ibis", "American Goshawk", "Mississippi Kite", "American Barn Owl", "Monk Parakeet", "Western Kingbird", "Loggerhead Shrike", "Fish Crow", "Bohemian Waxwing", "Bewick's Wren", "Townsend's Solitare", "Evening Grosbeak", "Harris's Sparrow", "LeConte's Sparrow", "Nelson's Sparrow", "Spotted Towhee", "Yellow-headed Blackbird", "Great-tailed Grackle", "Prairie Warbler")
# taken from "Quad City Avian Checklist" by Kelly J. McKay, Brian L. Blevins, Jason L. Monson

# et = endangered or threatened (both protected by law)
state_et_birds <- c("Red-shouldered Hawk", "Northern Harrier", "Piping Plover", "Common Barn Owl", "Least Tern", "King Rail", "Short-eared Owl", "Long-eared Owl", "Henslow's Sparrow", "Forester's Tern", "Black Tern", "Peregrine Falcon", "Bald Eagle")
# taken from Iowa Administrative Code 571-77.2(481B) Endangered, threatened, and special concern animals


# janitor
nm_all_monitors <- nm_all_monitors |> 
  janitor::clean_names() |> 
  rename(
    monitor = 'moniter'
  )
# nm_all_monitors


# final mutations
nm_all_monitors <- nm_all_monitors |> 
  mutate(
    time_period = cut(datetime, breaks = c(40000, 49999, 189999, 199999, 210000), labels = c("Early Dawn", "Late Dawn", "Early Dusk", "Late Dusk")),
    combined_date = make_date(year, month, day),
    rare_bird = common_name %in% c(rare_qc_birds)
    )
view(nm_all_monitors)


nm_def_here <- nm_all_monitors |> 
  group_by(common_name) |> 
  filter(n() > 10)
view(nm_def_here)


# Analysis ----------------------------------------------------------------

# How many rows per monitor                             # works (normalize by days)
nm_all_monitors |> 
  count(monitor)

nm_all_monitors |> 
  ggplot(aes(x = monitor)) + 
  geom_bar()  

# how many avg rows per day per monitor (works)
avg_recordings <- nm_all_monitors |> 
  group_by(monitor, combined_date) |> 
  summarize(count = n()) |> 
  summarize(avg_count = mean(count)) 
avg_recordings |>
  ggplot(aes(x = monitor, y = avg_count)) + 
  geom_bar(stat = "identity") + 
  labs(
    title = "Average Bird Calls by Day",
    x = "Monitor Location",
    y = "Count (averaged)"
  )                             # graph works


# How many rows per monitor per date (works? Would like to make each month-day one line, then each column be how many rows for each monitor)
nm_all_monitors |> 
  group_by(month, day) |> 
  count(monitor)

nm_all_monitors |> 
  group_by(monitor) |> 
  count(day)

# nm_all_monitors |> 
#   ggplot(aes(x = monitor)) +
#   geom_bar()

# How many rows per monitor per time period (works? see above note)
nm_all_monitors |> 
  group_by(time_period) |> 
  count(monitor)

nm_all_monitors |> 
  group_by(monitor) |> 
  count(time_period)

nm_all_monitors |> 
  count(monitor, time_period) |> 
  ggplot(aes(x = monitor, y = time_period)) +
  geom_tile(aes(fill = n))                # works

# How many distinct birds per monitor (does not work)
nm_all_monitors |> 
  group_by(monitor) |> 
  count(common_name)

nm_all_monitors |> 
  distinct(monitor, month, day, common_name)

nm_all_monitors |> 
  group_by(monitor, month, day, common_name) |> 
  summarize(count=n())

nm_all_monitors |> 
  group_by(monitor, common_name) |> 
  ggplot(aes(x = monitor)) + 
  geom_bar()

# Avg confidence of rows per monitor (works)
nm_all_monitors |> 
  group_by(monitor) |> 
  summarize(avg_conf = mean(confidence))

# How many bird calls per time period (works? see above)
nm_all_monitors |> 
  group_by(time_period) |> 
  count(monitor)

# Distribution of distinct birds over time (works? would like a count for each day)
nm_all_monitors |>
  group_by(month, day) |> 
  distinct(common_name)

# Avg confidence score in time periods (works)
nm_all_monitors |> 
  group_by(time_period) |> 
  summarize(avg_conf = mean(confidence))
  
# Number of distinct species (works)
nm_all_monitors |> 
  distinct(common_name)

# Avg confidence level per species (does not work)
nm_all_monitors |> 
  group_by(common_name) |> 
  summarize(avg_conf = mean(confidence))


# num distinct species by location
dis_species_by_loc <- nm_all_monitors |> 
  group_by(monitor) |> 
  summarize(num_dist_species = n_distinct(common_name)) |> 
  arrange(desc(num_dist_species))
dis_species_by_loc |> 
  ggplot(aes(x = monitor, y = num_dist_species)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Distinct Bird Species by Location",
    x = "Monitor Location",
    y = "Count"
  )


# table with total calls, avg calls, and distinct birds
nm_all_monitors |> 
  group_by(monitor, combined_date) |> 
  summarize(total_count = n(),
            avg_count = mean(total_count), .groups = "drop",
            num_dist_species = n_distinct(common_name),
            avg_conf = mean(confidence)
            )

nm_all_monitors |> 
  group_by(monitor) |> 
  summarize(count = n(),
            # avg_count = avg_recordings,
            count_dist_species = n_distinct(common_name),
            avg_conf = mean(confidence)
  ) 
nm_all_monitors |> 
  group_by(monitor, combined_date) |> 
  summarize(count = n()) |> 
  summarize(avg_count = mean(count)) 

# most common birds                                 # lists bird freq
all_bird_freq <- nm_all_monitors |> 
  count(common_name, rare_bird, sort = TRUE)
view(all_bird_freq)

middle_bird_freq <- nm_all_monitors |> 
  count(common_name, rare_bird) |> 
  filter(count(common_name) > 100 | count(common_name) < 10000)
view(middle_bird_freq)

# least common birds
nm_all_monitors |> 
  count(common_name, sort = TRUE) |> 
  arrange(n)




# Graphs ------------------------------------------------------------------

### Ordering (done before splitting off aux dfs)

# # monitor ordering
# monitor_order <- c("CRP 1", "CRP 2", "Main Marsh", "Catwalk", "INHF", "Carp Lake", "Rockingham Bottoms", "Drainage Stream Bank")
# nm_all_monitors$monitor <- factor(nm_all_monitors$monitor, levels = monitor_order)  
# monitor_colors <- c("CRP 1" = "maroon", "CRP 2" = "red", "Main Marsh" = "orange", "Catwalk" = "yellow", "INHF" = "green", "Carp Lake" = "darkgreen", "Rockingham Bottoms" = "blue", "Drainage Stream Bank" = "purple")
# reverse_monitor_order <- c("Drainage Stream Bank", "Rockingham Bottoms", "Carp Lake", "INHF", "Catwalk", "Main Marsh", "CRP 2", "CRP 1")
# 
# monitor_order_adv <- c("INHF", "Carp Lake", "Rockingham Bottoms", "Drainage Stream Bank", "CRP 1", "CRP 2", "Main Marsh", "Catwalk")
# nm_all_monitors$monitor <- factor(nm_all_monitors$monitor, levels = monitor_order_adv)

# monitor ordering (replaced "CRP 2" with "Beaver Complex")
monitor_order <- c("CRP 1", "Beaver Complex", "Main Marsh", "Catwalk", "INHF", "Carp Lake", "Rockingham Bottoms", "Drainage Stream Bank")
nm_all_monitors$monitor <- factor(nm_all_monitors$monitor, levels = monitor_order)  
monitor_colors <- c("CRP 1" = "maroon", "Beaver Complex" = "red", "Main Marsh" = "orange", "Catwalk" = "yellow", "INHF" = "green", "Carp Lake" = "darkgreen", "Rockingham Bottoms" = "blue", "Drainage Stream Bank" = "purple")
reverse_monitor_order <- c("Drainage Stream Bank", "Rockingham Bottoms", "Carp Lake", "INHF", "Catwalk", "Main Marsh", "Beaver Complex", "CRP 1")

monitor_order_adv <- c("INHF", "Carp Lake", "Rockingham Bottoms", "Drainage Stream Bank", "CRP 1", "Beaver Complex", "Main Marsh", "Catwalk")
nm_all_monitors$monitor <- factor(nm_all_monitors$monitor, levels = monitor_order_adv)




# # switched CL and RB (didn't work, don't use)
# monitor_order_2 <- c("CRP 1", "CRP 2", "Main Marsh", "Catwalk", "INHF", "Rockingham Bottoms", "Carp Lake", "Drainage Stream Bank")
# nm_all_monitors$monitor <- factor(nm_all_monitors$monitor, levels = monitor_order_2)
# reverse_monitor_order_2 <- c("Drainage Stream Bank", "Carp Lake", "Rockingham Bottoms", "INHF", "Catwalk", "Main Marsh", "CRP 2", "CRP 1")
# monitor_colors_2 <- c("CRP 1" = "maroon", "CRP 2" = "red", "Main Marsh" = "orange", "Catwalk" = "yellow", "INHF" = "green", "Rockingham Bottoms" = "darkgreen", "Carp Lake" = "blue", "Drainage Stream Bank" = "purple")

# first and last four monitors
first_four_monitors <- c("CRP 1", "CRP 2", "Main Marsh", "Catwalk")
first_four_monitor_order <- c("CRP 1", "CRP 2", "Main Marsh", "Catwalk")
last_four_monitors <- c("INHF", "Carp Lake", "Rockingham Bottoms", "Drainage Stream Bank")
last_four_monitor_order <- c("INHF", "Carp Lake", "Rockingham Bottoms", "Drainage Stream Bank")

# top 10 most common ordering
nm_top_10_order <- c("Warbling Vireo", "Tree Swallow", "Prothonotary Warbler", "Indigo Bunting", "Red-winged Blackbird", "American Robin", "Common Grackle", "Eastern Kingbird", "Common Nighthawk", "Killdeer")


# FINAL TASKS

# 1. Make df with only num bird calls > 10 (100?)
# nm_reduced <- nm_all_monitors |> 
#   group_by(common_name) |> 
#   filter(n() > 100) |> 
#   ungroup()
# # view(nm_reduced)
# 
# nm_over_1000 <- nm_reduced |> 
#   group_by(common_name) |> 
#   filter(n() > 1000) |> 
#   ungroup()
# # view(nm_over_1000)

nm_top_10 <- nm_all_monitors |> 
  group_by(common_name) |> 
  filter(n() > 2191) # crap but whatever
# view(nm_top_10)

# nm_middle_freq <- nm_all_monitors |> 
#   group_by(common_name) |> 
#   filter(n() < 10000 | n() > 100)
# view(nm_middle_freq)


# 2. Make a df with only rare birds
nm_rare_birds <- nm_all_monitors |> 
  group_by(rare_bird) |> 
  filter(rare_bird == TRUE)
# view(nm_rare_birds)

# 3. Find a way to make graphs that can use count(variable) as an x or y variable


# 4. Normalize "total call" graphs by days, instead analyze averages



# num bird calls over time                          # works
nm_all_monitors |>
  ggplot(aes(x = combined_date, fill = monitor)) + 
  scale_fill_manual(values = monitor_colors) + #scale_fill_discrete(limits = monitor_order)
  geom_bar() +
  # facet_wrap(~monitor) +
  labs(
    title = "Count of Bird Calls over Time",
    x = "Date", y = "Count",
    fill = "Monitor Location"
  )

  # reduced to only first four monitors
nm_all_monitors |>
  filter(monitor %in% first_four_monitors) |> 
  ggplot(aes(x = combined_date, fill = monitor)) + 
  scale_fill_manual(values = monitor_colors) +
  # scale_fill_discrete(limits = first_four_monitor_order) +
  geom_bar() +
  # facet_wrap(~monitor) +
  labs(
    title = "Bird Calls over Time",
    subtitle = "First Four Monitors only",
    x = "Date", y = "Count",
    fill = "Monitor"
  )

  #reduced to only last four monitors
nm_all_monitors |>
  filter(monitor %in% last_four_monitors) |> 
  ggplot(aes(x = combined_date, fill = monitor)) + 
  scale_fill_manual(values = monitor_colors) +
  # scale_fill_discrete(limits = last_four_monitor_order) +
  geom_bar() +
  # facet_wrap(~monitor) +
  labs(
    title = "Bird Calls over Time",
    subtitle = "Last Four Monitors only",
    x = "Date", y = "Count",
    fill = "Monitor"
  )


#   # reversed to show first four always at bottom        # Jimmy preferred version
# nm_all_monitors |>
#   ggplot(aes(x = combined_date, fill = monitor)) + 
#   scale_fill_manual(limits = monitor_order_adv, values = monitor_colors) +
#   # scale_fill_discrete(limits = monitor_order) +
#   geom_bar() +
#   # facet_wrap(~monitor) +
#   labs(
#     title = "Bird Calls over Time",
#     x = "Date", y = "Count",
#     fill = "Monitor Location"
#   )

# # total count of species     # done better down below
# nm_top_10 |>
#   ggplot(aes(y = (fct_rev(common_name)))) +
#   scale_y_discrete(limits = nm_top_10_order) +
#   geom_bar() + 
#   labs(
#     title = "Species Count",
#     subtitle = "Top 10 Most Common Species",
#     x = "Total Number of Recorded Calls",
#     y = "Bird Species"
#   )

# num distinct species by location                  # how to make distinct birds?
dis_by_loc <- nm_all_monitors |> 
  group_by(monitor) |>
  summarize(
    n_distinct_birds = n_distinct(common_name)
  ) 
# dis_by_loc

dis_by_loc |> 
  ggplot(aes(x = monitor, y  = n_distinct_birds)) +
  scale_x_discrete(limits = monitor_order) +
  geom_col(fill = monitor_colors) + 
  labs(
    title = "Distinct Bird Species by Location",
    x = "Monitor Location",
    y = "Distinct Species"
  )



### fill graphs


# num rows per monitor per time period                    #works
nm_all_monitors |> 
  count(monitor, time_period) |> 
  ggplot(aes(x = monitor, y = time_period)) +
  scale_x_discrete(limits = monitor_order, guide = guide_axis(n.dodge=2)) +
  scale_fill_gradient(low = "white", high = "red") + 
  geom_tile(aes(fill = n)) +
  # geom_text(aes(label = n), color = "white", size = 4) +
  coord_fixed() + 
  labs(
    title = "Bird Calls by Time and Location",
    y = "Time of Day", 
    x = "Monitor Location",
    fill = "Number of recorded calls"
  )

# num rows of rare birds per monitor per time period          #works
# nm_all_monitors |> 
#   count(monitor, time_period, rare_bird) |> 
#   ggplot(aes(x = monitor, y = time_period)) +
#   geom_tile(aes(fill = rare_bird)) +
#   coord_fixed() + 
#   labs(
#     title = "Presence of Rare Marsh Birds by Time and Location",
#     y = "Time of Day",
#     x = "Monitor Location",
#     fill = "Presence detected?"
#   )

nm_rare_birds |> 
  count(monitor, time_period) |> 
  ggplot(aes(x = monitor, y = time_period)) +
  scale_x_discrete(limits = monitor_order, guide = guide_axis(n.dodge=2)) +
  scale_fill_gradient(low = "white", high = "red") + 
  geom_tile(aes(fill = n)) +
  coord_fixed() +
  labs(
    title = "Rare Bird Calls by Time and Location",
    y = "Time of Day",
    x = "Monitor Location",
    fill = "Number of recorded calls"
  )

# num rows per time period             # works
nm_all_monitors |> 
  count(combined_date, time_period) |> 
  ggplot(aes(x = combined_date, y = time_period)) +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_tile(aes(fill = n))+
  # coord_fixed() +
  labs(
    title = "Count of Bird Calls by Date and Time",
    x = "Date", 
    y = "Time of Day",
    fill = "Number of recorded calls"
  )

# num rows of rare birds per time period (for usage for each monitor)   # works (but janky)
# nm_all_monitors |> 
#   count(combined_date, time_period, rare_bird) |> 
#   ggplot(aes(x = combined_date, y = time_period)) +
#   geom_tile(aes(fill = rare_bird))+
#   coord_fixed() + 
#   labs(
#     title = "Rare Bird Call Presence by Time of Day",
#     x = "Date",
#     y = "Time of Day"
#   )

nm_rare_birds |> 
  count(combined_date, time_period) |> 
  ggplot(aes(x = combined_date, y = time_period)) +
  geom_tile(aes(fill = n))+
  # coord_fixed() + 
  labs(
    title = "Rare Bird Calls over Date and Time",
    x = "Date",
    y = "Time of Day"
  )

# count detected species by time                              #works (might also want to filter top 5)
nm_top_10 |> 
  count(common_name, time_period) |> 
  ggplot(aes(x = time_period, y = (fct_infreq(common_name)))) + # 
  scale_y_discrete(limits = nm_top_10_order) +
  geom_tile(aes(fill = n)) +
  # geom_text(aes(label = n), color = "white", size = 4) +
  # coord_fixed() + 
  labs(
    title = "Bird Call Frequency by Time of Day",
    subtitle = "Top 10 most common species",
    y = "Bird Species",
    x = "Time of Day",
    fill = "Number of recorded calls"
  )

# num detected species by location                             #works
nm_top_10 |> 
  count(common_name, monitor) |> 
  ggplot(aes(x = monitor, y = fct_rev(fct_infreq(common_name)))) +
  scale_y_discrete(limits = nm_top_10_order) +
  scale_x_discrete(limits = monitor_order, guide = guide_axis(n.dodge=2)) +
  scale_fill_gradient(low = "white", high = "red") + 
  geom_tile(aes(fill = n)) +
  # geom_text(aes(label = n), color = "white", size = 4) +
  coord_fixed() + 
  labs(
    title = "Bird Species by Location",
    subtitle = "Top 10 most common species",
    y = "Bird Species",
    x = "Monitor Location",
    fill = "Number of recorded calls"
  )

# # num rows by date and datetime                           # works
# nm_all_monitors |> 
#   count(combined_date, time_period) |> 
#   ggplot(aes(x = combined_date, y = time_period)) +
#   geom_tile(aes(fill = n)) +
#   # geom_text(aes(label = n), color = "white", size = 4) +
#   coord_fixed() + 
#   labs(
#     title = "Bird Calls by Time and Date",
#     y= "Time Period", 
#     x = "Date",
#     fill = "Number of recorded calls"
#   )

# num bird calls by date and location                           # works
nm_all_monitors |> 
  count(combined_date, monitor) |> 
  ggplot(aes(x = combined_date, y = monitor)) +
  scale_y_discrete(limits = reverse_monitor_order) +
  scale_fill_gradient(low = "white", high = "blue") +
  geom_tile(aes(fill = n)) +
  # geom_text(aes(label = n), color = "white", size = 4) +
  # coord_fixed() + 
  labs(
    title = "Count of Bird Calls by Date and Location",
    y= "Monitor Location", 
    x = "Date",
    fill = "Number of recorded calls"
  )

 
                                                    # i want to split by location goddammit

### bar charts

# num distinct bird species by time period
dis_by_tp <- nm_all_monitors |> 
  group_by(time_period) |>
  summarize(
    n_distinct_birds = n_distinct(common_name)
  ) 

dis_by_tp |> 
  ggplot(aes(x = time_period, y = n_distinct_birds)) + 
  geom_col() +
  labs(
    title = "Unique Bird Species by Time of Day",
    x = "Time of Day",
    y = "Count"
  )        

# num rows for each monitor                     #works (normalize by num days recording)
nm_all_monitors |> 
  ggplot(aes(x = monitor)) + 
  # scale_x_manual(values = monitor_colors) +
  scale_x_discrete(limits = monitor_order) +
  geom_bar() + 
  labs(
    title = "Recordings by Monitor",
    x = "Monitor Location",
    y = "Number of Recordings"
  )

# num rare bird rows for each monitor           #works *
nm_rare_birds |> 
  ggplot(aes(x = monitor)) +
  scale_x_discrete(limits = monitor_order) +
  geom_bar() + 
  labs(
    title = "Rare Bird Recordings by Monitor",
    x = "Monitor Location",
    y = "Number of Recordings"
  )

# distinct bird species by day per monitor        #works
ndsd <- nm_all_monitors |> 
  group_by(monitor, combined_date) |> 
  summarize(
    num_dis_species_day = n_distinct(common_name)
  ) 
ndsd |> 
  group_by(monitor) |> 
  ggplot(aes(x = monitor, y = num_dis_species_day)) + 
  scale_x_discrete(limits = monitor_order, guide = guide_axis(n.dodge=2)) +
  geom_boxplot() +
  labs(
    title = "Bird Species Richness by Location",
    # subtitle = "Each point is one day",
    x = "Monitor Location",
    y = "Distinct Species per Day"
  )


# num rows for each time period                 #works
nm_all_monitors |> 
  ggplot(aes(x = time_period, fill = monitor)) +
  scale_fill_manual(values = monitor_colors) +
  # scale_fill_discrete(limits= monitor_order) +
  geom_bar() +
  labs(
    title = "Bird Recordings by Time of Day",
    x = "Time of Day",
    y = "Number of Recordings",
    fill = "Monitor Location"
  )


# num rare bird rows for each time period       #works
nm_rare_birds |> 
  ggplot(aes(x = time_period, fill = monitor)) +
  # scale_fill_discrete(limits = monitor_order) +
  scale_fill_manual(values = monitor_colors) +
  geom_bar() +
  labs(
    title = "Rare Bird Recordings by Time of Day",
    x = "Time Period",
    y = "Number of Recordings",
    fill = "Monitor Location"
  )



# confidence by monitor                         #works
nm_all_monitors |> 
  ggplot(aes(x = monitor, y = confidence)) + 
  scale_x_discrete(limits = monitor_order) +
  geom_boxplot() + 
  labs(
    title = "Bird Recording Confidence Scores by Location",
    x = "Monitor Location",
    y = "Confidence Score of Recording"
  )

# confidence by monitor for rare birds        #works
nm_rare_birds |> 
  ggplot(aes(x = monitor, y = confidence)) + 
  scale_x_discrete(limits = monitor_order) +
  geom_boxplot() + 
  labs(
    title = "Rare Bird Confidence Scores by Location",
    x = "Monitor Location",
    y = "Confidence Score of Recording"
  )


# "rare bird" detections are often less confident - probably incorrect
rbd <- nm_all_monitors |> 
  group_by(monitor, rare_bird) |> 
  summarize(avg_conf = mean(confidence))
rbd |> 
  # group_by(monitor, rare_bird) |> 
  ggplot(aes(x = monitor, y = avg_conf, fill = rare_bird)) + 
  scale_x_discrete(limits = monitor_order, guide = guide_axis(n.dodge=2)) +
  coord_cartesian(ylim = c(0.5, 0.9)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Recording Confidence by Monitor",
    x = "Monitor Location",
    y = "Average Recording Confidence",
    fill = "Rare Bird Recording?"
  )


# most common bird call frequency                        # works (cut off top 5) *
nm_top_10 |> 
  ggplot(aes(x = (fct_infreq(common_name)), fill = monitor)) + 
  scale_x_discrete(guide = guide_axis(n.dodge=2)) +
  scale_fill_manual(limits = monitor_order, values = monitor_colors) +
  # scale_fill_discrete(limits = monitor_order) +
  geom_bar() +
  labs(
    title = "Location Distribution of Most Common Bird Species",
    y = "Number of recorded calls",
    x = "Bird Species",
    fill = "Monitor Location"
  )


# bird call frequency                           # not really useful (but works)
nm_all_monitors |> 
  ggplot(aes(x = fct_infreq(common_name), fill = monitor)) +
  scale_fill_manual(values = monitor_colors) +
  scale_x_discrete(labels = NULL) + 
  # scale_fill_discrete(limits = monitor_order) +
  geom_bar() +
  labs(
    title = "Location Distribution of All Bird Species",
    x = "Bird Species",
    y = "Number of recorded calls",
    fill = "Monitor Location"
  )


# Notes -------------------------------------------------------------------


# saving plots
ggsave(filename = "[insert filename here].png")


# miscellaneous

# guide to ggplot
# ggplot(data = <DATA>) + 
#   <GEOM_FUNCTION>(
#     mapping = aes(<MAPPINGS>),
#     stat = <STAT>, 
#     position = <POSITION>
#   ) +
#   <COORDINATE_FUNCTION> +
#   <FACET_FUNCTION>
