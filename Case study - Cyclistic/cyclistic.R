install.packages('tidyverse')
install.packages('rio')
install.packages('hms')


library('tidyverse')
library('rio')
library('hms')

# Merging files .csv into one single file. 

bikes_data = list.files('C:\\Users\\luisf\\Desktop\\database\\database_by_month',
                        pattern='*.csv', full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows

export(bikes_data, file = 'C:\\Users\\luisf\\Desktop\\database\\cyclistic_year.csv')

# -------------------------------------------------------------------------------------
# It´s not necessary reload the file.
bikes_data = read_csv("C:\\Users\\luisf\\Desktop\\database\\Results\\cyclistic_tripdata_year.csv")


glimpse(bikes_data)
summary(bikes_data)

## Removing possible duplicate lines

glimpse(distinct(bikes_data))

## With glimpse() function, we can see that there wasn't any duplicate lines

summary(bikes_data)

## Adding column trip_time (ended_at - started_at). 

bikes_data["trip_time"] <- as_hms(bikes_data$ended_at - bikes_data$started_at)

glimpse(bikes_data)
summary(bikes_data)

## Adding column weekday

bikes_data["Weekday_start"] <- weekdays(bikes_data$started_at)

glimpse(bikes_data)
summary(bikes_data)

## Finding NA's in the dataset

n_row <-nrow(bikes_data)
round(colSums(is.na(bikes_data)*100/n_row), 2)

# After processing, the dataset will be save

export(bikes_data, file = 'C:\\Users\\luisf\\Desktop\\database\\Results\\Cyclistic_tripdata_year_clear.csv')

## Descriptive analysis ----------------------------------------------------------

# Again, it´s not necessary reload the file.
bikes_data = read_csv("C:\\Users\\luisf\\Desktop\\database\\Results\\Cyclistic_tripdata_year_clear.csv")

# Average, max and min (trip_time):
descriptive_analysis = bikes_data %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

## Here, I saw an inconsistency, negative trip_times.

inconsistencies <- bikes_data %>% 
  filter(trip_time < 0)

# Filtering the observations with negative trip times:
bikes_data <- bikes_data %>% 
  filter(trip_time > 0)

# Recalculate average, max and min:
descriptive_analysis = bikes_data %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

# Rides with less than one minute:
one_min_trips <- bikes_data %>% 
  filter(trip_time < 1*60)

n_row_one <- nrow(one_min_trips)
one_per_total <- round(((n_row_one/n_row)*100), 2)


## This observations will be filtering too (possible inconsistency in the dataset):

bikes_data <- bikes_data %>% 
  filter(trip_time > 1*60)

# Recalculate average, max and min:
descriptive_analysis = bikes_data %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

# Doubt regarding the time limit of the tours. Can there be long times (over 5 hours)? 

long_trip_time <- bikes_data %>% 
  filter(trip_time > 5*60*60)

n_row <- nrow(bikes_data)
n_row_long <- nrow(long_trip_time)
per_total <- round(((n_row_long/n_row)*100), 2)

# Only 0.22% of the rides has more than 5 hours. In the analysis, this observations will not be filtered.

# Days of the week and number of rides:
weekday_count <- bikes_data %>% 
  group_by(Weekday_start) %>% 
  summarise(count = n()) %>% 
  arrange(desc(count))
  
## Considering subdata members and casuals
 
## Members -------------------------------------------------------------------

bikes_data_members <- bikes_data %>% 
  filter(member_casual == 'member')

## Average, max and min - members:

descriptive_analysis_members = bikes_data_members %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

## Days of the week and number of rides - members:

weekday_count_members <- bikes_data_members %>% 
  group_by(Weekday_start) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

## Members use bicycles more during the week (probably for work)

## Casual

bikes_data_casual <- bikes_data %>% 
  filter(member_casual == 'casual')

## Average, max and min - casuals:

descriptive_analysis_casual = bikes_data_casual %>% 
  summarize(average_trip_time = as_hms(mean(trip_time)),
            max_trip = as_hms(max(trip_time)),
            min_trip = as_hms(min(trip_time)))

## Days of the week and number of rides - casuals:

weekday_count_casual <- bikes_data_casual %>% 
  group_by(Weekday_start) %>% 
  summarize(count = n()) %>% 
  arrange(desc(count))

# casual commuters use bicycles more on weekends.

# Insight: focus on casual commuters who use the bike during the week

## Visualization: -----------------------------------------------------

# The dataset will be summarized (eliminate columns that do not influence the analysis performed), for the visualization process
# Selecting the important columns for the analysis

bikes_data_viz <- bikes_data %>% 
  select(started_at, ended_at, member_casual, trip_time)

# Salving dataset, for future use in Power BI
export(bikes_data_viz, file = 'C:\\Users\\luisf\\Desktop\\database\\Results\\bikes_data_viz.csv')
