Hubway Data Visualization
================
Ignacio Carracedo
December 16, 2016

INTRODUCTION
============

Hubway is a Metro-Boston bicycle sharing company that aims to offer an efficient and affordable transportation service. In 2007 the Boston Bike program was founded which led to the development of the Hubway system. Cities such as Boston, Brookline, Cambridge and Somerville all committed to bringing the new initiative to their commmunities. Since the program would interfere with municipal boundaries the Metropolitan Area Planning Council stepped in and selected the company <i>Motivate</i> to oversee Hubway.

Hubway keeps a record of every rider's trip. In 2012 Hubway challenged the public to use its data for visualizing half a million of Hubway rides. While the challenge has already ended, the data is still available for use. In fact, Hubway has actually updated its data set to contain rides from the launch of the service in 2011, until the end of 2013.

OBJECTIVES
==========

Hubway's records provide information about the rides and the riders. We can find the start and end time for the rides, the stations involved and also some demographic data on riders such as age or gender. Not all riders are registered in the service, some of them are just casual users of Hubway's bikes (meaning most of the data that is usually associated with a registered user is not available.

Our goals for this paper are to understand how people use the service, and also how registered users differ from casual ones, in the way both groups of users benefit from the service. We look to answer the following questions:

<ul>
<li>
Are men or women more willing to use Hubway?
</li>
<li>
Does age affect trip duration?
</li>
<li>
Are most Hubway rides done by registered users or just casual users?
</li>
<li>
Does the daily frequency of trips differ between registered and casual users?
</li>
<li>
What are peak usage times in a day for the service?
</li>
</ul>
Furthermore, we look to understand how stations are used. Based on maps, we want to know:
<ul>
<li>
What are stations with more incoming than outcoming flow of riders?
</li>
<li>
Where are the most and least used stations?
</li>
</ul>
VARIABLE DESCRIPTIONS
=====================

Hubway provides two data sets freely available for download from the website <http://hubwaydatachallenge.org>:
<ul>
<li>
`hubway_trips.csv`: information regarding rides
</li>
<li>
`hubway_stations.csv`: information of every station in the Boston area
</li>
</ul>
<b>TRIPS:</b> The following table describes the details of the variables in the `hubway_trips.csv` data set:

| Variable Name | Variable Description                | Unit          |
|---------------|-------------------------------------|---------------|
| duration      | duration of the trip                | seconds       |
| start\_day    | start day and time                  | data and time |
| end\_date     | end day and time                    | data and time |
| strt\_statn   | id of the start station             | number        |
| end\_statn    | id of the end station               | number        |
| bike\_nr      | id of the bike used                 | number        |
| subs\_type    | type of user (registered or casual) | factor        |
| zip\_code     | zip\_code for registered users      | string        |
| birth\_date   | birth year of registered users      | year          |
| gender        | gender of users                     | factor        |

<b>STATIONS:</b> The following table describes the details of the variables in the `hubway_stations.csv` data set:

| Variable Name | Variable Description                                      | Unit   |
|---------------|-----------------------------------------------------------|--------|
| terminal      | id for the station                                        | string |
| station       | name of the station                                       | string |
| municipal     | city of the station (Boston, Somerville, Cambridge, etc.) | string |
| lat           | latitude of the station                                   | number |
| lng           | longitude of the station                                  | number |
| status        | if the station exists or has been removed                 | factor |

DATASET PREPARATION
===================

For preparing our data before diving into analysis we follow the process as described below:

-   Read both datasets and initialize libraries to be used.

-   Check for and remove missing values:

``` r
sapply(hw.stations, function(x) sum(is.na(x))) # check for missing values
```

    ##        id  terminal   station municipal       lat       lng    status 
    ##         0         0         0         0         0         0         0

``` r
sapply(hw.trips, function(x) sum(is.na(x))) # check for missing values
```

    ##     seq_id  hubway_id     status   duration start_date strt_statn 
    ##          0          0          0          0          0         14 
    ##   end_date  end_statn    bike_nr subsc_type   zip_code birth_date 
    ##          0         45          0          0          0    1228381 
    ##     gender 
    ##          0

-   We find that there are 49 negative values for the `duration` variable. As such, we remove these entries:

``` r
hw.trips %>% filter(., duration<0) %>% nrow() # check how many rows will be removed
```

    ## [1] 49

``` r
hw.trips %>% filter(., duration > 0) -> hw.trips #remove
```

<li>
We want to just take into account trips that last one day or less. Given the expanse of the Hubway network, trips that are that long are probably faulty entries or entries for users that did not return the bike to a Hubway rack. Therefore, we remove all trips that last more than 24 hours:
</li>
``` r
hw.trips$duration %>% quantile(c(.999)) #to make sure we are not removing a lot we check 99.9 quantile
```

    ##    99.9% 
    ## 52865.41

``` r
hw.trips %>% filter(., duration < 86400) -> hw.trips #remove
```

-   We move on to deleting resets: a bike's timer gets reset when it is taken out and put back into the same station in less than 5 minutes.

``` r
# Delete resets: Threshold of 5 min (300 seconds))
hw.trips %>% filter(., strt_statn==end_statn & duration<300) %>% nrow() # check
```

    ## [1] 10261

``` r
hw.trips %>% filter(., !(strt_statn==end_statn & duration<300)) -> hw.trips #remove
```

-   Due to the format of zipcodes we now remove the preceding `'` sign:

``` r
# fix zipcode. Remove prefix and turn into number
hw.trips$zip_code %>% sapply(., function(x) {substring(x, 1)}) %>% as.factor() -> hw.trips$zip_code
```

-   Create factor levels for stations and remove observations with start or end station set to `null` or `na`:

``` r
# create factors for stations
hw.trips$strt_statn <- as.factor(hw.trips$strt_statn)
hw.trips$end_statn <- as.factor(hw.trips$end_statn)
# check for missing values - remove rows with start station or end station null
sapply(hw.trips, function(x) sum(is.na(x))) 
```

    ##     seq_id  hubway_id     status   duration start_date strt_statn 
    ##          0          0          0          0          0         11 
    ##   end_date  end_statn    bike_nr subsc_type   zip_code birth_date 
    ##          0         27          0          0          0    1218236 
    ##     gender 
    ##          0

``` r
hw.trips %>% filter(., !is.na(strt_statn) & !is.na(end_statn)) -> hw.trips
```

<li>
Calculate age of Hubway users and save the result in a new variable named `age`:
</li>
``` r
hw.trips %>% mutate(. , age = 2012 - birth_date ) -> hw.trips
```

-   Create columns for time, month, year and day of the week separately:

``` r
# Extract time, month, year, and day of the week from dates 
#time (hour, minute)
hw.trips$end_date %>% as.POSIXct(.,format="%m/%d/%Y %H:%M:%S") %>%  strftime(., "%H:%M") ->  hw.trips$end_date_time
hw.trips$start_date %>% as.POSIXct(.,format="%m/%d/%Y %H:%M:%S") %>%  strftime(., "%H:%M") -> hw.trips$start_date_time
#dates
hw.trips$start_date %>% as.Date(. , format = "%m/%d/%Y") -> hw.trips$start_date
hw.trips$end_date %>% as.Date(. , format = "%m/%d/%Y") -> hw.trips$end_date
#month, year, weekdays
hw.trips$start_date %>% strftime(., "%m") %>% as.factor() -> hw.trips$start_date_month
hw.trips$start_date %>% strftime(., "%y") %>% as.factor() -> hw.trips$start_date_year
hw.trips$start_date %>% weekdays(.) %>% as.factor() -> hw.trips$start_date_weekday
hw.trips$end_date %>% strftime(., "%m") %>% as.factor() -> hw.trips$end_date_month
hw.trips$end_date %>% strftime(., "%y") %>% as.factor() -> hw.trips$end_date_year
hw.trips$end_date %>% weekdays(.) %>% as.factor() -> hw.trips$end_date_weekday
```

-   Save two new data sets:

-   `hubway_stations_clean.csv`
-   `hubway_trips_clean.csv`

VARIABLE SUMMARIES
==================

Now that we have two clean datasets to work with we move on to our analysis. We thus begin by importing the clean data and quickly inspecting it.

``` r
# load
hw.stations = read.csv("./data/hubway_stations_clean.csv")
hw.trips = read.csv("./data/hubway_trips_clean.csv") 
#checks
head(hw.stations)
```

    ##   id terminal                                       station municipal
    ## 1  3   B32006                        Colleges of the Fenway    Boston
    ## 2  4   C32000                   Tremont St. at Berkeley St.    Boston
    ## 3  5   B32012            Northeastern U / North Parking Lot    Boston
    ## 4  6   D32000                      Cambridge St. at Joy St.    Boston
    ## 5  7   A32000                                      Fan Pier    Boston
    ## 6  8   A32001 Union Square - Brighton Ave. at Cambridge St.    Boston
    ##        lat       lng   status
    ## 1 42.34002 -71.10081 Existing
    ## 2 42.34539 -71.06962 Existing
    ## 3 42.34181 -71.09018 Existing
    ## 4 42.36129 -71.06514 Existing
    ## 5 42.35341 -71.04462 Existing
    ## 6 42.35333 -71.13731 Existing

``` r
str(hw.stations)
```

    ## 'data.frame':    142 obs. of  7 variables:
    ##  $ id       : int  3 4 5 6 7 8 9 10 11 12 ...
    ##  $ terminal : Factor w/ 131 levels "A32000","A32001",..: 20 35 26 55 1 2 3 4 5 16 ...
    ##  $ station  : Factor w/ 137 levels "359 Broadway - Broadway at Fayette Street",..: 38 126 95 28 49 127 2 6 82 107 ...
    ##  $ municipal: Factor w/ 4 levels "Boston","Brookline",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ lat      : num  42.3 42.3 42.3 42.4 42.4 ...
    ##  $ lng      : num  -71.1 -71.1 -71.1 -71.1 -71 ...
    ##  $ status   : Factor w/ 2 levels "Existing","Removed": 1 1 1 1 1 1 1 1 1 1 ...

``` r
head(hw.trips)
```

    ##   seq_id hubway_id status duration start_date strt_statn   end_date
    ## 1     10        17 Closed     1108 2011-07-28         47 2011-07-28
    ## 2     11        18 Closed     1055 2011-07-28         47 2011-07-28
    ## 3     12        19 Closed     1042 2011-07-28         47 2011-07-28
    ## 4     13        23 Closed      994 2011-07-28         40 2011-07-28
    ## 5     15        27 Closed      952 2011-07-28         40 2011-07-28
    ## 6     17        29 Closed     1261 2011-07-28         22 2011-07-28
    ##   end_statn bike_nr subsc_type zip_code birth_date gender age
    ## 1        40  B00550 Registered     1867       1994   Male  18
    ## 2        40  B00580 Registered     1867       1956   Male  56
    ## 3        40  B00539 Registered     1867       1959 Female  53
    ## 4        47  B00368     Casual       NA         NA         NA
    ## 5        23  B00556 Registered     2128       1944   Male  68
    ## 6        45  B00454 Registered     2492       1975   Male  37
    ##   end_date_time start_date_time start_date_month start_date_year
    ## 1         12:13           11:55                7              11
    ## 2         12:13           11:55                7              11
    ## 3         12:12           11:55                7              11
    ## 4         12:16           12:00                7              11
    ## 5         12:16           12:00                7              11
    ## 6         12:21           12:00                7              11
    ##   start_date_weekday end_date_month end_date_year end_date_weekday
    ## 1           Thursday              7            11         Thursday
    ## 2           Thursday              7            11         Thursday
    ## 3           Thursday              7            11         Thursday
    ## 4           Thursday              7            11         Thursday
    ## 5           Thursday              7            11         Thursday
    ## 6           Thursday              7            11         Thursday

``` r
str(hw.trips)
```

    ## 'data.frame':    1563717 obs. of  22 variables:
    ##  $ seq_id            : int  10 11 12 13 15 17 19 20 21 22 ...
    ##  $ hubway_id         : int  17 18 19 23 27 29 31 33 34 35 ...
    ##  $ status            : Factor w/ 1 level "Closed": 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ duration          : int  1108 1055 1042 994 952 1261 1020 1264 1043 969 ...
    ##  $ start_date        : Factor w/ 628 levels "2011-07-28","2011-07-29",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ strt_statn        : int  47 47 47 40 40 22 38 38 38 22 ...
    ##  $ end_date          : Factor w/ 632 levels "2011-07-28","2011-07-29",..: 1 1 1 1 1 1 1 1 1 1 ...
    ##  $ end_statn         : int  40 40 40 47 23 45 36 44 44 44 ...
    ##  $ bike_nr           : Factor w/ 1164 levels "","A07799","A07800",..: 565 595 554 384 571 470 164 182 542 503 ...
    ##  $ subsc_type        : Factor w/ 2 levels "Casual","Registered": 2 2 2 1 2 2 2 2 2 2 ...
    ##  $ zip_code          : int  1867 1867 1867 NA 2128 2492 2118 2139 2351 2143 ...
    ##  $ birth_date        : int  1994 1956 1959 NA 1944 1975 1987 1985 1978 1971 ...
    ##  $ gender            : Factor w/ 3 levels "","Female","Male": 3 3 2 1 3 3 2 2 3 3 ...
    ##  $ age               : int  18 56 53 NA 68 37 25 27 34 41 ...
    ##  $ end_date_time     : Factor w/ 1440 levels "00:00","00:01",..: 734 734 733 737 737 742 739 743 739 738 ...
    ##  $ start_date_time   : Factor w/ 1440 levels "00:00","00:01",..: 716 716 716 721 721 721 722 722 722 722 ...
    ##  $ start_date_month  : int  7 7 7 7 7 7 7 7 7 7 ...
    ##  $ start_date_year   : int  11 11 11 11 11 11 11 11 11 11 ...
    ##  $ start_date_weekday: Factor w/ 7 levels "Friday","Monday",..: 5 5 5 5 5 5 5 5 5 5 ...
    ##  $ end_date_month    : int  7 7 7 7 7 7 7 7 7 7 ...
    ##  $ end_date_year     : int  11 11 11 11 11 11 11 11 11 11 ...
    ##  $ end_date_weekday  : Factor w/ 7 levels "Friday","Monday",..: 5 5 5 5 5 5 5 5 5 5 ...

Since we want to analyze information on registered and casual users we first compare the number of trips done by each category of users.

``` r
# Registered/unRegistered trips (bar) 
ggplot(hw.trips, aes(x=subsc_type, fill=subsc_type)) +
  geom_bar(color=I("black"), size=0,alpha = 0.7) +
  ylab("Count") +
  xlab("Subscriber type") +
  ggtitle("Total trips by user status")  +
  guides(fill=FALSE)+ #removes legend
  scale_y_continuous(labels = comma)
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-12-1.png)

Registered users have more than double the number of trips done by casual users. This matches our intiuitive hypothesis as we would expect registered users to exhibit higher levels of engagement with the service.

we check the duration of all trips, and by type of user.

``` r
# duration (hist)
ggplot(hw.trips, aes(x=duration)) +
  geom_histogram(bins=300, color=I("darkblue"), fill=I("darkblue"), size=0,alpha = 0.5) +
  coord_cartesian(xlim = c(0,6500)) +
  xlab("Trip duration (seconds)") +
  ylab("Count") +
  ggtitle("Trip duration in seconds (5 min = 1 bin)") + 
  scale_y_continuous(labels = comma)
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-13-1.png)

``` r
# duration per Registered/ unrRegistered
ggplot(hw.trips, aes(x=duration,fill=subsc_type)) +
  geom_histogram(bins=300, color="black", size=0, alpha = 0.5, position = 'identity') +
  coord_cartesian(xlim = c(0,6500)) + 
  ggtitle("Trip duration by user status (5 min = 1 bin)") +
  xlab("Duration (Seconds)") +
  ylab("Count") + 
  scale_y_continuous(labels = comma)
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-13-2.png)

It is interesting to note that registered users usually spend 10 to 15 minutes per ride. Casual users tend to spend more time, mainly between 15 to 25 minutes. This could be due to the fact that registered users have more frequent fixed trips that they incorporate into their commutes while casual users will probably use the service for more leisurly purposes.

If we just focus on registered users, we are able to analyze demographic information such as gender and age.

Based on the following visualization, it is noticeable that men use the service almost three times more than women do.

``` r
# gender of register users
hw.trips %>% filter(., subsc_type=='Registered') -> hw.trips.register

ggplot(hw.trips.register, aes(x=gender, fill=gender)) +
  geom_bar(color=I("black"), size=0,alpha = 0.9) +
  ylab("Count") +
  ggtitle("Total registered trips by gender") +
  guides(fill=FALSE) + #removes legend
  scale_y_continuous(labels = comma)
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-14-1.png)

When we look at the duration of rides by age, we can note that older people's trips are usually longer. That is especially obvious for users who are about 70 years old. It is important to keep in mind the fact that there are less users in this age group than there are younger ones.

``` r
# duration by age (only Registered users) # remove ages with less than 20 trips
hw.trips.register %>% group_by(age) %>% summarise(avg.duration=mean(duration), count=n()) %>% filter(., count > 20) -> age.avg.duration

ggplot(age.avg.duration, aes(x= age, y=avg.duration)) +
  geom_bar(stat="identity",color=I("darkblue"), fill=I("darkblue"), size=0,alpha = 0.5) +
  coord_cartesian(xlim = c(15,80)) +
  xlab("Age") +
  ylab("Avg duration (seconds)") +
  ggtitle("Trip duration by age of registered users")
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-15-1.png)

Now, we mix gender and age in a density plot. Here we have the distribution of age by gender. As we can see, female users tends to be younger than male users.

``` r
# Age/sex of Registered users. Using desity plot to compare distributions
ggplot(hw.trips.register, aes(x=age, fill=gender)) +
  geom_density(kernel = "gaussian",color=I("black"), size=0, alpha=0.5) +
  ylab("Probability") +
  ggtitle("Kernel density estimate of total registered trips by age/gender")
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-16-1.png) Despite the fact that users in their 20' to 30's are more likely to skew female, users who are in their 40's skew male.

Let's take a look at the distribution of duration of rides among the two sexes:

``` r
# duration  of Registered by gender
ggplot(hw.trips.register, aes(x=duration, fill=gender)) +
  geom_density(kernel = "gaussian",color=I("black"), size=0, alpha=0.5) +
  ggtitle("Kernel density estimate of trip duration by gender") +
  xlim(0, 3000) +
  xlab("Trip Duration (seconds)") +
  ylab("Probability") 
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-17-1.png) We notice that male users tend to have shorter rides than female users.

VARIABLE CONNECTIONS
====================

Now that we know more about the duration of rides,the age of Hubway users,their gender and wether or not they are registered or casual users, we can dive into deeper analysis, specifically looking at how these variables are connected or related.

The next graphic shows the number of trips by month and user status.

``` r
# number of trips - month (casual and Registered)
# aggregate month
hw.trips %>% group_by(start_date_month, subsc_type) %>% summarise(count=n()) %>% as.data.frame()-> trips.m
# add winter brakes (for plotting)
trips.m[nrow(trips.m)+1,] <- c(12,"Registered",0)
trips.m[nrow(trips.m)+1,] <- c(12,"Casual",0)
trips.m[nrow(trips.m)+1,] <- c(1,"Registered",0)
trips.m[nrow(trips.m)+1,] <- c(1,"Casual",0)
trips.m[nrow(trips.m)+1,] <- c(2,"Registered",0)
trips.m[nrow(trips.m)+1,] <- c(2,"Casual",0)
# set month names
trips.m$start_date_month <- plyr::mapvalues(trips.m$start_date_month,
                     from = c(1,2,3,4,5,6,7,8,9,10,11,12),
                     to = c(".1.Jan", ".2.Feb", ".3.Mar", ".4.Apr", ".5.May",".6.Jun",".7.Jul",".8.Aug",".9.Sep","10.Oct","11.Nov","12.Dec"))  
trips.m$count %>% as.numeric() -> trips.m$count
# sort
trips.m %>% arrange(., start_date_month, count)  -> trips.m
# plot
ggplot(trips.m, aes(x=start_date_month, y=count, fill=subsc_type)) +
  geom_bar(stat="identity",color=I("black"), size=0,alpha = 0.7) +
  xlab("Month") +
  ylab("Count") +
  ggtitle("Number of trips by month and user status") + 
  scale_y_continuous(labels = comma)
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-18-1.png) Both the number of registered users' trips and casual users' trips are higher during Fall. The proportion between the types of users is usually stable. However, during August, the proportion of casual users' trips is higher than in the rest of the month.

When we perform the same analysis but on a daily basis instead of monthly basis, we note that during weekends casual user trips are as high as or even higher than registered users' trips.

``` r
# number of trips - weekdays (2 lines - casual and registered)
# aggregate month
hw.trips %>% group_by(start_date_weekday, subsc_type) %>% summarise(count=n()) %>% as.data.frame()-> trips.d
# set month names
trips.d$start_date_weekday <- plyr::mapvalues(trips.d$start_date_weekday,
                                            from = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"),
                                            to = c("1.Monday","2.Tuesday","3.Wednesday","4.Thursday","5.Friday","6.Saturday","7.Sunday"))  
trips.d$count %>% as.numeric() -> trips.d$count
trips.d$start_date_weekday %>% as.character() -> trips.d$start_date_weekday
# sort
trips.d %>% arrange(., start_date_weekday, subsc_type)  -> trips.d
# plot
ggplot(trips.d, aes(x=start_date_weekday, y=count, fill=subsc_type)) +
  geom_bar(stat="identity",color=I("black"), size=0,alpha = 0.5) +
  xlab("Day of the Week") +
  ylab("Count") +
  ggtitle("Number of trips by weekday and user status")
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-19-1.png)

If we now break down the analysis by hour, we see casual users' trips tend to increase during the afternoon until 7 pm.

``` r
# time of trips by hour
# aggregate hour
hw.trips$start_date_time %>% substring(., 1, 2) -> hw.trips$start_date_hour
hw.trips %>% group_by(start_date_hour, subsc_type) %>% summarise(count=n()) %>% as.data.frame()-> trips.time
trips.time$start_date_hour %>% sapply(., function(x) {paste(x, "h", sep="")}) %>% as.character() -> trips.time$start_date_hour
# sort
trips.time %>% arrange(., start_date_hour)  -> trips.time
# plot
ggplot(trips.time, aes(x=start_date_hour, y=count, fill=subsc_type)) +
  geom_bar(stat="identity",color=I("black"), size=0,alpha = 0.5,width=0.8) +
  xlab("Hour") +
  ylab("Count") +
  ggtitle("Number of trips per hour and subscription type")
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-20-1.png)

Understandably not all stations will experience the same level of traffic. Based on the following graph, <i>South Station is</i>, by far, the station with the most trips. We note that there is an even number of incoming and outgoing trips. <i>Boston Medical Center</i> on Mass Ave. is the second station by number of trips, followed by <i>Central Square</i> station. [NOTE: Only showing top 20 stations by traffic for clarity]

``` r
# incoming/outgoing trips per station
# aggregate by station and sort by count
hw.trips %>% group_by(strt_statn) %>% summarise(outgoing_trips=n()) %>% 
  as.data.frame() %>% arrange(., desc(outgoing_trips)) %>% rename(id = strt_statn) -> trips.o
hw.trips %>% group_by(end_statn) %>% summarise(incoming_trips=n()) %>%
  as.data.frame() %>% arrange(., desc(incoming_trips)) %>% rename(id = end_statn) -> trips.i
# merge by station and add station info from hw.stations
full_join(trips.o, trips.i, by = "id") -> trips.station
left_join(trips.station, hw.stations, by = "id") %>% select(station, outgoing_trips, incoming_trips) %>%
  group_by(station) %>% summarise(outgoing_trips=sum(outgoing_trips), incoming_trips=sum(incoming_trips))  %>% arrange(., desc(outgoing_trips), desc(incoming_trips)) -> trips.station
# top 20
trips.station %>% slice(1:20) -> trips.station
# gather for plotting
trips.station %>% gather("trip","count", 2:3) %>% arrange(., desc(count))-> trips.station 
#plot
ggplot(trips.station, aes(x=station, y=count, fill=trip)) +
  geom_bar(stat="identity",color=I("black"), size=0,alpha = 0.5,width=0.8) +
  xlab("Hour") +
  ylab("Count") +
  ggtitle("Number of trips (incoming/outgoing) per station") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-21-1.png)

We decided to check what the flow of trips in Government Center is like. During weekdays, the number of incoming trips are higher around 9 am and outgoing trips peak around 5 pm. This is an indication that people usually use the service for going to work. Weekends have a different pattern. Both days, but especially Sundays, have a significant number of trips from 00 to 02 am, which means some people rent bikes for going out or for other late-night entertainment activities.

``` r
# Lets check detail incoming outcoming for Government Center to see how the commute affects inbound and outbound trips
# prepare outbound trips
hw.trips %>% filter(strt_statn=="23") %>% select(strt_statn,start_date_weekday,start_date_time) %>% 
  group_by(start_date_weekday, start_date_time) %>% summarise(count=n()) %>% as.data.frame()-> trips.ss.out
trips.ss.out$start_date_time %>% substring(., 1, 2) %>% as.numeric() -> trips.ss.out$start_date_time
trips.ss.out %>%  group_by(start_date_weekday, start_date_time) %>% summarise(count.out=sum(count)) %>% as.data.frame()-> trips.ss.out
trips.ss.out %>% rename(weekday = start_date_weekday, hour=start_date_time)-> trips.ss.out
# prepare inbound trips
hw.trips %>% filter(end_statn=="23") %>% select(end_statn,end_date_weekday,end_date_time) %>% 
  group_by(end_date_weekday, end_date_time) %>% summarise(count=n()) %>% as.data.frame()-> trips.ss.in
trips.ss.in$end_date_time %>% substring(., 1, 2) %>% as.numeric() -> trips.ss.in$end_date_time
trips.ss.in %>%  group_by(end_date_weekday, end_date_time) %>% summarise(count.in=sum(count)) %>% as.data.frame()-> trips.ss.in
trips.ss.in %>% rename(weekday = end_date_weekday, hour=end_date_time) -> trips.ss.in
# join in and out trips
full_join(trips.ss.in, trips.ss.out, by = c("weekday","hour")) -> trips.ss
trips.ss$count.in[is.na(trips.ss$count.in)] <- 0 #na to 0
trips.ss$count.out[is.na(trips.ss$count.out)] <- 0 #na to 0
# gather for plotting
trips.ss %>% gather("trip","count", 3:4) %>% arrange(., desc(weekday))-> trips.ss
# order weekdays for ploting
trips.ss$weekday <-factor(trips.ss$weekday, levels = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
# plot
ggplot(data =trips.ss,
       mapping = aes(x = hour, y = count, shape = weekday, colour = trip, fill=trip)) +
       geom_line(size=1,alpha = 0.5, aes(color=trip))+
       ggtitle("Number of trips (incoming/outgoing) - Government Center") + 
       facet_grid(facets = weekday ~ .) +
       theme_bw()
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-22-1.png)

MAP ANALYSIS
============

In this section we will help create a better idea of the data we have by visualizing maps.

``` r
hw.stations = read.csv("./data/hubway_stations_clean.csv")
hw.trips = read.csv("./data/hubway_trips_clean.csv")
```

First, we look at stations by city. Boston is the city with the largest number of stations. Brookline, on the other hand, has the lowest.

``` r
# map of all stations by municipal
ggmap(get_map("Boston, Massachusetts ",zoom=12,color = "bw")) +
  geom_point(data=hw.stations, aes(x=lng,y=lat,fill = municipal), alpha = 0.7, size = 3, shape = 21) +
  ggtitle("Stations by municipal")
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-24-1.png)

Additionally, we can see if stations tend to have more incoming or outgoing trips.

``` r
# map stions with more outgoing/incoming trips
hw.trips %>% group_by(strt_statn) %>% summarise(outgoing_trips=n()) %>% 
  as.data.frame() %>% arrange(., desc(outgoing_trips)) %>% rename(id = strt_statn) -> trips.o
hw.trips %>% group_by(end_statn) %>% summarise(incoming_trips=n()) %>%
  as.data.frame() %>% arrange(., desc(incoming_trips)) %>% rename(id = end_statn) -> trips.i
# merge by station and add station info from hw.stations
full_join(trips.o, trips.i, by = "id") -> trips.station
full_join(trips.station, hw.stations, by = "id") %>% select(id, station, municipal, lat, lng, status, incoming_trips,outgoing_trips)-> trips.station
# get if the a station has more incoming or outgoing
check <- function(x,y) {
  if (x > y) {
    result <- "incoming"
  }
  else if (x < y) {
    result <- "outgoing"
  }
  else {
    result <- "same"
  }
  return(result)
}


# apply funcition to get type of station
mapply(FUN=check,trips.station$incoming_trips, trips.station$outgoing_trips) -> trips.station$type
#plot
ggmap(get_map("Boston, Massachusetts ",zoom=12,color = "bw")) +
  geom_point(data=trips.station, aes(x=lng,y=lat,fill = type),alpha = 0.7, size = 3, shape = 21) +
  ggtitle("Stations by trip flow (incoming/outgoing)") 
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-25-1.png)

Finally, we split stations in quantiles based on the number of trips. We note with the help of the following graph that stations in the lower quantile are on the outskirts of the city, while stations in higher quantiles tend to be close to downtown.

``` r
# map staions with more traffic (4 quantiles)
hw.trips %>% group_by(strt_statn) %>% summarise(outgoing_trips=n()) %>% 
  as.data.frame() %>% arrange(., desc(outgoing_trips)) %>% rename(id = strt_statn) -> trips.o
hw.trips %>% group_by(end_statn) %>% summarise(incoming_trips=n()) %>%
  as.data.frame() %>% arrange(., desc(incoming_trips)) %>% rename(id = end_statn) -> trips.i
# merge by station and add station info from hw.stations
full_join(trips.o, trips.i, by = "id") -> trips.station
full_join(trips.station, hw.stations, by = "id") %>% select(id, station, municipal, lat, lng, status, incoming_trips,outgoing_trips)-> trips.station
# get if total number of trips
trips.station %>% mutate(total_trips=incoming_trips + outgoing_trips) -> trips.station
# get 4 quantiles:
make.ntiles = function (inputvar, n) { 
  inputvar %>%
    quantile(., 
             (1/n) * 1:(n-1),
             na.rm=TRUE
    ) %>%
    c(-Inf, ., Inf) %>%
    cut(inputvar, 
        breaks=., 
        paste("Q", 1:n, sep="")
    ) 
}
trips.station %>%  mutate(trips.station_q4 = make.ntiles(total_trips, 4 ))-> trips.station
trips.station$trips.station_q4 <- plyr::mapvalues(trips.station$trips.station_q4,
                                              from = c("Q1","Q2","Q3","Q4"),
                                              to = c("1.Low.quantile","2.Medium.low.quantile","3.Medium.high.quantile","4.High.quantile"))  
#plot
ggmap(get_map("Boston, Massachusetts ",zoom=12,color = "bw")) +
  geom_point(data=trips.station, aes(x=lng,y=lat,fill = trips.station_q4),alpha = 0.7, size = 3, shape = 21) +
  ggtitle("Total trips (4 quantiles)")  
```

![](Hubway_files/figure-markdown_github/unnamed-chunk-26-1.png)
