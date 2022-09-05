# clifro free
# https://cpb-ap-se2.wpmucdn.com/blogs.auckland.ac.nz/dist/5/91/files/2015/03/seers.pdf
# https://www.youtube.com/watch?v=facMJFmsUlw&t=386s&ab_channel=RiffomonasProject

# 2000 was a leap year with 29 days in feb

library(tidyverse)
library(clifro)
library(lubridate)

# reefton is only station available to public
public.cfuser = cf_user()

# Choose the datatypes
rainfall.dt = cf_datatype(3, 1, 1) # get all rain variables

# Choose the Reefton EWS station (only available public)
# stephens island station info on another file
reefton.st = cf_station()

# Send the query to CliFlo and retrieve the data
daily.datalist = cf_query(user = public.cfuser,
                          # want to drop wind
                          datatype = rainfall.dt, 
                          station = reefton.st,
                          start_date = "2000-01-01 00",
                          end_date = "2022-01-01 00")

daily.datalist
class(daily.datalist)

# returns plot for first dataset in object 

# For the second dataframe (Rain)
plot(daily.datalist)  
# provides: rain, soil runoff, soil deficit

# plot rainfall 
plot(daily.datalist, include_runoff = FALSE, ggtheme = "bw")

# dont think I will need any of this but keeping for now 
# stores data in a list when more than one variable eg:
# daily_rainfall <- daily.datalist[[2]] # retrieve daily rainfall
# create daily rainfall df for REEFTON
# subset date & daily rainfall (mm) cols
# rainfall_daily <- data.frame(daily_rainfall[,2:3])
# reefton_months = months(daily.datalist[1]$Date, abbreviate = TRUE)
# might need to adjust here so summer isnt base (for ggplot)
# could be too far upstream and it could be fixed in plot
# reefton_season = factor(rep("Summer", length(reefton_months)),levels = c("Summer", "Autumn","Winter", "Spring"))
# reefton_season[reefton_months %in% unique(reefton_months)[3:5]] ="Autumn"
# reefton_season[reefton_months %in% unique(reefton_months)[6:8]] ="Winter"
# reefton_season[reefton_months %in% unique(reefton_months)[9:11]] ="Spring"
# combine seasons & rainfall
# merge loses column name (changes to 'x')
# reefton_season_rainfall <- bind_cols(reefton_season, daily.datalist)
# what is going on with this? NAs? 
# skimr::skim(reefton_season_rainfall)

# create tibble
# rename all to lower initially? rename_all(tolower) %>% 
dly_rf_tbbl <- as.data.frame(daily.datalist) %>%
          janitor::clean_names() %>% 
          as_tibble() %>% 
          # need to tidy names rename(c(date = , rain = )) %>% 
          select(date_local, amount_mm) %>% 
          rename(date = 'date_local', rain = 'amount_mm') 
          
dly_rf_tbbl$date <- ymd_hm(dly_rf_tbbl$date)

# add date info via lubridate
# & make seasons
dates_rf <- dly_rf_tbbl %>%
          mutate( # using mutate not transmute solved the issue
                    julian = yday(date),
                    day = day(date),
                    weekday = wday(date, label = TRUE), # label = true for day of week being class(ord) rather than numeric 
                    # Julian day
                    month = month(date), # label = TRUE?
                    year = year(date),
                    austrl_season = 
                              as.factor(case_when(month %in% 3:5 ~ "Autumn",
                                        month %in% 6:8 ~ "Winter",
                                        month %in% 9:11 ~"Spring",
                                               TRUE ~ "Summer")))
# explore data
skimr::skim(dates_rf) # any NA values?


# DATA VIZ 
# Linegraphs rainfall

# cumsum prcp by year 
# will need to modify color for lots of years (eg maybe highlight a single year or color continuous by recent)
# or could keep lines black and shade plot background to show season by julian days 
dates_rf %>%
          mutate(month = month(date, label = TRUE)) %>% # needs to be named factor to group and (factor(month)) didnt work
          group_by(year) %>%
          mutate(cumsum_year_prcp = cumsum(rain)) %>%  # compute cumulative sum
          ggplot() +
          geom_line(aes(x = julian, y = cumsum_year_prcp, color = factor(year))) +
          labs(title = "percipitation throughout the year (Jan 2000 - Sept 2002)",
               x = "Julian day of year",
               y = "cumulative rainfall (mm)",
               color = "Year") +
          theme_linedraw()



# rainfall by year season color 
dates_rf %>% 
          group_by(year) %>% 
          ggplot()+
          geom_line(aes(x = julian, y = rain, color = austrl_season))+
          facet_wrap(~year, nrow = 3)+
          theme_linedraw()
          # issues
          # lines connecting seasons (and not connecting?)
                    # https://www.youtube.com/watch?v=Ky8CIMZzO54&ab_channel=RiffomonasProject 
                    # fixes a similar issue ~8m in
          # facet_wrap still shows all years
                    # group = year in geom_line doesnt fix ? nope
                    # neither does above: group_by(year) %>% 
                    # solves with x = julian but that aint cute 
                    # need to make a date column without year (ie DD/MM, & group by year column)

# graph 1 point with error bar wiskers for each month ?

# histogram 
dates_rf %>%
          ggplot()+
          geom_histogram(aes(x = rain, fill = austrl_season), binwidth = 5)+
          facet_wrap(~austrl_season)

# plot log of rainfall?
dates_rf %>% 
          # filter for days with rain
          filter(rain > 0) %>% 
          # SOLVED: still clumping issue showing all years on each facet with group_by(year) %>%
          ggplot()+
          geom_point(aes(x = julian, y = rain, size = rain, color = austrl_season), alpha = 0.7)+ # but using julian isntead of date does
          facet_wrap(~year)
# negative log issue from log of 0 issue above SOLVED by removing
# gosh this is ugly and not super informative

# # viz monthly rainfall between years (log)
dates_rf %>%
          mutate(month = month(date, label = TRUE), # want label for facets
                 # reorder months with december first (so seasons align?)
                 rain = (rain + 1)) %>% # add 1mm to each day to log, log(1) = 0
          ggplot() +
          geom_boxplot(aes(x = factor(year), y = rain, group = year, fill = austrl_season), width = 0.5, lwd = 0.4) + 
          facet_wrap(~ month, ncol = 3) +
          scale_y_log10() +
          labs(title = "daily rainfall by month (Jan 2000 - Sept 2002)",x = "Year", y = "log monthly rainfall (mm)", fill = "Austral season") +
          theme_linedraw()

# daily rainfall by season for each year 
dates_rf %>% 
          mutate(rain = (rain + 1)) %>% # add 1mm to each day to log, log(1) = 0 
          ggplot()+
          geom_boxplot(aes(x = factor(austrl_season), y = rain, fill = austrl_season, alpha = 0.4))+ # but using julian isntead of date does
          facet_wrap(~year)+
          scale_y_log10() +
          labs(title = "daily rainfall by Austral season by year (Jan 2000 - Sept 2002)", x = "Year", y = "log daily rainfall (mm)") +
          theme_linedraw()+
          theme(legend.position = "none")




# # HEAT MAP # # 
#  making those matrices 
# https://www.youtube.com/watch?v=y6OhR6MW3Mw&ab_channel=RiffomonasProject
# https://www.youtube.com/watch?v=BUFNZv72ow4&ab_channel=AndrewCouch
          # nesting data
          # assign(paste()) <- naming nested micro-models

# heat map of rainy days during year?
# code from https://www.littlemissdata.com/blog/heatmaps
# https://www.youtube.com/watch?v=5sWoexkhnGM&ab_channel=DavidKeyes
# https://www.youtube.com/watch?v=y6OhR6MW3Mw&ab_channel=RiffomonasProject

# place rain fall value (by day) into matrix of month?
# For each year, array of daily rainfall values across each month

# first pass at rough heatmap
# works but nothing too exciting
dates_rf %>% 
          ggplot(aes(x = julian, y = abs(desc(year)), fill = rain))+ # should just do a factor reorcer but abs(desc()) worked 
          geom_tile()+
          labs(fill = "daily percipitation (mm)", x = "julian day", y = "year")+
          scale_fill_gradient(low = '#ffffff', high = '#030bfc')+
          theme_linedraw()

# each nested tibble in 'data[[]]' will be 
# own matrix with 'day' as rowid for value 'rain'
hm_data_nested <- dates_rf %>% 
          # might not need day column if just row of vector
          select(rain, day, month, year) %>%
          group_by(year, month) %>% 
          nest() %>% 
          ungroup() # ungroup

# cool so now make matricies for all these fun little tibbles 
matrix(hm_data_nested$data[[1]]$rain)     



# # # STATISTICS # # # 
# summary statistics

# rainfall by month & year 
prcp_month_year_ss <- dates_rf %>% 
          mutate(month = month(date, label = TRUE)) %>% # want label for facets
          group_by(year, month) %>% 
          summarise(sum_month_rf = sum(rain),
                    mean_daily_rf_month = mean(rain),
                    median_daily_rf_month = median(rain),
                    sd_daily_rf_month = sd(rain),
                    iqr_daily_rf_month = IQR(rain),
                    n = length(rain)) %>% 
          ungroup()


# # compute # # 
# number of days with rain
# interval between rainy days



