library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)

urlfile_cases = "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"
urlfile_deaths="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv"

raw_data_cases <-read_csv(url(urlfile_cases))
raw_data_deaths <-read_csv(url(urlfile_deaths))


# I don't like the analysis by province because only some 
# countries have province data

makeDataTydy = function(input_raw_data){
  da = select(input_raw_data, -c(`Province/State`, Lat, Long))
  da = da %>%
    rename(country = `Country/Region`) %>%
    group_by(country) %>%
    summarise_all(mean)

  da = da  %>%
    pivot_longer(-country, "date", values_to = "cases") %>%
    mutate(date = as.Date(date, format = "%m/%d/%y"))
  return(da)
}

da_cases = makeDataTydy(raw_data_cases)
da_deaths = makeDataTydy(raw_data_deaths)

da_DE = da_cases  %>%
  filter(country == "Germany")


ggplot(data = da_DE,
       aes(x = date, y = cases)) +
  geom_point() + geom_line() +
  scale_y_continuous(name = "cumulative cases") +
  theme_light()

ggplot(data = da_DE,
       aes(x = date, y = cases)) +
  geom_point() + geom_line() +
  scale_y_continuous(name = "cumulative cases",
                     trans = "log10") +
  ggtitle("Germany COVID-19") +
  theme_light()

selected_countries = c("Italy",
                       "Germany",
                       "France",
                       "Spain",
                       "United Kingdom",
                       "Netherlands")

da_cases %>%
  filter(country %in% selected_countries) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_point() + geom_line() +
  scale_y_continuous(name = "cumulative cases",
                     trans = "log10") +
  facet_wrap(~country) + 
  theme_light()

estimateGrowthRate = function(input_raw_data,
                              sel_country   = "Germany",
                              init_date = as.Date("2020-02-21")){
  # create dependent variable Delta cases / cases_{t-1}
  da = input_raw_data %>%
    filter(country == sel_country) %>%
    filter(date > init_date & cases > 0) %>%
    mutate(Y = (cases - lag(cases))/lag(cases),
           x = scale(as.numeric(date) - as.numeric(init_date) + 1))
  
  reg = lm(Y ~ x, data = da)
  return(summary(reg))
}
estimateGrowthRate(da_cases)
estimateGrowthRate(da_cases, sel_country = "Italy")
estimateGrowthRate(da_cases, sel_country = "Spain")
estimateGrowthRate(da_cases, sel_country = "United Kingdom")

estimateGrowthRate(da_deaths)
estimateGrowthRate(da_deaths, sel_country = "Italy")
estimateGrowthRate(da_deaths, sel_country = "Spain")
estimateGrowthRate(da_deaths, sel_country = "United Kingdom")




