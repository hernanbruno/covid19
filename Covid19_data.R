library(readr)

urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv"

da <-read_csv(url(urlfile))

# I don't like the analysis by province because only some 
# countries have province data

library(dplyr)
da = select(da, -c(`Province/State`, Lat, Long))
da = da %>%
  rename(country = `Country/Region`) %>%
  group_by(country) %>%
  summarise_all(mean)

library(tidyr)
da = da  %>%
  pivot_longer(-country, "date", values_to = "cases") %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"))

da_DE = da  %>%
  filter(country == "Germany")

library(ggplot2)
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

da %>%
  filter(country %in% selected_countries) %>%
  ggplot(aes(x = date, y = cases)) +
  geom_point() + geom_line() +
  scale_y_continuous(name = "cumulative cases",
                     trans = "log10") +
  facet_wrap(~country) + 
  theme_light()



