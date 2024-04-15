library(tidyverse)
library(ggthemes)
library(tidytext)
library(lubridate)

full_data = 
        read_csv("full_data.csv") |>
        select(year, month, intent, police, sex, age, race, hispanic, place, education) |>
        mutate(date = as.Date(paste(year, month, "01", sep = "-"))) |>
        select(date, everything()) |>
        filter(!is.na(intent))

interactive_data = 
        read_csv("interactive_data.csv") |>
        select(Intent, Gender, Age, Race, Deaths, Population, Rate)

full_data |>
        group_by(
                year = factor(year(date)),
                month = lubridate::month(date, label = T),
                intent
        ) |>
        count() |>
        ggplot(
                aes(x=month,
                    y=n,
                    group = year,
                    by = intent                )
        )+
        geom_line(aes(color = year))+
        facet_wrap(intent ~.,
                   scales = "free_y")+
        theme_fivethirtyeight()+
        scale_color_viridis_d(option = 'B',
                              begin = 0.25,
                              end = 0.75)+
        geom_smooth(aes(group = intent),
                    color = 'grey60')