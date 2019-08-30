# install packages if not on R now----
# devtools::install_github("thomasp85/patchwork") # multiple plots
# install.packages("devtools") # install new things
# install.packages("tidyverse") # dplyr and piping and ggplot etc
# install.packages("lubridate") # dates and times
# install.packages("scales") # scales on ggplot ases
# install.packages("readxl") # read in excel files
# install.packages("skimr") # quick summary stats
# install.packages("styler") # style your code - nice
# install.packages("janitor") # clean names
# install.packages("plotly") # cool thing

# Load Libraries----
library(tidyverse)
library(lubridate)
library(scales)
library(patchwork)

# read peak flows ----
fox.df <- read_csv("data/fox peak flows.csv") %>%
  mutate(date = mdy(date))

rating.plot <- fox.df %>% ggplot(aes(x=gage_ht, y=discharge)) +
  geom_point() +
  labs(x="Gage height (Feet)", y="Discharge (CFS)")+
  scale_y_continuous(label = comma) +
  geom_smooth()
rating.plot



# read in long term data
fox_recur.df <- read_csv("data/fox long term.csv") %>%
  mutate(date = mdy(date),
         year = year(date),
         month = month(date))

# get the max by year
fox_recur_ann.df <- fox_recur.df %>%
  group_by(year) %>%
  summarize(discharge = max(discharge, na.rm = TRUE))

# Get the first 25
early.df <- fox_recur_ann.df %>%
  head(n=25) %>% 
  arrange(desc(discharge))

# get the last 25
late.df <- fox_recur_ann.df %>%
  tail(n=25) %>% 
  arrange(desc(discharge))

# bring together
recurring.df <- bind_cols(early.df, late.df) %>%
  mutate(id = row_number())

# now do a few calcualtions
recurring.df <- recurring.df %>%
  mutate(t = (length(id) +1) / id)

recurrance.plot <- recurring.df %>%
  ggplot(aes(x=t)) +
  geom_point(aes(y=discharge), color="red", size=3) +
  geom_point(aes(y=discharge1), color="blue", size=3) +
  geom_line(aes(y=discharge), color="red", size=1) +
  geom_line(aes(y=discharge1), color="blue", size=1) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x="Recurrance Interval", y = "Discharge (CFS)")
recurrance.plot
