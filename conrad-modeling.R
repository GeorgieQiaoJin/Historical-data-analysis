library(tidyverse)

read_clean <- function(f) {
  return(
   read_csv(f) %>%
     janitor::clean_names() %>%
     select(anon_student_id, time=time_34, detector_name, value)
  )
}

fs <- dir('LearnSphereDataMatch', pattern = '*.csv', full.names = TRUE)
d_detector <- map_dfr(fs, read_clean) %>%
  filter(detector_name != 'False') %>%
  mutate(
    value = ifelse(str_detect(value, '^0'), 0, 1) # naive recoding for now
  ) %>%
  pivot_wider(names_from = detector_name, values_from = value, values_fill = 0)

library(lubridate) # Following python code
d_detector$time <- ymd_hms(d_detector$time, tz = "UTC")
attr(d_detector$time, "tzone") <- NULL  

d <- read_csv('Updated_LiveLab.csv') %>%
  janitor::clean_names() %>%
  mutate(
    requested_help = ifelse(help_level>0, 1, 0),
    got_help = ifelse(helped_transaction, 1, 0)
         )

# modeling:

# round and group by 10 min window
d_log <- d %>%
  select(anon_student_id, cf_class_id, time, requested_help, got_help) %>%
  mutate(time = round_date(time, "15 mins")) %>%
  group_by(anon_student_id, cf_class_id, time) %>%
  summarize(
    requested_help = ifelse(sum(requested_help)>0, 1, 0),
    got_help = ifelse(sum(got_help)>0, 1, 0)
  ) %>%
  ungroup()

d_detector_join <- d_detector %>%
  mutate(time = round_date(time, "15 mins")) %>%
  group_by(anon_student_id, time) %>%
  summarize(
    critical_struggle = ifelse(sum(critical_struggle)>0, 1, 0),
    idle = ifelse(sum(idle)>0, 1, 0),
    system_misuse = ifelse(sum(system_misuse)>0, 1, 0),
    struggle = ifelse(sum(struggle)>0, 1, 0),
    student_doing_well = ifelse(sum(student_doing_well)>0, 1, 0)
  ) %>%
  ungroup()

# Join and model

d_model <- d_log %>%
  left_join(d_detector_join, by=c('anon_student_id', 'time'))

d_model[is.na(d_model)] <- 0

library(lme4)

# Here you can see that including idle ETC leads to too large standard errors
m <- glmer(got_help ~ (1 | anon_student_id) + (1 | cf_class_id) + requested_help, 
      d_model, family='binomial', verbose=2, nAGQ=0)

sjPlot::tab_model(m)
