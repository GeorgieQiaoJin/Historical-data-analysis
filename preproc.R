library(tidyverse)

d_ref <- read_delim('ref.txt', delim='\t') # Any DataShop Lynnette Export
#names(d_ref)

# Split dataset into two to bypass Learnsphere 400 mb upload limit
# Add and recode expected variables
# see:
# https://github.com/LearnSphere/WorkflowComponents/blob/dev/DetectorTester/program/CTAT-detector-plugins/Test_Rig/Test_Rig_WorkflowComponent.js

d <- read_csv('LiveLab_top_15_MATHia_7x_classes_anon.csv') %>%
  filter(!is.na(`Step Name`)) %>%
  filter(!is.na(`KC_Model(MATHia)`)) %>%
  mutate(`Step Name` = paste(`Step Name`, 'CustomAction', sep=' ')) %>%
  rename(`KC (Default)` = `KC_Model(MATHia)`) %>%
  mutate(`CF (tool_event_time)` = Time) %>%
  mutate(`CF (tutor_event_time)` = Time) %>%
  mutate(`CF (step_id)` = `Step Name`) %>%
  mutate(`Selection` = `Step Name`) %>%
  mutate(Outcome = ifelse(str_detect(Outcome, 'OK'), 'CORRECT', 'INCORRECT')) %>%
  mutate(`Total Num Hints` = NA) %>%
  mutate(`Student Response Subtype` = NA) %>%
  mutate(`Transaction Id` = sapply(1:n(), function(x) digest::digest(sample(letters, 10, replace = TRUE), algo = "md5")))

(d$`Transaction Id` %>% unique() %>% length()) == nrow(d)

#students <- d$`Anon Student Id` %>% unique() %>% sort()
#students_first_half <- students[1:length(students)/2]
#students_second_half <- students[(1+length(students)/2):length(students)]

# Column ordering
d <- d[,base::intersect(names(d_ref), names(d))]

d <- d %>% select(`Step Name`, everything()) # Parser error circumventing

d %>%
  #filter(`Anon Student Id` %in% students_first_half) %>%
  write_delim('LiveLab_top_15_MATHia_7x_classes_anon-full.txt', delim='\t')

#d %>%
#  filter(`Anon Student Id` %in% students_second_half) %>%
#  write_delim('LiveLab_top_15_MATHia_7x_classes_anon-secondhalf.txt', delim='\t')
