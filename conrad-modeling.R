library(tidyverse)

#d <- read_csv('final_filtered_test_file_15mins.csv')
#d <- read_csv('final_filtered_test_file_session.csv')

read_clean <- function(f) {
  return(
   read_csv(f) %>%
     janitor::clean_names() %>%
     select(anon_student_id, time=time_3, detector_name, value)
  )
}

fs <- dir('LearnSphereDataMatch', pattern = '*.csv', full.names = TRUE)
d_detector <- map_dfr(fs, read_clean) %>%
  filter(detector_name != 'False') %>%
  mutate(
    value = ifelse(str_detect(value, '^0'), 0, 1) # naive recoding for now
  ) 

d_detector <- d_detector %>%
  group_by(anon_student_id, time, detector_name) %>%
  summarize(value = max(value, na.rm=TRUE)) %>% # repeated seconds
  ungroup() %>%
  pivot_wider(names_from = detector_name, values_from = value, values_fill = 0)

library(lubridate) # Following python code
d_detector$time <- ymd_hms(d_detector$time, tz = "UTC")
attr(d_detector$time, "tzone") <- NULL  

d <- read_csv('New_LiveLab.csv') %>%
  janitor::clean_names() %>%
  mutate(
    requested_help = ifelse(help_level>0, 1, 0),
    got_help = ifelse(helped_transaction, 1, 0)
         )

# modeling:

mins_agg <- "15 mins"

# round and group by 10 min window
d_log <- d %>%
  select(anon_student_id, cf_class_id, time, requested_help, got_help) %>%
  mutate(time = round_date(time, mins_agg)) %>%
  group_by(anon_student_id, cf_class_id, time) %>%
  summarize(
    requested_help = ifelse(sum(requested_help)>0, 1, 0),
    got_help = ifelse(sum(got_help)>0, 1, 0)
  ) %>%
  ungroup()

d_detector_join <- d_detector %>%
  mutate(time = round_date(time, mins_agg)) %>%
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
performance::icc(m, by_group = TRUE)

m <- glmer(got_help ~ (1 | anon_student_id) + (1 | cf_class_id) + requested_help + idle, 
           d_model, family='binomial', verbose=2, nAGQ=0)

d_model

sjPlot::tab_model(m)
performance::icc(m, by_group = TRUE)

m <- glmer(got_help ~ (1 | anon_student_id) + (1 | cf_class_id) + requested_help + 
             idle + system_misuse + struggle + student_doing_well, 
           d_model, family='binomial', verbose=2, nAGQ=0)
sjPlot::tab_model(m)
performance::icc(m, by_group = TRUE)

# The relationship between the detectors and receiving teacher help/system hints help during difficult moments.
d_time_ref <- read_csv('final_filtered_test_file_15mins.csv') %>%
  janitor::clean_names() %>%
  mutate(time = time %>% as.POSIXct() %>% lubridate::round_date("1 mins")) %>%
  distinct(cf_class_id, time) %>% # Get difficult class moments
  mutate(challenge_moment = TRUE)

d_model_challenge <- d_model %>%
  mutate(time = time %>% as.POSIXct() %>% lubridate::round_date("15 mins")) %>%
  left_join(d_time_ref, by=c('cf_class_id', 'time')) %>%
  mutate(challenge_moment = ifelse(is.na(challenge_moment), 0, 1))

m <- glmer(got_help ~ (1 | anon_student_id) + (1 | cf_class_id) + 
             requested_help*challenge_moment + 
             idle*challenge_moment + 
             system_misuse*challenge_moment + 
             struggle*challenge_moment + 
             student_doing_well*challenge_moment, 
           d_model_challenge, family='binomial', verbose=2, nAGQ=0)
sjPlot::tab_model(m)
performance::icc(m, by_group = TRUE)

m_ror <- glmer(got_help ~ challenge_moment * idle + (1 | anon_student_id), 
               data = d_model_challenge, family = "binomial")

coefs_ror <- tidy(m_ror, effects = "fixed") %>%
  filter(term != "(Intercept)") %>%
  mutate(odds_ratio = exp(estimate),
         lower_CI = exp(estimate - 1.96 * std.error),
         upper_CI = exp(estimate + 1.96 * std.error))

# Display relative odds ratios
print(coefs_ror)

# By class --> sparse data
library(broom.mixed)
coefs <- d_model %>%
  group_by(cf_class_id) %>%
  group_modify(~ {
    model <- glmer(got_help ~ (1 | anon_student_id) + 
                     requested_help + idle + system_misuse + struggle + student_doing_well, 
                   data = ., family = 'binomial', nAGQ = 0)
    tidy(model) %>% filter(effect == "fixed") %>% mutate(cf_class_id = unique(.x$cf_class_id))
  }) %>%
  ungroup() %>%
  filter(std.error<50) %>%
  filter(term!='(Intercept)')

ggplot(coefs, aes(x = term, y = estimate, color = cf_class_id, group = cf_class_id)) +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_errorbar(aes(ymin = estimate - std.error, ymax = estimate + std.error), 
                width = 0.2, position = position_dodge(width = 0.5)) +
  theme_minimal() +
  labs(x = "Predictors", y = "Coefficient Estimate", color = "Class ID") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




d_ranef <- ranef(m)$anon_student_id %>%
  rownames_to_column('anon_student_id') %>%
  tibble() %>%
  janitor::clean_names()

d_afm <- d %>%
  filter(attempt_at_step==1) %>%
  filter(!is.na(kc_model_mat_hia)) %>%
  arrange(anon_student_id, time) %>%
  select(anon_student_id, kc=kc_model_mat_hia, outcome) %>%
  mutate(outcome = ifelse(outcome=='OK', 1, 0)) %>%
  group_by(anon_student_id, kc) %>%
  mutate(opportunity = 1:n()) %>%
  ungroup()

library(lme4)
#iafm_form <- "outcome ~ opportunity + 
#                 (1 | kc) +
#                 (opportunity | anon_student_id)"
#m_iafm <- glmer(iafm_form, d_afm, family=binomial(), verbose=2, nAGQ=0, control=glmerControl(optCtrl=list(rhoend=1e-4)))
#summary(m_iafm)
#iafm_ranef <- ranef(m_iafm)
#saveRDS(iafm_ranef, 'iafm_ranef.rds')
iafm_ranef <- readRDS('iafm_ranef.rds')
d_join_iafm <- iafm_ranef$anon_student_id %>%
  rownames_to_column('anon_student_id') %>%
  tibble() %>%
  janitor::clean_names() %>%
  rename(initial_prof = intercept)

d_corr <- d_ranef %>%
  left_join(d_join_iafm, by='anon_student_id')

cor.test(d_corr$intercept, d_corr$initial_prof)
cor.test(d_corr$intercept, d_corr$opportunity)

# individual OR, robustness

# Convert results to a data frame
samples <- replicate(1000,
  {
    d_model2 <- d_model[sample(nrow(d_model), replace = TRUE), ]
    tbl <- xtabs(~got_help + requested_help, d_model2)
    (tbl[2,2] * tbl[1,1]) / (tbl[1,2] * tbl[2,1])
  }     
)

quantile(samples, c(0.025, 0.5, 0.975))

samples <- replicate(1000,
                     {
                       d_model2 <- d_model[sample(nrow(d_model), replace = TRUE), ]
                       tbl <- xtabs(~got_help + idle, d_model2)
                       (tbl[2,2] * tbl[1,1]) / (tbl[1,2] * tbl[2,1])
                     }     
)

quantile(samples, c(0.025, 0.5, 0.975))

samples <- replicate(1000,
                     {
                       d_model2 <- d_model[sample(nrow(d_model), replace = TRUE), ]
                       tbl <- xtabs(~got_help + system_misuse, d_model2)
                       (tbl[2,2] * tbl[1,1]) / (tbl[1,2] * tbl[2,1])
                     }     
)

quantile(samples, c(0.025, 0.5, 0.975))

samples <- replicate(1000,
                     {
                       d_model2 <- d_model[sample(nrow(d_model), replace = TRUE), ]
                       tbl <- xtabs(~got_help + struggle, d_model2)
                       (tbl[2,2] * tbl[1,1]) / (tbl[1,2] * tbl[2,1])
                     }     
)

quantile(samples, c(0.025, 0.5, 0.975))

samples <- replicate(1000,
                     {
                       d_model2 <- d_model[sample(nrow(d_model), replace = TRUE), ]
                       tbl <- xtabs(~got_help + student_doing_well, d_model2)
                       (tbl[2,2] * tbl[1,1]) / (tbl[1,2] * tbl[2,1])
                     }     
)

quantile(samples, c(0.025, 0.5, 0.975))

