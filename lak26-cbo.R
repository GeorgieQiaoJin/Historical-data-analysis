library(tidyverse)
library(lme4)

d <- read_csv('session_level_data_with_help (1).csv') %>%
  janitor::clean_names() %>%
  filter(class_session_type=='classwork')

m <- glmer(helped_session ~ 
             prior_visits + 
             #relative_student_delayed_start + 
             idle + 
             #class_session_size + 
             (1 | anon_student_id) + (1 | class), d,
           family='binomial', verbose=2)

sjPlot::tab_model(m)
performance::icc(m, by_group = TRUE)

# Notes for Georgie meeting 8/19:

# teacher ID?

# --- Packages ---------------------------------------------------------------
library(tidyverse)
library(lubridate)
library(janitor)
library(lavaan)

d_raw <- read_csv('session_level_data_with_help (1).csv') %>%
  janitor::clean_names() %>%
  filter(class_session_type=='classwork')

# --- Load & create lags ------------------------------------------------------
d <- d_raw %>%
  mutate(
    class_session_end_time = lubridate::ymd_hms(class_session_end_time, tz = "America/New_York"),
    helped_session = as.integer(helped_session)
  ) %>%
  arrange(anon_student_id, class_session_end_time) %>%
  group_by(anon_student_id) %>%
  mutate(
    session_index = dplyr::row_number(),
    lag_helped = dplyr::lag(helped_session, 1),
    lag_idle   = dplyr::lag(idle, 1)
  ) %>%
  ungroup()

d_model <- d %>%
  select(
    anon_student_id, class, session_index,
    helped_session, lag_helped,
    idle, lag_idle,
    prior_visits, relative_student_delayed_start, class_session_size
  ) %>%
  tidyr::drop_na(helped_session, lag_helped, idle, lag_idle)

# If idle is binary, include it as ordered too
ordered_vars <- c("helped_session", "lag_helped")
if (all(na.omit(d_model$idle) %in% c(0,1)))     ordered_vars <- c(ordered_vars, "idle")
if (all(na.omit(d_model$lag_idle) %in% c(0,1))) ordered_vars <- c(ordered_vars, "lag_idle")

# --- CLPM model (mirrors your style) ----------------------------------------
model_clpm <- '
  # Stability & cross-lagged effects
  helped_session ~ a1*lag_helped + b1*lag_idle
  idle           ~ c1*lag_idle   + d1*lag_helped

  # Covariances
  helped_session ~~ idle
  lag_helped ~~ lag_idle
'

# ========= OPTION A: WLSMV (probit), ordered variables =========
fit_wlsmv <- sem(
  model_clpm, data = d_model,
  ordered = ordered_vars, estimator = "WLSMV",
  meanstructure = TRUE, missing = "pairwise"
)

summary(fit_wlsmv, fit.measures = TRUE, standardized = TRUE, rsquare = TRUE)
fitMeasures(fit_wlsmv, c("cfi.scaled","tli.scaled","rmsea.scaled","srmr"))

# Test each cross-lag separately
lavTestWald(fit_wlsmv, constraints = 'b1 == 0')  # idle_{t-1} → help_t
lavTestWald(fit_wlsmv, constraints = 'd1 == 0')  # help_{t-1} → idle_t

# Joint test of both cross-lags
lavTestWald(fit_wlsmv, constraints = c('b1 == 0', 'd1 == 0'))

# Directional dominance (are the two cross-lags equal?)
lavTestWald(fit_wlsmv, constraints = 'b1 == d1')

# Help stability (lag_helped → helped_session)
lavTestWald(fit_wlsmv, constraints = 'a1 == 0')

# Idle stability (lag_idle → idle)
lavTestWald(fit_wlsmv, constraints = 'c1 == 0')

# Joint test of both autoregressive paths
lavTestWald(fit_wlsmv, constraints = c('a1 == 0', 'c1 == 0'))

lavTestWald(fit_wlsmv, constraints = 'a1 == c1')





# teacher ID?



# ------------------ Setup & model (your code + small tweaks) ------------------
library(tidyverse)
library(lme4)
library(janitor)

d <- read_csv('session_level_data_with_help (1).csv') %>%
  clean_names() %>%
  filter(class_session_type == 'classwork') %>%
  mutate(helped_session = as.integer(helped_session))

m <- glmer(
  helped_session ~ prior_visits + idle +
    (1 | anon_student_id) + (1 | class),
  data = d, family = binomial, control = glmerControl(optimizer="bobyqa")
)

# ------------------ Helper: clean publication theme ---------------------------
pub_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(linewidth = 0.2, color = "grey85"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0),
    plot.subtitle = element_text(color = "grey30"),
    plot.caption = element_text(color = "grey40")
  )

# ------------------ A) Student means: help-rate vs mean idle ------------------
student_summ <- d %>%
  group_by(anon_student_id) %>%
  summarise(
    n_sessions = n(),
    help_rate = mean(helped_session, na.rm = TRUE),
    idle_mean = mean(idle, na.rm = TRUE),
    prior_visits_mean = mean(prior_visits, na.rm = TRUE),
    .groups = "drop"
  )

pA <- student_summ %>%
  filter(n_sessions >= 8) %>%  # avoid very small-sample noise
  ggplot(aes(x = idle_mean, y = help_rate, size = n_sessions)) +
  geom_point(alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, linewidth = 0.9) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Between-student differences: Help rate vs. Mean idleness",
    subtitle = "Points sized by # sessions per student (n ≥ 8)",
    x = "Mean idleness (student-level)",
    y = "Helped-session rate",
    caption = "Shaded band: LOESS ± SE"
  ) +
  pub_theme
pA
ggsave("plot_A_help_rate_vs_idle_mean.pdf", pA, width = 7, height = 5)

# ------------------ Extract random effects with CIs (students & classes) ------
# lme4: ranef(..., condVar=TRUE) stores posterior var for REs
re_list <- ranef(m, condVar = TRUE)

# Student REs
re_stu <- re_list$anon_student_id
pv_stu  <- attr(re_stu, "postVar")[1, 1, ]           # posterior variance per RE
df_stu <- tibble(
  anon_student_id = rownames(re_stu),
  re_logit = as.numeric(re_stu[, "(Intercept)"]),
  se = sqrt(pv_stu)
) %>%
  mutate(
    or = exp(re_logit),                 # random-intercept as OR vs average student
    lo = exp(re_logit - 1.96 * se),
    hi = exp(re_logit + 1.96 * se)
  ) %>%
  left_join(student_summ, by = "anon_student_id") %>%
  arrange(re_logit)

# Class REs
re_cls <- re_list$class
pv_cls <- attr(re_cls, "postVar")[1, 1, ]
df_cls <- tibble(
  class = rownames(re_cls),
  re_logit = as.numeric(re_cls[, "(Intercept)"]),
  se = sqrt(pv_cls)
) %>%
  mutate(
    or = exp(re_logit),
    lo = exp(re_logit - 1.96 * se),
    hi = exp(re_logit + 1.96 * se)
  ) %>%
  arrange(re_logit)

# ------------------ B) Caterpillar: students (top/bottom 25 by RE) -----------
top_bottom_stu <- bind_rows(
  head(df_stu, 25),
  tail(df_stu, 25)
) %>%
  mutate(label = fct_inorder(anon_student_id))

pB <- ggplot(top_bottom_stu, aes(x = or, y = label)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    title = "Student random intercepts (odds ratios)",
    subtitle = "Top/bottom 25 (controls: prior_visits, idle)",
    x = "OR vs. average student (logit RE exponentiated)",
    y = NULL,
    caption = "Horizontal bars: 95% posterior intervals"
  ) +
  pub_theme
pB
ggsave("plot_B_student_caterpillar_OR.pdf", pB, width = 7, height = 8)

# ------------------ C) Caterpillar: classes -----------------------------------
df_cls <- df_cls %>% mutate(label = fct_inorder(class))

pC <- ggplot(df_cls, aes(x = or, y = label)) +
  geom_point() +
  geom_errorbarh(aes(xmin = lo, xmax = hi), height = 0) +
  geom_vline(xintercept = 1, linetype = "dashed") +
  labs(
    title = "Class random intercepts (odds ratios)",
    subtitle = "Controls included: prior_visits, idle",
    x = "OR vs. average class (logit RE exponentiated)",
    y = NULL,
    caption = "Horizontal bars: 95% posterior intervals"
  ) +
  pub_theme
pC
ggsave("plot_C_class_caterpillar_OR.pdf", pC, width = 7, height = 6)

# ------------------ D) Funnel plot: student help-rate vs sample size ----------
p_hat <- mean(d$helped_session, na.rm = TRUE)
stu_funnel <- student_summ %>%
  transmute(anon_student_id, n = n_sessions, help_rate) %>%
  filter(n >= 3) %>%
  arrange(n) %>%
  mutate(
    se = sqrt(p_hat * (1 - p_hat) / n),
    lo95 = p_hat - 1.96 * se,
    hi95 = p_hat + 1.96 * se,
    lo99 = p_hat - 2.576 * se,
    hi99 = p_hat + 2.576 * se
  )

pD <- ggplot(stu_funnel, aes(x = n, y = help_rate)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(y = lo95), color = "grey50") +
  geom_line(aes(y = hi95), color = "grey50") +
  geom_line(aes(y = lo99), color = "grey70", linetype = "dotted") +
  geom_line(aes(y = hi99), color = "grey70", linetype = "dotted") +
  geom_hline(yintercept = p_hat, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Funnel plot of student help rates",
    subtitle = sprintf("Overall mean = %.1f%% (dashed); 95%% and 99%% binomial limits",
                       100 * p_hat),
    x = "Number of sessions per student",
    y = "Helped-session rate",
    caption = "Limits: binomial SE around overall mean"
  ) +
  pub_theme
pD
ggsave("plot_D_funnel_help_rate.pdf", pD, width = 7, height = 5)

# ---------------------------------- Setup -------------------------------------
library(tidyverse)
library(janitor)

d <- read_csv("session_level_data_with_help (1).csv") %>%
  clean_names() %>%
  filter(class_session_type == "classwork") %>%
  mutate(helped_session = as.integer(helped_session))

# Student-level aggregation (no modeling)
student_summ <- d %>%
  group_by(anon_student_id) %>%
  summarise(
    n_sessions = n(),
    help_rate  = mean(helped_session, na.rm = TRUE),
    helped_cnt = sum(helped_session, na.rm = TRUE),
    idle_mean  = mean(idle, na.rm = TRUE),
    prior_visits_mean = mean(prior_visits, na.rm = TRUE),
    .groups = "drop"
  )

# Publication theme
pub_theme <- theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(linewidth = 0.2, colour = "grey85"),
    panel.grid.minor = element_blank(),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    plot.subtitle = element_text(colour = "grey30"),
    plot.caption = element_text(colour = "grey40")
  )

# --------------------------------- Plot 1 -------------------------------------
# Distribution of student help rates (hist + density)
p1 <- ggplot(student_summ, aes(x = help_rate)) +
  geom_histogram(binwidth = 0.05, boundary = 0, closed = "left", alpha = 0.8) +
  geom_density(linewidth = 0.8, adjust = 1.2, alpha = 0.1) +
  geom_vline(xintercept = mean(student_summ$help_rate, na.rm = TRUE),
             linetype = "dashed") +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Distribution of student helped-session rates",
    subtitle = sprintf("Dashed line = mean %.1f%% (N students = %d)",
                       100 * mean(student_summ$help_rate, na.rm = TRUE),
                       nrow(student_summ)),
    x = "Helped-session rate (per student)",
    y = "Count of students"
  ) +
  pub_theme
p1
ggsave("P1_student_help_rate_distribution.pdf", p1, width = 7, height = 5)

# --------------------------------- Plot 2 -------------------------------------
# 2D binned heatmap: mean idleness vs help rate (shows density without overplot)
p2 <- ggplot(student_summ, aes(x = idle_mean, y = help_rate)) +
  geom_bin2d(bins = 30) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_fill_gradient(name = "Students", trans = "sqrt") +
  labs(
    title = "Student mean idleness vs helped-session rate",
    subtitle = "2D binned counts (darker = more students)",
    x = "Mean idleness (per student)",
    y = "Helped-session rate (per student)"
  ) +
  pub_theme
p2
ggsave("P2_idle_mean_vs_help_rate_bin2d.pdf", p2, width = 7, height = 5)

# --------------------------------- Plot 3 -------------------------------------
# Help rates across idle quartiles (student level)
student_summ_q <- student_summ %>%
  mutate(
    idle_q = ntile(idle_mean, 4),
    idle_q = factor(idle_q,
                    labels = c("Q1 (lowest idle)", "Q2", "Q3", "Q4 (highest idle)"))
  )

counts_q <- student_summ_q %>% count(idle_q) %>% mutate(lbl = paste0(idle_q, ": n=", n))
subtitle_q <- paste(counts_q$lbl, collapse = "   ")

p3 <- ggplot(student_summ_q, aes(x = idle_q, y = help_rate)) +
  geom_violin(trim = FALSE, alpha = 0.7) +
  geom_boxplot(width = 0.12, outlier.size = 0.8, alpha = 0.8) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Helped-session rates by student idle quartile",
    subtitle = subtitle_q,
    x = "Student mean idleness (quartiles)",
    y = "Helped-session rate (per student)"
  ) +
  pub_theme
p3
ggsave("P3_help_rate_by_idle_quartile.pdf", p3, width = 7, height = 5)

# --------------------------------- Plot 4 -------------------------------------
# Lorenz curve of help distribution across students (+ Gini)
lorenz_df <- student_summ %>%
  arrange(helped_cnt) %>%
  mutate(
    rank = row_number(),
    cum_students = rank / n(),
    cum_help = cumsum(helped_cnt),
    cum_help_share = if (sum(helped_cnt) > 0) cum_help / sum(helped_cnt) else 0
  )

# Gini (discrete formula, y sorted ascending)
gini <- {
  y <- lorenz_df$helped_cnt
  n <- length(y)
  if (sum(y) == 0) NA_real_ else
    (2 * sum((1:n) * y) / (n * sum(y))) - (n + 1) / n
}

p4 <- ggplot(lorenz_df, aes(x = cum_students, y = cum_help_share)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  geom_line(linewidth = 1) +
  coord_equal() +
  scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Lorenz curve: Distribution of help across students",
    subtitle = if (is.na(gini)) "No helped sessions observed"
    else sprintf("Gini = %.3f (0 = equal help, 1 = concentrated)", gini),
    x = "Cumulative share of students",
    y = "Cumulative share of helped sessions"
  ) +
  pub_theme
p4
ggsave("P4_lorenz_help_distribution.pdf", p4, width = 6, height = 6)

