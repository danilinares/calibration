library(tidyverse)
library(broom)
library(cowplot)
library(psyphy)
library(rlang)

list.files("R", full.names = TRUE) %>% walk(source)
source("parameters.R")

load("logdata/dat_resp.RData")

# probabilities ----------------------------------------------------------------
prob <- calculate_proportions(dat_resp, correct, duration, 
                              platform, size, participant)%>% 
  mutate(r = n - k, log10_duration = log10(duration)) %>% 
  ungroup()


# models ------------ ----------------------------------------------------------
models <- prob %>% 
  group_by(participant, platform) %>% 
  nest() %>% 
  mutate(
    dif_slope = map(data, 
                    ~glm(cbind(k, r) ~ size / log10_duration - 1, 
                         data = ., family = binomial(mafc.logit(2)))),
    same_slope = map(data, 
                    ~glm(cbind(k, r) ~ size + log10_duration - 1, 
                         data = ., family = binomial(mafc.logit(2))))
    )

model_comparisons <- models %>% 
  group_by(participant, platform) %>% 
  mutate(anov = map2(dif_slope, same_slope, anova, test = "Chisq"),
         p.value = map_dbl(anov, ~.$`Pr(>Chi)`[2])) %>% 
  filter(p.value < .05)

model_same_slope <- models %>% dplyr::select(-dif_slope)

# durations --------------------------------------------------------------------
log10_duration_seq <- prob %>% 
  distinct(size) %>% 
  crossing(tibble(log10_duration = seq(log_duration_min, 
                                       log_duration_max, length.out = 100)))

# psychometric functions  ------------------------------------------------------
calculate_predictions <- function (df, x_seq){
  df %>% 
    mutate(prob = map(same_slope, augment, 
                      newdata = x_seq, 
                      type.predict = "response")) %>% 
    dplyr::select(-data, -same_slope) %>% 
    unnest() %>% 
    mutate(duration = 10^log10_duration)
}

predictions <- model_same_slope%>% 
  calculate_predictions(prob %>% distinct(size, log10_duration))

psychometric_functions <- model_same_slope %>% 
  calculate_predictions(log10_duration_seq)

# thresholds -------------------------------------------------------------------
calculate_thresholds <- function(df) {
  df %>% 
    group_by(participant, platform) %>% 
    mutate(thresholds = map(same_slope, 
                            . %>% tidy() %>% 
                              dplyr::select(term, estimate) %>% 
                              spread(term, estimate) %>% 
                              mutate(Small = - sizeSmall / log10_duration, 
                                     Large = - sizeLarge / log10_duration) %>% 
                              dplyr::select(Small, Large) %>% 
                              gather(size, log_threshold) %>% 
                              mutate(threshold = 10^log_threshold)
    )) %>% 
    dplyr::select(-data, -same_slope) %>% 
    unnest()  
}

thresholds <- calculate_thresholds(model_same_slope)

# thresholds: bootstrap confidence intervals -----------------------------------
prob_samples <- prob %>% 
  group_by(participant, platform) %>% 
  summarise(n = first(n)) %>% 
  left_join(predictions) %>% 
  group_by(participant, platform) %>% 
  nest()

  nest() %>% 
  mutate(data_boot = map(data, 
                         
                         
                         
                         
                         tibble(sample = 1:10, 
                                n = . %>% distinct(n) %>% pull(),
                                prob = list(fit)) %>% 
                           unnest() %>%
                           rowwise() %>% 
                           mutate(k = rbinom(1, size = n, prob = .fitted), 
                                  r = n -k, 
                                  prob = k /n)
  )
          )
  
  

fit_boot <- boot_samples %>% 
  group_by(sample) %>% 
  nest() %>% 
  mutate(pre = map(data, fit_psy)) %>% 
  dplyr::select(-data) %>% 
  unnest()

# plot psychometric functions -------------------------------------------------- 

p_size <- ggplot(prob, aes(x = duration, y = prob, 
                                color = size, shape = size)) +
  facet_grid(platform~ participant, scales = "free") +
  geom_line(data = psychometric_functions, size = size_line, 
            aes(y = .fitted)) +
  geom_segment(data = thresholds,
               aes(x = threshold, xend = threshold,
                   y = -Inf, yend = .75, color = size),
               size = size_line * .5) +
  geom_point(size = size_point) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_fill_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = label_duration, y = label_proportion,
       color = label_size, shape = label_size, fill = label_size) +
  theme(legend.position = "top",
        legend.text = element_text(size = 9))


ggsave("figures/size.pdf", p_size, width = two_columns_width, height = 2.5) 
  


  #geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_ribbon(data = psycho_same_slope, 
              aes(x = duration,
                  ymin = prob_min, ymax = prob_max, 
                  fill = size), colour = NA, alpha = alpha_fill_level) +

  geom_linerange(aes(ymin = ymin, ymax = ymax), 
                 size = size_line * .5) +
  geom_segment(data = thresholds_same_slope_all,
               aes(x = threshold, xend = threshold,
                   y = -Inf, yend = prob, color = size),
               size = size_line * .5) +
  geom_segment(data = thresholds_same_slope_all, 
               aes(x = thresholdmax, xend = thresholdmin, 
                   y = prob, yend = prob, color = size),
               size = size_line * .5) +
  geom_line(data = psycho_same_slope, size = size_line) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_fill_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  coord_cartesian(ylim = c(0, 1)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size, fill = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9)) 

ggsave("figures/size_same_slope.pdf", p_size_same_slope, 
       width = two_columns_width, height = 2.5) 

  
  
                    ~tibble(prob = predict(., newdata = log10_duration_seq)),
                    log10_duration = log10_duration_seq) %>% 
  dplyr::select(-data, -same_slope) %>% 
  unnest()


psychometric_functions <- models %>% 
  dplyr::select(-dif_slope) %>% 
  mutate(prob = map(same_slope, 
                    ~tibble(prob = predict(., newdata = log10_duration_seq)),
                            log10_duration = log10_duration_seq))


                    ) %>% 
                     bind_cols(log10_duration_seq))
         
         %>% 
           bind_cols(log10_duration_seq)
  )
  
psycho = map(model, ~as_tibble(
  predict(., newdata = log10_duration_seq_size_df, 
          type = "response",
          se.fit = TRUE)) %>% 
    bind_cols(log10_duration_seq_size_df) %>% 
    rename(prob = fit) %>% 
    mutate(prob_min = prob - 2.58 * se.fit, 
           prob_max = prob + 2.58 * se.fit) # hay que buscar que numero va aqui (t)
)
)

pre_two_interaction_one_slope <- augment(
  model_two_interaction_one_slope, 
  newdata = log10_duration_seq_size_df,
  type.predict = "response")

