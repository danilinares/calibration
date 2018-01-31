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
predictions_n <- prob %>% 
  group_by(participant, platform) %>% 
  summarise(n = first(n)) %>% 
  left_join(predictions) 

prob_samples <- tibble(sample = 1:100, prob = list(predictions_n)) %>% 
  unnest() %>% 
  group_by(participant, platform, sample) %>% 
  nest() %>% 
  mutate(data = map(data, . %>% 
                      rowwise() %>% 
                      mutate(k = rbinom(1, size = n, prob = .fitted), 
                             r = n -k, 
                             prob = k /n)))

model_same_slope_boot <- prob_samples %>% 
  group_by(participant, platform, sample) %>% 
  mutate(
    same_slope = map(data, 
                     ~glm(cbind(k, r) ~ size + log10_duration - 1, 
                          data = ., family = binomial(mafc.logit(2))))
  )

thresholds_boot <- calculate_thresholds(model_same_slope_boot)


# confidence intervals 
conf_int <- thresholds_boot %>% 
  group_by(participant, platform, size) %>% 
  summarise(threshold_min = quantile(threshold, .05 /2),
            threshold_max = quantile(threshold, 1 - .05 /2))

conf_int_size <- conf_int %>% 
  mutate(prob = if_else(size == "Large", .75, .73))

conf_int_platform <- conf_int %>% 
  mutate(prob = if_else(platform == "CRT", .75, .73))

differences_size <- thresholds_boot %>% 
  dplyr::select(-log_threshold) %>% 
  spread(size, threshold) %>% 
  mutate(dif = Large - Small) %>% 
  group_by(participant, platform) %>% 
  summarise(dif_min = quantile(dif, .05 /2), 
            dif_max = quantile(dif, 1 - .05 /2), 
            significant = if_else(dif_min * dif_max > 0, "*",""))

differences_platform <- thresholds_boot %>% 
  dplyr::select(-log_threshold) %>% 
  spread(platform, threshold) %>% 
  mutate(dif = CRT - iPad) %>% 
  group_by(participant, size) %>% 
  summarise(dif_min = quantile(dif, .05 /2), 
            dif_max = quantile(dif, 1 - .05 /2), 
            significant = if_else(dif_min * dif_max > 0, "*",""))
  
                      
# plot psychometric functions -------------------------------------------------- 
p_size <- ggplot(prob) +
  facet_grid(platform~ participant, scales = "free") +
  geom_line(data = psychometric_functions, size = .5 * size_line, 
            aes(x = duration,y = .fitted,  color = size)) +

  geom_point(aes(x = duration, y = prob, 
                 color = size, shape = size), size = size_point) +
  geom_segment(data = conf_int_size, 
               aes(x = threshold_min, xend = threshold_max, 
                   y = prob, yend = prob, color = size),
               size = size_line) +
  geom_text(data = differences_size, aes(label = significant, 
                                    x = .01, y = .9)) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_fill_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0.005, .3)) +
  labs(x = label_duration, y = label_proportion,
       color = label_size, shape = label_size, fill = label_size) +
  theme(legend.position = "top",
        legend.text = element_text(size = 9))

ggsave("figures/size.pdf", p_size, width = two_columns_width, height = 2.5) 

p_platform <- ggplot(prob) +
  facet_grid(size ~ participant, scales = "free") +
  geom_line(data = psychometric_functions, size = .5 * size_line, 
            aes(x = duration,y = .fitted,  color = platform)) +
  geom_point(aes(x = duration, y = prob, 
                 color = platform, shape = platform), size = size_point) +
  geom_segment(data = conf_int_platform, 
               aes(x = threshold_min, xend = threshold_max, 
                   y = prob, yend = prob, color = platform),
               size = size_line) +
  geom_text(data = differences_platform, aes(label = significant, 
                                    x = .01, y = .9)) +
  scale_color_brewer(labels = name_size, palette = "Dark2") +
  scale_fill_brewer(labels = name_size, palette = "Dark2") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  coord_cartesian(ylim = c(0, 1), xlim = c(0.005, .3)) +
  labs(x = label_duration, y = label_proportion,
       color = label_platform, shape = label_platform, fill = label_platform) +
  theme(legend.position = "top",
        legend.text = element_text(size = 9))

ggsave("figures/platform.pdf", p_platform, width = two_columns_width, height = 2.5) 
  
p_psycho <- plot_grid(p_size, p_platform, ncol = 1, labels = c("A", "B"))

ggsave("figures/psycho.pdf", p_psycho, 
       width = two_columns_width, height = 5) 
