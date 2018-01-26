library(tidyverse)
library(broom)
library(cowplot)
library(modelfree)
library(psyphy)
library(modelr)

list.files("R", full.names = TRUE) %>% walk(source)
source("parameters.R")

load("logdata/dat_resp.RData")

# probabilities ----------------------------------------------------------------
prob <- calculate_proportions(dat_resp, correct, duration,
                              platform, size, participant) %>% 
  mutate(r = n - k, log10_duration = log10(duration)) %>% 
  ungroup()

# durations --------------------------------------------------------------------
log10_duration_seq_size_df <- prob %>% distinct(size) %>% 
  crossing(tibble(log10_duration = seq(log_duration_min, 
                                       log_duration_max, length.out = 100)))

log10_duration_seq_df <- tibble(log10_duration = seq(log_duration_min, 
                                                     log_duration_max, 
                                                     length.out = 100))

# glm different slope ----------------------------------------------------------
dif_slope <- prob %>% 
  group_by(participant, platform) %>% 
  nest() %>% 
  mutate(
    m = "dif_slope",
    model = map(data, 
                ~glm(cbind(k, r) ~ size / log10_duration, 
                     data = ., 
                     family = binomial(mafc.logit(2)))), 
    psycho = map(model,
               ~tibble(prob = predict(.,
                                      newdata = log10_duration_seq_size_df, 
                                      type = "response")) %>% 
                 bind_cols(log10_duration_seq_size_df))
  )

# hacer la predict con augment da errores (cuando hay Nas creo)

psycho_dif_slope <- dif_slope %>% 
  select(participant, platform, m, psycho) %>%
  unnest() %>% 
  mutate(duration = 10^log10_duration)

p_size_dif_slope <- ggplot(prob,
                           aes(x = duration, y = prob, 
                               color = size, shape = size)) +
  facet_grid(platform~ participant) +
  geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
  geom_line(data = psycho_dif_slope, size = size_line) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/size_dif_slope.pdf", p_size_dif_slope, 
       width = two_columns_width, height = 2.5) 

# glm different slope only one psychometric function ---------------------------
dif_slope_one <- prob %>% 
  group_by(participant, platform) %>% 
  nest() %>% 
  mutate(
    m = "dif_slope_one",
    model = map(data, 
                ~glm(cbind(k, r) ~ log10_duration, 
                     data = ., 
                     family = binomial(mafc.logit(2)))), 
    psycho = map(model,
                 ~tibble(prob = predict(.,
                                        newdata = log10_duration_seq_df, 
                                        type = "response")) %>% 
                   bind_cols(log10_duration_seq_df))
  )

psycho_dif_slope_one <- dif_slope_one %>% 
  select(participant, platform, m, psycho) %>%
  unnest() %>% 
  mutate(duration = 10^log10_duration)

p_size_dif_slope_one <- ggplot(prob) +
  facet_grid(platform~ participant) +
  geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_point(size = size_point,
             aes(x = duration, y = prob, color = size, shape = size)) +
  geom_line(data = psycho_dif_slope_one, size = size_line,
            aes(x = duration, y = prob)) +
  geom_line(data = psycho_dif_slope, size = size_line,
            aes(x = duration, y = prob, color = size)) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/size_dif_slope_one.pdf", p_size_dif_slope_one, 
       width = two_columns_width, height = 2.5) 


# comparing one vs two psychometric functions -------------------------------------------------------------------
dif_slope %>% 
  bind_rows(dif_slope_one) %>% 
  select(participant, platform, model, m) %>% 
  spread(m, model) %>% 
  mutate(anov = map2(dif_slope, dif_slope_one, anova, test = "Chisq"), 
         p.value = map_dbl(anov, ~.$`Pr(>Chi)`[2])) %>% # usar broom 
  select(participant, platform, p.value, everything()) %>% 
  filter(p.value > .01)

#' the result is in the line of two participants not showing suppression,
#' but to what we really need to do is too calculate the threshold and 
#' assess differences on the thresholds

# glm same slope ----------------------------------------------------------
same_slope <- prob %>% 
  group_by(participant, platform) %>% 
  nest() %>% 
  mutate(
    m = "same_slope",
    model = map(data, 
                ~glm(cbind(k, r) ~ size + log10_duration - 1, 
                     data = ., 
                     family = binomial(mafc.logit(2)))), 
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


psycho_same_slope <- same_slope %>% 
  select(participant, platform, m, psycho) %>%
  unnest() %>% 
  mutate(duration = 10^log10_duration)

thresholds_same_slope <- same_slope %>% 
  mutate(estimate = map(model, tidy),
         estimate = map(estimate, ~pull(., estimate)),
         sizeLarge = map_dbl(estimate, 1),
         sizeSmall = map_dbl(estimate, 2),
         log10_duration = map_dbl(estimate, 3),
         estimatemin = map(model, ~confint(., level = .99)[,1]),
         estimatemax = map(model, ~confint(., level = .99)[,2]),
         sizeLargemin = map_dbl(estimatemin, 1),
         sizeSmallmin = map_dbl(estimatemin, 2),
         log10_durationmin = map_dbl(estimatemin, 3),
         sizeLargemax = map_dbl(estimatemax, 1),
         sizeSmallmax = map_dbl(estimatemax, 2),
         log10_durationmax = map_dbl(estimatemax, 3)) %>% 
  dplyr::select(participant, platform, 
                sizeLarge, sizeSmall, log10_duration,
                sizeLargemin, sizeSmallmin, log10_durationmin, 
                sizeLargemax, sizeSmallmax, log10_durationmax) %>% 
  mutate(Large = - sizeLarge / log10_duration, 
         Small = - sizeSmall / log10_duration,
         Largemin = - sizeLargemin / log10_durationmin,
         Smallmin = - sizeSmallmin / log10_durationmin,
         Largemax = - sizeLargemax / log10_durationmax,
         Smallmax = - sizeSmallmax / log10_durationmax) %>% 
  dplyr::select(participant, platform, Large, Small, 
                Largemin, Smallmin, Largemax, Smallmax)

thresholds_same_slope_large <- thresholds_same_slope %>% 
  dplyr::select(participant, platform, Large, Largemin, Largemax) %>% 
  rename(threshold = Large, thresholdmin = Largemin, thresholdmax = Largemax) %>%
  mutate(size = "Large")

thresholds_same_slope_small <- thresholds_same_slope %>% 
  dplyr::select(participant, platform, Small, Smallmin, Smallmax) %>% 
  rename(threshold = Small, thresholdmin = Smallmin, thresholdmax = Smallmax) %>% 
  mutate(size = "Small")

thresholds_same_slope_all <- thresholds_same_slope_large %>% 
  bind_rows(thresholds_same_slope_small) %>% 
  mutate(threshold = 10^threshold, 
         thresholdmin = 10^thresholdmin, 
         thresholdmax = 10^thresholdmax, 
         prob = .75)


p_size_same_slope <- ggplot(prob,
                           aes(x = duration, y = prob, 
                               color = size, shape = size)) +
  facet_grid(platform~ participant, scales = "free") +
  #geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_ribbon(data = psycho_same_slope, 
              aes(x = duration,
                  ymin = prob_min, ymax = prob_max, 
                  fill = size), colour = NA, alpha = alpha_fill_level) +
  geom_point(size = size_point) +
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
  scale_y_continuous(breaks = seq(0, 1,.25), limits = c(0,1)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size, fill = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9)) 

ggsave("figures/size_same_slope.pdf", p_size_same_slope, 
       width = two_columns_width, height = 2.5) 

# comparing same vs different slope psychometric functions ----------------------------------
dif_slope %>% 
  bind_rows(same_slope) %>% 
  select(participant, platform, model, m) %>% 
  spread(m, model) %>% 
  mutate(anov = map2(dif_slope, same_slope, anova, test = "Chisq"), 
         p.value = map_dbl(anov, ~.$`Pr(>Chi)`[2])) %>% # usar broom 
  select(participant, platform, p.value, everything()) %>% 
  filter(p.value < .01)









#### porqueria














dif_slope$model[[2]] %>% augment(#newdata = log10_duration_seq_size_df,
                                 type.predict = "response")

    pred = map(model, ~augment(., 
                               newdata = log10_duration_seq_size_df,
                               type.predict = "response"))
  )
 
dif_slope$model %>% 
  map(~augment(.,newdata = log10_duration_seq_size_df,
               type.predict = "response"))

dif_slope$model[[4]] %>% augment(newdata = log10_duration_seq_size_df,
               type.predict = "response")

glms_same_slope <- prob %>% 
  group_by(participant, platform) %>% 
  nest() %>% 
  mutate(
    model = map(data, 
                ~glm(cbind(k, r) ~ log10_duration + size, 
                     data = ., 
                     family = binomial(mafc.logit(2)))),
    dur = list(distinct(prob, size) %>% 
                 crossing(
                   log10_duration = seq(log10(0.01), log10(.25), 
                                        length.out = 100))),
    pred = map2(model, dur, ~tibble(prob = predict(.x, .y, type = "response"))),
    aic = map_dbl(model, "aic"),
    estimate = map(model, ~tibble(estimate = tidy(.)$estimate,
                                  estimaten = 1:3))
  ) %>% 
  mutate(m = "same_slope")

# psychometric functions ####
curves_dif_slope <- glms_dif_slope %>% 
  select(participant, platform, pred, m) %>%
  unnest() %>% 
  mutate(duration = 10^log10_duration)

# plot psychometric functions ####


p_pro_platform_dif_slope <- ggplot(prob,
                                   aes(x = duration, y = prob, 
                                       color = factor(platform), 
                                       shape = factor(platform))) +
  facet_grid(size~ participant,
             labeller = labeller(size = name_size)) +
  geom_hline(color = "grey", yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
  geom_line(data = curves_dif_slope, size = size_line) +
  scale_color_brewer(labels = name_size, palette = "Dark2") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion,
       color = label_platform, shape = label_platform) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/pro_platform_dif_slope.pdf", p_pro_platform_dif_slope, 
       width = two_columns_width, height = 2.5) 


# glm same slope ---------------------------------------------------------------
glms_same_slope <- prob %>% 
  group_by(participant, platform) %>% 
  nest() %>% 
  mutate(
    model = map(data, 
                ~glm(cbind(k, r) ~ log10_duration + size, 
                     data = ., 
                     family = binomial(mafc.logit(2)))),
    dur = list(distinct(prob, size) %>% 
                 crossing(
                   log10_duration = seq(log10(0.01), log10(.25), 
                                        length.out = 100))),
    pred = map2(model, dur, ~tibble(prob = predict(.x, .y, type = "response"))),
    aic = map_dbl(model, "aic"),
    estimate = map(model, ~tibble(estimate = tidy(.)$estimate,
                                  estimaten = 1:3))
  ) %>% 
  mutate(m = "same_slope")

# psychometric functions ####
curves_same_slope <- glms_same_slope %>% 
  select(participant, platform, dur, pred, m) %>%
  unnest() %>% 
  mutate(duration = 10^log10_duration)

# plot psychometric functions ####
p_pro_size_same_slope <- ggplot(prob, 
                                aes(x = duration, y = prob, 
                                    color = size, shape = size)) +
  facet_grid(platform~ participant) +
  geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
  geom_line(data = curves_same_slope, size = size_line) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/pro_size_same_slope.pdf", 
       p_pro_size_same_slope, width = two_columns_width, height = 2.5) 

p_pro_platform_same_slope <- ggplot(prob,
                                    aes(x = duration, y = prob, 
                                        color = factor(platform), 
                                        shape = factor(platform))) +
  facet_grid(size~ participant,
             labeller = labeller(size = name_size)) +
  geom_hline(color = "grey", yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
  geom_line(data = curves_same_slope, size = size_line) +
  scale_color_brewer(labels = name_size, palette = "Dark2") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion,
       color = label_platform, shape = label_platform) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/pro_platform_same_slope.pdf", p_pro_platform_same_slope, 
       width = two_columns_width, height = 2.5) 



# plot all psychometric functions ----------------------------------------------
p_psycho_same <- plot_grid(p_pro_size_same_slope, 
                      p_pro_platform_same_slope, ncol = 1)

ggsave("figures/psycho_same.pdf", p_psycho_same, 
       width = two_columns_width, height = 5) 

p_psycho <- plot_grid(p_pro_size_dif_slope, 
                      p_pro_platform_dif_slope,
                      p_pro_size_same_slope, 
                      p_pro_platform_same_slope, ncol = 2)

ggsave("figures/psycho.pdf", 
       p_psycho, width = 1.25 * two_columns_width, height = 5) 

# anovas -----------------------------------------------------------------------
glms <-  glms_same_slope %>% bind_rows(glms_dif_slope)

anov <- glms %>% 
  select(participant, platform, model, m) %>% 
  spread(m, model) %>% 
  mutate(anov = map2(same_slope, dif_slope, anova, test = "Chisq"), 
         pvalue = map_dbl(anov, ~.$`Pr(>Chi)`[2])) %>% # usar broom 
  select(participant, platform, pvalue, everything()) 

# thresholds -------------------------------------------------------------------

thresholds_same_slope <- glms_same_slope %>%
  select(participant, platform, estimate, m) %>% 
  unnest() %>% 
  spread(estimaten, estimate, sep = "_") %>% 
  mutate(threshold = )
    
pred_df <- prob %>% distinct(size) %>% 
  crossing(log10_duration = seq(log10(0.001), 
                                log10(.25), 
                                length.out = 100)) %>% 
  group_by(size) %>% 
  data_grid(log10_duration) %>% 
  ungroup() %>% 
  mutate(pred_all = predict(model_all, ., type = "response"),
         pred_ind = predict(model_ind, ., type = "response"))


# AICS -------------------------------------------------------------------------
aics_dif_slope <- glms_dif_slope %>% 
  group_by(participant, platform, m) %>%
  summarise(aic = sum(aic))

aics_same_slope <- glms_same_slope %>% 
  select(participant, platform, m, aic)

aics <- aics_dif_slope %>% 
  bind_rows(aics_same_slope) %>% 
  spread(m, aic) %>% 
  mutate(best = if_else(dif_slope < same_slope, "dif", "same"))

aics_sum <- aics %>% 
  group_by(platform) %>% 
  summarise(dif_slope = sum(dif_slope), same_slope = sum(same_slope))



# proportion correct -----------------------------------------------------------
prob <- calculate_proportions(dat_resp, correct, duration,
                              platform, size, participant)

fit_without_lapses <- quickpsy(prob %>% filter(participant != "05"), 
                duration, k, n,
                group  =.(platform, participant, size),
                guess  = .5, 
                xmax = log10(.25), 
                bootstrap = "none",
                log = TRUE,
                prob = .75)

aic_without_lapses <- fit_without_lapses$aic %>% 
  rename(aic_without = aic)

# mirar que no estoy cayendo en los limites de parini
p_proportion_correct_size_without_lapses <- ggplot(prob,
                                    aes(x = duration, y = prob, 
                                        color = factor(size), 
                                        shape = factor(size))) +
  facet_grid(platform~ participant,
              labeller = labeller(contrast = name_contrasts)) +
  geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
  geom_line(data = fit_without_lapses$curves, size = size_line, aes(x = x, y = y)) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/proportion_correct_size_without_lapses.pdf", 
       p_proportion_correct_size_without_lapses, 
       width = two_columns_width, height = 2.5) 



fit_with_lapses <- quickpsy(prob %>% filter(participant != "05"), 
                    duration, k, n,
                    group  =.(platform, participant, size),
                    guess  = .5, 
                    lapses = TRUE, 
                    parini = list(c(-10, 10), c(0.03, 2), c(0, .05)),
                    xmax = log(.25), 
                    bootstrap = "none",
                    log = TRUE,
                    prob = .75)


ggplot(fit_with_lapses$par, aes(x = par)) +
  facet_wrap(~parn, scales = "free", ncol = 1) +
  geom_histogram()

aic_with_lapses <- fit_with_lapses$aic %>% rename(aic_with = aic) 

aics <- aic_without_lapses %>% 
  left_join(aic_with_lapses) %>% 
  mutate(smaller_aic = if_else(aic_with < aic_without, "with", "without")) %>% 
  arrange(participant)
  
p_proportion_correct_size_with_lapses <- ggplot(prob,
                                                   aes(x = duration, y = prob, 
                                                       color = factor(size), 
                                                       shape = factor(size))) +
  facet_grid(platform~ participant) +
  geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
 # geom_line(data = fit_with_lapses$curves, size = size_line, aes(x = x, y = y)) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/proportion_correct_size_with_lapses.pdf", 
       p_proportion_correct_size_with_lapses, 
       width = two_columns_width, height = 2.5) 

p_psycho <- plot_grid(p_proportion_correct_size_without_lapses, 
                      p_proportion_correct_size_with_lapses, 
                      ncol = 1, 
                      labels = c("A", "B"))

ggsave("figures/psycho.pdf", p_psycho, width = two_columns_width, height = 5) 








# mirar que no estoy cayendo en los limites de parini
p_proportion_correct_size <- ggplot(prob,
                                  aes(x = duration, y = prob, 
                                      color = factor(size), 
                                      shape = factor(size))) +
  facet_grid( platform~ participant,
              labeller = labeller(contrast = name_contrasts)) +
  geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
  geom_line(data = fit$curves, size = size_line, aes(x = x, y = y)) +
 # geom_line(size =size_line) +
  #  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .1) +
 # geom_text(data = prob %>% filter(signif), position = position_dodge(0.1),
 #           aes(y = 0.2, label = '*')) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/proportion_correct_size.pdf", p_proportion_correct_size, 
       width = two_columns_width, height = 2.5) 

p_proportion_correct_platform <- ggplot(prob,
                                    aes(x = duration, y = prob, 
                                        color = factor(platform), 
                                        shape = factor(platform))) +
  facet_grid(size~ participant,
              labeller = labeller(size = name_size)) +
  geom_hline(color = "grey", yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
  geom_line(data = fit$curves, size = size_line, aes(x = x, y = y)) +
  scale_color_brewer(labels = name_size, palette = "Dark2") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion,
       color = label_platform, shape = label_platform) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

p_psycho <- plot_grid(p_proportion_correct_size, 
                      p_proportion_correct_platform, 
                      ncol = 1, 
                      labels = c("A", "B"))

ggsave("figures/psycho.pdf", p_psycho, width = two_columns_width, height = 5) 




# glm different slope ----------------------------------------------------------
prob <- calculate_proportions(dat_resp, correct, duration,
                              platform, size, participant) %>% 
  mutate(r = n - k, log10_duration = log10(duration)) %>% 
  ungroup()

glms_dif_slope <- prob %>% 
  group_by(participant, platform, size) %>% 
  nest() %>% 
  mutate(
    model = map(data, 
                ~glm(cbind(k, r) ~ log10_duration, 
                     data = ., 
                     family = binomial(mafc.logit(2)))),
    dur = list(tibble(
      log10_duration = seq(log10(0.01), log10(.25), length.out = 100))),
    pred = map2(model, dur, 
                ~tibble(prob = predict(.x, .y, type = "response"))),
    aic = map_dbl(model, "aic")
  ) %>% 
  arrange(participant) %>% 
  mutate(m = "dif_slope")

curves_dif_slope <- glms_dif_slope %>% 
  select(participant, platform, size, dur, pred, m) %>%
  unnest() %>% 
  mutate(duration = 10^log10_duration)

p_pro_size_dif_slope <- ggplot(prob,
                               aes(x = duration, y = prob, color = factor(size), shape = factor(size))) +
  facet_grid(platform~ participant) +
  geom_hline(color = "grey",  yintercept = 0.5, size = size_line) +
  geom_point(size = size_point) +
  geom_line(data = curves_dif_slope, size = size_line) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  scale_x_log10(breaks = c(.01, .04, .16), labels = c(".01", ".04", ".16")) +
  scale_y_continuous(breaks = seq(0, 1,.25)) +
  labs(x = label_duration, y = label_proportion, 
       color = label_size, shape = label_size) +
  theme(legend.position = "top", 
        legend.text = element_text(size = 9))

ggsave("figures/pro_size_dif_slope.pdf", p_pro_size_dif_slope, 
       width = two_columns_width, height = 2.5) 



# Correlation plots ------------------------------------------------------------

thresholds_long_size <- fit$thresholds %>% spread(platform, thre)  

p_correlation_size <- ggplot(thresholds_long_size, 
                      aes(x = CRT, y = iPad, color = factor(size), 
                          shape = factor(size))) +
  geom_abline(color = "grey", size = size_line) +
  geom_point(size = size_point) +
  geom_smooth(method = "lm", se = FALSE, size = size_line) +
  scale_color_brewer(labels = name_size, palette = "Set1") +
  scale_shape_discrete(labels = name_size) +
  coord_equal() +
  scale_x_log10(breaks = c(.01, .04, .16), 
                labels = c(".01", ".04", ".16"), 
                limits = c(.008,.3)) +
  scale_y_log10(breaks = c(.01, .04, .16), 
                labels = c(".01", ".04", ".16"),
                limits = c(.008,.3)) +
  labs(x = label_crt, y = label_ipad, 
       color = label_size, shape = label_size) +
  theme(legend.text = element_text(size = 9))
p_correlation_size


thresholds_long_platform <- fit$thresholds %>% spread(size, thre) 

p_correlation_platform <- ggplot(thresholds_long_platform, 
                             aes(x = `1`, y = `4`, color = factor(platform), 
                                 shape = factor(platform))) +
  geom_abline(color = "grey", size = size_line) +
  geom_point(size = size_point) +
  geom_smooth(method = "lm", se = FALSE, size = size_line) +
   scale_color_brewer(palette = "Dark2") +
  # scale_shape_discrete(labels = name_platform) +
  coord_equal() +
  scale_x_log10(breaks = c(.01, .04, .16), 
                labels = c(".01", ".04", ".16"), 
                limits = c(.008,.3)) +
  scale_y_log10(breaks = c(.01, .04, .16), 
                labels = c(".01", ".04", ".16"),
                limits = c(.008,.3)) +
   labs(x = label_small, y = label_large, 
        color = label_platform, shape = label_platform) +
  theme(legend.text = element_text(size = 9))

p_cor <- plot_grid(p_correlation_size, 
                   p_correlation_platform, 
                      ncol = 1, 
                      labels = c("A", "B"))

ggsave("figures/cor.pdf", p_cor, width = single_column_width, height = 3.5) 






proportion_correct <- ggplot(prob,
                             aes(x = duration, y = prob, color = platform)) +
  facet_grid(size ~ participant,
             labeller = labeller(contrast = name_contrasts)) +
  geom_hline(lty = 2, yintercept = 0.5) +
  geom_point() +
  geom_line() +
 # geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .1) +
  geom_text(data = prob %>% filter(signif), position = position_dodge(0.1),
            aes(y = 0.2, label = '*')) +
  scale_y_continuous(breaks = seq(0,1,.1)) +
  scale_x_log10(breaks = c(.01, .02, .04, .08, .16),
                labels = c('.01', '.02','.04', '.08', '.16')) +
  labs(x = 'Duration (s)', y = 'Probability correct', color = 'Platform')
proportion_correct

proportion_correct2 <- ggplot(prob,
                             aes(x = duration, y = prob, color = factor(size))) +
  facet_grid( platform~ participant,
             labeller = labeller(contrast = name_contrasts)) +
  geom_hline(lty = 2, yintercept = 0.5) +
  geom_point() +
  geom_line() +
#  geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .1) +
  geom_text(data = prob %>% filter(signif), position = position_dodge(0.1),
            aes(y = 0.2, label = '*')) +
  scale_y_continuous(breaks = seq(0,1,.1)) +
  scale_x_log10(breaks = c(.01, .02, .04, .08, .16),
                labels = c('.01', '.02','.04', '.08', '.16')) +
  labs(x = 'Duration (s)', y = 'Probability correct', color = 'Size')
proportion_correct2

p <- plot_grid(proportion_correct, proportion_correct2, ncol = 1)

p



ggsave("figures/p.pdf", p, height = 5, width = 10)

# proportion correct all -------------------------------------------------------
prob_all <- calculate_proportions(dat_resp, correct, platform,
                                  size, contrast, duration)

fit_all <- quickpsy(prob_all, duration, k, n,
                group  =.(size, platform),
                guess  = .5, 
                xmax = log(.25), bootstrap = "none",
                log = TRUE,
                prob = .75)


proportion_correct_all <- ggplot(prob_all,
                             aes(x = duration, y = prob, 
                                 color = factor(size), lty = platform, shape = platform)) +
  geom_hline(lty = 2, yintercept = 0.5) +
  geom_point() +
  geom_line(data = fit_all$curves, aes(x = x, y = y)) +
  geom_segment(data = fit_all$thresholds, aes(x = thre, y = 0, 
                                              xend = thre, yend = prob)) +
  # geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .1) +
  geom_text(data = prob %>% filter(signif), position = position_dodge(0.1),
            aes(y = 0.2, label = '*')) +
  scale_y_continuous(breaks = seq(0,1,.1)) +
  scale_x_log10(breaks = c(.01, .02, .04, .08, .16),
                labels = c('.01', '.02','.04', '.08', '.16')) +
  labs(x = 'Duration (s)', y = 'Probability correct', color = 'Platform')
proportion_correct_all

ggsave("figures/proportion_correct_all.pdf", proportion_correct_all, width = 4, 
       height = 3)




prob3 <- calculate_proportions(dat_resp %>% 
                                 mutate(size_platform = paste(size, "deg",platform)), 
                               correct, session, 
                              participant, size_platform, contrast, duration)



proportion_correct3 <- ggplot(prob3,
                             aes(x = duration, y = prob, color = session)) +
  facet_grid(participant ~ size_platform,
             labeller = labeller(contrast = name_contrasts)) +
  geom_hline(lty = 2, yintercept = 0.5) +
  geom_point() +
  geom_line() +
  # geom_errorbar(aes(ymin = ymin, ymax = ymax), width = .1) +
  geom_text(data = prob3 %>% filter(signif), position = position_dodge(0.1),
            aes(y = 0.2, label = '*')) +
  scale_y_continuous(breaks = seq(0,1,.1)) +
  scale_x_log10(breaks = c(.01, .02, .04, .08, .16),
                labels = c('.01', '.02','.04', '.08', '.16')) +
  labs(x = 'Duration (s)', y = 'Probability correct', color = 'Session')

proportion_correct3

fit <- quickpsy(prob, duration, k, n,
                group  =.(participant, size, platform),
                guess  = .5, 
                xmax = log(.25), bootstrap = "none",
                log = TRUE,
                prob = .75)

plot_grid(plot(fit, color = size), plot(fit, color = platform))


#comentario rafa

