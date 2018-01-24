library(tidyverse)
library(quickpsy)
library(broom)
library(cowplot)
library(modelfree)
library(psyphy)
library(modelr)

list.files("R", full.names = TRUE) %>% walk(source)
source("parameters.R")

# read data response -----------------------------------------------------------
dat_resp_ipad <- quickreadfiles(path = "data", 
                                participant = str_pad(1:13, 2, pad = "0"),
                                platform = c("iPad"), 
                                session = c("01", "02")) %>%
  select(session, platform, participant, Duration, Size, Direction,
         Correct, Contrast) %>% 
  rename(duration = Duration, size = Size, contrast = Contrast, 
         direction = Direction, correct = Correct)

dat_resp_crt <- quickreadfiles(path = "data", 
                               participant =str_pad(1:13, 2, pad = "0"),
                               platform = c("CRT"), 
                               session = c("01", "02")) %>%
  select(session, platform, participant, duration, size, direction,
         correct, contrast) %>% 
  mutate(size = if_else(size == 90, 4, 1))


dat_resp <- dat_resp_crt %>% 
  bind_rows(dat_resp_ipad) %>% 
  mutate(duration_signed = duration * direction)

# probabilities ----------------------------------------------------------------
prob <- calculate_proportions(dat_resp, correct, duration,
                              platform, size, participant) %>% 
  mutate(r = n - k, log10_duration = log10(duration)) %>% 
  ungroup()

# glm same slope ---------------------------------------------------------------
glms_same_slope <- prob %>% 
  group_by(participant, platform) %>% 
  nest() %>% 
  mutate(
    model = map(data, 
                ~glm(cbind(k, r) ~ log10_duration + factor(size), 
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
                                aes(x = duration, y = prob, color = factor(size), shape = factor(size))) +
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


# glm different slope ----------------------------------------------------------
glms_dif_slope <- prob %>% 
  group_by(participant, platform) %>% 
  nest() %>% 
  mutate(
    model = map(data, 
                ~glm(cbind(k, r) ~ log10_duration * size, 
                     data = ., 
                     family = binomial(mafc.logit(2)))),
    pred = map(model, ~augment(., 
                               newdata = log10_duration_seq_df,
                               type.predict = "response")),
    m = "dif_slope")

# psychometric functions ####
curves_dif_slope <- glms_dif_slope %>% 
  select(participant, platform, pred, m) %>%
  unnest() %>% 
  mutate(duration = 10^log10_duration)

# plot psychometric functions ####
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
p_pro_size_dif_slope

ggsave("figures/pro_size_dif_slope.pdf", p_pro_size_dif_slope, 
       width = two_columns_width, height = 2.5) 

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
#comentario rafa 2


