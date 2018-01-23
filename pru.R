prob <- calculate_proportions(dat_resp, correct, duration,
                              size, participant)

fit_without_lapses <- quickpsy(prob %>% filter(participant != "05"), 
                               duration, k, n,
                               group  =.(participant, size),
                               guess  = .5, 
                               xmin = log10(.01), xmax = log10(.25), 
                               bootstrap = "none",
                               log = TRUE,
                               prob = .75)


# mirar que no estoy cayendo en los limites de parini
p_proportion_correct_size_without_lapses <- ggplot(prob,
                                                   aes(x = duration, y = prob, 
                                                       color = factor(size), 
                                                       shape = factor(size))) +
  facet_grid(.~ participant,
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
