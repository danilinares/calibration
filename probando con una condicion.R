# glm one--------------------------------------------------------------------------
one <- dat_resp %>% 
  filter(participant =="01",  platform == "CRT", size == 1)

prob <- calculate_proportions(one, correct, duration, size) %>% 
  mutate(r = n - k, 
         log10_duration = log10(duration)) %>% 
  ungroup()

p <- ggplot(prob, aes(x = log10_duration, y = prob)) +
  geom_point() 
p



model <- glm(cbind(k, r) ~ log10_duration, 
             data = prob, 
             family = binomial(mafc.logit(2)))


log10_duration_seq_df <- tibble(log10_duration = seq(log10(0.001), 
                                                     log10(.25), 
                                                     length.out = 100))
pre <- augment(model, 
               newdata = log10_duration_seq_df,
               type.predict = "response")


p <- p +
  geom_line(data = pre, aes(y = .fitted))
p

summary(model)

model_ind <- glm(cbind(k, r) ~ factor(size)/log10_duration , 
                 data = prob, 
                 family = binomial(mafc.logit(2)))
model_ind

model_ind <- glm(cbind(k, r) ~ factor(size) + log10_duration , 
                 data = prob, 
                 family = binomial(mafc.logit(2)))
model_ind

anova(model_ind, model_ind2, test = "Chisq")


pred_df <- prob %>% distinct(size) %>% 
  crossing(log10_duration = seq(log10(0.001), 
                                log10(.25), 
                                length.out = 100)) %>% 
  group_by(size) %>% 
  data_grid(log10_duration) %>% 
  ungroup() %>% 
  mutate(pred_all = predict(model_all, ., type = "response"),
         pred_ind = predict(model_ind, ., type = "response"))




curve <- tibble(x) %>% 
  mutate(y = predict(model, pred_df, type = "response"))


model <- glm(cbind(k, r) ~ size/log10_duration - 1, 
             data = prob, 
             family = binomial(mafc.logit(2)))



pred_df <- one %>% distinct(size) %>% 
  group_by(size) %>% 
  crossing(log10_duration = xseq)

yseq <- predict(model, pred_df, type = "response")
curves <- tibble(xseq, yseq)
ggplot(prob, aes(x = log10_duration, y = prob, color = factor(size))) +
  geom_point() #+
# geom_line(data = curves, aes(x =xseq, y= yseq))

