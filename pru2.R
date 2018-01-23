x <-  -2:2
dat_no_adapt <- tibble(x, k = c(61, 70, 81, 92, 97), cond = "no_adapt")
dat_adapt <- tibble(x, k = c(59, 59, 67, 86, 91), cond = "adapt")

dat <- dat_no_adapt %>% 
  bind_rows(dat_adapt) %>% 
  mutate(n = 100, s = n -k, prob = k /n)

dat_lesser <- dat %>% 
  group_by(x) %>% 
  summarise(k = sum(k), n = sum(n)) %>% 
  mutate(s = n - k, prob = k /n)


ggplot(dat) +
  geom_point(aes(x = x, y = prob, color = cond)) +
  geom_point(data = dat_full, aes(x = x, y = prob))


fit_quickpsy_lesser <- quickpsy(dat_lesser, x, k, n, guess = .5,
                              fun = logistic_fun)

fit_quickpsy_lesser$par

model_full <- glm(cbind(k, s) ~ x, data = dat_full, 
                  family = binomial(mafc.logit(2)))


fit_quickpsy_lesser <- quickpsy(dat, x, k, n, guess = .5,
                                fun = logistic_fun,
                                group = .(cond))
