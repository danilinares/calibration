library(tidyverse)
library(stringr)
library(quickpsy)

dat_resp_ipad <- quickreadfiles(path = "data", 
                                participant = str_pad(1:13, 2, pad = "0"),
                                platform = c("iPad"), 
                                session = c("01", "02")) %>%
  dplyr::select(session, platform, participant, Duration, Size, Direction,
         Correct, Contrast) %>% 
  rename(duration = Duration, size = Size, contrast = Contrast, 
         direction = Direction, correct = Correct)

dat_resp_crt <- quickreadfiles(path = "data", 
                               participant =str_pad(1:13, 2, pad = "0"),
                               platform = c("CRT"), 
                               session = c("01", "02")) %>%
  dplyr::select(session, platform, participant, duration, size, direction,
         correct, contrast) %>% 
  mutate(size = if_else(size == 90, 4, 1))

ages <- tibble(participant = str_pad(1:13, 2, pad = "0"),
              age = c(25, 33, 33, 24, 28, 25, 34, 22, 25, 25, 28, 22, 41),
              gender = c("F", "M", "M","M", "F","M", "F","F", "F","M", "F","F", "F"))


dat_resp <- dat_resp_crt %>% 
  bind_rows(dat_resp_ipad) %>% 
  mutate(size = if_else(size == 1, "Small", "Large"), 
                        duration_signed = duration * direction,
         duration = round(duration, 5))

save(dat_resp, file = "logdata/dat_resp.RData")

