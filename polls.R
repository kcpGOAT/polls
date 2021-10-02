library(tidyverse)
library(ggthemes)
library(modelr)

## COVID
by_pollster <- covid_concern %>%
  group_by(pollster) %>%
  summarize(sample_size = mean(sample_size, na.rm = TRUE), 
            prop_econ = sum(subject == "concern-economy")/n())
samplesize_mean <- mean(by_pollster$sample_size)
by_pollster <- by_pollster %>%
  mutate(aboveMean = sample_size > samplesize_mean)


ggplot(by_pollster, aes(reorder(pollster, sample_size), sample_size)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_y_continuous(breaks = pretty(0:40000, n = 9)) +
  theme_stata() +
  theme(axis.text.y = element_text(angle = 360, size = 7.5)) +
  labs(y = "Sample size", x = NULL, 
       title = "Pollsters by their average sample size")

# covid19_econ_questions.png
ggplot(by_pollster[by_pollster$prop_econ != 0, ], 
       aes(reorder(pollster, prop_econ), prop_econ)) +
  geom_bar(stat = "identity", aes(fill = prop_econ == 1)) +
  coord_flip() +
  theme_light() +
  scale_y_continuous(breaks = pretty(0:1, n = 11)) +
  theme(axis.text.y = element_text(angle = 360, size = 12),
        text = element_text(family = "Optima"), 
        panel.grid.major = element_blank()) +
  labs(x = NULL, y = NULL, 
       title = "COVID-19 pollsters with highest proportion of economic-related questions", 
       caption = "Source: fivethirtyeight") +
  guides(fill = "none")

## BIDEN
biden_approval <- biden_approval %>%
  mutate(net_approval = yes - no)
biden_approval$end_date <- as.Date(biden_approval$end_date, 
                                   format = "%m/%d/%y")

biden_approval1 <- biden_approval %>%
  group_by(end_date) %>%
  summarize(net_approval = mean(net_approval, na.rm = TRUE)) %>%
  mutate(isPos = net_approval > 0)

net_approval_mod <- lm(net_approval ~ end_date, data = biden_approval1)
biden_approval1 <- biden_approval1 %>%
  add_predictions(net_approval_mod, var = "net_approval_pred")

# biden_net_approval.png
ggplot(biden_approval1, aes(end_date, net_approval, group = 1)) +
  geom_line(aes(y = net_approval)) +
  geom_line(aes(y = net_approval_pred), alpha = 0.2) +
  geom_area(aes(y = net_approval), fill = "gray") +
  scale_x_date(breaks = pretty(biden_approval1$end_date, n = 6)) +
  scale_y_continuous(breaks = pretty(-5:25, n = 7)) +
  labs(x = "Date",
       y = "Net approval",
       title = "Biden's net approval over time") +
  theme_light() +
  guides(fill = "none") +
  theme(text = element_text(face = "bold"))

# biden_net_approval_color.png
ggplot(biden_approval1, aes(end_date, net_approval, group = 1)) +
  geom_line(aes(y = net_approval)) +
  geom_area(aes(y = net_approval), fill = "black") +
  geom_rect(xmin = as.Date(c("2021-01-29")), 
            xmax = as.Date(c("2021-09-13")), 
            ymin = 0, ymax = Inf, fill = "deepskyblue", alpha = 0.002) +
  geom_rect(xmin = as.Date(c("2021-01-29")), 
            xmax = as.Date(c("2021-09-13")), 
            ymin = -Inf, ymax = 0, fill = "red", alpha = 0.002) +
  scale_x_date(breaks = pretty(biden_approval1$end_date, n = 6)) +
  scale_y_continuous(breaks = pretty(-5:25, n = 7)) +
  labs(x = "Date",
       y = "Net approval",
       title = "Biden's net approval over time") +
  theme_light() +
  guides(fill = "none") +
  theme(text = element_text(face = "bold"))





  
