library(tidyverse)
library(Sleuth3)
library(broom)
data("case2001")
donner <- case2001
donner %>%
  mutate(Survived = if_else(Status == "Survived", 1, 0)) ->
  donner

ggplot(donner, aes(x = Age, y = Survived)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) ->
  pl
ggsave(pl, filename = "obs.pdf", height = 1.2, width = 1.6)

p <- mean(donner$Survived)

################
donner %>%
  mutate(sim = sample(c(0, 1), size = n(), replace = TRUE, prob = c(1-p, p))) ->
  donner
glm(sim ~ Age, data = donner, family = "binomial") %>%
  tidy()
ggplot(donner, aes(x = Age, y = sim)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = binomial), se = FALSE) +
  theme_classic() +
  theme(axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank()) ->
  pl
## ggsave(pl, filename = "sim4.pdf", height = 1.2, width = 1.6)

## Normal dist

tibble(x = seq(-4, 4, length.out = 500)) %>%
  mutate(y = dnorm(x = x)) ->
  tdat
ggplot(tdat, aes(x = x, y = y)) +
  geom_line() +
  theme_classic() +
  theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          axis.title = element_blank()) +
  geom_ribbon(data = filter(tdat, x > 2.06), aes(x = x, ymax = y), ymin = 0, fill = "blue") +
  geom_ribbon(data = filter(tdat, x < -2.06), aes(x = x, ymax = y), ymin = 0, fill = "blue") +
  geom_vline(xintercept = -2.06, lty = 2, col = 2) ->
  pl

ggsave(filename = "./zdist.pdf", plot = pl, height = 1, width = 1.5)

