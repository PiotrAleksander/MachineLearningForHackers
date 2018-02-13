top.1000.sites <- read.csv('data/top_1000_sites.tsv', sep='\t', stringsAsFactors = FALSE)

library(ggplot2)
ggplot(top.1000.sites, aes(x = PageViews, y = UniqueVisitors)) +
  geom_point()
ggplot(top.1000.sites, aes(x = PageViews)) +
  geom_density()
ggplot(top.1000.sites, aes(x = log(PageViews))) +
  geom_density()
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) + 
  geom_point()

#linia regresji
ggplot(top.1000.sites, aes(x = log(PageViews), y = log(UniqueVisitors))) +
  geom_point() +
  geom_smooth(method = 'lm', se = FALSE)
lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors), data = top.1000.sites)

summary(lm.fit)

lm.fit <- lm(log(PageViews) ~ HasAdvertising + log(UniqueVisitors) + InEnglish, data = top.1000.sites)
summary(lm.fit)

#jak pojedyńcze składniki objaśniają wariancję wyników:
lm.fit <- lm(log(PageViews) ~ HasAdvertising, data = top.1000.sites)
summary(lm.fit)$r.squared

lm.fit <- lm(log(PageViews) ~ log(UniqueVisitors), data = top.1000.sites)
summary(lm.fit)$r.squared

lm.fit <- lm(log(PageViews) ~ InEnglish, data = top.1000.sites)
summary(lm.fit)$r.squared
