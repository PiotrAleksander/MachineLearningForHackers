library(foreign)
library(ggplot2)

data.dir <- "data/roll_call/"
data.files <- list.files(data.dir)

rollcall.data <- lapply(data.files, function(f) read.dta(paste(data.dir, f, sep=""), convert.factors = FALSE))

rollcall.simplified <- function(df) {
  no.pres <- subset(df, state < 99)
  for(i in 10:ncol(no.pres)) {
    no.pres[,i] <- ifelse(no.pres[,i] > 6, 0, no.pres[,i])
    no.pres[,i] <- ifelse(no.pres[,i] > 0 & no.pres[,i] < 4, 1, no.pres[,i])
    no.pres[,i] <- ifelse(no.pres[,i] > 1, -1, no.pres[,i])
  }
  return(as.matrix(no.pres[,10:ncol(no.pres)]))
}

rollcall.simple <- lapply(rollcall.data, rollcall.simplified)

rollcall.dist <- lapply(rollcall.simple, function(m) dist(m %*% t(m)))
rollcall.mds <- lapply(rollcall.dist, function(d) as.data.frame((cmdscale(d, k=2)) * -1))

congresses <- 101:111
for(i in 1:length(rollcall.mds)) {
  names(rollcall.mds[[i]]) <- c("x", "y")
  congress <- subset(rollcall.data[[i]], state < 99)
  congress.names <- sapply(as.character(congress$name), 
                           function(n) strsplit(n, "[, ]")[[1]][1])
  rollcall.mds[[i]] <- transform(rollcall.mds[[i]], name=congress.names, 
                                 party=as.factor(congress$party), congress=congresses[i])
}

cong.110 <- rollcall.mds[[9]]

base.110 <- ggplot(cong.110, aes(x = x, y = y)) +
  scale_size(range = c(2,2), guide = 'none') +
  scale_alpha(guide = 'none') +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggtitle("Roll Call Vote MDS Clustering for 110th U.S. Senate") +
  xlab("") +
  ylab("") +
  scale_shape(name = "Party", breaks = c("100", "200", "328"),
              labels = c("Dem.", "Rep.", "Ind."), solid = FALSE) +
  scale_color_manual(name = "Party", values = c("100" = "black",
                                                "200" = "dimgray",
                                                "328"="grey"),
                     breaks = c("100", "200", "328"),
                     labels = c("Dem.", "Rep.", "Ind."))

print(base.110 + geom_point(aes(shape = party,
                                alpha = 0.75,
                                size = 2)))
print(base.110 + geom_text(aes(color = party,
                               alpha = 0.75,
                               label = cong.110$name,
                               size = 2)))

all.mds <- do.call(rbind, rollcall.mds)
all.plot <- ggplot(all.mds, aes(x = x, y = y)) +
  geom_point(aes(shape = party, alpha = 0.75, size = 2)) +
  scale_size(range = c(2, 2), guide = 'none') +
  scale_alpha(guide = 'none') +
  theme_bw() +
  theme(axis.ticks = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        panel.grid.major = element_blank()) +
  ggtitle("Roll Call Vote MDS Clustering for U.S. Senate (101st - 111th Congress)") +
  xlab("") +
  ylab("") +
  scale_shape(name = "Party",
              breaks = c("100", "200", "328"),
              labels = c("Dem.", "Rep.", "Ind."),
              solid = FALSE) +
  facet_wrap(~ congress)

print(all.plot)
