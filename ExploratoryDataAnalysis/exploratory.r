data.file <- file.path('01_heights_weights_genders.csv')
heights.weights <- read.csv(data.file, header=TRUE,sep=',')
heights.weights$Height <- heights.weights$Height * 2.54
heights.weights$Weight <- heights.weights$Weight * 0.454
heights.weights$Gender <- gsub("Female", "Kobieta", heights.weights$Gender)
heights.weights$Gender <- gsub("Male", "Mężczyzna", heights.weights$Gender)

heights <- with(heights.weights, Height)
summary(heights)

my.mean <- function(x) {
  return(sum(x) / length(x))
}

my.median <- function(x) {
  sorted.x <- sort(x)
  
  if(length(x) %% 2 == 0) {
    indices <- c(length(x) / 2, length(x) / 2 + 1)
    return(mean(sorted.x[indices]))
  }
  else {
    index <- ceiling(length(x) / 2)
    return(sorted.x[index])
  }
}

my.var <- function(x) {
  m <- mean(x)
  return(sum((x - m) ^ 2) / (length(x) - 1))
}

my.sd <- function(x) {
  return(sqrt(my.var(x)))
}

library('ggplot2')
ggplot(heights.weights, aes(x = Height))+geom_histogram(binwidth = 1) + labs(x="Wzrost", y="liczba")
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights, aes(x = Height)) + geom_density() + labs(x="Wzrost", y="liczba")
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density() + labs(x = "Wzrost", y="gęstość", fill="Płeć")
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights, aes(x = Weight, fill = Gender)) + geom_density() + labs(x = "Waga", y="gęstość", fill="Płeć")
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights, aes(x = Height, fill = Gender)) + geom_density() + labs(x = "Wzrost", y="gęstość", fill="Płeć") + facet_grid(Gender ~ .)
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + labs(x = "Wzrost", y ="Waga")
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights, aes(x = Height, y = Weight)) + geom_point() + labs(x = "Wzrost", y ="Waga") + geom_smooth()
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights[1:20,], aes(x = Height, y = Weight)) + geom_point() + labs(x = "Wzrost", y ="Waga") + geom_smooth()
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights[1:200,], aes(x = Height, y = Weight)) + geom_point() + labs(x = "Wzrost", y ="Waga") + geom_smooth()
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights[1:2000,], aes(x = Height, y = Weight)) + geom_point() + labs(x = "Wzrost", y ="Waga") + geom_smooth()
readline(prompt="Naciśnij [enter] aby kontynuować")
ggplot(heights.weights, aes(x = Height, y = Weight, color = Gender)) + geom_point() + labs(x = "Wzrost", y ="Waga", color="Płeć")
readline(prompt="Naciśnij [enter] aby kontynuować")

heights.weights <- transform(heights.weights, Male = ifelse(Gender == 'Mężczyzna', 1, 0))
logit.model <- glm(Male ~ Height + Weight, data = heights.weights, family = binomial(link = 'logit'))
ggplot(heights.weights, aes(x = Weight, y = Height, color = Gender)) + geom_point()
