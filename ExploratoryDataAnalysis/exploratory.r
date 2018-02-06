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
