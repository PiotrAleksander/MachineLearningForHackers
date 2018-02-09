library(ggplot2)
ages <- read.csv('data/longevity.csv')
ggplot(ages, aes(x = AgeAtDeath, fill = factor(Smokes))) +
  labs(x = "Długość życia", y = "gęstość", fill="Palacz") +
  geom_density() +
  facet_grid(Smokes ~ .)

#obliczanie średniokwadratowego błędu dla różnych założonych długości życia w porównaniu do wszystkich ze bioru longevity.csv
guess.accuracy <- data.frame()

for (guess in seq(63, 83, by = 1)) {
  prediction.error <- with(ages, mean((AgeAtDeath - guess) ^ 2))
  guess.accuracy <- rbind(guess.accuracy, data.frame(Guess = guess, Error = prediction.error))
}

#wizualizacja, jak odbieganie od średniej pogarsza jakość predykcji (mierzoną poprzez błąd średniokwadratowy)
ggplot(guess.accuracy, aes(x = Guess, y = Error)) +
  labs(x = "Prognoza", y = "błąd") +
  geom_point() +
  geom_line() +
  geom_vline(xintercept = mean(ages$AgeAtDeath), linetype = 2)

#sprawdzenie czy uda się zmniejszyć błąd średniokwadratowy podając inną predykcję dla palaczy, a inną dla niepalących
constant.guess <- with(ages, mean(AgeAtDeath))
single.prediciton <- with(ages, sqrt(mean((AgeAtDeath - constant.guess) ^2)))
smokers.guess <- with(subset(ages, Smokes == 1), mean(AgeAtDeath))
non.smokers.guess <- with(subset(ages, Smokes == 0), mean(AgeAtDeath))

ages <- transform(ages, NewPrediction = ifelse(Smokes == 0, non.smokers.guess, smokers.guess))
divided.prediction <- with(ages, sqrt(mean((AgeAtDeath - NewPrediction) ^ 2)))
#o tyle jest mniejszy błąd dla oddzielnie podawanych predykcji
single.prediciton - divided.prediction
