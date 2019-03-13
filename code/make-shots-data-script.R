# make shots data

# adds name column to connect shot data with the player
# clarifies level names for shot_made_flag
# calculates the minutes elapsed for each shot taken
# combines individual player data into one tibble

# inputs: individual .csv files for each player
# outputs: individual tibbles, combined tibble, .txt summaries for each

## @knitr make_shots_data

library(dplyr)
library(readr)

iguodala <- read_csv("../data/andre-iguodala.csv", col_types = "fcfiiiffficii")
green <- read_csv("../data/draymond-green.csv", col_types = "fcfiiiffficii")
durant <- read_csv("../data/kevin-durant.csv", col_types = "fcfiiiffficii")
thompson <- read_csv("../data/klay-thompson.csv", col_types = "fcfiiiffficii")
curry <- read_csv("../data/stephen-curry.csv", col_types = "fcfiiiffficii")

iguodala <- mutate(iguodala, name = "Andre Iguodala")
green <- mutate(green, name = "Draymond Green")
durant <- mutate(durant, name = "Kevin Durant")
thompson <- mutate(thompson, name = "Klay Thompson")
curry <- mutate(curry, name = "Stephen Curry")

levels(iguodala$shot_made_flag) <- c("shot_no", "shot_yes")
levels(green$shot_made_flag) <- c("shot_no", "shot_yes")
levels(durant$shot_made_flag) <- c("shot_no", "shot_yes")
levels(thompson$shot_made_flag) <- c("shot_no", "shot_yes")
levels(curry$shot_made_flag) <- c("shot_no", "shot_yes")

iguodala <- mutate(iguodala, minute = ((period - 1) * 12) + (12 - minutes_remaining))
green <- mutate(green, minute = ((period - 1) * 12) + (12 - minutes_remaining))
durant <- mutate(durant, minute = ((period - 1) * 12) + (12 - minutes_remaining))
thompson <- mutate(thompson, minute = ((period - 1) * 12) + (12 - minutes_remaining))
curry <- mutate(curry, minute = ((period - 1) * 12) + (12 - minutes_remaining))

sink(file = "../output/andre-iguodala-summary.txt", append = FALSE)
summary(iguodala)
sink()

sink(file = "../output/draymond-green-summary.txt", append = FALSE)
summary(green)
sink()

sink(file = "../output/kevin-durant-summary.txt", append = FALSE)
summary(durant)
sink()

sink(file = "../output/klay-thompson-summary.txt", append = FALSE)
summary(thompson)
sink()

sink(file = "../output/stephen-curry-summary.txt", append = FALSE)
summary(curry)
sink()

shots_data <- rbind(iguodala, green, durant, thompson, curry)
write.csv(shots_data, file = "../data/shots-data.csv")

sink(file = "../output/shots-data-summary.txt", append = FALSE)
summary(shots_data)
sink()