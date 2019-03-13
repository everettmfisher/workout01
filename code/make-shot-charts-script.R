# make shot charts

# plots the location and success of each shot taken for each player

# inputs: individual player tibbles, combined tibble
# outputs: .pdf images for individual players, .pdf and .png images for combined tibble

## @knitr make_shot_charts

library(jpeg)
library(grid)
library(ggplot2)

court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width = unit(1, "npc"),
  height = unit(1, "npc"))

curry_shot_chart <- ggplot(curry) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Stephen Curry (2016 season)") +
  theme_minimal()
ggsave("../images/stephen-curry-shot-chart.pdf", width=6.5, height=5)

durant_shot_chart <- ggplot(durant) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Kevin Durant (2016 season)") +
  theme_minimal()
ggsave("../images/kevin-durant-shot-chart.pdf", width=6.5, height=5)

green_shot_chart <- ggplot(green) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Draymond Green (2016 season)") +
  theme_minimal()
ggsave("../images/draymond-green-shot-chart.pdf", width=6.5, height=5)

iguodala_shot_chart <- ggplot(iguodala) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Andre Iguodala (2016 season)") +
  theme_minimal()
ggsave("../images/andre-iguodala-shot-chart.pdf", width=6.5, height=5)

thompson_shot_chart <- ggplot(thompson) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  ggtitle("Shot Chart: Klay Thompson (2016 season)") +
  theme_minimal()
ggsave("../images/klay-thompson-shot-chart.pdf", width=6.5, height=5)

gsw_shot_chart <- ggplot(shots_data) +
  annotation_custom(court_image, -250, 250, -50, 420) +
  geom_point(aes(x = x, y = y, color = shot_made_flag)) +
  ylim(-50, 420) +
  facet_wrap(~ name)
  ggtitle("shot Charts for the Golden State Warriors (2016 season)") +
  theme_minimal()
ggsave("../images/gsw-shot-charts.pdf", width = 8, height = 7)
ggsave("../images/gsw-shot-charts.png", width = 8, height = 7)