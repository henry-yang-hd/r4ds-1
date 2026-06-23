#
#
#
#
#
#
#
#
#
#
#| message: false
library(tidyverse)
library(maps)
#
#
#
billboard |> select(artist, track, date.entered, wk1:wk4)
#
#
#
billboard |> summarize(
  earliest = min(date.entered),
  latest   = max(date.entered),
  range    = latest - earliest
)
#
#
#
n <- sum(!is.na(billboard$wk6))

billboard |>
  ggplot(aes(x = wk6)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title = "Distribution of Week 6 Billboard Rankings",
    subtitle = paste("n =", n, "songs"),
    x = "Week 6 Rank",
    y = "Count"
  )
#
#
#
billboard |>
  select(wk1, wk4, wk10, wk20, wk40, wk76) |>
  pivot_longer(
    everything(),
    names_to  = "week",
    values_to = "rank"
  ) |>
  mutate(week = factor(week, levels = c("wk1", "wk4", "wk10", "wk20", "wk40", "wk76"))) |>
  group_by(week) |>
  summarize(
    present = sum(!is.na(rank)),
    missing = sum(is.na(rank))
  )
#
#
#
billboard |>
  pivot_longer(
    starts_with("wk"),
    names_to      = "week",
    names_prefix  = "wk",
    names_transform = list(week = as.integer),
    values_to     = "rank"
  ) |>
  arrange(artist, track, week) |>
  group_by(artist, track) |>
  summarize(
    re_entered = any(is.na(rank) & !is.na(lead(rank))),
    .groups = "drop"
  ) |>
  count(re_entered)
#
#
#
wk1_wk6 <- billboard |>
  select(artist, track, wk1, wk6) |>
  mutate(
    change = wk6 - wk1,
    status = case_when(
      is.na(wk6)  ~ "missing by wk6",
      change < 0  ~ "improved",
      change == 0 ~ "same",
      change > 0  ~ "worse"
    )
  )

wk1_wk6 |> count(status)

wk1_wk6 |>
  filter(!is.na(change)) |>
  summarize(median_change = median(change))
#
#
#
#| cache: true
billboard |>
  pivot_longer(
    starts_with("wk"),
    names_to      = "week",
    names_prefix  = "wk",
    names_transform = list(week = as.integer),
    values_to     = "rank",
    values_drop_na = TRUE
  ) |>
  ggplot(aes(x = week, y = rank, group = paste(artist, track))) +
  geom_line(alpha = 0.2, linewidth = 0.3, color = "steelblue") +
  scale_y_reverse(breaks = c(1, 25, 50, 75, 100)) +
  labs(
    title = "Billboard Chart Rankings Over Time",
    subtitle = "Each line is one song",
    x = "Week on chart",
    y = "Rank (1 = best)"
  )
#
#
#
song_summary <- billboard |>
  pivot_longer(
    starts_with("wk"),
    names_to        = "week",
    names_prefix    = "wk",
    names_transform = list(week = as.integer),
    values_to       = "rank",
    values_drop_na  = TRUE
  ) |>
  group_by(artist, track) |>
  summarize(
    first_rank      = rank[week == min(week)],
    best_rank       = min(rank),
    best_rank_week  = min(week[rank == min(rank)]),
    total_weeks     = n(),
    .groups = "drop"
  )

song_summary

# Fastest song to reach #1
song_summary |>
  filter(best_rank == 1) |>
  slice_min(best_rank_week, n = 1) |>
  select(artist, track, best_rank_week, total_weeks)

# Slowest song to reach #1
song_summary |>
  filter(best_rank == 1) |>
  slice_max(best_rank_week, n = 1) |>
  select(artist, track, best_rank_week, total_weeks)

# Top-10 song with the longest chart run
song_summary |>
  filter(best_rank <= 10) |>
  slice_max(total_weeks, n = 1) |>
  select(artist, track, best_rank, best_rank_week, total_weeks)
#
#
#
#| cache: true
# Identify the three notable songs with labels
notable <- bind_rows(
  song_summary |> filter(best_rank == 1) |> slice_min(best_rank_week, n = 1) |> mutate(label = "Fastest to #1"),
  song_summary |> filter(best_rank == 1) |> slice_max(best_rank_week, n = 1) |> mutate(label = "Slowest to #1"),
  song_summary |> filter(best_rank <= 10) |> slice_max(total_weeks,    n = 1) |> mutate(label = "Longest top-10 run")
) |>
  select(artist, track, label)

# Long-format data for all top-10 songs
top10_long <- billboard |>
  pivot_longer(
    starts_with("wk"),
    names_to        = "week",
    names_prefix    = "wk",
    names_transform = list(week = as.integer),
    values_to       = "rank",
    values_drop_na  = TRUE
  ) |>
  semi_join(song_summary |> filter(best_rank <= 10), by = c("artist", "track"))

notable_long <- top10_long |>
  inner_join(notable, by = c("artist", "track"))

ggplot() +
  geom_line(
    data = top10_long,
    aes(x = week, y = rank, group = paste(artist, track)),
    color = "gray75", linewidth = 0.4, alpha = 0.6
  ) +
  geom_line(
    data = notable_long,
    aes(x = week, y = rank, color = label, group = paste(artist, track)),
    linewidth = 1.3
  ) +
  scale_y_reverse(breaks = c(1, 5, 10)) +
  scale_color_brewer(palette = "Set1", name = NULL) +
  labs(
    title    = "Top-10 Songs: Chart Trajectories",
    subtitle = "Three notable songs highlighted",
    x        = "Week on chart",
    y        = "Rank (1 = best)"
  ) +
  theme(legend.position = "bottom")
#
#
#
#| message: false
music <- read_csv("data/music.csv")

music_year <- music |> filter(song.year != 0)
n <- nrow(music_year)

music_year |>
  ggplot(aes(x = song.year)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "white") +
  labs(
    title    = "Distribution of Song Release Year",
    subtitle = paste("n =", n, "songs with known year"),
    x        = "Year",
    y        = "Count"
  )
#
#
#
#
#
#
music |>
  summarize(
    artist.location    = sum(artist.location == "0"),
    release.name       = sum(release.name == "0"),
    song.title         = sum(song.title == "0"),
    song.year          = sum(song.year == 0),
    artist.familiarity = sum(artist.familiarity == 0),
    artist.hotttnesss  = sum(artist.hotttnesss == 0)
  ) |>
  pivot_longer(everything(), names_to = "column", values_to = "placeholder_count")
#
#
#
coord_counts <- music |>
  distinct(artist.id, artist.name, artist.latitude, artist.longitude) |>
  mutate(placeholder = artist.latitude == 0 & artist.longitude == 0) |>
  summarize(
    total_unique_artists = n(),
    usable = sum(!placeholder),
    placeholder = sum(placeholder)
  )

coord_counts
#
#
#
artist_locations <- music |>
  distinct(artist.id, artist.name, artist.latitude, artist.longitude) |>
  filter(!(artist.latitude == 0 & artist.longitude == 0))

world_map <- map_data("world")

ggplot() +
  geom_polygon(
    data = world_map,
    aes(x = long, y = lat, group = group),
    fill = "gray95",
    color = "gray70",
    size = 0.2
  ) +
  geom_point(
    data = artist_locations,
    aes(x = artist.longitude, y = artist.latitude),
    color = "steelblue",
    alpha = 0.6,
    size = 1.5
  ) +
  coord_quickmap() +
  labs(
    title = "Global Locations of Artists with Usable Coordinates",
    subtitle = "Only artists with non-zero latitude and longitude are shown; many artists have placeholder 0,0 coordinates.",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal()
#
#
#
#
#
#
