# ── Speech Topic Dispersion — Plot Variants ───────────────────────────────────
# Generates 4 PNG files so you can compare styles before choosing one.
#   plot_v1_ticks_single.png   — one thin line per speech, all topics overlaid
#   plot_v2_ticks_lanes.png    — 4 stacked lanes per speech (current approach)
#   plot_v3_small_multiples.png — faceted 2×2 grid, one panel per topic
#   plot_v4_density.png        — smoothed density curves per topic per speech
# ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidytext)
library(ggthemes)
library(scales)

# ── 1. Load & prepare data  (identical for all variants) ──────────────────────

speeches_url <- "https://docs.google.com/spreadsheets/d/1-ENbflM5JcHPZ0z7xRokMcxPRLHt39_OWJbanrqkYJk/export?format=csv"
topics_url   <- "https://docs.google.com/spreadsheets/d/1rB8-LaTg4bEdztpCaO6BQk64ikVCBUVnbL800QfMjAc/export?format=csv"

speeches_raw <- read_csv(speeches_url, show_col_types = FALSE) %>%
  filter(speech_type != "missing", tolower(eng) != "youtube") %>%
  mutate(year = as.integer(year))

topics_raw <- read_csv(topics_url, show_col_types = FALSE)

speeches <- speeches_raw %>%
  arrange(year) %>%
  mutate(speech_label = paste0(year, "  ", str_to_title(speech_type)))

speech_levels <- speeches$speech_label

topic_dict <- topics_raw %>%
  pivot_longer(everything(), names_to = "topic", values_to = "keyword") %>%
  filter(!is.na(keyword)) %>%
  mutate(keyword = str_to_lower(str_trim(as.character(keyword))),
         topic   = str_trim(topic))

topic_levels <- sort(unique(topic_dict$topic))
n_topics     <- length(topic_levels)
n_speeches   <- length(speech_levels)

speech_words <- speeches %>%
  select(speech_label, text = eng) %>%
  unnest_tokens(word, text, to_lower = TRUE) %>%
  group_by(speech_label) %>%
  mutate(word_index  = row_number(),
         total_words = n(),
         position    = word_index / total_words) %>%
  ungroup()

topic_hits <- speech_words %>%
  inner_join(topic_dict, by = c("word" = "keyword")) %>%
  mutate(speech_label = factor(speech_label, levels = speech_levels),
         topic        = factor(topic,        levels = topic_levels))

word_counts <- speech_words %>%
  distinct(speech_label, total_words) %>%
  mutate(speech_num = match(speech_label, speech_levels))

# Shared palette
palette_base <- c("#4E79A7", "#E15759", "#F28E2B", "#59A14F")
topic_colors <- setNames(palette_base[seq_len(n_topics)], topic_levels)

# Shared theme tweaks on top of theme_tufte
tufte_base <- list(
  theme_tufte(base_family = "Georgia", base_size = 11),
  theme(
    axis.title      = element_blank(),
    axis.ticks.y    = element_blank(),
    axis.ticks.x    = element_line(color = "gray65", linewidth = 0.25),
    axis.text.y     = element_text(hjust = 1, color = "gray20", size = 8.5),
    axis.text.x     = element_text(color = "gray45", size = 7.5),
    legend.position = "top",
    legend.justification = c(0, 1),
    legend.margin   = margin(0, 0, 6, 0),
    legend.text     = element_text(size = 8.5, color = "gray25"),
    plot.title      = element_text(hjust = 0, size = 12, color = "gray10",
                                   margin = margin(b = 3)),
    plot.subtitle   = element_text(hjust = 0, size = 8, color = "gray55",
                                   margin = margin(b = 10)),
    plot.caption    = element_text(size = 7, color = "gray65", hjust = 0),
    plot.margin     = margin(18, 70, 14, 14)
  )
)

wc_annotation <- geom_text(
  data   = word_counts,
  aes(x  = 1.02, y = speech_num,
      label = paste0(formatC(total_words, format = "d", big.mark = ","), " w")),
  hjust  = 0, size = 2.6, color = "gray60", family = "Georgia"
)

x_scale <- scale_x_continuous(
  breaks = c(0, .25, .5, .75, 1),
  labels = c("0 %", "25 %", "50 %", "75 %", "100 %"),
  expand = expansion(add = 0.01)
)

legend_override <- guide_legend(
  override.aes = list(linewidth = 2.5, alpha = 1),
  direction = "horizontal", nrow = 1, keywidth = unit(1.8, "lines")
)


# ══════════════════════════════════════════════════════════════════════════════
# VARIANT 1 — Single thin line, all topics overlaid
# ══════════════════════════════════════════════════════════════════════════════
# All 4 topics share the exact same horizontal line. Colours distinguish topics.
# Simplest / most minimal. Can be hard to separate topics if many hits overlap.

band_df_v1 <- tibble(speech_label = factor(speech_levels, levels = speech_levels),
                     y_num        = seq_len(n_speeches))

plot_data_v1 <- topic_hits %>%
  mutate(y_num = as.integer(speech_label))

p1 <- ggplot() +
  geom_segment(
    data = band_df_v1,
    aes(x = 0, xend = 1, y = y_num, yend = y_num),
    color = "#C8C0B0", linewidth = 1.0, lineend = "round"
  ) +
  geom_point(
    data  = plot_data_v1,
    aes(x = position, y = y_num, color = topic),
    shape = "|", size = 3.5, alpha = 0.55, stroke = 0.4
  ) +
  wc_annotation +
  x_scale +
  scale_y_continuous(breaks = seq_len(n_speeches), labels = speech_levels,
                     expand = expansion(add = 0.55)) +
  scale_color_manual(values = topic_colors, name = NULL, guide = legend_override) +
  tufte_base +
  labs(title    = "V1 — Single line per speech",
       subtitle = "All topics overlaid on one line per speech\nSimplest; colours distinguish topics but overlaps can hide detail",
       caption  = "Each tick = one keyword match")

ggsave("plot_v1_ticks_single.png", p1, width = 9, height = 5.5, dpi = 220, bg = "white")
message("Saved plot_v1_ticks_single.png")


# ══════════════════════════════════════════════════════════════════════════════
# VARIANT 2 — Stacked lanes within each band
# ══════════════════════════════════════════════════════════════════════════════
# Each speech strip is divided into 4 narrow horizontal lanes (one per topic).
# Still reads as one band-aid per speech but topics never overlap.

band_height <- 0.60
lane_height <- band_height / n_topics
tick_half   <- lane_height * 0.38

lane_offsets <- tibble(
  topic    = factor(topic_levels, levels = topic_levels),
  y_offset = seq(from = -band_height/2 + lane_height/2,
                 to   =  band_height/2 - lane_height/2,
                 length.out = n_topics)
)

plot_data_v2 <- topic_hits %>%
  left_join(lane_offsets, by = "topic") %>%
  mutate(speech_num = as.integer(speech_label),
         y_center   = speech_num + y_offset,
         y_lo       = y_center - tick_half,
         y_hi       = y_center + tick_half)

band_df_v2 <- tibble(
  speech_label = factor(speech_levels, levels = speech_levels),
  y_num = seq_len(n_speeches),
  y_lo  = seq_len(n_speeches) - band_height / 2,
  y_hi  = seq_len(n_speeches) + band_height / 2
)

p2 <- ggplot() +
  geom_rect(data = band_df_v2,
            aes(xmin = 0, xmax = 1, ymin = y_lo, ymax = y_hi),
            fill = "#F2EDE4", color = NA) +
  geom_segment(
    data = band_df_v2 %>% crossing(lane = seq_len(n_topics - 1)) %>%
      mutate(y_line = y_lo + lane * lane_height),
    aes(x = 0, xend = 1, y = y_line, yend = y_line),
    color = "white", linewidth = 0.3
  ) +
  geom_segment(
    data = plot_data_v2,
    aes(x = position, xend = position, y = y_lo, yend = y_hi, color = topic),
    alpha = 0.65, linewidth = 0.38
  ) +
  wc_annotation +
  x_scale +
  scale_y_continuous(breaks = seq_len(n_speeches), labels = speech_levels,
                     expand = expansion(add = 0.55)) +
  scale_color_manual(values = topic_colors, name = NULL, guide = legend_override) +
  tufte_base +
  labs(title    = "V2 — Stacked lanes within each band",
       subtitle = "4 narrow lanes per speech (one per topic) — no overlap, still reads as one strip",
       caption  = "Each tick = one keyword match")

ggsave("plot_v2_ticks_lanes.png", p2, width = 9, height = 5.5, dpi = 220, bg = "white")
message("Saved plot_v2_ticks_lanes.png")


# ══════════════════════════════════════════════════════════════════════════════
# VARIANT 3 — Small multiples: 2×2 grid, one panel per topic
# ══════════════════════════════════════════════════════════════════════════════
# Each panel shows all 8 speeches for ONE topic. Best for asking:
# "Where does this specific topic cluster across speeches?"

plot_data_v3 <- topic_hits %>%
  mutate(speech_num = as.integer(speech_label))

band_df_v3 <- tibble(
  speech_label = factor(speech_levels, levels = speech_levels),
  y_num        = seq_len(n_speeches)
)

p3 <- ggplot() +
  geom_segment(
    data = band_df_v3 %>% crossing(topic = factor(topic_levels, levels = topic_levels)),
    aes(x = 0, xend = 1, y = y_num, yend = y_num),
    color = "#C8C0B0", linewidth = 0.8, lineend = "round"
  ) +
  geom_point(
    data  = plot_data_v3,
    aes(x = position, y = speech_num, color = topic),
    shape = "|", size = 3, alpha = 0.60, stroke = 0.4
  ) +
  facet_wrap(~ topic, nrow = 2, ncol = 2) +
  x_scale +
  scale_y_continuous(breaks = seq_len(n_speeches), labels = speech_levels,
                     expand = expansion(add = 0.55)) +
  scale_color_manual(values = topic_colors, name = NULL, guide = "none") +
  tufte_base +
  theme(
    strip.text   = element_text(size = 9, color = "gray20", face = "plain",
                                margin = margin(b = 4)),
    panel.spacing = unit(1.5, "lines"),
    plot.margin  = margin(18, 20, 14, 14)
  ) +
  labs(title    = "V3 — Small multiples (one panel per topic)",
       subtitle = "Each panel shows all 8 speeches for a single topic\nBest for: 'Where does this topic cluster?'",
       caption  = "Each tick = one keyword match")

ggsave("plot_v3_small_multiples.png", p3, width = 10, height = 7, dpi = 220, bg = "white")
message("Saved plot_v3_small_multiples.png")


# ══════════════════════════════════════════════════════════════════════════════
# VARIANT 4 — Smoothed density curves
# ══════════════════════════════════════════════════════════════════════════════
# Instead of individual ticks, shows a kernel density estimate per topic.
# Makes it immediately obvious whether topics cluster at start/middle/end.
# Individual word positions are hidden — you see shape, not exact locations.

# Compute density curves manually so we control bandwidth and y-scaling
density_list <- topic_hits %>%
  group_by(speech_label, topic) %>%
  summarise(pos = list(position), .groups = "drop") %>%
  mutate(
    speech_num = match(as.character(speech_label), speech_levels),
    dens_data  = map(pos, ~ {
      if (length(.x) < 2) return(NULL)
      d <- density(.x, bw = 0.08, from = 0, to = 1, n = 256)
      tibble(x = d$x, y = d$y)
    })
  ) %>%
  filter(!map_lgl(dens_data, is.null)) %>%
  unnest(dens_data) %>%
  group_by(speech_label, topic) %>%
  mutate(y_norm = y / max(y)) %>%     # normalise to 0–1 per speech+topic
  ungroup()

band_height_v4 <- 0.55
lane_height_v4 <- band_height_v4 / n_topics
curve_half     <- lane_height_v4 * 0.44

lane_offsets_v4 <- tibble(
  topic    = factor(topic_levels, levels = topic_levels),
  y_offset = seq(from = -band_height_v4/2 + lane_height_v4/2,
                 to   =  band_height_v4/2 - lane_height_v4/2,
                 length.out = n_topics)
)

plot_data_v4 <- density_list %>%
  mutate(topic = factor(topic, levels = topic_levels)) %>%
  left_join(lane_offsets_v4, by = "topic") %>%
  mutate(speech_num = match(as.character(speech_label), speech_levels),
         y_base     = speech_num + y_offset,
         y_curve    = y_base + y_norm * curve_half)

band_df_v4 <- tibble(
  speech_label = factor(speech_levels, levels = speech_levels),
  y_num = seq_len(n_speeches),
  y_lo  = seq_len(n_speeches) - band_height_v4 / 2,
  y_hi  = seq_len(n_speeches) + band_height_v4 / 2
)

p4 <- ggplot() +
  geom_rect(data = band_df_v4,
            aes(xmin = 0, xmax = 1, ymin = y_lo, ymax = y_hi),
            fill = "#F2EDE4", color = NA) +
  geom_segment(
    data = band_df_v4 %>% crossing(lane = seq_len(n_topics - 1)) %>%
      mutate(y_line = y_lo + lane * lane_height_v4),
    aes(x = 0, xend = 1, y = y_line, yend = y_line),
    color = "white", linewidth = 0.3
  ) +
  # Filled area under density curve
  geom_ribbon(
    data = plot_data_v4,
    aes(x = x, ymin = y_base, ymax = y_curve, fill = topic, group = interaction(speech_label, topic)),
    alpha = 0.45
  ) +
  # Density curve line on top
  geom_line(
    data = plot_data_v4,
    aes(x = x, y = y_curve, color = topic, group = interaction(speech_label, topic)),
    linewidth = 0.45, alpha = 0.85
  ) +
  x_scale +
  scale_y_continuous(breaks = seq_len(n_speeches), labels = speech_levels,
                     expand = expansion(add = 0.55)) +
  scale_color_manual(values = topic_colors, name = NULL, guide = legend_override) +
  scale_fill_manual( values = topic_colors, name = NULL, guide = "none") +
  wc_annotation +
  tufte_base +
  labs(title    = "V4 — Smoothed density curves",
       subtitle = "Kernel density per topic; height = concentration of keyword hits\nBest for: seeing whether topics cluster at start, middle, or end",
       caption  = "Bandwidth = 8 % of speech length  ·  Curves normalised per speech–topic pair")

ggsave("plot_v4_density.png", p4, width = 9, height = 5.5, dpi = 220, bg = "white")
message("Saved plot_v4_density.png")

message("\nDone. Compare: plot_v1_ticks_single.png  |  plot_v2_ticks_lanes.png  |  plot_v3_small_multiples.png  |  plot_v4_density.png")
