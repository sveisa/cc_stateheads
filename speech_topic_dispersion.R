# ── Speech Topic Dispersion Plot ──────────────────────────────────────────────
# Visualises WHERE in each speech topic keywords appear.
# Design: Tufte-inspired lexical dispersion plot.
#   · One horizontal "band-aid" strip per speech (all same width = 0–100 %)
#   · Coloured vertical tick marks = topic keyword hits
#   · 4 topic lanes stacked within each speech band (no overlap)
# ──────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(tidytext)
library(ggthemes)
library(scales)

# ── 1. Load data ───────────────────────────────────────────────────────────────

speeches_url <- "https://docs.google.com/spreadsheets/d/1-ENbflM5JcHPZ0z7xRokMcxPRLHt39_OWJbanrqkYJk/export?format=csv"
topics_url   <- "https://docs.google.com/spreadsheets/d/1rB8-LaTg4bEdztpCaO6BQk64ikVCBUVnbL800QfMjAc/export?format=csv"

speeches_raw <- read_csv(speeches_url, show_col_types = FALSE) %>%
  filter(speech_type != "missing", tolower(eng) != "youtube") %>%
  mutate(year = as.integer(year))

topics_raw <- read_csv(topics_url, show_col_types = FALSE)

# Inspect if needed:
# glimpse(speeches_raw)
# glimpse(topics_raw)

# ── 2. Speech labels ───────────────────────────────────────────────────────────
# Build a readable label for each speech; adjust column names as needed.
speeches <- speeches_raw %>%
  arrange(year) %>%
  mutate(
    speech_label = make.unique(
      paste0(year, "  ", str_to_title(speech_type)),
      sep = " · "
    )
  )

speech_levels <- speeches$speech_label   # ordered oldest → newest

# ── 3. Topic dictionary ────────────────────────────────────────────────────────
# Expects 4 columns; each column header = topic name, rows = keywords.
topic_dict <- topics_raw %>%
  pivot_longer(
    cols      = everything(),
    names_to  = "topic",
    values_to = "keyword"
  ) %>%
  filter(!is.na(keyword)) %>%
  mutate(
    keyword = str_to_lower(str_trim(as.character(keyword))),
    topic   = str_trim(topic)
  )

topic_levels <- sort(unique(topic_dict$topic))
n_topics     <- length(topic_levels)

# ── 4. Tokenise & track word position ─────────────────────────────────────────
speech_words <- speeches %>%
  select(speech_label, text = eng) %>%
  unnest_tokens(word, text, to_lower = TRUE) %>%
  group_by(speech_label) %>%
  mutate(
    word_index  = row_number(),
    total_words = n(),
    position    = word_index / total_words    # 0–1 relative position
  ) %>%
  ungroup()

# ── 5. Find topic keyword hits ─────────────────────────────────────────────────
topic_hits <- speech_words %>%
  inner_join(topic_dict, by = c("word" = "keyword")) %>%
  mutate(speech_label = factor(speech_label, levels = speech_levels),
         topic        = factor(topic,        levels = topic_levels))

# Quick summary table
hit_summary <- topic_hits %>%
  count(speech_label, topic, name = "n_hits")
message("\n── Topic hits per speech ──")
print(hit_summary %>% pivot_wider(names_from = topic, values_from = n_hits, values_fill = 0))

# ── 6. Compute y-positions ─────────────────────────────────────────────────────
# Each speech is centred at integer y = 1..n_speeches.
# The 4 topic lanes are stacked within a band of height `band_height`,
# so they stay visually fused as one "band-aid" per speech.

n_speeches  <- length(speech_levels)
band_height <- 0.60                            # total height per speech strip
lane_height <- band_height / n_topics          # height of one topic lane
tick_half   <- lane_height * 0.38              # half-height of each tick mark

# Mapping: topic → y-offset within a band (centred at 0)
lane_offsets <- tibble(
  topic    = factor(topic_levels, levels = topic_levels),
  y_offset = seq(
    from = -band_height / 2 + lane_height / 2,
    to   =  band_height / 2 - lane_height / 2,
    length.out = n_topics
  )
)

plot_data <- topic_hits %>%
  left_join(lane_offsets, by = "topic") %>%
  mutate(
    speech_num = as.integer(speech_label),   # 1 = oldest
    y_center   = speech_num + y_offset,
    y_lo       = y_center - tick_half,
    y_hi       = y_center + tick_half
  )

# Background band-aid rectangle data
band_df <- tibble(
  speech_label = factor(speech_levels, levels = speech_levels),
  y_num        = seq_len(n_speeches),
  y_lo         = seq_len(n_speeches) - band_height / 2,
  y_hi         = seq_len(n_speeches) + band_height / 2
)

# Word-count annotation (right margin)
word_counts <- speech_words %>%
  distinct(speech_label, total_words) %>%
  mutate(speech_num = match(speech_label, speech_levels))

# ── 7. Colour palette ──────────────────────────────────────────────────────────
# Muted, print-safe; works in greyscale when lightness varies
palette_base <- c("#4E79A7", "#E15759", "#F28E2B", "#59A14F",
                  "#B07AA1", "#76B7B2", "#EDC948", "#FF9DA7")
topic_colors <- setNames(palette_base[seq_len(n_topics)], topic_levels)

# ── 8. Build plot ──────────────────────────────────────────────────────────────

p <- ggplot() +

  # ── Band-aid background rectangle ──
  geom_rect(
    data = band_df,
    aes(xmin = 0, xmax = 1, ymin = y_lo, ymax = y_hi),
    fill  = "#F2EDE4",
    color = NA
  ) +

  # ── Hairline separating topic lanes (very subtle) ──
  geom_segment(
    data = band_df %>%
      crossing(lane = seq_len(n_topics - 1)) %>%
      mutate(y_line = y_lo + lane * lane_height),
    aes(x = 0, xend = 1, y = y_line, yend = y_line),
    color     = "white",
    linewidth  = 0.25
  ) +

  # ── Topic keyword tick marks ──
  geom_segment(
    data      = plot_data,
    aes(x     = position,
        xend  = position,
        y     = y_lo,
        yend  = y_hi,
        color = topic),
    alpha     = 0.60,
    linewidth  = 0.35
  ) +

  # ── Speech labels (y-axis) ──
  scale_y_continuous(
    breaks = seq_len(n_speeches),
    labels = speech_levels,
    expand = expansion(add = 0.55)
  ) +

  # ── x-axis: percentage position ──
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("0 %", "25 %", "50 %", "75 %", "100 %"),
    expand = expansion(add = 0.01)
  ) +

  # ── Colour legend ──
  scale_color_manual(
    values = topic_colors,
    name   = NULL,
    guide  = guide_legend(
      override.aes = list(linewidth = 2.5, alpha = 1),
      direction    = "horizontal",
      nrow         = 1,
      keywidth     = unit(1.8, "lines")
    )
  ) +

  # ── Word-count annotation on right margin ──
  geom_text(
    data = word_counts,
    aes(x = 1.02, y = speech_num,
        label = paste0(formatC(total_words, format = "d", big.mark = ","), " w")),
    hjust   = 0,
    size    = 2.7,
    color   = "gray60",
    family  = "Georgia"
  ) +

  # ── Tufte theme ────────────────────────────────────────────────────────────
  theme_tufte(base_family = "Georgia", base_size = 11) +
  theme(
    # Axes
    axis.title.x       = element_blank(),
    axis.title.y       = element_blank(),
    axis.ticks.y       = element_blank(),
    axis.ticks.x       = element_line(color = "gray65", linewidth = 0.25),
    axis.text.y        = element_text(hjust = 1,  color = "gray20", size = 9),
    axis.text.x        = element_text(hjust = 0.5, color = "gray45", size = 7.5),

    # Legend
    legend.position    = "top",
    legend.justification = c(0, 1),
    legend.margin      = margin(0, 0, 6, 0),
    legend.text        = element_text(size = 8.5, color = "gray25"),

    # Title / subtitle
    plot.title         = element_text(
      hjust = 0, size = 12, color = "gray10",
      margin = margin(b = 3)
    ),
    plot.subtitle      = element_text(
      hjust = 0, size = 8, color = "gray55",
      margin = margin(b = 10)
    ),
    plot.caption       = element_text(size = 7, color = "gray65", hjust = 0),

    # Margins — extra right space for word-count labels
    plot.margin        = margin(18, 70, 14, 14)
  ) +

  labs(
    title    = "Topic presence across speeches",
    subtitle = paste0(
      "Each strip spans one speech from beginning (left) to end (right).\n",
      "Coloured marks show where topic keywords occur."
    ),
    caption  = "Each tick = one keyword match  ·  Speeches ordered oldest to newest"
  )

print(p)

# ── 9. Save ────────────────────────────────────────────────────────────────────
ggsave("speech_topic_dispersion.png",
       p, width = 9, height = 5.5, dpi = 220, bg = "white")

message("\nSaved: speech_topic_dispersion.png")
