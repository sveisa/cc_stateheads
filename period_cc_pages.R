# ── Climate Change paragraphs across speeches: change over time ──────────────
# Visualises (a) frequency and (b) placement of CC paragraphs per period.
#
# Design notes
# ────────────
# • The "mean position" of CC paragraphs is misleading when CC tends to land
#   at the start AND/OR end of a speech — a mean would push it to the middle.
#   So we never collapse position to a single number. Instead we:
#     1. Compute each paragraph's relative position by *cumulative word count*
#        within its speech (start word .. end word)/(total words).
#     2. Allocate each paragraph's words across N=40 bins along [0,1].
#     3. Aggregate per period: share of words in each bin that are CC.
#   The result reads as a "page" (40 stacked lines) where each line shows
#   how much of the speech-at-that-position is CC.
#
# • A small companion ridge plot shows the raw density of CC paragraph
#   positions per period — bimodality (front + end) becomes visible directly.
#
# Run from a working dir that can write PNGs.
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(ggridges)
library(patchwork)
library(scales)

# ── 1. Settings (edit me) ────────────────────────────────────────────────────

CSV_URL    <- "https://raw.githubusercontent.com/sveisa/cc_stateheads/refs/heads/main/overview_cleaned2.csv"

# Period cutoffs: edit to taste. Each entry = lower bound of a period.
# Default: 3 ~equal slices of 1997-2024.
period_breaks <- c(1997, 2007, 2016)
period_max    <- 2024     # upper bound of last period (inclusive)

N_BINS  <- 40             # vertical "lines" per page
CC_COL  <- "#175E54"      # forest green (matches your React palette)
BG_COL  <- "#e5e7eb"      # light gray for non-CC line backing
PAGE_BG <- "#fdfdfc"      # paper colour
PAPER_BORDER <- "#cfcfcf"

# ── 2. Load & prep ───────────────────────────────────────────────────────────

df_raw <- read_csv(CSV_URL, show_col_types = FALSE)

df <- df_raw %>%
  mutate(
    graf_num = as.integer(str_extract(graf_id, "(?<=graf)\\d+")),
    about_cc = as.integer(about_cc),
    wc       = as.integer(wc),
    year     = as.integer(year)
  ) %>%
  arrange(speech_id, graf_num) %>%
  group_by(speech_id) %>%
  mutate(
    total_wc     = sum(wc),
    cum_wc_end   = cumsum(wc),
    cum_wc_start = lag(cum_wc_end, default = 0),
    pos_start    = cum_wc_start / total_wc,
    pos_end      = cum_wc_end   / total_wc,
    pos_mid      = (pos_start + pos_end) / 2,
    n_grafs      = n()
  ) %>%
  ungroup()

# Assign period
period_labels <- paste0(
  period_breaks, "–",
  c(period_breaks[-1] - 1, period_max)
)
df <- df %>%
  mutate(
    period_idx = findInterval(year, period_breaks),
    period     = factor(period_labels[period_idx], levels = period_labels)
  ) %>%
  filter(!is.na(period), year <= period_max)

# ── 3. Frequency per period ──────────────────────────────────────────────────

freq <- df %>%
  group_by(period) %>%
  summarise(
    n_speeches         = n_distinct(speech_id),
    n_speeches_with_cc = n_distinct(speech_id[about_cc == 1]),
    n_grafs            = n(),
    n_cc_grafs         = sum(about_cc),
    pct_cc_grafs       = n_cc_grafs / n_grafs * 100,
    pct_cc_words       = sum(wc[about_cc == 1]) / sum(wc) * 100,
    pct_speeches_cc    = n_speeches_with_cc / n_speeches * 100,
    .groups = "drop"
  )
print(freq)

# ── 4. Page composite: words-in-bin × CC share, per period ───────────────────
# For each paragraph, distribute its wc across the bins it overlaps.

bin_edges <- seq(0, 1, length.out = N_BINS + 1)

graf_bins <- df %>%
  select(period, speech_id, graf_id, pos_start, pos_end, wc, about_cc) %>%
  rowwise() %>%
  do({
    g <- .
    span <- pmax(g$pos_end - g$pos_start, 1e-9)
    overlap <- pmax(0, pmin(g$pos_end, bin_edges[-1]) -
                       pmax(g$pos_start, bin_edges[-length(bin_edges)]))
    tibble(
      period   = g$period,
      bin      = seq_len(N_BINS),
      words    = overlap / span * g$wc,
      cc_words = overlap / span * g$wc * g$about_cc
    )
  }) %>%
  ungroup()

period_bins <- graf_bins %>%
  group_by(period, bin) %>%
  summarise(
    words    = sum(words),
    cc_words = sum(cc_words),
    .groups  = "drop"
  ) %>%
  mutate(
    pct_cc = ifelse(words > 0, cc_words / words, 0),
    bin_top    = bin - 1,            # 0..N_BINS-1, top = start of speech
    bin_bottom = bin
  )

# ── 5. Plot 1: page-style composite ──────────────────────────────────────────

# X coords: lines drawn on a [0,1] line span; CC segment length = pct_cc
LINE_PAD <- 0.05      # left/right margin inside the page
LINE_X0  <- LINE_PAD
LINE_X1  <- 1 - LINE_PAD
LINE_LEN <- LINE_X1 - LINE_X0

period_bins <- period_bins %>%
  mutate(
    cc_x1 = LINE_X0 + LINE_LEN * pct_cc
  )

# Page outlines
pages <- freq %>%
  mutate(
    label = sprintf(
      "%s\n%d speeches · %d¶ · %.1f%% CC",
      period, n_speeches, n_grafs, pct_cc_grafs
    )
  )

p_pages <- ggplot() +
  # Paper rectangle
  geom_rect(
    data = pages,
    aes(xmin = 0, xmax = 1, ymin = 0, ymax = N_BINS),
    fill = PAGE_BG, color = PAPER_BORDER, linewidth = 0.3
  ) +
  # Background gray line for every bin (full width)
  geom_segment(
    data = period_bins,
    aes(x = LINE_X0, xend = LINE_X1,
        y = bin - 0.5, yend = bin - 0.5),
    color = BG_COL, linewidth = 1.1, lineend = "butt"
  ) +
  # Green segment overlay = CC share
  geom_segment(
    data = period_bins %>% filter(pct_cc > 0),
    aes(x = LINE_X0, xend = cc_x1,
        y = bin - 0.5, yend = bin - 0.5),
    color = CC_COL, linewidth = 1.1, lineend = "butt"
  ) +
  scale_y_reverse(expand = expansion(add = c(0.5, 0.5))) +   # top = start
  scale_x_continuous(expand = expansion(add = 0)) +
  facet_wrap(~ period, nrow = 1,
             labeller = as_labeller(setNames(pages$label, pages$period))) +
  coord_fixed(ratio = 1 / N_BINS * 1.4) +
  labs(
    title    = "Climate change in speeches: how often, and where in the speech",
    subtitle = "Each 'page' = one period. Top of page = start of a speech, bottom = end.\nLine length at each row = share of words at that relative position that are about climate change.",
    caption  = sprintf("N = %d paragraphs, %d speeches, %d–%d", nrow(df),
                       n_distinct(df$speech_id), min(df$year), max(df$year))
  ) +
  theme_void(base_family = "Georgia") +
  theme(
    plot.title       = element_text(size = 13, color = "gray10",
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9.5, color = "gray45",
                                    margin = margin(b = 14), lineheight = 1.2),
    plot.caption     = element_text(size = 8, color = "gray60", hjust = 0,
                                    margin = margin(t = 10)),
    strip.text       = element_text(size = 10, color = "gray15", lineheight = 1.2,
                                    margin = margin(b = 8)),
    panel.spacing.x  = unit(1.2, "lines"),
    plot.margin      = margin(18, 18, 14, 18),
    plot.background  = element_rect(fill = PAGE_BG, color = NA)
  )

# ── 6. Plot 2: position density of CC paragraphs (sanity / bimodality) ───────

cc_grafs <- df %>%
  filter(about_cc == 1) %>%
  select(period, speech_id, graf_id, pos_mid, pos_start, pos_end, wc)

# Sample points within each CC paragraph's [pos_start, pos_end], one per ~10 words,
# so longer CC paragraphs contribute proportional density.
set.seed(1)
cc_sampled <- cc_grafs %>%
  rowwise() %>%
  mutate(samples = list(runif(max(1, round(wc / 10)), pos_start, pos_end))) %>%
  unnest(samples) %>%
  ungroup()

p_density <- ggplot(cc_sampled, aes(x = samples, y = period, fill = period)) +
  geom_density_ridges(
    bandwidth = 0.04, scale = 0.95, color = "white",
    rel_min_height = 0.005, alpha = 0.85
  ) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("start", "25%", "middle", "75%", "end"),
    expand = expansion(add = 0.01)
  ) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = rep(CC_COL, length(period_labels))) +
  labs(
    title    = "Where in a speech does climate change appear?",
    subtitle = "Density of CC paragraphs by relative position (weighted by word count).\nA flat-with-bumps shape at start AND end signals bookend placement; a single hump in the middle would signal centre placement.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Georgia", base_size = 11) +
  theme(
    legend.position  = "none",
    plot.title       = element_text(size = 13, color = "gray10",
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9.5, color = "gray45",
                                    margin = margin(b = 14), lineheight = 1.2),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    axis.text.y      = element_text(color = "gray20"),
    axis.text.x      = element_text(color = "gray45"),
    plot.background  = element_rect(fill = PAGE_BG, color = NA),
    plot.margin      = margin(18, 18, 14, 18)
  )

# ── 7. Single-period pages (one hi-res PNG per period, for slides) ───────────

make_single_page <- function(per) {
  pb   <- period_bins %>% filter(period == per)
  meta <- pages %>% filter(period == per)

  ggplot() +
    annotate("rect",
             xmin = 0, xmax = 1, ymin = 0, ymax = N_BINS,
             fill = PAGE_BG, color = PAPER_BORDER, linewidth = 0.4) +
    geom_segment(
      data = pb,
      aes(x = LINE_X0, xend = LINE_X1,
          y = bin - 0.5, yend = bin - 0.5),
      color = BG_COL, linewidth = 1.6, lineend = "butt"
    ) +
    geom_segment(
      data = pb %>% filter(pct_cc > 0),
      aes(x = LINE_X0, xend = cc_x1,
          y = bin - 0.5, yend = bin - 0.5),
      color = CC_COL, linewidth = 1.6, lineend = "butt"
    ) +
    scale_y_reverse(expand = expansion(add = c(0.5, 0.5))) +
    scale_x_continuous(expand = expansion(add = 0)) +
    coord_fixed(ratio = 1 / N_BINS * 1.4) +
    labs(
      title    = as.character(per),
      subtitle = sprintf("%d speeches · %d paragraphs · %.1f%% about climate change",
                         meta$n_speeches, meta$n_grafs, meta$pct_cc_grafs),
      caption  = "Top of page = start of speech.  Line length = share of words at that position about climate change."
    ) +
    theme_void(base_family = "Georgia") +
    theme(
      plot.title      = element_text(size = 22, color = "gray10",
                                     margin = margin(b = 4), face = "bold"),
      plot.subtitle   = element_text(size = 12, color = "gray45",
                                     margin = margin(b = 18)),
      plot.caption    = element_text(size = 9, color = "gray60", hjust = 0,
                                     margin = margin(t = 14)),
      plot.margin     = margin(28, 28, 22, 28),
      plot.background = element_rect(fill = PAGE_BG, color = NA)
    )
}

for (per in period_labels) {
  fname <- paste0("cc_page_", gsub("[–-]", "_", per), ".png")
  ggsave(fname, make_single_page(per),
         width = 6, height = 8, dpi = 300, bg = PAGE_BG)
  message("Saved: ", fname)
}

# ── 8. Save combined views ───────────────────────────────────────────────────

ggsave("cc_pages_by_period.png",   p_pages,   width = 9, height = 7,   dpi = 300, bg = PAGE_BG)
ggsave("cc_density_by_period.png", p_density, width = 9, height = 4.5, dpi = 300, bg = PAGE_BG)

# Combined panel
combined <- p_pages / p_density + plot_layout(heights = c(2.2, 1))
ggsave("cc_combined.png", combined, width = 9, height = 11, dpi = 300, bg = PAGE_BG)

message("\nSaved: cc_pages_by_period.png, cc_density_by_period.png, cc_combined.png")
