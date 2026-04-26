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

N_BINS  <- 10             # vertical "lines" per page
ROW_GAP <- 0.02           # whitespace between lines (0 = none, 0.5 = half row)
CC_COL      <- "#175E54"  # forest green: climate change (drawn from the LEFT)
CRISIS_COL  <- "#8C1515"  # deep red:    local crisis   (drawn from the RIGHT)
BG_COL      <- "#e5e7eb"  # light gray for line backing
PAGE_BG <- "#fdfdfc"      # paper colour
PAPER_BORDER <- "#cfcfcf"

# ── 2. Load & prep ───────────────────────────────────────────────────────────

df_raw <- read_csv(CSV_URL, show_col_types = FALSE)

df <- df_raw %>%
  mutate(
    graf_num   = as.integer(str_extract(graf_id, "(?<=graf)\\d+")),
    about_cc   = as.integer(about_cc),
    wc         = as.integer(wc),
    year       = as.integer(year),
    # Local-crisis flag: paragraphs tagged is_crisis == "yes" that are NOT CC
    # (so the two categories on the page are mutually exclusive).
    is_crisis_bin = as.integer(is_crisis == "yes" & about_cc == 0)
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
    n_crisis_grafs     = sum(is_crisis_bin),
    pct_cc_grafs       = n_cc_grafs     / n_grafs * 100,
    pct_crisis_grafs   = n_crisis_grafs / n_grafs * 100,
    pct_cc_words       = sum(wc[about_cc == 1])      / sum(wc) * 100,
    pct_crisis_words   = sum(wc[is_crisis_bin == 1]) / sum(wc) * 100,
    pct_speeches_cc    = n_speeches_with_cc / n_speeches * 100,
    .groups = "drop"
  )
print(freq)

# ── 4. Page composite: words-in-bin × CC share, per period ───────────────────
# For each paragraph, distribute its wc across the bins it overlaps.

bin_edges <- seq(0, 1, length.out = N_BINS + 1)

graf_bins <- df %>%
  select(period, speech_id, graf_id, pos_start, pos_end, wc, about_cc, is_crisis_bin) %>%
  rowwise() %>%
  do({
    g <- .
    span <- pmax(g$pos_end - g$pos_start, 1e-9)
    overlap <- pmax(0, pmin(g$pos_end, bin_edges[-1]) -
                       pmax(g$pos_start, bin_edges[-length(bin_edges)]))
    tibble(
      period       = g$period,
      bin          = seq_len(N_BINS),
      words        = overlap / span * g$wc,
      cc_words     = overlap / span * g$wc * g$about_cc,
      crisis_words = overlap / span * g$wc * g$is_crisis_bin
    )
  }) %>%
  ungroup()

period_bins <- graf_bins %>%
  group_by(period, bin) %>%
  summarise(
    words        = sum(words),
    cc_words     = sum(cc_words),
    crisis_words = sum(crisis_words),
    .groups  = "drop"
  ) %>%
  mutate(
    pct_cc     = ifelse(words > 0, cc_words     / words, 0),
    pct_crisis = ifelse(words > 0, crisis_words / words, 0),
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
    cc_x1     = LINE_X0 + LINE_LEN * pct_cc,
    crisis_x0 = LINE_X1 - LINE_LEN * pct_crisis
  )

# Long-format bin data so we can use a single geom_rect with a fill legend.
plot_bins <- bind_rows(
  period_bins %>%
    transmute(period, bin,
              category = "Climate change",
              xmin = LINE_X0,
              xmax = cc_x1,
              pct  = pct_cc),
  period_bins %>%
    transmute(period, bin,
              category = "Local crisis",
              xmin = crisis_x0,
              xmax = LINE_X1,
              pct  = pct_crisis)
) %>%
  mutate(category = factor(category,
                           levels = c("Climate change", "Local crisis"))) %>%
  filter(pct > 0)

cat_pal <- c("Climate change" = CC_COL, "Local crisis" = CRISIS_COL)

# Page outlines
pages <- freq %>%
  mutate(
    label = sprintf(
      "%s\n%d speeches · %d paragraphs\n%.1f%% CC  ·  %.1f%% local crisis",
      period, n_speeches, n_grafs, pct_cc_grafs, pct_crisis_grafs
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
  geom_rect(
    data = period_bins,
    aes(xmin = LINE_X0, xmax = LINE_X1,
        ymin = bin - 1 + ROW_GAP, ymax = bin - ROW_GAP),
    fill = BG_COL
  ) +
  # CC (left) + Local crisis (right) overlays, both keyed to the legend
  geom_rect(
    data = plot_bins,
    aes(xmin = xmin, xmax = xmax,
        ymin = bin - 1 + ROW_GAP, ymax = bin - ROW_GAP,
        fill = category)
  ) +
  scale_fill_manual(values = cat_pal, name = NULL) +
  scale_y_reverse(expand = expansion(add = c(0.5, 0.5))) +   # top = start
  scale_x_continuous(expand = expansion(add = 0)) +
  facet_wrap(~ period, nrow = 1,
             labeller = as_labeller(setNames(pages$label, pages$period))) +
  coord_fixed(ratio = 1 / N_BINS * 1.4) +
  guides(fill = guide_legend(override.aes = list(color = NA))) +
  labs(
    title    = "Climate change vs. local crisis in speeches: how often, and where",
    subtitle = "Each 'page' = one period. Top = speech start, bottom = speech end.\nGreen grows from the LEFT = share of words at that position about climate change.\nRed grows from the RIGHT = share about a local crisis (excluding CC paragraphs).",
    caption  = sprintf("N = %d paragraphs, %d speeches, %d–%d", nrow(df),
                       n_distinct(df$speech_id), min(df$year), max(df$year))
  ) +
  theme_void(base_family = "Georgia") +
  theme(
    legend.position    = "top",
    legend.justification = c(0, 1),
    legend.margin      = margin(0, 0, 6, 0),
    legend.text        = element_text(size = 9, color = "gray25"),
    plot.title       = element_text(size = 13, color = "gray10",
                                    margin = margin(b = 4)),
    plot.subtitle    = element_text(size = 9.5, color = "gray45",
                                    margin = margin(b = 14), lineheight = 1.2),
    plot.caption     = element_text(size = 8, color = "gray60", hjust = 0,
                                    margin = margin(t = 10)),
    strip.text       = element_text(size = 9.5, color = "gray15", lineheight = 1.2,
                                    margin = margin(b = 8)),
    panel.spacing.x  = unit(1.2, "lines"),
    plot.margin      = margin(18, 18, 14, 18),
    plot.background  = element_rect(fill = PAGE_BG, color = NA)
  )

# ── 6. Plot 2: position density of CC paragraphs (sanity / bimodality) ───────

cat_grafs <- bind_rows(
  df %>% filter(about_cc == 1) %>%
    mutate(category = "Climate change"),
  df %>% filter(is_crisis_bin == 1) %>%
    mutate(category = "Local crisis")
) %>%
  mutate(category = factor(category, levels = c("Climate change","Local crisis"))) %>%
  select(period, category, speech_id, graf_id, pos_start, pos_end, wc)

# Sample points within each paragraph's [pos_start, pos_end], one per ~10 words,
# so longer paragraphs contribute proportional density.
set.seed(1)
cc_sampled <- cat_grafs %>%
  rowwise() %>%
  mutate(samples = list(runif(max(1, round(wc / 10)), pos_start, pos_end))) %>%
  unnest(samples) %>%
  ungroup()

p_density <- ggplot(cc_sampled, aes(x = samples, y = period, fill = category)) +
  geom_density_ridges(
    bandwidth = 0.04, scale = 0.95, color = "white",
    rel_min_height = 0.005, alpha = 0.85
  ) +
  facet_wrap(~ category, nrow = 1) +
  scale_x_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = c("start", "25%", "middle", "75%", "end"),
    expand = expansion(add = 0.01)
  ) +
  scale_y_discrete(limits = rev) +
  scale_fill_manual(values = cat_pal) +
  labs(
    title    = "Where in a speech do climate change and local-crisis paragraphs appear?",
    subtitle = "Density by relative position, weighted by word count.\nBumps at the start AND end signal bookend placement; a single hump in the middle would signal centre placement.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Georgia", base_size = 11) +
  theme(
    legend.position  = "none",
    strip.text       = element_text(size = 10, color = "gray15",
                                    margin = margin(b = 6)),
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
  pb_long <- plot_bins %>% filter(period == per)
  meta <- pages %>% filter(period == per)

  ggplot() +
    annotate("rect",
             xmin = 0, xmax = 1, ymin = 0, ymax = N_BINS,
             fill = PAGE_BG, color = PAPER_BORDER, linewidth = 0.4) +
    geom_rect(
      data = pb,
      aes(xmin = LINE_X0, xmax = LINE_X1,
          ymin = bin - 1 + ROW_GAP, ymax = bin - ROW_GAP),
      fill = BG_COL
    ) +
    geom_rect(
      data = pb_long,
      aes(xmin = xmin, xmax = xmax,
          ymin = bin - 1 + ROW_GAP, ymax = bin - ROW_GAP,
          fill = category)
    ) +
    scale_fill_manual(values = cat_pal, name = NULL) +
    scale_y_reverse(expand = expansion(add = c(0.5, 0.5))) +
    scale_x_continuous(expand = expansion(add = 0)) +
    coord_fixed(ratio = 1 / N_BINS * 1.4) +
    guides(fill = guide_legend(override.aes = list(color = NA))) +
    labs(
      title    = as.character(per),
      subtitle = sprintf("%d speeches · %d paragraphs · %.1f%% CC · %.1f%% local crisis",
                         meta$n_speeches, meta$n_grafs,
                         meta$pct_cc_grafs, meta$pct_crisis_grafs),
      caption  = "Top = speech start. Green from left = climate change. Red from right = local crisis."
    ) +
    theme_void(base_family = "Georgia") +
    theme(
      legend.position = "top",
      legend.justification = c(0, 1),
      legend.text     = element_text(size = 11, color = "gray25"),
      legend.margin   = margin(0, 0, 8, 0),
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

# ── 8. Crowding-out experiments ──────────────────────────────────────────────
# Three different angles on the same question: does local-crisis content
# compete with climate-change content for space in the speech?

# Period palette: light → dark green so progression reads visually
period_pal <- c("#a3c4b8", "#4f8a76", "#175E54")
names(period_pal) <- period_labels

# (A) Annual time series ------------------------------------------------------
yearly <- df %>%
  group_by(year) %>%
  summarise(
    `Climate change` = sum(wc[about_cc == 1])      / sum(wc) * 100,
    `Local crisis`   = sum(wc[is_crisis_bin == 1]) / sum(wc) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "category", values_to = "pct") %>%
  mutate(category = factor(category, levels = c("Climate change", "Local crisis")))

p_timeseries <- ggplot(yearly, aes(x = year, y = pct, color = category)) +
  geom_line(linewidth = 0.5, alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE, span = 0.55, linewidth = 1.5) +
  scale_color_manual(values = cat_pal, name = NULL) +
  scale_y_continuous(labels = function(z) paste0(z, "%"),
                     expand = expansion(mult = c(0.02, 0.08))) +
  scale_x_continuous(breaks = seq(1998, 2024, 4)) +
  labs(
    title    = "Climate change vs. local crisis over time",
    subtitle = "Yearly share of all speech words. Thin lines = annual, thick = LOESS smooth.\nIf crisis rises while climate change stays flat or falls, that's crowding out.",
    x = NULL, y = "Share of speech words"
  ) +
  theme_minimal(base_family = "Georgia", base_size = 11) +
  theme(
    legend.position    = "top",
    legend.justification = c(0, 1),
    legend.margin      = margin(0, 0, 6, 0),
    plot.title         = element_text(size = 13, color = "gray10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9.5, color = "gray45",
                                      margin = margin(b = 14), lineheight = 1.2),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    plot.background    = element_rect(fill = PAGE_BG, color = NA),
    plot.margin        = margin(18, 22, 14, 18)
  )

ggsave("cc_crowding_timeseries.png", p_timeseries,
       width = 9, height = 5, dpi = 300, bg = PAGE_BG)
message("Saved: cc_crowding_timeseries.png")

# (B) Speech-level scatter ----------------------------------------------------
# One dot per speech: % crisis words on x, % CC words on y.
# A negative slope = within a speech, more crisis ↔ less CC.

speech_lvl <- df %>%
  group_by(speech_id, year, period) %>%
  summarise(
    pct_cc     = sum(wc[about_cc == 1])      / sum(wc) * 100,
    pct_crisis = sum(wc[is_crisis_bin == 1]) / sum(wc) * 100,
    n_words    = sum(wc),
    .groups = "drop"
  )

# Fit overall linear model for annotation
fit <- lm(pct_cc ~ pct_crisis, data = speech_lvl)
slope <- coef(fit)[["pct_crisis"]]
r     <- cor(speech_lvl$pct_cc, speech_lvl$pct_crisis)

p_scatter <- ggplot(speech_lvl,
                    aes(x = pct_crisis, y = pct_cc,
                        color = period, size = n_words)) +
  geom_jitter(alpha = 0.55, width = 0.4, height = 0.4) +
  geom_smooth(aes(group = 1), method = "lm", se = TRUE,
              color = "gray25", fill = "gray85", linewidth = 0.7) +
  scale_color_manual(values = period_pal, name = NULL) +
  scale_size_continuous(range = c(1.2, 6), guide = "none") +
  scale_x_continuous(labels = function(z) paste0(z, "%"),
                     expand = expansion(mult = c(0.02, 0.05))) +
  scale_y_continuous(labels = function(z) paste0(z, "%"),
                     expand = expansion(mult = c(0.02, 0.08))) +
  labs(
    title    = "One dot = one speech",
    subtitle = sprintf(
      "Within a speech: more crisis content ↔ less climate-change content?\nLinear slope = %+.2f pp CC per +1pp crisis · Pearson r = %+.2f",
      slope, r),
    x = "Share of speech about local crisis",
    y = "Share about climate change",
    caption = "Dot size = speech length. Color = period."
  ) +
  theme_minimal(base_family = "Georgia", base_size = 11) +
  theme(
    legend.position    = "top",
    legend.justification = c(0, 1),
    legend.margin      = margin(0, 0, 6, 0),
    plot.title         = element_text(size = 13, color = "gray10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9.5, color = "gray45",
                                      margin = margin(b = 14), lineheight = 1.2),
    plot.caption       = element_text(size = 8, color = "gray60", hjust = 0,
                                      margin = margin(t = 10)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    plot.background    = element_rect(fill = PAGE_BG, color = NA),
    plot.margin        = margin(18, 22, 14, 18)
  )

ggsave("cc_crowding_scatter.png", p_scatter,
       width = 8, height = 6, dpi = 300, bg = PAGE_BG)
message("Saved: cc_crowding_scatter.png")

# (C) 100% stacked area -------------------------------------------------------
stacked_data <- df %>%
  group_by(year) %>%
  summarise(
    `Climate change` = sum(wc[about_cc == 1]),
    `Local crisis`   = sum(wc[is_crisis_bin == 1]),
    `Other`          = sum(wc[about_cc == 0 & is_crisis_bin == 0]),
    total            = sum(wc),
    .groups = "drop"
  ) %>%
  mutate(across(c(`Climate change`, `Local crisis`, `Other`),
                ~ . / total * 100)) %>%
  select(-total) %>%
  pivot_longer(-year, names_to = "category", values_to = "pct") %>%
  mutate(category = factor(category,
                           levels = c("Climate change", "Other", "Local crisis")))

stack_pal <- c("Climate change" = CC_COL,
               "Local crisis"   = CRISIS_COL,
               "Other"          = "#ececec")

p_stacked <- ggplot(stacked_data,
                    aes(x = year, y = pct, fill = category)) +
  geom_area(position = "stack", color = "white", linewidth = 0.25) +
  scale_fill_manual(values = stack_pal, name = NULL,
                    breaks = c("Climate change", "Local crisis", "Other")) +
  scale_y_continuous(labels = function(z) paste0(z, "%"),
                     expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(1998, 2024, 4),
                     expand = c(0, 0)) +
  labs(
    title    = "What share of speech words goes to each category, year by year?",
    subtitle = "100%-stacked. Green and red squeeze the gray; if green and red trade space, that's crowding out.",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Georgia", base_size = 11) +
  theme(
    legend.position    = "top",
    legend.justification = c(0, 1),
    legend.margin      = margin(0, 0, 6, 0),
    plot.title         = element_text(size = 13, color = "gray10",
                                      margin = margin(b = 4)),
    plot.subtitle      = element_text(size = 9.5, color = "gray45",
                                      margin = margin(b = 14), lineheight = 1.2),
    panel.grid          = element_blank(),
    axis.text.y        = element_text(color = "gray45"),
    axis.text.x        = element_text(color = "gray35"),
    plot.background    = element_rect(fill = PAGE_BG, color = NA),
    plot.margin        = margin(18, 22, 14, 18)
  )

ggsave("cc_crowding_stacked.png", p_stacked,
       width = 9, height = 5, dpi = 300, bg = PAGE_BG)
message("Saved: cc_crowding_stacked.png")

# Print top of speech_lvl + summary stats for reference
cat("\nCrowding-out fit (speech level):\n")
print(summary(fit)$coefficients)
cat(sprintf("Pearson r(crisis%%, CC%%) = %+.3f over %d speeches\n\n",
            r, nrow(speech_lvl)))

# ── 9. Save combined views ───────────────────────────────────────────────────

ggsave("cc_pages_by_period.png",   p_pages,   width = 9, height = 7,   dpi = 300, bg = PAGE_BG)
ggsave("cc_density_by_period.png", p_density, width = 9, height = 4.5, dpi = 300, bg = PAGE_BG)

# Combined panel
combined <- p_pages / p_density + plot_layout(heights = c(2.2, 1))
ggsave("cc_combined.png", combined, width = 9, height = 11, dpi = 300, bg = PAGE_BG)

message("\nSaved: cc_pages_by_period.png, cc_density_by_period.png, cc_combined.png")
