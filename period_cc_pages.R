# ── Climate Change paragraphs across speeches: change over time ──────────────
# Visualises frequency and placement of CC paragraphs per period.
#
# Run from a working dir that can write PNGs.
# ─────────────────────────────────────────────────────────────────────────────

library(tidyverse)
library(scales)

# ── 1. Settings (edit me) ────────────────────────────────────────────────────

CSV_URL    <- "https://raw.githubusercontent.com/sveisa/cc_stateheads/refs/heads/main/overview_cleaned2.csv"

period_breaks <- c(1997, 2007, 2016)
period_max    <- 2024      

N_BINS  <- 40             
ROW_GAP <- 0.25           
CC_COL      <- "#006064"  
CRISIS_COL  <- "#BF360C"  
BG_COL      <- "#e5e7eb"  
PAGE_BG <- "#fdfdfc"      
PAPER_BORDER <- "#cfcfcf"

# ── 2. Load & prep ───────────────────────────────────────────────────────────

df_raw <- read_csv(CSV_URL, show_col_types = FALSE)

df <- df_raw %>%
  mutate(
    graf_num   = as.integer(str_extract(graf_id, "(?<=graf)\\d+")),
    about_cc   = as.integer(about_cc),
    wc         = as.integer(wc),
    year       = as.integer(year),
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
    pct_cc_grafs       = n_cc_grafs      / n_grafs * 100,
    pct_crisis_grafs   = n_crisis_grafs / n_grafs * 100,
    pct_cc_words       = sum(wc[about_cc == 1])      / sum(wc) * 100,
    pct_crisis_words   = sum(wc[is_crisis_bin == 1]) / sum(wc) * 100,
    pct_speeches_cc    = n_speeches_with_cc / n_speeches * 100,
    .groups = "drop"
  )

# ── 4. Page composite: words-in-bin × CC share, per period ───────────────────

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
    pct_cc     = ifelse(words > 0, cc_words      / words, 0),
    pct_crisis = ifelse(words > 0, crisis_words / words, 0),
    bin_top    = bin - 1, 
    bin_bottom = bin
  )

# ── 5. Plot 1: page-style composite (CC Only) ────────────────────────────────

LINE_PAD <- 0.05      
LINE_X0  <- LINE_PAD
LINE_X1  <- 1 - LINE_PAD
LINE_LEN <- LINE_X1 - LINE_X0

period_bins <- period_bins %>%
  mutate(
    cc_x1     = LINE_X0 + LINE_LEN * pct_cc,
    crisis_x0 = LINE_X1 - LINE_LEN * pct_crisis
  )

pages <- freq %>%
  mutate(
    label = sprintf(
      "%s\n%d speeches · %d paragraphs\n%.1f%% CC",
      period, n_speeches, n_grafs, pct_cc_grafs
    )
  )

p_pages <- ggplot() +
  geom_rect(
    data = pages,
    aes(xmin = 0, xmax = 1, ymin = 0, ymax = N_BINS),
    fill = PAGE_BG, color = PAPER_BORDER, linewidth = 0.3
  ) +
  geom_rect(
    data = period_bins,
    aes(xmin = LINE_X0, xmax = LINE_X1,
        ymin = bin - 1 + ROW_GAP, ymax = bin - ROW_GAP),
    fill = BG_COL
  ) +
  geom_rect(
    data = period_bins %>% filter(pct_cc > 0),
    aes(xmin = LINE_X0, xmax = cc_x1,
        ymin = bin - 1 + ROW_GAP, ymax = bin - ROW_GAP),
    fill = CC_COL
  ) +
  scale_y_reverse(expand = expansion(add = c(0.5, 0.5))) +   
  scale_x_continuous(expand = expansion(add = 0)) +
  facet_wrap(~ period, nrow = 1,
             labeller = as_labeller(setNames(pages$label, pages$period))) +
  coord_fixed(ratio = 1 / N_BINS * 1.4) +
  labs(
    title    = "Climate change in speeches: how often, and where",
    subtitle = "Each 'page' = one period. Top = speech start, bottom = speech end.\nGreen grows from the LEFT = share of words at that position about climate change.",
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
    strip.text       = element_text(size = 9.5, color = "gray15", lineheight = 1.2,
                                    margin = margin(b = 8)),
    panel.spacing.x  = unit(1.2, "lines"),
    plot.margin      = margin(18, 18, 14, 18),
    plot.background  = element_rect(fill = PAGE_BG, color = NA)
  )

# ── 6. Single-period pages (CC Only) ─────────────────────────────────────────

make_single_page <- function(per) {
  pb   <- period_bins %>% filter(period == per)
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
      data = pb %>% filter(pct_cc > 0),
      aes(xmin = LINE_X0, xmax = cc_x1,
          ymin = bin - 1 + ROW_GAP, ymax = bin - ROW_GAP),
      fill = CC_COL
    ) +
    scale_y_reverse(expand = expansion(add = c(0.5, 0.5))) +
    scale_x_continuous(expand = expansion(add = 0)) +
    coord_fixed(ratio = 1 / N_BINS * 1.4) +
    labs(
      title    = as.character(per),
      subtitle = sprintf("%d speeches · %d paragraphs · %.1f%% CC",
                         meta$n_speeches, meta$n_grafs,
                         meta$pct_cc_grafs),
      caption  = "Top = speech start. Green from left = climate change."
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

# ── 7. Combined three-period page ────────────────────────────────────────────

ggsave("cc_pages_by_period.png", p_pages,
       width = 9, height = 7, dpi = 300, bg = PAGE_BG)
message("Saved: cc_pages_by_period.png")

# ── 8. Crowding-out: words time series (Smooth Only) ─────────────────────────

YEAR_BREAKS <- seq(1998, 2024, 4)

cat_pal <- c("Climate change" = CC_COL, "Local crisis" = CRISIS_COL)

yearly_words <- df %>%
  group_by(year) %>%
  summarise(
    `Climate change` = sum(wc[about_cc == 1])      / sum(wc) * 100,
    `Local crisis`   = sum(wc[is_crisis_bin == 1]) / sum(wc) * 100,
    .groups = "drop"
  ) %>%
  pivot_longer(-year, names_to = "category", values_to = "pct") %>%
  mutate(category = factor(category, levels = c("Climate change", "Local crisis")))

p_timeseries_words <- ggplot(yearly_words,
                             aes(x = year, y = pct, color = category)) +
  geom_smooth(method = "loess", se = FALSE, span = 0.55, linewidth = 1.5) +
  scale_color_manual(values = cat_pal, name = NULL) +
  scale_y_continuous(labels = function(z) paste0(z, "%"),
                     expand = expansion(mult = c(0.01, 0.04))) + # Tighter vertical padding
  scale_x_continuous(breaks = YEAR_BREAKS,
                     expand = expansion(mult = c(0.01, 0.01))) + # Tighter horizontal padding
  labs(
    title    = "Climate change vs. local crisis talk — smoothed annual share of all speech words.",
    x = NULL, y = "Share of speech words"
  ) +
  theme_minimal(base_family = "Georgia", base_size = 11) +
  theme(
    legend.position    = "top",
    legend.justification = c(0, 1),
    legend.margin      = margin(0, 0, 0, 0),       # Removed standard legend margin
    legend.box.margin  = margin(0, 0, -5, 0),      # Pulls the chart slightly up into the legend area
    plot.title         = element_text(size = 13, color = "gray10",
                                      margin = margin(b = 4)),
    panel.grid.minor   = element_blank(),
    panel.grid.major.x = element_line(color = "gray92", linewidth = 0.3),
    panel.grid.major.y = element_line(color = "gray92", linewidth = 0.3),
    plot.background    = element_rect(fill = PAGE_BG, color = NA),
    plot.margin        = margin(5, 10, 5, 5)       # Drastically reduced from (18, 22, 14, 18)
  )

ggsave("cc_crowding_timeseries_words.png", p_timeseries_words,
       width = 9, height = 5, dpi = 300, bg = PAGE_BG)
message("Saved: cc_crowding_timeseries_words.png")


# ── 9. Crowding-out: stacked paragraphs ──────────────────────────────────────

stack_pal <- c("Climate change" = CC_COL,
               "Local crisis"   = CRISIS_COL,
               "Other"          = "#ececec")

stacked_paras <- df %>%
  group_by(year) %>%
  summarise(
    `Climate change` = sum(about_cc == 1),
    `Local crisis`   = sum(is_crisis_bin == 1),
    `Other`          = sum(about_cc == 0 & is_crisis_bin == 0),
    total            = n(),
    .groups = "drop"
  ) %>%
  mutate(across(c(`Climate change`, `Local crisis`, `Other`),
                ~ . / total * 100)) %>%
  select(-total)

# Pre-calculate the exact y-coordinates for each boundary
plot_data <- stacked_paras %>%
  transmute(
    year,
    y_cc_top     = `Climate change`,
    y_other_top  = `Climate change` + `Other`,
    y_crisis_top = 100
  )

p_stacked_paras <- ggplot(plot_data, aes(x = year)) +
  
  # LAYER 1: The 'Other' middle band (Bottom of the sandwich)
  geom_ribbon(aes(ymin = y_cc_top, ymax = y_other_top, fill = "Other"),
              color = "white", linewidth = 0.25) +
  
  # LAYER 2: The Gridlines (Drawn OVER 'Other', but before the top/bottom colors)
  geom_hline(yintercept = seq(0, 100, by = 25), color = "gray92", linewidth = 0.3) +
  geom_vline(xintercept = YEAR_BREAKS, color = "gray92", linewidth = 0.3) +
  
  # LAYER 3: The solid colors (Drawn ON TOP of the gridlines to hide them)
  geom_ribbon(aes(ymin = 0, ymax = y_cc_top, fill = "Climate change"),
              color = "white", linewidth = 0.25) +
  geom_ribbon(aes(ymin = y_other_top, ymax = y_crisis_top, fill = "Local crisis"),
              color = "white", linewidth = 0.25) +
  
  # Scale and Labels
  scale_fill_manual(values = stack_pal, name = NULL,
                    breaks = c("Local crisis", "Other", "Climate change")) +
  scale_y_continuous(labels = function(z) paste0(z, "%"), expand = c(0, 0)) +
  scale_x_continuous(breaks = YEAR_BREAKS, expand = c(0, 0)) +
  labs(
    title    = "What share of speech paragraphs goes to each category, year by year?",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_family = "Georgia", base_size = 11) +
  theme(
    legend.position    = "top",
    legend.justification = c(0, 1),
    legend.margin      = margin(0, 0, 0, 0),
    legend.box.margin  = margin(0, 0, -5, 0),
    plot.title         = element_text(size = 13, color = "gray10",
                                      margin = margin(b = 4)),
    # Disable default theme gridlines so we don't double-draw over our manual ones
    panel.grid         = element_blank(),
    axis.text.y        = element_text(color = "gray45"),
    axis.text.x        = element_text(color = "gray35"),
    plot.background    = element_rect(fill = PAGE_BG, color = NA),
    plot.margin        = margin(5, 10, 5, 5)
  )

ggsave("cc_crowding_stacked_paragraphs_v5.png", p_stacked_paras,
       width = 9, height = 5, dpi = 300, bg = PAGE_BG)
message("Saved: cc_crowding_stacked_paragraphs_v5.png")
