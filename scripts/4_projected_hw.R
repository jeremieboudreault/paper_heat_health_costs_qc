# 4_projected_hw.R


# Step 4: Compute projected heatwaves based on INSPQ thresholds.


# Project : paper_heat_health_costs
# Author  : Jeremie Boudreault
# Email   : Prenom.Nom@inrs.ca
# Depends : R (v4.3.0)
# Imports : jutils (v0.1) [https://github.com/jeremieboudreault/rjutils]
# License : CC BY-NC-ND 4.0


# Packages ---------------------------------------------------------------------


library(data.table)
library(rjutils)


# Globals ----------------------------------------------------------------------


# Global of the projects.
source("scripts/funs/globals.R")
source("scripts/funs/compute_heatwaves.R")


# Batch computing heatwaves in with projected data ------------------------------------


# # Create a folder for cache.
# dir.create("cache/")
# 
# # Loop on SSP.
# for (ssp in ssps) {
# 
#     # Load climatic data.
#     clim <- qs::qread(file = paste0(
#         path_clim, "daily_1980_2100_", ssp, "_stns_centrois_byrss_tmeanbcdaymet.qs"
#     ))[
#         i = RSS %in% rss_sel,
#         j = .(RSS, GCM, DATE, YEAR, MONTH = as.integer(format(DATE, "%m")), T_MAX, T_MIN)
#     ][MONTH %in% 4:10, -c("MONTH")] # Extended season april to octobre.
# 
#     # Extract GCMS.
#     gcms <- unique(clim$GCM)
# 
#     # Process all GCM.
#     for (gcm in gcms) {
# 
#         # Message.
#         message("--- Processing GCM ", which(gcm == gcms), "/", length(gcms), ". (", ssp, ") ---")
# 
#         # Copy dataset.
#         x <- copy(clim[GCM == gcm, ])
# 
#         # Compute heatwaves.
#         # NOTE : Temporary removed the check of dates because of 360-days GCMs.
#         hw_gcm <- compute_heatwaves(x, verbose = FALSE, check_dates = FALSE)
# 
#         # Add GCM and SSP.
#         hw_gcm[, GCM := gcm]
#         hw_gcm[, SSP := ssp]
# 
#         # Cache results.
#         qs::qsave(hw_gcm, paste0("cache/hw_", gcm, "_", ssp, ".qs"))
# 
#     }
# 
# }
# 
# # Reload all files and merge.
# files <- list.files("cache/", full.names = TRUE)
# hw <- do.call(rbind, lapply(files, qs::qread))
# 
# # Exports.
# qs::qsave(hw, "data/hw_proj/hw_proj_apr_oct_1980_2099_byrss.qs")


# Imports ----------------------------------------------------------------------


# Population data.
pop <- fread("data/pop/pop_1980_2100_byrss.csv")

# Heatwaves data.
hw <- qs::qread("data/hw_proj/hw_proj_apr_oct_1980_2099_byrss.qs")

# Add month.
hw[, MONTH_START := as.integer(format(START, "%m"))]
hw[, MONTH_END := as.integer(format(END, "%m"))]

# Remove heatwaves in April and Octobre only.
hw <- hw[!(MONTH_START == (min(months_sel) - 1L) & MONTH_END == (min(months_sel) - 1L)), ]
hw <- hw[!(MONTH_START == (max(months_sel) + 1L) & MONTH_END == (max(months_sel) + 1L)), ]

# Add period.
hw[YEAR %in% years_h, PERIOD := "hist"]
hw[YEAR %in% years_f, PERIOD := "future"]


# Plot of net change in HW between 2000 and 2050 -------------------------------


# Count heatwaves and total durations.
hw_stat <- hw[
    i  = RSS %in% rss_sel, 
    j  = .(HW_N = .N/30, HW_DURATION = sum(DURATION)/30), 
    by = c("PERIOD", "RSS", "GCM", "SSP")][!is.na(PERIOD), ] |> 
melt(
    id.vars       = c("GCM", "PERIOD", "RSS", "SSP"), 
    value.name    = "VALUE", 
    variable.name = "INDEX"
)

# Expand grid to all possible values prior to compute median.
hw_stat <- expand_dt(hw_stat, c("GCM", "PERIOD", "RSS", "SSP", "INDEX"), fill = 0L)

# Merge period and SSP.
periods <- c("Historical", "Future (SSP2-4.5)", "Future (SSP5-8.5)")
hw_stat[PERIOD == "hist" & SSP == "ssp245",   PERIOD_NEW := periods[1L]]
hw_stat[PERIOD == "future" & SSP == "ssp245", PERIOD_NEW := periods[2L]]
hw_stat[PERIOD == "future" & SSP == "ssp585", PERIOD_NEW := periods[3L]]
hw_stat[, `:=`(PERIOD = NULL, SSP = NULL)]
hw_stat <- hw_stat[!is.na(PERIOD_NEW), .(GCM, PERIOD = PERIOD_NEW, RSS, INDEX, VALUE)]
hw_stat[, PERIOD := factor(PERIOD, levels = periods)]

# Compute median and percentiles values.
hw_stat_med <- hw_stat[, .(
    MEDIAN = median(VALUE), 
    LOW    = quantile(VALUE, probs = 0.10), 
    HIGH   = quantile(VALUE, probs = 0.90)), 
by = c("PERIOD", "RSS", "INDEX")]

# Create palette.
pal <- c("grey20", ul(pal_rbow[c("blue1", "red")]))

# Plot results - Number of heatwaves.
p1 <- ggplot(hw_stat_med[INDEX == "HW_N", ], aes(x = as.factor(RSS), fill = PERIOD)) + 
    geom_col(aes(y = MEDIAN), position = "dodge", col = NA) +
    geom_errorbar(aes(ymin = LOW, ymax = HIGH), width = 0.2, lwd = 0.3, position=position_dodge(0.9)) + 
    labs(x = "", y = "Number of heatwaves (#) per year", fill = "") +
    ggtitle("a) Number of heatwaves") + 
    scale_fill_manual(values = pal) + 
    jtheme(expand_xy = "x_only", title_hjust = 0) +
    theme(legend.margin = margin(t = 7), axis.ticks.x = element_blank())

# Plot results - Total heatwaves durations.
p2 <- ggplot(hw_stat_med[INDEX == "HW_DURATION", ], aes(x = as.factor(RSS), fill = PERIOD)) + 
    geom_col(aes(y = MEDIAN), position = "dodge", col = NA) +
    geom_errorbar(aes(ymin = LOW, ymax = HIGH), width = 0.2, lwd = 0.3, position=position_dodge(0.9)) + 
    labs(x = "Health region (HR)", y = "Heatwaves days per year", fill = "") +
    ggtitle("b) Heatwaves duration") + 
    scale_fill_manual(values = pal) + 
    jtheme(expand_xy = "x_only", title_hjust = 0) +
    theme(legend.margin = margin(t = 7), axis.ticks.x = element_blank())

# Combine both plots.
jarrange(list(p1, p2), ncol = 1)

# Export.
save_ggplot("plots/hw_proj/fig_1_hw_proj_byrss.png", size = c(8, 7))


# Table of net change in HW between 2000 and 2050 ------------------------------


# Convert to table.
hw_tbl <- hw_stat_med |> dcast(INDEX + RSS ~ PERIOD, value.var = "MEDIAN")
hw_tbl[is.na(Historical), Historical := 0]

# Compute change in percentage.
delta_ssp245 <- paste0("+", to_percent(hw_tbl[, (`Future (SSP2-4.5)` - Historical)/Historical]))
delta_ssp585 <-  paste0("+", to_percent(hw_tbl[, (`Future (SSP5-8.5)` - Historical)/Historical]))
delta_ssp245[delta_ssp245 %in% c("+Inf%", "+NaN%")] <- "-"
delta_ssp585[delta_ssp585 %in% c("+Inf%", "+NaN%")] <- "-"

# Update values.
hw_tbl[, Historical          := round_trim(Historical, 1L)]
hw_tbl[, `Future (SSP2-4.5)` := paste0(round_trim(`Future (SSP2-4.5)`, 1L), " (", delta_ssp245, ")")]
hw_tbl[, `Future (SSP5-8.5)` := paste0(round_trim(`Future (SSP5-8.5)`, 1L), " (", delta_ssp585, ")")]

# Fix header of the table.
hw_tbl[, INDEX := c("Number of HW", rep("", 14L), "HW duration", rep("", 14L))]

# Update colnames.
names(hw_tbl)[1:2] <- c(" ", "HR")

# Export to .CSV.
data.table::fwrite(hw_tbl, "out/tbl_hw_proj_stats_byhr.csv", sep = ";")

# Export table for number of heatwaves.
hw_tbl[, ` ` := md_to_bold(` `)]
hw_tbl[, `HR` := md_to_bold(`HR`)]
knitr::kable(hw_tbl, align = "lcccccc")


# Plot of temporal evolution (1980-2100) ---------------------------------------


# Count heatwaves and total durations across Quebec.
hw_qc <- hw[
    i  = RSS %in% rss_sel, 
    j  = .(HW_N = .N, HW_DURATION = sum(DURATION)), 
    by = c("YEAR", "GCM", "SSP")
] |> melt(
    id.vars       = c("GCM", "YEAR", "SSP"), 
    value.name    = "VALUE", 
    variable.name = "INDEX"
)

# Merge with all possible values prior to compute median.
hw_qc <- data.table(expand.grid(
    GCM    = unique(hw$GCM),
    YEAR   = 1980:2099,
    SSP    = c("ssp245", "ssp585"),
    INDEX  = c("HW_N", "HW_DURATION")
)) |> merge(hw_qc, all.x = TRUE, by = c("GCM", "YEAR", "SSP", "INDEX"))
hw_qc[is.na(VALUE), VALUE := 0]

# Compute median values.
hw_qc_med <- hw_qc[, .(
    MEDIAN = median(VALUE), 
    LOW    = quantile(VALUE, probs = 0.10), 
    HIGH   = quantile(VALUE, probs = 0.90)), 
    by = c("YEAR", "SSP", "INDEX")
]
hw_qc_med[SSP == "ssp245", SSP := "SSP2-4.5"]
hw_qc_med[SSP == "ssp585", SSP := "SSP5-8.5"]

# Palette.
pal <- ul(pal_rbow[c("blue1", "red")])

# Plot 1 - Number of heatwaves.
p1 <- ggplot(hw_qc_med[INDEX == "HW_N", ], aes(x = YEAR, y = MEDIAN, col = SSP, fill = SSP)) +
    geom_rect(aes(xmin = 1990, xmax = 2020, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
    geom_rect(aes(xmin = 2040, xmax = 2070, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
    geom_line(lwd = 1, show.legend = FALSE) +
    geom_ribbon(aes(ymin = LOW, ymax = HIGH, x = YEAR), alpha = 0.2, col = NA) +
    scale_linetype_manual(values = rep(1L, 35L)) +
    scale_x_continuous(breaks = c(1980 + 0:12 * 10), expand = expansion(0, 3)) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(y = "Number of heatwaves across 15 HR", x = "", fill = "") +  
    ggtitle("a) Number of heatwaves") + 
    jtheme(legend_alpha = 1, title_hjust = 0) + 
    theme(legend.margin = margin(t = 7))

# Plot 2 - Heatwaves duration.
p2 <- ggplot(hw_qc_med[INDEX == "HW_DURATION", ], aes(x = YEAR, y = MEDIAN, col = SSP, fill = SSP)) +
    geom_rect(aes(xmin = 1990, xmax = 2020, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
    geom_rect(aes(xmin = 2040, xmax = 2070, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
    geom_line(lwd = 1, show.legend = FALSE) +
    geom_ribbon(aes(ymin = LOW, ymax = HIGH, x = YEAR), alpha = 0.2, col = NA) +
    scale_linetype_manual(values = rep(1L, 35L)) +
    scale_x_continuous(breaks = c(1980 + 0:12 * 10), expand = expansion(0, 3)) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(y = "Heatwaves days across 15 HR", x = "Year", fill = "") +  
    ggtitle("b) Heatwaves duration") + 
    jtheme(legend_alpha = 1, title_hjust = 0) + 
    theme(legend.margin = margin(t = 7))

# Combine both plots.
jarrange(list(p1, p2), ncol = 1)

# Save.
save_ggplot("plots/hw_proj/fig_2_hw_evolution_1980_2100.png", size = c(8, 7))


# Compute number of heatwaves by period ----------------------------------------


# Count heatwaves and total durations across Quebec.
hw_number <- hw[
    i  = RSS %in% rss_sel, 
    j  = .(HW_N = .N/(30L)), 
    by = c("PERIOD", "GCM", "SSP")
][!is.na(PERIOD), ]

# Merge with all possible values prior to compute median.
hw_number <- data.table(expand.grid(
    GCM    = unique(hw$GCM),
    PERIOD = c("hist", "future"),
    SSP    = c("ssp245", "ssp585")
)) |> merge(hw_number, all.x = TRUE, by = c("GCM", "PERIOD", "SSP"))
hw_number[is.na(HW_N), HW_N := 0]

# Grand summary.
hw_number_med <- hw_number[, .(
    MEDIAN = median(HW_N), 
    LOW    = quantile(HW_N, probs = 0.10), 
    HIGH   = quantile(HW_N, probs = 0.90)), 
    by = c("PERIOD", "SSP")
]

# Palette.
pal <- c("grey50", "grey10", ul(pal_rbow[c("blue1", "red")]))
pal_val <- unique(hw_number_med[, paste0(PERIOD, "_", SSP)])

# Plot results.
ggplot(hw_number) + 
geom_point(aes(x = HW_N, y = GCM, col = paste0(PERIOD, "_", SSP))) + 
geom_vline(data = hw_number_med, aes(xintercept = MEDIAN, col = paste0(PERIOD, "_", SSP))) +
geom_rect(data = hw_number_med, aes(xmin = LOW, xmax = HIGH, ymin = -Inf, ymax = Inf, fill = paste0(PERIOD, "_", SSP)), alpha = 0.1) +
labs(x = "Number of heatwaves per year across 15 HR", y = NULL, fill = "", col = "") + 
ggtitle("a) Number of heatwaves per year") + 
scale_color_manual(values = pal, breaks = pal_val) + 
scale_fill_manual(values = pal, breaks = pal_val) + 
jtheme(legend_alpha = 1L,  title_hjust = 0)

# Export.
save_ggplot("plots/hw_proj/fig_3a_proj_hw_number_bygcm.png", size = c(9, 4.5))

# Final table.
hw_number_final <- rbind(
    hw_number,
    hw_number_med[, .(GCM = "MEDIAN", PERIOD, SSP, HW_N = MEDIAN)],
    hw_number_med[, .(GCM = "Q10", PERIOD, SSP, HW_N = LOW)],
    hw_number_med[, .(GCM = "Q90", PERIOD, SSP, HW_N = HIGH)]
)

# Export.
data.table::fwrite(hw_number_final, "data/hw_proj/hw_stats_number_hist_fut_rss_sel.csv", sep = ";")


# Compute number of people affected by heatwaves -------------------------------


# Rename SSP in pop dataset.
pop[SSP == "SSP2", SSP := "ssp245"]
pop[SSP == "SSP5", SSP := "ssp585"]

# Merge heatwaves with demo.
hw_popfix <- merge(hw, pop[DEMO == "fixed", ], by = c("RSS", "YEAR", "SSP"), all.x = TRUE)
hw_popssp <- merge(hw, pop[DEMO == "ssp", ], by = c("RSS", "YEAR", "SSP"), all.x = TRUE)
hw_pop <- rbind(hw_popfix, hw_popssp)

# Compute population affected by heatwaves.
daypop <- hw_pop[, .(DAYPOP = sum(POP * DURATION)), by = c("YEAR", "GCM", "SSP", "DEMO", "PERIOD")]

# Create a canvas because not all GCM have heatwaves.
daypop <- data.table(expand.grid(
    GCM    = unique(hw$GCM),
    YEAR   = 1980:2099,
    SSP    = c("ssp245", "ssp585"),
    DEMO   = c("fixed", "ssp")
)) |> merge(daypop, all.x = TRUE, by = c("GCM", "YEAR", "SSP", "DEMO"))
daypop[is.na(DAYPOP), DAYPOP := 0]

# Compute statistics.
daypop_stats <- daypop[, .(DAYPOP = mean(DAYPOP), DAYPOP_SD = sd(DAYPOP)), by = c("GCM", "PERIOD", "SSP", "DEMO")][!is.na(PERIOD), ]

# Grand summary.
daypop_stats_med <- daypop_stats[, .(
    MEDIAN = median(DAYPOP), 
    LOW    = quantile(DAYPOP, probs = 0.10), 
    HIGH   = quantile(DAYPOP, probs = 0.90)), 
    by = c("PERIOD", "SSP", "DEMO")
]

# Create palette.
pal <- c("grey70", "grey50", "grey30", "grey10", ul(pal_rbow[c("blue1", "purple",  "orange", "red")]))
pal_val <- unique(daypop_stats[, paste0(PERIOD, "_", SSP, "_pop", DEMO)])

# Plot results.
ggplot(daypop_stats) + 
    geom_point(aes(x = DAYPOP/10^6, y = GCM, col = paste0(PERIOD, "_", SSP, "_pop", DEMO))) + 
    geom_vline(data = daypop_stats_med, aes(xintercept = MEDIAN/10^6, col = paste0(PERIOD, "_", SSP, "_pop", DEMO))) +
    geom_rect(data = daypop_stats_med, aes(xmin = LOW/10^6, xmax = HIGH/10^6, ymin = -Inf, ymax = Inf, fill = paste0(PERIOD, "_", SSP, "_pop", DEMO)), alpha = 0.1) +
    labs(x = "People-days (M) affected by HW", y = NULL, fill = "", col = "", fill = "") + 
    scale_color_manual(values = pal, breaks = pal_val) + 
    scale_fill_manual(values = pal, breaks = pal_val) + 
    ggtitle("b) People-days affected by HW") + 
    jtheme(legend_alpha = 1L, title_hjust = 0)

# Export.
save_ggplot("plots/hw_proj/fig_3b_proj_hw_pop_bygcm.png", size = c(9, 4.5))

# Final table.
daypop_stats_final <- rbind(
    daypop_stats, 
    daypop_stats_med[, .(GCM = "MEDIAN", PERIOD, SSP, DEMO, DAYPOP = MEDIAN, DAYPOP_SD = NA)],
    daypop_stats_med[, .(GCM = "Q10", PERIOD, SSP, DEMO, DAYPOP = LOW, DAYPOP_SD = NA)],
    daypop_stats_med[, .(GCM = "Q90", PERIOD, SSP, DEMO, DAYPOP = HIGH, DAYPOP_SD = NA)]
)

# Export.
data.table::fwrite(daypop_stats_final, "data/hw_proj/hw_stats_pop_hist_fut_rss_sel.csv", sep = ";")


# Export results to the table of projected AN ----------------------------------


# Create table.
tbl <- data.table(expand.grid(
    SSP     = c("ssp245", "ssp585"), 
    HO      = c("HW_N", "HW_POPDAY"),
    CLIM    = c("Historical", "Future"),
    POP     = c("Historical", "Future")
))[order(SSP, HO, CLIM, POP)][!(CLIM == "Historical" & POP == "Future"), ]

# Fill table.
for (ssp in c("ssp245", "ssp585")) {
    tbl[
        i = HO == "HW_N" & SSP == ssp & CLIM == "Historical" & POP == "Historical", 
        j = AN := hw_number_med[PERIOD == "hist" & SSP == ssp, MEDIAN]
    ]
    tbl[
        i = HO == "HW_N" & SSP == ssp & CLIM == "Future", 
        j = AN := hw_number_med[PERIOD == "future" & SSP == ssp, MEDIAN]
    ]
    tbl[
        i = HO == "HW_POPDAY" & SSP == ssp & CLIM == "Historical" & POP == "Historical", 
        j = AN := daypop_stats_med[PERIOD == "hist" & SSP == ssp & DEMO == "fixed", MEDIAN/10^6]
    ]
    tbl[
        i = HO == "HW_POPDAY" & SSP == ssp & CLIM == "Future" & POP == "Historical", 
        j = AN := daypop_stats_med[PERIOD == "future" & SSP == ssp & DEMO == "fixed", MEDIAN/10^6]
    ]
    tbl[
        i = HO == "HW_POPDAY" & SSP == ssp & CLIM == "Future" & POP == "Future", 
        j = AN := daypop_stats_med[PERIOD == "future" & SSP == ssp & DEMO == "ssp", MEDIAN/10^6]
    ]
}

# Compute percentage change.
tbl[c(1:4 * 3 - 1), AN_P := ((tbl$AN[c(1:4 * 3 - 1)] / tbl$AN[c(1:4 * 3 - 2)]) - 1) * 100]
tbl[c(1:4 * 3 - 1), AN_P := ((tbl$AN[c(1:4 * 3 - 1)] / tbl$AN[c(1:4 * 3 - 2)]) - 1) * 100]
tbl[c(1:4 * 3 - 0), AN_P := ((tbl$AN[c(1:4 * 3 - 0)] / tbl$AN[c(1:4 * 3 - 2)]) - 1) * 100]
tbl[c(1:4 * 3 - 0), AN_P := ((tbl$AN[c(1:4 * 3 - 0)] / tbl$AN[c(1:4 * 3 - 2)]) - 1) * 100]

# Format numbers.
tbl[, AN := round_trim(AN, 1L)]
tbl[, AN_P := paste0("+", round_trim(AN_P, 0), "%")]
tbl[AN_P == "+NA%", AN_P := "Ref."]

# Get rid of some repetive HO.
tbl[HO == "HW_N", HO := "# of HW"]
tbl[HO == "HW_POPDAY", HO := "Pop-days"]
tbl[, HO := as.character(HO)]
tbl[c((1:4 * 3) - 1, 1:4 * 3), HO := ""]

# Rename columns.
tbl <- tbl[, .(
    SSP, 
    ` `                = HO,  
    Climate            = CLIM, 
    Population         = POP, 
    `Total heat (#)`   = AN,
    `Total heat (%)`   = AN_P,
    `Extreme heat (#)` = "",
    `Extreme heat (%)` = ""
)]

# Export as one table per SSP.
for (ssp in ssps) {
    
    # Message.
    message("Table of projected AN for ", ssp, " :")
    
    # Subset the current SSP.
    tbl_sub <- tbl[SSP == ssp, -c("SSP")]
    
    # Export to .CSV.
    data.table::fwrite(tbl_sub, paste0("out/tbl_hw_proj_", ssp, ".csv"), sep = ";")
    
    # Export to Markdown.
    tbl_sub[, 1] <- md_to_bold(ul(tbl_sub[, 1]))
    print(knitr::kable(tbl_sub, align = "lllccccc"))
    
}


# Export table of HW -----------------------------------------------------------


# Compute summary.
hw_stat <- hw[, .(N_HW = .N, N_HWDAYS = sum(DURATION)), by = c("RSS", "YEAR", "GCM", "SSP")]

# Add population data.
hw_popfix <- merge(hw_stat, pop[DEMO == "fixed", ], by = c("RSS", "YEAR", "SSP"), all.x = TRUE)
hw_popssp <- merge(hw_stat, pop[DEMO == "ssp", ], by = c("RSS", "YEAR", "SSP"), all.x = TRUE)
hw_pop <- rbind(hw_popfix, hw_popssp)

# Sum heatwaves and daypop.
hw_pop <- hw_pop[, .(HW_N = sum(N_HW), DAYPOP = sum(N_HWDAYS * POP)), by = c("YEAR", "SSP", "DEMO", "GCM")]
hw_pop <- expand_dt(hw_pop, c("YEAR", "SSP", "DEMO", "GCM"), fill = 0L)

# Export results.
qs::qsave(hw_pop, "data/hw_proj/hw_stats_proj_1980_2099.qs")
