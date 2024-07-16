# 6_projected_costs.R


# Step 6: Compute projected health costs.


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


source("scripts/funs/globals.R")


# Import fixed values ----------------------------------------------------------


# Attributable number for SSP245 and SSP585 for Quebec.
health <- do.call(rbind, lapply(
    X   = xpaste0(ssps, paste0("demo", c("fixed", "ssp"))), 
    FUN = function(sce) 
        qs::qread(paste0("data/an_proj/an_proj_", sce, "_1980_2099_byrss.qs"))
))[RSS == 99L, -c("RSS")]
health <- expand_dt(health, cols = c("GCM", "YEAR", "TRANGE", "HO", "SSP", "POP"), fill = 0L)

# Heatwaves for SSP245 and SSP585 for Quebec.
hw <- qs::qread("data/hw_proj/hw_stats_proj_1980_2099.qs")

# Unit costs based on Table 1.
cost <- fread("data/cost/unit_health_costs.csv")

# Length of Stays (LoS) based on Table 1.
los <- fread("data/cost/los.csv")

# Other monetary values based on Table 1.
source("scripts/funs/costs.R")

# Simulations for CI95% intervals.
sim <- qs::qread(paste0(path_simul, "cost_sim_all.qs"))


# Compute median costs by year and GCM -----------------------------------------


# Direct costs associated with AN.
health_cost <- health[cost, on = "HO"][, .(HO, YEAR, TRANGE, GCM, SSP, POP, CAT = "COST", MIDDLE = AN * MIDDLE)]
health_cost[HO == "MOR", CAT := "SOC"] # For mortality, change category to "Societal".

# Indirect cost associated with AN.
health_prod <- health[los, on = "HO"][, .(HO, YEAR, TRANGE, GCM, SSP, POP, CAT = "PROD", MIDDLE = AN * MIDDLE * wage)]

# Heatwave costs.
hw_cost <- hw[, .(HO = "HW", YEAR, GCM, SSP, POP = DEMO, CAT = "COST", MIDDLE = HW_N * emerg)]
hw_wb <- hw[, .(HO = "HW", YEAR, GCM, SSP, POP = DEMO, CAT = "SOC", MIDDLE = DAYPOP * mrad)]

# Merge all costs.
cost_year <- rbind(
    health_cost,
    health_prod,
    copy(hw_cost)[, TRANGE := "heat_all"],
    copy(hw_cost)[, TRANGE := "heat_extreme"],
    copy(hw_wb)[, TRANGE := "heat_all"],
    copy(hw_wb)[, TRANGE := "heat_extreme"]
)

# Add <PERIOD>.
cost_year[YEAR %in% years_h, PERIOD := "hist"]
cost_year[YEAR %in% years_f, PERIOD := "future"]
cost_year[, PERIOD := factor(PERIOD, levels = c("hist", "future"))]


# Aggregate simulations --------------------------------------------------------


# Aggregate simulations by <CAT>.
sim_agg <- rbind(
    sim[, .(COST = sum(COST, na.rm = TRUE)), by = c("SIM_NEW", "CAT", "SSP", "POP", "TRANGE", "PERIOD")],
    sim[, .(COST = sum(COST, na.rm = TRUE), CAT = "ALL"), by = c("SIM_NEW",  "SSP", "POP",  "TRANGE",  "PERIOD")]
)

# Add <CAT_F>.
sim_agg[,  CAT_F := factor(ul(map_cost_cat[CAT]), levels = ul(map_cost_cat))]

# Compute CI95%.
ci95 <- sim_agg[, .(
    LOW = quantile(COST, probs = 0.025), HIGH = quantile(COST, probs = 0.975)
), by = c("SSP", "POP", "CAT_F", "PERIOD", "TRANGE")]


# Create tables of median costs by category ------------------------------------


# Aggregate costs by <CAT> and compute median of climate models.
agg_cost_cat <- cost_year[
    j  = .(MIDDLE = sum(MIDDLE)/30L), 
    by = c("PERIOD", "CAT", "SSP", "POP", "TRANGE", "GCM")
][, .(MIDDLE = median(MIDDLE)), by = c("PERIOD", "CAT", "SSP", "POP", "TRANGE")
] |> dcast(CAT + SSP + POP + PERIOD ~ TRANGE, value.var = "MIDDLE")

# Remove historical period with fixed population.
agg_cost_cat <- agg_cost_cat[!(PERIOD == "hist" & POP == "fixed") & !(is.na(PERIOD)), ]

# Compute total.
agg_cost_cat_tot <- agg_cost_cat[, .(CAT = "ALL", heat_all = sum(heat_all), heat_extreme = sum(heat_extreme)), by = c("SSP", "POP", "PERIOD")]
agg_cost_cat <- rbind(agg_cost_cat, agg_cost_cat_tot)
agg_cost_cat[, CAT_F := factor(ul(map_cost_cat[CAT]), levels = ul(map_cost_cat))]

# Extract relevant information.
tbl <- agg_cost_cat[, .(SSP, CAT_F, PERIOD, POP, heat_all, heat_extreme)][order(SSP, CAT_F, PERIOD), ]

# Compute % increase.
mat <- as.matrix(tbl[, c("heat_all", "heat_extreme")])
ncol <- 8
tbl[c(1:ncol * 3 - 1), heat_all_p     := ((mat[c(1:ncol * 3 - 1), 1] / mat[c(1:ncol * 3 - 2), 1]) - 1) * 100]
tbl[c(1:ncol * 3 - 1), heat_extreme_p := ((mat[c(1:ncol * 3 - 1), 2] / mat[c(1:ncol * 3 - 2), 2]) - 1) * 100]
tbl[c(1:ncol * 3 - 0), heat_all_p     := ((mat[c(1:ncol * 3 - 0), 1] / mat[c(1:ncol * 3 - 2), 1]) - 1) * 100]
tbl[c(1:ncol * 3 - 0), heat_extreme_p := ((mat[c(1:ncol * 3 - 0), 2] / mat[c(1:ncol * 3 - 2), 2]) - 1) * 100]

# Rearrange format of number.
tbl[, heat_all := as.character(heat_all)]
tbl[, heat_extreme := as.character(heat_extreme)]
tbl[CAT_F %in% c("Direct", "Indirect"), heat_all := to_curM(as.numeric(heat_all), symbol = FALSE)]
tbl[CAT_F %in% c("Direct", "Indirect"), heat_extreme := to_curM(as.numeric(heat_extreme), symbol = FALSE)]
tbl[CAT_F %in% c("Intangible", "Total"), heat_all := to_curM(as.numeric(heat_all), dec = 0, symbol = FALSE)]
tbl[CAT_F %in% c("Intangible", "Total"), heat_extreme := to_curM(as.numeric(heat_extreme), 0, symbol = FALSE)]
tbl[, heat_all_p := paste0("+", round(heat_all_p, 0L), "%")]
tbl[, heat_extreme_p := paste0("+", round(heat_extreme_p, 0L), "%")]
tbl[heat_all_p == "+NA%", heat_all_p := "Ref."]
tbl[heat_extreme_p == "+NA%", heat_extreme_p := "Ref."]

# Extract 95% information.
ci95_tbl <- ci95[!(PERIOD == "hist" & POP == "fixed"), ]
ci95_tbl[CAT_F %in% c("Direct", "Indirect"), CI := paste0(" (", to_curM(as.numeric(LOW), symbol = FALSE), "–", to_curM(as.numeric(HIGH), symbol = FALSE), ")")]
ci95_tbl[CAT_F %in% c("Intangible", "Total"), CI := paste0(" (", to_curM(as.numeric(LOW), dec = 0L, symbol = FALSE), "–", to_curM(as.numeric(HIGH), dec = 0L, symbol = FALSE), ")")]
tbl <- merge(tbl, ci95_tbl[TRANGE == "heat_all", .(SSP, CAT_F, PERIOD, POP, heat_all_ci = CI)], by = c("SSP", "CAT_F", "PERIOD", "POP"), sort = FALSE)
tbl <- merge(tbl, ci95_tbl[TRANGE == "heat_extreme", .(SSP, CAT_F, PERIOD, POP, heat_extreme_ci = CI)], by = c("SSP", "CAT_F", "PERIOD", "POP"), sort = FALSE)
tbl[, `:=`(heat_all = paste0(heat_all, heat_all_ci), heat_extreme = paste0(heat_extreme, heat_extreme_ci))]
tbl[, `:=`(heat_all_ci = NULL, heat_extreme_ci = NULL)]

# Fix values prior to export.
tbl[, POP := ul(map_pop[POP])]
tbl[, PERIOD := ul(map_period[PERIOD])]
tbl[PERIOD == "Historical", POP := "Historical"]

# Get rid of some repetive CAT_F.
tbl[c((1:ncol * 3) - 1, 1:ncol * 3), CAT_F := ""]

# Rename columns.
tbl <- tbl[, .(
    SSP, 
    ` `                  = CAT_F,  
    Climate              = PERIOD, 
    Population           = POP, 
    `Total heat (M$)`     = heat_all,
    `Total heat (%)`     = heat_all_p,
    `Extreme heat (M$)`   = heat_extreme,
    `Extreme heat (%)`   = heat_extreme_p
)]

# Export as one table per SSP.
for (ssp in ssps) {
    
    # Message.
    message("Table of projected costs for ", ssp, " :")
    
    # Subset the current SSP.
    tbl_sub <- tbl[SSP == ssp, -c("SSP")]
    
    # Export to .CSV.
    data.table::fwrite(tbl_sub, paste0("out/tbl_cost_proj_", ssp, ".csv"), sep = ";", bom = TRUE)
    
    # Export to Markdown.
    tbl_sub[, 1] <- md_to_bold(as.character(ul(tbl_sub[, 1])))
    print(knitr::kable(tbl_sub, align = "lllrcrc"))
    
}


# Create tables of detailed costs by category -----------------------------------


# Extract median cost of GCM by period.
agg_cost_gcm <- cost_year[!is.na(PERIOD), .(MIDDLE = mean(MIDDLE)), by = c("PERIOD", "HO", "CAT", "GCM", "SSP", "POP", "TRANGE")]
agg_cost <- agg_cost_gcm[, .(MIDDLE = median(MIDDLE)), by = c("PERIOD", "CAT", "HO", "SSP", "POP", "TRANGE")]

# Create a new table of detailed costs.
agg_cost_det <- dcast(agg_cost, CAT + HO + SSP + POP + PERIOD ~ TRANGE, value.var = "MIDDLE")
agg_cost_det[, HO_F := factor(HO, levels = c(hos, "HW"))]
agg_cost_det[, CAT_F := factor(ul(map_cost_cat[CAT]), levels = ul(map_cost_cat))]

# Remove unecessary rows
agg_cost_det <- agg_cost_det[!(HO == "AMB" & CAT == "PROD"), ]
agg_cost_det <- agg_cost_det[!(PERIOD == "hist" & POP == "fixed"), ]

# Extract relevant information.
tbl <- agg_cost_det[, .(SSP, CAT_F, HO_F, PERIOD, POP, heat_all, heat_extreme)][order(SSP, CAT_F, HO_F, PERIOD), ]

# Compute % increase.
mat <- as.matrix(tbl[, c("heat_all", "heat_extreme")])
nrow <- 20
tbl[c(1:nrow * 3 - 1), heat_all_p     := ((mat[c(1:nrow * 3 - 1), 1] / mat[c(1:nrow * 3 - 2), 1]) - 1) * 100]
tbl[c(1:nrow * 3 - 1), heat_extreme_p := ((mat[c(1:nrow * 3 - 1), 2] / mat[c(1:nrow * 3 - 2), 2]) - 1) * 100]
tbl[c(1:nrow * 3 - 0), heat_all_p     := ((mat[c(1:nrow * 3 - 0), 1] / mat[c(1:nrow * 3 - 2), 1]) - 1) * 100]
tbl[c(1:nrow * 3 - 0), heat_extreme_p := ((mat[c(1:nrow * 3 - 0), 2] / mat[c(1:nrow * 3 - 2), 2]) - 1) * 100]

# Compute % of the the category and grand total.
agg_cost_share <- agg_cost_det[, .(HO_F, heat_all_share = heat_all/sum(heat_all), heat_extreme_share = heat_extreme/sum(heat_extreme)), by = c("CAT_F", "SSP", "PERIOD", "POP")]
agg_cost_share_tot <- agg_cost_det[, .(HO_F, CAT_F, heat_all_share_t = heat_all/sum(heat_all), heat_extreme_share_t = heat_extreme/sum(heat_extreme)), by = c("SSP", "PERIOD", "POP")]
tbl <- merge(tbl, agg_cost_share, by = c("CAT_F", "HO_F", "SSP", "PERIOD", "POP"), all.x = TRUE, sort = FALSE) |>
    merge(agg_cost_share_tot, by = c("CAT_F", "HO_F", "SSP", "PERIOD", "POP"), all.x = TRUE, sort = FALSE)

# Rearrange format of number.
tbl[CAT_F %in% c("Direct", "Indirect"), heat_all_t := to_curM(heat_all, symbol = FALSE)]
tbl[CAT_F %in% c("Direct", "Indirect"), heat_extreme_t := to_curM(heat_extreme, symbol = FALSE)]
tbl[CAT_F %in% c("Intangible", "Total"), heat_all_t := to_curM(heat_all, dec = 0, symbol = FALSE)]
tbl[CAT_F %in% c("Intangible", "Total"), heat_extreme_t := to_curM(heat_extreme, dec = 0, symbol = FALSE)]
tbl[, `:=`(heat_all = heat_all_t, heat_extreme = heat_extreme_t, heat_all_t = NULL, heat_extreme_t = NULL)]
tbl[, heat_all_p := paste0("+", round(heat_all_p, 0L), "%")]
tbl[, heat_extreme_p := paste0("+", round(heat_extreme_p, 0L), "%")]
tbl[, heat_all_share := to_percent(heat_all_share)]
tbl[, heat_extreme_share := to_percent(heat_extreme_share)]
tbl[, heat_all_share_t := to_percent(heat_all_share_t)]
tbl[, heat_extreme_share_t := to_percent(heat_extreme_share_t)]
tbl[heat_all_p == "+NA%", heat_all_p := "Ref."]
tbl[heat_extreme_p == "+NA%", heat_extreme_p := "Ref."]

# Fix values prior to export.
tbl[, POP := ul(map_pop[POP])]
tbl[, PERIOD := ul(map_period[PERIOD])]
tbl[PERIOD == "Historical", POP := "Historical"]

# Get rid of some repetive CAT_F and HO_F.
tbl[c((1:nrow * 3) - 1, 1:nrow * 3), HO_F := ""]
tbl[c(2:15, 17:24, 26:30, 32:45, 47:54, 56:60), CAT_F := ""]

# Rename columns.
tbl <- tbl[, .(
    SSP,
    ` `                  = CAT_F,
    ` `                  = HO_F,  
    Climate              = PERIOD, 
    Population           = POP, 
    `Cost (M$)`          = heat_all,
    `Increase (%)`       = heat_all_p,
    `Category %`         = heat_all_share,
    `Total %`            = heat_all_share_t,
    `Cost (M$)`          = heat_extreme,
    `Increase (%)`       = heat_extreme_p,
    `Category %`         = heat_extreme_share,
    `Total %`            = heat_extreme_share_t
)]

# Export as one table per SSP.
for (ssp in ssps) {
    
    # Message.
    message("Table of projected costs for ", ssp, " :")
    
    # Subset the current SSP.
    tbl_sub <- tbl[SSP == ssp, -c("SSP")]
    
    # Export to .CSV.
    data.table::fwrite(tbl_sub, paste0("out/tbl_cost_proj_", ssp, "_detailed.csv"), sep = ";", bom = TRUE)
    
    # Export to Markdown.
    tbl_sub[, 1] <- md_to_bold(as.character(ul(tbl_sub[, 1])))
    tbl_sub[, 2] <- md_to_bold(as.character(ul(tbl_sub[, 2])))
    print(knitr::kable(tbl_sub, align = "llccrcccrccc"))
    
}


# Barchart of costs increase ---------------------------------------------------


# Generate data for the plot.
agg_cost_bar <- agg_cost_cat |> melt(
    id.vars = c("CAT", "SSP", "POP", "PERIOD", "CAT_F"),
    var     = "TRANGE",
    value   = "COST"
) |> merge(ci95, by = c("SSP", "POP", "PERIOD", "TRANGE", "CAT_F"))

# Combine SSP, POP and PERIOD.
agg_cost_bar[SSP == "ssp245" & POP == "ssp" & PERIOD == "hist", SSPPOP := "hist"]
agg_cost_bar[PERIOD == "future", SSPPOP := paste0(SSP, "_pop", POP)]
agg_cost_bar <- agg_cost_bar[!is.na(SSPPOP), ]
agg_cost_bar[, SSPPOP := factor(map_ssppop2[SSPPOP], levels = ul(map_ssppop2))]

# Palette.
pal <- ul(pal_rbow[c("black", "blue1", "purple",  "orange", "red")])

# Plot.
plist <- lapply(names(map_trange), function(trange) {
    ggplot(agg_cost_bar[TRANGE == trange, ], aes(x = SSPPOP, y = COST/10^6, fill = SSPPOP)) + 
    geom_col(col = NA, alpha = 0.8, show.legend = FALSE) +
    facet_wrap("CAT_F", nrow = 1L, scales = "free_y") +
    scale_fill_manual(values = pal) + 
    labs(x = NULL, y = "Costs (2019M$)") +
    ggtitle(paste0(letters[which(trange == names(map_trange))], ") ", map_trange[[trange]])) + 
    jtheme(rotate_x_labs = 45L, expand_xy = "x_only", show_grid = TRUE, title_size = 12L, title_hjust = -0.12, borders = "normal") +
    theme(plot.margin = margin(, 0.2, , 0.8, "cm"))
})

# Hide labels from first sub-plots.
plist[[1]] <- plist[[1]] + theme(axis.text.x = element_text(size = 1, color = "white"))

# Combine both plots.
jarrange(plist, ncol = 1, heights = c(0.37, 0.63)/10)

# Save plot.
save_ggplot("plots/costs_proj/fig_1_costs_evolution.png", size = c(8, 5.5))


# Pie-chart of costs repartition -----------------------------------------------


# Copy table of results and add names.
agg_cost_pchart <- copy(agg_cost)[!(HO == "AMB" & CAT == "PROD"), NAME_F := factor(
    x      = map_cost_names_pchart[paste0(HO, "_", CAT)], 
    levels = ul(map_cost_names_pchart)
)]

# Combine SSP, POP and PERIOD.
agg_cost_pchart[SSP == "ssp245" & POP == "ssp" & PERIOD == "hist", SSPPOP := "hist"]
agg_cost_pchart[PERIOD == "future", SSPPOP := paste0(SSP, "_pop", POP)]
agg_cost_pchart <- agg_cost_pchart[!is.na(SSPPOP), ]
agg_cost_pchart[, SSPPOP := factor(map_ssppop_pchart[SSPPOP], levels = ul(map_ssppop_pchart))]

# Create palette.
pal <- RColorBrewer::brewer.pal(11, "Set3")[c(2L, 6L, 4L, 9L,  5L, 1L, 11L, 7L, 8L, 10L)]
pal <- c(pal[1:5], "#FFFFFF", pal[6:8], pal[9:10])
pal_val <- ul(map_cost_names_pchart)

# Loop on temperature range and SSP.
for (trange in names(map_trange)) {
    
    # Produce pie-chart.
    plist <- lapply(c("COST", "PROD", "SOC", "ALL"), function(cat) {
        
        # Subset data.
        if (cat != "ALL") {
            agg_cost_pchart_sub <- agg_cost_pchart[TRANGE == trange & CAT == cat , ]
        } else {
            agg_cost_pchart_sub <- agg_cost_pchart[TRANGE == trange, ]
        }
        
        # Loop on historical and future.    
        plist <- lapply(ul(map_ssppop_pchart), function(ssppop) {
            
            # Plot.
            p <- ggplot(agg_cost_pchart_sub[SSPPOP == ssppop, ], aes(x="", y=MIDDLE, fill=NAME_F)) +
            geom_bar(stat="identity", width=1, color="white", show.legend = TRUE) +
            coord_polar("y", start=0) +
            scale_fill_manual(breaks = pal_val, values = pal, drop = FALSE) + 
            jtheme(axes = FALSE, borders = "hide", legend_nrow = 3L, facet_size = 11L) +
            labs(x = NULL, y = NULL, fill = NULL) +
            facet_wrap("SSPPOP") +
            theme(legend.text = element_text(margin = margin(r = 20, l = 5)),
                  legend.key.spacing.y = unit(0.02, "cm"))

            # Add left title.
            if (ssppop == "Historical\n") 
                p <- p + xlab(map_cost_cat_l[[cat]]) + theme(axis.title.y = element_text(face = "bold", size = 11L, margin = margin(r = 20)))
            
            # Remove title if not the first occurent.
            if (cat != "COST")
                p <- p + theme(strip.text = element_text(size = 0.1, color = "white"))

            # Return plot.
            return(p)
            
        })
        
    })
    
    # Combine plots.
    jarrange(do.call(c, plist), ncol = 5L, nrow = 4, legend = "bottom", 
             heights = c(1, rep(0.8, 3L)),
             widths  = c(1.02, rep(0.8, 4L)))
    
    # Save plot.
    save_ggplot(paste0("plots/costs_proj/fig_2_pichart_", trange, ".png"), size = c(8, 7.5))
    
}


# Plot time series of costs ----------------------------------------------------


# Total by category.
cost_peryear <- rbind(
    cost_year[, .(COST = sum(MIDDLE)), by = c("GCM", "SSP", "POP", "TRANGE", "YEAR", "CAT")],
    cost_year[, .(COST = sum(MIDDLE), CAT = "ALL"), by = c("GCM", "SSP", "POP", "TRANGE", "YEAR")]
)
cost_peryear[, CAT := factor(ul(map_cost_cat_l[CAT]), levels = ul(map_cost_cat_l))]

# Create SSP-POP combinaiton.
cost_peryear[, SSPPOP := paste0(SSP, "_pop", POP)]
cost_peryear[, SSPPOP := factor(map_ssppop[SSPPOP], levels = ul(map_ssppop))]

# Compute median.
cost_peryear_med <- cost_peryear[YEAR %in% 1990:2070, .(
    MED  = median(COST)/10^6,
    LOW  = quantile(COST, probs = 0.10)/10^6, 
    HIGH = quantile(COST, probs = 0.90)/10^6, 
    GCM  = "Median"), 
    by = c("YEAR", "SSPPOP", "CAT", "TRANGE")
] 

# Palette.
pal <- ul(pal_rbow[c("blue1", "purple",  "orange", "red")])

# Loop on temperature range.
for (trange in c("heat_all", "heat_extreme")) {
    
    # Plot.
    ggplot(cost_peryear_med[TRANGE == trange, ], aes(x = YEAR, y = MED, col = SSPPOP, fill = SSPPOP)) +
    geom_rect(aes(xmin = 1990, xmax = 2020, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
    geom_rect(aes(xmin = 2040, xmax = 2070, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
    geom_line(lwd = 1, show.legend = FALSE) +
    geom_ribbon(aes(ymin = LOW, ymax = HIGH, x = YEAR), alpha = 0.1, col = NA) +
    scale_linetype_manual(values = rep(1L, 35L)) +
    scale_x_continuous(breaks = c(1980 + 0:12 * 10), expand = expansion(0, 3)) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(y = paste0("Cost attributable to ", tolower(map_trange[trange]), " (2019M$)"), x = "Year", fill = "") + 
    facet_wrap("CAT", ncol = 1, scales = "free_y", strip.position = "right") +
    jtheme(legend_alpha = 1)
    
    # Save plot.
    save_ggplot(paste0("plots/costs_proj/fig_3_cost_proj_", trange, ".png"), size = c(8, 7))
    
}


# Plot box plots of simulations ------------------------------------------------


# Get of some periods.
sim_agg_bp <- copy(sim_agg)[, SSPPOP := paste0(SSP, "_pop", POP)][, -c("POP", "SSP")]
sim_agg_bp <- sim_agg_bp[PERIOD == "future" | (PERIOD == "hist" & SSPPOP == "ssp245_popssp"), ]
sim_agg_bp[PERIOD == "hist", SSPPOP := "hist"]
sim_agg_bp[, SSPPOP := factor(map_ssppop2[SSPPOP], levels = ul(map_ssppop2))]

# Palette.
pal <- c("grey80", ul(pal_rbow[c("blue1", "purple",  "orange", "red")]))

# Produce plot for all heat and extreme heat. 
plist <- lapply(names(map_trange), function(trange) {
    
    # Plot.
    ggplot(sim_agg_bp[TRANGE == trange, ], aes(y = COST/10^6, x = SSPPOP, fill = SSPPOP)) + 
    geom_boxplot(lwd = 0.35, show.legend = FALSE, outliers = FALSE, alpha = 0.8) +
    facet_wrap("CAT_F", scales = "free_y", nrow = 1L) + 
    labs(y = "Costs (2019M$)", x = NULL, fill = NULL) + 
    scale_fill_manual(values = pal) + 
    ggtitle(paste0(letters[which(trange == names(map_trange))], ") ", map_trange[[trange]])) + 
    jtheme(rotate_x_labs = 45L, show_grid = TRUE, title_size = 12L, title_hjust = -0.12, borders = "normal") +
    theme(plot.margin = margin(, 0.2, , 0.8, "cm"))
    
})

# Hide labels from first sub-plots.
plist[[1]] <- plist[[1]] + theme(axis.text.x = element_text(size = 1, color = "white"))

# Combine both plots.
jarrange(plist, ncol = 1, heights = c(0.39, 0.61)/10)

# Save plot.
save_ggplot("plots/costs_proj/fig_4_costs_sim_boxplots_new.png", size = c(8, 5.5))


# Perform Dunn-test for pair-wise comparison -----------------------------------


# Run tests for all heat and extreme heat.
tests <- rbind(
    dtlapply(names(map_cost_cat), function(cat) {
        message("Performing ", cat, ". (heat_all)")
        x <- rstatix::dunn_test(
            data            = sim_agg_bp[CAT == cat & TRANGE == "heat_all", ], 
            formula         = COST ~ SSPPOP, 
            p.adjust.method = "bonferroni"
        )
        x <- data.table(x)[, `:=`(CAT = cat, TRANGE = "heat_all")]
        return(x)
    }),
    dtlapply(names(map_cost_cat), function(cat) {
        message("Performing ", cat, ". (heat_extreme)")
        x <- rstatix::dunn_test(
            data            = sim_agg_bp[CAT == cat & TRANGE == "heat_extreme", ], 
            formula         = COST ~ SSPPOP, 
            p.adjust.method = "bonferroni"
        )
        x <- data.table(x)[, `:=`(CAT = cat, TRANGE = "heat_extreme")]
        return(x)
    })
)

# Add some factors prior to plot.
tests[, SSPPOP1 := factor(group1, levels = (ul(map_ssppop2)))]
tests[, SSPPOP2 := factor(group2, levels = rev(ul(map_ssppop2)))]
tests[, CAT_F := factor(map_cost_cat[CAT], levels = ul(map_cost_cat))]

# Plots.
jarrange(lapply(names(map_trange), function(trange) {
    ggplot(tests[TRANGE == trange, ], aes(x = SSPPOP1, y = SSPPOP2)) +
    geom_tile(fill = pal_rdbu$light_blue, col = "black") +
    geom_text(aes(label = format(round(p.adj, 3L), nsmall = 3)), family = "Source Sans Pro", size = 3) +
    facet_wrap("CAT_F") +
    scale_x_discrete(expand = c(0, 0)) + 
    scale_y_discrete(expand = c(0, 0)) + 
    labs(x = NULL, y = NULL) + 
    ggtitle(paste0(letters[which(names(map_trange) == trange)], ") ", map_trange[[trange]])) + 
    jtheme(rotate_x_labs = 30, borders = "all", title_hjust = -0.5 + (trange == "heat_all") * 0.05)
}), ncol = 1L)

# Export.
save_ggplot("plots/costs_proj/fig_5_dunntests_pval.png", size = c(7, 8))
