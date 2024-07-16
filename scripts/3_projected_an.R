# 3_projected_an.R


# Step 3 : Projected AN to heat using on E-R functions and socio-climatic projections.


# Project : paper_heat_health_costs
# Author  : Jeremie Boudreault
# Email   : Prenom.Nom@inrs.ca
# Depends : R (v4.3.0)
# Imports : jutils (v0.1) [https://github.com/jeremieboudreault/rjutils]
# License : CC BY-NC-ND 4.0


# Packages ---------------------------------------------------------------------


library(data.table)
library(dlnm)
library(rjutils)


# Globals ----------------------------------------------------------------------


# Load globals.
source("scripts/funs/globals.R")

# Path for climate data.
path_clim <- "/Volumes/ExtDataPhD/clim/espo_g6_r2_stns_centroid_byrss/"

# Define health outcomes.
hos <- c("MOR", "HOS", "EDV", "AMB", "811")


# Imports ----------------------------------------------------------------------


# Health outcomes summary.
ho_summ <- data.table::fread("data/health/ho_doy_1996_2019_byrss.csv")

# Exposure response-functions.
erfct_list <- qs::qread("data/health/reduced_blups_mmt_byrss.qs")

# Quantile of Tmean for extreme heat.
qrr <- fread("data/health/qrr_tmean_byrss.csv")

# Load population data.
pop <- fread("data/pop/pop_1980_2100_byrss.csv")

# Load temperature data used to develop E-R curve.
hist_temp <- fread(paste0(
    "../data_socioenv/",
    "out/socioenv_daily_data_byrss_v20231124.csv"
))[, .(RSS = rss, DATE = date, YEAR = year, MONTH = month, T_MEAN = tmean0)]


# Batch computing AN in with projected data ------------------------------------


# Loop on SSP245 and SSP585.
for (ssp in ssps) {
    
    # Load climatic data.
    clim <- qs::qread(file = paste0(
        path_clim, "daily_1980_2100_", ssp, "_stns_centrois_byrss_tmeanbcdaymet.qs"
    ))[
        i = RSS %in% rss_sel, 
        j = .(RSS, GCM, DATE, YEAR, MONTH = as.integer(format(DATE, "%m")), DOY = as.integer(format(DATE, "%j")), T_MEAN)
    ][MONTH %in% months_sel, -c("MONTH")]
    
    # Fix <DOY> for leap years.
    clim[(YEAR %% 4) == 0L, DOY := DOY - 1]
    
    # Loop on demographic scenario.
    for (demo in c("ssp", "fixed")) {
        
        # Extract population of interest.
        pop_sub <- pop[SSP == toupper(substr(ssp, 1L, 4L)) & DEMO == demo & RSS %in% rss_sel, ]
  
        # Loop on all HOs.
        results_tbl <- do.call(rbind, lapply(hos, function(ho) {
            
            # Message.
            message("Processing ", ho, " (", ssp, "+demo=", demo, ")")
            
            # Add daily mortality/morbidity rate.
            data <- merge(clim, ho_summ[HO == ho, ], all.x = TRUE, by = c("RSS", "DOY"))
            
            # Merge population data with climate and health.
            data <- merge(data, pop_sub, all.x = TRUE, by = c("RSS", "YEAR"))
            
            # Loop on all RSS.
            for (rss in rss_sel) {
            
                # Load exposure-response.
                er <- erfct_list[[ho]][[which(names(erfct_list[[ho]]) == rss)]]
                
                # Extract temperature data used to fit BLUP.
                temp_rss <- ul(hist_temp[
                    i = YEAR %in% map_years_ho[[ho]] & RSS == rss & MONTH %in% months_sel, 
                    j = "T_MEAN"
                ])
                
                # Argument for the tvar cross-basis.
                argvar_tvar <- list(
                    fun   = "ns", 
                    knots = quantile(temp_rss, probs = c(0.50, 0.90)), 
                    Bound = range(temp_rss) 
                ) 
                
                # Extract MMT and threshold for extreme temperature
                mmt <- er$cen
                ext <- qrr[RSS == rss, QRR]
                
                # Extract coef and vcov from the meta-regression.
                coef <- er$coefficients
                vcov <- er$vcov
                
                # Centered basis functions.
                bvar <- do.call(onebasis, c(list(x = ul(data[RSS == rss, T_MEAN])), argvar_tvar))
                cenvec <- do.call(onebasis, c(list(x = mmt), argvar_tvar))
                bvarcen <- scale(bvar, center = cenvec, scale = F)
                
                # Indicators for heat and extreme heat days.
                data[RSS == rss, heat_all     := T_MEAN > mmt]
                data[RSS == rss, heat_extreme := T_MEAN > max(mmt, ext)]
                
                # Compute AN.
                data[RSS == rss, AN := (1 - exp(-bvarcen %*% coef)) * COUNTRATE * POP]
                
                # Note : We could add Monte Carlo simulations here to get e95%CI.
                
            }
            
            # Compute AN for "total" and "extreme" heat.
            results_tbl <- do.call(rbind, lapply(names(map_trange), function(trange) {
                
                # Summary by year and RSS.
                data_byrss <- data[get(trange) == TRUE, .(AN = sum(AN), TRANGE = trange), by = c("GCM", "YEAR", "RSS")]
                data_qc <- data[get(trange) == TRUE, .(AN = sum(AN), TRANGE = trange, RSS = 99L), by = c("GCM", "YEAR")]
                
                # Return.
                return(rbind(data_byrss, data_qc))
                
            }))

            # Add health outcomes and SSP.
            results_tbl[, HO  := ho]
            results_tbl[, SSP := ssp]
            results_tbl[, POP := demo]
            
            # Return results_tbl.
            return(results_tbl)
            
        }))
    
        # Export results.
        qs::qsave(results_tbl, paste0("data/an_proj/an_proj_", ssp, "_demo", demo, "_1980_2099_byrss.qs"))
    
    }
}


# Import results ---------------------------------------------------------------


# Load datasets for SSP245 andSSP585.
x <- do.call(rbind, lapply(
    X   = xpaste0(ssps, paste0("demo", c("fixed", "ssp"))), 
    FUN = function(sce) 
        qs::qread(paste0("data/an_proj/an_proj_", sce, "_1980_2099_byrss.qs"))
))

# Create factors.
x[YEAR %in% years_h, PERIOD := "hist"]
x[YEAR %in% years_f, PERIOD := "future"]
x[, HO := factor(HO, levels = hos)]
x[, PERIOD := factor(PERIOD, level = c("hist", "future"))]
x[, SSP := factor(SSP, level = c("ssp245", "ssp585"))]
x[, POP := factor(POP, level = c("fixed", "ssp"))]
x[, TRANGE := factor(TRANGE, level = c("heat_all", "heat_extreme"))]

# Create SSP-POP combinaiton.
x[, SSPPOP := paste0(SSP, "_pop", POP)]
x[, SSPPOP := factor(map_ssppop[SSPPOP], levels = ul(map_ssppop))]


# Create table of projected AN for SSP245 and SSP585 ---------------------------


# Extract number of years for the priod.
nyears <- length(years_h)

# Summarize data across periods for the Quebec.
x_byper <- x[RSS == 99L, .(AN = sum(AN)/nyears), by = c("SSP", "POP", "GCM", "PERIOD", "HO", "TRANGE")]

# Median in GCM.
x_byper_med <-  x_byper[, .(AN = median(AN)), by = c("SSP", "POP", "PERIOD", "HO", "TRANGE")]

# Values.
tbl <- x_byper_med[PERIOD %in% c("hist", "future"), ] |> dcast(SSP + HO + PERIOD + POP ~ TRANGE, value.var = "AN")

# Get rid of every "hist" and "ssp".
tbl <- tbl[!(PERIOD == "hist" & POP == "ssp"), ]
tbl[, heat_all_p := as.numeric(NA)]
tbl[, heat_extreme_p :=  as.numeric(NA)]

# Compute % increase.
mat <- as.matrix(tbl[, c("heat_all", "heat_extreme")])
tbl[c(1:10 * 3 - 1), heat_all_p     := ((mat[c(1:10 * 3 - 1), 1] / mat[c(1:10 * 3 - 2), 1]) - 1) * 100]
tbl[c(1:10 * 3 - 1), heat_extreme_p := ((mat[c(1:10 * 3 - 1), 2] / mat[c(1:10 * 3 - 2), 2]) - 1) * 100]
tbl[c(1:10 * 3 - 0), heat_all_p     := ((mat[c(1:10 * 3 - 0), 1] / mat[c(1:10 * 3 - 2), 1]) - 1) * 100]
tbl[c(1:10 * 3 - 0), heat_extreme_p := ((mat[c(1:10 * 3 - 0), 2] / mat[c(1:10 * 3 - 2), 2]) - 1) * 100]

# Rearrange format of number.
tbl[, heat_all := round(heat_all, 0L)]
tbl[, heat_extreme := round(heat_extreme, 0L)]
tbl[, heat_all_p := paste0("+", round(heat_all_p, 0L), "%")]
tbl[, heat_extreme_p := paste0("+", round(heat_extreme_p, 0L), "%")]
tbl[heat_all_p == "+NA%", heat_all_p := "Ref."]
tbl[heat_extreme_p == "+NA%", heat_extreme_p := "Ref."]

# Maps values of population and climate prior to export.
tbl[, POP := ul(map_pop[POP])]
tbl[, PERIOD := ul(map_period[PERIOD])]

# Get rid of some repetive HO.
tbl[, HO := paste0("AN of ", HO)]
tbl[c((1:10 * 3) - 1, 1:10 * 3), HO := ""]

# Rename columns.
tbl <- tbl[, .(
    SSP, 
    ` `                = HO,  
    Climate            = PERIOD, 
    Population         = POP, 
    `Total heat (#)`   = heat_all,
    `Total heat (%)`   = heat_all_p,
    `Extreme heat (#)` = heat_extreme,
    `Extreme heat (%)` = heat_extreme_p
)]

# Export as one table per SSP.
for (ssp in ssps) {
    
    # Message.
    message("Table of projected AN for ", ssp, " :")
    
    # Subset the current SSP.
    tbl_sub <- tbl[SSP == ssp, -c("SSP")]
    
    # Export to .CSV.
    data.table::fwrite(tbl_sub, paste0("out/tbl_an_proj_", ssp, ".csv"), sep = ";")
    
    # Export to Markdown.
    tbl_sub[, 1] <- md_to_bold(ul(tbl_sub[, 1]))
    print(knitr::kable(tbl_sub, align = "lllccccc"))
    
}


# Plot time series --------------------------------------------------------------


# Loop on temperature range.
for (trange in c("heat_all", "heat_extreme")) {
    
    # Summarize across RSS.
    x_byyear <- x[RSS == 99L & TRANGE == trange & YEAR %in% 1990:2070, .(COUNT = sum(AN)), by = c("YEAR", "GCM", "SSPPOP", "HO")]
    
    # Median
    x_byyear_med <- x_byyear[YEAR %in% 1990:2070, .(
        MED  = median(COUNT),
        LOW  = quantile(COUNT, probs = 0.10), 
        HIGH = quantile(COUNT, probs = 0.90), 
        GCM  = "Median"), 
        by = c("YEAR", "SSPPOP", "HO"
    )] 
    
    # Palette.
    pal <- ul(pal_rbow[c("blue1", "purple",  "orange", "red")])
    #plot_pal(pal)
    
    # Plot.
    ggplot(x_byyear_med, aes(x = YEAR, y = MED, col = SSPPOP, fill = SSPPOP)) +
    geom_rect(aes(xmin = 1990, xmax = 2020, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
    geom_rect(aes(xmin = 2040, xmax = 2070, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
    geom_line(lwd = 1, show.legend = FALSE) +
    geom_ribbon(aes(ymin = LOW, ymax = HIGH, x = YEAR), alpha = 0.1, col = NA) +
    scale_linetype_manual(values = rep(1L, 35L)) +
    scale_x_continuous(breaks = c(1980 + 0:12 * 10), expand = expansion(0, 3)) +
    scale_color_manual(values = pal) +
    scale_fill_manual(values = pal) +
    labs(y = "Attributable numbers to heat during summer", x = "Year", fill = "") + 
    facet_wrap("HO", ncol = 1, scales = "free_y", strip.position = "right") +
    jtheme(legend_alpha = 1)
    
    # Save plot.
    save_ggplot(paste0("plots/an_proj/fig_1_an_proj_", trange, ".png"), size = c(8, 7))
    
}

