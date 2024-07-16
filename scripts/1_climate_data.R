# 1_climate_data.R


# Step 1 : Climate data of EPSO-G6-R2 at ECCC stations.


# Project : paper_heat_health_costs
# Author  : Jeremie Boudreault
# Email   : Prenom.Nom@inrs.ca
# Depends : R (v4.3.0)
# Imports : jutils (v0.1) [https://github.com/jeremieboudreault/rjutils]
# License : CC BY-NC-ND 4.0


# Note : Climate data was extracted from PAVICS using the EPSO-G6-R2 dataset
#        of Lavoie et al.  (2024), consisting of multiple members from 
#        CMIP6, downscaled at a 10x10km resolution and bias-correction, at
#        ECCC reference stations used by INSPQ for heat alert.


# Packages ---------------------------------------------------------------------


library(data.table)
library(rjutils)


# Globals ----------------------------------------------------------------------


# Globals.
source("scripts/funs/globals.R")

# Available scenarios.
sce <- c("ssp245", "ssp370", "ssp585")

# Palette for SSP.
pal <- ul(pal_rbow[c("black", "yellow" , "green1", "blue1",  "orange", "red")])


# Imports data from ECCC at weather stations -----------------------------------


# Import.
stns <- qs::qread("data/weather/eccc_daily_1980_2021.qs")

# Clear duplicated stations.
stns[, RSS := paste0("RSS", RSS)]
stns <- stns[RSS %nin% c("RSS13", "RSS17", "RSS18")]

# Rename some RSS.
stns[RSS == "RSS6", RSS := "RSS613"]
stns[RSS == "RSS10", RSS := "RSS1018"]

# Check final values.
unique(stns$RSS) == unique(ul(map_stns_rss))

# Compute valid years for comparison.
stns_yearok <- stns[YEAR <= 2019L, .(YEAR_OK = sum(is.na(T_MAX) | is.na(T_MIN)) <= 18L), by = c("RSS", "YEAR")]


# Import pop-weighted Tmean data from Daymet -----------------------------------


# Import.
daymet <- qs::qread(
    "../data_socioenv/data/daymet/daymet_daily_lagged_data_byrss.qs"
)[, .(DATE, T_MEAN, RSS)]

# Add year.
daymet[, YEAR := as.integer(format(DATE, "%Y"))]


# Initial loading of climate data and tidying ----------------------------------


# List files.
files_dt <- data.table(FILE = list.files(file.path(path_clim, "raw")))

# Extract information from file.
files_str <- tstrsplit(files_dt$FILE, "_")
files_dt[, GCM := paste0(files_str[[8L]])]
files_dt[, SSP := files_str[[9L]]]

# Check that combination of GCP-SSP are unique.
gcm_ssp <- paste0(files_dt$gcms, "_", files_dt$ssps)
length(gcm_ssp) == length(unique(gcm_ssp))

# Check that we do not have "hot" GCM in our ensembles.
files_dt[GCM %in% gcms_hot, .N] == 0L

# Loop on all SSP for data importation.
for (ssp in unique(files_dt$SSP)) {
    
    # Loop on all GCM runs.
    clim <- lapply(files_dt[SSP == ssp, FILE], function(file) {
        
        # Message.
        message("Loading file ", file, " (SSP=", ssp, ")")
        
        # Load file.
        x <- fread(file.path(path_clim, "raw", file))
        
        # Subset years not relevant.
        x[, DATE := as.Date(DATE)]
        x[, YEAR := as.integer(format(DATE, "%Y"))]
        x <- x[YEAR >= 1980L & YEAR <= 2099L, ]
        
        # Add SSP and GCM.
        x[, SSP := ssp]
        x[, GCM := ul(files_dt[FILE == file, GCM])]
        
        # Compute T_MEAN.
        x[, T_MEAN := (T_MAX + T_MIN)/2]
        
        # Return x.
        return(x[, .(RSS, DATE, YEAR, SSP, GCM, T_MIN, T_MEAN, T_MAX, PRECIP)])
        
    })
    
    # Bind all files.
    clim <- rbindlist(clim)
    
    # Export as a cleaned QS object.
    qs::qsave(clim, file.path(path_clim, paste0("daily_1980_2100_", ssp, "_stns_centrois_byrss.qs")))
    
}


# Plots ------------------------------------------------------------------------


# Loop on SSP.
for (ssp in sce) {
    
    # Load data.
    clim <- qs::qread(file.path(path_clim, paste0("daily_1980_2100_", ssp, "_stns_centrois_byrss.qs")))
    
    # Add observed data to clim.
    clim <- rbind(clim, 
        stns[, .(RSS, DATE, YEAR, SSP = "obs", GCM = "obs", T_MIN, T_MEAN, T_MAX, PRECIP = NA)]
    )
    
    # Update RSS to factor.
    clim[, RSS := factor(RSS, levels = unique(ul(map_stns_rss)))]
    
    # Compute somes summaries.
    clim_an <- clim[, .(T_MAX = mean(T_MAX), T_MEAN = mean(T_MEAN), T_MIN = mean(T_MIN)), by = c("YEAR", "GCM", "SSP", "RSS")]
    clim_an_all <- clim_an[, .(T_MAX = median(T_MAX), T_MEAN = median(T_MEAN), T_MIN = median(T_MIN)), by = c("YEAR", "SSP", "RSS")]
    
    # Plot time series with all GCM and SSP
    ggplot(clim_an[GCM != "obs", ], aes(x = YEAR)) +
    geom_line(aes(y = T_MIN, lty = GCM, col = "T_MIN"), alpha = 0.1, show.legend = FALSE) +
    geom_line(aes(y = T_MAX, lty = GCM, col = "T_MAX"), alpha = 0.1, show.legend = FALSE) +
    geom_line(data = clim_an_all[SSP != "obs"], aes(x = YEAR, y = T_MIN, col = "T_MIN"), lty = 1, lwd = 1) + 
    geom_line(data = clim_an_all[SSP != "obs"], aes(x = YEAR, y = T_MAX, col = "T_MAX"), lty = 1, lwd = 1) + 
    geom_point(data = clim_an_all[SSP == "obs"], aes(x = YEAR, y = T_MIN, col = "obs"), pch = 19, size = 1, alpha = 0.8) + 
    geom_point(data = clim_an_all[SSP == "obs"], aes(x = YEAR, y = T_MAX, col = "obs"), pch = 19, size = 1, alpha = 0.8) + 
    facet_wrap("RSS") +
    scale_x_continuous(breaks = c(1980, 2010, 2040, 2070), expand = expansion(0, 0)) +
    scale_color_manual(values = ul(pal_rbow[c("black", "red", "blue1")])) + 
    scale_linetype_manual(values = rep(1, 30)) + 
    labs(x = "Year", y = "Temperature (ºC)", title = paste0(ssp), col = "") +
    jtheme(legend_nrow = 1L, tight_facets = TRUE)
    
    # Save plot.
    save_ggplot(paste0("plots/clim/fig_1_temp_", ssp, ".jpg"), size = c(10, 8))

}


# Temperature distribution comparaison -----------------------------------------


# Loop on SSP.
for (ssp in sce) {

    # Load data.
    clim <- qs::qread(file.path(path_clim, paste0("daily_1980_2100_", ssp, "_stns_centrois_byrss.qs")))
    
    # Keep historical data only for comparison. 
    clim <- clim[YEAR <= 2019, ]
    clim[, PRECIP := NULL]
    
    # Add observed data to clim.
    clim <- rbind(clim, 
        stns[, .(RSS, DATE, YEAR, SSP = "obs", GCM = "obs", T_MIN, T_MEAN, T_MAX)]
    )
    
    # Only keep valid year for comparison.
    clim <- merge(clim, stns_yearok, by = c("RSS", "YEAR"))[YEAR_OK == TRUE, ]
    
    # Update RSS to factor.
    clim[, RSS := factor(RSS, levels = unique(ul(map_stns_rss)))]
    
    # Plot results for T_MAX and T_MIN.
    for (tvar in c("T_MAX", "T_MIN")) {

        ggplot(clim, aes(x = get(tvar), y = after_stat(density), fill = SSP)) +
        geom_density(alpha = 0.5, col = NA, adjust = 1.5) +
        facet_wrap("RSS") + 
        scale_fill_manual(values = ul(pal_rdbu[c("red", "blue")])) + 
        scale_x_continuous(breaks = c(-40, -20, 0, 20)) + 
        labs(y = "Density", x = "Temperature (ºC)", fill = NULL) + 
        ggtitle(paste0(tvar, " (", ssp, ")")) + 
        jtheme(tight_facets = TRUE, expand_xy = "x_only")
        save_ggplot(paste0("plots/clim/fig_2_temp_stns_", tolower(tvar), "_", ssp, ".jpg"), size = c(10, 8))
        
    }
}


# Perform bias correction for mean temperature ---------------------------------


# Functions of Hempel from Vicedo-Cabrera et al. (2018).
source("scripts/funs/fhempel_main.R")
source("scripts/funs/fhempel_corr.R")

# Years for bias correction.
ref_years <- 1990:2019

# Correct each SSP.
for (ssp in sce) {
    
    # Load data.
    clim <- qs::qread(file.path(path_clim, paste0("daily_1980_2100_", ssp, "_stns_centrois_byrss.qs")))
    
    # Rearrange RSS because Daymet has 1:18.
    clim_13 <- copy(clim[RSS == "RSS613", ])[, RSS := "RSS13"]
    clim_17 <- copy(clim[RSS == "RSS1018", ])[, RSS := "RSS17"]
    clim_18 <- copy(clim[RSS == "RSS1018", ])[, RSS := "RSS18"]
    clim[RSS == "RSS613", RSS := "RSS6"]
    clim[RSS == "RSS1018", RSS := "RSS10"]
    clim <- rbind(clim, clim_13, clim_17, clim_18)
    clim[, RSS := as.integer(gsub("RSS", "", RSS))]
    
    # Merge Daymet data.
    clim <- rbind(clim,
        daymet[, .(RSS, DATE, YEAR, SSP = "obs", GCM = "none", T_MIN = NA, T_MEAN, T_MAX = NA, PRECIP = NA)]
    )

    # Extract unique RSS and GCM.
    gcms <- unique(clim$GCM)
    gcms <- gcms[gcms != "none"]
    hrs <- sort(unique(clim$RSS))
    
    # Loop on all RSS and GCM 
    for (hr in hrs) {
        for (gcm in gcms) {
            
            # Message.
            message("Performing bias correction for RSS ", hr, ", ", gcm, " and ", ssp, ".")
            
            # Compare original dist.
            # clim_sub <- clim[RSS == hr & YEAR %in% ref_years & GCM %in% c(gcm, "none"), ]
            # p1 <- ggplot(clim_sub, aes(x = T_MEAN, fill = SSP, y = after_stat(density))) +
            #     geom_density(alpha = 0.3) +
            #     labs(subtitle = "Original", fill = "") +
            #     jtheme(expand_xy = "x_only")
            
            # Extract observed and projected in 
            hist_obs <- as.data.frame(clim[RSS == hr & GCM == "none" & YEAR %in% ref_years, .(DATE, T_MEAN)])
            hist_proj <- as.data.frame(clim[RSS == hr & GCM == gcm & YEAR %in% ref_years, .(DATE, T_MEAN)])
            
            # Compute Hempel bias correction.
            hemp_fct <- fhempel_main(hist_obs, hist_proj, output = "correction")
            
            # Apply bias correction.
            clim[
                RSS == hr & GCM == gcm,
                T_MEAN := fhempel_corr(
                    series     = as.data.frame(clim[RSS == hr & GCM == gcm, .(DATE, T_MEAN)]), 
                    correction = hemp_fct
                )$T_MEAN
            ]
            
            # Compare final dist.
            # clim_sub <- clim[RSS == hr & YEAR %in% ref_years & GCM %in% c(gcm, "none"), ]
            # p2 <- ggplot(clim_sub, aes(x = T_MEAN, fill = SSP, y = after_stat(density))) +
            #     geom_density(alpha = 0.3) +
            #     labs(subtitle = "Bias-corrected", fill = "NN") +
            #     jtheme(expand_xy = "x_only")
            # jarrange(list(p1, p2), title = paste0("RSS ", hr, " (GCM=", gcm, ", SSP=", ssp, ")"))
            
        }
    }
    
    # Plot bias-corrected values.
    clim_an <- clim[, .(T_MEAN = mean(T_MEAN)), by = c("YEAR", "RSS", "GCM", "SSP")]
    clim_an_all <- clim[, .(T_MEAN = mean(T_MEAN)), by = c("YEAR", "RSS", "SSP")]

    # Plot time series with all GCMs.
    ggplot(clim_an, aes(x = YEAR, y = T_MEAN, col = SSP, lty = GCM)) +
    geom_line(alpha = 0.1, show.legend = FALSE) +
    geom_line(data = clim_an_all[SSP == ssp, ], aes(x = YEAR, y = T_MEAN, col = SSP), lty = 1, lwd = 1) + 
    geom_line(data = clim_an_all[SSP == "obs", ], aes(x = YEAR, y = T_MEAN, col = "obs"), lty = 1, lwd = 1) + 
    scale_linetype_manual(values = rep(1L, 27)) + 
    labs(x = "Year", y = "Mean temperature (ºC)", title = ssp, col = "") +
    scale_x_continuous(breaks = c(1980, 2010, 2040, 2070), expand = expansion(0, 0)) + 
    scale_color_manual(breaks = c("obs", ssp), values = ul(pal_rbow[c("black", "red")])) + 
    facet_wrap("RSS") + 
    jtheme(tight_facets = TRUE)
    save_ggplot(paste0("plots/clim/fig_3_tmean_", ssp, "_bc.jpg"), size = c(10, 7))
    
    # Get rid of observed Daymet.
    clim <- clim[GCM != "none", ]
    
    # Export bias corrected values.
    qs::qsave(clim, file.path(path_clim, paste0("daily_1980_2100_", ssp, "_stns_centrois_byrss_tmeanbcdaymet.qs")))
    
}


# Plot Tmean bias-corrected values ---------------------------------------------


# Import all data of SSP245 and SSP585.
clim <- do.call(rbind, lapply(
    X   = c("ssp245", "ssp585"), 
    FUN = function(ssp) qs::qread(paste0(path_clim, "/daily_1980_2100_", ssp, "_stns_centrois_byrss_tmeanbcdaymet.qs"))
))

# Add Daymet observations.
clim <- rbind(clim,
    daymet[, .(RSS, DATE, YEAR, SSP = "obs", GCM = "none", T_MIN = NA, T_MEAN, T_MAX = NA, PRECIP = NA)]
)

# Compute mean annual temperature evolution over Quebec.
clim[, MONTH := as.integer(format(DATE, "%m"))]
clim_summ <- clim[RSS %in% c(rss_sel) & MONTH %in% months_sel, mean(T_MEAN), by = c("SSP", "YEAR", "GCM")][, .(
    MEDIAN = median(V1), HIGH = quantile(V1, 0.90), LOW = quantile(V1, 0.10)), by = c("SSP", "YEAR")]
#clim_summ[YEAR <= 2015 & SSP != "obs", SSP := "hist"]
clim_summ[SSP == "obs", SSP := "Observed"]
clim_summ[SSP == "ssp245", SSP := "SSP2-4.5"]
clim_summ[SSP == "ssp585", SSP := "SSP5-8.5"]

# Plot.
ggplot(clim_summ[YEAR >= 1990 & YEAR <= 2070L], aes(x = YEAR, y = MEDIAN, col = SSP, fill = SSP)) + 
#geom_rect(aes(xmin = 1990, xmax = 2020, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
#geom_rect(aes(xmin = 2040, xmax = 2070, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
geom_ribbon(aes(ymax = HIGH, ymin = LOW), alpha = 0.3, col = NA, show.legend = FALSE) +
geom_line() +
geom_line(data = clim_summ[YEAR >= 1990 & YEAR <= 2100 & SSP == "Observed"], aes(x = YEAR, y = MEDIAN, col = SSP)) +
scale_x_continuous(breaks = c(1980 + 0:12 * 10), expand = expansion(0, 3)) + 
scale_color_manual(values = pal[c(1, 4, 6)] ) + 
scale_fill_manual(values = pal[c(1, 4, 6)]) + 
labs(x = "Years", y = "Mean summer temperature over Quebec (ºC)", col = "") + 
ggtitle("a) Summer temperature evolution") + 
jtheme(legend_nrow = 1,  borders = "normal", title_hjust = 0, show_grid = TRUE) + 
guides(fill = guide_legend(override.aes = list(fill = pal[c(1, 4, 6)])))

# Save plot.
save_ggplot("plots/clim/fig_4_tmean_quebc_hist_proj.jpg", size = "rect")
