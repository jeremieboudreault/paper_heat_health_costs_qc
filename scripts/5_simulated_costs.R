# 5_simulated_costs.R


# Step 5: Simulated costs.


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


source("scripts/funs/globals.R")


# Imports to simulate AN -------------------------------------------------------


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


# Import fixed values ----------------------------------------------------------


# Computed heatwaves for SSP245 and SSP585 for Quebec.
hw <- qs::qread("data/hw_proj/hw_stats_proj_1980_2099.qs")

# Unit costs based on Table 1.
cost <- fread("data/cost/unit_health_costs.csv")

# Length of Stays (LoS) based on Table 1.
los <- fread("data/cost/los.csv")

# Other monetary values based on Table 1.
source("scripts/funs/costs.R")


# Simulate costs ---------------------------------------------------------------


# Options.
sim_start <- 1L
sim_end <- 1000L

# Select SSP and DEMO scenarios.
ssp <- ssps[1L] # Need to be run for both SSP.
demo <- demos[1L] # Need to be run for both DEMO.

# Load climatic data.
clim <- qs::qread(file = paste0(
    path_clim, "daily_1980_2100_", ssp, "_stns_centrois_byrss_tmeanbcdaymet.qs"
))[
    i = RSS %in% rss_sel, 
    j = .(RSS, GCM, DATE, YEAR, MONTH = as.integer(format(DATE, "%m")), DOY = as.integer(format(DATE, "%j")), T_MEAN)
][MONTH %in% months_sel & YEAR %in% c(years_h, years_f), -c("MONTH")]

# Fix <DOY> during summer for leap years.
clim[(YEAR %% 4) == 0L, DOY := DOY - 1]

# Extract unique GCM and years.
gcmyear <- unique(clim[, .(YEAR, GCM)])
ngcmyear <- nrow(gcmyear)
gcms <- unique(clim$GCM)
ngcms <- length(gcms)

# Extract population of interest.
pop_sub <- pop[SSP == toupper(substr(ssp, 1L, 4L)) & DEMO == demo & RSS %in% rss_sel, ]

# Loop on simulatons.
for (sim in sim_start:sim_end) {

    # Message.
    message("Simul ", sim, "/", sim_end)
    
    # Set seed.
    set.seed(sim + which(ssp == ssps) * 10^6 + which(demo == demos) * 10^9)
    
    # Create canvas of <YEAR>, <GCM> and <SIM_NEW>.
    sim_canva <- data.table(cbind(expand.grid(GCM = gcms, YEAR = c(years_h, years_f))))
    sim_canva[YEAR %in% years_h, SIM_NEW := sample(rep(1:ngcms, length(years_h)))]
    sim_canva[YEAR %in% years_f, SIM_NEW := sample(rep(1:ngcms, length(years_f))) + ngcms]
    
    # Simulate AN.
    an_sim <- dtlapply(hos, function(ho) {
        
        # Add daily mortality/morbidity rate to data.
        data <- merge(clim, ho_summ[HO == ho, ], all.x = TRUE, by = c("RSS", "DOY"))
        
        # Merge population data with climate and health.
        data <- merge(data, pop_sub[, -c("SSP", "DEMO")], all.x = TRUE, by = c("RSS", "YEAR"))
        
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
            
            # Sample values of the coefficient.
            coef <- MASS::mvrnorm(ngcms * 2L, coef, vcov)
            
            # Merge coef with data.
            coef_dt <- merge(sim_canva, data.table(SIM_NEW = 1:(ngcms * 2L), coef), all.x = TRUE, by = "SIM_NEW")
            data_rss <- merge(data[RSS == rss, ], coef_dt, by = c("YEAR", "GCM"), all.x = TRUE, sort = FALSE)
            coef <- as.matrix(data_rss[, .(y1, y2, y3)])
            
            # Compute AN for each combination of YEAR / GCM.
            data[RSS == rss, AN := (1 - exp(-rowSums(bvarcen * coef))) * COUNTRATE * POP]
            
        }
        
        # Compute AN for "total" and "extreme" heat.
        dtlapply(names(map_trange), function(trange) {
            data[get(trange) == TRUE, .(AN = sum(AN), TRANGE = trange, HO = ho), by = c("YEAR", "GCM")]
        })
        
    })
    
    # Overwrite AN < 0.
    an_sim <- expand_dt(an_sim, c("YEAR", "GCM", "TRANGE", "HO"), fill = 0)
    an_sim[AN < 0, AN := 0]
    
    # Merge back <SIM_NEW>.
    an_sim <- merge(an_sim, sim_canva, all.x = TRUE, by = c("YEAR", "GCM"))
    
    # Extract HW for the current SSP/DEMO.
    hw_sim <- hw[SSP == ssp & DEMO == demo & YEAR %in% c(years_h, years_f), -c("SSP", "DEMO")] |>
        merge(sim_canva, by = c("YEAR", "GCM"), all.x = TRUE)
    
    # Simulate costs for each GCM/year.
    cost_sim <- dtlapply(hos, function(ho) {
        data.table(
            HO      = ho,
            SIM_NEW = 1:(ngcms * 2L),
            MIDDLE  = cost[HO == ho, rdist(DIST, MIDDLE, LOW, HIGH, ngcms * 2L)]
        )
    })
    
    # Simulate LoS for each GCM/year.
    los_sim <- dtlapply(hos[-1L], function(ho) {
        data.table(
            HO      = ho,
            SIM_NEW = 1:(ngcms * 2L),
            MIDDLE = los[HO == ho, rdist(DIST, MIDDLE, LOW, HIGH, ngcms * 2L)]
        )
    })

    # Simulate other metric for each GCM/YEAR.
    wage_sim <- cbind(data.table(SIM_NEW = 1:(ngcms * 2L)), data.table(WAGE = rdist("NA", wage, n = ngcms * 2L)))
    emerg_sim <- cbind(data.table(SIM_NEW = 1:(ngcms * 2L)), data.table(EMERG = rdist("NA", emerg, n = ngcms * 2L)))
    mrad_sim <- cbind(data.table(SIM_NEW = 1:(ngcms * 2L)), data.table(MRAD = rdist("SD", mrad, mrad_sd, n = ngcms * 2L)))
    mrad_sim[MRAD < 0, MRAD := 0]
    
    # Compute direct costs associated with AN.
    health_cost <- merge(an_sim, cost_sim, by = c("HO", "SIM_NEW"), all.x = TRUE)
    health_cost[, `:=`(CAT = "COST", COST = AN * MIDDLE)]
    health_cost[HO == "MOR", CAT := "SOC"] # For mortality, change category to "Societal".
    
    # Indirect cost associated with AN.
    health_prod <- merge(an_sim, los_sim, by = c("HO", "SIM_NEW"), all.x = TRUE) |>
        merge(wage_sim, by = c("SIM_NEW"), all.x = TRUE)
    health_prod[, `:=`(CAT = "PROD", COST = AN * MIDDLE * WAGE)]
    
    # Heatwave direct costs.
    hw_cost <- merge(hw_sim, emerg_sim, by = c("SIM_NEW"), all.x = TRUE)
    hw_cost[, `:=`(CAT = "COST", HO = "HW", COST = HW_N * EMERG)]
    
    # Heatwaves intangible costs.
    hw_wb <- merge(hw_sim, mrad_sim, by = c("SIM_NEW"), all.x = TRUE)
    hw_wb[, `:=`(CAT = "SOC", HO = "HW", COST = DAYPOP * MRAD)]
    
    # Merge all costs.
    cost_sim <- rbind(
        health_cost[, -c("AN", "MIDDLE")],
        health_prod[, -c("AN", "MIDDLE", "WAGE")],
        copy(hw_cost)[, `:=`(TRANGE = "heat_all",     HW_N = NULL, DAYPOP = NULL, EMERG = NULL)],
        copy(hw_cost)[, `:=`(TRANGE = "heat_extreme", HW_N = NULL, DAYPOP = NULL, EMERG = NULL)],
        copy(hw_wb)[, `:=`(TRANGE = "heat_all",       HW_N = NULL, DAYPOP = NULL, MRAD = NULL)],
        copy(hw_wb)[, `:=`(TRANGE = "heat_extreme",   HW_N = NULL, DAYPOP = NULL, MRAD = NULL)]
    )
    
    # Add simulation number.
    cost_sim[, SIM := sim]
    cost_sim[, SSP := ssp]
    cost_sim[, POP := demo]
    
    # Cache results.
    qs::qsave(cost_sim, paste0(path_simul, "raw/cost_sim_", ssp, "_pop", demo, "_sim", sim, ".qs"))
    
}


# Process batch simulations ----------------------------------------------------


# Import all simulations.
sim_dt <- dtlapply(list.files(paste0(path_simul, "raw"), full.names = TRUE), qs::qread)
format(object.size(sim_dt), "Gb")

# Add period.
sim_dt[YEAR %in% years_h, PERIOD := "hist"]
sim_dt[YEAR %in% years_f, PERIOD := "future"]

# Update <SIM_NEW>.
sim_dt[YEAR %in% years_f, SIM_NEW := SIM_NEW - 15L]
sim_dt[, SIM_NEW := SIM_NEW + (SIM - 1L) * 15L]

# Aggregate all simulations by <SIM_NEW> (30 years averages).
sim_dt <- sim_dt[, .(COST = sum(COST, na.rm = TRUE)/30), by = c("HO", "TRANGE", "CAT", "SSP", "POP", "PERIOD", "SIM_NEW")]

# Export simulations.
qs::qsave(sim_dt, file.path(path_simul, "cost_sim_all.qs"))

