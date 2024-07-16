# 2_pop_data.R


# Step 2 : Compute population time series.


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


# Load globals.
source("scripts/funs/globals.R")

# Path.
pop_path <- "/Volumes/ExtDataPhD/pop"


# Imports and merge StatCan data -----------------------------------------------


# # Import all StatCan data.
# pop_statcan <- lapply(c("FA", "HG", "LG", "M1", "M2", "M3", "M4", "M5", "SA"), function(sce) {
# 
#     # Data from Statistics Canada.
#     statcan <- openxlsx::read.xlsx(
#         xlsxFile = paste0(pop_path, "/statcan_hr_2018_2100/Request2021_Scenario", sce, "_2018-2100_Final.xlsx"),
#         sheet    = "Population",
#         startRow = 9L
#     )
# 
#     # Convert to DT.
#     statcan <- setDT(statcan)
# 
#     # Compute 0-19, 20-64 and 65+.
#     statcan[, pop_0_19 := rowSums(statcan[, as.character(0:19), with = FALSE])]
#     statcan[, pop_20_64 := rowSums(statcan[, as.character(20:64), with = FALSE])]
#     statcan[, pop_65p := rowSums(statcan[, c(as.character(65:99), "100+"), with = FALSE])]
#     statcan[, pop_75p := rowSums(statcan[, c(as.character(75:99), "100+"), with = FALSE])]
#     statcan[, pop_tot := pop_0_19 + pop_20_64 + pop_65p]
# 
#     # Subset Quebec data.
#     statcan <- statcan[Code > 2400 & Code < 2420, ]
# 
#     # Select columns.
#     statcan <- statcan[, .(
#         RSS      = as.integer(Code - 2400),
#         YEAR     = `Année`,
#         SEX      = Sexe,
#         pop_tot, pop_0_19, pop_20_64, pop_65p, pop_75p
#     )]
# 
#     # Melt.
#     statcan <- statcan |> melt(id.vars = c("RSS", "YEAR", "SEX"), value.name = "POP", variable.name = "AGE")
#     statcan[AGE == "pop_tot", AGE := "ALL"]
#     statcan[AGE == "pop_0_19", AGE := "0_19"]
#     statcan[AGE == "pop_20_64", AGE := "20_64"]
#     statcan[AGE == "pop_65p", AGE := "65P"]
#     statcan[AGE == "pop_75p", AGE := "75P"]
# 
#     # Add scenario.
#     statcan[, SCENARIO := sce]
# 
#     # Return file.
#     return(statcan)
# 
# })
# 
# # Merge.
# pop_statcan <- rbindlist(pop_statcan)
# 
# # Export.
# fwrite(pop_statcan, paste0(pop_path, "/statcan_hr_2018_2100/qc/pop_qc_2018_2100.csv"))

# Import.
statcan <- fread(paste0(pop_path, "/statcan_hr_2018_2100/qc/pop_qc_2018_2100.csv"))

# Update SSP.
statcan[SCENARIO == "FA", SSP := "SSP1"]
statcan[SCENARIO == "M3", SSP := "SSP2"]
statcan[SCENARIO == "HG", SSP := "SSP5"]

# Keep all age, sex and SSP.
statcan <- statcan[!is.na(SSP) & AGE == "ALL" & SEX == 3, -c("AGE", "SEX", "SCENARIO")]


# Import data from MSSS --------------------------------------------------------


# Extrapolated during 1980 to 1996.
msss_ext <- fread("../data_socioenv/data/pop/pop_yearly_data_byrss_1980_2022.csv")

# Values from 1996 to 2041.
msss_fut  <- fread("/Volumes/ExtDataPhD/pop/msss_rss_1996_2041/pop_msss_rss_1996_2041.csv")

# Merge.
msss <- rbind(
    msss_ext[YEAR >= 1980 & YEAR <= 1995L & AGE == "ALL" & SEX == "T", .(POP = sum(POP), SSP = "MSSS"), by = c("RSS", "YEAR")], 
    msss_fut[YEAR >= 1996L & AGE == "ALL" & SEX == "T", .(POP = sum(POP), SSP = "MSSS"), by = c("RSS", "YEAR")]
)


# Compare datasets across Quebec -----------------------------------------------


# Aggregate MSSS and StatCan data. 
pop_qc <- rbind(msss, statcan)[, .(POP = sum(POP)), by = c("SSP", "YEAR")]

# Create palette for the different scenarios.
pal <- ul(pal_rbow[c("black", "green1", "blue1", "red")])

# Plot results.
ggplot(pop_qc, aes(x = YEAR, y = POP/10^6, col = SSP)) +
geom_rect(aes(xmin = 1990, xmax = 2020, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
geom_rect(aes(xmin = 2040, xmax = 2070, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
geom_line(lwd = 1, alpha = 0.8) +
labs(y = "Quebec total population (M)", x = "Years", color = "") + 
scale_x_continuous(breaks = c(1980 + 0:12 * 10), expand = expansion(0, 3)) + 
scale_color_manual(breaks = c("MSSS", "SSP1", "SSP2", "SSP5"), values = pal) + 
jtheme(legend_nrow = 1,  borders = "normal", title_hjust = 0)

# Save plot.
save_ggplot("plots/pop/fig_1_pop_scenarios_inital.jpg", size = "rect")


# Compute final observed and future --------------------------------------------


# Merge data.
pop <- rbind(statcan[SSP %nin% "SSP1", ], msss)

# Fix years.
years_tofix <- c(2018:2020)
pop_smoth <- pop[YEAR %in% years_tofix, .(POP = mean(POP)), by = c("YEAR", "RSS")]
pop_smoth_msss <- copy(pop_smoth[, SSP := "MSSS"])
pop_smoth_ssp2 <- copy(pop_smoth[, SSP := "SSP2"])
pop_smoth_ssp5 <- copy(pop_smoth[, SSP := "SSP5"])

# Merge. 
pop <- rbind(pop[YEAR %nin% years_tofix, ], pop_smoth_msss, pop_smoth_ssp2, pop_smoth_ssp5)

# Remove observed StatCan data and projected for MSSS.
pop <- pop[!(SSP == "MSSS" & YEAR >= (max(years_tofix) + 1L)), ]
pop <- pop[!(SSP == "SSP2" & YEAR < max(years_tofix)), ]
pop <- pop[!(SSP == "SSP5" & YEAR < max(years_tofix)), ]

# Fix SSP for observed.
pop[SSP == "MSSS", SSP := "Observed"]


# Plot final values ------------------------------------------------------------


# Aggregate. 
pop_qc <- pop[, .(POP = sum(POP)), by = c("SSP", "YEAR")]

# Create palette for the different scenarios.
pal <- ul(pal_rbow[c("black", "blue1", "red")])

# Plot results.
ggplot(pop_qc[YEAR >= min(years_h) & YEAR <= max(years_f+1L)], aes(x = YEAR, y = POP/10^6, col = SSP)) +
#geom_rect(aes(xmin = 1990, xmax = 2020, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
#geom_rect(aes(xmin = 2040, xmax = 2070, ymin = -Inf, ymax = Inf), fill = "grey90", color = NA, alpha = 0.2) + 
geom_line(lwd = 1, alpha = 0.8) +
labs(y = "Quebec's population (M)", x = "Years", color = "") + 
scale_x_continuous(breaks = c(1980 + 0:12 * 10), expand = expansion(0, 3)) + 
scale_color_manual(breaks = c("Observed", "SSP2", "SSP5"), values = pal) + 
ggtitle("b) Demographic evolution") + 
jtheme(legend_nrow = 1,  borders = "normal", title_hjust = 0, show_grid = TRUE)

# Save plot.
save_ggplot("plots/pop/fig_2_pop_scenarios_final.jpg", size = "rect")


# Create baseline reference scenario -------------------------------------------


# Copy pop historical to SSP2 and SSP5.
pop_hist <- pop[YEAR %in% c(1980:2019)]
pop_hist_ssp2 <- copy(pop_hist)[, SSP := "SSP2"]
pop_hist_ssp5 <- copy(pop_hist)[, SSP := "SSP5"]

# Recreate pop.
pop <- rbind(pop[SSP %nin% c("Observed"), ], pop_hist_ssp2, pop_hist_ssp5)
pop <- pop[order(SSP, YEAR, RSS), ]

# Compute fixed population.
pop_fixed <- copy(pop)
pop_mean <- pop[YEAR %in% c(1990:2019), .(POP = mean(POP)), by = c("SSP", "RSS")]

# Replace pop in pop_fixed.
for (ssp in c("SSP2", "SSP5"))
    for (rss in 1:18)
        pop_fixed[RSS == rss & SSP == ssp, POP := ul(pop_mean[RSS == rss & SSP == ssp, POP])]

# Merge both dataset.
pop <- rbind(copy(pop)[, DEMO := "ssp"], pop_fixed[, DEMO := "fixed"])


# Export final population ------------------------------------------------------


# Export.
data.table::fwrite(pop, "data/pop/pop_1980_2100_byrss.csv", sep = ";")
