# globals.R


# Globals variables of the project ---------------------------------------------


# Path for climate data.
path_clim <- "/Volumes/ExtDataPhD/clim/espo_g6_r2_stns_centroid_byrss/"

# Path for simulations.
path_simul <- "/Volumes/ExtDataPhD/simul/"

# Define health outcomes.
hos <- c("MOR", "HOS", "EDV", "AMB", "811")

# Shared-Socioeconomic Pathways
ssps <- c("ssp245", "ssp585")
demos <- c("fixed", "ssp")

# Selected months for the analysis.
months_sel <- 5:9

# Selected RSS for the analysis.
rss_sel <- c(1:9, 11:16)

# Number of days (based on selected months).
ndays <- length(seq(as.Date("2020-05-01"), as.Date("2020-09-30"), by = 1L))

# Period of interest for climate change. 
years_h <- 1990:2019 # Centered on 2000.
years_f <- 2040:2069 # Centered on 2050.

# GCMs to remove to avoid the "hot" models problems.
gcms_hot <- c(
    "CanESM5",
    "CNRM-CM6-1",
    "EC-Earth3",
    "IPSL-CM6A-LR",
    "UKESM1-0-LL",
    "NorESM2-MM",
    "EC-Earth3-CC",
    "NESM3",
    "EC-Earth3-Veg",
    "INM-CM4-8",
    "TaiESM1"
)


# Function to convert to currency ----------------------------------------------


to_curk <- function(x, symbol = TRUE, dec = 1L) {
    paste0(format(round(x/10^3, dec), big.mark = " ", nsmall = 0, trim = TRUE), c("", "k$")[symbol + 1])
}

to_curM <- function(x, symbol = TRUE, dec = 1L) {
    paste0(format(round(x/10^6, dec), big.mark = " ", nsmall = 0, trim = TRUE), c("", "M$")[symbol + 1])
}


# Function to add bold for Markdown table --------------------------------------


md_to_bold <- function(x) {
    ul(sapply(x,  function(x) {
        if (nchar(x) == 0) 
            ""
        else 
            paste0("**", x, "**")
    }))
}


# Function to simulate from any distribution -----------------------------------


rdist <- function(dist = "NA", middle, low, high, n = 1L) {
    if (!is.na(dist) & dist == "UNIF") {
        runif(n, low, high)
    } else if (!is.na(dist) & dist == "SD") {
        rnorm(n, middle, sd = low)
    } else if (!is.na(dist) & dist == "GAMMA") {
        rgamma(n, low, scale = high)
    } else if (!is.na(dist) & dist == "TRI") {
        EnvStats::qtri(runif(n), low, high, middle)
    } else if (!is.na(dist) & dist == "CI95") {
        rnorm(n, middle, sd = (high - low)/(2 * 1.96))
    } else {
        EnvStats::qtri(runif(n), min = 0.667 * middle, max = 1.333 * middle, mode = middle)
    }
}


# Function to lapply and bind resulting data.table -----------------------------


dtlapply <- function(X, FUN, ...) {
    do.call(rbind, lapply(X = X, FUN = FUN, ...))
}


# Function to expand a DT based on available values ----------------------------


expand_dt <- function(x, cols, fill = NA) {
    
    # Extract unique values.
    un <- lapply(cols, function(col) sort(unique(x[[col]])))
    names(un) <- cols
    
    # Create a canvas to overide.
    canvas <- as.data.table(expand.grid(un, stringsAsFactors = FALSE))
    
    # Extract all cols.
    cols_all <- colnames(x)
    cols_val <- cols_all[cols_all %nin% cols]
    
    # Merge.
    x <- merge(canvas, x, by = cols, all.x = TRUE)
    
    # Update missing values.
    for (col in cols_val)
        set(x, i = which(is.na(x[[col]])), j = col, value = fill)
    
    # Return table.
    return(x)
    
}


# Mapping between ECCC stations and RSS ----------------------------------------


# Reference stations by RSS.
map_stns_rss <- list(
    `5761`  = "RSS1",
    `26887` = "RSS1",
    `55359` = "RSS2",
    `5889`  = "RSS2",
    `5251`  = "RSS3",
    `26892` = "RSS3",   
    `5426`  = "RSS4",   
    `10732` = "RSS4",   
    `5397`  = "RSS5", 
    `5415`  = "RSS613", 
    `30165` = "RSS613", 
    `4337`  = "RSS7", 
    `49568` = "RSS7", 
    `6081`  = "RSS8", 
    `30172` = "RSS8", 
    `5662`  = "RSS9", 
    `30173` = "RSS9", 
    `48968` = "RSS9", 
    `49491` = "RSS1018", 
    `10791` = "RSS1018", 
    `6053`  = "RSS1018", 
    `6052`  = "RSS1018", 
    `51297` = "RSS11", 
    `54299` = "RSS11", 
    `54918` = "RSS11", 
    `5794`  = "RSS11", 
    `5314`  = "RSS12", 
    `26777` = "RSS12", 
    `5237`  = "RSS14", 
    `5594`  = "RSS15", 
    `5490`  = "RSS16", 
    `30170` = "RSS16", 
    `48374` = "RSS16"
)


# Palette for RSS --------------------------------------------------------------


pal_rss <- list(
    `16` = "#FCAE91",
    `6`  = "#FB6A4A", 
    `13` = "#DE2D26", 
    `14` = "#A50F15", 
    `5`  = "#C7E9C0",
    `7`  = "#A1D99B", 
    `15` = "#74C476",
    `4`  = "#31A354",
    `12` = "#006D2C",
    `3`  = "#BCBDDC",
    `8`  = "#756BB1",
    `2`  = "#54278F",
    `1`  = "#BDD7E7",
    `11` = "#6BAED6", 
    `9`  = "#3182BD", 
    `10` = "#08519C", 
    `18` = "#f1b6da", 
    `17` = "#de77ae"
)


# Extreme heat alert thresholds ------------------------------------------------


# Heatwave threshold for INSPQ (Laurendites 15 is using Tmax 33ºC)
hw_thresh <- data.table(
    RSS          = 1:18,
    T_MAX_THRESH = c(rep(31, 5), 33, rep(31, 6), rep(33, 4), 31, 31),
    T_MIN_THRESH = c(16, rep(18, 4L), 20, 18, 18, 16, 16, 16, 18, rep(20, 4), 16, 16)
)


# Mappings for costs names and category ----------------------------------------


# Cost names.
map_cost_names <- list(
    HOS_COST   = "Costs of HOS",
    EDV_COST   = "Costs of EDV",
    AMB_COST   = "Costs of AMB",
    `811_COST` = "Costs of 811",
    HW_COST    = "Costs of HW teams",
    HOS_PROD   = "Wage loss for HOS",
    EDV_PROD   = "Wage loss for EDV",
    AMB_PROD   = "Wage loss for AMB",
    `811_PROD` = "Wage loss for 811",
    MOR_SOC    = "Valuation of mortality",
    HW_SOC     = "MRAD during HW"
)

# Cost names for pie charts.
map_cost_names_pchart <- list(
    HOS_COST   = "Costs of HOS",
    EDV_COST   = "Costs of EDV",
    AMB_COST   = "Costs of AMB",
    `811_COST` = "Costs of 811",
    HW_COST    = "Costs of HW teams",
    NA1        = "  ",
    HOS_PROD   = "Wage loss for HOS",
    EDV_PROD   = "Wage loss for EDV",
    #AMB_PROD   = "Wage loss for AMB",
    `811_PROD` = "Wage loss for 811",
    MOR_SOC    = "Valuation of mortality",
    HW_SOC     = "MRAD during HW"
)

# Cost category.
map_cost_cat <- list(
    COST       = "Direct",
    PROD       = "Indirect",
    SOC        = "Intangible",
    ALL        = "Total"
)

# Cost category with sub-letters.
map_cost_cat_l <- list(
    COST       = "a) Direct",
    PROD       = "b) Indirect",
    SOC        = "c) Intangible",
    ALL        = "d) Total"
)

# Palette for category costs.
pal_cat <- c("#FFEDC5", "#E9E9E9", "#E1CAE8", "#F08080")


# Mapping for health outcomes (HO) ---------------------------------------------


map_ho <- list(
    MOR   = "Mortality",
    HOS   = "Hospital admission",
    EDV   = "Emergency department visit",
    AMB   = "Ambulance transports",
    `811` = "811-health hotline calls",
    HW    = "Number of heatwaves",
    POP   = "People-days (M) affected by HW"
)


# Available years for each health outcomes -------------------------------------


map_years_ho <- list(
    MOR   = 1996:2019,
    HOS   = 1996:2019,
    EDV   = 2014:2019,
    AMB   = 2014:2019,
    `811` = 2008:2019,
    HW    = 1990:2019
)


# Mapping for heat category ----------------------------------------------------


map_trange <- list(
    heat_all     = "All heat",
    heat_extreme = "Extreme heat"
)

map_trange_l <- list(
    heat_all     = "a) All heat",
    heat_extreme = "b) Extreme heat"
)


# Mapping for SSP-POP combination ----------------------------------------------


# Map scenarios of population
map_pop <- list(
    fixed   = "Historical",
    ssp     = "Projected"
)

# Map scenarios of climate.
map_period <- list(
    hist   = "Historical",
    future = "Projected"
)

# Map for combination of SSP and POP scenarios.
map_ssppop <- list(
    ssp245_popfixed = "SSP2-4.5 (Hist. demo.)",
    ssp245_popssp   = "SSP2-4.5 (Proj. demo.)",
    ssp585_popfixed = "SSP5-8.5 (Hist. demo.)",
    ssp585_popssp   = "SSP5-8.5 (Proj. demo.)"
)

# Map for combination of SSP and POP scenarios.
map_ssppop2 <- list(
    hist            = "Historical",
    ssp245_popfixed = "Proj. climate (SSP2-4.5)",
    ssp245_popssp   = "Proj. climate + demo. (SSP2-4.5)",
    ssp585_popfixed = "Proj. climate (SSP5-8.5)",
    ssp585_popssp   = "Proj. climate + demo. (SSP5-8.5)"
)

# Map for combination of SSP and POP scenarios for pie-chart.
map_ssppop_pchart <- list(
    hist            = "Historical\n",
    ssp245_popfixed = "Proj. climate\n(SSP2-4.5)",
    ssp245_popssp   = "Proj. climate +\ndemo. (SSP2-4.5)",
    ssp585_popfixed = "Proj. climate\n(SSP5-8.5)",
    ssp585_popssp   = "Proj. climate +\ndemo. (SSP5-8.5)"
)
