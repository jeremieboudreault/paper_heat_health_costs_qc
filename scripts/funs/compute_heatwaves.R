# compute_heatwaves.R


# A function to comptue heatwaves based on INSPQ thresholds.
# Author @Jeremie Boudreault.


compute_heatwaves <- function(x, verbose = TRUE, check_dates = TRUE) {
    
    # Step 1 : Calculate rolling average temperature values.
    if (verbose) message("1. Calculating rolling average temperature values.")
    for (rss in unique(x$RSS)) {
        for (y in x[RSS == rss, unique(YEAR)]) {
            x[RSS == rss & YEAR == y, T_MAX_3DAY := rjutils::create_lagged_var(T_MAX, DATE, 0, -2, fun = function(x) x[1]* 0.4 + x[2] * 0.4 + x[3] * 0.2, check_dates = check_dates)]
            x[RSS == rss & YEAR == y, T_MIN_3DAY := rjutils::create_lagged_var(T_MIN, DATE, 0, -2, fun = function(x) x[1]* 0.4 + x[2] * 0.4 + x[3] * 0.2, check_dates = check_dates)]
        }
    }
    
    # Bring thresholds.
    x <- merge(x, hw_thresh, by = "RSS", all.x = TRUE)
    
    # Create indicator for heatwaves.
    x[, HW := 0]
    x[T_MAX_3DAY >= T_MAX_THRESH & T_MIN_3DAY >= T_MIN_THRESH, HW := 1L]
    
    # Step 2. Compute start and end dates of heatwaves.
    if (verbose) message("2. Computing heatwaves.")
    hw <- data.table(RSS = 0, YEAR = 9999, START = as.Date("2022-01-01"), END = as.Date("2022-01-01"))
    for (rss in unique(x$RSS)) {
        for (y in x[RSS == rss, unique(YEAR)]) {
            
            # Subset the current year and rss.
            dt <- x[RSS == rss & YEAR == y, ]
            
            # Compute different in HW.
            dt[, diff := c(diff(HW), 0)]
            
            # Start date are when doing from 0 to 1.
            dt[which(diff == 1) + 1, start := 1]
            
            # End dat is when goinf from 1 to 0.
            dt[which(diff == -1), end := 1]
            
            # Merge both start and end date (add 2 days at the end of each heatwave.)
            dt <- cbind(dt[start == 1, .(START = DATE)], dt[end == 1, .(END = DATE + 2)])
            dt[, `:=`(RSS = rss, YEAR = y)]
            
            # Return the events.
            hw <- rbindlist(list(hw, dt), use.names = TRUE)
            
        }
    }
    hw <- hw[-1, ]
    
    # Step 3. Merge heatwaves with similar end and start date.
    if (verbose) message("3. Fixing heatwaves.")
    is <- which((hw[-.N, END] - hw[-1, START]) %in% c(0, 1, 2))
    hw[, REMOVE := FALSE]
    for (i in is) {
        #message("Fixing :")
        #print(hw[c(i, i + 1)])
        if (hw[i, RSS] == hw[i + 1, RSS] & hw[i, YEAR] == hw[i + 1, YEAR] ) {
            hw[i, "END"] <- hw[i + 1, END]
            hw[i, DURATION := as.integer(END - START + 1)]
            hw[i + 1, "REMOVE"] <- TRUE
        }    
    }
    hw <- hw[REMOVE == FALSE, ]
    hw[, REMOVE := NULL]
    
    # Compute duration.
    hw[, DURATION := as.integer(END - START + 1L)]
    
    # Return the heatwave dataset.
    return(hw)
    
}

