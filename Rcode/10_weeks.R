# Distribution of number weeks per months, quarters and years ####

require(data.table)
require(ISOweek)
require(lubridate)

year1 <- 2000L
year2 <- year1 + 400L

# The first Thursday of the year1
date1 <- ISOweek2date(paste(year1, "W01", "4", sep = "-"))
# The last Thursday of the year2
date2 <- ISOweek2date(paste(year2, "W01", "4", sep = "-")) - 7L

dt <- data.table(Thursday = seq(date1, date2, by = 7L))
dt[, ISOweek := ISOweek(Thursday)]
dt[, year := substr(ISOweek, 1, 4)]
dt[, week := substr(ISOweek, 7, 8)]

# Test
dt[, all.equal(year, format(Thursday, format = "%Y"))]

dt[, leap.year := leap_year(Thursday)]

tab <- dt[, .(num_weeks = .N), keyby = year]
tab
tab[, .N, keyby = num_weeks][, P := prop.table(N)][]
tab[num_weeks > 52]


# Calendar month and quarter
dt[, month := as.integer(format(Thursday, format = "%m"))]
dt[, quarter := 1 + (month - 1) %/% 3]
dcast(dt, month ~ quarter)


# Distribution of weeks ####
tab <- dt[, .(weeks = .N), keyby = .(year, leap.year, quarter, month)]
tab <- tab[, .(weeks = sum(weeks), pattern = paste(weeks, collapse = "-")),
           keyby = .(year, leap.year, quarter)]
tab[, pattern := paste(pattern, paste0("(", weeks, ")"))]
tab

tab <- tab[, .(weeks = sum(weeks), pattern = paste(pattern, collapse = " | ")),
           keyby = .(year, leap.year)]
tab
tab[weeks > 52][1:10]

tab_agg1 <- tab[, .N, keyby = .(weeks, pattern)][, P := prop.table(N)][]
tab_agg1

tab_agg2 <- tab[, .N, keyby = .(leap.year, weeks, pattern)][, P := prop.table(N)][]
tab_agg2

tab_agg3 <- dcast(tab, pattern ~ paste("leap =", leap.year))
tab_agg3



# LFS calendar month and quarter
dt[, weeks_in_q := .N, by = .(year, quarter)]
dt[, year_12w1q := any(weeks_in_q == 12), by = year]

dt[, .N, keyby = weeks_in_q]
dt[, .N / 52, keyby = year_12w1q]

dt[!(year_12w1q), LFS_quarter := quarter]
dt[ (year_12w1q), LFS_quarter := 1 + (as.integer(week) - 1) %/% 13]

dcast(dt, quarter ~ LFS_quarter)

dt[quarter != LFS_quarter]
dt[quarter == LFS_quarter, LFS_month := month]
dt[quarter != LFS_quarter, LFS_month := month - 1L]

dcast(dt, month ~ LFS_month)

tab_LFS <- dt[, .(weeks = .N), keyby = .(year, leap.year, LFS_quarter, LFS_month)]
tab_LFS <- tab_LFS[, .(weeks = sum(weeks), pattern = paste(weeks, collapse = "-")),
                   keyby = .(year, leap.year, LFS_quarter)]
tab_LFS[, pattern := paste(pattern, paste0("(", weeks, ")"))]

tab_LFS <- tab_LFS[, .(weeks = sum(weeks), pattern = paste(pattern, collapse = " | ")),
                   keyby = .(year, leap.year)]
tab_LFS
tab_LFS[weeks > 52][1:10]
tab_LFS[, .N, keyby = .(weeks, pattern)][, P := prop.table(N)][]
tab_LFS[, .N, keyby = .(leap.year, weeks, pattern)][, P := prop.table(N)][]


# Save ####

sink(file = "results/distr_weeks.txt")
  cat("\nDistribution of years by week pattern\n\n")
  print(tab_agg1)
  cat("\nDistribution of years by week pattern and leap year\n\n")
  print(tab_agg2)
  cat("\nDistribution of years by week pattern and leap year (table)\n\n")
  print(tab_agg3)
sink()

fwrite(tab, file = "results/weeks_by_year.csv")
