
catalog.estc <- readRDS("shinytest2.Rds")
catalog.estc.years <- catalog.estc$publication_year
# rm(catalog.estc)

years.range <- 1450:1850
years.titles <- vector("integer", length(years.range))

i <- 1
for (year in years.range) {
  # print(year)
  # print(length(which(catalog.estc.years == year)))
  years.titles[i] <- length(which(catalog.estc.years == year))
  i <- i + 1 
}

catalog.estc.amount.by.year <- data.frame(years = years.range,
                                          titles = years.titles)
