
get_publications_yearly <- function(catalog_data) {
  
  catalog_years <- catalog_data$publication_year
  years_range <- 1450:1850
  years_titles <- vector("integer", length(years_range))
  
  i <- 1
  for (year in years_range) {
    years_titles[i] <- length(which(catalog_years == year))
    i <- i + 1 
  }
  
  publications_yearly <- data.frame(years = years_range,
                                            titles = years_titles)

  return(publications_yearly)
}


get_hits_yearly <- function(catalog_data, years, keyword) {

  years_range <- years[1]:years[2]
  yearly_hits <- vector("integer", length(years_range))
  keyword <- tolower(keyword)
    
  items_considered <- subset(catalog_data,
                             publication_year >= years[1] &
                               publication_year <= years[2])

  items_hit <- subset(items_considered,
                      grepl(keyword,
                            tolower(items_considered$title)))

  i <- 1
  for (year in years_range) {
    yearly_hits[i] <- length(which(items_hit$publication_year == year))
    i <- i + 1 
  }

  keyword_hits_yearly <- data.frame(years = years_range,
                                    hits = yearly_hits)
  
  return(keyword_hits_yearly)
}
