
library(rmarkdown)
rmarkdown::render("report.Rmd", params = list(min.year = 1470, max.year = 1880, place = "All", language = "any", subtitle = "My subtitle", idsource = "freewill.txt", update.bibliographica = FALSE, time.window = 10, document.type = "All"))

#rmarkdown::render("report.Rmd", params = list(min.year = 1470, max.year = 1880, subtitle = "My subtitle", idsource = "history_of_england_headings.txt", update.bibliographica = TRUE))
# system("evince report.pdf")
