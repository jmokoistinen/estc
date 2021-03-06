## ----summaryinit, echo=FALSE, message=FALSE, warning=FALSE, echo=FALSE----
library(stringr)
library(bibliographica)
library(estc)
library(dplyr)
#df.orig <- read_bibliographic_metadata("estc.history.csv.gz", verbose = TRUE)
#df <- read.csv(file = "estc.history.csv.gz", sep = "|")
#df <- read.csv(file = "estc-LIBER20151023-backup.csv", sep = "|")
#df$gatherings <- order_gatherings(df$gatherings)

# Read the preprocessed data
df <- readRDS("estc.history.Rds")
timespan <- c(1470, 1830)

df <- df %>%
		filter(publication_year >=  min(timespan) &
		       publication_year <= max(timespan)) %>%
		filter(author_death >=  min(timespan) &
			   author_birth <= max(timespan)) %>%
		dplyr::rename(paper = paper.consumption.km2)

dfo <- df

## ---- echo=TRUE, message=FALSE, warning=FALSE, eval=FALSE----------------
## library(knitr)
## knit("20151023-LIBER.Rmd")

## ---- echo=FALSE, message=FALSE, warning=FALSE---------------------------
library(rmarkdown)
library(knitr)
library(ggplot2)
library(estc)
library(bibliographica)
opts_chunk$set(cache=FALSE, fig.path = "figure_20151023_LIBER/")
library(dplyr)
library(ggthemes)

## ----20151023LIBER-1, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=6, fig.width=8----
library(ggplot2)
library(dplyr)
dfs <- filter(dfo, !is.na(author_birth) & !is.na(author_death))
theme_set(theme_bw(50))
selected.authors <- c("Prynne, William (1600-1669)", "Defoe, Daniel (1661-1731)", "Hume, David (1711-1776)", "Hume, David (1560-1630)")
top <- na.omit(names(rev(sort(table(dfs$author)))))[1:20]
top <- c(top, "Hume, David (1560-1630)")
dfs <- filter(dfs, author %in% top)
p <- top_plot(dfs, "author", ntop = 30, highlight = selected.authors)
p <- p + guides(fill = "none")
p <- p + ylab("Title count")
p <- p + scale_fill_manual(values = c("darkgray", "black"))
p <- p + ggtitle("Top early modern history authors in ESTC")
print(p)

## ----20151023LIBER-2, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=5, fig.width=7.5----
theme_set(theme_bw(20))
dfa <- dfs[, c("author_name", "author", "author_birth", "author_death")]
dfa <- dfa[!duplicated(dfa), ]
dfa <- arrange(dfa, author_birth)
# Order authors by birth year
dfa$author_name <- factor(dfa$author_name, levels = unique(dfa$author_name))
dfa$index <- sample(factor(1:nrow(dfa)))
dfa$fill <- rep("black", nrow(dfa))
dfa$fill[dfa$author %in% selected.authors] <- "red"

p <- ggplot(dfa)
p <- p + geom_segment(aes(y = author_name, yend = author_name, x = author_birth, xend = author_death, color = fill), size = 2) 
p <- p + theme(axis.text.y = element_text(size = 14))
p <- p + xlab("Author life span (year)") + ylab("")
p <- p + guides(color = FALSE)
p <- p + scale_color_manual(values = c("darkgray", "black"))
p <- p + ggtitle("Top early modern author life span")
print(p)

## ----20151023LIBER-3, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=4, fig.width=10----
theme_set(theme_bw(20))
dfs <- dfo %>%
         filter(author %in% selected.authors) %>%
	 group_by(author, publication_decade) %>%
	 tally() %>%
	 arrange(publication_decade)

# Rearrange author levels
dfs$author <- factor(dfs$author, levels = c("Prynne, William (1600-1669)", "Hume, David (1560-1630)", "Defoe, Daniel (1661-1731)", "Hume, David (1711-1776)"))
theme_set(theme_bw(20))

# Barplot version
p2 <- ggplot(dfs, aes(x = publication_decade, y = n, fill = author)) +
       geom_bar(stat = "identity", position = "stack", color = "black") +
       xlab("Publication Year") +
       ylab("Title Count")
p2 <- p2 + scale_fill_manual(values = c("black", "darkgray", "lightgray", "white"))
p2 <- p2 + guides(fill = guide_legend("Author"))
p2 <- p2 + ggtitle("Title count timeline for selected authors")
p2

## ----20151023LIBER-4, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=6, fig.width=7----
theme_set(theme_bw(20))
dfs <- dfo
top <- names(rev(sort(table(dfs$author))))[1:20]
top <- c(top, "Hume, david (1560-1630)")
top <- setdiff(top, "Caesar, Julius (NA-NA)")
topa <- top
dfs <- dfs %>% filter(author %in% topa) %>%
       	   filter(!is.na(author_birth) & !is.na(author_death))
dfs <- dfs %>% group_by(author) %>%
	        summarize(titles = n(),
	                  paper = sum(paper, na.rm = T))
dfs$highlight <- rep("darkgray", nrow(dfs))
dfs$highlight[dfs$author %in% selected.authors] <- "black"
p <- ggplot(dfs, aes(x = titles, y = paper, color = highlight)) 
p <- p + geom_text(aes(label = author), size = 7) 
p <- p + scale_color_manual(values = rev(c("darkgray", "black")))
p <- p + guides(color = "none") 
p <- p + xlim(c(-68, 210)) 
p <- p + scale_x_continuous(breaks = c(-70, 0, 50, 100, 150), labels = c("", "0", "50", "100", "150"), limits = c(-70, 214))       
p <- p + xlab("Title count") 
p <- p + ylab("Paper consumption")
p <- p + ggtitle("Title count versus paper (authors)")
print(p)	 

## ----20151023LIBER-5, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=6, fig.width=10----
theme_set(theme_bw(20))
dfs <- dfo %>% filter(author_gender == "female")
rm.inds <- c(grep("Jean", dfs$author), grep("Pierre", dfs$author), grep("Capriata", dfs$author), grep("Mesurier", dfs$author), grep("Bouhours", dfs$author))
dfs <- dfs[-rm.inds,]


dfs$author <- gsub("whitrowe, joan \\(NA-NA\\)", "whitrowe, joan (1630-1707)", dfs$author)

# Withrow, Joan (1630-1707)
# bouhours is male
#Pier Giovanni Capriata is male
#Le Mesurier, Havilland is male 
p <- top_plot(dfs, "author", ntop = 20)
p <- p + scale_fill_manual(values = "black")
p <- p + guides(fill = "none")
p <- p + ylab("Title count")
p <- p + ggtitle("Top female authors")
p <- p + theme(axis.text.x = element_text(size = 16), axis.text.y = element_text(size = 16), axis.title.x = element_text(size = 25), axis.title.y = element_text(size = 22), title = element_text(size = 22))
print(p)	 

## ----20151023LIBER-video, echo=TRUE, message=FALSE, cache=FALSE, eval = FALSE----
## source("20151023-LIBER-video.R")

## ----20151023LIBER-topplace, echo=FALSE, message=FALSE, cache=FALSE, fig.height=6, fig.width=6----
theme_set(theme_bw(30))
dfs <- df
p <- top_plot(dfs, "publication_place", ntop = 20)
p <- p + scale_fill_manual(values = "black")
p <- p + guides(fill = "none")
p <- p + ylab("Title count (Log10 scale)")
p <- p + scale_y_log10()
p <- p + ggtitle("Top publication places")
print(p)	 

## ----20151023LIBER-8, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=6, fig.width=8----
df <- dfo
theme_set(theme_bw(20))
top <- names(rev(sort(table(df$publication_place))))[1:50]
dfs <- df %>% filter(publication_place %in% top) %>% group_by(publication_place) %>% summarize(titles = n(), paper = sum(paper, na.rm = T))
dfs$country <- get_country(dfs$publication_place)
dfs$usa <- dfs$country == "USA"
p <- ggplot(dfs, aes(x = titles, y = paper))
p <- p + geom_text(aes(label = publication_place, color = usa), size = 5)
#p <- p + xlim(c(1.2, max(na.omit(dfs$titles[!is.infinite(dfs$titles)]))))
p <- p + xlab("Title Count") + ylab("Paper consumption")
p <- p + scale_color_manual(values = c("darkgray", "black"))
p <- p + guides(color = "none")
p <- p + scale_x_log10() + scale_y_log10() 
p <- p + ggtitle("Title count versus paper (places)")
print(p)	 

## ----20150611paris-places4, message=FALSE, fig.height=5, fig.width=10, echo=FALSE----
theme_set(theme_bw(20))
df2 <- dfo %>%
    filter(!is.na(country)) %>%
    group_by(country) %>%
    summarize(paper = sum(paper, na.rm = TRUE),
	      docs = n()) %>%
    arrange(desc(docs)) %>%
    filter(country %in% c("Scotland", "Ireland", "USA"))

p1 <- ggplot(df2, aes(x = country, y = docs)) + geom_bar(stat = "identity") + ggtitle("Title count") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Documents") + xlab("")
p2 <- ggplot(df2, aes(x = country, y = paper)) + geom_bar(stat = "identity") + ggtitle("Paper consumption") + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + ylab("Paper") + xlab("")
library(gridExtra)
grid.arrange(p1, p2, nrow = 1)

## ----20151023LIBER-docsizecomp1, echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=5----
dfs <- df %>% select(gatherings, country) %>% group_by(gatherings, country) %>% tally() %>% filter(country %in% c("Scotland", "Ireland", "USA"))
p <- ggplot(dfs, aes(x = gatherings, y = country, size = n)) +
     geom_point()
print(p)

## ----20151023LIBER-docsizecomp2, echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=5----
dfs <- df %>% select(pagecount, country) %>% filter(country %in% c("Scotland", "Ireland", "USA"))
p <- ggplot(dfs, aes(x = pagecount)) +
     geom_histogram() + facet_grid(country ~ .) + scale_x_log10()
print(p)

## ----20151023LIBER-docsizecomp3, echo=FALSE, message=FALSE, warning=FALSE, fig.width=10, fig.height=10----
dfs <- df %>% select(pagecount, gatherings, country) %>% filter(country %in% c("Scotland", "Ireland", "USA")) %>% group_by(pagecount, gatherings, country) %>% tally()
p <- ggplot(dfs, aes(x = pagecount, y = gatherings, size = n)) +
     geom_point() + facet_grid(country ~ .) + scale_x_log10()
print(p)

## ----20151023LIBER-14, echo=FALSE, message=FALSE, warning=FALSE, echo=FALSE, fig.width=8, fig.height=4----
# Compare to overall publication stats
# 1470 - 1790
library(gdata)
pubstat <- read.xls("Simons-ESTC-1477-1800.xlsx")

# Remove duplicated titles
tmp <- df[, c("publication_year", "title")]
tmp <- tmp[!duplicated(tmp), ]
pubyears.history <- table(tmp$publication_year)

years <- 1470:1799
dff <- data.frame(list(year = years, 
              ESTC.yearly = pubstat$ESTC.yearly[match(years, pubstat$Year)],
	      ESTC.history = as.numeric(pubyears.history[match(years, names(pubyears.history))])))
dff$decade <- round(dff$year, -1)
library(reshape)
dfm <- melt(dff, id = c("year", "decade"))
dfi <- aggregate(dfm[, "value"], by = dfm[, c("variable", "decade")], sum)
dfi$documents.total <- dfi$x
dfi$documents.annual <- dfi$x/10
theme_set(theme_bw(20))
p <- ggplot(dfi, aes(x = decade, y = x, shape = variable))
p <- p + geom_point(size = 4) + geom_smooth(size = 1, color = "black")
p <- p + xlab("Year") + ylab("Publications per decade")
p <- p + ggtitle("History versus all publishing 1470-1800")
p <- p + scale_color_manual(values=c("#CC6666", "#9999CC"))
p <- p + guides(color = "none", shape = "none")
p2 <- p
print(p2)

## ----20151023LIBER-10a, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=4, fig.width=8----
library(sorvi)
dfs <- dfo %>% filter(publication_year >= 1470) %>% 
         group_by(publication_year) %>%
         summarize(titles = n(), paper = sum(paper, na.rm = T))
#p1 <- regression_plot(titles ~ publication_year, data = dfs)
p1 <- ggplot(dfs, aes(y = titles, x = publication_year))
p1 <- p1 + geom_smooth(color = "black") + geom_point()
p1 <- p1 + xlab("Publication Year") + ylab("Title Count") 
p1 <- p1 + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))
p1 <- p1 + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
p1 <- p1 + guides(fill = "none", alpha = "none")
p1 <- p1 + ggtitle("History publications 1470-1800: title count")
print(p1)

## ----20151023LIBER-10b, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=4, fig.width=8.2----
#p2 <- regression_plot(paper ~ publication_year, data = dfs) +
p2 <- ggplot(dfs, aes(y = paper, x = publication_year))
p2 <- p2 + geom_smooth(color = "black") + geom_point() +
       xlab("Publication Year") +
       ylab("Paper Consumption")
       
p2 <- p2 + theme(axis.text.x = element_text(size = 20), axis.text.y = element_text(size = 20))
p2 <- p2 + theme(axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))
p2 <- p2 + guides(fill = "none", alpha = "none")
p2 <- p2 + ggtitle("History publications 1470-1800: paper consumption")
print(p2)

## ----20151023LIBER-11, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=5, fig.width=9----
theme_set(theme_bw(25))
library(sorvi)
dfs <- df %>% filter(publication_year >= 1470) %>% 
         group_by(publication_year) %>%
         summarize(paper.per.doc = mean(paper, na.rm = T))

p <- ggplot(dfs, aes(y = paper.per.doc, x = publication_year)) +
       geom_line() + geom_point() +
       xlab("Publication Year") + ylab("Paper per document")
p <- p + ggtitle("Average paper consumption per document")
print(p)

## ----20151023LIBER-12, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=6, fig.width=12----
df2 <- df
df2$document.type <- rep(NA, nrow(df2))
df2$document.type[df2$pagecount > 32] <- "book"
df2$document.type[df2$pagecount <= 32] <- "pamphlet"
df2 <- df2[which(!is.na(df2$document.type)),]
df2$document.type <- factor(df2$document.type)
df2 <- df2 %>% group_by(publication_decade, document.type) %>% summarize(paper = sum(paper, na.rm = TRUE), n = n()) 
p <- ggplot(df2, aes(x = publication_decade, y = paper, group = document.type))
p <- p + geom_point(aes(shape = document.type), size = 5)
p <- p + geom_smooth(method = "loess", color = "black")
p <- p + ggtitle("Paper consumption per document type")
p <- p + xlab("Year")
p <- p + ylab("Paper consumption")
p <- p + scale_color_manual(values=c("black", "darkgray"))
p <- p + guides(shape = "none")
p <- p + ggtitle("Books versus pamphlets: paper consumption 1470-1800")
print(p)

## ----20151023LIBER-13, echo=FALSE, message=FALSE, warning=FALSE, echo=FALSE, fig.width=11, fig.height=6----
df2 <- df %>% group_by(publication_decade, gatherings) %>% summarize(paper = sum(paper, na.rm = TRUE), n = n()) 
df2 <- filter(df2, gatherings %in% setdiff(names(which(table(df2$gatherings) >= 15)), "NA"))
p <- ggplot(df2, aes(y = paper, x = publication_decade, shape = gatherings, linetype = gatherings))
p <- p + geom_point(size = 4)
p <- p + geom_smooth(method = "loess", size = 1, color = "black")
p <- p + ggtitle("Paper consumption in time by gatherings")
p <- p + xlab("Year")
p <- p + ylab("Paper consumption")
p <- p + guides(linetype = guide_legend(keywidth = 5), shape = guide_legend(keywidth = 5))
p <- p + ggtitle("Paper consumption for different document formats")
print(p)

## ----20151023LIBER-3b, echo=FALSE, message=FALSE, warning=FALSE, cache=FALSE, fig.height=7, fig.width=7----
library(tidyr)
dfs <- dfo

# 2long/2fo/2small -> folio; 8vo -> octavo
dfs$gatherings <- map(dfs$gatherings, gatherings_table(),
	       	  		from = "Standard", to = "Name")
dfs <- dfs %>% filter(author %in% topa &
	        gatherings %in% c("folio", "octavo")) %>%
	 group_by(author, gatherings) %>%
	 tally() %>%
	 spread(gatherings, n)
dfs$highlight <- dfs$author %in% c(selected.authors, "Burke, Edmund (1729-1797)")
dfs[is.na(dfs)] <- 0
dfs[dfs$author == "Hume, David (1560-1630)","highlight"] <- FALSE
dfs$size <- 2 + 1*as.numeric(dfs$highlight)

theme_set(theme_bw(20))
p <- ggplot(dfs, aes(x = folio, y = octavo)) +
       geom_text(aes(label = author, color = highlight, size = size)) +       
       xlab("Folio") + ylab("Octavo") +
       guides(color = "none", size = "none") + 
       scale_color_manual(values = c("darkgray", "black")) +
       xlim(-21,max(dfs$folio) + 8) +
       scale_size(range = c(4,7))
p <- p + scale_x_continuous(breaks = c(-20, 0, 20, 40), labels = c("", "0", "20", "40"), limits = c(-20, 40))       
p <- p + ggtitle("Octavo vs Folio: top authors")
print(p)

## ----20151023LIBER-Edinburgh, echo=FALSE, warning=FALSE, fig.width=11, fig.height=4----
# Scotland publishing summaries
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(estc)
library(bibliographica)
library(dplyr)

# Complete data
# Pick Scotland documents only
selected_place <- "Edinburgh"
sel.country <- "Scotland"
df <- filter(dfo, country == sel.country)
# Use 5 year intervals
df <- filter(dfo, country == sel.country)
timeinterval <- 5
df$timeunit <- round(df$publication_year/timeinterval)*timeinterval 
df$unity = rep(1, nrow(df))
publications <- tapply(df$unity, list(df$timeunit, df$publication_place), sum)
publications[is.na(publications)] <- 0 # Set NAs to 0
publications <- publications/timeinterval # Instead of decadal sum, use average annual output 
dfm <- melt(publications) 
names(dfm) <- c("Time", "Place", "Documents")
dfm <- filter(dfm, Place == selected_place)
dfm <- transform(dfm, date = as.character(Time))
dfs <- spread(dfm, Place, Documents)
dfs$date <- as.numeric(as.character(dfs$date))
dfs$varname <- dfs[[selected_place]]

# Custom highlight for specific time intervals
rect_left <- c(min(na.omit(dfs$date)),
               1642, 1651+1, 
               1660, 1660+1,
               1688, 1689+1,
               1706, 1707+1,
               1776, 1776+1,
               max(na.omit(dfs$date)))
  rectangles <- data.frame(
    xmin = rect_left[-length(rect_left)],
    xmax = rect_left[-1],
    ymin = min(dfs$varname),
    ymax = max(dfs$varname))
  rectangles$shade <- rep(c("White", "Highlight"), length = nrow(rectangles))


# Draw Figure
theme_set(theme_bw(20))
p <- ggplot()
p <- p + geom_rect(data = rectangles, 
	   aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=shade), alpha=0.8) + 
         scale_fill_manual(values = c("gray", "white")) +
         guides(fill = "none") 
p <- p + geom_line(data = dfs, aes(x = date, y = varname), col = "black")
p <- p + geom_point(data = dfs, aes(x = date, y = varname), col = "black")
p <- p + scale_x_continuous(breaks=seq(min(dfs$date), max(dfs$date), 20))
p <- p + ggtitle(paste("Publishing activity in ", selected_place))
p <- p + ylab("Documents / Year")
p <- p + xlab("Year")
p <- p + ggtitle("History publishing in Edinburgh 1470-1800")
print(p)

## ----sessioninfo, message=FALSE, warning=FALSE---------------------------
sessionInfo()

