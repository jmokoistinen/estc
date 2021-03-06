#' @title Harmonize pages specialcases
#' @description Harmonize pages (special cases)
#' @param x A character vector
#' @return Polished vector
#' @export
#' @author Leo Lahti \email{leo.lahti@@iki.fi}
#' @references See citation("estc")
#' @examples # harmonize_pages_specialcases("Caption title 2")
#' @keywords utilities
harmonize_pages_specialcases <- function (x) {

  x <- as.character(x)

  f <- system.file("extdata/harmonize_pages_estc.csv", package = "estc")
  sn <- read_mapping(f, sep = "\t", mode = "table")
  x <- map(x, sn, verbose = FALSE)
  x[grep("Caption title", x)] <- NA

  x
}
