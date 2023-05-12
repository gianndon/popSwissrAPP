get_rf <- function(){
  URL <- "https://www.yourmoney.ch/ym/details/4961368%2C1526%2C1#Tab0"
  pagecode <- xml2::read_html(x=URL)
  pagecode <- rvest::html_nodes(pagecode, css=".detailPrice")
  pagecode <- rvest::html_text(pagecode)
  pagecode_clean <- gsub(" ", "", pagecode, fixed = TRUE)
  rf <- as.numeric(substr(x=pagecode_clean[1], start=1, stop=nchar(pagecode_clean[1])-1))
  rf <- rf/100
  return(rf)
}
