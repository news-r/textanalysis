# install.packages("remotes")
# remotes::install_github("news-r/nltk4r")
library(nltk4r)

.get_cat_text <- function(category) {
  files <- reuters_files(category, to_r = TRUE)
  txt <- purrr::map(files, reuters_raw, to_r = TRUE) %>% 
    unlist()
  tibble::tibble(
    text = txt,
    category = category
  )
}

.get_all_cat <- function(categories){
  purrr::map_dfr(categories, .get_cat_text)
}

cats <- c(
  "coffee", "gold", "rice", "soybean", "barley",
  "corn", "rubber", "sugar", "gas", "cotton"
)
reuters <- .get_all_cat(cats)

usethis::use_data(reuters, overwrite = TRUE)
