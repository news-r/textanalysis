library(nltk4r)

hamlet <- gutenberg_raw("shakespeare-hamlet.txt", to_r = TRUE)

usethis::use_data(hamlet, overwrite = TRUE)