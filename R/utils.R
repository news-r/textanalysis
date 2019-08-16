globalVariables(c("."))

call_julia <- function(func, ...) {
  assert_that(has_ta(func))
  julia_call(func, ...)
}

.build_type <- function(x){
  if(x == "ngram_document")
    return("NGramDocument")
  
  x <- strsplit(x, "_")[[1]]
  x <- tools::toTitleCase(x)
  paste0(x, collapse = "")
}

warning_in_place <- function(what){
  cat(
    crayon::yellow(cli::symbol$warning),
    " This function changes `", what, "`",
    crayon::yellow(" in place"), "!\n",
    sep = ""
  )
}

.clean_language <- function(lang){
  lang %>% 
    as.character() %>% 
    gsub("Languages\\.", "", .) %>% 
    gsub("\\(\\)", "", .)
}

.doc_by_type <- function(doc, type){
  if(type == "token")
    token_document(doc)
  else if(type == "ngram")
    ngram_document(doc)
  else
    string_document(doc)
}