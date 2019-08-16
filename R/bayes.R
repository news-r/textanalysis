#' Naive Bayes Classifier
#'
#' Naive Bayes classifier for text classification.
#' 
#' @param labels A vector of labels (factor).
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' classes <- factor(c("A", "B"))
#' model <- init_naive_classifer(classes)
#' }
#' 
#' @name init_naive_classifer
#' @export
init_naive_classifer <- function(labels) UseMethod("init_naive_classifer")

#' @rdname init_naive_classifer
#' @method init_naive_classifer factor
#' @export
init_naive_classifer.factor <- function(labels){
  assert_that(length(labels) > 1)
  labels <- paste0(":", labels, collapse = ", ")
  expr <- paste0("NaiveBayesClassifier([", labels, "])")
  nb <- julia_eval(expr)
  .construct_naive_bayes(nb)
}

#' Train Naive Bayes Classifier
#' 
#' Train Naive Bayes classifier (\code{\link{init_naive_classifer}}).
#' 
#' @param model A naive Bayes classifier as returned by 
#' \code{\link{init_naive_classifer}}.
#' @param labels Bare column name containing labels.
#' @param data A data.frame containing \code{text} and \code{labels}.
#' @param text Text to train model on \code{labels}.
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' classes <- factor(c("financial", "legal"))
#' model <- init_naive_classifer(classes)
#' 
#' train <- tibble::tibble(
#'   text = c("this is financial doc", "this is legal doc"),
#'   labels = factor("financial", "legal")
#' )
#' 
#' train_naive_classifier(model, train, text, labels)
#' }
#' 
#' @name train_naive_classifier
#' @export
train_naive_classifier <- function(model, data, text, labels) UseMethod("train_naive_classifier")

#' @rdname train_naive_classifier
#' @method train_naive_classifier naive_bayes_model
#' @export
train_naive_classifier.naive_bayes_model <- function(model, data, text, labels){
  # assertions
  assert_that(is_missing(text))
  assert_that(is_missing(labels))
  assert_that(is_missing(data))

  warning_in_place("model")

  # prepare data
  text_quo   <- dplyr::enquo(text)
  labels_quo <- dplyr::enquo(labels)

  data <- data %>% 
    dplyr::select(
      text      = !!text_quo,
      label     = !!labels_quo
    )

  julia_assign("naiveModel", model)

  data %>% 
    apply(1, as.list) %>% 
    purrr::map(function(x){
      expr <- paste0('fit!(naiveModel, "', x[["text"]], '", :', x[["label"]], ')')
      julia_eval(expr)
    })
  
  invisible()
}

#' Predict Class
#' 
#' Predict class using naive Bayes classifier.
#' 
#' @inheritParams train_naive_classifier
#' 
#' @examples
#' \dontrun{
#' init_textanalysis()
#' classes <- factor(c("financial", "legal"))
#' model <- init_naive_classifer(classes)
#' 
#' train <- tibble::tibble(
#'   text = c("this is financial doc", "this is legal doc"),
#'   labels = factor("financial", "legal")
#' )
#' 
#' train_naive_classifier(model, train, text, labels)
#' 
#' test <- tibble::tibble(
#'   text = "this should be predicted as a legal document"
#' )
#' predict_class(model, test, text)
#' }
#' 
#' @name predict_class
#' @export
predict_class <- function(model, data, text) UseMethod("predict_class")

#' @rdname predict_class
#' @method predict_class naive_bayes_model
#' @export
predict_class.naive_bayes_model <- function(model, data, text){
  # assertions
  assert_that(is_missing(text))
  assert_that(is_missing(data))

  # prepare data
  text_quo   <- dplyr::enquo(text)

  data <- dplyr::select(data, text = !!text_quo)

  data %>% 
    apply(1, as.list) %>% 
    purrr::map(function(x){
      expr <- paste0('predict(naiveModel, "', x[["text"]], '")')
      julia_eval(expr)
    }) %>% 
    purrr::map_dfr(tibble::as_tibble)
}