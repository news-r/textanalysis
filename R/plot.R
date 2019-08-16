#' @export
plot.coom <- function(x, y, ...){
  mn <- min(x)
  mx <- max(x)
  x %>% 
    echarts4r::e_charts() %>% 
    echarts4r::e_correlations(order = "hclust", visual_map = FALSE) %>% 
    echarts4r::e_visual_map(min = mn, max = mx) %>% 
    echarts4r::e_tooltip()
}