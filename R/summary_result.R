#' Summary Results
#'
#' @param data_input Data frame or tibble input for analysis.
#' @param num_var Numerical variable to get statistics.
#'
#' @return Summary of the data input by numerical variable.
#' @export
#'
#' @examples
create_num_var_table <- function(data_input, num_var)
  {
    col <- data_input[,get(num_var)]
    if (length(col)>5000) col_norm <- sample(col,5000) else col_norm <- col
    norm_test <- stats::shapiro.test(col_norm)
    statistic <- c("N","Nulos","Minimo","P5 (5%)","Q1 (25%)","Media Aritmetica","Mediana",
                   "Trimmed mean (10%)","Q3 (75%)","P95 (95%)", "RIQ","MAD","Sd","As","K","CV",
                   "Shapiro statistic", "Shapiro p-valor")
    value <- c(round(length(col),3), #base
               round(sum(is.na(col))), #base
               round(min(col,na.rm=TRUE),3), #base
               round(stats::quantile(col, 0.05,na.rm=TRUE),3),
               round(stats::quantile(col, 0.25,na.rm=TRUE),3),
               round(mean(col,na.rm=TRUE),3),  #base
               round(stats::median(col,na.rm=TRUE),3),
               round(mean(col,trim = 0.10,na.rm=TRUE),3), #base
               round(stats::quantile(col, 0.75,na.rm=TRUE),3),
               round(stats::quantile(col, 0.95,na.rm=TRUE),3),
               round(stats::IQR(col,na.rm=TRUE),3),
               round(stats::mad(col,na.rm=TRUE),3),
               round(stats::sd(col,na.rm=TRUE),3),
               round(psych::skew(col,na.rm=TRUE),3),
               round(psych::kurtosi(col,na.rm=TRUE),3),
               round((stats::sd(col,na.rm=TRUE)/mean(col,na.rm=TRUE))*100,3),
               norm_test$statistic, norm_test$p.value)
    DT::data.table(statistic, value)
}
