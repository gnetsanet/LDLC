#' A percentile function
#'
#' This function creates a percentile table given a data frame.
#' @param df dataframe
#' @keywords percentile quantile table
#' @export
#' @examples
#' create.perc.table()

create.perc.table<-function(df) {
    percentile_tab<-NULL # Initialize would be matrix that will hold percentile data
    for (i in 7:12) { 
      percentile_tab<-cbind(percentile_tab,as.matrix(quantile(df[,i], c(0.05,0.10,0.25,0.5,0.75,0.9,0.95))))
    }
    colnames(percentile_tab)<-factor(colnames(df)[7:12])
    formattable(as.data.frame(percentile_tab))
}