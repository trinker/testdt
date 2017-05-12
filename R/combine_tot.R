#' Combine y by x
#'
#' Combine x by x
#'
#' @param x A 2 col dataframe
#' @export
#' @examples
#' x <- data.frame(a= c('a', 'a', 's', 'f', 'd', 'a', 's', 's'), b = state.name[1:8])
#' combine_tot(x)
#'
#' #DESIRED OUTPUT
#' ##    a                    b
#' ## 1: a       Alabama Alaska
#' ## 2: s              Arizona
#' ## 3: f             Arkansas
#' ## 4: d           California
#' ## 5: a             Colorado
#' ## 6: s Connecticut Delaware
combine_tot <- function(x){
    nms <- colnames(x)
    colnames(x) <- c('person', 'texts')
    x <- data.table::data.table(x)

    exp <- parse(text='list(text = paste(texts, collapse = " "))')[[1]]
    out <- x[, eval(exp),
        by = list(person, 'new' = data.table::rleid(person))][,
        'new' := NULL][]
    data.table::setnames(out, nms)
    out
}
