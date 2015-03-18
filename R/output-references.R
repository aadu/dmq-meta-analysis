#' Saves refs to file
#'
#' @import RCurl
#' @save_path Location to save
#' @return references.
#' @export
#' @include import-google.R
output_references = function(save_path='references.tex'){
    studies = load_dmq_data()
    data = list('norms', 'cor', 'personality')
    names(data) = data
    .sort = function(x){
        x[ ,c(colnames(x)[grep("[.]", colnames(x), invert=T)],
              sort(colnames(x)[grep("[.]", colnames(x))]))]
    }
    data = lapply(data, function(x)(.sort(
                  merge.data.frame(studies, load_dmq_data(x, by='id')))))
    refs = sort(unique(do.call(c, lapply(data, function(x)(unique(x$refs))))))
    sink(save_path)
    cat(paste0("\\nocitemeta{", refs, "}\n\n"))
    sink()
    return(refs)
}
