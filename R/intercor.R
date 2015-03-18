#' Process dmq intercorrelations
#'
#' @import R2jags
#' @import metafor
#' @import MAc
#' @export
#' @include import-google.R
#' @include formatters.R

dmq_intercorrelations = function(){
  
  .set_intercor_data = function(x, df){
    r = df[[x]]
    r.variance = df[[paste(x, ".v", sep="")]]
    r = r[!is.na(r)]
    r.variance = r.variance[!is.na(r.variance)]
    if(length(r) != length(r.variance))
      warning("Length of r and r.variance is not equal!!!")
    mydata = list(r = as.vector(r), r.variance = as.vector(r.variance))
    return(mydata)
  }
  
  .run_it = function(x, df){
    mydata = .set_intercor_data(x, df)
    fit = jags(data=mydata, parameters.to.save=c("theta", "tau", "bpvalue"),
               model.file="./bugs.txt")
    fit = autojags(fit)
    get_hdi("theta", fit)
    out = list(r = fit$BUGSoutput$mean$theta,
               r.var = fit$BUGSoutput$mean$tau,
               hdi.l = get_hdi("theta", fit)[1],
               hdi.u = get_hdi("theta", fit)[2],
               hdi = paste0("[", fC(get_hdi("theta", fit)[1], 3), ", ",
                           fC(get_hdi("theta", fit)[2], 3), "]"),
               hdi2 = paste0("[", fC2(get_hdi("theta", fit)[1], 3), ", ",
                             fC2(get_hdi("theta", fit)[2], 3), "]"))
    return(out)
  }
  
  .meta_i = function(x, df){
    mydata = .set_intercor_data(x, df)
    out = omni(r, r.variance, data=mydata)
    return(out)
  }
  
  .n_k = function(var, df){
    ss = subset(df, !is.na(df[[var]]))
    paste(nrow(ss), " (", prettyNum(sum(ss$n), big.mark=","), ")", sep="")
  }
  
  .asf = function(x){
    paste(fC(x$r, 3), x$hdi)
  }
  
  .asf2 = function(x){
    paste(fC2(x$r, 3), x$hdi2)
  }
  
  # Load the data
  studies = load_dmq_data()
  df = merge.data.frame(load_dmq_data('cor'), studies, by="id")
  df$c.e.v =   escalc(ri=c.e, ni=n, data=df, measure="COR")[['vi']]
  df$c.s.v = escalc(ri=c.s, ni=n, data=df, measure="COR")[['vi']]
  df$c.cn.v = escalc(ri=c.cn, ni=n, data=df, measure="COR")[['vi']]
  df$e.s.v = escalc(ri=e.s, ni=n, data=df, measure="COR")[['vi']]
  df$e.cn.v = escalc(ri=e.cn, ni=n, data=df, measure="COR")[['vi']]
  df$s.cn.v = escalc(ri=s.cn, ni=n, data=df, measure="COR")[['vi']]
  df = df[,c(colnames(df)[grep("[.]", colnames(df), invert=T)],
             sort(colnames(df)[grep("[.]", colnames(df))]))]
  
  # Save references
  refs = unique(df$ref)
  saveRDS(refs, file="./data/refs.rds")
  
  # Run analyses  
  .meta_i("c.s", df)
  .meta_i("c.cn", df)
  .meta_i("e.s", df)
  .meta_i("e.cn", df)
  .meta_i("s.cn", df)
  c.e = .run_it("c.e", df)
  c.s = .run_it("c.s", df)
  c.cn = .run_it("c.cn", df)
  e.s = .run_it("e.s", df)
  e.cn = .run_it("e.cn", df)
  s.cn = .run_it("s.cn", df)
  
  # Save table
  sink("./tables/table-intercor-guts.tex")
  cat("1. Cope & - &", .n_k("c.e", df), "&", .n_k("c.s", df),
      "&", .n_k("c.cn", df), "\\\\\n")
  cat("2. Enhance &", .asf(c.e), "& - &", .n_k("e.s", df),
      "&", .n_k("e.cn", df), "\\\\\n")
  cat("3. Social &", .asf(c.s), "&", .asf(e.s), "& - &",
      .n_k("s.cn", df), "\\\\\n")
  cat("4. Conform &", .asf(c.cn), "&", .asf(e.cn), "&",
      .asf(s.cn), "& -\\\\\n")
  sink()
  
  # Save 2nd Table
  sink("./tables/intercor.md")
  cat(" 0 | 1 | 2 | 3 | 4  \n")
  cat("---|---|---|---|---\n")
  cat("1. Cope | - |", .n_k("c.e", df), "|", .n_k("c.s", df),
      "|", .n_k("c.cn", df), "\n")
  cat("2. Enhance |", .asf2(c.e), "| - |", .n_k("e.s", df),
      "|", .n_k("e.cn", df), "\n")
  cat("3. Social |", .asf2(c.s), "|", .asf2(e.s), "| - |",
      .n_k("s.cn", df), "\n")
  cat("4. Conform |", .asf2(c.cn), "|", .asf2(e.cn), "|",
      .asf2(s.cn), "| -\n")
  sink()
  
  return(NULL)
}
