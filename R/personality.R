#' Process personality correlations
#'
#' @import R2jags
#' @import metafor
#' @save_path Location to save
#' @return references.
#' @export
#' @include import-google.R
#' @include formatters.R
process_personality = function(){
  # Load the data
  studies = load_dmq_data()
  df = merge.data.frame(load_dmq_data('personality'), studies, by="id")
  df$cope.v = escalc(ri=cope, ni=n, data=df, measure="COR")[['vi']]
  df$enhance.v = escalc(ri=enhance, ni=n, data=df, measure="COR")[['vi']]
  df$social.v = escalc(ri=social, ni=n, data=df, measure="COR")[['vi']]
  df$conform.v = escalc(ri=conform, ni=n, data=df, measure="COR")[['vi']]
  df = df[,order(colnames(df))]
  
  # Conduct chi-square tests
  x1 = matrix(c(sum(!is.na(df$cope)), sum(is.na(df$cope)),
                sum(!is.na(df$enhance)),sum(is.na(df$enhance))),
              nrow=2)
  cat(chisq.test(x1))
  x2 = matrix(c(sum(!is.na(df$cope)), sum(is.na(df$cope)),
                sum(!is.na(df$social)),sum(is.na(df$social))),
              nrow=2)
  cat(chisq.test(x2))
  x3 = matrix(c(sum(!is.na(df$social)), sum(is.na(df$social)),
                sum(!is.na(df$enhance)),sum(is.na(df$enhance))),
              nrow=2)
  cat(chisq.test(x3))
  x4 = matrix(c(sum(!is.na(df$social)), sum(is.na(df$social)),
                sum(!is.na(df$conform)),sum(is.na(df$conform))),
              nrow=2)
  cat(chisq.test(x4))
  
  # Percent not missing
  for x in c('cope', 'enhance', 'social', 'conform'){
    cat("Percent not missing: ")
    cat(round(sum(!is.na(df[,x])/nrow(df), 2), "\n")
  }
  
  # Which constructs have more than three studies?
  cs = NA
  df = df[!is.na(df$construct),]
  for (i in unique(df$construct)){
    x = subset(df, construct == i)
    if(nrow(x) > 3){
      cs = c(cs, i)
    }
  }
  cs = cs[2:length(cs)]
  cat(cs)

  # Process each construct
  constructs = c("Drinking Quantity",
                 "Drinking Frequency",
                 "Alcohol Use",
                 "Alcohol-related Problems",
                 "Binge Drinking",
                 "Alcohol Dependence",
                 "AUDIT", "RAPI",
                 "Anxiety", "Social Anxiety", "Depression",
                 "Positive Affect", "Negative Affect",
                 "Positive Urgency", "Negative Urgency",
                 "Sensation Seeking",
                 "Self-Control", "Impulsivity",
                 "Childhood Abuse/Trauma",
                 "Hostility", "Risky Behaviors",
                 "Extraversion", "Agreeableness",
                 "Conscientiousness", "Neuroticism", "Openness",
                 "Age", "Male")
  # "Negative Urgency", "Positive Urgency", "Sensation Seeking"
  # Save table
  out = ""
  for(i in 1:length(constructs)){
    out[i] = try(.format.tabmov.row(df, constructs[i]))
  }
  sink("./tables/table-perscon-guts.tex")
  cat(paste(out, collapse=""))
  sink()
  
  # Create funnel plots
  for(each in c("Alcohol-related Problems", "Binge Drinking", "AUDIT",
                "Drinking Quantity", "Drinking Frequency")){
    x = subset(df, construct == each)
    x = rma(x$cope, x$cope.v, ni=x$n)
    funnel(x)
  }
}


#' @describeIn process_personality
#' @export
.set_perscor_data = function(x, constr, df){
  df = subset(df, construct == constr)
  r = df[[x]]
  r.variance = df[[paste(x, ".v", sep="")]]
  r = r[!is.na(r)]
  r.variance = r.variance[!is.na(r.variance)]
  if(length(r) != length(r.variance))
    warning("Length of r and r.variance is not equal!!!")
  mydata = list(r = as.vector(r), r.variance = as.vector(r.variance))
  return(mydata)
}


#' @describeIn process_personality
#' @export
.run_it = function(x, constr, df){
  mydata = .set_perscor_data(x, constr, df)
  if(length(mydata$r > 0)) {
    inits = function(){list(theta=rnorm(1, 0, .1),
                            precision.tau=runif(1, 0.000001, .1))}
    fit = jags(data=mydata, parameters.to.save=c("theta", "tau", "bpvalue"),
               model.file="./bugs.txt")
    fit = autojags(fit)
    out = list(r = fC(fit$BUGSoutput$mean$theta, 2),
               r.var = fC(fit$BUGSoutput$mean$tau, 2),
               hdi = paste("[", fC(get_hdi("theta", fit)[1], 2),
                           ",~",fC(get_hdi("theta", fit)[2], 2), "]", sep=""),
               bp = fC(fit$BUGSoutput$mean$bpvalue, 2))
  }
  else {
    out = list(r = "", r.var = "", hdi="", bp="")
  }
  return(out)
}


#' @describeIn process_personality
#' @export
.format.tabmov.row = function(df, con){
  df = subset(df, construct == con)
  motives = c("cope", "enhance", "social", "conform")
  out = ""
  for(m in motives){
    res = suppressWarnings(rma(yi=eval(parse(text=m)),
                               vi=eval(parse(text=paste(m, ".var", sep=""))),
                               data=df))
    new.out = paste0(formatC(res$b, 2, format="f"), " [",
                     formatC(res$ci.lb, 2,format="f"), ",~",
                     formatC(res$ci.ub, 2, format="f"), "] & ")
    new.out = gsub("0[.]", ".", x=new.out)
    new.out = gsub("^\\.|([^\\-])\\.", "\\1~.", x=new.out)
    out = paste(out, new.out, sep="")
  }
  out = paste0(out, prettyNum(sum(df$n), big.mark=","), " & ", nrow(df),
               " \\\\\n")
  paste0(con, " & ", out)
}


#' @describeIn process_personality
#' @export
.format.tabmov.row2 = function(df, con){
  df = subset(df, construct == con)
  motives = c("cope", "enhance", "social", "conform")
  out = ""
  for(m in motives){
    res = try(.run_it(m, con, df))
    out = paste(out, res$r, res$hdi, "&")
  }
  out = paste(out, " ", prettyNum(sum(df$n), big.mark=","),
              " & ", nrow(df), " \\\\\n", sep="")
  paste(con, " & ", out, sep="")
}


#' @describeIn process_personality
#' @export
.sink_refs = function(){
  refs = readRDS("./data/refs.rds")
  refs2 = unique(df$ref)
  refs = c(refs, refs2)
  refs = unique(refs)
  refs = sort(refs)
  length(refs)
  sink("./data/refsa.tex")
  cat("\\nocite{", paste(refs, collapse=", "), "}\n", sep="")
  sink()
  refs = paste("\\fullcite[*][]{", refs, "}\n\n", sep="")
  sink("./data/refsb.tex")
  cat(refs)
  sink()
}
