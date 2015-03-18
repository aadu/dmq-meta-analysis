#' Process dmq norms
#'
#' @import R2jags
#' @import metafor
#' @import MAd
#' @export
#' @include import-google.R
#' @include formatters.R
dmq_norms = function(){
  df = rescale_norms(df)
  replace_median = function(x){
    x[is.na(x)] = median(x, na.rm=T)
    return(x)}
  df$country = factor(df$country)
  df$undergrad = df$undergrad * 2
  df$undergrad[df$adolescent == 1] = 1
  df$measure = factor(df$measure)
  df$man = df$male / 100
  df$man[df$man > 0 & df$man < 1] = NA
  df$man = factor(df$man)
  df$mvf = factor(df$mvf)
  
  cope = .run_it("cope", df)
  en = .run_it("en", df)
  social = .run_it("social", df)
  conf = .run_it("conf", df)

  rma(yi=cope.m, vi=cope.sd, ni=n, data=df)
  rma(yi=en.m, vi=en.sd, ni=n, data=df)
  rma(yi=social.m, vi=social.sd, ni=n, data=df)
  rma(yi=conf.m, vi=conf.sd, ni=n, data=df)
  
  # Coping motives
  df.cope = subset(df, !is.na(cope.m))
  df.cope$cope.sd = replace_median(df.cope$cope.sd)
  macat(cope.m, cope.sd, country, df.cope)
  # plotcat(cope.m, cope.sd, usa, df.cope)
  macat(cope.m, cope.sd, undergrad, df.cope)
  macat(cope.m, cope.sd, clinical, df.cope)
  macat(cope.m, cope.sd, measure, df.cope)
  macat(cope.m, cope.sd, mvf, df.cope)
  macat(cope.m, cope.sd, man, df.cope)
  
  # Enhancement motives
  df.en = subset(df, !is.na(en.m))
  df.en$en.sd = replace_median(df.en$en.sd)
  macat(en.m, en.sd, country, df.en)
  #plotcat(en.m, en.sd, usa, df.en)
  macat(en.m, en.sd, undergrad, df.en)
  macat(en.m, en.sd, clinical, df.en)
  macat(en.m, en.sd, man, df.en)
  macat(en.m, en.sd, measure, df.en)
  
  # Social motives
  df.social = subset(df, !is.na(social.m))
  df.social$social.sd = replace_median(df.social$social.sd)
  macat(social.m, social.sd, country, df.social)
  macat(social.m, social.sd, undergrad, df.social)
  macat(social.m, social.sd, clinical, df.social)
  macat(social.m, social.sd, man, df.social)
  macat(social.m, social.sd, measure, df.social)
  
  # Conformity motives
  df.conf = subset(df, !is.na(conf.m))
  df.conf$conf.sd = replace_median(df.conf$conf.sd)
  macat(conf.m, conf.sd, country, df.conf)
  #plotcat(conf.m, conf.sd, usa, df.conf)
  macat(conf.m, conf.sd, undergrad, df.conf)
  macat(conf.m, conf.sd, clinical, df.conf)
  macat(conf.m, conf.sd, man, df.conf)
  macat(conf.m, conf.sd, measure, df.conf)  
  
  # Meta-regression
  summary(mareg(cope.m ~ age + measure + male + year + undergrad + clinical + n,
                var=cope.sd, data=df))
  summary(mareg(en.m ~ age + measure + male + year + undergrad + clinical + n,
                var=en.sd, data=df))
  summary(mareg(social.m ~ age + measure + male + year + undergrad + clinical + n,
                var=social.sd, data=df))
  summary(mareg(conf.m ~ age + measure + male + year + undergrad + clinical + n,
                var=conf.sd, data=df))
  summary(mareg(cope.m ~ age + male + white + black + asian + hisp,
                var=cope.sd,data=df))
  summary(mareg(en.m ~ age + male + white + black + asian + hisp,
                var=en.sd, data=df))
  summary(mareg(social.m ~ age + male + white + black + asian + hisp,
                var=social.sd, data=df))
  summary(mareg(conf.m ~ age + male + white + black + asian + hisp,
                var=conf.sd, data=df))
  
}


#' @describeIn dmq_norms
#' @export
rescale_norms = function(df){
  # Add 1 to 0-based scales
  i = grep("0-", df$scale)
  df$scale[i] = gsub("0", "1", df$scale[i])
  df$cope.m[i] = df$cope.m[i] + 1
  df$en.m[i] = df$en.m[i] + 1
  df$social.m[i] = df$social.m[i] + 1
  df$conf.m[i] = df$conf.m[i] + 1
  rescale = function(df, x, y=5){
    i = grep(paste0("-", x), df$scale)
    df$scale[i] = gsub(x, y, df$scale[i])
    df$cope.m[i] = df$cope.m[i] / x * y
    df$en.m[i] = df$en.m[i] / x * y
    df$social.m[i] = df$social.m[i] / x * y
    df$conf.m[i] = df$conf.m[i] / x * y
    return(df)
  }
  # Rescale 3-point scales to 5-point
  df = rescale(df, 3, 5)
  # Rescale 4-point scales to 5-point
  df = rescale(df, 4, 5)
  # etcetera
  df = rescale(df, 6, 5)
  df = rescale(df, 7, 5)
  return(df)
}


#' @describeIn dmq_norms
#' @export
.mydata = function(x, df){
  m = df[[paste(x, ".m", sep="")]]
  m.sd = df[[paste(x, ".sd", sep="")]]
  n = df$n
  m.sd = m.sd[!is.na(m)]
  n = n[!is.na(m)]
  m.sd[is.na(m.sd)] = mean(m.sd, na.rm=T)
  m = m[!is.na(m)]
  if(length(m) != length(m.sd))
    warning("Length of m and m.sd is not equal!!!")
  se = m.sd / sqrt(n)
  mydata = list(m = as.vector(m), m.sd = as.vector(m.sd), n = n, se = se)
  return(mydata)
}


#' @describeIn dmq_norms
#' @export
.run_it = function(x, df) {
  mydata = .mydata(x, df)
  alph = df[[paste(x, ".a", sep="")]]
  out = list(m = grand.mean(mydata$m, (mydata$n)),
             sd = grand.sd(mydata$m.sd, mydata$m, (mydata$n)),
             n = sum(mydata$n),
             k = length(mydata$n),
             a = round(mean(alph, na.rm=T), 3),
             a.min = min(alph, na.rm=T),
             a.max = max(alph, na.rm=T))
  return(out)
}


#' @describeIn dmq_norms
#' @export
run_jags = function(){
  .jags_norms = function(x, df){
    mydata = .mydata(x, df)
    mydata = list(m = mydata$m, m.sd = mydata$se)
    fit = jags(data=mydata,
               parameters.to.save=c("theta", "tau", "bpvalue", "cSD"),
               model.file="mean.txt")
    fit = autojags(fit)
    #get_hdi("theta", fit)
    out = list(m = fit$BUGSoutput$mean$theta,
               m.sd = fit$BUGSoutput$mean$tau,
               se = fit$BUGSoutput$sd$theta,
               m.sd.se = fit$BUGSoutput$sd$tau,
               sd = fit$BUGSoutput$mean$cSD,
               hdi.l = get_hdi("theta", fit)[1],
               hdi.u = get_hdi("theta", fit)[2],
               hdi = paste0("[", fC(get_hdi("theta", fit)[1], 3), ", ",
                           fC(get_hdi("theta", fit)[2], 3), "]"))
    return(out)
  }
  j.cope = .jags_norms("cope", df)
  j.en = .jags_norms("en", df)
  j.social = .jags_norms("social", df)
  j.conf = .jags_norms("conf", df)
  # Save references
  dfn = df
  ref = unique(dfn$ref)
  saveRDS(ref, "./data/refs.rds")
}
