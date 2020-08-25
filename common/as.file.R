wants <- c('ggpubr', 'templates', 'rstatix', 'dplyr')
has <- wants %in% rownames(installed.packages())
if (any(!has)) install.packages(wants[!has])

library(rstatix)
library(templates)

tTestQQPlots <- function(dv, subgroups, iv, data = 'data', ext = "Rmd") {
  do.call(paste0, lapply(seq(1, length(subgroups)), FUN = function(i) {
    strwhich <- paste0(data,'[["',iv,'"]] == "', as.character(subgroups[i]),'"')
    strwhich <- paste0("which(", strwhich,")")
    qq.plots <- paste0("qqPlot( ~ `",dv,"`, data = ", data, "[",strwhich,",])")
    if (ext == "Rmd") {
      qq.plots <- paste0(
        "* QQ plot in the ", paste0('**',iv,'**: "', as.character(subgroups[i]), '"')," \n",
        "```{r}\n", paste0(qq.plots, "\n"), "```\n"
      )
    }
    return(paste0(qq.plots, "\n"))
  }))
}

factorialAnovaQQPlots <- function(dv, subgroups, ivs = colnames(subgroups), data = 'data', ext = "Rmd") {
  for (iv in ivs) subgroups[[iv]] <- as.character(subgroups[[iv]])
  do.call(paste0, lapply(seq(1, nrow(subgroups)), FUN = function(i) {
    subgroup <- subgroups[i, c(ivs)]
    strwhich <- paste0(paste0(data,'[["',ivs,'"]] == "',subgroup[,ivs],'"'), collapse = " & ")
    strwhich <- paste0("which(", strwhich,")")
    qq.plots <- paste0("qqPlot( ~ `",dv,"`, data = ", data, "[",strwhich,",])")
    if (ext == "Rmd") {
      qq.plots <- paste0(
        "* QQ plot in the ", paste0(paste0('**',ivs,'**: "', subgroup[,ivs], '"'), collapse = '; ')," \n",
        "```{r}\n", paste0(qq.plots, "\n"), "```\n"
      )
    }
    return(paste0(qq.plots, "\n"))
  }))
}

factorialAnovaPWCs <- function(dv, ivs, p.adjust.method = 'bonferroni', ext = 'Rmd') {
  do.call(paste0, lapply(ivs, FUN = function(iv) {
    
    sdat <- 'sdat'
    diff.ivs <- setdiff(ivs, iv)
    cnames <- c(diff.ivs, "group1", "group2","estimate","se","df","statistic","p", "p.adj","p.adj.signif")
    
    if (length(diff.ivs) > 0) sdat <- paste0('group_by(sdat, ',paste0(diff.ivs, collapse = ','),')') 
    emm <- paste0('emm[["',iv,'"]] <- emmeans_test(',sdat,', `',dv,'` ~ `',iv,'`, p.adjust.method = "',p.adjust.method,'", detailed = T)')
    if (ext == 'Rmd') {
      emm <- paste0(
        "* Estimated marginal means for **", iv, "**\n",
        "```r\n(", emm, ")\n```\n",
        "```{r echo=FALSE}\n", emm, "\n",
        "kdf <- add_significance(emm[[\"",iv,"\"]])\n",
        "kdf <- kdf[,c(",paste0(paste0('"',cnames,'"'), collapse = ','),")]\n",
        "kdf$p.adj <- round(kdf$p.adj, 4)\n",
        "kdf$p.adj[which(kdf$p.adj < 0.0001)] <- '< 0.0001'\n",
        "kable(kdf, digits = 4)\n",
        "```\n"
      )
    } else if(ext == 'R') {
      emm <- paste0('(', emm, ')')
    }
    return(paste0(emm, "\n"))
  }))
}

factorialAnovaBoxPlots <- function(dv, ivs, fig.width = 1000, fig.height = 1000, addParam = c("jitter"), font.label.size = 10, step.increase = 0.005, ext = 'Rmd') {
  saddParam <- paste0("c(",paste0(paste0('"', addParam, '"') , collapse = ",") ,")")
  do.call(paste0, lapply(ivs, FUN = function(iv) {
    box.plot <- paste0("ggPlotAoV(sdat, \"", iv, "\", \"", dv, "\", aov=res.aov, pwc=emm[[\"",iv,"\"]], addParam=", saddParam, ", font.label.size = ", font.label.size, ", step.increase = ",step.increase,")")
    if (ext == 'Rmd') {
      box.plot <- paste0(
        "```{r, fig.width=", ceiling(fig.width/100), ", fig.height=", ceiling(fig.height/100), "}\n",
        box.plot, "\n",
        "```\n")
    }
    return(paste0(box.plot, "\n"))
  }))
}

tTestBoxPlots <- function(dv, iv, fig.width = 1000, fig.height = 1000, addParam = c("jitter"), font.label.size = 10, ext = 'Rmd') {
  saddParam <- paste0("c(",paste0(paste0('"', addParam, '"') , collapse = ",") ,")")
  box.plot <- paste0("ggPlotTTest(sdat, \"", iv, "\", \"", dv, "\", tt, ", saddParam, ", ", font.label.size, ")")
  if (ext == 'Rmd') {
    box.plot <- paste0(
      "```{r, fig.width=", ceiling(fig.width/100), ", fig.height=", ceiling(fig.height/100), "}\n",
      box.plot, "\n",
      "```\n")
  }
  return(paste0(box.plot, "\n"))
}

getTTestBoxPlots <- function(dvs, iv, tts = 'tts', plotParams = list(), ext = 'Rmd') {
  
  box.plot <- do.call(paste0, c(list(collapse = '\n'), lapply(dvs, FUN = function(dv) {
    
    width <- 800
    height <- 600
    font.size <- 10
    addParam <- c("jitter")
    if (!is.null(plotParams[[dv]]$width)) width <- plotParams[[dv]]$width
    if (!is.null(plotParams[[dv]]$height)) height <- plotParams[[dv]]$height
    if (!is.null(plotParams[[dv]]$font.size)) font.size <- plotParams[[dv]]$font.size
    if (!is.null(plotParams[[dv]]$addParam)) addParam <- c(plotParams[[dv]]$addParam)
    saddParam <- paste0("c(",paste0(paste0('"', addParam, '"') , collapse = ",") ,")")
    
    tt <- paste0(tts,'[["',dv,'"]]')
    sbp <- paste0('ggPlotTTest(sdat[which(sdat[["var"]] == "',dv,'"),],"',iv,'","',dv,'",',tt,',',saddParam,",",font.size,")")
    if (ext == 'Rmd') {
      sbp <- paste0(
        "* Plot for the dependent variable: ",dv,"\n",
        "```{r, fig.width=", ceiling(width/100), ", fig.height=", ceiling(height/100), "}\n",
        sbp, "\n",
        "```\n")
    }
    return(sbp)
  })))
  return(paste0(box.plot, "\n"))
}

getPreprocessing <- function(dvs, skewness = list(), ext = 'Rmd') {
  skew.cmd <- c()
  for (dv in dvs) {
    if (dv %in% unlist(skewness)) {
      skew.cmd <- c(skew.cmd, '### Visualization of data distribution')
      if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```{r}") }
      skew.cmd <- c(skew.cmd, paste0('ggdensity(dat, x = "',dv,'", fill = "lightgray", title= "Density of ',dv,' before transformation") +'))
      skew.cmd <- c(skew.cmd, paste0(' stat_overlay_normal_density(color = "red", linetype = "dashed")'))
      if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```") }
      skew.cmd <- c(skew.cmd, ' ')
      
      if (dv %in% skewness$posSqrt) {
        skew.cmd <- c(skew.cmd, paste0('### Dealing with positive moderate skewness in ', dv))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```{r}") }
        skew.cmd <- c(skew.cmd, paste0('dat[["',dv,'"]] <- sqrt(dat[["',dv,'"]])'))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```") }
      }
      if (dv %in% skewness$negSqrt) {
        skew.cmd <- c(skew.cmd, paste0('### Dealing with negative moderate skewness in ', dv))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```{r}") }
        skew.cmd <- c(skew.cmd, paste0('dat[["',dv,'"]] <- sqrt(max(dat[["',dv,'"]]+1) - dat[["',dv,'"]])'))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```") }
      }
      if (dv %in% skewness$posLog) {
        skew.cmd <- c(skew.cmd, paste0('### Dealing with positive greater skewness in ', dv))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```{r}") }
        skew.cmd <- c(skew.cmd, paste0('dat[["',dv,'"]] <- log10(dat[["',dv,'"]])'))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```") }
      }
      if (dv %in% skewness$negLog) {
        skew.cmd <- c(skew.cmd, paste0('### Dealing with negative greater skewness in ', dv))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```{r}") }
        skew.cmd <- c(skew.cmd, paste0('dat[["',dv,'"]] <- log10(max(dat[["',dv,'"]]+1) - dat[["',dv,'"]])'))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```") }
      }
      if (dv %in% skewness$posInv) {
        skew.cmd <- c(skew.cmd, paste0('### Dealing with positive severe skewness in ', dv))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```{r}") }
        skew.cmd <- c(skew.cmd, paste0('dat[["',dv,'"]] <- 1/(dat[["',dv,'"]])'))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```") }
      }
      if (dv %in% skewness$negInv) {
        skew.cmd <- c(skew.cmd, paste0('### Dealing with negative severe skewness in ', dv))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```{r}") }
        skew.cmd <- c(skew.cmd, paste0('dat[["',dv,'"]] <- 1/(max(dat[["',dv,'"]]+1) - dat[["',dv,'"]])'))
        if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```") }
      }
      if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```{r}") }
      skew.cmd <- c(skew.cmd, paste0('ggdensity(dat, x = "',dv,'", fill = "lightgray", title= "Density of ',dv,' after transformation") +'))
      skew.cmd <- c(skew.cmd, paste0(' stat_overlay_normal_density(color = "red", linetype = "dashed")'))
      if (ext == 'Rmd') { skew.cmd <- c(skew.cmd, "```") }
    }
    return(paste0(paste0(skew.cmd, collapse = '\n'), '\n'))
  }
}

as.file <- function(type, ext, wid, dv, ivs, subgroups, outliers = c(), non.normal = c(), extra.params = list()) {
  if (type == 'factorialAnova') {
    
    aovType <- 2
    effect.size <- 'ges'
    fileCSVpath <- 'data.csv'
    p.adjust.method <- 'bonferroni'
    
    if (!is.null(extra.params$aovType)) aovType <- extra.params$aovType
    if (!is.null(extra.params$fileCSVpath)) fileCSVpath <- extra.params$fileCSVpath
    if (!is.null(extra.params$p.adjust.method)) p.adjust.method <- extra.params$p.adjust.method
    if (!is.null(extra.params$effect.size)) effect.size <- extra.params$effect.size
    
    fig.width <- 1000
    fig.height <- 1000
    addParam <- c("jitter")
    if (!is.null(extra.params$fig.width)) fig.width <- extra.params$fig.width
    if (!is.null(extra.params$fig.height)) fig.height <- extra.params$fig.height
    if (!is.null(extra.params$addParam)) addParam <- extra.params$addParam
    if (!is.null(extra.params$skewness)) skewness <- extra.params$skewness 
    
    templateFile <- paste0(getwd(), "/templates/", type, ".", ext)
    return(as.character(tmpl(
      paste(readLines(templateFile), collapse="\n"),
      wid = wid, ivs = ivs, dv = dv, fileCSVpath = fileCSVpath,
      preprocessing.data = getPreprocessing(dv, skewness = skewness, ext = ext),
      outliers = outliers, non.normal.data = non.normal,
      aovType = aovType, effect.size = effect.size,
      pairwise.comp = factorialAnovaPWCs(dv, ivs, p.adjust.method, ext),
      qq.plots.pre = factorialAnovaQQPlots(dv, subgroups, ivs, 'rdat', ext),
      qq.plots.pos = factorialAnovaQQPlots(dv, subgroups, ivs, 'sdat', ext),
      box.plots = factorialAnovaBoxPlots(dv, ivs, fig.width, fig.height, addParam, ext)
    )))
  } else if (type == 'kruskal') {
    
    ci <- TRUE
    fileCSVpath <- 'data.csv'
    p.adjust.method <- 'bonferroni'
    
    if (!is.null(extra.params$ci)) ci <- extra.params$ci
    if (!is.null(extra.params$fileCSVpath)) fileCSVpath <- extra.params$fileCSVpath
    if (!is.null(extra.params$p.adjust.method)) p.adjust.method <- extra.params$p.adjust.method
    
    fig.width <- 1000
    fig.height <- 1000
    addParam <- c("jitter")
    if (!is.null(extra.params$fig.width)) fig.width <- extra.params$fig.width
    if (!is.null(extra.params$fig.height)) fig.height <- extra.params$fig.height
    if (!is.null(extra.params$addParam)) addParam <- extra.params$addParam
    
    templateFile <- paste0(getwd(), "/templates/", type, ".", ext)
    return(as.character(tmpl(
      paste(readLines(templateFile), collapse="\n"),
      wid = wid, iv = ivs, dv = dv, fileCSVpath = fileCSVpath,
      p.adjust.method = p.adjust.method,
      fig.width = fig.width, fig.height = fig.height,
      ci = ci, post.hoc = "dunn_test"
    )))
  }
}

