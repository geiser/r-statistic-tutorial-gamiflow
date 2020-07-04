library(ggpubr)

ggPlotAoV <- function(data, x, y, color = c(), aov, pwc, linetype = color, by = c(), addParam = c() ) {
  pwc <- tryCatch(add_xy_position(pwc, x = x), error = function(e) NULL)
  if (is.null(pwc)) return(ggplot())
  if (length(color) > 0) {
    bxp <- ggboxplot(data, x = x, y = y, color = color, palette = "jco", add=addParam, facet.by = by)
    bxp <- bxp + stat_pvalue_manual(pwc, color = color, linetype = linetype, hide.ns = T
                                    , tip.length = 0, step.increase = 0.1, step.group.by = by)
  } else {
    bxp <- ggboxplot(data, x = x, y = y, color = x, palette = "jco", add=addParam, facet.by = by)
    bxp <- bxp + stat_pvalue_manual(pwc, linetype = linetype, hide.ns = T
                                    , tip.length = 0, step.increase = 0.1, step.group.by = by)
  }
  bxp <- bxp + labs(subtitle = get_test_label(aov, detailed = T), caption = get_pwc_label(pwc))
  return(bxp)
}

threeWayAnovaPlots <- function(data, dv, ivs, aov, pwcs, addParam=c(), width = 800, height = 800) {
  do.call(verticalLayout, lapply(ivs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    do.call(verticalLayout, lapply(setdiff(ivs, iv), FUN = function(gby) {
      color <- setdiff(setdiff(ivs, iv), gby)
      verticalLayout(
        br(), p(strong(paste("Gráficos com base de:", iv, "e separado por:", gby,"- Color:", color)))
        , renderPlot({
          ggPlotAoV(subset(data, var == dv), iv, dv, color=color, by=gby, aov=aov, pwc=pwc, addParam=addParam)
        }
        , width = width
        , height = height)
      )
    }))
  }))
}

twoWayAnovaPlots <- function(data, dv, ivs, aov, pwcs, addParam=c(), width = 800, height = 800) {
  do.call(verticalLayout, lapply(ivs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    color <- setdiff(ivs, iv)
    verticalLayout(
      br(), p(strong(paste("Gráficos com base de:", iv, "- Color:", color)))
      , renderPlot({
        ggPlotAoV(subset(data, var == dv), iv, dv, color=color, aov=aov, pwc=pwc, addParam=addParam)
      }
      , width = width
      , height = height)
    )
  }))
}

oneWayAnovaPlots <- function(data, dv, ivs, aov, pwcs, addParam=c(), width = 800, height = 800) {
  do.call(verticalLayout, lapply(ivs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    verticalLayout(
      br(), p(strong(paste("Gráficos com base de:", iv)))
      , renderPlot({
        ggPlotAoV(subset(data, var == dv), iv, dv, aov=aov, pwc=pwc, addParam=addParam)
      }
      , width = width
      , height = height)
    )
  }))
}

