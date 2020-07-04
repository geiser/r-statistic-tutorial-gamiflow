library(ggpubr)

scheirerPlots <- function(data, dv, ivs, srh, pwcs, addParam=c(), width = 800, height = 800) {
  do.call(verticalLayout, lapply(ivs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    color <- setdiff(ivs, iv)
    print(srh)
    verticalLayout(
      br(), p(strong(paste("Gráficos com base de:", iv, "- Color:", color)))
      , renderPlot({
        
        sdf <- data.frame(srh)
        idx <- which(rownames(sdf) == iv)
        statistic <- round(as.double(sdf$H[idx]), 4)
        dof <- round(as.double(sdf[['Df']][idx]), 4)
        x2 <- round(as.double(sdf[['Sum.Sq']][idx]), 4)
        p <- round(as.double(sdf[['p.value']][idx]), 4)
        pval <- ifelse(p < 0.0001, paste0(' , p  < 0.0001'), paste0(' , p  = ', p))
        
        bxp <- ggboxplot(data, x = iv, y = dv, color = iv, palette = "jco", add=addParam)
        bxp <- bxp + stat_pvalue_manual(pwc, linetype = c(), hide.ns = T, tip.length = 0, step.increase = 0.1, y.position = max(data[[dv]]))
        bxp <- bxp + labs(subtitle = paste0('Scheirer-Ray-Hare X(',dof,') = ', x2,',  H = ', statistic, pval), caption = get_pwc_label(pwc))
        bxp
      }
      , width = width
      , height = height)
    )
  }))
}

kruskalPlots <- function(data, dv, ivs, kwm, pwcs, addParam=c(), width = 800, height = 800) {
  do.call(verticalLayout, lapply(ivs, FUN = function(iv) {
    pwc <- pwcs[[iv]]
    verticalLayout(
      br(), p(strong(paste("Gráficos com base de:", iv))),
      renderPlot({
        pwc <- tryCatch(add_xy_position(pwc, x = iv), error = function(e) NULL)
        bxp <- ggboxplot(data, x = iv, y = dv, color = iv, palette = "jco", add=addParam)
        bxp <- bxp + stat_pvalue_manual(pwc, linetype = c(), hide.ns = T, tip.length = 0, step.increase = 0.1)
        bxp <- bxp + labs(subtitle = get_test_label(kwm, detailed = T), caption = get_pwc_label(pwc))
        bxp
      },
      width = width, height = height)
    )
  }))
}
