clst_plot <- function(x, org_x = x, clst, alpha_lvl,
                      unit, x_range = c(min(org_x),max(org_x)),
                      plt_mgn = 0){
  clst   <- lapply(clst,function(x)x[x[,3]<=alpha_lvl,,drop = F])
  clst_x <- lapply(clst,function(foo)
    t(apply(foo[,1:2,drop = F],1,function(i)x[i])))

  ## cluster colors ##
  p_values <- unlist(lapply(clst,function(x)x[,3,drop = F]))
  num_col  <- (log10(alpha_lvl+10^-16) - log10(p_values+10^-16))/
    (log10(alpha_lvl+10^-16)-min(log10(10^-16)))

  ## histogram breaks ##
  bds <- ceiling(range(org_x/unit))
  breaks <- seq(bds[1]-1,bds[2])*unit

  if(requireNamespace("ggplot2", quietly = TRUE)){
    df_org_x <- data.frame(x = org_x)
    my_plot <- ggplot2::ggplot(df_org_x, ggplot2::aes(x)) +
      ggplot2::stat_bin(breaks = breaks,
                        closed = "right",
                        fill = "white",col = "black") +
      ggplot2::theme_bw() +
      ggplot2::coord_cartesian(xlim = x_range)

    y_max <- ggplot2::layer_scales(my_plot)$y$range$range[2]

    ## cluster data.frame convert ##
    plot_data <- function(ind){
      x_lim <- clst_x[[ind]]
      y_bot <- (ind-1)*y_max/length(clst_x)
      y_up  <- ind*y_max/length(clst_x)

      res   <- data.frame(x = numeric(0),
                          y = numeric(0),
                          col = numeric(0),
                          group = character(0))

      if(ncol(x_lim)>0){
        for (i in seq.int(nrow(x_lim))){
          temp <- data.frame(x = rep(c(x_lim[i,1] - plt_mgn,
                                       x_lim[i,2] + plt_mgn), each = 2),
                             y = c(y_bot,y_up,y_up,y_bot),
                             group = paste(ind, i, sep = "-"))
          res <- rbind.data.frame(res,temp)
        }
      }
      return(res)
    }

    if (requireNamespace("parallel", quietly = TRUE)){
      df <- parallel::mclapply(seq.int(length(clst_x)), plot_data)
    }else{
      df <- lapply(seq.int(length(clst_x)), plot_data)
    }

    df_clst <- do.call("rbind.data.frame",df)
    df_clst$col <- rep(num_col,each = 4)

    ## cluster plot ##
    p_round <- round(alpha_lvl,3)
    a <- nchar(p_round)-2
    b <- p_round*10^a
    p_labels <- b*10^-(seq(a,16,by = 2))
    p_breaks  <- (log10(alpha_lvl+10^-16) - log10(p_labels+10^-16))/
      (log10(alpha_lvl+10^-16)-min(log10(10^-16)))

    p_labels <- formatC(p_labels,format = "e",digits = 0)

    my_plot <- my_plot +
      ggplot2::geom_polygon(data = df_clst,
                            ggplot2::aes(x = x, y = df_clst$y,
                                         group = df_clst$group, fill = col)) +
      ggplot2::scale_fill_gradient(
        low = grDevices::rgb(red = 1,green = 0.7,blue = 0.7,alpha = 0.6),
        high = grDevices::rgb(red = 1,green = 0.1,blue = 0.1,alpha = 0.6),
        breaks = p_breaks, name = expression( ~ alpha ~level),
        labels = p_labels, guide = "colourbar",limits = c(0,1)) +
      ggplot2::theme(panel.background = ggplot2::element_blank(),
                     panel.grid.minor = ggplot2::element_blank(),
                     panel.grid.major = ggplot2::element_blank(),
                     legend.position = "right",
                     legend.key.height = ggplot2::unit(0.1,"npc"),
                     plot.title = ggplot2::element_text(hjust = 0.5,size = 40),
                     axis.title = ggplot2::element_text(size = 25),
                     axis.text = ggplot2::element_text(size = 20),
                     legend.text = ggplot2::element_text(size = 20),
                     legend.title = ggplot2::element_text(size = 25)) +
      ggplot2::guides(fill = ggplot2::guide_colorbar(reverse=T)) +
      ggplot2::xlab("")
    return(my_plot)
  }else{
    old_par_mar <- graphics::par()$mar
    ## split plot in left and right parts ##
    graphics::layout(matrix(c(1,2), 1, 2, byrow = TRUE), widths=c(9,1))

    graphics::par(mar=c(5, 4, 4, 3) + 0.1)
    graphics::par(yaxs = 'r')

    graphics::hist(org_x,breaks = breaks,xlim = x_range,
                   xlab = NA,ylab = NA, main = NA, cex.axis = 1.5)
    graphics::title(ylab = "count", line = 2.5, cex.lab = 2)

    y_scale <- graphics::par("usr")[4]

    g_b_col <- num_col * 0.6 + 0.1
    hm_color <- grDevices::rgb(red = 1, green = g_b_col, blue = g_b_col,
                               alpha = 0.6, maxColorValue = 1)

    ## colorful bars ##
    ## cluster data.frame convert ##
    plot_data <- function(ind){
      x_lim <- clst_x[[ind]]
      y_bot <- (ind-1)*y_max/length(clst_x)
      y_up  <- ind*y_max/length(clst_x)

      res   <- data.frame(x = numeric(0),
                          y = numeric(0))

      if(ncol(x_lim)>0){
        for (i in seq.int(nrow(x_lim))){
          temp <- data.frame(x = c(rep(c(x_lim[i,1] - plt_mgn,
                                         x_lim[i,2] + plt_mgn),each = 2),NA),
                             y = c(y_bot,y_up,y_up,y_bot,NA))
          res <- rbind.data.frame(res,temp)
        }
      }
      return(res)
    }

    if (requireNamespace("parallel", quietly = TRUE)){
      df <- parallel::mclapply(seq.int(length(clst_x)),plot_data)
    }else{
      df <- lapply(seq.int(length(clst_x)),plot_data)
    }

    df_clst <- do.call("rbind.data.frame",df)

    graphics::polygon(df_clst$x, df_clst$y,
                      col = hm_color, border = NA)
    #------------------------------- plot legend ------------------------------#
    graphics::par(mar=c(2,0,2.5,2.5))
    graphics::par(yaxs = 'i')

    ## legend colors ##
    p_round <- round(alpha_lvl,3)
    a <- nchar(p_round)-2
    b <- p_round*10^a
    p_labels <- b*10^-(seq(a,16,by = 2))
    p_breaks  <- (log10(alpha_lvl+10^-16) - log10(p_labels+10^-16))/
      (log10(alpha_lvl+10^-16)-min(log10(p_labels+10^-16)))

    p_labels <- formatC(p_labels,format = "e",digits = 0)

    label_color <- grDevices::rgb(red = 1,
                                  green = p_breaks*0.6+0.1,
                                  blue = p_breaks*0.6+0.1,
                                  alpha = 0.6, maxColorValue = 1)

    graphics::plot(NA, axes = F,
                   ylim = c(0.5,length(p_labels)+0.5), xlim = c(0,1))
    graphics::rect(xleft = 0.2,
                   ybottom = length(p_labels):1 - 0.5,
                   xright = 0.7,
                   ytop = length(p_labels):1 + 0.5,
                   col = label_color,border = NA)

    graphics::axis(4,at = length(p_labels):1,
                   labels = p_labels, cex.axis = 1.5, tick =F)
    graphics::axis(2,at = length(p_labels)/2,
                   labels = expression(alpha),tick = F,
                   cex.axis = 1.5, las = 1)

    ## reset par() ##
    graphics::par(mfrow=c(1,1))
    graphics::par(mar = old_par_mar)
    graphics::par(yaxs = 'r')
    return(grDevices::recordPlot())
  }
}
