SummarizeGrowthByPlateCB <- function (plate, t_trim = 0, bg_correct = "min", plot_fit = FALSE, 
                                    plot_file = "growthcurver.pdf") 
{
  if (is.data.frame(plate) != TRUE) {
    stop("The 'plate' input data must be formatted as a data.frame.", 
         call. = FALSE)
  }
  if (length(grep("time", names(plate), ignore.case = TRUE)) != 
      1) {
    stop("There must be exactly one column named 'time' in the 'plate' data.frame.", 
         call. = FALSE)
  }
  names(plate)[grep("time", names(plate), ignore.case = TRUE)] <- "time"
  if (length(names(plate)) < 2) {
    stop("You must have at least two columns in the 'plate' data.frame: one for time, and the other for absorbance.")
  }
  if (bg_correct == "blank") {
    if (length(grep("blank", names(plate), ignore.case = TRUE)) != 
        1) {
      stop("There must be exactly one column named 'blank' in the 'plate' data.frame if you have selected the bg_correct 'plate' option.", 
           call. = FALSE)
    }
    names(plate)[grep("blank", names(plate), ignore.case = TRUE)] <- "blank"
  }
  n <- length(plate) - sum(grepl("time|plate", names(plate), 
                                 ignore.case = TRUE))
  d_gc <- data.frame(sample = character(n), k = numeric(n), 
                     n0 = numeric(n), r = numeric(n), t_mid = numeric(n), 
                     t_gen = numeric(n), auc_l = numeric(n), auc_e = numeric(n), 
                     sigma = numeric(n), note = character(n), stringsAsFactors = FALSE)
  if (plot_fit == TRUE) {
    grDevices::cairo_pdf(plot_file, width = 12, height = 8)
    old_par <- graphics::par(mfcol = c(8, 12), mar = c(2, 
                                                       0.5, 2, 0.5))
    idx_to_plot <- length(plate$time) * 1:100/100
    y_lim_max <- max(plate[, setdiff(names(plate), "time")]) - 
      min(plate[, setdiff(names(plate), "time")])
  }
  n <- 1
  for (col_name in names(plate)) {
    if (!col_name %in% c("time", "blank")) {
      if (bg_correct == "blank") {
        gc_fit <- SummarizeGrowth(data_t = plate$time, 
                                  data_n = plate[, col_name], t_trim = t_trim, 
                                  bg_correct = bg_correct, blank = plate$blank)
      }
      else {
        gc_fit <- SummarizeGrowth(data_t = plate$time, 
                                  data_n = plate[, col_name], t_trim = t_trim, 
                                  bg_correct = bg_correct)
      }
      d_gc$sample[n] <- col_name
      d_gc$k[n] <- gc_fit$vals$k
      d_gc$n0[n] <- gc_fit$vals$n0
      d_gc$r[n] <- gc_fit$vals$r
      d_gc$t_mid[n] <- gc_fit$vals$t_mid
      d_gc$t_gen[n] <- gc_fit$vals$t_gen
      d_gc$auc_l[n] <- gc_fit$vals$auc_l
      d_gc$auc_e[n] <- gc_fit$vals$auc_e
      d_gc$sigma[n] <- gc_fit$vals$sigma
      d_gc$max_od[n]<-max(gc_fit$data$N)
      d_gc$note[n] <- gc_fit$vals$note
      n <- n + 1
      if (plot_fit == TRUE) {
        graphics::plot(gc_fit$data$t[idx_to_plot], gc_fit$data$N[idx_to_plot], 
                       pch = 20, ylim = c(0, y_lim_max), cex = 0.6, 
                       xaxt = "n", yaxt = "n")
        graphics::mtext(paste(outputPath), side = 3, line = -2, outer = TRUE)
        graphics::text(x = max(gc_fit$data$t)/4, y = y_lim_max, 
                       labels = col_name, pos = 1)
        if (gc_fit$vals$note == "") {
          graphics::lines(gc_fit$data$t, stats::predict(gc_fit$model), 
                          col = "red")
        }
      }
    }
  }
  if (plot_fit == TRUE) {
    grDevices::dev.off()
  }
  return(d_gc)
}


