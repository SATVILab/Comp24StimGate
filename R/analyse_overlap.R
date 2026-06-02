#' Calculate Overlap and Plot Bimodal Distribution
#'
#' @param mean_neg Mean of the negative component
#' @param sd_neg Standard deviation of the negative component
#' @param mean_pos Mean of the positive component
#' @param sd_pos Standard deviation of the positive component
#' @param prob_pos Mixing proportion of the positive component (default: 0.5)
#' @param trans The transformation to apply: "normal" (exact) or "gamma" (simulation)
#' @param n_sim Number of simulated cells per component (used only if trans = "gamma")
#'
#' @return A list containing a summary dataframe of the metrics and a ggplot object
analyze_overlap <- function(mean_neg = 0, sd_neg = 1.22, 
                            mean_pos = 8, sd_pos = 1.22, 
                            prob_pos = 0.5, 
                            trans = c("normal", "gamma"),
                            n_sim = 1e6,
                            coord_cartesian = NULL,
                            coord_expand = NULL) {
  
  trans <- match.arg(trans)
  
  if (trans == "normal") {
    # ---------------------------------------------------------
    # NORMAL SPACE (Exact Analytical Calculation)
    # ---------------------------------------------------------
    q_pos_05 <- qnorm(0.05, mean = mean_pos, sd = sd_pos)
    prop_neg_above_pos05 <- pnorm(q_pos_05, mean = mean_neg, sd = sd_neg, lower.tail = FALSE)
    
    q_neg_95 <- qnorm(0.95, mean = mean_neg, sd = sd_neg)
    prop_pos_below_neg95 <- pnorm(q_neg_95, mean = mean_pos, sd = sd_pos)
    
    joint_density <- function(x) {
      (1 - prob_pos) * dnorm(x, mean = mean_neg, sd = sd_neg) + 
           prob_pos  * dnorm(x, mean = mean_pos, sd = sd_pos)
    }
    
    search_interval <- sort(c(mean_neg, mean_pos))
    opt_res <- optimize(joint_density, interval = search_interval)
    antimode <- opt_res$minimum
    
    prop_neg_above_antimode <- pnorm(antimode, mean = mean_neg, sd = sd_neg, lower.tail = FALSE)
    prop_pos_below_antimode <- pnorm(antimode, mean = mean_pos, sd = sd_pos)
    prop_pos_above_antimode <- pnorm(antimode, mean = mean_pos, sd = sd_pos, lower.tail = FALSE)
    prop_neg_below_antimode <- pnorm(antimode, mean = mean_neg, sd = sd_neg)
    
    # Plotting setup
    x_min <- min(mean_neg, mean_pos) - 4 * max(sd_neg, sd_pos)
    x_max <- max(mean_neg, mean_pos) + 4 * max(sd_neg, sd_pos)
    
    p <- ggplot(data.frame(x = c(x_min, x_max)), aes(x = x)) +
      stat_function(fun = function(x) (1 - prob_pos) * dnorm(x, mean_neg, sd_neg),
                    aes(color = "Negative Component"), linewidth = 1) +
      stat_function(fun = function(x) prob_pos * dnorm(x, mean_pos, sd_pos),
                    aes(color = "Positive Component"), linewidth = 1) +
      stat_function(fun = joint_density,
                    aes(color = "Joint Distribution"), linewidth = 1, linetype = "dashed")
    
    title_label <- "Gaussian Component Overlap"
    x_label <- "Expression Value"
    
  } else {
    # ---------------------------------------------------------
    # GAMMA TRANSFORMATION (Monte Carlo Simulation)
    # ---------------------------------------------------------
    gamma_transform <- function(x) { gamma(1 + abs(x) / 4) }
    
    raw_neg <- rnorm(n_sim, mean = mean_neg, sd = sd_neg)
    raw_pos <- rnorm(n_sim, mean = mean_pos, sd = sd_pos)
    
    trans_neg <- gamma_transform(raw_neg)
    trans_pos <- gamma_transform(raw_pos)
    
    q_pos_05 <- quantile(trans_pos, 0.05)
    prop_neg_above_pos05 <- mean(trans_neg > q_pos_05)
    
    q_neg_95 <- quantile(trans_neg, 0.95)
    prop_pos_below_neg95 <- mean(trans_pos < q_neg_95)
    
    n_pos_joint <- round(n_sim * prob_pos)
    n_neg_joint <- round(n_sim * (1 - prob_pos))
    joint_sample <- c(sample(trans_neg, n_neg_joint), sample(trans_pos, n_pos_joint))
    
    dens <- density(joint_sample, n = 2048)
    search_bounds <- sort(c(median(trans_neg), median(trans_pos)))
    
    valid_idx <- which(dens$x >= search_bounds[1] & dens$x <= search_bounds[2])
    antimode_idx <- valid_idx[which.min(dens$y[valid_idx])]
    antimode <- dens$x[antimode_idx]
    
    prop_neg_above_antimode <- mean(trans_neg > antimode)
    prop_pos_below_antimode <- mean(trans_pos < antimode)
    prop_pos_above_antimode <- mean(trans_pos > antimode)
    prop_neg_below_antimode <- mean(trans_neg < antimode)
    
    # Plotting setup
    plot_n <- min(n_sim, 50000)
    plot_data <- data.frame(
      Value = c(sample(trans_neg, plot_n), sample(trans_pos, plot_n)),
      Population = factor(rep(c("Negative Component", "Positive Component"), each = plot_n))
    )
    joint_plot_data <- data.frame(
      Value = sample(joint_sample, plot_n),
      Population = factor("Joint Distribution")
    )
    
    p <- ggplot() +
      geom_density(data = plot_data |> dplyr::filter(Population == "Negative Component"),
                   aes(x = Value, color = Population, weight = (1 - prob_pos)), 
                   linewidth = 1, adjust = 1.5) +
      geom_density(data = plot_data |> dplyr::filter(Population == "Positive Component"),
                   aes(x = Value, color = Population, weight = prob_pos), 
                   linewidth = 1, adjust = 1.5) +
      geom_density(data = joint_plot_data,
                   aes(x = Value, color = Population), 
                   linewidth = 1, linetype = "dashed", adjust = 1.5)
                   
    title_label <- expression(Gamma * "(1 + |x|/4) Transformation")
    x_label <- "Transformed Expression Value"
  }
  
  # ---------------------------------------------------------
  # Common Calculation & Plotting Rules
  # ---------------------------------------------------------
  
  # Weighted counts relative comparison
  rel_neg_above_antimode <- (prop_neg_above_antimode * (1 - prob_pos)) / (prop_pos_above_antimode * prob_pos)
  rel_pos_below_antimode <- (prop_pos_below_antimode * prob_pos) / (prop_neg_below_antimode * (1 - prob_pos))
  
  # Base plot formatting
  p <- p +
    geom_vline(xintercept = antimode, linetype = "dotted", color = "black", linewidth = 0.8) +
    scale_color_manual(values = c("Negative Component" = "#E41A1C", 
                                  "Positive Component" = "#377EB8", 
                                  "Joint Distribution" = "gray30")) +
    labs(title = title_label,
         subtitle = sprintf("Mixing: Pos = %.4f, Neg = %.4f", prob_pos, 1 - prob_pos),
         x = x_label, 
         y = "Density (Weighted)",
         color = "Distribution") +
    cowplot::theme_cowplot() +
    cowplot::background_grid(major = "xy") +
    theme(legend.position = "bottom")

  # Setup default gamma limits to ignore extreme simulation tails
  if (trans == "gamma") {
    p <- p + coord_cartesian(xlim = c(min(trans_neg, trans_pos), quantile(trans_pos, 0.999)))
  }

  # Apply user coordinates
  if (!is.null(coord_cartesian)) {
    xlim_cart <- if("x" %in% names(coord_cartesian)) coord_cartesian[["x"]] else NULL
    ylim_cart <- if("y" %in% names(coord_cartesian)) coord_cartesian[["y"]] else NULL
    if (is.null(names(coord_cartesian))) { xlim_cart <- ylim_cart <- coord_cartesian }
    
    p <- p + coord_cartesian(xlim = xlim_cart, ylim = ylim_cart)
  }

  if (!is.null(coord_expand)) {
    expand_x <- if("x" %in% names(coord_expand)) coord_expand[["x"]] else waiver()
    expand_y <- if("y" %in% names(coord_expand)) coord_expand[["y"]] else waiver()
    if (is.null(names(coord_expand))) { expand_x <- expand_y <- coord_expand }
    
    p <- p + expand_limits(x = expand_x, y = expand_y)
  }
  
  metrics <- data.frame(
    Metric = c("Prop of Neg Pop > 5th Pctl of Pos Pop",
               "Prop of Pos Pop < 95th Pctl of Neg Pop",
               "Calculated Antimode Location",
               "Prop of Neg Pop > Antimode",
               "Prop of Pos Pop < Antimode",
               "Ratio of count of neg to pos points above antimode",
               "Ratio of count of neg to pos points below antimode"),
    Value = c(prop_neg_above_pos05, prop_pos_below_neg95, antimode,
              prop_neg_above_antimode, prop_pos_below_antimode,
              rel_neg_above_antimode, rel_pos_below_antimode)
  )
  
  return(list(metrics = metrics, plot = p))
}
