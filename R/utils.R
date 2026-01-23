#--------------------------------------------------------#
#--------------------------------------------------------#
# docs for functions were generated with help of chatgpt #
#--------------------------------------------------------#
#--------------------------------------------------------#

negaffect_items = c("distressed","anxious","vulnerable","irritable") 
posaffect_items = c("good", "happy", "confident", "safe", "relaxed")
embselfcn_items = c("embarassed","selfconscious")
sideffect_items = c("dizzy","dry_mouth", "blunted", "nauseous","warm_face","not_myself") 

#' Create a Flextable with Optional Features
#'
#' This function creates a flextable from the provided data object, with options
#' to add a caption, a footnote, and to specify the number of digits for numeric
#' columns. It automatically applies the `theme_vanilla()` and `autofit()`
#' functions to format the table.
#'
#' @param tbl A data object that can be converted into a flextable.
#' @param caption Optional; a character string to use as the table caption.
#' @param footnote Optional; a character string to add as a footnote to the table.
#' @param digits Optional; an integer specifying the number of decimal places 
#'        to use for numeric columns.
#'
#' @return A flextable object with the specified features.
#' @export
#'
#' @examples
#' # Assuming `data` is a data.frame or similar object
#' my_flextable(data, caption = "Summary Table", footnote = "Source: Data Source", digits = 2)
my_flextable = function(tbl, caption = NULL, footnote = NULL, digits = NULL) {
  ft = 
    flextable(tbl) %>% 
    theme_vanilla() %>% 
    autofit()
  if (!is.null(caption)) ft = set_caption(ft, caption = caption)
  if (!is.null(footnote)) ft = add_footer_lines(ft, values = footnote)
  if (!is.null(digits)) ft = colformat_double(ft, digits = digits)
  return(ft)
}

#' Summarize and Visualize Model Outputs
#'
#' This function provides a summary of model parameters and diagnostic checks
#' for a fitted model object. It generates a table of parameter estimates and
#' diagnostic statistics, and a set of histograms for selected statistics.
#' The function is tailored for models with specific naming conventions in
#' parameter names.
#'
#' @param fit A brms-model object, which should contain elements like formula, family,
#'        and fitting results.
#' @param grep_string A regular expression string to select specific parameters
#'        for summarization. Default is set to capture a common set of parameter
#'        name prefixes.
#'
#' @return A list containing two elements: 'plot' and 'table'. The 'plot' is a 
#'         ggplot object showing histograms of selected diagnostic statistics.
#'         The 'table' is a formatted table of parameter summaries, including 
#'         mean, standard deviation, and diagnostic statistics like Rhat and 
#'         effective sample size.
model_summary = function(fit,grep_string = "b_|sd_|bs_|bcs_|sigma") {
  footnote_text = 
    paste(
      c(paste("model: ",fit$formula[1]),
        "b_ = fix effect parameters",
        "sd_ standard deviation of hierarchical parameters",
        "ess = effective sample size"),
      collapse = "\n")
  
  sample_diags = 
    fit$fit %>% 
    as_draws() %>% 
    subset_draws(grep_string, regex = TRUE) %>% 
    summarize_draws() %>% 
    data.table() %>% 
    .[, c("mad","median") := NULL] %>% 
    setnames("variable","parameter") 
  
  
  if (fit$family$family == "acat")
    sample_diags[, parameter := gsub("b_Intercept","Threshold",parameter)]
  lbs = c("very good","good","'OK'","bad")
  plt = 
    sample_diags %>% 
    .[, .(parameter, rhat, ess_bulk, ess_tail)] %>% 
    melt(id.vars = "parameter", variable.name = "stat") %>% 
    .[stat == "rhat", status := cut(value, breaks = c(0, 1.01,1.05,1.1,Inf), labels = lbs)] %>% 
    .[stat != "rhat", status := cut(value, breaks = c(0, 250,500,2000,Inf), labels = rev(lbs))] %>% 
    ggplot(aes(x = value, fill = status)) + 
    geom_histogram(bins = 25) + 
    scale_fill_manual(values = c("green3","palegreen","orange","red")) + 
    facet_wrap(~stat, scale = "free", nrow = 1) + xlab("") + 
    ylab("") + 
    theme(axis.line.y = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y=element_blank(),
          legend.margin=margin(t = 0, unit='cm'))
  
  tbl = 
    sample_diags %>% 
    kable(caption = "Summary of model paramters",
          digits = c(1,1,2,1,1,3,0,0)) %>% 
    kable_styling(full_width = FALSE) %>% 
    kable_classic() %>% 
    add_footnote(footnote_text, notation = "none")
  
  return(list(plot = plt, table = tbl))
}

#' Print Formatted Effect Size Statistics
#'
#' This function formats and prints effect size statistics, including the mean,
#' 95% credible interval, Bayes Factor, and probability of contrast direction.
#' It offers options for number formatting and concise output.
#'
#' @param x A numeric vector of a posterior sample representing the effect size estimate.
#' @param digits Integer; the number of decimal places to use in the output.
#' @param short Logical; if `TRUE`, provides a shorter version of the output.
#' @param BF.thresh Numeric; threshold for Bayes Factor calculation.
#' @param contr Character; specifies the contrast for the probability calculation,
#'        typically in the format "S>C" (Succeeds vs. Contrast).
#'
#' @return A character string with the formatted effect size statistics.
#' @export
#'
#' @examples
#' # Assuming `effects` is a numeric vector of effect size estimates
#' print_effect(effects, digits = 2, short = FALSE, BF.thresh = 3, contr = "S>C")
print_effect = function(x, digits = 0, short = FALSE, BF.thresh = 0, contr = "S>C") {
  formatP = function(d) ifelse(d > .999,">0.999", ifelse(d < .001,"<0.001", paste0("=",d)))
  
  
  x = as.numeric(x)
  vals = c(
    mean(x),
    quantile(x,probs = c(.025,.975)),
    mean(x>BF.thresh),
    round(sum(x>BF.thresh)/sum(x<BF.thresh),1)
  )  
  vals[1:3] = round(vals[1:3], digits = digits)
  vals[4] = round(vals[4], digits = 2)
  vals[5] = round(vals[5], digits = 2)
  return(
    ifelse(
      short == FALSE,
      paste0(
        vals[1],
        " (95% CrI=[",vals[2],", ",vals[3],"]",
        "; P(", contr, ")", formatP(vals[4]),")"
      ),
      paste0(vals[1]," (",vals[2],", ",vals[3],")"))
  )
}


#' Calculate Mean and Credible Interval, Optionally Include probability of being larger then 0
#'
#' This function calculates the mean and credible interval (CI) for a given posterior distribution in numeric vector. 
#' It optionally includes the p-value for the test of the null hypothesis that the mean is 0.
#'
#' @param value A numeric vector for which the mean, CI, and optional p-value are calculated.
#' @param digits An integer indicating the number of decimal places for the output. Defaults to 0.
#' @param get.P A logical value indicating whether to include the p-value in the output. Defaults to TRUE.
#' @return A character string containing the formatted mean, CI, and optionally the p-value.
get_mci = function(value, digits=0, get.P = TRUE) {
  get_P = function(value) {
    x = mean(value>0)
    if (x > .999) {
      return(">.999")
    } else if (x < .001) {
      return("<.001")
    } else {
      return(paste("=",x))
    }
  }
  frmt = gsub("D",digits,"%.Df (%.Df, %.Df)")
  Pr = ifelse(get.P == TRUE, paste0("; Pr ",get_P(value)),"")
  return(paste0(sprintf(frmt,
                        mean(value),
                        quantile(value,probs = .025),
                        quantile(value,.975)),
                Pr))
}

#' Print Posterior Summaries
#'
#' This function creates a formatted string summarizing posterior statistics, including the mean,
#' 95% credible interval (CrI), and Bayes Factor (BF) from a given matrix or data frame.
#'
#' @param x A data.table where each row represents a different statistic and the columns 
#'          contain values for 'mean', 'CI' (credible interval), and 'BF' (Bayes Factor).
#' @return A character string containing the formatted mean, 95% CrI, and BF for the first row of the input.
#' @examples
#' # Assuming 'result' is a matrix with columns 'mean', 'CI', and 'BF'
#' print_PSs(result)
print_PSs = function(x) {
  paste0(round(x[1,mean],1),
         " (95% CrI=[", x[1,CI],"], BF=",x[1,BF],")")
}

#' Create a Table with Statistical Summaries
#'
#' This function takes a data.table object and generates a table with statistical summaries. 
#' It processes the data by calculating mean and confidence intervals using the `get_mci` function, 
#' and then formats the 'effect' column for better readability. Finally, it creates a flextable with 
#' the provided caption.
#'
#' @param dt A data.table object containing the data to be summarized. The data.table is expected 
#'           to have a column named 'est' for estimates and a column named 'effect' for effect names.
#' @param caption A character string to be used as the caption for the generated table.
#' @return A flextable object containing the formatted statistical summaries and the specified caption.
make_tbl = function(dt, caption) {
  tmp = 
    dt %>% 
    .[, .(`stats` = get_mci(est)), by = .(effect)] %>% 
    .[, effect := gsub("_vs_"," - ", effect)] %>%
    .[, effect := gsub("_in_"," in ", effect)] %>% 
    .[, effect := gsub("in_","", effect)]
  tmp %>% 
    setnames(names(tmp), c("Effect~a~", "mean (CrI)~b~")) %>% 
    my_flextable(caption = caption) 
}


#' Coarsen a Continuous Variable into Bins
#'
#' This function coarsens a continuous variable into a specified number of bins. It takes a 
#' data.table object and a variable name, and optionally the number of bins, range, or specific breaks. 
#' It creates a new variable in the data.table representing the binned categories of the original variable.
#'
#' @param dt A data.table object containing the data.
#' @param var The name of the variable in `dt` to be coarsened.
#' @param bins An integer specifying the number of bins to divide the range into. Defaults to 20.
#' @param range A numeric vector of length two specifying the range over which to divide into bins. 
#'              Defaults to c(0,100).
#' @param breaks An optional numeric vector specifying the break points for the bins. If NULL, 
#'               breaks are calculated based on `range` and `bins`.
#' @return A modified data.table object with added columns for the coarsened variable and its 
#'         corresponding levels.
coarsen = function(dt, var, bins = 20, range = c(0,100), breaks = NULL) {
  var.c = paste0(var,".c")
  if (is.null(breaks)) {
    breaks = seq(range[1],range[2],length.out = bins + 1)
  }
  my_cut = function(x) {
    return(
      cut(x,
          breaks = breaks, 
          include.lowest = TRUE, 
          right = FALSE,
          ordered_result = TRUE)
    )
  }
  
  dt %>% 
    .[, (var.c) := my_cut(get(var))]
  
  c.levels = 
    my_cut(range[1]:range[2]) %>% 
    levels()
  
  var.L = paste0(var,".L")
  dt[, (var.L) := which(get(var.c) == c.levels),by = 1:nrow(dt)]
  return(dt)
}

#' Calculate First Time to Target
#'
#' This function calculates the first time point at which a specified target is reached or exceeded 
#' in a given dataset, considering potential bias. If the bias is negative, it identifies the first 
#' time the values in `x` are greater than or equal to the target. If the bias is non-negative, it 
#' looks for the first time the values are less than or equal to the target.
#'
#' @param x A numeric vector representing observed values over time.
#' @param target A numeric value specifying the target to be achieved.
#' @param time A numeric vector of the same length as `x`, representing the corresponding time 
#'             points for each value in `x`.
#' @param bias A numeric value indicating the direction and magnitude of bias. Negative values 
#'             suggest an underestimation bias.
#' @return A numeric value indicating the first time point at which the target is achieved, or 
#'         `Inf` if the target is never achieved.
get_ftt = function(x, target, time, bias) {
  # Assign the result of tryCatch to 'result'
  result = tryCatch({
    # --- Expression to try ---
    # Use standard if/else for clarity over ifelse with single condition
    if (any(bias < 0)) {
      if (any(x >= target)) {
        min(time[x >= target]) # This value is returned by the block if no error
      } else {
        Inf # This value is returned by the block if no error
      }
    } else {
      if (any(x <= target)) {
        min(time[x <= target]) # This value is returned by the block if no error
      } else {
        Inf # This value is returned by the block if no error
      }
    }
  }, 
  # --- Correct Error Handler ---
  error = function(e) { 
    # This function is called ONLY if an error happens above
    return(NA) 
  }) # End of tryCatch
  
  # Return the variable holding the tryCatch outcome
  return(result) 
}

# contrast samples for effect obtained
# As a general procedure, we predict from model parameters 
# for each participant expected outcomes under each condition
# and calculate contrasts as comparisons between conditions
make_plot_samples = function(fit, dt, outcome.var = NULL) {
  if (is.null(outcome.var)) {
    stop("The function requires explicit declaration of an outcome variable 'outcome.var'")
  }
  if (is.null(dt)) {
    stop("The function requires explicit declaration of the original data set")
  }
  
  fn = paste0("fits/obtained/pp_",abs(as_draws_df(fit)[,1][[1]][1]),".Rdata")
  
  if (file.exists(fn)) {
    load(fn)
  } else {
    # generate data for posterior predictions
    # to remove effects of session, we replicate data such that
    # we generated predicted values for each participant and condition four times
    # one time for each session
    new_data = do.call(rbind,lapply(1:4, function(x) data.table(fit$data)[, session := x]))
    # predict probabilities that response falls into levels of the coarsened outcome variable
    
    var = gsub("\\.c","",outcome.var)
    if (outcome.var == "effect_obtained.cc") var = gsub("c$","",var)
    if (outcome.var == "effect_obtained.cn") var = gsub("n$","",var)
    
    tmp = 
      fit %>% 
      pred_cont_from_ordered(
        new_data = new_data,
        var.ordered = dt[,get(outcome.var)],
        var.continuous = dt[, get(var)])
    
    ## calculate the effect obtained by state.condition and sex
    dt.samples = 
      new_data %>% 
      .[, .(state.condition, Sex, session)] %>% 
      cbind(tmp) %>% 
      melt(id.vars = c("state.condition", "Sex", "session"),
           variable.name = "iter") %>% 
      .[, .(value = mean(value)), by = .(state.condition, Sex, iter)] %>% 
      .[, iter := as.numeric(gsub("V","",iter))]
    
    dt.samples = rbind(
      dt.samples,
      dt.samples %>% 
        .[, .(value = mean(value)), by = .(state.condition,iter)] %>% 
        .[, Sex := "all"]
    )
    
    ## contrast between stress (state.conditions), split by sex
    dt.samples.d = 
      new_data %>% 
      .[, .(state.condition, Sex, session)] %>% 
      cbind(tmp) %>% 
      melt(id.vars = c("state.condition", "Sex", "session"),
           variable.name = "iter") %>% 
      .[, .(value = mean(value)), by = .(state.condition, Sex, iter)] %>% 
      dcast(Sex + iter ~ state.condition, value.var = "value") %>% 
      .[, value := stress - control] %>% 
      .[, iter := as.numeric(gsub("V","",iter))]
    
    ## add contrasts
    ## - average effect across men and women
    ## - sex difference
    dt.samples.d = rbind(
      dt.samples.d[, .(Sex,iter,value)],
      dt.samples.d %>% 
        .[, .(value = mean(value)), by = .(iter)] %>% 
        .[, Sex := "all"],
      dt.samples.d %>% 
        dcast(iter ~ Sex, value.var = "value") %>% 
        .[, .(value = men-women, Sex = "sex diff: m-f"), by = .(iter)]
    )
    
    dt.samples.d %>% 
      .[, grp := ifelse(Sex %in% c("women","men"), "by_sex","all")]
    save(dt.samples,dt.samples.d, file = fn)
  }
  
  return(list(
    dt.samples = dt.samples,
    dt.samples.d = dt.samples.d
  ))
  
}

#' Plot Histogram of Contrasts with Half-Eye Representation
#'
#' This function creates a half-eye plot (a type of violin plot) for visualizing the distribution of 
#' contrast values. It is specifically designed to handle and differentiate data based on sex, using 
#' custom colors. The plot includes options for adjusting base text size and other aesthetics.
#'
#' @param dts A data.table object containing the posterior distributions to be plotted and additional information.
#' @param sx A vector of sex categories to be included in the plot.
#' @param base.size An optional numeric value for setting the base text size in the plot. 
#'                  If NULL, the default size is used. Defaults to NULL.
#' @return A ggplot object representing the distribution of contrast values with half-eye plots, 
#'         separated by sex categories.
diff.plotter = function(dts, sx, base.size = NULL) {
  dts[, Sex := factor(Sex, levels = sx)]
  clr.values = sex_colors[sx]
  clr.values[is.na(clr.values)] = "grey"
  
  annotations <- data.frame(
    Sex = sx,
    text = c(sx),
    X = c(Inf),
    Y =  c(Inf),
    x_adjust = c(1),
    y_adjust = c(1))
  
  p = 
    dts %>% 
    .[Sex %in% sx] %>% 
    ggplot(aes(y = value)) + 
    stat_halfeye(aes(fill = Sex, alpha = after_stat(y>0)), color  ="black") + 
    scale_fill_manual(values = clr.values) + 
    scale_alpha_manual(values = c(.2,1)) +
    theme(legend.position = "none") +
    ylab("Stress effect") + 
    coord_cartesian(ylim = quantile(dts[!is.na(Sex),value], c(.0005,.9995))) +
    geom_hline(yintercept = 0, lty = 2) + 
    facet_wrap(~Sex, ncol = 1) + 
    theme(panel.border = element_blank(),
          text = element_text(size = base.size),
          axis.line.y = element_line(color="black"),
          axis.title.x = element_blank(),
          axis.ticks.length.x = unit(0,"cm"),
          axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.text = element_blank())
    
  if (length(unique(dts$Sex)) > 1) {
    p = p + 
      geom_text(aes(x = X, y=Y,hjust=x_adjust,vjust=y_adjust,label=text),
                data = annotations, inherit.aes = FALSE)
  }
  
  return(p)
}


#' Plot Results and Calculate effects on the original VAS scale from an ordinal brms model for binned data
#'
#' This function analyzes and visualizes the results from a Bayesian regression model fitted using 
#' the 'brms' package. It calculates effects of stress on drug wanting, reports results as both a 
#' table and figures, and handles prior predictive checks. The function includes generating 
#' posterior distributions, calculating statistics, and creating density and difference plots.
#'
#' @param fit A 'brmsfit' object representing the fitted Bayesian model.
#' @param dt A data.table object containing the data used in the model.
#' @param ylim An optional numeric vector of length 2 specifying the y-axis limits for the plot. 
#'             If NULL, limits are automatically determined. Defaults to NULL.
#' @return A list containing several elements: 'stats' (a data.table with summary statistics), 
#'         'simple.stats' (basic summary statistics), 'plot' (a combined ggplot object of the 
#'         results), 'p1' and 'p2' (individual ggplot objects), and 'dt.samples' & 'dt.samples.d' 
#'         (data.tables with sample data).
#' @examples
#' # Assuming 'fit' is a brmsfit object and 'data' is a data.table
#' results = plot_results_origscale(fit, data)
plot_results_origscale = function(fit, dt, ylim = NULL) {
  outcome.var = "effect_obtained.c"
  var = outcome.var %>% gsub("\\.c","",.)
  
  fn = paste0("fits/pp_",outcome.var,"_",abs(as_draws_df(fit)[,1][[1]][1]),".Rdata")
  if (file.exists(fn)) {
    load(fn)
  } else {
    new_data = do.call(rbind,lapply(1:4, function(x) data.table(fit$data)[, session := x]))
    
    tmp = 
      fit %>% 
      pred_cont_from_ordered(
        new_data = new_data,
        var.ordered = dt[,get(outcome.var)],
        var.continuous = dt[, get(var)])
    
    ## wanting by stress
    dt.samples = 
      new_data %>% 
      .[, .(state.condition, session)] %>% 
      cbind(tmp) %>% 
      melt(id.vars = c("state.condition", "session"),
           variable.name = "iter") %>% 
      .[, .(value = fmean(value)), by = .(state.condition, iter)] %>% 
      .[, iter := as.numeric(gsub("V","",iter))]
    
    ## wanting stress contrast
    dt.samples.d = 
      new_data %>% 
      .[, .(state.condition, session)] %>% 
      cbind(tmp) %>% 
      melt(id.vars = c("state.condition", "session"),
           variable.name = "iter") %>% 
      .[, .(value = fmean(value)), by = .(state.condition, iter)] %>% 
      dcast(iter ~ state.condition, value.var = "value") %>% 
      .[, value := stress - control] %>% 
      .[, iter := as.numeric(gsub("V","",iter))]
    
    save(dt.samples,dt.samples.d, file = fn)
  }
  # needed for prior predictive
  dt.samples = dt.samples %>% na.omit()
  dt.samples.d = dt.samples.d %>% na.omit()
  
  # control direction (left/right) of posterior density plot
  dt.samples[, scl := ifelse(state.condition == "stress",.25, -.25)]
  
  if (is.null(ylim)) {
    ylim = c(floor(fmin(dt.samples[iter %in% 1:150,value])/10)*10, 120)
  }
  
  p1 =
    dt.samples %>% 
    na.omit() %>%
    ggplot(aes(x = state.condition, y = value)) + 
    geom_line(data = dt.samples[iter %in% 1:150], aes(group = factor(iter)), alpha = .15)  +
    stat_halfeye(aes(scale = scl, slab_alpha = after_stat(f)),
                 height = .5, show.legend = c(slab_alpha = FALSE, size = FALSE)) +
    stat_pointinterval() + 
    theme(legend.position = "none", axis.line = element_line(),text = element_text(size = 20)) +
    guides(y = guide_axis_truncated(
      trunc_lower = ylim[1],
      trunc_upper = ylim[2])) + 
    coord_cartesian(xlim = c(1.25,1.75), ylim = ylim) +
    ylab("% effect obtained (0-125)") +
    xlab("State condition") 
  
  contr.stats = 
    dt.samples.d %>% 
    .[, .(mean = fmean(value),
          lower = round(fquantile(value,.025)),
          upper = round(fquantile(value, .975)),
          `P > 0` = fmean(value>0),
          BF = round(fmean(value > 0)/mean(value < 0),1)),
      by = .()] %>% 
    .[, CI := paste(round(c(lower,upper),2), collapse = ", ")] 
  
  simple.stats = 
    dt.samples %>% 
    .[, .(`m (CrI)` = sprintf("%.0f (%.0f, %.0f)",
                              fmean(value),
                              fquantile(value,probs = .025),
                              fquantile(value,.975))),
      by = .(state.condition)]
  
  p2 = 
    diff.plotter(dt.samples.d[, Sex := "all"],"all") + 
    ylab("Effect of stress on drug wanting (difference % obtained)")
  
  p = p1 | p2
  
  return(list(stats = contr.stats, simple.stats = simple.stats, plot = p, p1 = p1, p2 = p2,
              dt.samples = dt.samples, dt.samples.d = dt.samples.d))
  
}



#' Stratified Analysis and Visualization of BRMS Model Results by Sex
#' Calculate effects on the original VAS scale from an ordinal brms model for binned data
#' 
#' This function extends the analysis of a BRMS model in the function `plot_results_origscale` 
#' to stratify by sex. It calculates the effects  of stress on drug wanting, stratifies the analysis by sex,
#' and reports the results in both table and figure formats. The function handles data preparation, 
#' statistical analysis, and graphical representation, including stratification and comparison by sex.
#'
#' @param fit A 'brmsfit' object representing the fitted Bayesian model.
#' @param dt A data.table object containing the data used in the model.
#' @param outcome.var A string specifying the name of the outcome variable to be analyzed.
#' @param plot.sex_avg A string that controls how sex averages are plotted: "yes" for including total 
#'                     averages, "no" for excluding, and "only" for only showing total averages.
#' @param base.size An optional numeric value for setting the base text size in the plot. 
#'                  Defaults to 22.
#' @param ylim An optional numeric vector of length 2 specifying the y-axis limits for the plot. 
#'             Defaults to NULL, allowing automatic determination.
#' @param ymax An optional numeric value specifying the maximum y-axis limit for the plot. 
#'             Defaults to 120.
#' @return A list containing several components: 'stats' and 'stats.scale' (summary statistics), 
#'         'simple.stats' (basic summary statistics), 'plot', 'p1', and 'p2' (ggplot objects), 
#'         'dt.samples' and 'dt.samples.d' (data.tables with sample data), and 'tbl_me_sex' (a flextable 
#'         object for the main effect of sex).
plot_results_origscale_sex = function(fit, dt, outcome.var = NULL, plot.sex_avg = "yes", base.size = 22, ylim = NULL, ymax = 120) {
  
  if (is.null(outcome.var)) {
    stop("The function requires explicit declaration of an outcome variable 'outcome.var'")
  }
  if (is.null(dt)) {
    stop("The function requires explicit declaration of the original data set")
  }
  
  var = outcome.var %>%
    gsub("\\.c","",.)
  
  plotting_samples = make_plot_samples(fit, dt, outcome.var)
  
  dt.samples = plotting_samples[["dt.samples"]] %>% 
    .[, scl := ifelse(state.condition == "stress",.25, -.25)] # control direction (left/right) of posterior density plot
  dt.samples.d = plotting_samples[["dt.samples.d"]] 
  
  dt.samples %>% 
    .[, Sex := gsub("man","men",Sex)] %>% 
    .[Sex == "all", Sex := "total"]
  
  all_contrasts_tbl = 
    dcast(dt.samples,formula = iter ~ Sex + state.condition, value.var = "value") %>% 
    .[, all_control := NULL] %>% 
    .[, all_stress := NULL] %>% 
    .[, women   := (women_control + women_stress)/2] %>% 
    .[, men     := (men_control + men_stress)/2] %>% 
    .[, stress  := (women_stress + men_stress)/2] %>% 
    .[, control := (women_control + men_control)/2] %>% 
    .[, `women-men` := women-men] %>% 
    .[, `stress - ctrl` := stress - control] %>% 
    .[, `stress - ctrl in men`   := men_stress-men_control] %>% 
    .[, `stress - ctrl in women` := women_stress-women_control] %>% 
    .[, `women - men in ctrl` := women_control - men_control] %>% 
    .[, `women - men in stress` := women_stress - men_stress] %>% 
    .[, three_way_interaction := `stress - ctrl in women` - `stress - ctrl in men`] %>% 
    melt(id.var = "iter") %>% 
    .[, .(stats = print_effect(value, contr = "E>0")), by = variable] %>% 
    .[, stats := gsub("95% CrI=\\[","",stats)] %>% 
    .[, stats := gsub("\\]",")", stats)] %>% 
    .[, stats := gsub("$\\)","",stats)] %>% 
    setnames(c("variable","stats"),c("Effect or contrast", "Statistics"))
  
  dt.samples.d %>% 
    .[, Sex := gsub("man","men",Sex)] %>% 
    .[Sex == "all", Sex := "total"]
  
  my.clrs = palette()[c(8,2,4)]
  names(my.clrs) = c("total","women","men")
  if(plot.sex_avg == "no") {
    dt.samples = dt.samples[Sex != "total"]
    dt.samples.d = dt.samples.d[Sex != "total"]
    my.clrs = my.clrs[-1]
  } else if (plot.sex_avg == "only") {
    dt.samples = dt.samples[Sex == "total"]
    dt.samples.d = dt.samples.d[Sex == "total"]
  } else {
    dt.samples[, grp := ifelse(Sex == "total","a","b")]
    dt.samples.d[, grp := ifelse(Sex == "total","a","b")]
  }
  
  ## wanting by stress
  stats.scale = dt.samples %>% 
    na.omit() %>%
    .[, .(mean = mean(value),
          lower = quantile(value,.025),
          upper = quantile(value, .975)),
      by = .(state.condition, Sex)] %>% 
    .[, CI := paste(round(c(lower,upper),2), collapse = ", "), by = .(Sex,state.condition)]
  
  stats.scale[, grp := ifelse(Sex == "all", "all", "by sex")]
  
  if (plot.sex_avg != "only") {
  ## main effect of sex
  tbl_me_sex = 
    dt.samples %>% 
    dcast(state.condition + iter ~ Sex, value.var = "value") %>% 
    .[, main_woman := men-women] %>% 
    .[, .(state.condition, iter, main_woman)] %>% 
    dcast(iter ~ state.condition, value.var = "main_woman")  %>% 
    .[, overall := (control+stress)/2] %>% 
    melt(id.vars = "iter", variable.name = "state condition") %>% 
    .[, .(effect_woman = get_mci(value,get.P = TRUE)), by = c("state condition")] %>% 
    my_flextable(caption = "Effect of sex, calculated as effect obtained in men - women.",
                 footnote = "Numbers are means and CIs: Upper and lower bound of 95% credible intervals")
  } else  {
    tbl_me_sex = NULL
  }
  
  if (is.null(ymax))
    ymax = ceiling(fmax(dt.samples[iter %in% 1:150,value])*1.1)
  if (is.null(ylim)) {
    ylim = c(floor(fmin(dt.samples[iter %in% 1:150,value])/10)*10, ymax)
  } else {
    yrange = layer_scales(p1)$y$range$range
    ylim = c(yrange[1]+diff(yrange)/2000,ymax)
  }
  
  p1 =
    dt.samples %>%
    na.omit() %>%
    .[, sc := as.numeric(factor(state.condition))] %>% 
    ggplot(aes(x = sc, y = value, color = Sex, group = factor(iter):factor(Sex))) + 
    geom_line(data = dt.samples[iter %in% 1:150], alpha = .15)  +
    stat_halfeye(aes(group = Sex, fill = Sex, scale = scl, slab_alpha = after_stat(f)),
                 height = .5, show.legend = c(slab_alpha = FALSE, size = FALSE)) +
    stat_pointinterval(aes(group = Sex)) + 
    theme(legend.position = "none", axis.line = element_line(),text = element_text(size = base.size)) +
    ylab("% effect obtained (0-125)") +
    xlab("") +
    scale_col_sex + 
    guides(y = guide_axis_truncated(
      trunc_lower = ylim[1],
      trunc_upper = ylim[2])) + 
    coord_cartesian(xlim = c(0.8,2.2), ylim = ylim) + 
    scale_x_continuous(breaks = c(1,2), labels = c("control","stress"))
  
  
  if (plot.sex_avg == "yes")
    p1 = p1 + facet_wrap(~grp) + theme(strip.text = element_blank())
  
  ## wanting stress contrast
  contr.stats = dt.samples.d %>% 
    .[, .(mean = mean(value),
          lower = quantile(value,.025),
          upper = quantile(value, .975),
          `P > 0` = mean(value>0),
          BF = round(mean(value > 0)/mean(value < 0),1)),
      by = .(Sex, grp)] %>% 
    .[, CI := paste(round(c(lower,upper),2), collapse = ", "), by = .(Sex)] 
  
  # estimated effect obtained
  css = function(value) {sprintf("%.0f (%.0f, %.0f)",
                                 mean(value),
                                 quantile(value,probs = .025),
                                 quantile(value,.975))}
  simple.stats = 
    dt.samples %>% 
    .[, .(`value` = css(value)), by = .(state.condition,Sex)] %>% 
    .[, Sex := factor(Sex,levels = c("men","women","total"))] %>% 
    dcast(Sex~state.condition, value.var = "value")
  

  if (plot.sex_avg == "yes") {
    p2 = diff.plotter(dt.samples.d,c("men","total","women"), base.size = base.size)
  } else if (plot.sex_avg == "no") {
    p2 = diff.plotter(dt.samples.d,c("men","women"), base.size = base.size)
  } else {
    p2 = diff.plotter(dt.samples.d,c("total"), base.size = base.size)
  }
  
  p = (p1 | p2) + plot_layout(widths = c(4,1))
  
  return(list(stats = contr.stats, simple.stats = simple.stats, stats.scale = stats.scale, plot = p, p1 = p1, p2 = p2,
              dt.samples = dt.samples, dt.samples.d = dt.samples.d, tbl_me_sex = tbl_me_sex, all_contrasts_tbl = all_contrasts_tbl))
}

#' Plot and Analyze Buffer Effect from BRMS Model
#' Effects are calculated on the original VAS, based on an ordinal brms model for binned data
#'
#' This function analyzes and visualizes the buffer effect from a Bayesian regression model fitted 
#' using the 'brms' package. It includes processing the model's posterior predictions, creating 
#' various plots to illustrate the buffer effect, and calculating relevant statistics. The function 
#' handles data preparation, statistical analysis, and graphical representation, focusing on the 
#' contrast between conditions and groups (e.g., sex, drug type).
#'
#' @param fit A 'brmsfit' object representing the fitted Bayesian model.
#' @param the_data A data.table object containing the original dataset used in the model.
#' @param var A string specifying the variable used for contrast in the buffer effect analysis.
#' @return A list containing 'buffer.stats' (summary statistics for buffer effects), 'stress.plot' 
#'         (a plot showing stress effects), 'buffer.plot' (a plot illustrating buffer effects), and 
#'         'ppc' (posterior predictive check plot).
plot_buffer_effect = function(fit,the_data,var) {
  tmp = fit$fit %>% as_draws()
  fn = paste0("fits/buffer/contr_",var,tmp[1],".Rdata")
  rm(tmp)
  gc()
  if (!file.exists(fn)) {
    new_data = do.call(rbind,lapply(1:4, function(x) data.table(fit$data)[, session := x]))
    epred.data = fit$data %>% data.table() %>% .[, response.c := NULL]
    # average response on 0-100 scale in response categories
    obs.ratings = the_data[,.(r = mean(response)), by = .(response.c)][,r] %>% sort()
    batches = (fit$fit@sim$iter-fit$fit@sim$warmup)*4/500
    pp_basic_l = vector(mode = "list", length = batches)
    pp_l = vector(mode = "list", length = batches)
    for(j in 1:batches) { # circumvent memory problems
      my_draw_ids = as.integer((1:500)+((j-1)*500))
      epred = posterior_epred(fit, newdata = new_data, draw_ids = my_draw_ids)
      # calculate expected score per row in data
      epred.r = 
        apply(epred, 1, function(a) apply(a, 1, function(p) fsum(obs.ratings, w = p)))
      rm(epred)
      gc()
      
      pp_basic_l[[j]] = 
        cbind(epred.data, epred.r) %>% 
        melt(id.vars = names(epred.data), variable.name = "iter") %>% 
        .[, .(value = collapse::fmean(value)), by = .(Sex,Drug,Stress, reminder,iter)] %>% 
        .[, iter := NULL] %>% 
        .[, iter := my_draw_ids, by = .(Sex,Drug,Stress, reminder)] %>% 
        dcast(iter ~ Sex + Drug + Stress + reminder, value.var = "value", sep = ":", fun.aggregate = mean) %>% 
        # stress-effect = post-pre difference
        .[, .(m_pp_plac_contr = `men:placebo:control:post` - `men:placebo:control:pre`,
              m_pp_plac_stress = `men:placebo:stress:post` - `men:placebo:stress:pre`,
              m_pp_oxy_contr = `men:oxycodone:control:post` - `men:oxycodone:control:pre`,
              m_pp_oxy_stress = `men:oxycodone:stress:post` - `men:oxycodone:stress:pre`,
              f_pp_plac_contr = `women:placebo:control:post` - `women:placebo:control:pre`,
              f_pp_plac_stress = `women:placebo:stress:post` - `women:placebo:stress:pre`,
              f_pp_oxy_contr = `women:oxycodone:control:post` - `women:oxycodone:control:pre`,
              f_pp_oxy_stress = `women:oxycodone:stress:post` - `women:oxycodone:stress:pre`),
          by = .(iter)]
      
      tmp = 
        pp_basic_l[[j]] %>% 
        # stress-effects by sex and drug
        .[, .(m_plac_sr = m_pp_plac_stress - m_pp_plac_contr,
              m_oxy_sr = m_pp_oxy_stress - m_pp_oxy_contr,
              f_plac_sr = f_pp_plac_stress - f_pp_plac_contr,
              f_oxy_sr = f_pp_oxy_stress - f_pp_oxy_contr), by = .(iter)] %>% 
        # state-condition X stress interaction 
        .[, .(men = m_oxy_sr - m_plac_sr,
              women = f_oxy_sr - f_plac_sr), by = .(iter)] %>% 
        .[, avg := (men+women)/2] %>% 
        .[, `women-men` := (women-men)] %>% 
        melt(id.var = "iter", value.name = "buffer_drug_effect", variable.name = "Sex")
      pp_l[[j]] = tmp
      rm(tmp)
      gc()
    }
    
    pp = do.call(rbind,pp_l)
    pp_basic = do.call(rbind,pp_basic_l)
    rm(pp_l,pp_basic_l)
    gc()
    save(pp,pp_basic, file = fn)
  } else {
    load(fn)
  }
  
  tmp =
    pp_basic %>% 
    melt(id.vars = "iter") %>% 
    .[, variable := gsub("_pp","",variable)] %>% 
    .[, c("Sex","Drug","state.condition") := tstrsplit(variable,"_")] %>% 
    .[, Sex := ifelse(Sex == "m","men","women")] %>% 
    .[, Drug := ifelse(Drug == "plac","placebo","oxycodone")] %>% 
    .[, state.condition := ifelse(state.condition == "stress","stress","control")] %>% 
    .[, variable := NULL]
  
  obs_diff = 
    the_data %>% 
    dcast(participant + Drug + state.condition + Sex ~ stage, value.var = "response", fun.aggregate = mean) %>% 
    .[, .(diff = mean(`6`-`5`)), by = .(Drug, state.condition, Sex)] 
  
  ppc = 
    obs_diff %>% 
    ggplot(aes(x = state.condition, y=diff, color = Drug)) + 
    geom_point(position = position_dodge(0.2)) + 
    facet_wrap(~Sex) + 
    geom_line(aes(group=Drug),position = position_dodge(0.2)) + 
    stat_interval(data = tmp[, diff := value],alpha = .25,
                  position = position_dodge(0.2)) + 
    ylab("post-pre reminder difference")
  
  pp_cells =
    tmp %>% 
    dcast(iter + Drug + Sex ~ state.condition) %>% 
    .[, value := stress-control] %>% 
    dcast(iter + Sex ~ Drug) %>% 
    .[, .(stress = (oxycodone+placebo)/2,
          `stress in placebo` = placebo,
          `stress in oxycodone` = oxycodone,
          buffer = oxycodone-placebo), by = .(iter,Sex)] %>% 
    melt(id.vars = c("iter","Sex")) %>% 
    dcast(iter + variable ~ Sex) %>% 
    .[, total := (men+women)/2] %>% 
    .[, `Sex difference` := women-men] %>% 
    melt(id.vars = c("iter","variable"), variable.name = "Sex") %>% 
    setnames("variable","contrast")
  
  stress_effects = 
    pp_basic %>% 
    .[, .(m_plac_sr = m_pp_plac_stress - m_pp_plac_contr,
          m_oxy_sr = m_pp_oxy_stress - m_pp_oxy_contr,
          f_plac_sr = f_pp_plac_stress - f_pp_plac_contr,
          f_oxy_sr = f_pp_oxy_stress - f_pp_oxy_contr), by = .(iter)] %>% 
    .[, iter := 1:nrow(pp_basic)] %>% 
    melt(id.var = "iter", value.name = "stress_effect") %>% 
    .[, c("Sex","Drug","x") := tstrsplit(variable,"_")] %>% 
    .[, Sex := ifelse(Sex == "m","men","women")] %>% 
    .[, Drug := ifelse(Drug == "plac","placebo","oxycodone")] %>% 
    .[, x := NULL]
  
  stress.plot =
    stress_effects %>% 
    ggplot(aes(x = Sex, y = stress_effect, color = Drug)) +
    stat_pointinterval(position = position_dodge(.2)) + 
    geom_hline(yintercept = 0, col = "red", lty = 2) + 
    scale_col_drug
  
  buffer.plot =
    pp %>% 
    .[, Sex := gsub("man","men",Sex)] %>% 
    .[Sex == "women-men", Sex := "Sex difference"] %>% 
    .[Sex == "avg", Sex := "total"] %>% 
    .[, Sex := factor(Sex, levels = c("men","women","total","Sex difference"))] %>% 
    ggplot(aes(x = buffer_drug_effect, fill = Sex)) + 
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = c(-5,5), lty = 2) +
    stat_halfeye(alpha = .5) + 
    facet_wrap(~Sex, scales = "free") +
    ylab("") +
    gg_no_y_axis +
    scale_col_sex.b
  
  buffer.stats = 
    pp_cells %>% 
    .[, .(stats = get_mci(value)), by = .(Sex,contrast)] %>% 
    dcast(Sex ~ contrast, value.var = "stats")
  
  return(list(buffer.stats = buffer.stats,
              stress.plot = stress.plot,
              buffer.plot = buffer.plot,
              ppc = ppc))
}

#' Retrieve and Process Timing Data
#'
#' This function loads and processes timing data from a specific dataset. It filters and reshapes 
#' the data to focus on key activities, calculates mean times, and determines the start and end 
#' times for each activity. The function is designed to work with a pre-specified dataset loaded 
#' from an Rdata file.
#'
#' @return A list containing two elements: 'tx', a data.table with processed timing data for 
#'         selected activities, and 'TS', the original dataset filtered to exclude certain activities.
get_timing_data = function() {
  load("data/Times.Rdata")
  tx = TS[Activity %in% c("state_prepmainmath","drug_1_admin","drug_2_admin", "reminder_1","reminder_2"),
          .(Activity, Activity_mid, Activity_start, Activity_end)] %>% 
    setnames(c("Activity_start", "Activity_end","Activity_mid"),c("xmin","xmax","time")) %>% 
    .[!is.na(xmin), xmid := NA] %>% 
    .[, .(time = mean(time), xmin = min(xmin), xmax = max(xmax)), by = .(Activity)]
  
  tx = tx[Activity != "reminder_2"]
  TS = TS[Activity != "reminder_2"]
  return(list(tx = tx, TS = TS))
}

get_timing_data() %>% list2env(envir = .GlobalEnv)

#' Create Stage-Based Plots with Optional Group Stratification
#'
#' This function generates a plot based on stages from a given dataset. It allows for stratification 
#' by sex and/or drug type. The function calculates within-participant standard error and confidence 
#' intervals, then creates a plot showing the average response across stages with options for 
#' differentiating by state condition, drug type, or sex.
#'
#' @param dt A data.table object containing the dataset.
#' @param my_stages A numeric vector specifying the stages to be included in the plot.
#' @param by.Sex A logical value indicating whether to stratify the plot by sex.
#' @param ymin.min A numeric value specifying the minimum y-axis limit for the plot.
#' @param by.Drug A logical value indicating whether to stratify the plot by drug type.
#' @param fill A string specifying the column name in `dt` used for fill color in the plot.
#' @param shape A string specifying the column name in `dt` used for point shapes in the plot.
#' @param lty A string specifying the column name in `dt` used for line types in the plot.
#' @return A ggplot object representing the average response across specified stages, with optional 
#'         stratifications and visual encodings.
#' @details The function allows the user to create a customized plot to visualize stages of data. It supports 
#' grouping by Sex and Drug and provides options to control the appearance of points, lines, and colors in the plot.
#'
plot_stages = function(dt, my_stages = 1:10, by.Sex = FALSE, ymin.min = 0, by.Drug = FALSE, 
                       fill = "state.condition", shape = "state.condition", lty = "state.condition") {
  bw = NULL
  if (by.Sex != FALSE) bw = "Sex"
  wi = c("state.condition","stage")
  if (by.Drug != FALSE) wi = c(wi,"Drug")
  pdata = 
    seWithin(
      data = dt[stage %in% my_stages][, state.condition := factor(state.condition)][,stage := factor(stage)],
      measurevar = "response",
      withinvars = wi,
      betweenvars = bw,
      idvar = "participant",
      conf.interval = .95
    ) %>% 
    data.table() %>% 
    .[, Activity := paste0("Q",stage)] %>% setkeyv("Activity") %>% 
    .[TS, time := Activity_start] %>% 
    .[, `:=`(lower = response-2*se, upper = response+2*se)]
  
  
  gg.aes = aes(x = time, y = response, shape = .data[[shape]], color = .data[[fill]], 
                 fill = .data[[fill]], lty = .data[[shape]])
  scale_col = scale_col_stress
  if (by.Drug == TRUE) {
    scale_col = scale_col_drug
  }
  
  
  tx[, `:=`(ymin = min(c(ymin.min,pdata$lower),na.rm = TRUE)*0.5,
            ymax = min(c(100,max(pdata$upper)),na.rm = TRUE)*1.5,
            response = mean(pdata$response), 
            state.condition = pdata$state.condition[1],
            Drug = "placebo",
            Sex = "women")]
  tx = rbind(copy(tx)[, Sex := "men"],tx[, Sex := "women"])
  tx = tx[!Activity == "reminder_2"]
  p = 
    pdata %>% 
    ggplot(gg.aes) + 
    plot_exp(y.max = max(pdata$upper, na.rm = TRUE)) + 
    geom_line() + 
    geom_point() + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2, color = NA) + 
    scale_x_continuous(breaks = seq(-60,80,20)) + 
    theme(panel.grid.major.y = element_blank(),
          axis.line = element_line(),
          legend.key.size = unit(1,"line")) +
    coord_cartesian(ylim = c(min(ymin.min,pdata$lower, na.rm = TRUE),max(pdata$upper))) + 
    theme(axis.line = element_line()) +
    scale_col +
    scale_y_continuous(expand = c(0,0,0.06,.25)) + 
    ylab("VAS response (0-100)")
  
    yrange = c(min(pdata$lower, na.rm = TRUE),max(pdata$upper, na.rm = TRUE))
    
    if (is.na(ymin.min))
      p = p + 
      guides(y = guide_axis_truncated(
        trunc_upper = yrange[2],
        trunc_lower = yrange[1]+diff(yrange)/500))
    p
}

#' Create Modified Data Table
#'
#' This function modifies a given data table by selecting specific stages and phases of the data.
#'
#' @param dt A data.table object containing the data.
#' @param phase A character string specifying the data phase to be selected (e.g., "induction", "reminder", "drug").
#' @param n.post.stages The number of post-stages to include in the selection when the phase is "induction". Default is 1.
#'
#' @return A modified data.table object with selected stages and phase.
#'
#' @details This function allows the user to create a modified data table by selecting specific stages and 
#' phases of the data. The 'phase' argument specifies which phase to select, and 'n.post.stages' controls 
#' the number of post-stages to include when the phase is "induction".
make_the_data = function(dt, phase = "induction", n.post.stages = 1) {
  if (phase == "induction") {
    pre.stage = 3
    stages = pre.stage:(pre.stage + n.post.stages) %>% as.character()
  } else if (phase == "reminder") {
    pre.stage = c(5,7)
    stages = c(5:8)  %>% as.character()
  }  else if (phase == "reminder1") {
    pre.stage = c(5)
    stages = c(5:6)  %>% as.character()
  } else if (phase == "drug") {
    pre.stage = c(4)
    stages = c(4:5)  %>% as.character()
  } else if (phase == "stressdrug") {
    pre.stage = 1:3
    stages = c(1:3,5)  %>% as.character()
  }
  
  
  stage.nms = paste0(c("pre.","post."),phase)
  dt = 
    dt %>% 
    .[stage %in% stages] %>% 
    .[, orig.stage := stage] %>% 
    .[, stage := as.character(stage)] %>% 
    .[, stage := ifelse(stage %in% pre.stage,stage.nms[1],stage.nms[2])] %>% 
    .[, stage := factor(stage, levels = stage.nms)]
  
  tbl = 
    dt[, .(N = length(unique(participant))), by = .(stage, Drug, state.condition)] %>% 
    dcast(Drug + state.condition ~ stage, value.var = "N") %>% 
    my_flextable(caption = "Number of participants per condition by condition and stage")
  
  setattr(dt,"N",tbl)
  
  return(dt)
}


#' Plot Pre and Post VAS Ratings from raw data
#'
#' This function generates a plot to visualize VAS ratings before and after experimental manipulations.
#'
#' @param dt A data.table object containing the data.
#' @param phase A character string specifying the experimental phase to plot (e.g., "induction", "drug").
#' @param by.Sex A logical value indicating whether to facet the plot by Sex (TRUE) or not (FALSE). Default is FALSE.
#'
#' @return A plot showing VAS ratings before and after experimental manipulations.
#'
#' @details The function allows the user to create a plot that visualizes VAS ratings before and after 
#' experimental manipulations. It supports different plotting configurations based on the 'phase' argument 
#' and provides the option to facet the plot by Sex.
plot_pre_post = function(dt, phase = "induction", by.Sex = FALSE) {
  if (phase == "drug") {
    p.aes = aes(x = x, y = response, color = Drug,
                group = factor(participant):factor(session))
    p.wrap = facet_wrap(~Drug)
    if (by.Sex == TRUE)
      p.wrap = facet_grid(Sex ~ Drug)
    scale_col = scale_col_drug
  } else if (phase == "drug:induction") {
    p.aes = aes(x = x, y = response, color = Drug,
                group = factor(participant):factor(session):factor(state.condition))
    p.wrap = facet_wrap(~state.condition)
    if (by.Sex == TRUE)
      p.wrap = facet_grid(Sex ~ Drug)
    scale_col = scale_col_drug
  } else {
    p.aes = aes(x = x, y = response, color = state.condition,
                group = factor(participant):factor(session))
    p.wrap = facet_wrap(~state.condition)
    if (by.Sex == TRUE)
      p.wrap = facet_grid(Sex ~ state.condition)
    scale_col = scale_col_sex
  }
  
  p = 
    dt %>% 
    .[, .(response = mean(response)),
      by = .(stage,Sex,state.condition, participant, session, Drug)] %>% 
    .[, x := ifelse(grepl("pre",stage),1,2)] %>% 
    .[, x := x + runif(1,-.1,.1), by = .(participant, session)] %>% 
    ggplot(p.aes) + 
    geom_line(alpha = .25) + 
    geom_point(alpha = .5) +
    scale_x_continuous(breaks = c(1,2), labels = c("pre","post")) +
    xlab("") + 
    p.wrap + 
    ylim(0,100) + 
    scale_col + 
    ylab("VAS response (0-100)") + 
    xlab(stringr::str_to_title(phase))
  
  return(p)
} 

#' Plot Density of Drug Effects in Raw Data
#'
#' This function generates a plot to visualize the density of drug effects in raw data, calculated as the 
#' difference of pre-post ratings between the drug and control condition.
#'
#' @param the_data A data.table object containing the raw data.
#'
#' @return A plot showing the density of drug effects in raw data.
#'
#' @details The function calculates the difference of pre-post ratings between the drug and control condition 
#' for each participant and visualizes the density of these drug effects. The plot is facetted by 'state.condition' 
#' and includes vertical lines at x = 0 to indicate no effect.
plot_drug_effect_raw = function(the_data) {
  the_data %>%  
    .[, .(response = mean(response)),
      by = .(stage,Sex,state.condition, participant, session, Drug)] %>% 
    dcast(participant + Sex + Drug + state.condition ~ stage, value.var = "response", fun.aggregate = mean)  %>% 
    .[, stage_diff := post.drug - pre.drug] %>% 
    dcast(participant + Sex + state.condition ~ Drug, value.var = "stage_diff")  %>%
    .[, drug_effect := oxycodone - placebo] %>% 
    ggplot(aes(x = drug_effect, fill = Sex)) + 
    facet_wrap(~state.condition) +
    geom_density(alpha = .25, color = NA) + 
    geom_vline(xintercept = 0) +
    ggtitle("drug_effect = rating_in_oxycodone-rating_in_placebo") + 
    scale_col_sex
}

#' Plot Density of Stress Effects in Raw Data
#'
#' This function generates a plot to visualize the density of stress effects in raw data, calculated as the 
#' difference of pre-post ratings between the stress and control condition.
#'
#' @param the_data A data.table object containing the raw data.
#' @param by.Sex A logical value indicating whether to facet the plot by Sex (TRUE) or not (FALSE). Default is TRUE.
#'
#' @return A plot showing the density of stress effects in raw data.
#'
#' @details The function calculates the difference of pre-post ratings between the stress and control condition 
#' for each participant and visualizes the density of these stress effects. The plot can be facetted by Sex, 
#' and vertical lines at x = 0 indicate no effect.
plot_stresseff_raw = function(the_data, by.Sex = TRUE) {
  if (by.Sex == FALSE) {
    dt.by = c("stage","state.condition", "participant", "session", "Drug")
    dt.cast1 = participant + Drug + state.condition ~ stage
    dt.cast2 = participant + Drug ~ state.condition
    by.tbl = c("Drug")
    gg.aes = aes(x = stress_effect, fill = "1")
    scale_col = list(
      scale_fill_manual(values = c("grey25")), 
      scale_color_manual(values = c("grey25"))  
    )
  } else {
    dt.by = c("stage","Sex","state.condition", "participant", "session", "Drug")
    dt.cast1 = participant + Sex + Drug + state.condition ~ stage
    dt.cast2 = participant + Sex + Drug ~ state.condition
    by.tbl = c("Drug","Sex")
    gg.aes = aes(x = stress_effect, fill = Sex)
    scale_col = scale_col_sex
  }
  
  postpre = unique(the_data$stage) %>% as.character() %>% sort()
  pdata = 
    the_data %>%  
    .[, .(response = mean(response)), by = dt.by] %>% 
    dcast(dt.cast1, value.var = "response", fun.aggregate = mean)  %>% 
    .[, stage_diff := get(postpre[2]) - get(postpre[1])] %>% 
    dcast(dt.cast2, value.var = "stage_diff")  %>%
    .[, stress_effect := control - stress] 
  
  tbl = pdata %>% 
    seWithin(
      measurevar = "stress_effect",
      withinvars = by.tbl,
      idvar = "participant")
  
  p = 
    pdata %>% 
    ggplot(gg.aes) + 
    facet_wrap(~Drug) +
    geom_density(alpha = .25, color = NA) + 
    geom_vline(xintercept = 0) +
    ggtitle("stress effect = rating_in_stress-rating_in_control") + 
    scale_col + 
    xlab("stress effect")
  
  attr(p,"table") = tbl
  
  return(p)
}


#' Plot Density of Stress Effects in Raw Data (Version 2)
#'
#' This function generates a plot to visualize the density of stress effects in raw data, calculated as the 
#' difference of pre-post ratings between the stress and control condition.
#'
#' @param the_data A data.table object containing the raw data.
#' @param by.Sex A logical value indicating whether to facet the plot by Sex (TRUE) or not (FALSE). Default is TRUE.
#'
#' @return A plot showing the density of stress effects in raw data.
#'
#' @details The function calculates the difference of pre-post ratings between the stress and control condition 
#' for each participant and visualizes the density of these stress effects. The plot can be facetted by Sex, 
#' and error bars represent the 95% confidence interval of the effect.
plot_stresseff_raw2 = function(the_data, by.Sex = TRUE) {
  if (by.Sex == FALSE) {
    dt.by = c("stage","state.condition", "participant", "session", "Drug")
    dt.by2 = c("Drug")
    dt.cast1 = participant + Drug + state.condition ~ stage
    dt.cast2 = participant + Drug ~ state.condition
    gg.aes = aes(x = Drug, y = stress_effect, color = Drug, fill = Drug)
    scale_col = list(
      scale_fill_manual(values = c("grey25")), 
      scale_color_manual(values = c("grey25"))  
    )
  } else {
    dt.by = c("stage","Sex","state.condition", "participant", "session", "Drug")
    dt.by2 = c("Sex", "Drug")
    dt.cast1 = participant + Sex + Drug + state.condition ~ stage
    dt.cast2 = participant + Sex + Drug ~ state.condition
    gg.aes = aes(x = Drug, y = stress_effect, fill = Drug, color = Drug)
    scale_col = scale_col_drug
  }
  
  postpre = unique(the_data$stage) %>% as.character() %>% sort()
  pdata.i =
    the_data %>%  
    .[, .(response = mean(response, na.rm = TRUE)),
      by = .(stage,Sex,state.condition, participant, session, Drug)] %>% 
    dcast(dt.cast1, value.var = "response", fun.aggregate = mean)  %>% 
    .[, stage_diff := get(postpre[2]) - get(postpre[1])]  %>% 
    dcast(dt.cast2, value.var = "stage_diff") %>% 
    .[, stress_effect := control - stress]
  pdata.stats = 
    pdata.i %>% 
    .[, .(stress_effect = mean(stress_effect, na.rm = TRUE),
          sd = sd(control - stress, na.rm = TRUE),
          N = length(unique(participant))), by  = dt.by2] %>% 
    .[, se := sd/sqrt(N)] %>% 
    .[, lower := stress_effect - 2*se] %>% 
    .[, upper := stress_effect + 2*se] 
  
  p = 
    pdata.stats %>% 
    ggplot(gg.aes) + 
    geom_bar(stat = "identity", alpha = .75, color = NA) + 
    geom_errorbar(aes(ymin = lower, ymax = upper), linewidth = 1, color = "black", width = .2) + 
    geom_quasirandom(data = pdata.i) +
    scale_col_drug + 
    guides(fill="none") + 
    ylab("Effect of stress induction")
  if (by.Sex == TRUE) p = p + facet_wrap(~Sex)
  return(p)
  
}

#' Calculate Effects from Posterior Predictions of brms Model and Produce Results
#'
#' This function calculates effects on the original VAS scale from the posterior predictions 
#' of an ordinal brms model for binned data and produces results in the form of density plots and tables.
#'
#' @param fit The fitted brms model.
#' @param the_data A data.table object containing the raw data.
#' @param outcome A character string specifying the outcome variable.
#' @param my_contrast A character string specifying the contrast to calculate (e.g., "stress" or "drug").
#'
#' @return A list containing:
#'   - plot: A plot showing the density of the calculated effect.
#'   - post_stress_effect: A data.table with posterior predictions of the effect.
#'   - table: A flextable object summarizing the results.
#'   - summary_table: A data.table summarizing the results.
#'
#' @details This function calculates the specified contrast effect from the posterior predictions of a brms model 
#' and provides the results in the form of a density plot, a data.table, and a summary table. It can calculate 
#' effects for both "stress" and "drug" contrasts.
plot_prepost_contrast = function(fit,the_data,outcome, my_contrast = NULL) {
  stopifnot(!is.null(my_contrast))
  my_effect = paste0(my_contrast,"_effect")
  tmp = fit %>% as_draws_df() %>% .[1,1] %>% as.numeric() %>% round(5)*100000
  
  if (any(grepl("reminder",the_data$stage))) {
    fn = paste0("fits/Reminder/",outcome,"_",tmp,".Rdata")
  } else {
    fn = paste0("fits/",outcome,"_contrasts_",
                paste(sort(unique(the_data$stage)), collapse = "_"),"_",
                "_",tmp,".Rdata")
  }
  
  if (file.exists(fn)) {
    load(fn)
  } else {
    # names of pre and post manipulation stages
    postpre = unique(the_data$stage) %>% as.character() %>% sort()
    # posterior expectations, here probabilities for response categories
    new_data = do.call(rbind,lapply(1:4, function(x) data.table(fit$data)[, session := x]))
    epred = posterior_epred(fit, newdata = new_data)
    # average response on 0-100 scale in response categories
    obs.ratings = the_data[,.(r = mean(response)), by = .(response.c)][,r] %>% sort()
    
    # calculate expected score per row in data
    epred.r = 
      apply(epred, 1, function(a) apply(a, 1, function(p) fsum(obs.ratings, w = p)))
    
    # posterior predictive (sanity) check
    # plot(the_data$response, rowMeans(epred.r))
    
    # get relevant condition info from the data
    if (my_contrast == "stress") {
      epred.data = 
        fit$data %>% data.table() %>% 
        .[, .(induced.state, stage, Sex, participant, session)] %>% 
        .[, state.condition := ifelse(any(induced.state == "stress"),"stress","control"),
          by = .(participant,session)] %>% 
        .[, induced.state := NULL] %>% 
        .[, condition := state.condition]
      c.1 = "stress"
      c.2 = "control"
    } else if (my_contrast == "drug") {
      epred.data = 
        fit$data %>% data.table() %>% 
        .[, .(Drug, stage, Sex, participant, session)] %>% 
        .[, condition := Drug]
      c.1 = "oxycodone"
      c.2 = "placebo"
    }
    
    # merge posterior predictions with condition info from the data
    # calculate stress effects and 
    pp = 
      cbind(epred.data, epred.r) %>% 
      melt(id.vars = names(epred.data), variable.name = "iter") %>% # everything in long format
      dcast(participant + Sex + condition + iter ~ stage, # pre/post manipulation stages in columns
            value.var = "value", fun.aggregate = mean)
    
    pp = 
      pp %>% 
      .[, stage_diff := get(postpre[1]) - get(postpre[2])] %>% # calculate pre/post manipulation scores
      dcast(participant + Sex + iter ~ condition, value.var = "stage_diff") %>%  # manipulation conditions in columns
      .[, effect := get(c.1) - get(c.2)] %>% # calculate effect of manipulation
      .[, .(effect = mean(effect),
            c.1 = mean(get(c.1)),
            c.2 = mean(get(c.2))),
        by = .(Sex,iter)]  %>% # effect of manipulation by sex
      setnames(c("c.1","c.2","effect"), c(c.1,c.2,my_effect)) %>% 
      melt(id.vars = c("Sex","iter"), variable.name = "contrast")
    
    # sex difference in effect
    sex.diff = 
      pp %>% 
      dcast(iter + contrast ~ Sex, value.var = "value") %>% 
      .[, value := women-men] %>% 
      .[,.(iter,contrast,value)] %>% 
      .[, Sex := "women-men"]
    
    # average effect over sexes
    sex.avg = 
      pp %>% 
      dcast(iter + contrast ~ Sex, value.var = "value") %>% 
      .[, value := (women+men)/2] %>% 
      .[,.(iter,contrast,value)] %>% 
      .[, Sex := "avg"]
    
    # put all effects in one dt
    pp = 
      rbind(pp, sex.diff, sex.avg) %>% 
      .[, Sex := factor(Sex, levels = c("men","women","women-men","avg"))]
    save(pp,file = fn)
  }
  ## rename some values
  new.nms = c("men","women","total","Sex difference")
  pp %>% 
    .[, Sex := gsub("man","men",Sex)] %>% 
    .[Sex == "avg", Sex := "total"] %>% 
    .[Sex == "women-men", Sex := "Sex difference"] %>% 
    .[, Sex := factor(Sex, levels = new.nms)]
  
  post = pp[contrast == my_effect]
  p =
    post %>% 
    ggplot(aes(x = value, color = Sex, fill = Sex)) + 
    geom_vline(xintercept = 0) +
    stat_halfeye(alpha = .5) + 
    facet_wrap(~Sex, scales = "free", nrow = 1) +
    ylab("") + 
    xlab(paste(my_effect,"for the item",outcome)) +
    gg_no_y_axis +
    scale_col_sex.b
  my_caption = 
    ifelse(my_contrast == "stress",
           "Modeled post- minus pre state induction ratings of xx by condition -control, stress- and the stress effect by sex.",
           "Modeled post- minus pre drug administration ratings of xx by condition -placebo, oxycodone- and the drug effect by sex.")
  my_caption = gsub("xx",outcome,my_caption)
  
  tbl = 
    pp %>% 
    .[, .(stats = get_mci(value, get.P = FALSE)), by = .(Sex,contrast)] %>% 
    .[, contrast := factor(contrast,levels = c("placebo","control","oxycodone","stress",my_effect))] %>% 
    dcast(Sex~contrast, value.var = "stats") %>% 
    my_flextable(caption = my_caption, footnote = "95% credible intervals in parentheses.") 
  
  summary_tbl = 
    pp %>% 
    .[, .(stats = get_mci(value, get.P = ifelse(grepl("-|difference",Sex),TRUE,FALSE))), by = .(Sex,contrast)] %>% 
    .[, contrast := factor(contrast,levels = c("placebo","control","oxycodone","stress",my_effect))] %>% 
    dcast(contrast~Sex, value.var = "stats") %>% 
    .[contrast == my_effect] %>% 
    .[,new.nms,with = FALSE] 
  summary_tbl = cbind(Outcome = outcome, summary_tbl)
  
  return(list(plot = p,post_stress_effect = post, table = tbl, summary_table = summary_tbl))
}

#' Plot Density of Drug Effects in Raw Data
#'
#' This function generates a plot to visualize the density of drug effects in raw data, calculated as the 
#' difference of ratings between the oxycodone and placebo conditions.
#'
#' @param the_data A data.table object containing the raw data.
#'
#' @return A plot showing the density of drug effects in raw data.
#'
#' @details The function calculates the difference of ratings between the oxycodone and placebo conditions for each 
#' participant and visualizes the density of these drug effects. The plot is facetted by 'state.condition' and includes 
#' vertical lines at x = 0 to indicate no effect.
plot_drug = function(the_data) {
  the_data %>% 
    .[, .(r = mean(response)), by = .(participant, Sex, state.condition, Drug)] %>% 
    dcast(participant + Sex + state.condition ~ Drug, value.var = "r") %>% 
    .[, drug_effect := oxycodone-placebo] %>% 
    ggplot(aes(x = drug_effect, fill = Sex)) + 
    facet_wrap(~state.condition) +
    geom_density(alpha = .25, color = NA) + 
    geom_vline(xintercept = 0) +
    ggtitle("drug_effect = rating_in_oxycodone-rating_in_placebo") + 
    scale_col_sex
}

#' Plot Drug Effects Over Time with Lines from raw data
#'
#' This function generates a plot to visualize the drug effects over time with lines connecting the data points.
#'
#' @param the_data A data.table object containing the raw data.
#'
#' @return A plot showing drug effects over time with lines.
#'
#' @details The function calculates and plots the drug effects for each participant over time (Drug vs. response) 
#' with lines connecting the data points. The plot is facetted by 'state.condition' and color-coded by 'Sex'.
plot_drug_lines = function(the_data) {
  if (length(unique(the_data$item_name)) > 1) {
    p.data = copy(the_data) %>% 
      .[, response := mean(response), by = .(participant,Drug,stage,state.condition)]
  } else {
    p.data = copy(the_data)
  }
    
  p.data %>% 
    .[, Drug := factor(Drug, levels = c("placebo","oxycodone"))] %>% 
    ggplot(aes(x = Drug, y = response, color = Sex, group = factor(participant):factor(stage))) + 
    geom_line(alpha = .5) +
    facet_wrap(~state.condition) + 
    scale_col_sex
}

#' @param fit The fitted model object (e.g., from brms).
#' @param new_data A data.frame or data.table containing the new data for prediction.
#' @param var.ordered The name of the ordered categorical variable used in modeling.
#' @param var.continuous The name of the continuous variable used in modeling.
#'
#' @return A matrix containing the predicted continuous outcome.
#'
#' @details The function calculates the predicted continuous outcome by first calculating the posterior 
#' predictions of probabilities for each (ordered) response category, multiplying these probabilities with 
#' the average VAS value for each response category, and finally summing these products of response-category 
#' probabilities and mean-responses.
pred_cont_from_ordered = function(fit, new_data = NULL, var.ordered, var.continuous, parallelise = TRUE) {
  epred = posterior_epred(fit, newdata = new_data)
  obs.ratings = 
    data.table(
      ord = var.ordered,
      cont = var.continuous) %>% 
    .[,.(r = mean(cont)), by = .(ord)] %>% 
    .[,r] %>% 
    sort()
  
  if (parallelise == TRUE) {
    n_splits = ifelse(Sys.info()["sysname"] == "Windows",2,4)
    splits = split(1:nrow(epred),ceiling(seq_along(1:nrow(epred))/(nrow(epred)/n_splits)))
    epred.splits = lapply(splits,function(x) epred[x,,])
    
    ord2cont = function(epred,obs.ratings) {
      apply(epred, 1, function(a) apply(a, 1, function(p) collapse::fsum(obs.ratings, w = p)))
    }
    
    library(future.apply)
    options(future.globals.maxSize = 40 * 1024^3)
    plan(multisession, workers = n_splits)
    epred.cont.c = 
      do.call(cbind,
              future_lapply(epred.splits, FUN = ord2cont, obs.ratings = obs.ratings))
  } else {
    epred.cont.c = apply(epred, 1, function(a) apply(a, 1, function(p) collapse::fsum(obs.ratings, w = p)))
  }
  return(epred.cont.c)

  
}

#' Calculate Drug Effects on the original scale from an ordinal brms Model for binned data
#'
#' This function calculates the drug effects from a brms model and reports the results in the form of figures and tables.
#'
#' @param fit The fitted brms model object.
#' @param the_data A data.table object containing the raw data used for modeling.
#'
#' @return A data.table containing the calculated drug effects.
#'
#' @details The function calculates drug effects from a brms model using posterior predictions. It also generates 
#' figures and tables summarizing the drug effects. The results include various contrasts such as the effect of 
#' oxycodone vs. placebo in different conditions and by sex, as well as other relevant statistics.
get_drug_contrasts = function(fit, the_data) {
  fn = paste0("fits/drug/contrasts_",the_data$item_name[1],"_",fit$fit@sim$samples[[1]][1,1],".Rdata")
  if (file.exists(fn)) {
    load(fn)
  } else {
    new_data = do.call(rbind,lapply(1:4, function(x) data.table(fit$data)[, session := x]))
    epred.r = 
      pred_cont_from_ordered(
        fit,
        new_data = new_data,
        var.ordered = the_data$response.c,
        var.continuous = the_data$response,
        parallelise = ifelse(length(unique(the_data$item_name)) == 1,TRUE,FALSE))
    
    epred.data = new_data[, .(state.condition, Sex,participant, session, Drug)]
    
    ppx = cbind(epred.data, epred.r) %>% 
      melt(id.vars = names(epred.data), variable.name = "iter") %>% 
      .[, .(value = mean(value)), by = .(state.condition, Sex, Drug, iter)] %>% 
      .[, .(mCI = get_mci(value,get.P = FALSE)), by = .(state.condition, Sex,Drug)] %>% 
      dcast(Sex + state.condition ~ Drug, value.var = "mCI")
    pp = cbind(epred.data, epred.r) %>% 
      melt(id.vars = names(epred.data), variable.name = "iter") %>% 
      .[, .(value = mean(value)), by = .(state.condition, Sex, Drug, iter)] %>% 
      dcast(iter ~ Drug + state.condition + Sex, value.var = "value") 
    
    if (any(grepl("man",names(pp))))
      setnames(pp,names(pp),gsub("man","men", names(pp)))
    # calculate drug effects:
    pp = pp[, .(women_in_control = oxycodone_control_women - placebo_control_women,
            men_in_control = oxycodone_control_men - placebo_control_men,
            women_in_stress = oxycodone_stress_women - placebo_stress_women,
            men_in_stress = oxycodone_stress_men - placebo_stress_men),
        by = .(iter)] %>% 
      .[, `:=`(in_women = (women_in_control + women_in_stress)/2,
               in_men = (men_in_control + men_in_stress)/2,
               in_stress = (women_in_stress+men_in_stress)/2,
               in_control = (women_in_control+men_in_control)/2)] %>% 
      .[, `:=`(women_vs_men = in_women-in_men,
               stress_vs_ctrl = in_stress-in_control,
               stress_vs_ctrl_in_men = men_in_stress-men_in_control,
               stress_vs_ctrl_in_women = women_in_stress-women_in_control,
               women_vs_men_in_ctrl = women_in_control-men_in_control,
               women_vs_men_in_stress = women_in_stress-men_in_stress)] %>% 
      .[, three_way := women_vs_men_in_ctrl-women_vs_men_in_stress] %>% 
      melt(id.vars = "iter", variable.name = "effect", value.name = "est")
    attr(pp,"cell_stats") = ppx
    save(pp,file = fn)
  }
  attr(pp,"outcome") = paste(unique(the_data$item_name),collapse = ", ")
  pp[, effect := gsub("man","men",effect)]
  cell_stats = attr(pp,"cell_stats")
  pp = rbind(pp, data.table(iter = unique(pp$iter), effect = "total", est = (pp[effect == "in_control",est] + pp[effect == "in_stress",est])/2))
  attr(pp,"cell_stats") = cell_stats[, Sex  := gsub("man","men",Sex )]
  attr(pp, "contrast_stats") = pp[, .(stats = get_mci(est)), by = .(effect)]
  return(pp)
}

#' Drug Plotter Function
#'
#' This function generates plots summarizing drug effects calculated from a brms model.
#'
#' @param post.contrasts A data.table containing the calculated drug effects.
#'
#' @return A combined ggplot2 plot showing various drug effects and contrasts.
#'
#' @details The function takes a data.table containing drug effects and contrasts calculated from a brms model
#' and generates a combined ggplot2 plot that shows various drug effects, interactions, and contrasts.
drug_plotter = function(post.contrasts) {
  dplotter = function(dt, title) {
    dt %>% ggplot(aes(x = est)) + 
      stat_halfeye() + 
      facet_wrap(~effect, scale = "free_x") + 
      geom_vline(xintercept = 0) + 
      ggtitle(title)
  }
  ox = attr(post.contrasts,"outcome")
  p1 = post.contrasts %>% 
    .[effect %in% c("in_women","in_men")] %>% 
    .[, effect := gsub("in_","",effect)] %>% 
    ggplot(aes(x = est, fill = effect)) +
    stat_halfeye(alpha = .5) + 
    geom_vline(xintercept = 0) + 
    ggtitle(paste0("Drug effect by sex on '",ox,"'")) + 
    scale_col_sex + 
    ylab("") +
    gg_no_y_axis
  p2 = post.contrasts %>% 
    .[effect %in% c("in_control","in_stress")] %>% 
    .[, effect := gsub("in_","",effect)] %>% 
    ggplot(aes(x = est, fill = effect)) +
    stat_halfeye(alpha = .5) + 
    geom_vline(xintercept = 0) + 
    ggtitle(paste0("Drug effect by stress on '",ox,"'")) + 
    scale_col_stress + 
    ylab("") +
    gg_no_y_axis
  
  pA = p1|p2
  
  p3 = post.contrasts[grepl("vs",effect)] %>% 
    .[, effect := gsub("_vs_"," - ", effect)] %>%
    .[, effect := gsub("_in_"," in ", effect)] %>% 
    .[, effect := gsub("_"," in ", effect)] %>% 
    dplotter(paste0("Contrasts and interactions for '",ox,"'")) + 
    theme(strip.text = element_text(size = 6), axis.text = element_text(size = 6)) + 
    ylab("") +
    gg_no_y_axis
  p4 = post.contrasts[grepl("way",effect)] %>% 
    dplotter("Three way interaction:\nwoman_vs_man_in_ctrl-\nwoman_vs_man_in_stress") +
    theme(plot.title = element_text(size = 10)) + 
    ylab("") +
    gg_no_y_axis
  
  pB = (p3|p4) + plot_layout(widths = c(2,1))
  
  return(
    (pA / pB) + plot_layout(heights = c(1.2,2))
  )
}

#' Get Drug Effects Statistics
#'
#' This function calculates and summarizes drug effects statistics from a data table of drug effects and contrasts.
#'
#' @param post.contrasts A data.table containing the calculated drug effects.
#' @param my_outcome The name of the outcome variable for which drug effects are calculated.
#'
#' @return A data.table containing summary statistics of drug effects.
#'
#' @details The function takes a data.table containing drug effects and contrasts and calculates summary statistics
#' for the specified outcome variable. It provides statistics for men, women, total, and sex differences in drug effects.
#' 
get_drug_effects.stats = function(post.contrasts, my_outcome) {
  old.nms = c("in_men", "in_women", "total", "women_vs_men")
  new.nms = c("Men", "Women", "Total", "Sex difference")
  tmp =
    post.contrasts %>%
    .[effect %in% c("in_men", "in_women", "women_vs_men")] %>%
    dcast(iter ~ effect, value.var = "est") %>%
    .[, total := (in_women + in_men) / 2] %>%
    melt(id.vars = "iter") %>%
    .[, .(stats = get_mci(value, get.P = ifelse(grepl("vs", variable), TRUE, FALSE))), by = .(variable)] %>%
    dcast(. ~ variable, value.var = "stats") %>%
    setnames(old.nms, new.nms) %>%
    .[, new.nms, with = FALSE]
  
  return(cbind(Outcome = my_outcome, tmp))
}


#' Get Drug Effects Statistics (Option B)
#'
#' This function calculates and summarizes drug effects statistics from a data table of drug effects and contrasts.
#'
#' @param post.contrasts A data.table containing the calculated drug effects.
#' @param my_outcome The name of the outcome variable for which drug effects are calculated.
#'
#' @return A data.table containing summary statistics of drug effects.
#'
#' @details The function takes a data.table containing drug effects and contrasts and calculates summary statistics
#' for the specified outcome variable. It provides statistics for men and women in different states of the condition,
#' including the drug effect, lower and upper bounds of the credible interval.
#'
get_drug_effects.stats.b = function(post.contrasts, my_outcome) {
  tmp =
    post.contrasts %>%
    .[grepl("^men_in|^women_in", effect)] %>%
    .[, .(stats = get_mci(est, get.P = FALSE)), by = .(effect)] %>%
    .[, c("Sex", "state.condition") := tstrsplit(effect, "_in_")] %>%
    .[, stats := gsub("\\)|;", "", stats)] %>%
    .[, c("drug_effect", "lower", "upper") := tstrsplit(stats, ", | \\(")] %>%
    .[, `:=`(drug_effect = as.numeric(drug_effect), lower = as.numeric(lower), upper = as.numeric(upper))] %>%
    .[, c("effect", "stats") := NULL] %>%
    .[, Outcome := my_outcome]
  
  return(tmp)
}


#' Get Threshold Prior Mean
#'
#' This function calculates the mean for the threshold prior from a vector of ordered response categories.
#'
#' @param x A vector of ordered response categories.
#'
#' @return The threshold prior mean.
#'
#' @details The function takes a vector of ordered response categories and calculates the threshold prior mean
#' based on the cumulative proportions of each category.
#'
get_thresh_prior_mean = function(x) {
  t_starts = x %>% table() %>% prop.table() %>% cumsum()
  return(t_starts[1:(length(unique(x))-1)] %>% qlogis())
}

#' Add Cumulative Threshold Prior
#'
#' This function adds a cumulative threshold prior to an existing prior distribution.
#'
#' @param my_prior A prior distribution specified in brm() formula syntax.
#' @param responses A vector of ordered response categories.
#'
#' @return A modified prior distribution with the cumulative threshold prior added.
#'
#' @details The function takes an existing prior distribution specified in brm() formula syntax and a vector of ordered
#' response categories. It calculates the threshold prior mean from the response categories and adds a cumulative
#' threshold prior to the existing prior distribution.
#'
add_cumthresh_prior = function(my_prior = c(), responses) {
  t_starts = get_thresh_prior_mean(responses)
  for (k in 1:length(t_starts)) {
    txt = paste0("prior(normal(", t_starts[k], ", 1), class = Intercept, coef = ", k, ")")
    my_prior = my_prior + eval(parse(text = txt))
  }
  return(my_prior)
}


#' Get Threshold Priors for Adjancent Catgory ordered logistic regression models
#'
#' This function calculates priors for the thresholds in an adjacent categories ordinal regression model.
#'
#' @param y A vector of ordered categorical responses.
#' @param my_prior A prior distribution specified in brm() formula syntax.
#' @param sd The standard deviation for the threshold priors.
#'
#' @return A modified prior distribution with the asymmetric threshold priors added.
#'
#' @details The function calculates asymmetric threshold priors for ordered categorical responses based on the
#' observed category counts in the data. It uses the log odds ratio of adjacent categories to determine the thresholds.
#'
get_acat_thresh_priors = function(y, my_prior = c(), sd = 2) {
  cat.counts = table(y)
  cat.counts = cat.counts[cat.counts > 0]
  n.cat = length(cat.counts)
  N = sum(cat.counts)
  t_starts = log((head(cat.counts, n.cat - 1) / N) / (tail(cat.counts, n.cat - 1) / N)) %>% round(2)
  for (k in 1:(n.cat - 1)) {
    txt = paste0("prior(normal(", t_starts[k], ",", sd, "), class = Intercept, coef = ", k, ")")
    my_prior = my_prior + eval(parse(text = txt))
  }
  return(my_prior)
}


#' Fit Ordinal Model
#'
#' This function fits an ordinal model to the given data using the brms package.
#'
#' @param the_data The data frame containing the response variable and predictors.
#' @param the_formula A brms formula specifying the model.
#' @param fn The filename to save the model.
#' @param family The family of the ordinal model (default is cumulative()).
#' @param adapt_delta The target acceptance rate for the adaptation phase.
#' @param warmup The number of warmup iterations.
#' @param iter The total number of iterations.
#' @param sd.prior The prior distribution for the standard deviation of the random effects.
#'
#' @return The fitted brm model.
#'
#' @details This function fits an ordinal model using the brms package. It allows for different families of ordinal models,
#' including cumulative and acat models. It automatically sets good priors for the threshold parameters based on the data.
#'
fit_ordinal_model = function(the_data, the_formula, fn, family = cumulative(), 
                             adapt_delta = NA, warmup = 1000, iter = 2000, 
                             sd.prior = prior(normal(0,2), class = "sd")) {
  if (family$family == "cumulative") {
    # get good priors for thresholds
    t_starts = get_thresh_prior_mean(the_data[,response.c])
    my_prior = prior(normal(0,2), class = b)
    for (k in 1:length(t_starts)) {
      txt = paste0("prior(normal(",t_starts[k],",1), class = Intercept, coef = ",k,")")
      my_prior = my_prior + eval(parse(text = txt))
    }
    control = list(adapt_delta = ifelse(is.na(adapt_delta),.8,adapt_delta))
  } else if (family$family == "acat") {
    my_prior = prior(normal(0,2), class = b) + sd.prior
    my_prior = get_acat_thresh_priors(the_data$response.c,my_prior)
    control = list(adapt_delta = ifelse(is.na(adapt_delta),.8,adapt_delta))
  }
  # run model
  fit = brm(
    the_formula,
    family = family,
    data = the_data,
    prior = my_prior,
    backend = "cmdstanr",
    control=control,
    #threads = threading(2, grainsize = 168),
    iter = iter,
    warmup = warmup)
  save(fit,file = fn)
  return(fit)
}

#' Select Data
#'
#' This function selects data from a larger dataset based on a list of item names.
#'
#' @param items A character vector of item names to select.
# @return A subset of the dataset containing the selected items.
#'
select_data = function(items) {
  setDT(my_data.Q) # Convert my_data.Q to a data.table if it's not already
  
  selected_data = 
    my_data.Q[
      item_name %in% items, 
      .(participant, session, stage, response, state.condition, 
        induced.state, Sex, Drug, item_name)]
  
  selected_data[, session := factor(session)] # Convert session to a factor
  
  return(selected_data)
}

#' Fit a non-parametric brsm model to display raw data and credible intervals.
#'
#' This function takes a list of items and an independent variable (IV) and returns a data.table
#' and a fitted model using the data.table and brms packages.
#'
#' @param my_items A character vector of item names.
#' @param IV A character indicating the independent variable ("Drug" or "state.condition").
#'
#' @return A list containing the data.table (dt) and the fitted model (fit).
#'
#' @details
#' This function first processes the input data by selecting relevant columns, coarsening data, 
#' and converting the 'stage' column to a factor. Depending on the conditions specified, it 
#' creates a model formula (mf) for the brms package. If the specified file exists, it loads 
#' a previously saved model fit; otherwise, it fits an ordinal model using the 'fit_ordinal_model' 
#' function. The result is returned as a list containing the data.table and the fitted model.
#'
#' @export
get_dt_fit = function(my_items, IV) {
  dt = 
    select_data(my_items) %>% 
    coarsen("response") %>% 
    .[, stage := factor(stage)]
  
  if (length(my_items) > 1) 
    dt = dt[stage != 1]
  
  if ("embarassed" %in% my_items)
    dt = dt[stage %in% 2:7]
  
  if (IV == "Drug") {
    mf = response.c ~ cs(stage:Drug:state.condition:Sex) + (1 | participant/session)
    if (length(my_items) > 1)
      mf = response.c ~ cs(stage:Drug:state.condition:Sex) + (1 | participant/session) + (1 | item_name)
    if ("embarassed" %in% my_items)
      mf = response.c ~ cs(stage:Drug:state.condition:Sex) + (1 | participant/session)
  } else if (IV == "state.condition") {
    mf = response.c ~ cs(stage:state.condition:Sex) + (1 | participant/session)
    if (length(my_items) > 1) 
      mf = response.c ~ cs(stage:state.condition:Sex) + (1 | participant/session) + (1 | item_name)
  }
  
  fn = paste0("fits/TC/",paste0(my_items,collapse = "_"),".Rdata")
  if (file.exists(fn)) {
    load(fn)
  } else {
    fit = fit_ordinal_model(
      dt,mf,fn, family = acat(), 
      warmup = 500, iter = 1000)
  }
  return(list(dt = dt, fit = fit))
}

#' Function to Compute Posterior Predictive Distributions
#'
#' This function computes posterior predictive distributions on the original scale 
#' for a given fitted ordinal brms model for binned data.
#'
#' @param fit A fitted model object obtained from the brms package.
#' @param dt A data.table object containing the dataset.
#' @param my_items A character vector of item names.
#' @param IV A character indicating the independent variable ("Drug" or "state.condition").
#'
#' @return A data.table containing posterior predictive distributions.
#'
#' @details
#' This function first checks if a file containing the posterior predictive distributions 
#' already exists. If it does, it loads the saved data; otherwise, it calculates the posterior 
#' predictive distributions using the fitted model and the dataset. The function computes 
#' expected responsed on the original scale (epred.r) based on the model and dataset, and then 
#' calculates the posterior predictive distributions. The result is returned as a data.table 
#' with columns for the mean, lower quantile (2.5%), and upper quantile (97.5%) of the posterior 
#' predictive distributions.
#'
make_OR_pp = function(fit,dt,my_items,IV) {
  fn = paste0("fits/TC/pp_",IV,"_",paste(my_items,collapse = "_"),".Rdata")
  if (file.exists(fn)) {
    load(fn)
  } else {
    epred = posterior_epred(fit)
    obs.ratings = dt[,.(r = mean(response)), by = .(response.c)][,r] %>% sort()
    epred.r = apply(epred, 1, function(a) apply(a, 1, function(p) fsum(obs.ratings, w = p)))
    
    bx = c("stage","state.condition","Sex")
    if(IV == "Drug" | all(my_items == posaffect_items)) bx = c(bx,"Drug") 
    if("embarassed" %in% my_items) bx = c(bx,"Drug")
    
    pp = 
      cbind(fit$data, epred.r) %>% 
      data.table() %>% 
      melt(id.vars = names(fit$data), variable.name = "iter") %>% 
      .[, value := (mean(value)), by =  c(bx,"iter")] %>% 
      .[, .(m = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)),
        by = bx]
    save(pp,file = fn)
  }
  return(pp)
}

#' Function to Plot the Experiment With Relevant Timing Information
#'
#' This function generates a basic ggplot plot with relevant information about the experiment
#'
#' @param y.min The minimum value for the y-axis.
#' @param y.max The maximum value for the y-axis.
#' @param by.y The interval for y-axis tick marks.
#' @param by.x The interval for x-axis tick marks.
#'
#' @return A list of ggplot2 layers for creating the experimental plot.
#'
#' @details
#' This function reads timing data and assigns images to specific activities based on 
#' activity names. It then generates a plot with rectangular bars and vertical dashed lines 
#' representing different activities and time intervals. The y-axis range and tick marks 
#' can be customized using the parameters provided. The resulting plot layers are returned 
#' as a list suitable for creating the experimental plot.
#'
plot_exp = function(y.min=0, y.max=100, by.y = 20, by.x = 20) {
  td = get_timing_data()
  td$tx %>% 
    .[grepl("drug_[1-2]_admin",Activity), image := "assets/syringe_4.png"] %>% 
    .[grepl("prepmainmath|reminder_1",Activity), image := "assets/alarm_2.png"] %>% 
    .[, y := y.max]
  Q.times = td$TS[grepl("Q[0-9]",Activity)]
  e_plot = 
    list( 
      geom_rect(data = td$tx[!is.na(xmin)], inherit.aes = FALSE, alpha = .125,
                aes(ymin=y.min,ymax=y.max, xmin = xmin, xmax=xmax, group = Activity)),
      geom_linerange(data = td$tx[is.na(xmin)], inherit.aes = FALSE, lty = 2,
                     aes(x = time, ymin = y.min, ymax = y.max, group = Activity)),
      scale_x_continuous(breaks = seq(-20,70,by = by.x), limits = c(-23,75),expand = c(0,0,.05,0)),
      scale_y_continuous(breaks = seq(ceiling(y.min/10)*10,y.max,by = by.y), 
                         expand = c(0,0,.05,0), limits = c(y.min,y.max+diff(c(y.min,y.max))*.01)),
      theme(axis.line = element_line(), text = element_text(size = 11)),
      xlab("Time"),
      geom_image(data = td$tx, aes(x = time, y = y, image = image),
                 inherit.aes = FALSE, size = 0.05, position = position_nudge(y = y.max/100))
      )
  return(e_plot)
}


base.size.tc = 15
#' Plot Time Courses Based on a Bayesian Ordinal Regression Model
#'
#' @param my_items A character vector of items for analysis.
#' @param IV The independent variable of interest.
#' @param base.size The base size for text elements in the plot.
#'
#' @return A ggplot2 object representing the treatment effects plot.
#'
#' @details
#'   This function takes a list of items (my_items) and an independent variable (IV) as input.
#'   It performs several data manipulations and uses ggplot2 to create a visualization of
#'   treatment effects based on Bayesian analysis results. The plot includes points,
#'   ribbons for uncertainty, and lines to represent treatment effects.
#'
plot_tc_OR_bayes = function(my_items, IV, base.size = base.size.tc) {
  
  tmp = get_dt_fit(my_items,IV) 
  dt = tmp$dt
  fit = tmp$fit
  pp = make_OR_pp(fit,dt,my_items,IV)
  Q.times = get_timing_data()$TS[grepl("Q[0-9]",Activity)]
  
  if (IV == "Drug") {
    pp[, grp := paste0(Drug,state.condition)]
    scale_clrs = scale_col_drug
  } else {
    pp[, grp := state.condition]
    scale_clrs = scale_col_stress
  }
  if ("embarassed" %in% my_items)
    scale_clrs = scale_col_drug
  pdata = 
    pp[stage != 1] %>% 
    .[, Activity := paste0("Q",stage)] %>% 
    merge(Q.times[, .(Activity,Activity_mid)],
          by = "Activity", all.x = TRUE, all.y = FALSE) 
  p = 
    pdata %>% 
    ggplot(aes(x = Activity_mid, y = m, color = .data[[IV]], fill = .data[[IV]],
               shape = state.condition, group = grp)) + 
    plot_exp() + 
    facet_wrap(~Sex,nrow = 2) +
    geom_point() + 
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25, color = NA) +
    ylab(paste(stringr::str_to_title(my_items),"(VAS 0-100)")) + 
    scale_clrs + # + theme(legend.position = "none") + 
    theme(text = element_text(size = base.size), axis.title = element_text(size = base.size*3/4))
  
  if (IV == "Drug") {
    p = p + geom_line(aes(lty = .data[[IV]]))
  } else {
    p = p + geom_line()
  }
  
  if ("embarassed" %in% my_items)
    p = p + facet_wrap(~Sex, nrow = 2) + scale_x_continuous(breaks = seq(-20,30,10), limits = c(-21.5,36.5), expand = c(0,0))
  return(p)
}

#' Plot Time Course for Cortisol Data Based on a Bayesian Regression Model
#'
#' @param base.size The base size for text elements in the plot.
#'
#' @return A ggplot2 object representing the treatment effects plot for cortisol analysis.
#'
#' @details
#'   This function loads data from "my_data_cortisol.Rdata," performs data manipulations, and
#'   generates a ggplot2 visualization of the cortisol time course. The function checks if the
#'   needed analyses are already run and otherwise runs it. 
#'   The plot includes points, ribbons for uncertainty, and lines to represent treatment effects based
#'   on Bayesian analysis results.
#'
plot_tc_cortisol = function(base.size = base.size.tc) {
  
  load("data/my_data_cortisol.Rdata")
  my_data.cortisol = 
    my_data %>% 
    .[, result := result*100*0.0001] %>% 
    .[, log.result := log(result)] %>% 
    .[participant %in% unique(my_data$participant)]
  
  my_data.cortisol = 
    my_data.cortisol %>% 
    .[, concentr := mean(result), by = .(Sample.ID)] %>% 
    .[, cv.concentr := sd(result)/concentr, by = .(Sample.ID)] %>% 
    .[, .(participant, Drug, Stress, stage, plate, concentr, gender, session, time)] %>% 
    unique() %>% 
    .[, l.concentr := log(concentr)] %>% 
    .[, Sex := ifelse(gender == "f","women","men")] %>%
    .[, induced.state := ifelse(Stress == "stress" & stage > 3, "stress", "control")] %>% 
    .[, l.bl := mean(l.concentr[stage %in% 2:3]), by = .(participant, Drug, Stress)] %>% 
    .[, l.bl.m := mean(l.concentr[stage %in% 2:3], na.rm = TRUE)] %>% 
    .[, l.concentr.blc := l.concentr-l.bl+l.bl.m] %>% 
    .[, bl := mean(concentr[stage %in% 2:3]), by = .(participant, Drug, Stress)] %>% 
    .[, bl.m := mean(concentr[stage %in% 2:3], na.rm = TRUE)] %>% 
    .[, concentr.blc := concentr-bl+bl.m] %>% 
    setnames("Stress","state.condition") 
  
  
  fn = "fits/TC/pp_cortisol.Rdata"
  if (file.exists(fn)) {
    load(fn)
  } else {
    fn.fit = "fits/TC/cortisol_student.Rdata"
    if (file.exists(fn)) {
      load(fn)
    } else {
      my_data.cortisol[, stage := factor(stage)]
      #mf = concentr ~ 0 + stage:state.condition + s(time) + session + (1 | participant/session)
      mf = concentr ~ 0 + stage:state.condition:Sex + s(time) + (1 | participant/session)
      my_prior = c(
        prior(normal(0,5), class = "b", coef = "stime_1"),
        #prior(normal(0,5), class = "b", coef = "stime.is_1"),
        prior(normal(0,5), class = "sds"),
        prior(normal(10,2), class = "b"),
        prior(normal(0,5), class = "sd", coef = "Intercept", group = "participant"),
        prior(normal(0,2), class = "sd"),
        prior(gamma(2,.025), class = "nu"),
        prior(normal(0,3), class = "sigma")
      )
      fit =
        brm(mf,
            prior = my_prior,
            family = student(),
            data = my_data.cortisol,
            iter = 2000,
            #threads = threading(2, grainsize = ceiling(926/2)),
            control = list(adapt_delta = .8),
            backend = "cmdstanr")
      save(fit, file = fn.fit)
    }
    
    new_data = 
      fit$data %>% 
      data.table() 
    pp =
      cbind(
        fit$data %>% data.table(),
        t(posterior_linpred(fit, newdata = new_data))*10
      ) %>%
      melt(id.vars = names(fit$data), variable.name = "iter") %>%
      .[, .(value = mean(value)), by = .(state.condition,stage,iter,Sex)] %>% 
      .[, .(m = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)),
        by = .(state.condition,stage,Sex)]
    save(pp,file = fn)
  }
  
  Q.times = get_timing_data()$TS[grepl("Q[0-9]",Activity)]
  
  p_cortisol = 
    pp %>%
    .[, Activity := paste0("Q",stage)] %>% 
    merge(Q.times[, .(Activity,Activity_start)],
          by = "Activity", all.x = TRUE, all.y = FALSE) %>% 
    ggplot(aes(x = Activity_start, y = m,group = state.condition, shape = state.condition,
               color = state.condition, fill = state.condition)) +
    geom_point() +
    geom_line() +
    facet_wrap(~Sex, nrow = 2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25, color = NA) +
    scale_col_stress + 
    coord_capped_cart(left="bottom") +
    plot_exp(57,125) +
    ylab("Plasma cortisol (mg/mL)") + # + theme(legend.position = "none") 
    scale_y_continuous(limits = c(57,127), expand = c(0,0,.075,0)) + 
    theme(text = element_text(size = base.size), axis.title = element_text(size = base.size*3/4))
  
  # my_data.cortisol %>% 
  #   .[, .(m = mean(concentr)*10), by = .(state.condition,stage)] %>% 
  #   ggplot(aes(x= stage, y = m, group = state.condition, color = state.condition)) + 
  #   geom_line() + scale_col_stress
  # 
  # my_data.cortisol %>% 
  #   ggplot(aes(x= stage, y = concentr, color = state.condition)) + 
  #   geom_boxplot() + scale_col_stress
  
  return(p_cortisol)
}



#' Plot Time Course for Heart Rate Data Based on a Bayesian Regression Model
#'
#' @param base.size The base size for text elements in the plot.
#'
#' @return A ggplot2 object representing the treatment effects plot for cortisol analysis.
#'
#' @details
#'   This function loads data from "HRVactivity.Rdata," performs data manipulations, and
#'   generates a ggplot2 visualization of the cortisol time course. The function checks if the
#'   needed analyses are already run and otherwise runs it. 
#'   The plot includes points, ribbons for uncertainty, and lines to represent treatment effects based
#'   on Bayesian analysis results.
#'
plot_tc_HeartRate = function(base.size = base.size.tc) {
  load("data/HRVactivity.Rdata")
  HR.activity =
    HR.activity %>%
    setnames(c("Participant","Session"),c("participant","session")) %>%
    .[, participant := as.numeric(gsub("E","",participant))] %>%
    .[participant %in% (my_data.Q$participant %>% unique())] %>%
    .[, state.condition := ifelse(Stress == "Stress","stress","control")] %>%
    .[, Sex := ifelse(Gender == "Women","women","men")] %>%
    .[, c("Stress","Gender") := NULL] %>%
    .[, time.start := a.start[Activity == "script"], by = .(participant,session)] %>%
    .[, a.start.0 := a.start-time.start] %>%
    .[, a.mid.0 := a.start.0 + duration/2]
  
  durations =
    HR.activity %>%
    .[, .(m.duration = median(duration), N = .N, a.mid.0 = mean(a.mid.0)), by = .(Activity)] %>%
    .[m.duration < 1000 & N > 200] %>%
    .[order(a.mid.0)]
  
  HR.activity =
    HR.activity %>%
    .[Activity %in% durations$Activity] %>%
    .[!(Activity %in% c("state_prepmainmath","state_mainmath"))] %>%
    .[, BPS.s := scale(BPS)] %>%
    .[, Activity := factor(Activity, levels = durations$Activity)] %>%
    .[, session := factor(session)] %>%
    .[, state.condition := factor(state.condition)] %>%
    .[, participant := factor(participant)] %>%
    .[, Nx := .N, by = .(participant, session)] %>%
    setkeyv(c("participant","session","Activity"))
  
  # HR.activity[session == 1 & participant == 1] %>%
  #   ggplot(aes(x = Activity, ymin = a.start, ymax = a.end)) +
  #   geom_linerange() +
  #   coord_flip()
  
  HR.activity %>%
    ggplot(aes(x = Activity, y = BPS)) +
    geom_boxplot() +
    coord_flip()
  
  fn = "fits/TC/pp_HeartRate.Rdata"
  if (file.exists(fn)) {
    load(fn)
  } else {
    fn.fit = "fits/TC/HRV.Rdata"
    if (file.exists(fn)) {
      load(fn)
    } else {
      #mf = BPS.s ~ Activity:state.condition + session + (1 | participant/session)
      mf = BPS.s ~ Activity:state.condition:Sex + (1 | participant/session)
      my_prior = c(
        prior(normal(0,1), class = "Intercept"),
        prior(normal(0,5), class = "b"),
        prior(normal(0,5), class = "sd", coef = "Intercept", group = "participant"),
        prior(normal(0,5), class = "sd"),
        prior(normal(0,5), class = "b", dpar = "sigma"),
        prior(normal(0,5), class = "Intercept", dpar = "sigma")
      )
      fit =
        brm(bf(mf, sigma ~ Activity),
            prior = my_prior,
            family = gaussian(),
            data = HR.activity,
            iter = 3000,
            warmup = 1000,
            #threads = threading(2, grainsize = ceiling(nrow(HR.activity)/2)),
            backend = "cmdstanr")
      save(fit, file = fn.fit)
    }
    
    HR.timing =
      get_timing_data()$TS %>% 
      .[, .(Activity,Activity_mid)] %>%
      setkeyv("Activity")
    
    new_data =
      fit$data %>%
      data.table() %>%
      merge(HR.timing)
    
    pp =
      cbind(
        new_data,
        t(posterior_linpred(fit, newdata = new_data))*sd(HR.activity$BPS) + mean(HR.activity$BPS)
      ) %>%
      melt(id.vars = names(new_data), variable.name = "iter") %>%
      .[, .(value = mean(value)), by = .(state.condition,Activity,iter,Sex)] %>%
      .[, .(m = mean(value),
            lower = quantile(value, .025),
            upper = quantile(value, .975)),
        by = .(state.condition,Activity,Sex)] %>%
      setkeyv("Activity") %>%
      merge(HR.timing)
    save(pp,file = fn)
  }
  
  p_HeartRate = 
    pp %>%
    .[Activity_mid > - 30 & !(Activity %in% c("state_math","state_prep"))] %>%
    ggplot(aes(x = Activity_mid, y = m,group = state.condition, shape = state.condition,
               color = state.condition, fill = state.condition)) +
    plot_exp(65,103,by.y = 10) +
    geom_point() +
    geom_line() +
    facet_wrap(~Sex, nrow = 2) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .25, color = NA) +
    coord_capped_cart(left='bottom') +
    scale_col_stress +
    ylab("Heart rate (BPM)") + #+ theme(legend.position = "none") 
    scale_y_continuous(limits = c(65,106), expand = c(0,0,.05,0)) + 
    theme(text = element_text(size = base.size), axis.title = element_text(size = base.size*3/4))
  
  return(p_HeartRate)
}


#' Analyse Obtained Data Using Bayesian Modeling
#' 
#' This function fits a Bayesian ordinal regression model using the 
#' \code{brms} package. If a saved model exists, it loads it; otherwise, 
#' it fits the model and saves the result. Optionally, results are analyzed 
#' separately by sex if the formula includes `Sex`.
#' 
#' @param f Formula for the Bayesian model.
#' @param data A data frame containing the variables used in the model.
#' @param fn Character string representing the filename where the fitted model should be saved or loaded from.
#' @param my_prior Optional. A list of priors to be used in the model. If NULL, a default prior is generated.
#' 
#' @return A list containing plots and statistical summaries of the model results.
#' 
#' @examples
#' # Example usage:
#' # result <- analyse_obtained(my_formula, my_data, "model_fit.RData")
#' 
#' @export
analyse_obtained = function(f, data, fn, my_prior = NULL) {
  if (is.null(my_prior)) {
    my_prior = 
      get_acat_thresh_priors(
        my_data_beh$effect_obtained.c,
        make_prior_b(2) + 
          prior(normal(0,2), class = "sd"))
  }
  
  if (file.exists(fn)) {
    load(fn)
  } else {
    fit = brm(
      analysis_formula,
      family = acat(),
      data = data,
      prior = my_prior,
      backend = "cmdstanr",
      iter = 2000)
    save(fit, file = fn)
  }
  
  by_sex = any("Sex" == all.vars(analysis_formula))
  if (by_sex == TRUE) {
    plot_stats = plot_results_origscale_sex(fit, data, outcome.var = "effect_obtained.c")
    plot_stats$stats = 
      plot_stats$stats %>% 
      .[,.(Sex, mean, CI, `P > 0`)] %>% 
      .[, Sex := factor(Sex, levels = c("total", "women", "men", "sex diff: m-f"))] %>% 
      setkeyv("Sex") 
  } else {
    plot_stats = plot_results_origscale(fit, data)
  }
  return(plot_stats)
}

# thanks to Chatgpt
library(ggplot2)
library(scales) # Needed for hue_pal()

my_cor <- function(data, mapping, method = "kendall", ...) {
  
  # 1. Extract x and y variable names safely
  x_var <- rlang::as_name(mapping$x)
  y_var <- rlang::as_name(mapping$y)
  
  # 2. Try to extract grouping variable
  group_var <- NULL
  if (!is.null(mapping$colour)) {
    group_var <- rlang::as_name(mapping$colour)
  } else if (!is.null(mapping$shape)) {
    group_var <- rlang::as_name(mapping$shape)
  }
  
  # 3. Compute overall correlation
  corr_all <- cor.test(data[[x_var]], data[[y_var]], use = "complete.obs", method = method)
  
  # Create the "All" label
  labels <- data.frame(
    label = paste0("All: ", round(corr_all$estimate, 2), " (p=", round(corr_all$p.value, 3), ")"),
    x = 0.5,
    y = 0.8,
    col = "All"
  )
  
  # 4. Compute group-wise correlations (if groups exist)
  if (!is.null(group_var)) {
    groups <- unique(data[[group_var]])
    
    group_corrs <- sapply(groups, function(g) {
      idx <- data[[group_var]] == g
      # Calculate correlation for this specific group
      ct <- cor.test(data[[x_var]][idx], data[[y_var]][idx], use = "complete.obs", method = method)
      return(c(val = ct$estimate, p.value = ct$p.value))
    })
    
    # We use row index [1,] instead of name ["cor.tau",] so this works for Pearson too
    group_labels <- data.frame(
      label = paste0(groups, ": ", round(group_corrs[1, ], 2), " (p=", round(group_corrs[2, ], 3), ")"),
      x = 0.5,
      y = seq(0.6, 0.6 - 0.2 * (length(groups) - 1), by = -0.2),
      col = as.character(groups)
    )
    
    labels <- rbind(labels, group_labels)
  }
  
  # 5. Create color palette
  # We want "All" to be black, and the rest to use the standard ggplot hue
  unique_cols <- unique(labels$col)
  n_colors_needed <- length(unique_cols) - 1 # exclude "All"
  
  # Use scales::hue_pal() to generate the default ggplot colors
  # If you don't have 'scales' installed, use: install.packages("scales")
  standard_colors <- scales::hue_pal()(n_colors_needed)
  
  # Create a named vector: All = black, others = standard colors
  # We filter unique_cols to exclude "All" to match the palette
  group_colors <- c("black", standard_colors)
  names(group_colors) <- c("All", unique_cols[unique_cols != "All"])
  
  # 6. Plot
  ggplot(data = data, mapping = mapping) +
    geom_text(data = labels, aes(x = x, y = y, label = label, color = col), size = 3, hjust = 0.5) +
    theme_void() +
    ylim(0, 1) + 
    scale_color_manual(values = group_colors, guide = "none")
}

