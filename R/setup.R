library(knitr)
library(kableExtra)

library(magrittr)
library(data.table)
library(collapse)

library(brms)
library(posterior)
library(bayesplot)
library(ggdist)
library(GGally)

library(ggplot2)
library(lemon)
library(sjPlot)
library(patchwork)
library(ggbeeswarm)
library(ggh4x)
library(gtsummary)
library(ggthemes)
library(colorspace)
library(viridis)  

library(hausekeep)
library(grateful)
library(ggimage)

library(flextable)

theme_set(theme_hc())
theme_update(axis.title.y = element_text(angle = 90),
             legend.text = element_text(size = 10),
             legend.key.size = unit(.3,"cm"),
             legend.title = element_text(size = 11))
options(mc.cores = 4)

theme_apa = function (x, ...) 
{
    if (!inherits(x, "flextable")) {
        stop(sprintf("Function `%s` supports only flextable objects.", 
            "theme_apa()"))
    }
    apa.border <- list(width = flextable:::flextable_global$defaults$border.width, 
        color = "black", style = "solid")
    x <- font(x, part = "all", fontname = "Times New Roman")
    x <- line_spacing(x, space = 1, part = "all")
    x <- hline_top(x, part = "head", border = apa.border)
    x <- hline_bottom(x, part = "head", border = apa.border)
    x <- hline_top(x, part = "body", border = apa.border)
    x <- hline_bottom(x, part = "body", border = apa.border)
    x <- align(x, align = "center", part = "all")
    x <- valign(x, valign = "center", part = "all")
    x <- colformat_double(x, digits = 2)
    fix_border_issues(x)
}

set_flextable_defaults(
  font.size = 10, theme_fun = theme_apa)


# colors for gender
sex_colors =  c(men = "#30D5C8",women = "#CD0BBC",
                `women-men` = "orange", avg = "blue4")
scale_col_sex = list(
  scale_fill_manual(values = sex_colors), 
  scale_color_manual(values = sex_colors)  
)

sex_colors.b =  c(men = "#30D5C8",women = "#CD0BBC",
                  `Sex difference` = "orange", total = "blue4")
scale_col_sex.b = list(
  scale_fill_manual(values = sex_colors.b), 
  scale_color_manual(values = sex_colors.b)  
)

sex_colors2 =  c("#30D5C8","#CD0BBC")
scale_col_sex2 = list(
  scale_fill_manual(values = sex_colors2), 
  scale_color_manual(values = sex_colors2)  
)

sex_colors2a =  adjustcolor(c("#30D5C8","#CD0BBC"),alpha = .5)
scale_col_sex2a = list(
  scale_fill_manual(values = sex_colors2a), 
  scale_color_manual(values = sex_colors2a)  
)

# colors for drug
drug_colors =  c("blue","grey25","orange")
scale_col_drug = list(
  scale_fill_manual(values = drug_colors), 
  scale_color_manual(values = drug_colors)  
)
# colors for stress condition
stress_colors =  c("#1b9e77","#d95f02")
scale_col_stress = list(
  scale_fill_manual(values = stress_colors), 
  scale_color_manual(values = stress_colors)  
)
gg_no_y_axis = 
  theme(axis.text.y = element_blank(),
        axis.ticks.length.y = unit(0,"mm"),
        panel.grid.major.y = element_blank()) 
gg_no_x_axis = 
  theme(axis.text.x = element_blank(),
        axis.ticks.length.x = unit(0,"mm"),
        panel.grid.major.x = element_blank()) 
