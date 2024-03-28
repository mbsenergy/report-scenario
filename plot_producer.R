

# Packages and Setup ------------------------------------------------------------------

# library(mbsenergyUtils)
library(ggplot2)
library(data.table)
# library(openxlsx)
# library(flextable)
# 
# set_flextable_defaults(
#     font.color = "#747E7E",
#     font.size = 9,
#     font.family = 'calibri',
#     padding = 5,
#     border.color = "#dfe2e5",
#     background.color = "whitesmoke",
#     split = FALSE,
#     theme_fun = "theme_box",
#     decimal.mark = ",",
#     big.mark = " ",
#     na_str = "<na>")



## GDP Growth Rate -------------------------

dt_gdp_gwr = openxlsx::read.xlsx(file.path('data', 'Grafici report scenari.xlsx'), sheet = 1) %>% setDT() 

dt_gdp_gwr_lg = dt_gdp_gwr %>% 
    melt(id.vars = 'GDP.Growth.Rate.(%)', variable.name = 'anni', value.name = 'valori') 

dt_gdp_gwr_lg[, anni := as.numeric(anni)]
setorderv(dt_gdp_gwr_lg, cols = c('GDP.Growth.Rate.(%)', 'anni'))

        
plot_1 =
    dt_gdp_gwr_lg %>%
    ggplot(aes(x = anni,
               y = valori,
               color = `GDP.Growth.Rate.(%)`)) +
    geom_point() +
    geom_line(group = 1) +
        theme_light() +
            theme(legend.position = 'bottom') +
            labs(x = NULL,
                 y = '€/MWh',
                 caption = 'Elaborazione MBS')

plot_1

ggsave(file.path('report_gen', 'figs', '6_ttf.png'), plot_6,
       width = 8, height = 3)
