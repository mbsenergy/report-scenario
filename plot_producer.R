
# Packages and Setup ------------------------------------------------------------------

library(mbsenergyUtils)
library(ggplot2)
library(data.table)
library(openxlsx)
library(flextable)

set_flextable_defaults(
    font.color = "#747E7E",
    font.size = 9,
    font.family = 'calibri',
    padding = 5,
    border.color = "#dfe2e5",
    background.color = "whitesmoke",
    split = FALSE,
    theme_fun = "theme_box",
    decimal.mark = ",",
    big.mark = " ",
    na_str = "<na>")

# Upload File ---------------------------------------

## Tables -------------------------

dt_tables = openxlsx::read.xlsx(file.path('report_gen', 'data', 'tables_report.xlsx'), sheet = 'Sheet0') %>% 
    setDT() 

### Table 1

dt_1 =
  melt(dt_tables, id.vars = c(1,2), variable.name = 'Year', value.name = 'Values') %>% 
    dcast(NET.POWER ~ Scenario + Year, value.var = 'Values')
setcolorder(dt_1, neworder = c('NET.POWER', 'Reference_2025', 'Reference_2030', 'Reference_2040', 'Reference_2050',
                                           'Low_2025', 'Low_2030', 'Low_2040', 'Low_2050',
                                           'High_2025', 'High_2030', 'High_2040', 'High_2050'))

dt_1 |> flextable() %>% 
    separate_header() %>%  
    align(align = "center", part = "all") %>% 
    bg(bg = "#B2BEBF", part = "header") %>% 
    color(part = "header", color = 'white') %>% 
    vline(j = c('NET.POWER', 'Reference_2050', 'Low_2050', 'High_2050'), border = officer::fp_border(color = "#dfe2e5", width = 2), part = "all") %>% 
    bold(i = NULL, j = 1) %>% 
    width(width = 2, j = 1) 

        


## Plots -----------------------
dt_plots_6 = openxlsx::read.xlsx(file.path('report_gen', 'data', 'dati_grafici_report_scenario.xlsx'), sheet = '6.TTF') %>% 
    setDT() %>% 
        melt(id.vars = 'Scenario', variable.name = 'Year', value.name = 'Values') %>% 
            .[, Year := as.numeric(as.character(Year))] %>% 
            setorder(Year, Scenario)  


plot_6 =
    dt_plots_6 %>%
    ggplot(aes(x = Year,
               y = Values,
               color = Scenario)) +
    geom_point() +
    geom_line() +
        theme_light() +
            theme(legend.position = 'bottom') +
            labs(x = NULL,
                 y = '€/MWh')

plot_6

ggsave(file.path('report_gen', 'figs', '6_ttf.png'), plot_6,
       width = 8, height = 3)
