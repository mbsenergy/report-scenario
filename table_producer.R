
# Packages and Setup ------------------------------------------------------------------

library(mbsenergyUtils)

box::use(ggplot2[...],
         data.table[...],
         scales[...],
         flextable[...],
         xl = openxlsx[read.xlsx])


set_flextable_defaults(
    font.color = "#54565B",
    font.size = 7,
    font.family = 'calibri',
    padding = 2,
    border.color = "#dfe2e5",
    background.color = "#dfe2e5",
    split = FALSE,
    theme_fun = "theme_box",
    decimal.mark = ",",
    big.mark = " ",
    na_str = "<NA>")

# Upload File ---------------------------------------

## Tables -------------------------

dt_tables = openxlsx::read.xlsx(file.path('data', 'tables_report.xlsx'), sheet = 'Sheet0') %>% 
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
