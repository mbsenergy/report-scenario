

# Packages and Setup ------------------------------------------------------------------

options(box.path = getwd())

# library(mbsenergyUtils)
box::use(ggplot2[...],
         data.table[...],
         scales,
         xl = openxlsx[read.xlsx])

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

report_num = 'III2023'

years_to_display = c(2025, 2030, 2035, 2040, 2045, 2050)
levels_order = c("High", "Reference", "Low", "III2023")
color_mapping = c(High = "#007F77", Low = "#97BBFF", Reference = "#FF9933")
color_mapping[report_num] = "#FF9933"


## File 
excel_file = file.path('data', 'Grafici report scenari.xlsx')
excel_file_sn = xl$getSheetNames(excel_file)

# Plots ------------------------------------------------------------------------------------------------

## Line graphs

vec_plot_line = excel_file_sn[c(1,2,4,6,7,8,13,18,23,24,27,32,35,40,41)]

## Produce Plots

for (i in 1:length(vec_plot_line)) {
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, DASHED := fifelse(get(c_name) != report_num, "A", "B")]
    
    plot_line =
        dt_line_lg %>%
        ggplot(aes(x = anni,
                   y = valori,
                   color = get(c_name))) +
        geom_line(aes(linetype = DASHED), linewidth = 1.1) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_linetype_manual(values = c("solid", "dashed"), breaks = c("A", "B"), labels = NULL) +
        scale_x_continuous(breaks = years_to_display, labels = years_to_display) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               linetype = 'none') +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              text = element_text(family = "Aptos Narrow"),
              plot.title = element_text(face = "bold"), 
              plot.subtitle = element_text(face = "italic"),
              legend.position = 'top',
              legend.text = element_text(color = "#595959"),
              plot.caption = element_text(hjust = 0, face = "italic", margin = margin(t = 20))) + 
        labs(title = c_name_clean,
             subtitle = unit_measure,
             x = NULL,
             y = NULL,
             caption = expression(bold("Source: ") * "MBS Consulting elaborations"))
    
    if("%" %in% unit_measure) {
        plot_line = plot_line + 
            scale_y_continuous(labels = scales::percent_format()) 
    } else {
        plot_line
        }   
    
    ggsave(file.path('report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = 3)
    
}
