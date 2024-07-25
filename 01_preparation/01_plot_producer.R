

# Packages and Setup ------------------------------------------------------------------
t0= Sys.time()

options(box.path = getwd())

library(dplyr)
library(stringr)
library(ggpubr)
library(ggplot2)

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

report_num = 'II2024'

years_to_display = c(2024:2040, 2045, 2050)

altezza_grafici = 4

levels_order = c("High", "Reference", "Low", report_num)
color_mapping = c(High = "#007F77", Low = "#97BBFF", Reference = "#FF9933")
color_mapping[report_num] = "#FF9933"

linetype_mapping = c("High" = "solid", 
                     "Low" = "solid",
                     "Reference" = "solid")

linetype_mapping[report_num] = "dashed"

## File 
excel_file = file.path('01_preparation', 'Grafici report scenari.xlsx')
excel_file_sn = xl$getSheetNames(excel_file)

## Line graphs ----

vec_plot_line = excel_file_sn[c(1,2,4,6,7,8,13,18,23,24,27,32,35,40,41)]

## Produce Plots

for (i in 1:length(vec_plot_line)) { #i = 9
    
    if (i %in% c(9,14))  {
        dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) 
        dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]
        dt_line = dt_line[ , colSums(is.na(dt_line)) == 0] %>% setDT()
    } else {
        dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    }
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    #dt_line_lg[, DASHED := fifelse(get(c_name) != report_num, "A", "B")]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), linetype = get(c_name),group = get(c_name))) +
        geom_line(linewidth = 1.1) +
        scale_linetype_manual(values = linetype_mapping, breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        theme_light() +
        guides(color = guide_legend(title = NULL),
               linetype = guide_legend(title = NULL)) +
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
    
    
    if (i %in% c(3,4)) {
    
    plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), linetype = get(c_name),group = get(c_name))) +
        geom_line(linewidth = 1.1) +
        scale_linetype_manual(values = linetype_mapping, breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        scale_y_continuous(limits = c(0,dt_line_lg$valori %>% max()), breaks=seq(0,dt_line_lg$valori %>% max(),by=10))+
        theme_light() +
        guides(color = guide_legend(title = NULL),
               linetype = guide_legend(title = NULL)) +
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
    }
    
    if (i ==5 ) {
        plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), linetype = get(c_name),group = get(c_name))) +
            geom_line(linewidth = 1.1) +
            scale_linetype_manual(values = linetype_mapping, breaks = levels_order) +
            scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
            scale_y_continuous(limits = c(0,7), breaks=seq(0,7,by=1))+
            scale_colour_manual(values=color_mapping, breaks = levels_order)+
            #ylim(0,7)+
            theme_light() +
            guides(color = guide_legend(title = NULL),
                   linetype = guide_legend(title = NULL)) +
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
        
        
    }
    
    if (i == 9) {
        
        plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), linetype = get(c_name),group = get(c_name))) +
            geom_line(linewidth = 1.1) +
            scale_linetype_manual(values = linetype_mapping, breaks = levels_order) +
            scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
            scale_colour_manual(values=color_mapping, breaks = levels_order)+
            scale_y_continuous(limits = c(20,dt_line_lg$valori %>% max()%>% round(0)+1), breaks=seq(20,dt_line_lg$valori %>% max()%>% round(0)+1,by=10))+
            theme_light() +
            guides(color = guide_legend(title = NULL),
                   linetype = guide_legend(title = NULL)) +
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
    }
    
    
    
    
    if("%" %in% unit_measure) {
        plot_line = plot_line + 
            scale_y_continuous(labels = scales::percent_format()) 
    } else {
        plot_line
    }   
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 12. World LNG Demand ----

vec_plot_line = excel_file_sn[c(3)]

color_mapping = c(Existing = "#D9D9D9", 
                  `Under Construction` = "#D4E2D8", 
                  Uncertain = "#FFCC99",
                  `Historical Demand` = "#000000",
                  `Demand - Reference Scenario` = "#FF9933",
                  `Demand - High Scenario` = "#007F77",
                  `Demand - Low Scenario` = "#97BBFF")
color_mapping %>% names()
levels_order = c("Existing"    ,                "Under Construction"   ,       "Uncertain",                  
                 "Historical Demand" ,          "Demand - Reference Scenario" ,"Demand - High Scenario" ,    
                 "Demand - Low Scenario")

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    names(dt_line)[1] = "Quarter"
    
    dt_line <- dt_line %>%
        mutate(Under.Construction = Existing + Under.Construction)
    
    dt_line <- dt_line %>%
        mutate(Uncertain = Under.Construction + Uncertain)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = 'Quarter', variable.name = 'Variable', value.name = 'Values') 
    
    dt_line_lg <- dt_line_lg[str_detect(Quarter, "^Q2|^Q4")]
    
    dt_line_lg$Quarter = factor(dt_line_lg$Quarter, levels = unique(dt_line_lg$Quarter))
    
    dt_line_lg[, lower_bound := 0]
    
    dt_line_lg$Variable = gsub("\\."," ",dt_line_lg$Variable)
    
    plot_line =
        dt_line_lg %>%
        ggplot(aes(group = Variable)) +
        geom_ribbon(data = dt_line_lg %>% filter(Variable == "Uncertain"), 
                    aes(x = Quarter, ymin = dt_line_lg %>% filter(Variable == "Uncertain") %>% pull(lower_bound),
                        ymax = dt_line_lg %>% filter(Variable == "Uncertain") %>% pull(Values),
                        fill = Variable)) +
        geom_ribbon(data = dt_line_lg %>% filter(Variable == "Under Construction"), 
                    aes(x = Quarter, ymin = dt_line_lg %>% filter(Variable == "Under Construction") %>% pull(lower_bound),
                        ymax = dt_line_lg %>% filter(Variable == "Under Construction") %>% pull(Values),
                        fill = Variable)) +
        geom_ribbon(data = dt_line_lg %>% filter(Variable == "Existing"), 
                    aes(x = Quarter, ymin = dt_line_lg %>% filter(Variable == "Existing") %>% pull(lower_bound),
                        ymax = dt_line_lg %>% filter(Variable == "Existing") %>% pull(Values),
                        fill = Variable)) +
        geom_line(data = filter(dt_line_lg, !Variable %in% c('Uncertain','Under Construction','Existing')), aes(x = Quarter, y = Values, color = Variable, linetype = Variable), linewidth = 1.1) +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_linetype_manual(values = c(Existing = "solid", 
                                         `Under Construction` = "solid", 
                                         Uncertain = "solid",
                                         `Historical Demand` = "dashed",
                                         `Demand - Reference Scenario` = "solid",
                                         `Demand - High Scenario` = "solid",
                                         `Demand - Low Scenario` = "solid"
        ), breaks = levels_order) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
              legend.position = 'right',
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 14. Spread TTF-PSV ----

vec_plot_line = excel_file_sn[c(5)]

color_mapping = c(TTF = "#007F77", PSV = "#97BBFF", `Spread TTF-PSV` = "#D4E2D8")
levels_order = c("Spread TTF-PSV", "TTF", "PSV")

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = filter(dt_line_lg, get(c_name) == "Spread TTF-PSV"), aes(x = anni, y = valori*20, fill = get(c_name)), width = 0.6) +
        geom_line(data = filter(dt_line_lg, get(c_name) != "Spread TTF-PSV"), aes(x = anni, y = valori, color = get(c_name), group = get(c_name)), linewidth = 1.1) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_y_continuous(name = NULL,sec.axis = sec_axis( trans=~./20, name=NULL)) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 19, 20, 21 . DAM Energy Balance ----

vec_plot_line = excel_file_sn[c(10,11,12)]

years_to_display = c(2024:2040, 2045, 2050)

years_to_display = as.character(years_to_display)

levels_order = c("Demand + storage consumption",                   
                 "Industrial self-production" ,                    
                 "Net import",                       
                 "Hydro - Reservoirs and run-of-river",            
                 "Pumped hydro production",                   
                 "BESS (Battery Energy Storage System) production",
                 "Renewables"  ,                                   
                 "Gas-fired thermal plants",                       
                 "Coal-fired thermal plants",                      
                 "Other production")

levels_order = rev(levels_order)

color_mapping = c("Demand + storage consumption" = "#FF9933" ,               
                  "Industrial self-production" = "#669895",                    
                  "Net import" = "#97BBA3",                       
                  "Hydro - Reservoirs and run-of-river" = "#0047CA",            
                  "Pumped hydro production" = "#97BBFF",                   
                  "BESS (Battery Energy Storage System) production" = "#D5E4FF",
                  "Renewables" = "#D4E2D8" ,                                   
                  "Gas-fired thermal plants" = "#FFCC99",                       
                  "Coal-fired thermal plants" = "#000000",                      
                  "Other production" = "#FFCC00")

#levels_order = rev(levels_order)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) 
    dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]
    dt_line = dt_line[ , colSums(is.na(dt_line)) == 0] %>% setDT()
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.character(anni)]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    plot_line = ggplot() +
        geom_col(data = filter(dt_line_lg, get(c_name) != "Demand + storage consumption"), aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        geom_line(data = filter(dt_line_lg, get(c_name) == "Demand + storage consumption"), aes(x = anni, y = valori, color = get(c_name), group = 1), linewidth = 1) +
        geom_point(data = filter(dt_line_lg, get(c_name) == "Demand + storage consumption"), aes(x = anni, y = valori, color = get(c_name), group = 1), size = 3) +
        scale_x_discrete(expand = c(0.5, 0)) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        scale_fill_manual(values=color_mapping, breaks = levels_order) + 
        theme_light() +
        guides(color = guide_legend(title = NULL, reverse = FALSE),
               fill = guide_legend(title=NULL),
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
              legend.position = 'right',
              legend.text = element_text(color = "#595959"),
              legend.key.size = unit(0.5, 'cm'),
              #legend.key.width = unit(0.4, 'cm'),
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}


## 23. E-mobility ----

vec_plot_line = excel_file_sn[c(14)]

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) %>%
        setDT()
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'cluster', value.name = 'valori') 
    
    setorderv(dt_line_lg, cols = c(c_name, 'cluster'))
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = c('Reference','Low','High'))]
    
    dt_line_lg[, cluster := factor(cluster, levels = c('PHEV','BEV'))]
    
    plot_line = ggplot() +
        geom_col(data = dt_line_lg, aes(x = get(c_name), y = valori, fill = cluster), width = 0.6) +
        scale_x_discrete(expand = c(0.5, 0)) +
        scale_fill_manual(values=c('BEV'='#00544F', 'PHEV'= '#97BBA3'), breaks = c('BEV','PHEV'))+
        theme_light() +
        guides(color = guide_legend(title = NULL, reverse = FALSE),
               fill = guide_legend(title=NULL),
               linetype = 'none') +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5, color = "#595959"),
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 24. Heating & Cooling ----

vec_plot_line = excel_file_sn[c(15)]

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) %>%
        setDT()
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'cluster', value.name = 'valori') 
    
    setorderv(dt_line_lg, cols = c(c_name, 'cluster'))
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = c('Reference','Low','High'))]
    
    plot_line = ggplot() +
        geom_col(data = dt_line_lg, aes(x = get(c_name), y = valori, fill = cluster), width = 0.6, show.legend = FALSE) +
        scale_x_discrete(expand = c(0.5, 0)) +
        scale_fill_manual("",values=c('#00544F'))+
        theme_light() +
        guides(color = guide_legend(title = NULL, reverse = FALSE),
               fill = guide_legend(title=NULL),
               linetype = 'none') +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5, color = "#595959"),
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 25. Self-Produc and Self-Consum ----

vec_plot_line = excel_file_sn[c(16)]

color_mapping = c(`Excess Production Sold on the Market` = "#E8E8E8", 
                  `Installed capacity (GW)` = "#669895", 
                  `Self-production (TWh)` = "#97BBA3",
                  `Self-consumption (TWh)` = "#FFCC99")

levels_order = c("Excess Production Sold on the Market", "Installed capacity (GW)", "Self-production (TWh)", "Self-consumption (TWh)")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_geom_ribbon = filter(dt_line_lg, get(c_name) %in% c("Self-production (TWh)","Self-consumption (TWh)")) %>% setDT()
    
    names(dt_geom_ribbon)[1] = "title"
    
    dt_geom_ribbon <- dcast(dt_geom_ribbon, anni ~ title, fun.aggregate = mean,value.var = "valori")
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = filter(dt_line_lg, get(c_name) == "Installed capacity (GW)"), aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        geom_line(data = filter(dt_line_lg, get(c_name) == "Self-consumption (TWh)"), aes(x = anni, y = valori/3, color = get(c_name)), linewidth = 1.1) +
        geom_line(data = filter(dt_line_lg, get(c_name) == "Self-production (TWh)"), aes(x = anni, y = valori/3, color = get(c_name)), linewidth = 1.1) +
        geom_ribbon(data = dt_geom_ribbon,aes(x = anni,
                                              ymin = `Self-consumption (TWh)`/3,
                                              ymax = `Self-production (TWh)`/3, 
                                              fill = "Excess Production Sold on the Market"),
                    alpha = 0.5) + 
        scale_fill_manual(values = color_mapping) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_x_continuous(breaks = years_to_display, labels = years_to_display) +
        scale_y_continuous(name = NULL,sec.axis = sec_axis( trans=~.*3, name=NULL)) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 26. Zonal Distribution Elec Dem ----

vec_plot_line = excel_file_sn[c(17)]

color_mapping = c("Calabria"= "#AEAEAE",
                  "Centre-North"= "#669895",
                  "Centre-South" = "#97BBA3",
                  "North" = "#00544F" ,
                  "Sardinia" = "#97BBFF" ,
                  "Sicily" = "#FFCC99",      
                  "South" = "#D4E2D8")

levels_order = c("North", "Centre-North", "Centre-South", "South", "Sardinia", "Sicily", "Calabria")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) 
    dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]
    dt_line = dt_line[ , colSums(is.na(dt_line)) == 0] %>% setDT()
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    dt_line_lg[, valori := as.numeric(valori)]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, valori := valori/1000]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = dt_line_lg, aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
              legend.position = 'right',
              legend.key.size = unit(0.5, 'cm'),
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 28. Newbuild capacity ----

vec_plot_line = excel_file_sn[c(19)]

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) %>%
        setDT()
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = rev(levels_order))]
    
    #dt_line_lg[, cluster := factor(cluster, levels = c('PHEV','BEV'))]
    
    plot_line = ggplot() +
        geom_col(data = dt_line_lg, aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        scale_x_discrete(expand = c(0.5, 0)) +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        theme_light() +
        guides(color = guide_legend(title = NULL, reverse = FALSE),
               fill = guide_legend(title=NULL),
               linetype = 'none') +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = 0, vjust = 0, hjust = 0.5, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              text = element_text(family = "Aptos Narrow"),
              plot.title = element_text(face = "bold"), 
              plot.subtitle = element_text(face = "italic"),
              legend.position = 'right',
              legend.key.size = unit(0.5, 'cm'),
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 29, 30, 31. Inst thermo capacity ----

vec_plot_line = excel_file_sn[c(20,21,22)]

dt_line_lg$`Installed.Thermoelectric.Capacity,.Reference.Scenario.(GW)` %>% unique()

color_mapping = c("CCGT"= "#00544F",
                  "CHP"= "#669895",
                  "Conventional" = "#97BBA3",
                  "Turbogas Diesel" = "#D4E2D8" ,
                  "Turbogas/Diesel" = "#D4E2D8" ,
                  "Ex-CIP6" = "#97BBFF" ,
                  "Coal" = "#000000",   
                  "Coal USC" = "#00322F", 
                  "CCGT - CM 22-23" = "#D1D1D1",
                  "Self-producers" = "#808080",
                  "Repowering CCGT - CM 22-23" = "#3D5B59",
                  "OCGT/Peaker - CM 22-23" = "#507B5E",
                  "CCGT - CM 24-25" = "#0046CB",
                  "OCGT/Peaker - CM 24-25" = "#FF9933")

levels_order = c("CCGT"   ,                    "CHP"    ,                    "Conventional"   ,           
                 "Turbogas Diesel","Turbogas/Diesel"    ,        "Ex-CIP6"  ,                  "Coal" ,   "Coal USC",            
                 "CCGT - CM 22-23"    ,  "Self-producers",      "Repowering CCGT - CM 22-23" ,"OCGT/Peaker - CM 22-23"  ,  
                 "CCGT - CM 24-25"    ,        "OCGT/Peaker - CM 24-25")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 3
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]])  %>% setDT
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    dt_line_lg[, valori := as.numeric(valori)]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = rev(levels_order))]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = dt_line_lg, aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        scale_fill_manual(values = color_mapping, breaks = rev(levels_order)) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
              legend.position = 'right',
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 34, 35. Net Renewable Installed Cap and prod ----

vec_plot_line = excel_file_sn[c(25,26)]

color_mapping = c(Hydro = "#00544F", 
                  Wind = "#669895", 
                  Geothermal = "#97BBA3",
                  Biomass = "#D4E2D8",
                  Solar = "#FFCC99",
                  `Low Scenario` = "#97BBFF",
                  `High Scenario` = "#007F77")

levels_order = c("Hydro" ,        "Wind" ,         "Geothermal",    "Biomass" ,      "Solar",        
                 "Low Scenario",  "High Scenario")

years_to_display = c(2017,2019,2022,2025, 2030, 2035, 2040, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    dt_line_lg[, (c_name) := fifelse(get(c_name)=="Low","Low Scenario",get(c_name))]
    
    dt_line_lg[, (c_name) := fifelse(get(c_name)=="High","High Scenario",get(c_name))]
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = rev(levels_order))]
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = filter(dt_line_lg, !get(c_name) %in% c("Low Scenario","High Scenario")), aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        geom_line(data = filter(dt_line_lg, get(c_name) == "Low Scenario"), aes(x = anni, y = valori, color = get(c_name), group = 1), linewidth = 1.1) +
        geom_line(data = filter(dt_line_lg, get(c_name) == "High Scenario"), aes(x = anni, y = valori, color = get(c_name), group = 1), linewidth = 1.1) +
        geom_point(data = filter(dt_line_lg, get(c_name) == "Low Scenario"), aes(x = anni, y = valori, color = get(c_name), group = 1), size = 3) +
        geom_point(data = filter(dt_line_lg, get(c_name) == "High Scenario"), aes(x = anni, y = valori, color = get(c_name), group = 1), size = 3) +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_y_continuous(name = NULL) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
              legend.position = 'right',
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 37. Electrolyzer installed cap ----

vec_plot_line = excel_file_sn[c(28)]

color_mapping = c(Reference = "#FF9933", 
                  Low = "#97BBFF")

levels_order = c("Reference","Low")

years_to_display = c(2037:2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = dt_line_lg, aes(x = anni, y = valori, fill = get(c_name)), width = 0.6,position = "dodge") +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_x_continuous(breaks = years_to_display, labels = years_to_display) +
        scale_y_continuous(name = NULL) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 38, 38b, 39. Fixed tilt Solar PV  ----

vec_plot_line = excel_file_sn[c(29,30,31)]

color_mapping = c(`LCOE range` = "#E8E8E8",
                  `Reference` = "#FF9933", 
                  `Low` = "#97BBFF", 
                  `High` = "#007F77",
                  `LCOE Reference` = "#747474"
)
levels_order = c( "LCOE range", "Reference" ,    "Low" ,           "High"    ,       "LCOE Reference")

years_to_display = seq(2025,2050, by = 5)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := fifelse(get(c_name)=="Reference ","Reference",get(c_name))]
    
    dt_geom_ribbon = filter(dt_line_lg, get(c_name) %in% c("LCOE low","LCOE range")) %>% setDT()
    
    names(dt_geom_ribbon)[1] = "title"
    
    dt_geom_ribbon <- dcast(dt_geom_ribbon, anni ~ title, fun.aggregate = mean,value.var = "valori")
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        #geom_ribbon(data = dt_geom_ribbon,aes(x = anni,
        #                                      ymin = `LCOE low`,
        #                                      ymax = `LCOE range`, 
        #                                      fill = "LCOE range"),
        #            alpha = 0.5) + 
        geom_line(data = filter(dt_line_lg, !get(c_name) %in%  c("LCOE low","LCOE range")), aes(x = anni, y = valori, color = get(c_name)), linewidth = 1.1) +
        scale_fill_manual(values = color_mapping) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_x_continuous(breaks = years_to_display, labels = years_to_display) +
        scale_y_continuous(name = NULL) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, color = "#595959"),
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 41, 42. PI + EI Electroch stor  ----

vec_plot_line = excel_file_sn[c(33,34)]

color_mapping = c(
    `Reference` = "#00544F", 
    `Low` = "#669895", 
    `High` = "#97BBA3"
    
)

color_mapping = c(High = "#007F77", Low = "#97BBFF", Reference = "#FF9933")
levels_order = c(  "Reference" ,    "Low" ,           "High"   )

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 2
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := fifelse(get(c_name)=="Reference ","Reference",get(c_name))]
    
    if ( i == 1){
        dt_line_lg = dt_line_lg %>%
            filter(anni < 2038)
        years_to_display = c(2024:2037)
    }
    
    if ( i == 2) {
        years_to_display = c(2024:2040, 2045, 2050)
    }
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = dt_line_lg, aes(x = anni, y = valori, fill = get(c_name)), width = 0.6, position = "dodge") +
        scale_fill_manual(values = color_mapping) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_y_continuous(name = NULL) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 45. Peak-off peak PUN ----

vec_plot_line = excel_file_sn[c(36)]

color_mapping = c(
    `Peak` = "#00544F", 
    `Off-peak` = "#669895", 
    `Holidays` = "#97BBA3"
    
)
levels_order = c(  "Peak" ,    "Off-peak" ,           "Holidays"   )

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    #dt_line_lg[, (c_name) := fifelse(get(c_name)=="Reference ","Reference",get(c_name))]
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    dt_line_lg[, anni := as.character(anni)]
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = dt_line_lg, aes(x = anni, y = valori, fill = get(c_name)), width = 0.6, position = "dodge") +
        scale_fill_manual(values = color_mapping) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        #scale_x_continuous(breaks = years_to_display, labels = years_to_display) +
        scale_y_continuous(name = NULL) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 46. Baseload PUN compon (ref) ----

vec_plot_line = excel_file_sn[c(37)]

color_mapping = c(`Fuel variable cost - CCGT 53%` = "#00544F", 
                  `Logistic variable cost - CCGT 53%` = "#D4E2D8", 
                  `ETS impact - CCGT 53%` = "#97BBA3", 
                  `Clean Spark Spread - CCGT 53%` = "#97BBFF",
                  `PUN` = "#FF9933")


levels_order = c("Fuel variable cost - CCGT 53%" ,    "Logistic variable cost - CCGT 53%",
                 "ETS impact - CCGT 53%"      ,       "Clean Spark Spread - CCGT 53%"  ,  
                 "PUN")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = rev(levels_order))]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = filter(dt_line_lg, get(c_name) != "PUN"), aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        geom_line(data = filter(dt_line_lg, get(c_name) == "PUN"), aes(x = anni, y = valori, color = get(c_name), group = 1), linewidth = 1.1) +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_y_continuous(name = NULL) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 47. Hourly PUN Shape (referenc) ----

vec_plot_line = excel_file_sn[c(38)]

color_mapping = c(`2025` = "#00544F", 
                  `2030` = "#669895", 
                  `2035` = "#FFCC99", 
                  `2040` = "#D4E2D8", 
                  `2050` = "#97BBFF")


levels_order = c("2025", "2030", "2035" ,"2040" ,"2050")

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) 
    dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]
    dt_line = dt_line[ , colSums(is.na(dt_line)) == 0] %>% setDT()
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    new_names = as.numeric(names(dt_line)[2:ncol(dt_line)])*24
    new_names = round(new_names,0)
    
    names(dt_line)[2:ncol(dt_line)] = new_names
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'ora', value.name = 'valori') 
    
    dt_line_lg[, valori := as.numeric(valori)]
    
    setorderv(dt_line_lg, cols = c(c_name, 'ora'))
    
    #dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    plot_line = ggplot() +
        geom_line(data = dt_line_lg, aes(x = ora, y = valori, color = get(c_name), group = get(c_name)), linewidth = 1) +
        #scale_x_discrete(expand = c(0.5, 0)) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        scale_fill_manual(values=color_mapping, breaks = levels_order) + 
        theme_light() +
        guides(color = guide_legend(title = NULL, reverse = FALSE),
               fill = guide_legend(title=NULL),
               linetype = 'none') +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, color = "#595959"),
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 48. Baseload Zonal Prices (ref) ----

vec_plot_line = excel_file_sn[c(39)]

color_mapping = c(`PUN` = "#669895",
                  `North` = "#97BBA3", 
                  `Centre-North` = "#D4E2D8", 
                  `Centre-South` = "#97BBFF", 
                  `South` = "#FFCC99", 
                  `Sicily` = "#00322F",
                  `Sardinia` = "#3D5B59",
                  `Calabria` = "#507B5E")


levels_order = c("PUN", "North", "Centre-North", "Centre-South", "South", "Sardinia", "Sicily", "Calabria")

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) 
    dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]
    dt_line = dt_line[ , colSums(is.na(dt_line)) == 0] %>% setDT()
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, valori := as.numeric(valori)]
    
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    plot_line = ggplot() +
        geom_col(data = dt_line_lg, aes(x = anni, y = valori, fill = get(c_name)), width = 0.6, position = "dodge") +
        #scale_x_discrete(expand = c(0.5, 0)) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        scale_fill_manual(values=color_mapping, breaks = levels_order) + 
        theme_light() +
        guides(color = guide_legend(title = NULL, reverse = FALSE),
               fill = guide_legend(title=NULL),
               linetype = 'none') +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, color = "#595959"),
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 51. Captured CSS of CCGT (ref) ----

vec_plot_line = excel_file_sn[c(42)]

color_mapping = c(`South 53%` = "#00544F",
                  `Baseload` = "#FF9933", 
                  `North 53%` = "#97BBA3", 
                  `Centre-South 53%` = "#97BBFF")


levels_order = c("South 53%" ,       "Baseload" ,
                 "North 53%"   ,     "Centre-South 53%")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, valori := as.numeric(valori)]
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    #dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line = ggplot() +
        geom_line(data = dt_line_lg %>% filter(get(c_name)== "Baseload"), aes(x = anni, y = valori, color = get(c_name), group = get(c_name)), linewidth = 1.1) +
        geom_point(data = dt_line_lg %>% filter(get(c_name)== "Baseload"), aes(x = anni, y = valori, color = get(c_name), group = get(c_name)), size = 3,shape=16) +
        geom_col(data = dt_line_lg %>% filter(get(c_name)!= "Baseload"), aes(x = anni, y = valori, fill = get(c_name)), width = 0.6,position = "dodge") +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        scale_fill_manual(values=color_mapping, breaks = levels_order) + 
        theme_light() +
        guides(color = guide_legend(title = NULL, reverse = FALSE),
               fill = guide_legend(title=NULL),
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 52. Solar Captured Prices (ref) ----

vec_plot_line = excel_file_sn[c(43)]

color_mapping = c(`North Fixed tilt` = "#97BBA3",
                  `North Solar TR` = "#97BBA3", 
                  `South Fixed tilt` = "#97BBFF", 
                  `South Solar TR` = "#97BBFF",
                  `PUN Fixed tilt` = "#00544F",
                  `PUN solar TR` = "#00544F")


levels_order = c("North Fixed tilt" ,"North Solar TR",    "South Fixed tilt" ,"South Solar TR" , 
                 "PUN Fixed tilt" ,  "PUN solar TR")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, valori := as.numeric(valori)]
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := fifelse(get(c_name) =="Noth Solar TR",
                                     "North Solar TR",get(c_name))]
    
    #dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), linetype = get(c_name),group = get(c_name))) +
        geom_line(linewidth = 1) +
        scale_linetype_manual(values = c("North Fixed tilt" = "solid", 
                                         "PUN Fixed tilt" = "solid",
                                         "South Fixed tilt" = "solid",
                                         "North Solar TR" = "dashed", 
                                         "PUN solar TR" = "dashed",
                                         "South Solar TR" = "dashed"
        ), breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        theme_light() +
        guides(color = guide_legend(title = NULL),
               linetype = guide_legend(title = NULL)) +
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 53. Wind Captured Prices (ref) ----

vec_plot_line = excel_file_sn[c(44)]

color_mapping = c(`North - Wind Onshore` = "#00544F",
                  `North - Wind Offshore` = "#00544F", 
                  `South - Wind Onshore` = "#669895", 
                  `South - Wind Offshore` = "#669895",
                  `PUN Onshore` = "#FF9933",
                  `PUN Offshore` = "#FF9933")

levels_order = c("North - Wind Onshore", "North - Wind Offshore", "South - Wind Onshore" ,
                 "South - Wind Offshore", "PUN Onshore"  ,         "PUN Offshore" )

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, valori := as.numeric(valori)]
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    dt_line_lg[, (c_name) := fifelse(get(c_name) =="Noth - Wind Offshore",
                                     "North - Wind Offshore",get(c_name))]
    
    dt_line_lg[, (c_name) := fifelse(get(c_name) =="North -  Wind Onshore",
                                     "North - Wind Onshore",get(c_name))]
    
    #dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), linetype = get(c_name), group = get(c_name))) +
        geom_line(linewidth = 1) +
        scale_linetype_manual(values = c("North - Wind Onshore" = "solid", 
                                         "South - Wind Onshore" = "solid",
                                         "PUN Onshore" = "solid",
                                         "North - Wind Offshore" = "dashed", 
                                         "South - Wind Offshore" = "dashed",
                                         "PUN Offshore" = "dashed"
        ), breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        theme_light() +
        guides(color = guide_legend(title = NULL),
               linetype = guide_legend(title = NULL)) +
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 54. Hydro Run-of-river (ref) ----

vec_plot_line = excel_file_sn[c(45)]

color_mapping = c(`North` = "#00544F",
                  `South` = "#97BBA3", 
                  `PUN` = "#FF9933")

levels_order = c("North","South","PUN")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, valori := as.numeric(valori)]
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    #dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), group = get(c_name))) +
        geom_line(linewidth = 1) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        theme_light() +
        guides(color = guide_legend(title = NULL),
               linetype = guide_legend(title = NULL)) +
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
    
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 55. ASM Ex-ante Zonal Vol (ref) ----

vec_plot_line = excel_file_sn[c(46)]

color_mapping = c(North = "#00544F", 
                  `Centre-North` = "#669895", 
                  `Centre-South` = "#97BBA3",
                  South = "#D4E2D8",
                  Calabria = "#97BBFF",
                  Sicily = "#FFCC99")

levels_order = c("North" ,       "Centre-North" ,"Centre-South",
                 "South"     ,    "Calabria"  ,  "Sicily")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    #dt_line_lg[, anni := as.character(anni)]
    
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = rev(levels_order))]
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = dt_line_lg, aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
        scale_x_continuous(breaks = years_to_display, labels = years_to_display) +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_y_continuous(name = NULL) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 56. ASM Ex-ante Vol (alternat) ----

vec_plot_line = excel_file_sn[c(47)]

color_mapping = c(Reference = "#D4E2D8", 
                  High = "#97BBFF", 
                  Low = "#007F77")

levels_order = c("Reference","High","Low")

years_to_display = c(2024:2040, 2045, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    #dt_line_lg[, anni := as.character(anni)]
    
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = rev(levels_order))]
    
    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display = as.character(years_to_display)
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = filter(dt_line_lg, get(c_name)== "Reference"), aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        geom_line(data = filter(dt_line_lg, get(c_name)!= "Reference" & valori >0), aes(x = anni, y = valori, color = get(c_name), group = get(c_name)), linewidth = 1.1) +
        geom_line(data = filter(dt_line_lg, get(c_name)!= "Reference" & valori <0), aes(x = anni, y = valori, color = get(c_name), group = get(c_name)), linewidth = 1.1) +
        geom_point(data = filter(dt_line_lg, get(c_name)!= "Reference" & valori >0), aes(x = anni, y = valori, color = get(c_name)), size = 3) +
        geom_point(data = filter(dt_line_lg, get(c_name)!= "Reference" & valori <0), aes(x = anni, y = valori, color = get(c_name)), size = 3) +
        geom_hline(yintercept = 0, linetype = "solid", color = "grey") +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_y_continuous(name = NULL) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
    
    ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}

## 57. Start up Shut down ----

vec_plot_line = excel_file_sn[c(48,49,50,51)]

color_mapping = c(North = "#00544F", 
                  `Centre-South` = "#669895", 
                  South = "#97BBA3")

levels_order = c("North","Centre-South","South")

years_to_display = c(2017,2019,2022,2025, 2030, 2035, 2040, 2050)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 3
    
    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]]) %>% setDT() 
    
    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = str_extract_all(c_name_clean, "\\((.*?)\\)")[[1]]
    unit_measure = gsub("\\(|\\)", "", unit_measure)
    unit_measure = paste(rev(unit_measure), collapse = ",")
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
    dt_line_lg[, anni := as.numeric(as.character(anni))]
    setorderv(dt_line_lg, cols = c(c_name, 'anni'))
    
    #dt_line_lg[, anni := as.character(anni)]
    
    #dt_line_lg[, (c_name) := factor(get(c_name), levels = rev(levels_order))]
    
    names(dt_line_lg)[1] = "title"
    
    curr_plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_line(data = dt_line_lg, aes(x = anni, y = valori, color = title), linewidth = 1.1) +
        scale_fill_manual(values = color_mapping, breaks = levels_order) +
        scale_color_manual(values = color_mapping, breaks = levels_order) +
        scale_y_continuous(name = NULL) +
        ylim(0, max(dt_line_lg$valori)+50) +
        theme_light() +
        guides(color = guide_legend(title = NULL),
               fill = guide_legend(title = NULL),
               linetype = 'none',
               width = 'none') +
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
             y = NULL)
    
    #plots_[[i]] = curr_plot_line
    assign(paste0('plot_line',i),curr_plot_line)
    
    rm(curr_plot_line)
    
    
}

plot_line3$labels$caption = expression(bold("Source: ") * "MBS Consulting elaborations")

plot_line = ggarrange(plot_line1,plot_line2,
                      plot_line3, plot_line4,
                      labels = c("", "", "",""),
                      ncol = 2, nrow = 2)

ggsave(file.path('02_report_gen', 'figs', '57. ASM.png'), plot_line,
       width = 9, height = 6)


## 58. 59.  CAPEX ----

vec_plot_line = excel_file_sn[c(52,53)]

levels_order = c("Inverter + Transformer",               
                 "Module",                    
                 "Tracker",                       
                 "Bos + structure",            
                 "Other",                   
                 "Turbine + Inverter",
                 "Bop",                                   
                 "Grid Connection")

color_mapping = c("Inverter + Transformer" = "#FF9933" ,               
                  "Module" = "#669895",                    
                  "Tracker" = "#97BBA3",                       
                  "Bos + structure" = "#0047CA",            
                  "Other" = "#97BBFF",                   
                  "Turbine + Inverter" = "#97BBA3",
                  "Bop" = "#0047CA" ,                                   
                  "Grid Connection" = "#97BBFF"                       
                  )
#Other:  (residual BOP, connection, land)

#levels_order = rev(levels_order)

## Produce Plot

for (i in 1:length(vec_plot_line)) { #i = 1
    
    if ( i == 1) { 
    
    dt_line1 = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) 
    #dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]
    #dt_line = dt_line[ , colSums(is.na(dt_line)) == 0] %>% setDT()
    dt_line1 = dt_line1 %>% setDT()
    
    c_name1 = names(dt_line1)[1]
    c_name_clean1 = gsub("\\.", " ", c_name1)
    unit_measure1 = sub(".*\\((.*)\\).*", "\\1", c_name_clean1)
    c_name_clean1 = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean1)
    
    dt_line_lg1 = dt_line1 %>% 
        melt(id.vars = c_name1, variable.name = 'CAPEX', value.name = 'valori') 
    
    dt_line_lg1[, (c_name1) := factor(get(c_name1), levels = levels_order)]
    
    
    
    plot_line1 = ggplot() +
        geom_col(data = dt_line_lg1, aes(x = CAPEX, y = valori, fill = get(c_name1)), width = 0.4) +
        #scale_x_discrete(expand = c(0.5, 0)) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        scale_fill_manual(values=color_mapping, breaks = levels_order) + 
        theme_light() +
        ylim(0,1500)+
        guides(color = guide_legend(title = NULL, reverse = FALSE),
               fill = guide_legend(title=NULL),
               linetype = 'none') +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              #axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              text = element_text(family = "Aptos Narrow"),
              plot.title = element_text(face = "bold"), 
              plot.subtitle = element_text(face = "italic"),
              legend.position = 'right',
              legend.text = element_text(color = "#595959"),
              legend.key.size = unit(0.3, 'cm'),
              #legend.key.width = unit(0.4, 'cm'),
              plot.caption = element_text(hjust = 0, face = "italic", margin = margin(t = 20))) + 
        labs(title = c_name_clean1,
             subtitle = unit_measure1,
             x = NULL,
             y = NULL,
             caption = expression(bold("Source: ") * "MBS Consulting elaborations"))
    }
    else { 
        
        dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) 
        #dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]
        #dt_line = dt_line[ , colSums(is.na(dt_line)) == 0] %>% setDT()
        dt_line = dt_line %>% setDT()
        
        c_name = names(dt_line)[1]
        c_name_clean = gsub("\\.", " ", c_name)
        unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
        c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
        
        dt_line_lg = dt_line %>% 
            melt(id.vars = c_name, variable.name = 'CAPEX', value.name = 'valori') 
        
        dt_line_lg[, (c_name) := factor(get(c_name), levels = levels_order)]
        
        plot_line2 = ggplot() +
            geom_col(data = dt_line_lg, aes(x = CAPEX, y = valori, fill = get(c_name)), width = 0.4) +
            #scale_x_discrete(expand = c(0.5, 0)) +
            scale_colour_manual(values=color_mapping, breaks = levels_order)+
            scale_fill_manual(values=color_mapping, breaks = levels_order) + 
            theme_light() +
            #ylim(0,1500)+
            guides(color = guide_legend(title = NULL, reverse = FALSE),
                   fill = guide_legend(title=NULL),
                   linetype = 'none') +
            theme(panel.grid.major = element_blank(),  
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  #axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, color = "#595959"),
                  axis.ticks.x = element_blank(),
                  axis.text.y = element_text(color = "#595959"),
                  axis.ticks.y = element_blank(),
                  text = element_text(family = "Aptos Narrow"),
                  plot.title = element_text(face = "bold"), 
                  plot.subtitle = element_text(face = "italic"),
                  legend.position = 'right',
                  legend.text = element_text(color = "#595959"),
                  legend.key.size = unit(0.3, 'cm'),
                  #legend.key.width = unit(0.4, 'cm'),
                  plot.caption = element_text(hjust = 0, face = "italic", margin = margin(t = 20))) + 
            labs(title = c_name_clean,
                 subtitle = unit_measure,
                 x = NULL,
                 y = NULL,
                 caption = expression(bold("Source: ") * "MBS Consulting elaborations"))
    }
}

plot_line1$labels$caption = expression(bold("Source: ") * "MBS Consulting elaborations")

plot_line = ggarrange(plot_line1,plot_line2,
                      labels = c("", ""),
                      ncol = 2, nrow = 1)


ggsave(file.path('02_report_gen', 'figs', paste0(vec_plot_line[[i]], '.png')), plot_line,
       width = 9, height = altezza_grafici)



t1 = Sys.time()

time_diff = round(difftime(t1, t0, units = "mins"),0)

print(paste("Tempo creazione grafici:", time_diff, "minuti"))

