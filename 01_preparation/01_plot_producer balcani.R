
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

#**********************************************************************************
#*               DEFINIRE SE SCENARIO, STORICO O SCENARIO CON STORICO     *********
#**********************************************************************************

periodo = "SCENARIO"
# periodo = "STORICO"
# periodo = "SCENARIO_STORICO"


altezza_grafici = 4

levels_order = c("Greece", "Bulgaria", "Romania")
color_mapping = c(Bulgaria = "#0079C5", Romania = "#008996", Greece = "#001437")

linetype_mapping = c("Bulgaria" = "solid", 
                     "Romania" = "solid",
                     "Greece" = "solid")


## File 
excel_file = file.path('documenti', 'Grafici report scenari balcani.xlsx')
excel_file_sn = xl$getSheetNames(excel_file)



if(periodo=="SCENARIO"){
    years_to_display = c(2025:2040, 2045, 2050)
    years_label = c(2025,2030,2040,2050)
    vec_plot_line = excel_file_sn[c(2,3,4,5,9,12,19,20,27,28,30,34,35,36,37)]
    
    angolo_asse = 90

} 
if (periodo=="STORICO"){
    years_to_display = c(2019:2024)
    years_label = c(2019:2024)
    vec_plot_line = excel_file_sn[c(2,3,4,5,9,12)]

    angolo_asse = 0
}
    

years_label_scen = c(2025,2030,2040,2050)

## Produce Plots

for (i in 1:length(vec_plot_line)) { 

    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) %>% setDT() 
    dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]

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

    if(i %in% c(1,2,3)){
        plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), linetype = get(c_name),group = get(c_name))) +
        geom_line(linewidth = 1.1) +
        scale_linetype_manual(values = linetype_mapping, breaks = levels_order) +
        scale_x_discrete(breaks = years_to_display, labels = years_to_display) +
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        theme_light() +
        #guides(color = guide_legend(title = NULL),
        #      linetype = guide_legend(title = NULL)) +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = angolo_asse, vjust = 0.5, hjust = 1, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              axis.line = element_line(colour = "#595959", linewidth = 0.3, linetype = "solid"),
              text = element_text(family = "Aptos Narrow"),
              plot.title = element_text(face = "bold"), 
              plot.subtitle = element_text(face = "italic"),
              legend.position = 'none',
              #legend.position = 'top',
              legend.text = element_text(color = "#595959"),
              plot.caption = element_text(hjust = 0, face = "italic", margin = margin(t = 20))) + 
        labs(title = c_name_clean,
             subtitle = unit_measure,
             x = NULL,
             y = NULL,
             caption = expression(bold("Source: ") * "MBS Consulting elaborations"))+
        geom_text(data=subset(dt_line_lg,anni %in% years_label_scen),aes(anni, valori, color = get(c_name),label=round(valori,1)),
            nudge_y=2,nudge_x=-0.25,show.legend = FALSE,size=3,check_overlap = TRUE)+ 
        geom_text(data=subset(dt_line_lg,anni %in% c(2019,2022,2024)),aes(anni, valori, color = get(c_name),label=round(valori,1)),
            nudge_y=6,nudge_x=0,show.legend = FALSE,size=3,check_overlap = TRUE)+ 
        geom_text(data=subset(dt_line_lg,anni %in% c(2020,2021)),aes(anni, valori, color = get(c_name),label=round(valori,1)),
            nudge_y=6,nudge_x=-0.1,show.legend = FALSE,size=3,check_overlap = TRUE)+  
        geom_text(data=subset(dt_line_lg,anni %in% c(2023)),aes(anni, valori, color = get(c_name),label=round(valori,1)),
            nudge_y=6,nudge_x=+0.1,show.legend = FALSE,size=3,check_overlap = TRUE)+      
        geom_point(data=subset(dt_line_lg,anni %in% years_label),aes(anni, valori))
    }else{
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
              axis.text.x = element_text(angle = angolo_asse, vjust = 0.5, hjust = 1, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              axis.line = element_line(colour = "#595959", linewidth = 0.3, linetype = "solid"),
              text = element_text(family = "Aptos Narrow"),
              plot.title = element_text(face = "bold"), 
              plot.subtitle = element_text(face = "italic"),
              #legend.position = 'none',
              legend.position = 'top',
              legend.text = element_text(color = "#595959"),
              plot.caption = element_text(hjust = 0, face = "italic", margin = margin(t = 20))) + 
        labs(title = c_name_clean,
             subtitle = unit_measure,
             x = NULL,
             y = NULL,
             caption = expression(bold("Source: ") * "MBS Consulting elaborations"))+
        geom_text(data=subset(dt_line_lg,anni %in% years_label),aes(anni, valori, color = get(c_name),label=round(valori,1)),
                    nudge_y=2,nudge_x=0,show.legend = FALSE,size=3,check_overlap = TRUE)+ 
        geom_point(data=subset(dt_line_lg,anni %in% years_label),aes(anni, valori))
    }
    
    if("%" %in% unit_measure) {
        plot_line = plot_line + 
            scale_y_continuous(labels = scales::percent_format()) 
    } else {
        plot_line
    }   
    
    if(periodo=="SCENARIO"){
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    }else if(periodo=="STORICO"){
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '_storico.png')), plot_line,
           width = 9, height = altezza_grafici)
    }
    

}


###########################################################################################################################################
## 15,16,17 . DAM Energy Balance ----

vec_plot_line = excel_file_sn[c(6,7,8)]

levels_order = c("Demand + storage consumption",                   
                 "Net import",                       
                 "Hydro",                       
                 "BESS production",
                 "Renewables"  ,                                   
                 "Gas",                       
                 "Coal/lignite",           
                 "Nuclear",           
                 "Other")

levels_order = rev(levels_order)

color_mapping = c("Demand + storage consumption" = "#001437" ,                     
                  "Net import" = "#D75641",                       
                  "Hydro" = "#0079C5",                      
                  "BESS production" = "#C1D6C8",
                  "Renewables" = "#008996" ,                                   
                  "Gas" = "#001437",                       
                  "Coal/lignite" = "#BFBFBF",    
                  "Nuclear" = "#32587B",                  
                  "Other" = "#9FBDD8")

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

    dt_line_lg = dt_line_lg %>%
        filter(anni %in% years_to_display)
    
    plot_line = ggplot() +
        geom_col(data = filter(dt_line_lg, get(c_name) != "Demand + storage consumption"), aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        geom_line(data = filter(dt_line_lg, get(c_name) == "Demand + storage consumption"), aes(x = anni, y = valori, color = get(c_name), group = 1), linewidth = 1) +
        geom_point(data = filter(dt_line_lg, get(c_name) == "Demand + storage consumption"), aes(x = anni, y = valori, color = get(c_name), group = 1), size = 3) +
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
              axis.text.x = element_text(angle = angolo_asse, vjust = 0.5, hjust = 1, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              axis.line = element_line(colour = "#595959", linewidth = 0.3, linetype = "solid"),
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
    
    if(periodo=="SCENARIO"){
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    }else if(periodo=="STORICO"){
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '_storico.png')), plot_line,
           width = 9, height = altezza_grafici)
    }
    
}


###########################################################################################################################################
##22,23,24 - 25,26,27. Inst thermo capacity and thermo production----

vec_plot_line = excel_file_sn[c(13,14,15,16,17,18)]

dt_line_lg$`Installed.Thermoelectric.Capacity,.Greece.(GW)` %>% unique()

color_mapping = c("CCGT"= "#001437",
                  "Coal/lignite" = "#BFBFBF", 
                  "Nuclear" = "#0079C5")

levels_order = c("CCGT"   ,                    "Coal/lignite"    ,                    "Nuclear")


## Produce Plot

for (i in 1:length(vec_plot_line)) { 
    
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
              axis.text.x = element_text(angle = angolo_asse, vjust = 0.5, hjust = 1, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              text = element_text(family = "Aptos Narrow"),
              axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
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
    
    if(periodo=="SCENARIO"){
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    }else if(periodo=="STORICO"){
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '_storico.png')), plot_line,
           width = 9, height = altezza_grafici)
    }
    
}
###########################################################################################################################################

## 30,31,32,33,34, 35. Net Renewable Installed Cap and prod ----

vec_plot_line = excel_file_sn[c(21,22,23,24,25,26)]

color_mapping = c(Hydro = "#001437", 
                  Wind = "#0079C5", 
                  Biomass = "#D75641",
                  Solar = "#008996",
                  `TOTAL` = "#001437")

levels_order = c("Hydro" ,        "Wind" ,    "Biomass" ,      "Solar",        
                 "TOTAL")

if(periodo=="SCENARIO"){
    years_to_display_res = c(2025, 2030, 2035, 2040, 2050)
}else if(periodo=="STORICO"){
    years_to_display_res = years_to_display
}

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
        filter(anni %in% years_to_display_res)
    
    dt_line_lg[, anni := as.character(anni)]
    
    years_to_display_res = as.character(years_to_display_res)

    
    dt_line_lg[, (c_name) := fifelse(get(c_name)=="Total","TOTAL",get(c_name))]
    
    dt_line_lg[, (c_name) := factor(get(c_name), levels = rev(levels_order))]
    
    plot_line =
        dt_line_lg %>%
        ggplot() +
        geom_col(data = filter(dt_line_lg, !get(c_name) %in% c("TOTAL")), aes(x = anni, y = valori, fill = get(c_name)), width = 0.6) +
        geom_line(data = filter(dt_line_lg, get(c_name) == "TOTAL"), aes(x = anni, y = valori, color = get(c_name), group = 1), linewidth = 1.1) +
        geom_point(data = filter(dt_line_lg, get(c_name) == "TOTAL"), aes(x = anni, y = valori, color = get(c_name), group = 1), size = 3) +
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
              axis.text.x = element_text(angle = angolo_asse, vjust = 0.5, hjust = 1, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
              text = element_text(family = "Aptos Narrow"),
              plot.title = element_text(face = "bold"), 
              plot.subtitle = element_text(face = "italic"),
              legend.position = 'right',
              legend.text = element_text(color = "#595959"),
              plot.caption = element_text(hjust = 0, face = "italic", margin = margin(t = 20))) + 
        geom_text(data=filter(dt_line_lg, get(c_name) %in% c("TOTAL")),aes(anni, valori, color = get(c_name),label=round(valori,1)),
            nudge_y=1.5,nudge_x=0,show.legend = FALSE,size=3)+ 
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
    
    if(periodo=="SCENARIO"){
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
           width = 9, height = altezza_grafici)
    }else if(periodo=="STORICO"){
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '_storico.png')), plot_line,
           width = 9, height = altezza_grafici)
    }
    
}


####################################################################################################################
#                                   GRAFICI SOLO SCENARIO                                                         #
####################################################################################################################

if(periodo=="SCENARIO"){

    ###########################################################################################################################################
    ## 10. World LNG Demand ----

    if(periodo=="SCENARIO"){
        vec_plot_line = excel_file_sn[c(1)]

    color_mapping = c(`Existing Capacity` = "#D9D9D9", 
                    `Under Construction Capacity` = "#9FBDD8", 
                    `PreFID  Capacity` = '#9FBDD8',
                    `Uncertain Capacity` = "#D75641",
                    `Historical Demand` = "#7F7F7F",
                    `Demand` = "#001437")
    color_mapping %>% names()
    levels_order = c("Existing Capacity"    ,                "Under Construction Capacity"   ,       "PreFID  Capacity",
                    "Uncertain Capacity",                      "Historical Demand" ,          "Demand" )

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
            mutate(Under.Construction.Capacity = Existing.Capacity + Under.Construction.Capacity + PreFID.Capacity)
        
        dt_line <- dt_line %>%
            mutate(Uncertain.Capacity = Under.Construction.Capacity + Uncertain.Capacity)
        
        dt_line_lg = dt_line %>% 
            melt(id.vars = 'Quarter', variable.name = 'Variable', value.name = 'Values') 
        
        dt_line_lg <- dt_line_lg[str_detect(Quarter, "^Q2|^Q4")]
        
        dt_line_lg$Quarter = factor(dt_line_lg$Quarter, levels = unique(dt_line_lg$Quarter))
        
        dt_line_lg[, lower_bound := 0]
        
        dt_line_lg$Variable = gsub("\\."," ",dt_line_lg$Variable)
        
        plot_line =
            dt_line_lg %>%
            ggplot(aes(group = Variable)) +
            geom_ribbon(data = dt_line_lg %>% filter(Variable == "Uncertain Capacity"), 
                        aes(x = Quarter, ymin = dt_line_lg %>% filter(Variable == "Uncertain Capacity") %>% pull(lower_bound),
                            ymax = dt_line_lg %>% filter(Variable == "Uncertain Capacity") %>% pull(Values),
                            fill = Variable)) +
         geom_ribbon(data = dt_line_lg %>% filter(Variable == "PreFID  Capacity"), 
                        aes(x = Quarter, ymin = dt_line_lg %>% filter(Variable == "PreFID  Capacity") %>% pull(lower_bound),
                            ymax = dt_line_lg %>% filter(Variable == "PreFID  Capacityn") %>% pull(Values),
                            fill = Variable)) +
            geom_ribbon(data = dt_line_lg %>% filter(Variable == "Under Construction Capacity"), 
                        aes(x = Quarter, ymin = dt_line_lg %>% filter(Variable == "Under Construction Capacity") %>% pull(lower_bound),
                            ymax = dt_line_lg %>% filter(Variable == "Under Construction Capacity") %>% pull(Values),
                            fill = Variable)) +
            geom_ribbon(data = dt_line_lg %>% filter(Variable == "Existing Capacity"), 
                        aes(x = Quarter, ymin = dt_line_lg %>% filter(Variable == "Existing Capacity") %>% pull(lower_bound),
                            ymax = dt_line_lg %>% filter(Variable == "Existing Capacity") %>% pull(Values),
                            fill = Variable)) +
            geom_line(data = filter(dt_line_lg, !Variable %in% c('Uncertain Capacity','PreFID  Capacity','Under Construction Capacity','Existing Capacity')), aes(x = Quarter, y = Values, color = Variable, linetype = Variable), linewidth = 1.1) +
            scale_fill_manual(values = color_mapping, breaks = levels_order) +
            scale_color_manual(values = color_mapping, breaks = levels_order) +
            scale_linetype_manual(values = c(`Existing Capacity` = "solid", 
                                            `Under Construction Capacity` = "solid", 
                                            `PreFID  Capacity` = "solid", 
                                            `Uncertain Capacity` = "solid",
                                            `Historical Demand` = "dashed",
                                            `Demand` = "solid"
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
                axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
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
        
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
            width = 9, height = altezza_grafici)
        
        }
    }

    ###########################################################################################################################################
    ## 19. E-mobility ----
    vec_plot_line = excel_file_sn[c(10)]

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
        
        dt_line_lg[, (c_name) := factor(get(c_name), levels = c('Greece','Bulgaria','Romania'))]
        
        dt_line_lg[, cluster := factor(cluster, levels = c('PHEV','BEV'))]
        
        plot_line = ggplot() +
            geom_col(data = dt_line_lg, aes(x = get(c_name), y = valori, fill = cluster), width = 0.6) +
            scale_x_discrete(expand = c(0.5, 0)) +
            scale_fill_manual(values=c('BEV'='#001437', 'PHEV'= '#9FBDD8'), breaks = c('BEV','PHEV'))+
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
                axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
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
        
        
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
            width = 9, height = altezza_grafici)
        
    }

    ###########################################################################################################################################
    ## 20. Heating & Cooling ----

    vec_plot_line = excel_file_sn[c(11)]

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
        
        dt_line_lg[, (c_name) := factor(get(c_name), levels = c('Greece','Bulgaria','Romania'))]
        
        plot_line = ggplot() +
            geom_col(data = dt_line_lg, aes(x = get(c_name), y = valori, fill = cluster), width = 0.6, show.legend = FALSE) +
            scale_x_discrete(expand = c(0.5, 0)) +
            scale_fill_manual("",values=c('#001437'))+
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
                axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
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
        
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
            width = 9, height = altezza_grafici)
        
    }

    ###########################################################################################################################################

    ## 38 Electroch stor  ----

    vec_plot_line = excel_file_sn[c(29)]

    color_mapping = c(
        `Greece` = "#001437", 
        `Bulgaria` = "#0079C5", 
        `Romania` = "#008996"
        
    )

    color_mapping = c(Romania = "#008996", Bulgaria = "#0079C5", Greece = "#001437")
    levels_order = c(  "Greece" ,    "Bulgaria" ,           "Romania"   )

    years_to_display = c(2025,2030,2035,2040, 2045, 2050)

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
        
        dt_line_lg[, (c_name) := fifelse(get(c_name)=="Greece ","Greece",get(c_name))]
        
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
                axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
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
        
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
            width = 9, height = altezza_grafici)
        
    }
    ###########################################################################################################################################

    ## 40,41,42. Hourly Price Shape ----

    vec_plot_line = excel_file_sn[c(31,32,33)]

    color_mapping = c(`2025` = "#001437", 
                    `2030` = "#008996", 
                    `2035` = "#D75641", 
                    `2040` = "#0079C5", 
                    `2050` = "#96C768")


    levels_order = c("2025", "2030", "2035" ,"2040" ,"2050")

    ## Produce Plot

    for (i in 1:length(vec_plot_line)) { #i = 1
        
        dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) 
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
            #scale_colour_manual(values=color_mapping, breaks = levels_order)+
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
                axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
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
        
        
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
            width = 9, height = altezza_grafici)
        
    }
    ###########################################################################################################################################

    ## 47,48.  CAPEX ----

    vec_plot_line = excel_file_sn[c(38,39)]

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
                axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
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
                    axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
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


    ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '.png')), plot_line,
        width = 9, height = altezza_grafici)



}  #FINE GRAFICI SCENARIO


####################################################################################################################
#                                   GRAFICI SOLO STORICO                                           #
####################################################################################################################

if(periodo=="STORICO"){
    
    ################################################################################################################
    ## 30 Baseload Price

    vec_plot_line = excel_file_sn[c(30,36,37)]

    color_mapping = c(
        `Greece` = "#001437", 
        `Bulgaria` = "#0079C5", 
        `Romania` = "#008996"
        
    )

    color_mapping = c(Romania = "#008996", Bulgaria = "#0079C5", Greece = "#001437")
    levels_order = c(  "Greece" ,    "Bulgaria" ,           "Romania"   )

    ## Produce Plot

    for (i in 1:length(vec_plot_line)) { #i = 2
        
        dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) %>% setDT() 
        dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]
        
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
        
        dt_line_lg[, (c_name) := fifelse(get(c_name)=="Greece ","Greece",get(c_name))]
        
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
            #scale_y_continuous(name = NULL) +
            scale_y_continuous(limits=c(0, max(dt_line_lg[,valori])+50),n.breaks=4) + 
            theme_light() +
            guides(color = guide_legend(title = NULL),
                fill = guide_legend(title = NULL),
                linetype = 'none',
                width = 'none') +
            theme(panel.grid.major = element_blank(),  
                panel.grid.minor = element_blank(),
                panel.border = element_blank(),
                axis.text.x = element_text(angle = angolo_asse, vjust = 0.5, hjust = 1, color = "#595959"),
                axis.ticks.x = element_blank(),
                axis.text.y = element_text(color = "#595959"),
                axis.ticks.y = element_blank(),
                axis.line = element_line(colour = "#595959", size = 0.3, linetype = "solid"),
                text = element_text(family = "Aptos Narrow"),
                plot.title = element_text(face = "bold"), 
                plot.subtitle = element_text(face = "italic"),
                legend.position = 'top',
                legend.text = element_text(color = "#595959"),
                plot.caption = element_text(hjust = 0, face = "italic", margin = margin(t = 20))) + 
            geom_text(data=subset(dt_line_lg,anni %in% years_label),aes(anni, valori, color = get(c_name),label=round(valori,1)),
                show.legend = FALSE,size=3,check_overlap = TRUE,position = position_dodge(1.1),
                vjust =-0.5)+ 
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
        
        ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '_storico.png')), plot_line,
            width = 9, height = altezza_grafici)
        
    }


}   #FINE GRAFICI STORICI



###########################################################################################################################################

t1 = Sys.time()

time_diff = round(difftime(t1, t0, units = "mins"),0)

print(paste("Tempo creazione grafici:", time_diff, "minuti"))









vec_plot_line = excel_file_sn[c(41,42,43)]


#date_mese = c("Jan-19","Feb-19","Mar-19","Apr-19","May-19","Jun-19","Jul-19","Aug-19","Sep-19","Oct-19","Nov-19","Dec-19",
            "Jan-20","Feb-20","Mar-20","Apr-20","May-20","Jun-20","Jul-20","Aug-20","Sep-20","Oct-20","Nov-20","Dec-20",
            "Jan-21","Feb-21","Mar-21","Apr-21","May-21","Jun-21","Jul-21","Aug-21","Sep-21","Oct-21","Nov-21","Dec-21",
            "Jan-22","Feb-22","Mar-22","Apr-22","May-22","Jun-22","Jul-22","Aug-22","Sep-22","Oct-22","Nov-22","Dec-22",
            "Jan-23","Feb-23","Mar-23","Apr-23","May-23","Jun-23","Jul-23","Aug-23","Sep-23","Oct-23","Nov-23","Dec-23",
            "Jan-24","Feb-24","Mar-24","Apr-24","May-24","Jun-24","Jul-24","Aug-24","Sep-24","Oct-24","Nov-24","Dec-24")


            date_mese = c("Jan-19","","","","","","Jul-19","","","","","",
            "Jan-20","","","","","","Jul-20","","","","","",
            "Jan-21","","","","","","Jul-21","","","","","",
            "Jan-22","","","","","","Jul-22","","","","","",
            "Jan-23","","","","","","Jul-23","","","","","",
            "Jan-24","","","","","","Jul-24","","","","","Dec-24")
            aa=dt_line_lg[seq(1,length(date_mese),6),anni]

for (i in 1:length(vec_plot_line)) { 

    dt_line = xl$read.xlsx(excel_file, sheet = vec_plot_line[[i]], skipEmptyRows=FALSE) %>% setDT() 
    dt_line = dt_line[1:which(is.na(dt_line))[1]-1,]

    c_name = names(dt_line)[1]
    c_name_clean = gsub("\\.", " ", c_name)
    unit_measure = sub(".*\\((.*)\\).*", "\\1", c_name_clean)
    c_name_clean = gsub("\\s*\\([^\\)]+\\)", "", c_name_clean)
    
    dt_line_lg = dt_line %>% 
        melt(id.vars = c_name, variable.name = 'anni', value.name = 'valori') 
    
   #   dt_line_lg[, anni := as.character(date_mese)]
  # setorderv(dt_line_lg, cols = c(c_name, 'anni'))
     plot_line = ggplot(dt_line_lg, aes(anni, valori, color = get(c_name), linetype = get(c_name),group = get(c_name))) +
        geom_line(linewidth = 1.1) +
        scale_linetype_manual(values = linetype_mapping, breaks = levels_order) +
        scale_x_discrete( labels = date_mese )+
        scale_colour_manual(values=color_mapping, breaks = levels_order)+
        theme_light() +
        #guides(color = guide_legend(title = NULL),
        #      linetype = guide_legend(title = NULL)) +
        theme(panel.grid.major = element_blank(),  
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              axis.text.x = element_text(angle = angolo_asse, vjust = 0.5, hjust = 1, color = "#595959"),
              axis.ticks.x = element_blank(),
              axis.text.y = element_text(color = "#595959"),
              axis.ticks.y = element_blank(),
              axis.line = element_line(colour = "#595959", linewidth = 0.3, linetype = "solid"),
              text = element_text(family = "Aptos Narrow"),
              plot.title = element_text(face = "bold"), 
              plot.subtitle = element_text(face = "italic"),
              legend.position = 'none',
              #legend.position = 'top',
              legend.text = element_text(color = "#595959"),
              plot.caption = element_text(hjust = 0, face = "italic", margin = margin(t = 20))) + 
        labs(title = c_name_clean,
             subtitle = unit_measure,
             x = NULL,
             y = NULL,
             caption = expression(bold("Source: ") * "MBS Consulting elaborations"))+
        geom_text(data=subset(dt_line_lg,anni %in% aa) ,aes(anni, valori, color = get(c_name),label=round(valori,1)),
            nudge_y=10,nudge_x=0,show.legend = FALSE,size=3,check_overlap = TRUE)
        
    
    
    if("%" %in% unit_measure) {
        plot_line = plot_line + 
            scale_y_continuous(labels = scales::percent_format()) 
    } else {
        plot_line
    }   
    

       ggsave(file.path('documenti', paste0(vec_plot_line[[i]], '_storico.png')), plot_line,
           width = 9, height = altezza_grafici)
    
}
