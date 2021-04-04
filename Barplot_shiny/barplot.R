
# 安装package ---------------------------------------------------------------------

packages=c("shiny","ggprism","htmltools","thematic","tidyverse","ggpubr","ggthemes","rstatix","DT","ggpubr", "ggsci", "agricolae","shinydashboard","shinyjs")
ipak <- function(pkg){
    new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
    if (length(new.pkg)) 
        install.packages(new.pkg, dependencies = TRUE, repos='https://mirrors.tuna.tsinghua.edu.cn/CRAN/' )
    sapply(pkg, require, character.only = TRUE)
}
ipak(packages)
jscode <- "shinyjs.closeWindow = function() { window.close(); }" # using shinyjs to stop shiny app

# 读入package ---------------------------------------------------------------
library(shinyjs)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggpubr)
library(ggthemes)
library(rstatix)
library(ggprism)
library(ggsci)
library(DT)
library(agricolae)

# UI界面 --------------------------------------------------------------------

ui <- dashboardPage(
    dashboardHeader(title = "Make a barplot",
                    titleWidth = 300,
                    tags$li(class = "dropdown",
                            tags$li(
                                actionLink(
                                    "close",
                                    "Stop the App",
                                    class = "btn-warning",
                                    icon = icon("stop-circle"),
                                    height = 100
                                )
                            ))), 
    
# 侧边栏 ---------------------------------------------------------------------
    
    dashboardSidebar(
        width = 300,
        menuItem(
            text = "Data Import",
            icon = icon("table"),
            startExpanded = FALSE,
            fileInput(
                "import_data",
                "Choose CSV Data",
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv")
            ),
            selectInput(
                "pivot_long",
                "Long Pivot",
                choices = c("FALSE" = "False", "TRUE" = "True"),
                selected = "FALSE"
            ),
            selectInput(
                "name",
                "Group name",
                choices = colnames(iris),
                selected = "Species"
            )
        ),
        menuItem(
            "Charts",
            icon = icon("chart-bar"),
            startExpanded = FALSE,
            selectInput(
                "position",
                "Position",
                choices = c("Group" = "group", "Stack" = "stack")
            ),
            selectInput(
                "label",
                "Label:",
                choices = c("Asterisk", "Letter"),
                selected = "Asterisk"
            )
        ),
        menuItem(
            "Theme and Palette",
            icon = icon("palette"),
            startExpanded = FALSE,
            sliderInput("slider_theme", "Theme:", 1, 7, 1, value = 7),
            sliderInput("slider_palette", "Palette:", 1, 10, 1, value = 10)
        ),
        menuItem(
            "Set Bars",
            icon = icon("signal"),
            sliderInput("bar_width", "Bar width:", 0, 1, .1, value = 0.6),
            sliderInput("bar_gap", "Bar gap:", 0, 1, .1, value = 0.8),
            sliderInput("y_limit", label = "Y limits", 0, 10, .5, value = 0),
            selectInput(
                "line_color",
                "Line color:",
                c("Grey10", "Grey30", "Grey50", "Grey70", "Grey90", "black"),
                selected = "black"
            ),
            selectInput("bar_fill",
                        "Fill",
                        c("group", "key"),
                        selected = "key")
        ),
        menuItem(
            "Facet",
            icon = icon("border-all"),
            selectInput("facet_warp", "Facet warp", c("Yes", "No")),
            selectInput("facet_scale", "Facet scale", c("free", "fixed")),
            selectInput("facet_row", "Facet row", c("Null", 1:10)),
            selectInput("facet_col", "Facet column", c("Null", 1:10))
        )
    ),
    
# 主体 ----------------------------------------------------------------------
    
    dashboardBody(
        
        useShinyjs(),
        extendShinyjs(text = jscode, functions = c("closeWindow")),
        
# CSS修改一些细节，实在不知道怎么修改更好了 --------------------------------------------------
        
        tags$head(type = "text/css", tags$style(HTML(".main-sidebar li a span {color: white;font-size: 20px;}"))),
        tags$style(type = "text/css", 
                   ".js-irs-0 .irs-grid-text:nth-child(n) {color: white}",
                   ".js-irs-0 .irs-grid-pol:nth-of-type(n) {background: white}",
                   ".js-irs-0 .irs-min {color: white}",
                   ".js-irs-0 .irs-max {color: white}",
                   ".js-irs-1 .irs-grid-text:nth-child(n) {color: white}",
                   ".js-irs-1 .irs-grid-pol:nth-of-type(n) {background: white}",
                   ".js-irs-1 .irs-min {color: white}",
                   ".js-irs-1 .irs-max {color: white}",
                   ".js-irs-2 .irs-grid-text:nth-child(n) {color: white}",
                   ".js-irs-2 .irs-grid-pol:nth-of-type(n) {background: white}",
                   ".js-irs-2 .irs-min {color: white}",
                   ".js-irs-2 .irs-max {color: white}",
                   ".js-irs-3 .irs-grid-text:nth-child(n) {color: white}",
                   ".js-irs-3 .irs-grid-pol:nth-of-type(n) {background: white}",
                   ".js-irs-3 .irs-min {color: white}",
                   ".js-irs-3 .irs-max {color: white}",
                   ".js-irs-4 .irs-grid-text:nth-child(n) {color: white}",
                   ".js-irs-4 .irs-grid-pol:nth-of-type(n) {background: white}",
                   ".js-irs-4 .irs-min {color: white}",
                   ".js-irs-4 .irs-max {color: white}"),
        
        fluidRow(
            box(
                solidHeader = T,
                collapsible = T,
                status = "primary",
                title = "Figure Label and Size",
                textInput("x_title", "X title:"),
                textInput("y_title", "Y title:"),
                textInput("fill_lab", "Legend:"),
                sliderInput("fig.width",
                            "Figure width:",
                            100, 900, 50, value = 800),
                sliderInput("fig.length",
                            "Figure length:",
                            100, 900, 50, value = 600),
                sliderInput("font_size",
                            "Label font size",
                            6, 32, 1, value = 16),
                sliderInput("label_size",
                            "Asterisk/letter size",
                            1, 20, 1, value = 6),
                selectInput(
                    "legend_position",
                    "Legend position",
                    c("right","top","bottom","none")
                ),
                selectInput(
                    "label_angle",
                    "X lab angle:",
                    choices = c(0, 45, 90, -45),
                    selected = 0
                ),

                downloadButton(outputId = "downloader_pdf",
                               label = "Download PDF"),
                downloadButton(outputId = "downloader_jpg",
                               label = "Download JPG"),
                width = 3
            ),
            box(
                title = "Barplot",
                status = "success",
                solidHeader = T,
                plotOutput("barplot"),
                width  = 9,
                height = 1000
            )
        ),
        fluidRow(
            box(
                plotOutput("barplot_total"),
                solidHeader = T,
                collapsible = T,
                title = "Barplot total",
                status = "warning",
                height = 600
            ),
            box(
                DT::dataTableOutput("table"),
                solidHeader = T,
                collapsible = T,
                title = "Table total",
                status = "danger",
                height = 600
            )
        ))
)



# server ------------------------------------------------------------------
server <- function(input, output,session) {

    
# 更新参数 --------------------------------------------------------------------
    observeEvent(input$import_data, {
        dta <- read_csv(input$import_data$datapath)
        updateSelectInput(session, "name", label = "Select", choices = colnames(dta))
        updateSliderInput(session, "y_limit",label = "Y limits", 0, max(dta %>% select(where(is.numeric))),.5, value = 0)
    })
    
# 读取文件 --------------------------------------------------------------------
    
    
    data <- reactive({
        if (is.null(input$import_data)) { #默认数据
            data <- iris
        } else { #读取文件
            data <- read_csv(input$import_data$datapath)
        }
        
        if (input$pivot_long == "True") { #如果你已经调好格式了
            colnames(data) <- c("group","key","value") #改个名就好啦
        } else { #正常人的选择
            data <- data %>%
                gather(.,
                       key,
                       value,
                       -input$name) %>% #边长
                rename("group" = input$name) #改个名
        }
        
        data$group <- factor(data$group, levels = unique(data$group)) #以防万一
        data$key <- factor(data$key, level = unique(data$key)) #以防万一 again
        data$value <- as.numeric(data$value)
        data <- data %>%
            drop_na(value) #以防万一 again and again
    })
    
#画表格 ----------------------------------------------------------------------
    output$table <- DT::renderDataTable({
        if (is.null(data())) {
            dta_table <- iris %>%
                gather(.,
                       key,
                       value,
                       -Species) %>%
                rename("group" = Species)
        } #不忍直视了
    dta_table <- data()
        dta_table <- dta_table %>% #过于好用，不需要解释了。
            group_by(group, key) %>% #这参数快被淘汰了，下次可能用across写了
            summarise_each(funs(mean,
                                sd))
        dta_table
    })
    
# 最主要的图 -------------------------------------------------------------------
    
    plotInput <- reactive({
        if(is.null(data())) {dta_barplot <- iris %>%
            gather(.,
                   key,
                   value,
                   -Species) %>%
            rename("group" = Species)} #又不是不能用
        
        dta_barplot <- data() #真正的读入数据
            if (dta_barplot %>% distinct(group) %>% nrow() > 2) {
                
                key_name <- dta_barplot %>% distinct(key) %>% .$key
                posthoc <- data.frame(
                    value = double(),
                    groups = character(),
                    key = character(),
                    group = character(),
                    stringsAsFactors = FALSE
                )

                for (i in 1:length(key_name)) {
                    anova <- dta_barplot %>%
                        filter(key == key_name[i]) %>%
                        aov(value ~ group, .)
                    posthoc.test <- anova %>%
                        LSD.test(., "group", p.adj = 'bonferroni')
                    posthoc <- posthoc.test$groups %>%
                        mutate(key = key_name[i],
                               group = rownames(.)) %>%
                        bind_rows(., posthoc)
                } #合并结果
            } else if (dta_barplot %>% distinct(key) %>% nrow() > 2) {
                anova <- aov(value ~ group + key, dta_barplot) #ANOVA
                posthoc.test <-
                    LSD.test(anova, c('group', 'key'), p.adj = 'bonferroni') 

                posthoc <-
                    posthoc.test$groups %>%
                    mutate(name = row.names(.)) %>%
                    separate(name, into = c("group", "key"), sep = ":") #合并结果
            }
        
        #set theme
        switch(
            input$slider_theme,
            theme_set(theme_few()),
            theme_set(theme_bw()),
            theme_set(theme_classic()),
            theme_set(theme_pubclean()),
            theme_set(theme_pubr()),
            theme_set(theme_minimal()),
            theme_set(theme_prism())
        )
        #set palette
        mypal <- switch(
            input$slider_palette,
            pal_npg()(9),
            pal_jco()(9),
            pal_lancet()(9),
            pal_locuszoom()(9),
            prism_fill_pal(palette = "prism_light")(9),
            prism_fill_pal(palette = "floral")(12),
            prism_fill_pal(palette = "prism_dark")(10),
            prism_fill_pal(palette = "viridis")(6),
            prism_fill_pal(palette = "warm_and_sunny")(10),
            prism_fill_pal(palette = "black_and_white")(9)
        )
        label_just = case_when(
            input$label_angle == 0 ~ c(.5,.5),
            input$label_angle == 45 ~ c(1,1),
            input$label_angle == -45 ~ c(0,1),
            input$label_angle == 90 ~ c(1,.5)
        )
        
# 开始画图 --------------------------------------------------------------------
        
        if (input$position == "stack") {
            # stacked bars
            p <- dta_barplot %>%
                group_by(group, key) %>% #分组
                summarise_each(funs(mean,
                                    sd)) %>% #计算均值，标准差
                group_by(group) %>% #分组
                mutate(SDPos = cumsum(rev(mean))) %>% #精髓的reverse + cumsum
                left_join(., posthoc, by = c("group","key")) %>% 
                ggplot(aes(x = group, y = mean, fill = key)) + #做图
                geom_bar(
                    color = input$line_color,
                    stat = "identity",
                    width = input$bar_width,
                    position = position_stack(input$bar_gap)
                ) + # 柱状图
                geom_text(aes(label = groups), 
                          position = position_stack(vjust = 0.5),
                          vjust = 0.5,
                          size = input$label_size,
                          fontface = "bold") + #标签
                geom_errorbar(
                    aes(ymax = SDPos + sd, ymin = SDPos - sd),
                    width = input$bar_width / 4,
                    position = "identity"
                ) + #误差线
                scale_fill_manual(values = mypal) + #颜色
                scale_y_continuous(expand = expansion(mult = c(0, .1))) + #设置柱状图从(0,0)点开始
                labs(x = input$x_title,
                     y = input$y_title,
                     fill = input$fill_lab)  + #xy title name
                theme(
                    axis.text.x = element_text(
                        angle = as.numeric(input$label_angle),
                        hjust = as.numeric(label_just[1]),
                        vjust = as.numeric(label_just[2])
                    ),
                    legend.position = input$legend_position,
                    text = element_text(size = input$font_size)
                ) #一点细节
            
        } else {
            if (input$label == "Asterisk") { 
                #带星号标记t检验的facet barplot
                stat.test <- dta_barplot %>%
                    group_by(key) %>%
                    t_test(value ~ group) #遇事不决t检验
                stat.test <- stat.test %>%
                    adjust_pvalue(method = "bonferroni") %>% 
                    add_significance("p.adj") %>% 
                    mutate(p.signif = p,
                           p.signif = case_when(
                               p.signif <= 0.0001 ~ "****",
                               p.signif <= 0.001 ~ "***",
                               p.signif <= 0.01 ~ "**",
                               p.signif <= 0.05 ~ "*",
                               p.signif > 0.05~"ns"
                           )
                    ) #不知道为什么当数据只有两组会没有p signif，这里手动写一下。
                stat.test <- stat.test %>% 
                    add_xy_position(fun = "mean_sd", 
                                    x = "group", 
                                    dodge = input$bar_gap) #给标签加上位置信息
                
                # facet bars with asterisks
                p <- dta_barplot %>%
                    group_by(group, key) %>% #嗨呀，跟上个图一样
                    summarise_each(funs(mean,
                                        sd)) %>%
                    ungroup() %>% 
                    ggplot(aes(x = group, y = mean)) +
                    geom_bar(
                        aes_string(fill = input$bar_fill),
                        stat = "identity",
                        width = input$bar_width,
                        color = input$line_color,
                        position = position_dodge(input$bar_gap)
                    ) +
                    geom_errorbar(
                        aes(ymax = mean + sd, ymin = mean - sd),
                        position = "identity",
                        width = input$bar_width / 3
                    ) +
                    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
                    scale_fill_manual(values = mypal) +
                    coord_cartesian(ylim = c(as.double(input$y_limit), NA)) +
                    labs(x = input$x_title,
                         y = input$y_title,
                         fill = input$fill_lab) +
                    stat_pvalue_manual(
                        stat.test,
                        label = "p.signif",
                        size = input$label_size,
                        tip.length = 0.01,
                        hide.ns = TRUE
                    )  +
                    theme(axis.text.x = element_text(
                        angle = as.numeric(input$label_angle),
                        hjust = as.numeric(label_just[1]),
                        vjust = as.numeric(label_just[2])
                    ),
                    legend.position = input$legend_position,
                    text = element_text(size = input$font_size))
                if (input$facet_warp == "Yes") {
                    p +
                        facet_wrap(~ key,
                                   scales = input$facet_scale, 
                                   nrow = as.integer(input$facet_row),
                                   ncol = as.integer(input$facet_col))
                }
                else {
                    p +
                        facet_wrap(
                            ~ key,
                            scales = "fixed",
                            nrow = 1)
                }
            } else {
                # group bars with label
                p <- dta_barplot %>%
                    group_by(group, key) %>%
                    summarise_each(funs(mean,
                                        sd)) %>%
                    left_join(., posthoc, by = c("group","key")) %>% #懒得注释了，连字母都是从前面扒的
                    ggplot(aes_string(x = "group", y = "mean", group = "key", fill = input$bar_fill)) +
                    geom_bar(
                        stat = "identity",
                        position = position_dodge(input$bar_gap),
                        width = input$bar_width,
                        color = input$line_color
                    ) +
                    geom_text(
                        aes(label = groups,
                            y = mean + sd * 2), 
                        position = position_dodge(input$bar_gap),
                        vjust = 0,
                        size = input$label_size,
                        fontface = "bold"
                    )+
                    geom_errorbar(
                        aes(ymax = mean + sd, ymin = mean - sd),
                        position = position_dodge(input$bar_gap),
                        width = input$bar_width / 3
                    ) +
                    scale_fill_manual(values = mypal) +
                    coord_cartesian(ylim = c(as.double(input$y_limit), NA)) +
                    labs(x = input$x_title,
                         y = input$y_title,
                         fill = input$fill_lab) +
                    scale_y_continuous(expand = expansion(mult = c(0, .1))) +
                    theme(
                        axis.text.x = element_text(
                            angle = as.numeric(input$label_angle),
                            hjust = as.numeric(label_just[1]),
                            vjust = as.numeric(label_just[2])
                        ),
                        text = element_text(size = input$font_size),
                        legend.position = input$legend_position
                    ) 
                
                if (input$facet_warp == "Yes") {
                    p +
                        facet_wrap(~ key,
                                   scales = input$facet_scale, 
                                   nrow = as.integer(input$facet_row),
                                   ncol = as.integer(input$facet_col))
                }
                else {p}
            }
        }
    })
    output$barplot <- renderPlot({
        #没这个就看不到图了
        print(plotInput())
        ggsave(
            "plot.pdf",
            plotInput(),
            width = input$fig.width / 72,
            height = input$fig.length / 72
        )
        ggsave(
            "plot.jpg",
            plotInput(),
            width = input$fig.width / 72,
            height = input$fig.length / 72,
            dpi = 300
        )
    }, width = function() {
        input$fig.width
    },
    height = function() {
        input$fig.length
    }) #其实可以等用户点了再生成图片的，直接先偷偷生成两个吧
    # 另一个柱状图 ------------------------------------------------------------------
    output$barplot_total <- renderPlot({
        if(is.null(data())){dta_total <- df} #丈育环节
        dta_total <- data() 
        if (input$pivot_long == "True") {#如果你已经调好格式了
            dta_total <- dta_total %>%
                transmute(group = paste0(group," ", key), #改名
                          value = value)
        } else { #正常人的选择
            dta_total <- dta_total %>%
                transmute(group = paste0(group," ", key), #合体
                          value = value)
        }
        
        anova <- aov(value ~ group, dta_total) #ANOVA
        posthoc.test <- LSD.test(anova, 'group', p.adj = 'bonferroni') 
        
        #这结果也就看看，尤其是你把各种维度的数据丢一起比较的时候
        
        #set theme
        switch(
            input$slider_theme,
            theme_set(theme_few()),
            theme_set(theme_bw()),
            theme_set(theme_classic()),
            theme_set(theme_pubclean()),
            theme_set(theme_pubr()),
            theme_set(theme_minimal()),
            theme_set(theme_prism())
        )
        #set palette
        mypal <- switch(
            input$slider_palette,
            pal_npg()(9),
            pal_jco()(9),
            pal_lancet()(9),
            pal_locuszoom()(9),
            prism_fill_pal(palette = "prism_light")(9),
            prism_fill_pal(palette = "floral")(12),
            prism_fill_pal(palette = "prism_dark")(10),
            prism_fill_pal(palette = "viridis")(6),
            prism_fill_pal(palette = "warm_and_sunny")(10),
            prism_fill_pal(palette = "black_and_white")(9)
        )
        
        dta_sum <- dta_total %>% 
            group_by(group) %>% 
            summarise_each(funs(mean, 
                                sd)) %>% #总之就是好用，但是要被淘汰了
            ungroup() 
        out <- posthoc.test$groups %>% mutate(group = row.names(.)) %>% select(-value)
        dta_sum %>% left_join(.,out, by = "group") %>% 
            ggplot(aes(x = group, y = mean)) + #画图嘛，都差不多
            geom_bar(
                aes_string(fill = input$bar_fill),
                stat = "identity",
                width = input$bar_width,
                color = input$line_color,
                fill = mypal[1],
                position = position_dodge(input$bar_gap)
            ) +
            geom_errorbar(aes(ymax = mean + sd, ymin = mean - sd),
                          position = "identity",
                          width = input$bar_width / 3) +
            scale_y_continuous(expand = expansion(mult = c(0, .1))) +
            coord_cartesian(ylim=c(as.double(input$y_limit),NA)) + 
            labs(x = input$x_title, y = input$y_title, fill = input$fill_lab) +
            geom_text(aes(label = groups, y = (mean + 2 * sd) * 1.05), 
                      position = position_dodge(input$bar_gap),
                      size = 6,
                      vjust = 0,
                      fontface = "bold") + 
            theme(
                axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5),
                text = element_text(size=16)
            )
    }, height = 500
    )
    
    # Download ----------------------------------------------------------
    output$downloader_pdf <- downloadHandler( #保存图片
        filename = function() {
            "plot.pdf"
        },
        content = function(file) {
            file.copy("plot.pdf", file, overwrite=TRUE)
        }
    )
    
    output$downloader_jpg <- downloadHandler( #保存图片*2
        filename = function() {
            "plot.jpg"
        },
        content = function(file) {
            file.copy("plot.jpg", file, overwrite=TRUE)
        }
    )
    ## colose window
    observeEvent(input$close, {
        js$closeWindow()
        stopApp()
    })
    session$onSessionEnded(function() {
        stopApp()
    })
}

shinyApp(ui, server)
