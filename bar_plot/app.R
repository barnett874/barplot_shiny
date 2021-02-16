

# 读入package ---------------------------------------------------------------

library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggprism)
library(ggpubr)
library(tidyverse)
library(ggthemes)
library(rstatix)
library(ggprism)
library(ggsci)
library(DT)
library(agricolae)

# UI界面 --------------------------------------------------------------------

ui <- dashboardPage(skin = "blue",
    header = dashboardHeader(title = "Make a barplot",titleWidth = 300),

# 侧边栏 ---------------------------------------------------------------------
    
sidebar = dashboardSidebar(
    width = 300,
    menuItem(
        text = "Data Import",
        icon = icon("table"),
        startExpanded = TRUE,
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
            choices = c("False", "True"),
            selected = "False"
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
        )
    ),
    menuItem(
        "Others",
        icon = icon("question-circle"),
        selectInput("facet_scale", "Facet scale", c("free", "fixed"))
    )
), 

# 主体 ----------------------------------------------------------------------

# CSS修改一点点细节 --------------------------------------------------------------

    dashboardBody(
        tags$head(type = "text/css", tags$style(HTML(".main-sidebar li a span {color: white;font-size: 20px;}"))),
        tags$style(type = "text/css", 
                   ".js-irs-0 .irs-grid-text:nth-child(n) {color: white}",
                   ".js-irs-0 .irs-grid-pol:nth-of-type(n) {background: white}",
                   ".js-irs-0 .irs-min {color: white}",
                   ".js-irs-0 .irs-max {color: white}",
                   ".js-irs-1 .irs-grid-text:nth-child(n) {color: white}",
                   ".js-irs-1 .irs-grid-pol:nth-of-type(n) {background: white}",
                   ".js-irs-1 .irs-min {color: white}",
                   ".js-irs-1 .irs-max {color: white}"),

# 开始定义主体 ------------------------------------------------------------------

        
        fluidRow(
            box(solidHeader = T, collapsible = T, status = "primary",
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
            selectInput("label_hjust",
                        "X lab hjust:",
                        choices = c(0, .5, 1),
                        selected = 0.5), 
            selectInput("label_vjust",
                        "X lab vjust:",
                        choices = c(0, .5, 1),
                        selected = 0.5), 
            selectInput("label_angle",
                        "X lab angle:",
                        choices = c(0, 45, 90, -45),
                        selected = 0),
            downloadButton(outputId = "downloader_pdf",
                           label = "Download PDF"),
            downloadButton(outputId = "downloader_jpeg",
                           label = "Download JPEG"), width = 3),
        box(
            title = "Barplot",
            status = "success",
            solidHeader = T,
            plotOutput("barplot"),
            width  = 9,
            height = 1000
        )), 
        fluidRow(
            box(plotOutput("barplot_total"), solidHeader = T, collapsible = T, 
                title = "Barplot total", status = "warning", height = 600),
            box(DT::dataTableOutput("table"), solidHeader = T, collapsible = T, 
                title = "Table total", status = "danger", height = 600)
        )))

# server ------------------------------------------------------------------
server <- function(input, output,session) {
    
    df <- iris %>%
        gather(.,
               key,
               value,
               -Species) %>%
        rename("group" = Species)
    

# 更新参数 --------------------------------------------------------------------
    observeEvent(input$import_data, {
        dta <- read_csv(input$import_data$datapath)
        updateSelectInput(session, "name", label = "Select", choices = colnames(dta))
        updateSliderInput(session, "y_limit",label = "Y limits", 0, max(dta$value),.5, value = 0)
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
        
        data$group <- as.factor(data$group) #以防万一
        data$key <- as.factor(data$key) #以防万一 again
        data <- data %>%
            drop_na(value) #以防万一 again and again
    })
    
# 画表格 ----------------------------------------------------------------------
    output$table <- DT::renderDataTable({
        if(is.null(data())){dta_table <- df} #不忍直视了
        dta_table <- data()
        dta_table <- dta_table %>% #过于好用，不需要解释了。
            group_by(group, key) %>% #这参数快被淘汰了，下次可能用across写了
            summarise_each(funs(mean,
                                sd))
        dta_table
    })

# 最主要的图 -------------------------------------------------------------------

# 数据预处理 -------------------------------------------------------------------

    plotInput <- reactive({
        if(is.null(data())) {dta_barplot <- df} #又不是不能用
        
        dta_barplot <- data() #真正的读入数据
        if (data() %>% distinct(key) %>% nrow() > 1) {
            anova <- aov(value ~ group + key, dta_barplot) #ANOVA
            posthoc.test <-
                LSD.test(anova, c('group', 'key'), p.adj = 'bonferroni') #Post hoc
            posthoc.test <-
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
        

# 开始画图 --------------------------------------------------------------------

        if (input$position == "stack") {
            # stacked bars
            p <- dta_barplot %>%
                group_by(group, key) %>% #分组
                summarise_each(funs(mean,
                                    sd)) %>% #计算均值，标准差
                group_by(group) %>% #分组
                mutate(SDPos = cumsum(rev(mean))) %>% #精髓的reverse + cumsum
                left_join(., posthoc.test, by = c("group","key")) %>% 
                ggplot(aes(x = group, y = mean, fill = key), color = input$line_color) + #做图
                geom_bar(
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
                theme(axis.text.x = element_text(
                    angle = as.numeric(input$label_angle),
                    hjust = as.numeric(input$label_hjust),
                    vjust = as.numeric(input$label_vjust)
                ),
                text = element_text(size = input$font_size)) #一点细节
            
        } else {
            if (input$label == "False") { 
                #带星号标记t检验的facet barplot
                stat.test <- dta_barplot %>%
                    group_by(key) %>%
                    t_test(value ~ group) #遇事不决t检验
                stat.test <- stat.test %>%
                    mutate(p.signif = p,
                           p.signif = case_when(
                               p.signif <= 0.0001 ~ "****",
                               p.signif <= 0.001 ~ "***",
                               p.signif <= 0.01 ~ "**",
                               p.signif <= 0.05 ~ "*",
                               p.signif > 0.05~"ns"
                           )
                    ) #不知道为什么当数据只有两组会没有p signif，这里手动写一下。
                stat.test <- stat.test %>% add_y_position(fun = "mean_sd") #给标签加上位置信息
                
                # facet bars with asterisks
                p <- dta_barplot %>%
                    group_by(group, key) %>% #嗨呀，跟上个图一样
                    summarise_each(funs(mean,
                                        sd)) %>%
                    ggplot(aes(x = group, y = mean, fill = key)) +
                    geom_bar(
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
                    facet_wrap( ~ key, scales = input$facet_scale) +
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
                        hjust = as.numeric(input$label_hjust),
                        vjust = as.numeric(input$label_vjust)
                    ),
                    strip.background = element_blank(),
                    strip.text.x = element_blank(),
                    text = element_text(size = input$font_size))
                
            } else {
                # group bars with label
                p <- dta_barplot %>%
                    group_by(group, key) %>%
                    summarise_each(funs(mean,
                                        sd)) %>%
                    left_join(., posthoc.test, by = c("group","key")) %>% #懒得注释了，连字母都是从前面扒的
                    ggplot(aes(x = group, y = mean, fill = key)) +
                    geom_bar(
                        stat = "identity",
                        position = position_dodge(input$bar_gap),
                        width = input$bar_width,
                        color = input$line_color
                    ) +
                    geom_text(aes(label = groups, y = mean + sd * 2),
                              position = position_dodge(input$bar_gap),
                              vjust = 0,
                              size = input$label_size,
                              fontface = "bold") +
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
                    theme(axis.text.x = element_text(
                        angle = as.numeric(input$label_angle),
                        hjust = as.numeric(input$label_hjust),
                        vjust = as.numeric(input$label_vjust)
                    ),
                    text = element_text(size = input$font_size))
            }
        }
    })
    output$barplot <- renderPlot({ #没这个就看不到图了
        print(plotInput())
        ggsave("plot.pdf",
               plotInput(),
               width = input$fig.width/72,
               height = input$fig.length/72)
        ggsave("plot.jpeg",
               plotInput(),
               width = input$fig.width/72,
               height = input$fig.length/72, dpi = 300)
    }, width = function() {input$fig.width}, 
    height = function() {input$fig.length}) #其实可以等用户点了再生成图片的，直接先偷偷生成两个吧
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
            geom_bar(stat = "identity", width = input$bar_width, 
                     color = input$line_color, 
                     fill = mypal[1],
                     position = position_dodge(input$bar_gap)) +
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
    
    output$downloader_jpeg <- downloadHandler( #保存图片*2
        filename = function() {
            "plot.jpeg"
        },
        content = function(file) {
            file.copy("plot.jpeg", file, overwrite=TRUE)
        }
    )
}


shinyApp(ui, server)
