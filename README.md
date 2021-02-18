
## 写在前面

春节期间跟家里人聊天的时候，再一次提到了小时候的时候拆家里手表的事。然后又是对我一阵数落。孩童时的我对这个结构复杂、设计精巧的玩意充满了好奇，总是想要机会拆开看看里面到底装着什么。父母知道我的心思自然不敢把手表轻易放在外头。但机会这东西，等一等总会有的。终于还是被我逮到了机会。拆开容易，拼回去就难了。结局免不了一顿暴揍，然后被父母记到现在......手表某种程度上代表了人类制造工具的某种极致，而拆手表嘛则是小孩子极致的好奇心了。至于挨揍嘛，纯属活该了......扯远了。

## 一个画柱状图的插件

本来我是准备当一个安静的鸽子，等陈博士把rserver插件完善了（起码写一个可用的交互界面吧）。但，架不住~~博士小姐姐~~shawn有特别的姿势。一顿骚操作就把shiny包搞定了，加上陈博士写个网页弹出功能，shiny的最后一块拼图算是齐了。工头喊一嗓子，开工了......这次的插件主要功能是画柱状图......柱状图算是科研作图的基础，任何一个杂志随便翻翻都能看到柱状图。之前也写过几篇关于柱状图的文章检验自己对R语言的学习程度。新的一年了，正好是时候检验一下对r和ggplot2的认识又到了什么水准了。这次，我们用shiny折腾了一个柱状图。我才不会说是因为Barplot这个名字看着就会排在前面呢。如果那样子，我肯定会改名叫A barplot了，你细品，你仔细品。总之这次的插件主要能画三种图，分别是带星号标记的，带字母标记的分组柱状图和堆积柱状图。

![](https://upload-images.jianshu.io/upload_images/24272581-1e754b74bcbb5aff.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)


PS：输入文件参考鸢尾花数据集。如果你还不清楚，直接去rstudio里输入iris就能看到了。大概类似下面的结构。

| Group  | Factor_1 | Factor_2 | ... | Factor_n |
|--------|----------|----------|-----|----------|
| Name_1 | value    | value    | ... | value    |
| Name_1 | value    | value    | ... | value    |
| ...    | ...      | ...      | ... | ...      |
| Name_2 | value    | value    | ... | value    |
| Name_2 | value    | value    | ... | value    |
| Name_2 | value    | value    | ... | value    |

## 界面

![image.png](https://upload-images.jianshu.io/upload_images/24272581-c484c883c284b2d5.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)


先从最左边的菜单开始做起。

![image.png](https://upload-images.jianshu.io/upload_images/24272581-1adbdc81985d8def.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)


左边侧边栏第一个是输入数据的地方，默认CSV文件（既默认数据以半角逗号分隔）。然后选择数据是否已经整理为透视表，这个选项不常用。一般默认第一列是分组名，如果不是，第三个选项可以换一下。已经设定好会读取输入的文件的列名并自动更新到下拉菜单里。

此外，因为没有改默认设置，数据文件默认不超过5 M。

![](https://upload-images.jianshu.io/upload_images/24272581-68ce24bb3b6800b2.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)


然后就是选择是分组柱状图或者是堆积图，需要星号标记或者是星号标记。这里未来会给更多的选项让你选择到底用t检验，ANOVA，还是非参数检验。此外标记默认数据服从正态分布并且满足方差齐性。未来可能把对应的检验也加进去（感觉又在给自己挖坑了）。另外字母标记对所有的可能性还是没做到遍历，这部分未来还需要改进，希望得到用户的反馈。

    # t检验

    stat.test <- dta_barplot %>%

      group_by(key) %>%

      t_test(value ~ group) #遇事不决t检验

    stat.test <- stat.test %>%

      adjust_pvalue(method = "bonferroni") %>%

      add_significance("p.adj")

    # 两因素ANOVA（如果需要请自行魔改）

    anova <-

      aov(value ~ group + key, dta_barplot) #ANOVA

    posthoc.test <-

      LSD.test(anova, c('group', 'key'), p.adj = 'bonferroni')

    posthoc <-

      posthoc.test$groups %>%

      mutate(name = row.names(.)) %>%

      separate(name, into = c("group", "key"), sep = ":")

    # 单因素ANOVA

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

    }

![](https://upload-images.jianshu.io/upload_images/24272581-1fecb9526f7a8f5f.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

第三个菜单是设置颜色和主题，这里集成了ggpubr，ggprism和ggthemes的部分主题，如果你有更好的，不妨自己加到代码里。

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

![](https://upload-images.jianshu.io/upload_images/24272581-692f0b1fddb3d646.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)


接着是柱子的设定，分别是柱子宽度，间隔（不一定可设置），这里设定误差线的宽度是柱宽度的1/3，如果觉得丑，请用意志力挺过去。Y轴最小值，这个也是准备取消的东西，因为变了会造成差别很大的错觉，想不从0点开始请选择箱线图。然后是柱形边框颜色和颜色填充的类别。颜色填充这块也没有覆盖到所有情况，所以，在改了在改了......

![image.png](https://upload-images.jianshu.io/upload_images/24272581-7473578302c1231f.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)


分面的选项，嗯。

![](https://upload-images.jianshu.io/upload_images/24272581-2bb75cef6e1d07f6.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)


其实星号标记的柱状图没有设定不分面，如果选否起始是设定为行数为1，固定y轴的分面图，本人发际线实在不够用了。

另外三个是facet里比较有意思的选项，自行体会吧。

![](https://upload-images.jianshu.io/upload_images/24272581-51ddac85fc52cf9c.png?imageMogr2/auto-orient/strip%7CimageView2/2/w/1240)

最后就是X，Y轴标题和标签的标题（这个ggprism填了也不出东西，神秘，可能有更具体的修改选项吧）。这块没有写expression函数，估计是不支持特殊的字符和上下角标了......然后就是图的尺寸，整个图（除了字母和星号标记）的字号，字母和星号标记的尺寸，标签的位置（起始还可以有个坐标选项的，下次加上吧）和最后的x轴标签角度。

大概就介绍这么多吧，想要做的选项还有很多。例如：Y轴的tick，Y轴是否使用log_scale以及上面提到的......TBtools的界面方面，毕竟有shiny了，就这么简化着吧......

## 最后

当初学习R的动力就是想画个柱状图，学会后才发现如果学R只是为了画图那就买椟还珠了。R语言提供了从数据载入、清洗到统计分析再到作图的全套流程。特别是dplyr提供的整套类似thinking flow的数据处理流程和purrr简化的匿名函数，分析数据的整个流程如水流般水到渠成。如果这个小程序能够激发起你学习R的兴趣，如果你好奇R到底是怎么把数据变成图表的，索性就拆开这个包，看看里面的函数都是做什么的，最后尝试自己用R处理数据吧。希望拆开这个小插件后能在里面发现自己的好奇心。还能孩子多久......

PS：今天用别的电脑打开才发现，插件里的中文注释都是乱码......我有一句mmp不知当讲不当讲
