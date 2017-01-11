get_default_contrast <- function(type, m) {
    if (type=="seq") {
        default_contrast_seq(m)
    } else {
        default_contrast_div(m)
    }
}


plot_brewer_pals <- function(type, m, contrast, stretch, cex=.8) {
    pal_info <- brewer.pal.info
    pal_nm <- row.names(pal_info)[pal_info$category==type]

    n <- length(pal_nm)


    grid.newpage()
    label_width <- convertWidth(stringWidth("Acbdefghij"), "in", valueOnly = TRUE) * cex
    lH <- convertHeight(unit(1, "lines"), "in", valueOnly=TRUE) * cex
    vp <- viewport(layout = grid.layout(nrow = 2*n, ncol=m+2,
                                        widths = unit(c(label_width, rep(1, m), label_width/4), c("in", rep("null", m), "in")),
                                        heights = unit(c(rep(c(lH, .33*lH), length.out=2*n+1), 1), c(rep("in", 2*n-1), "null"))))
    pushViewport(vp)
    lapply(1L:n, function(i) {
        pushViewport(viewport(layout.pos.row = i*2-1, layout.pos.col = 1))
        grid.text(pal_nm[i], x = .1, just = "left", gp=gpar(cex=cex))
        popViewport()

        if (type=="qual") {
            pal <- get_brewer_pal(pal_nm[i], n=m, stretch=stretch, plot=FALSE)
        } else {
            pal <- get_brewer_pal(pal_nm[i], n=m, contrast=contrast, plot=FALSE)
        }

        lapply(1L:m, function(j) {
            pushViewport(viewport(layout.pos.row = i*2-1, layout.pos.col = 1+j))
            grid.rect(gp=gpar(fill=pal[j]))
            popViewport()
        })
    })
    popViewport()
}

palette_explorer <- function() {
    runApp(list(

        ui = shinyUI(fluidPage(
            div(
            titlePanel("Hello Shiny!"),

            fluidRow(
                column(4,
                       sliderInput("m_seq", "Number of colors:",
                                   min = 1, max = 20, value = 5),
                       sliderInput("contrast_seq", "Contrast:",
                                   min = 0, max = 1, value = c(.2,.6))
                ), column(8, plotOutput("plot_seq", height = "300px"))),
            fluidRow(
                column(4,
                       sliderInput("m_cat", "Number of colors:",
                                   min = 1, max = 20, value = 5),
                       checkboxInput("stretch", "Stretch", value=FALSE)
                ), column(8, plotOutput("plot_cat", height = "169px"))),
            fluidRow(
                column(4,
                       sliderInput("m_div", "Number of colors:",
                                   min = 1, max = 20, value = 5),
                       sliderInput("contrast_div", "Contrast:",
                                   min = 0, max = 1, value = c(.2,.6))
                ), column(8, plotOutput("plot_div", height = "192px"))),
            style = "font-size:75%"
            )
        )),
        server = function(input, output) {
            output$plot_seq <- renderPlot({
                plot_brewer_pals(type="seq", m = input$m_seq, contrast = input$contrast_seq)
            })
            output$plot_cat <- renderPlot({
                print(input$m_cat)
                print(input$stretch)
                plot_brewer_pals(type="qual", m = input$m_cat, stretch = input$stretch)
            })
            output$plot_div <- renderPlot({
                plot_brewer_pals(type="div", m = input$m_div, contrast = input$contrast_div)
            })
        }
    ))
}
