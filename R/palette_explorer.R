get_default_contrast <- function(type, m) {
    if (type=="seq") {
        default_contrast_seq(m)
    } else {
        default_contrast_div(m)
    }
}


plot_brewer_pals <- function(type, m, contrast=NULL, stretch=NULL, cex=.8) {
    pal_info <- brewer.pal.info
    pal_nm <- row.names(pal_info)[pal_info$category==type]

    n <- length(pal_nm)


    grid.newpage()
    label_width <- convertWidth(stringWidth("Acbdefghij"), "in", valueOnly = TRUE) * cex
    lH <- convertHeight(unit(1, "lines"), "in", valueOnly=TRUE) * cex
    vp <- viewport(layout = grid.layout(nrow = 2*n + 1, ncol=m+2,
                                        widths = unit(c(label_width, rep(1, m), label_width/4), c("in", rep("null", m), "in")),
                                        heights = unit(c(lH, rep(c(lH, .33*lH), length.out=2*n+1), 1), c(rep("in", 2*n), "null"))))
    pushViewport(vp)

    lapply(1L:m, function(j) {
        pushViewport(viewport(layout.pos.row = 1, layout.pos.col = 1+j))
        grid.text(j, gp=gpar(cex=cex))
        popViewport()
    })

    lapply(1L:n, function(i) {
        pushViewport(viewport(layout.pos.row = i*2, layout.pos.col = 1))
        grid.text(pal_nm[i], x = .1, just = "left", gp=gpar(cex=cex))
        popViewport()

        if (type=="qual") {
            pal <- get_brewer_pal(pal_nm[i], n=m, stretch=stretch, plot=FALSE)
            ids <- which(pal==pal[1])[-1]
        } else {
            pal <- get_brewer_pal(pal_nm[i], n=m, contrast=contrast, plot=FALSE)
            ids <- numeric(0)
        }

        lapply(1L:m, function(j) {
            pushViewport(viewport(layout.pos.row = i*2, layout.pos.col = 1+j))
            grid.rect(gp=gpar(fill=pal[j]))
            if (j %in% ids) {
                grid.circle(x=0, y=.5, r=.25, gp=gpar(fill="white", lwd=1))
            }
            popViewport()
        })
    })
    popViewport()
}

palette_explorer <- function() {
    shinyApp(ui = fluidPage(
            shinyjs::useShinyjs(),
            div(
                style = "font-size:75%;line-height:20px",
                fluidRow(
                    column(4,
                           h4("Sequential"),
                           sliderInput("m_seq", "Number of colors:",
                                       min = 3, max = 20, value = 7),
                           checkboxInput("auto_seq", label = "Automatic contrast range", value = TRUE),
                           uiOutput("contrast_seq_slider")
                    ), column(8, h3(), plotOutput("plot_seq", height = "285px"), p())),
                fluidRow(
                    column(4,
                           h4("Categorical"),
                           sliderInput("m_cat", "Number of colors:",
                                       min = 2, max = 20, value = 8),
                           checkboxInput("stretch", "Stretch", value=FALSE)
                    ), column(8, h3(), plotOutput("plot_cat", height = "131px"), p())),
                fluidRow(
                    column(4,
                           h4("Diverging"),
                           sliderInput("m_div", "Number of colors:",
                                       min = 3, max = 20, value = 9),
                           checkboxInput("auto_div", label = "Automatic contrast range", value = TRUE),
                           uiOutput("contrast_div_slider")
                    ), column(8, h3(), plotOutput("plot_div", height = "147px"), p())),
                fluidRow(
                    column(4,
                           p("Output Code"),
                           radioButtons("direct_tmap", "Type of code", choices = c("Direct", "tmap"), selected = "Direct")
                    ), column(8, p(), uiOutput("code")))
            )
        ),
        server = function(input, output) {

            output$contrast_seq_slider <- renderUI({
                rng <- get_default_contrast("seq", input$m_seq)
                if (is.null(input$auto_seq) || input$auto_seq) {
                    isolate({
                        sliderInput("contrast_seq", "Contrast:",
                                min = 0, max = 1, value = c(rng[1], rng[2]), step = .01)
                    })
                } else {
                    isolate({
                        crng <- input$contrast_seq
                        sliderInput("contrast_seq", "Contrast:",
                                    min = 0, max = 1, value = c(crng[1], crng[2]), step = .01)
                    })
                }
            })
            output$contrast_div_slider <- renderUI({
                rng <- get_default_contrast("div", input$m_div)
                if (is.null(input$auto_div) || input$auto_div) {
                    isolate({
                        sliderInput("contrast_div", "Contrast:",
                                    min = 0, max = 1, value = c(rng[1], rng[2]), step = .01)
                    })
                } else {
                    isolate({
                        crng <- input$contrast_div
                        sliderInput("contrast_div", "Contrast:",
                                    min = 0, max = 1, value = c(crng[1], crng[2]), step = .01)
                    })
                }
            })

            observe({
                input$m_seq
                if (input$auto_seq) {
                    delay(0, {
                        toggleState("contrast_seq", !input$auto_seq)
                    })
                }
            })

            observe({
                input$m_div
                if (input$auto_div) {
                    delay(0, {
                        toggleState("contrast_div", !input$auto_div)
                    })
                }
            })


            output$plot_seq <- renderPlot({
                if (is.null(input$m_seq) || is.null(input$contrast_seq)) return(NULL)
                plot_brewer_pals(type="seq", m = input$m_seq, contrast = input$contrast_seq)
            })
            output$plot_cat <- renderPlot({
                if (is.null(input$m_cat) || is.null(input$stretch)) return(NULL)
                plot_brewer_pals(type="qual", m = input$m_cat, stretch = input$stretch)
            })
            output$plot_div <- renderPlot({
                if (is.null(input$m_div) || is.null(input$contrast_div)) return(NULL)
                plot_brewer_pals(type="div", m = input$m_div, contrast = input$contrast_div)
            })




            output$code <- renderText({
                contr <- input$contrast_seq
                paste("Get palette colors:  get_brewer_pal(\"Blues\", n = ", input$m_seq, ", contrast = c(", contr[1], ", ", contr[2], "))", sep = "")
            })



        }
    )
}
