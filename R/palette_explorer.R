get_default_contrast <- function(type, m) {
    if (type=="seq") {
        default_contrast_seq(m)
    } else {
        default_contrast_div(m)
    }
}


plot_brewer_pals <- function(type, m, contrast=NULL, stretch=NULL, cex=.8, print.hex = FALSE, col.blind="normal") {
    pal_info <- brewer.pal.info
    pal_nm <- row.names(pal_info)[pal_info$category==type]
    pal_labels <- pal_nm

    n <- length(pal_nm)

    if (col.blind!="normal") {
        cb <- pal_info[pal_nm, ]$colorblind
        pal_labels[pal_labels %in% c("Dark2", "Paired", "Set2")] <- c("Dark2 (3)", "Paired (4)", "Set2 (3)")
    } else {
        cb <- rep(TRUE, n)
    }


    grid.newpage()
    label_width <- convertWidth(stringWidth("Acbdefghijk"), "in", valueOnly = TRUE) * cex
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
        #if (cb[i]) grid.rect(gp=gpar(col=NA, fill="yellow"))
        grid.text(pal_labels[i], x = .1, just = "left", gp=gpar(cex=cex, col=ifelse(cb[i], "#000000", "#999999")))
        #if (!cb[i]) grid.lines(x=c(.1,.9), y=c(.5, .5), gp=gpar(col="#AAAAAA"))
        popViewport()

        if (type=="qual") {
            pal <- get_brewer_pal(pal_nm[i], n=m, stretch=stretch, plot=FALSE)
            ids <- which(pal==pal[1])[-1]
        } else {
            pal <- get_brewer_pal(pal_nm[i], n=m, contrast=contrast, plot=FALSE)
            ids <- numeric(0)
        }

        if (col.blind != "normal") pal <- dichromat(pal, type=col.blind)


        fontcol <- ifelse(is_light(pal), "black", "white")
        fontwidth <- convertWidth(stringWidth("#FFFFFFAA"), unitTo = "npc", valueOnly = TRUE)
        fontsize <- min(cex, (1/fontwidth) / m)


        lapply(1L:m, function(j) {
            pushViewport(viewport(layout.pos.row = i*2, layout.pos.col = 1+j))
            grid.rect(gp=gpar(fill=pal[j]))
            if (print.hex) grid.text(pal[j], gp=gpar(cex=fontsize, col=fontcol[j]))
            if (j %in% ids) {
                grid.circle(x=0, y=.5, r=.25, gp=gpar(fill="white", lwd=1))
            }
            popViewport()
        })
    })
    popViewport()
}

is_light <- function(col) {
    colrgb <- col2rgb(col)
    apply(colrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
}

#' Explore color palettes
#'
#' This interactive tool shows all Color Brewer palettes, where the number of colors can be adjusted as well as the constrast range. Categorical (qualitative) palettes can be stretched when the number of colors exceeds the number of palette colors. Output code needed to get the desired color values is generated. Finally, all colors can be tested for color blindness.
#' @export
#' @importFrom grDevices col2rgb
#' @importFrom dichromat dichromat
#' @example ./examples/palette_explorer.R
#' @seealso \code{\link{get_brewer_pal}}, \code{\link[dichromat:dichromat]{dichromat}}, \code{\link[RColorBrewer:RColorBrewer]{RColorBrewer}}
#' @references \url{http://www.color-blindness.com/types-of-color-blindness/}
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
palette_explorer <- function() {
    if (!requireNamespace("shiny")) stop("shiny package needed for this function to work. Please install it.", call. = FALSE)
    if (!requireNamespace("shinyjs")) stop("shinyjs package needed for this function to work. Please install it.", call. = FALSE)

    shiny::shinyApp(ui = shiny::fluidPage(
            shinyjs::useShinyjs(),
            shiny::div(
                style = "font-size:75%;line-height:20px",
                    shiny::column(4,
                          shiny::br(),
                          shiny::h4("Sequential"),
                          shiny::sliderInput("m_seq", "Number of colors",
                               min = 3, max = 20, value = 7),
                          shiny::strong("Contrast range"),
                          shiny::checkboxInput("auto_seq", label = "Automatic", value = TRUE),
                          shiny::uiOutput("contrast_seq_slider"),
                          shiny::br(),
                          shiny::h4("Categorical"),
                          shiny::sliderInput("m_cat", "Number of colors",
                                             min = 3, max = 20, value = 8),
                          shiny::checkboxInput("stretch", "Stretch", value=TRUE),
                          shiny::br(),
                          shiny::h4("Diverging"),
                          shiny::sliderInput("m_div", "Number of colors",
                                             min = 3, max = 20, value = 9),
                          shiny::strong("Contrast range"),
                          shiny::checkboxInput("auto_div", label = "Automatic", value = TRUE),
                          shiny::uiOutput("contrast_div_slider")
            ),
            shiny::column(8, shiny::h4(), shiny::plotOutput("plot_seq", height = "285px"),
                             shiny::uiOutput("code_seq"),
                             shiny::p(),
                          shiny::h4(), shiny::plotOutput("plot_cat", height = "131px"),
                          shiny::uiOutput("code_cat"),
                          shiny::p(),
                          shiny::plotOutput("plot_div", height = "147px"),
                          shiny::uiOutput("code_div"),
                          shiny::wellPanel(
                              shiny::fluidRow(
                                  shiny::column(4,
                                                shiny::h4("Options"),
                                                shiny::checkboxInput("hex", "Print color values", value = FALSE)
                                  ),
                                  shiny::column(4,
                                                shiny::radioButtons("direct_tmap", "Code generator", choices = c("Direct code", "tmap layer function code"), selected = "Direct code", inline = FALSE)
                                  ),
                                  shiny::column(4,
                                                shiny::radioButtons("col_blind", "Color blindness simulator", choices = c("Normal" ="normal", "Deuteranopia" = "deutan", "Protanopia" = "protan", "Tritanopia" = "tritan"), selected = "normal", inline = FALSE)
                                  )
                          ))

                    )
        )),
        server = function(input, output) {

            output$contrast_seq_slider <- shiny::renderUI({
                rng <- get_default_contrast("seq", input$m_seq)
                if (is.null(input$auto_seq) || input$auto_seq) {
                    shiny::isolate({
                        shiny::div(
                            style = "font-size:0;margin-top:-20px",
                            shiny::sliderInput("contrast_seq", "",
                                min = 0, max = 1, value = c(rng[1], rng[2]), step = .01)

                        )
                    })
                } else {
                    shiny::isolate({
                        crng <- input$contrast_seq
                        shiny::div(
                            style = "font-size:0;margin-top:-20px",
                            shiny::sliderInput("contrast_seq", "",
                                    min = 0, max = 1, value = c(crng[1], crng[2]), step = .01)
                        )
                    })
                }
            })
            output$contrast_div_slider <- shiny::renderUI({
                rng <- get_default_contrast("div", input$m_div)
                if (is.null(input$auto_div) || input$auto_div) {
                    shiny::isolate({
                        shiny::div(
                            style = "font-size:0;margin-top:-20px",
                            shiny::sliderInput("contrast_div", "",
                                    min = 0, max = 1, value = c(rng[1], rng[2]), step = .01)
                        )
                    })
                } else {
                    shiny::isolate({
                        crng <- input$contrast_div
                        shiny::div(
                            style = "font-size:0;margin-top:-20px",
                            shiny::sliderInput("contrast_div", "",
                                    min = 0, max = 1, value = c(crng[1], crng[2]), step = .01)
                        )
                    })
                }
            })

            shiny::observe({
                input$m_seq
                if (input$auto_seq) {
                    shinyjs::delay(0, {
                        shinyjs::toggleState("contrast_seq", !input$auto_seq)
                    })
                }
            })

            shiny::observe({
                input$m_div
                if (input$auto_div) {
                    shinyjs::delay(0, {
                        shinyjs::toggleState("contrast_div", !input$auto_div)
                    })
                }
            })


            output$plot_seq <- shiny::renderPlot({
                if (is.null(input$m_seq) || is.null(input$contrast_seq)) return(NULL)
                plot_brewer_pals(type="seq", m = input$m_seq, contrast = input$contrast_seq, print.hex = input$hex, col.blind = input$col_blind)
            })
            output$plot_cat <- shiny::renderPlot({
                if (is.null(input$m_cat) || is.null(input$stretch)) return(NULL)
                plot_brewer_pals(type="qual", m = input$m_cat, stretch = input$stretch, print.hex = input$hex, col.blind = input$col_blind)
            })
            output$plot_div <- shiny::renderPlot({
                if (is.null(input$m_div) || is.null(input$contrast_div)) return(NULL)
                plot_brewer_pals(type="div", m = input$m_div, contrast = input$contrast_div, print.hex = input$hex, col.blind = input$col_blind)
            })



            output$code_seq <- shiny::renderUI({
                text <- get_palette_code(type="seq", m=input$m_seq, contrast=input$contrast_seq, auto=input$auto_seq, tmap=(input$direct_tmap == "tmap layer function code"))

                shiny::div(
                    style = "font-family: Lucida Console,Lucida Sans Typewriter,monaco,Bitstream Vera Sans Mono,monospace;text-align:right;",
                    shiny::p(text)
                )
            })

            output$code_div <- shiny::renderUI({
                text <- get_palette_code(type="div", m=input$m_div, contrast=input$contrast_div, auto=input$auto_div, tmap=(input$direct_tmap == "tmap layer function code"))
                shiny::div(
                    style = "font-family: Lucida Console,Lucida Sans Typewriter,monaco,Bitstream Vera Sans Mono,monospace;text-align:right;",
                    shiny::p(text)
                )
            })

            output$code_cat <- shiny::renderUI({
                text <- get_palette_code(type="cat", m=input$m_cat, auto=input$stretch, tmap= (input$direct_tmap == "tmap layer function code"))
                shiny::div(
                    style = "font-family: Lucida Console,Lucida Sans Typewriter,monaco,Bitstream Vera Sans Mono,monospace;text-align:right;",
                    shiny::p(text)
                )
            })



        }
    )
}

get_palette_code <- function(type, m, contrast=NULL, auto=FALSE, tmap=FALSE) {
    header <- ifelse(tmap, "tm_polygons(..., palette = ", "get_brewer_pal(")
    pal <- ifelse(type=="seq", "\"Blues\"", ifelse(type=="cat", "\"Accent\"", "\"BrBG\""))
    mtext <- paste(", n = ", m, sep="")
    cntr <- ifelse(auto || is.null(contrast), "", paste(", contrast = c(", contrast[1], ", ", contrast[2], ")", sep = ""))
    apm <- ifelse(type!="cat" || auto, "", ifelse(tmap, ", auto.palette.mapping = FALSE", ", stretch = FALSE"))
    tailer <- ifelse(tmap, ", ...)", ")")

    paste(header, pal, mtext, cntr, apm, tailer, sep="")
}

