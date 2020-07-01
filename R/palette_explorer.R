tmap.pal.info <- local({
    br <- RColorBrewer::brewer.pal.info[, 1:2]
    br$origin <- factor("brewer", levels = c("brewer", "viridis"))

    vr <- data.frame(maxcolors = rep.int(0, 5), category = factor("seq", levels = c("div", "qual", "seq")), origin = factor("viridis", levels = c("brewer", "viridis")), row.names = c("viridis", "magma", "plasma", "inferno", "cividis"))

    rbind(br, vr)
})


get_default_contrast <- function(type, m) {
    if (type=="seq") {
        default_contrast_seq(m)
    } else {
        default_contrast_div(m)
    }
}


plot_tmap_pals <- function(type, origin, m, contrast=NULL, stretch=NULL, cex =.9, cex_line = .8, print.hex = FALSE, col.blind="normal") {
    pal_info <- tmap.pal.info

    pal_nm <- row.names(pal_info)[pal_info$category==type & pal_info$origin == origin]
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
    lH <- convertHeight(unit(1, "lines"), "in", valueOnly=TRUE) * cex_line
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

        if (origin == "brewer") {
            if (type=="qual") {
                pal <- get_brewer_pal(pal_nm[i], n=m, stretch=stretch, plot=FALSE)
                ids <- which(pal==pal[1])[-1]
            } else {
                pal <- get_brewer_pal(pal_nm[i], n=m, contrast=contrast, plot=FALSE)
                ids <- numeric(0)
            }
        } else {
            pal <- viridis(m, option = pal_nm[i], begin = contrast[1], end = contrast[2])
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
#' \code{palette_explorer()} starts an interactive tool shows all Color Brewer and viridis palettes, where the number of colors can be adjusted as well as the constrast range. Categorical (qualitative) palettes can be stretched when the number of colors exceeds the number of palette colors. Output code needed to get the desired color values is generated. Finally, all colors can be tested for color blindness. The data.frame \code{tmap.pal.info} is similar to \code{\link[RColorBrewer:ColorBrewer]{brewer.pal.info}}, but extended with the color palettes from viridis.
#' @export
#' @importFrom grDevices col2rgb
#' @importFrom dichromat dichromat
#' @importFrom viridisLite viridis
#' @name palette_explorer
#' @rdname palette_explorer
#' @example ./examples/palette_explorer.R
#' @seealso \code{\link{get_brewer_pal}}, \code{\link[dichromat:dichromat]{dichromat}}, \code{\link[RColorBrewer:ColorBrewer]{RColorBrewer}}
#' @references \url{http://www.color-blindness.com/types-of-color-blindness/}
palette_explorer <- function() {
    if (!requireNamespace("shiny")) stop("shiny package needed for this function to work. Please install it.", call. = FALSE)
    if (!requireNamespace("shinyjs")) stop("shinyjs package needed for this function to work. Please install it.", call. = FALSE)

    shiny::shinyApp(ui = shiny::fluidPage(
            shinyjs::useShinyjs(),
            shiny::div(
                style = "font-size:75%;line-height:20px",
                shiny::fluidRow(
                    shiny::column(3,
                                  shiny::br(),
                                  shiny::br(),
                                  shiny::br(),
                                  shiny::h4("Brewer"),
                                  shiny::sliderInput("m_seq", "Number of colors",
                                                     min = 3, max = 20, value = 7)),
                    shiny::column(3,
                                  shiny::br(),
                                  shiny::br(),
                                  shiny::br(),
                                  shiny::h4("Sequential"),
                                  shiny::strong("Contrast range"),
                                  shiny::checkboxInput("auto_seq", label = "Automatic", value = TRUE),
                                  shiny::uiOutput("contrast_seq_slider")),
                    shiny::column(6,
                                  shiny::plotOutput("plot_seq", height = "285px"),
                                  shiny::uiOutput("code_seq"))
                ),
                shiny::fluidRow(
                    shiny::column(3,
                                  shiny::h4("Brewer"),
                                  shiny::sliderInput("m_cat", "Number of colors",
                                                     min = 3, max = 20, value = 8)
                    ),
                    shiny::column(3,
                                  shiny::h4("Categorical"),
                                  shiny::checkboxInput("stretch", "Stretch", value=TRUE)
                    ),
                    shiny::column(6,
                                  shiny::plotOutput("plot_cat", height = "131px"),
                                  shiny::uiOutput("code_cat")
                    )
                ),
                shiny::fluidRow(
                    shiny::column(3,
                                  shiny::h4("Brewer"),
                                  shiny::sliderInput("m_div", "Number of colors",
                                                     min = 3, max = 20, value = 9)
                    ),
                    shiny::column(3,
                                  shiny::h4("Diverging"),
                                  shiny::strong("Contrast range"),
                                  shiny::checkboxInput("auto_div", label = "Automatic", value = TRUE),
                                  shiny::uiOutput("contrast_div_slider")
                    ),
                    shiny::column(6,
                                  shiny::plotOutput("plot_div", height = "147px"),
                                  shiny::uiOutput("code_div")
                    )
                ),
                shiny::fluidRow(
                    shiny::column(3,
                                  shiny::h4("Viridis"),
                                  shiny::sliderInput("m_vir", "Number of colors",
                                                     min = 3, max = 20, value = 20)),
                    shiny::column(3,
                                  shiny::h4("Sequential"),
                                  shiny::sliderInput("contrast_vir", "Contrast range",
                                                     min = 0, max = 1, value = c(0, 1), step = .01)),
                    shiny::column(6,
                                  shiny::h4(),
                                  shiny::plotOutput("plot_vir", height = "85px"),
                                  shiny::uiOutput("code_vir"))
                    ),
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
                plot_tmap_pals(type="seq", origin = "brewer", m = input$m_seq, contrast = input$contrast_seq, print.hex = input$hex, col.blind = input$col_blind)
            })
            output$plot_cat <- shiny::renderPlot({
                if (is.null(input$m_cat) || is.null(input$stretch)) return(NULL)
                plot_tmap_pals(type="qual", origin = "brewer", m = input$m_cat, stretch = input$stretch, print.hex = input$hex, col.blind = input$col_blind)
            })
            output$plot_div <- shiny::renderPlot({
                if (is.null(input$m_div) || is.null(input$contrast_div)) return(NULL)
                plot_tmap_pals(type="div", origin = "brewer", m = input$m_div, contrast = input$contrast_div, print.hex = input$hex, col.blind = input$col_blind)
            })

            output$plot_vir <- shiny::renderPlot({
                if (is.null(input$m_vir) || is.null(input$contrast_vir)) return(NULL)
                plot_tmap_pals(type="seq",  origin = "viridis", m = input$m_vir, contrast = input$contrast_vir, print.hex = input$hex, col.blind = input$col_blind)
            })


            output$code_seq <- shiny::renderUI({
                text <- get_palette_code(type="seq", origin = "brewer", m=input$m_seq, contrast=input$contrast_seq, auto=input$auto_seq, tmap=(input$direct_tmap == "tmap layer function code"))

                shiny::div(
                    style = "font-family: Lucida Console,Lucida Sans Typewriter,monaco,Bitstream Vera Sans Mono,monospace;text-align:right;",
                    shiny::p(text)
                )
            })

            output$code_div <- shiny::renderUI({
                text <- get_palette_code(type="div", origin = "brewer", m=input$m_div, contrast=input$contrast_div, auto=input$auto_div, tmap=(input$direct_tmap == "tmap layer function code"))
                shiny::div(
                    style = "font-family: Lucida Console,Lucida Sans Typewriter,monaco,Bitstream Vera Sans Mono,monospace;text-align:right;",
                    shiny::p(text)
                )
            })

            output$code_cat <- shiny::renderUI({
                text <- get_palette_code(type="cat", origin = "brewer", m=input$m_cat, auto=input$stretch, tmap= (input$direct_tmap == "tmap layer function code"))
                shiny::div(
                    style = "font-family: Lucida Console,Lucida Sans Typewriter,monaco,Bitstream Vera Sans Mono,monospace;text-align:right;",
                    shiny::p(text)
                )
            })

            output$code_vir <- shiny::renderUI({
                text <- get_palette_code(type="seq", origin = "viridis", m=input$m_vir, contrast=input$contrast_vir, tmap= (input$direct_tmap == "tmap layer function code"))
                shiny::div(
                    style = "font-family: Lucida Console,Lucida Sans Typewriter,monaco,Bitstream Vera Sans Mono,monospace;text-align:right;",
                    shiny::p(text)
                )
            })

        }
    )
}

#' @name tmap.pal.info
#' @rdname palette_explorer
#' @export
tmap.pal.info <- local({
    br <- RColorBrewer::brewer.pal.info
    br$origin <- factor("brewer", levels = c("brewer", "viridis"))

    vr <- data.frame(maxcolors = rep.int(Inf, 5), category = factor("seq", levels = c("div", "qual", "seq")), origin = factor("viridis", levels = c("brewer", "viridis")), colorblind = c(FALSE, FALSE, FALSE, FALSE, TRUE), row.names = c("viridis", "magma", "plasma", "inferno", "cividis"))

    rbind(br, vr)
})


get_palette_code <- function(type, origin, m, contrast=NULL, auto=FALSE, tmap=FALSE) {

    if (origin == "brewer") {
        header <- ifelse(tmap, "tm_polygons(..., palette = ", "get_brewer_pal(")
        pal <- ifelse(type=="seq", "\"Blues\"", ifelse(type=="cat", "\"Accent\"", "\"BrBG\""))
        mtext <- paste(", n = ", m, sep="")
        cntr <- ifelse(auto || is.null(contrast), "", paste(", contrast = c(", contrast[1], ", ", contrast[2], ")", sep = ""))
        apm <- ifelse(type!="cat" || auto, "", ifelse(tmap, ", stretch.palette = FALSE", ", stretch = FALSE"))
        tailer <- ifelse(tmap, ", ...)", ")")

        paste(header, pal, mtext, cntr, apm, tailer, sep="")
    } else {
        header <- ifelse(tmap, "tm_polygons(..., palette = \"viridis\"", "viridisLite::viridis(")
        mtext <- ifelse(tmap, paste(", n =", m), m)
        cntr <- ifelse(is.null(contrast) || (contrast[1] == 0 && contrast[2] == 1), "",
                ifelse(tmap,
                       paste(", contrast = c(", contrast[1], ", ", contrast[2], ")", sep = ""),
                       paste0(", begin = ", contrast[1], ", end = ", contrast[2])))
        tailer <- ifelse(tmap, ", ...)", ")")
        paste(header, mtext, cntr, tailer, sep="")
    }
}

