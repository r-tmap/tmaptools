#' Get and plot a (modified) Color Brewer palette
#'
#' Get and plot a (modified) palette from Color Brewer. In addition to the base function \code{\link[RColorBrewer:brewer.pal]{brewer.pal}}, a palette can be created for any number of classes. The contrast of the palette can be adjusted for sequential and diverging palettes. For categorical palettes, intermediate colors can be generated. An interactive tool that uses this function is \code{\link{palette_explorer}}.
#'
#' The default contrast of the palette depends on the number of colors, \code{n}, in the following way. The default contrast is maximal, so \code{(0, 1)}, when \code{n = 9} for sequential palettes and \code{n = 11} for diverging palettes. The default contrast values for smaller values of \code{n} can be extracted with some R magic: \code{sapply(1:9, tmaptools:::default_contrast_seq)} for sequential palettes and \code{sapply(1:11, tmaptools:::default_contrast_div)} for diverging palettes.
#'
#' @param palette name of the color brewer palette. Run \code{\link{palette_explorer}} (or \code{\link[RColorBrewer:display.brewer.pal]{display.brewer.pal}}) for options.
#' @param n number of colors
#' @param contrast a vector of two numbers between 0 and 1 that defines the contrast range of the palette. Applicable to sequential and diverging palettes. For sequential palettes, 0 stands for the leftmost color and 1 the rightmost color. For instance, when \code{contrast=c(.25, .75)}, then the palette ranges from 1/4 to 3/4 of the available color range. For diverging palettes, 0 stands for the middle color and 1 for both outer colors. If only one number is provided, the other number is set to 0. The default value depends on \code{n}. See details.
#' @param stretch logical that determines whether intermediate colors are used for a cateogorical palette when \code{n} is greater than the number of available colors.
#' @param plot should the palette be plot, or only returned? If \code{TRUE} the palette is silently returned.
#' @return vector of color values. It is silently returned when \code{plot=TRUE}.
#' @example ./examples/get_brewer_pal.R
#' @seealso \code{\link{palette_explorer}}
#' @import grid
#' @export
get_brewer_pal <- function(palette, n=5, contrast=NA, stretch=TRUE, plot=TRUE) {
    call <- names(match.call(expand.dots = TRUE)[-1])

    reverse <- (substr(palette, 1, 1) == "-")

    if (reverse) palette <- substr(palette, 2, nchar(palette))

	nmax <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
	if (RColorBrewer::brewer.pal.info[palette, "category"]=="qual") {
	    if ("contrast" %in% call) warning("contrast not used in qualitative color palettes")
		brewerpal <- RColorBrewer::brewer.pal(min(nmax, max(n, 3)), name=palette)
		if (stretch && n > length(brewerpal)) {
			p <- colorRampPalette(brewerpal)(n)
		} else {
			p <- rep(brewerpal, length.out=n)
		}
	} else if (RColorBrewer::brewer.pal.info[palette, "category"]=="seq") {
	    if ("stretch" %in% call) warning("stretch not used in sequential color palettes")
		if (is.na(contrast[1])) contrast <- default_contrast_seq(n)
		if (length(contrast)==1) contrast <- c(0, contrast)
		brewerpal <- RColorBrewer::brewer.pal(nmax, name=palette)
		contrastIDs <- round(seq(contrast[1]*100, contrast[2]*100, length.out=n))+1
		p <- colorRampPalette(brewerpal)(101)[contrastIDs]
	} else {
	    if ("stretch" %in% call) warning("stretch not used in diverging color palettes")
		if (is.na(contrast[1])) contrast <- default_contrast_div(n)
		if (length(contrast)==1) contrast <- c(0, contrast)
		brewerpal <- RColorBrewer::brewer.pal(nmax, name=palette)
		contrastIDs <- map2divscaleID(breaks=seq(-10,10, length.out=n+1), contrast=contrast)
		p <- colorRampPalette(brewerpal)(101)[contrastIDs]
	}

	if (reverse) p <- rev(p)

	if (plot) {
	    grid.newpage()
	    fontsize <- min(1, (.8/n) / convertWidth(stringWidth("#ABCDEF"), unitTo = "npc", valueOnly = TRUE))
	    pushViewport(viewport(layout = grid.layout(nrow=4, ncol=n+2, widths = unit(c(.1, rep(1, n), .1), c("npc", rep("null", n), "npc")),
	                                               heights = unit(c(1, 2, 1.5*fontsize, 1), c("null", "lines", "lines", "null")))))
	    lapply(1L:n, function(i) {
	        pushViewport(viewport(layout.pos.row = 2, layout.pos.col = i+1))
	        grid.rect(gp=gpar(fill=p[i]))
	        popViewport()
	        pushViewport(viewport(layout.pos.row = 3, layout.pos.col = i+1))
	        grid.text(p[i], gp=gpar(cex=fontsize))
	        popViewport()
	    })
	    popViewport()
	    invisible(p)
	} else {
	    p
	}
}
