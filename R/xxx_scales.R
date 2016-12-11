are_breaks_diverging <- function(brks) {
    # if !divx then c-Inf, 2, 5, 10) is considered sequential
    negb <- any(brks[brks!=-Inf]<0) || (brks[1] == -Inf && brks[2]<=0)
    nb <- length(brks)
    posb <- any(brks[brks!=Inf]>0) || (brks[nb] == Inf && brks[nb-1]>=0)
    negb && posb
}



fancy_breaks <- function(vec, intervals=FALSE, interval.closure="left", fun=NULL, scientific=FALSE, text.separator="to", text.less.than="less than", text.or.more="or more", digits=NA, ...) {
    args <- list(...)
    n <- length(vec)

    if (!is.null(fun)) {
        x <- do.call(fun, list(vec))
    } else {
        ### analyse the numeric vector
        frm <- gsub(" ", "", sprintf("%20.10f", abs(vec[!is.infinite(vec)])))

        # get width before decimal point
        mag <- max(nchar(frm)-11)

        # get number of decimals (which is number of decimals in vec, which is reduced when mag is large)
        ndec <- max(10 - nchar(frm) + nchar(sub("0+$","",frm)))
        if (is.na(digits)) digits <- max(min(ndec, 4-mag), 0)

        if (!scientific) {
            if (mag>11 || (mag > 9 && all(vec - floor(vec/1e9)*1e9 < 1))) {
                vec <- vec / 1e9
                ext <- " bln"
            } else if (mag > 8 || (mag > 6 && all(vec - floor(vec/1e6)*1e6 < 1))) {
                vec <- vec / 1e6
                ext <- " mln"
            } else {
                ext <- ""
            }

            # set default values
            if (!("big.mark" %in% names(args))) args$big.mark <- ","
            if (!("format" %in% names(args))) args$format <- "f"
            if (!("preserve.width" %in% names(args))) args$preserve.width <- "none"
            x <- paste(do.call("formatC", c(list(x=vec, digits=digits), args)), ext, sep="")
        } else {
            if (!("format" %in% names(args))) args$format <- "g"
            x <- do.call("formatC", c(list(x=vec, digits=digits), args))
        }
    }

    if (intervals) {
        if (scientific) {
            if (interval.closure=="left") {
                lbls <- paste("[", x[-n], ", ", x[-1], ")", sep="")
                lbls[n-1] <- paste(substr(lbls[n-1], 1, nchar(lbls[n-1])-1), "]", sep="")
            } else {
                lbls <- paste("(", x[-n], ", ", x[-1], "]", sep="")
                lbls[1] <- paste("[", substr(lbls[1], 2, nchar(lbls[1])), sep="")
            }
        } else {
            x[vec==-Inf] <- ""
            lbls <- paste(x[-n], x[-1], sep = paste0(" ", text.separator, " "))
            if (vec[1]==-Inf) lbls[1] <- paste(text.less.than, x[2])
            if (vec[n]==Inf) lbls[n-1] <- paste(x[n-1], text.or.more)
        }
    }

    if (intervals) lbls else x
}



num2breaks <- function(x, n, style, breaks, approx=FALSE, interval.closure="left") {
    # create intervals and assign colors
    if (style=="fixed") {
        q <- list(var=x,
                  brks=breaks)
        attr(q, "style") <- "fixed"
        attr(q, "nobs") <- sum(!is.na(x))
        attr(q, "intervalClosure") <- interval.closure
        class(q) <- "classIntervals"
    } else {
        if (length(x)==1) stop("Statistical numerical variable only contains one value. Please use a constant value instead, or specify breaks", call. = FALSE)
        q <- suppressWarnings(classIntervals(x, n, style= style, intervalClosure=interval.closure))
    }

    if (approx && style != "fixed") {
        if (n >= length(unique(x)) && style=="equal") {
            # to prevent classIntervals to set style to "unique"
            q <- list(var=x, brks=seq(min(x, na.rm=TRUE), max(x, na.rm=TRUE), length.out=n))
            attr(q, "intervalClosure") <- interval.closure
            class(q) <- "classIntervals"
        } else {
            brks <- q$brks

            # to prevent ugly rounded breaks such as -.5, .5, ..., 100.5 for n=101
            qm1 <- suppressWarnings(classIntervals(x, n-1, style= style, intervalClosure=interval.closure))
            brksm1 <- qm1$brks
            qp1 <- suppressWarnings(classIntervals(x, n+1, style= style, intervalClosure=interval.closure))
            brksp1 <- qp1$brks
            if (min(brksm1) > min(brks) && max(brksm1) < max(brks)) {
                q <- qm1
            } else if (min(brksp1) > min(brks) && max(brksp1) < max(brks)) {
                q <- qp1
            }
        }
    }
    q
}

num2pal <- function(x, n = 5,
                    style = "pretty",
                    breaks = NULL,
                    interval.closure="left",
                    palette = NULL,
                    auto.palette.mapping = TRUE,
                    contrast = 1,
                    legend.labels = NULL,
                    colorNA = "#FF1414",
                    legend.NA.text = "Missing",
                    showNA=NA,
                    process.colors=NULL,
                    legend.format=list(scientific=FALSE)) {
    breaks.specified <- !is.null(breaks)
    is.cont <- (style=="cont" || style=="order")
    if (is.cont) {
        style <- ifelse(style=="order", "quantile", "equal")
        if (is.null(legend.labels)) {
            ncont <- n
        } else {
            ncont <- length(legend.labels)
        }
        q <- num2breaks(x=x, n=101, style=style, breaks=breaks, approx=TRUE, interval.closure=interval.closure)
        n <- length(q$brks) - 1
    } else {
        q <- num2breaks(x=x, n=n, style=style, breaks=breaks, interval.closure=interval.closure)
    }

    breaks <- q$brks
    nbrks <- length(breaks)
    int.closure <- attr(q, "intervalClosure")

    # reverse palette
    if (length(palette)==1 && substr(palette[1], 1, 1)=="-") {
        revPal <- function(p)rev(p)
        palette <- substr(palette, 2, nchar(palette))
    } else revPal <- function(p)p


    # map palette
    is.brewer <- palette[1] %in% rownames(brewer.pal.info)

    if (is.brewer) {
        mc <- brewer.pal.info[palette, "maxcolors"]
        pal.div <- (brewer.pal.info[palette, "category"]=="div")
    } else {
        palette.type <- palette_type(palette)

        if (auto.palette.mapping && palette.type=="cat") {
            warning("could not determine whether palette is sequential or diverging. auto.palette.mapping will be set to FALSE.", call. = FALSE)
            auto.palette.mapping <- FALSE
        }
        pal.div <- palette.type=="div"


        colpal_light <- get_light(palette[c(1, length(palette)/2, length(palette))])
        # figure out whether palette is diverging
        pal.div <- ((colpal_light[2]>colpal_light[1] && colpal_light[2]>colpal_light[3]) || (colpal_light[2]<colpal_light[1] && colpal_light[2]<colpal_light[3]))
    }

    if (auto.palette.mapping) {
        if (is.brewer) {
            colpal <- colorRampPalette(revPal(brewer.pal(mc, palette)), space="rgb")(101)
        } else {
            colpal <- colorRampPalette(revPal(palette), space="rgb")(101)
        }

        ids <- if (pal.div) {
            if (is.na(contrast[1])) contrast <- if (is.brewer) default_contrast_div(n) else c(0, 1)
            map2divscaleID(breaks, n=101, contrast=contrast)
        } else {
            if (is.na(contrast[1])) contrast <- if (is.brewer) default_contrast_seq(n) else c(0, 1)
            map2seqscaleID(breaks, n=101, contrast=contrast, breaks.specified=breaks.specified)
        }

        legend.palette <- colpal[ids]
        if (any(ids<51) && any(ids>51)) {
            ids.neutral <- min(ids[ids>=51]-51) + 51
            legend.neutral.col <- colpal[ids.neutral]
        } else {
            legend.neutral.col <- colpal[ids[round(((length(ids)-1)/2)+1)]]
        }

    } else {
        if (is.brewer) {
            if (nbrks-1 > mc) {
                legend.palette <- colorRampPalette(revPal(brewer.pal(mc, palette)), space="rgb")(nbrks-1)
            } else legend.palette <- revPal(brewer.pal(nbrks-1, palette))
        } else {
            legend.palette <- colorRampPalette(revPal(palette), space="rgb")(nbrks-1) #rep(palette, length.out=nbrks-1)
        }
        neutralID <- if (pal.div) round(((length(legend.palette)-1)/2)+1) else 1
        legend.neutral.col <- legend.palette[neutralID]
    }

    legend.palette <- do.call("process_color", c(list(col=legend.palette), process.colors))
    legend.neutral.col <- do.call("process_color", c(list(col=legend.neutral.col), process.colors))
    colorNA <- do.call("process_color", c(list(col=colorNA), process.colors))

    # 	if (!is.null(process.colors)) {
    # 		legend.palette <- process.colors(legend.palette)
    # 		legend.neutral.col <- process.colors(legend.neutral.col)
    # 		colorNA <- process.colors(colorNA)
    # 	}
    #
    # 	legend.palette <- get_alpha_col(legend.palette, alpha)
    # 	legend.neutral.col <- get_alpha_col(legend.neutral.col, alpha)
    # 	colorNA <- get_alpha_col(colorNA, alpha)


    ids <- findCols(q)
    cols <- legend.palette[ids]
    anyNA <- any(is.na(cols))
    breaks.palette <- legend.palette
    if (anyNA) {
        if (is.na(showNA)) showNA <- TRUE
        cols[is.na(cols)] <- colorNA
    } else {
        if (is.na(showNA)) showNA <- FALSE
    }

    if (showNA && !is.cont) legend.palette <- c(legend.palette, colorNA)


    if (is.cont) {
        # recreate legend palette for continuous cases
        if (style=="quantile") {
            id <- seq(1, n+1, length.out=ncont)
            b <- breaks[id]
            nbrks_cont <- length(b)
        } else {
            b <- pretty(breaks, n=ncont)
            b <- b[b>=breaks[1] & b<=breaks[length(breaks)]]
            nbrks_cont <- length(b)
            id <- as.integer(cut(b, breaks=breaks, include.lowest = TRUE))
        }

        id_step <- id[2] - id[1]
        id_lst <- lapply(id, function(i){
            res <- round(seq(i-floor(id_step/2), i+ceiling(id_step/2), length.out=11))[1:10]
            res[res<1 | res>101] <- NA
            res
        })
        legend.palette <- lapply(id_lst, function(i) legend.palette[i])
        if (showNA) legend.palette <- c(legend.palette, colorNA)

        # temporarily stack gradient colors
        legend.palette <- sapply(legend.palette, paste, collapse="-")

        # create legend labels for continuous cases
        if (is.null(legend.labels)) {
            legend.labels <- do.call("fancy_breaks", c(list(vec=b, intervals=FALSE, interval.closure=int.closure), legend.format))
        } else {
            legend.labels <- rep(legend.labels, length.out=nbrks_cont)
        }
        if (showNA) {
            legend.labels <- c(legend.labels, legend.NA.text)
        }
        attr(legend.palette, "style") <- style
    } else {
        # create legend labels for discrete cases
        if (is.null(legend.labels)) {
            legend.labels <- do.call("fancy_breaks", c(list(vec=breaks, intervals=TRUE, interval.closure=int.closure), legend.format))
        } else {
            if (length(legend.labels)!=nbrks-1) warning("number of legend labels should be ", nbrks-1, call. = FALSE)
            legend.labels <- rep(legend.labels, length.out=nbrks-1)
        }

        if (showNA) legend.labels <- c(legend.labels, legend.NA.text)
    }
    list(cols=cols, legend.labels=legend.labels, legend.palette=legend.palette, breaks=breaks, breaks.palette=breaks.palette, legend.neutral.col = legend.neutral.col)
}


# Map breaks to index numbers of a diverging colour scale
#
# Determines index numbers of a potential diverging colour scale given a vector of breaks.
#
# @param breaks vector of breaks
# @param n number of classes, i.e. the length of a diverging colour palette. This should preferable be an odd number, since it contains a neutral middle color.
# @param contrast value between 0 and 1 that determines how much of the \code{(1, n)} range is used. Value \code{contrast=1} means that the most extreme break value, i.e. \code{max(abs(breaks))} is maped to either 1 or n (depending on whether it is a minimum or maximum). There is no contrast at all for \code{contrast=0}, i.e. all index numbers will correspond to the middle class (which has index number \code{((n-1)/2)+1}.
# @return vector of index numbers
map2divscaleID <- function(breaks, n=101, contrast=1) {
    nbrks <- length(breaks)

    if (length(contrast)==1) {
        contrast <- c(0, contrast)
    }
    crange <- contrast[2] - contrast[1]

    lw <- breaks[1]
    hg <- breaks[nbrks]

    # omit infinity values
    if (lw==-Inf) lw <- breaks[2]
    if (hg==Inf) hg <- breaks[nbrks-1]
    mx <- max(abs(c(lw, hg)))


    is.div <- any(breaks<0) && any(breaks>0)

    cat0 <- !any(breaks==0)

    h <- ((n-1)/2)+1

    if (is.div && !cat0) {
        npos <- sum(breaks>0)
        nneg <- sum(breaks<0)
        step <- round((h-1)*crange/((max(npos, nneg)-.5)*2))
    } else {
        npos <- sum(breaks>=0) - !is.div
        nneg <- sum(breaks<=0) - !is.div
        step <- 0
    }

    pid <- h + step
    nid <- h - step

    ids <- rep(h, nbrks-1)
    if (npos>0) ids[(nbrks-npos):(nbrks-1)] <- pid +
        seq((n-pid)/mx*hg*contrast[1], (n-pid)/mx*hg*contrast[2], length.out=npos)
    if (nneg>0) ids[1:nneg] <- seq(nid-((nid-1)/mx*-lw*contrast[2]), nid-((nid-1)/mx*-lw*contrast[1]),
                                   length.out=nneg)
    if (is.div && cat0) ids[nneg] <- h
    round(ids)
}



# Map breaks to index numbers of a sequential colour scale
#
# Determines index numbers of a potential sequential colour scale given a vector of breaks.
#
# @param breaks vector of breaks
# @param n number of classes, i.e. the length of a sequential colour palette.
# @param contrast value between 0 and 1 that determines how much of the \code{(1, n)} range is used. Value \code{contrast=1} means that the most extreme break value, i.e. \code{max(abs(breaks))} is maped to n. There is no contrast at all for \code{contrast=0}, i.e. all index numbers will correspond to the first class (which has index number \code{1}.
# @param breaks.specified logical that determines whether breaks have been specified by the user. If so a warning is shown if breaks are diverging.
# @return vector of index numbers
map2seqscaleID <- function(breaks, n=101, contrast=1, breaks.specified=TRUE, impute=TRUE) {
    if (are_breaks_diverging(breaks) && breaks.specified) warning("Breaks contains positive and negative values. Better is to use diverging scale instead, or set auto.palette.mapping to FALSE.", call. = FALSE)
    m <- (n*2)-1
    mh <- ((m-1)/2)+1
    ids <- map2divscaleID(breaks, n=m, contrast=contrast)

    ids <- if (any(breaks>0)) {
        ids - mh + 1
    } else {
        (mh+1) - ids
    }

    # checks:
    if (any(ids>n)) {
        if (impute) {
            ids[ids>n] <- n
        } else {
            warning("Some index numbers exceed n and are replaced by NA", call. = FALSE)
            ids[ids>n] <- NA
        }

    } else if (any(ids<1)) {
        if (impute) {
            ids[ids<1] <- 1
        } else {
            warning("Some index numbers exceed 0 and are replaced by NA", call. = FALSE)
            ids[ids<1] <- NA
        }
    }
    round(ids)
}

# function to determine whether a diverging of sequential palette is used given the values and the breaks
use_diverging_palette <- function(v, brks) {
    x <- na.omit(v)
    divx <- any(x<0) && any(x>0)

    if (divx || is.null(brks)) {
        return(divx)
    } else {
        are_breaks_diverging(brks)
    }
}

