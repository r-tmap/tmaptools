#' Map coloring
#'
#' Color the polygons of a map such that adjacent polygons have different colors. This function returns the color indices
#'
#' As of tmaptools 3.3, the deprecated color functions \code{get_brewer_pal} and \code{palette_explorer}, have been removed. These have been replaced \code{c4a} and \code{c4a_gui} respectively from the package cols4all. Therefore, \code{map_coloring} will return color indices and will ignore the input arguments \code{palette} and \code{contrast}. See example.
#'
#' Adjacency is determined by shared boundary lines (as in "rook" contiguity), not by shared points alone. This matters for e.g. Voronoi tessellations, where three or more cells can meet at a single vertex without actually sharing an edge; treating such point-touches as adjacency can force the use of unnecessarily many colors.
#'
#' \code{ncols} is the target number of colors, with a minimum of 4 (the four-color theorem guarantees that 4 colors suffice for any planar map). All \code{ncols} of them are used, about equally often, so that no color dominates the map. Only when \code{ncols} colors are too few for a valid coloring is the number increased, in which case the user is informed via \code{\link[cli:cli_inform]{cli_inform}}. Use \code{ncols = 4} for a map that uses as few colors as possible.
#'
#' Evening out the colors is possible because a polygon may take any color that none of its neighbours has. So as long as one color is used at least two polygons more often than another, some polygon can be moved from the first to the second. A color that the coloring itself did not need is simply one that is used zero times, and is filled up the same way; this is what spreads a coloring that needs only 4 colors over all \code{ncols} of them.
#'
#' A map has many colorings, and which one comes out is decided by choices between equally good options: which color a polygon is given, which polygon is moved to even out the colors, and which colors are handed to which group in the end. The argument \code{permutation} settles those choices, so that each number gives a different, equally valid map. Raise it until the map looks good.
#'
#' As of tmaptools 3.4, the argument \code{minimize} has been removed: \code{ncols} now determines the number of colors on its own. The result no longer depends on the random seed, and the random number generator of the caller is left untouched, so that repeated plots of the same map keep the same colors. To go looking for a nicer looking map, vary \code{permutation} instead of the seed.
#'
#' The \code{"dsatur"} algorithm first runs the DSatur heuristic (Brelaz, 1979), which colors the most constrained polygon at each step: the one with the largest number of distinct colors among its already colored neighbours, ties broken by the number of neighbours. Since this is a heuristic, it may fail even when a valid coloring exists; on maps of contiguous regions it typically needs one color more than necessary. It is therefore followed by a tabu search (Hertz and de Werra, 1987), which starts from a coloring that uses \code{ncols} colors but may have neighbours sharing one, and then repeatedly recolors a polygon involved in such a clash until none are left. Two reductions keep this cheap: polygons with fewer than \code{ncols} neighbours are set aside and colored last (a free color is then always available), and the rest are split into groups that are not connected to each other and searched separately. Use \code{algorithm = "greedy"} to skip the tabu search.
#'
#' @param x Either a shape (i.e. a \code{\link[sf:sf]{sf}} or \code{SpatialPolygons(DataFrame)} (\code{sp} package) object), or an adjacency list.
#' @param algorithm either \code{"dsatur"} (default), the DSatur heuristic backed by a tabu search, or \code{"greedy"}, the heuristic on its own, which is faster but may use more colors than needed. See details.
#' @param ncols target number of colors, all of which are used about equally often. By default 8. Raised to 4 if set lower, and raised further, with a message, if more colors are needed for a valid coloring.
#' @param permutation number that selects one of the many colorings a map has. By default 0. Any other whole number gives another one, so trying 1, 2, 3, ... is a way to look for a map that pleases. It changes both which polygons share a color and which color each group is given, and every permutation is equally valid and equally evenly spread.
#' @param ... to catch deprecated arguments \code{palette}, \code{contrast} and \code{minimize}. See details.
#' @return A vector of color indices.
#' @references Brelaz, D. (1979) New methods to color the vertices of a graph. Communications of the ACM 22(4), 251-256.
#' @references Hertz, A. and de Werra, D. (1987) Using tabu search techniques for graph coloring. Computing 39(4), 345-351.
#' @example ./examples/map_coloring.R
#' @export
map_coloring <- function(x, algorithm = "dsatur", ncols = 8, permutation = 0, ...) {
	if (inherits(x, "Spatial")) x <- as(x, "sf")
	if (inherits(x, "sf")) {
		# get adjacency list
		adj <- get_neighbours(x)
	} else if (is.list(x)) {
		adj <- lapply(x, as.integer)
	} else stop("Unknown x argument")

	args <- list(...)
	if (any(c("palette", "contrast") %in% names(args))) warning("palette and contrast are not used anymore as of tmaptools 3.3. Please use cols4all: see manual")
	if ("minimize" %in% names(args)) warning("minimize is not used anymore as of tmaptools 3.4. The number of colors is now set with ncols alone: see manual")

	if (!(algorithm %in% c("dsatur", "greedy"))) stop("Unknown algorithm")

	n <- length(adj)
	if (!n) return(integer(0))

	ncols_pref <- max(as.integer(ncols), 4L)
	maxiter <- max(2000L, 100L * n)

	gen <- lcg(permutation)

	solve_k <- function(k) {
		cols <- dsatur_heuristic(adj, k, gen)
		if (is.null(cols) && algorithm == "dsatur") cols <- dsatur_repair(adj, k, maxiter, gen)
		cols
	}

	# the heuristic never fails when k equals the number of polygons, so this
	# is guaranteed to terminate
	k <- ncols_pref
	cols <- solve_k(k)
	while (is.null(cols) && k < n) {
		k <- k + 1L
		cols <- solve_k(k)
	}

	if (k > ncols_pref) {
		cli::cli_inform("Could not color the map with {ncols_pref} color{?s}; used {k} colors instead.")
	}

	cols <- balance_colors(adj, cols, k, gen)
	permute_colors(cols, gen)
}

# A small generator of its own, used to settle the choices the steps below have
# between equally good options: which color a polygon gets, which polygon is
# moved to even out the colors, and which of the equally good tabu moves is
# taken. It is seeded with the permutation number and kept apart from the random
# number generator of the caller, so that a permutation number always gives the
# same map while consecutive numbers give visibly different ones. Returns a
# function that draws a whole number from 1 to n.
lcg <- function(p) {
	state <- as.numeric(p) %% 2^32
	step <- function() state <<- (1664525 * state + 1013904223) %% 2^32
	for (i in seq_len(8L)) step()
	function(n) {
		step()
		# the low bits of such a generator cycle quickly, so draw from the high ones
		floor(state / 2^32 * n) + 1L
	}
}

# Relabel the colors, so that the same grouping of polygons can be handed out
# over the palette in different ways. Colors that stayed unused, which happens
# when there are fewer polygons than colors, are relabeled away, so that the
# highest index is always the number of colors that the map actually uses.
permute_colors <- function(cols, gen) {
	used <- sort(unique(cols))
	cols <- match(cols, used)
	k <- length(used)
	if (k < 2L) return(cols)

	perm <- seq_len(k)
	for (i in seq.int(k, 2L)) {
		j <- gen(i)
		z <- perm[i]
		perm[i] <- perm[j]
		perm[j] <- z
	}
	perm[cols]
}

# Even out the color classes. A polygon may take a color that none of its
# neighbours has, so as long as the largest class is at least two bigger than
# the smallest, move a polygon over. Colors that the coloring left unused are
# classes of size 0, and so fill up first: this is also what spreads a coloring
# that needs only a few colors over the full ncols range. Every move lowers the
# spread, so this terminates.
balance_colors <- function(adj, cols, k, gen) {
	cnt <- vapply(adj, function(z) tabulate(cols[z], k), integer(k))
	sizes <- tabulate(cols, k)

	repeat {
		moved <- FALSE
		for (a in order(sizes, decreasing = TRUE)) {
			for (b in order(sizes)) {
				if (sizes[a] - sizes[b] < 2L) break
				cand <- which(cols == a & cnt[b, ] == 0L)
				if (!length(cand)) next
				v <- cand[gen(length(cand))]

				cols[v] <- b
				sizes[a] <- sizes[a] - 1L
				sizes[b] <- sizes[b] + 1L
				z <- adj[[v]]
				cnt[a, z] <- cnt[a, z] - 1L
				cnt[b, z] <- cnt[b, z] + 1L
				moved <- TRUE
				break
			}
			if (moved) break
		}
		if (!moved) break
	}
	cols
}

# DSatur: color the polygon with the largest number of distinct colors among
# its already colored neighbours, ties broken by number of neighbours. Returns
# NULL if k colors did not suffice.
dsatur_heuristic <- function(adj, k, gen) {
	n <- length(adj)
	deg <- lengths(adj)
	cols <- integer(n)

	# saturation is tracked incrementally; seen[c, v] tells whether color c
	# occurs among the neighbours of v
	seen <- matrix(FALSE, nrow = k, ncol = n)
	sat <- integer(n)
	todo <- rep(TRUE, n)

	# rank on saturation first, degree second
	wsat <- max(deg) + 1

	for (i in seq_len(n)) {
		cand <- which(todo)
		v <- cand[which.max(sat[cand] * wsat + deg[cand])]

		free <- which(!seen[, v])
		if (!length(free)) return(NULL)
		z <- free[gen(length(free))]

		cols[v] <- z
		todo[v] <- FALSE
		nb <- adj[[v]]
		nb <- nb[!seen[z, nb]]
		seen[z, nb] <- TRUE
		sat[nb] <- sat[nb] + 1L
	}
	cols
}

# Second attempt when the heuristic fails. Polygons with fewer than k
# neighbours are peeled off and colored last: at that point they have fewer
# than k colored neighbours, so a free color is guaranteed. What remains is
# split into connected groups, each repaired by tabu search. Returns NULL if
# no k-coloring was found.
dsatur_repair <- function(adj, k, maxiter, gen) {
	n <- length(adj)

	alive <- rep(TRUE, n)
	deg <- lengths(adj)
	peeled <- integer(n)
	np <- 0L
	repeat {
		cand <- which(alive & deg < k)
		if (!length(cand)) break
		for (v in cand) {
			alive[v] <- FALSE
			np <- np + 1L
			peeled[np] <- v
			nb <- adj[[v]]
			nb <- nb[alive[nb]]
			deg[nb] <- deg[nb] - 1L
		}
	}

	cols <- integer(n)
	for (grp in connected_groups(adj, which(alive), alive)) {
		loc <- integer(n)
		loc[grp] <- seq_along(grp)
		nb <- lapply(grp, function(v) {
			z <- loc[adj[[v]]]
			z[z != 0L]
		})
		res <- tabu_search(nb, k, maxiter, gen)
		if (is.null(res)) return(NULL)
		cols[grp] <- res
	}

	for (i in rev(seq_len(np))) {
		v <- peeled[i]
		z <- cols[adj[[v]]]
		cols[v] <- match(FALSE, seq_len(k) %in% z[z != 0L])
	}
	cols
}

connected_groups <- function(adj, vs, alive) {
	seen <- logical(length(adj))
	out <- list()
	for (s in vs) {
		if (seen[s]) next
		seen[s] <- TRUE
		grp <- integer(0)
		stack <- s
		while (length(stack)) {
			v <- stack[length(stack)]
			stack <- stack[-length(stack)]
			grp <- c(grp, v)
			nb <- adj[[v]]
			nb <- nb[alive[nb] & !seen[nb]]
			seen[nb] <- TRUE
			stack <- c(stack, nb)
		}
		out[[length(out) + 1L]] <- grp
	}
	out
}

# Tabu search (Hertz & de Werra, 1987). Start from a coloring that uses k
# colors but may have neighbours sharing one, then repeatedly recolor a polygon
# involved in such a clash, picking the move that removes the most clashes.
# Recently undone (polygon, color) pairs are tabu for a while, which is what
# lets the search climb out of local optima. Returns NULL if no clash-free
# coloring was reached within maxiter moves.
tabu_search <- function(nb, k, maxiter, gen) {
	m <- length(nb)
	deg <- lengths(nb)
	cols <- integer(m)

	# start from a DSatur ordering, taking the least clashing color
	sat <- integer(m)
	todo <- rep(TRUE, m)
	wsat <- max(deg) + 1
	for (i in seq_len(m)) {
		cand <- which(todo)
		v <- cand[which.max(sat[cand] * wsat + deg[cand])]
		cols[v] <- which.min(tabulate(cols[nb[[v]]], k))
		todo[v] <- FALSE
		sat[nb[[v]]] <- sat[nb[[v]]] + 1L
	}

	# cnt[c, v]: number of neighbours of v that have color c
	cnt <- vapply(nb, function(z) tabulate(cols[z], k), integer(k))
	nclash <- sum(cnt[cbind(cols, seq_len(m))]) / 2L
	if (!nclash) return(cols)

	tabu <- matrix(0L, k, m)
	best <- nclash

	for (iter in seq_len(maxiter)) {
		cur <- cnt[cbind(cols, seq_len(m))]
		vc <- which(cur > 0L)

		# change in number of clashes for every (color, clashing polygon) move
		d <- cnt[, vc, drop = FALSE] - rep(cur[vc], each = k)
		# a tabu move is allowed only if it beats the best coloring so far
		d[tabu[, vc, drop = FALSE] >= iter & (nclash + d) >= best] <- NA_integer_
		d[cbind(cols[vc], seq_along(vc))] <- NA_integer_
		if (all(is.na(d))) next

		w <- which(d == min(d, na.rm = TRUE))
		w <- w[gen(length(w))]
		z <- ((w - 1L) %% k) + 1L
		v <- vc[((w - 1L) %/% k) + 1L]

		old <- cols[v]
		nclash <- nclash + d[w]
		cols[v] <- z
		cnt[old, nb[[v]]] <- cnt[old, nb[[v]]] - 1L
		cnt[z, nb[[v]]] <- cnt[z, nb[[v]]] + 1L
		if (!nclash) return(cols)

		# tabu search needs some jitter in the tenure to avoid running in circles
		tabu[old, v] <- iter + as.integer(0.6 * length(vc)) + gen(10L)
		best <- min(best, nclash)
	}
	NULL
}

#' Get neighbours list from spatial objects
#'
#' Get neighbours list from spatial objects. The output is similar to the function \code{poly2nb} of the \code{spdep} package with \code{queen = FALSE}, but uses \code{sf} instead of \code{sp}. Two polygons are considered neighbours only when their boundaries share a line segment (an edge), not merely a point.
#'
#' @param x a shape object, i.e., a \code{\link[sf:sf]{sf}} object or a \code{SpatialPolygons(DataFrame)} (\code{sp} package).
#' @return A list where the items correspond to the features. Each item is a vector of neighbours.
#' @export
get_neighbours <- function(x) {
	if (inherits(x, "Spatial")) x <- as(x, "sf")

	# "F***1****": the boundaries intersect in something of dimension 1 (a
	# shared line), not merely dimension 0 (a shared point). This avoids
	# treating polygons that only touch at a corner (e.g. adjacent Voronoi
	# cells) as neighbours.
	# Whether two boundaries share a line segment does not depend on the
	# coordinates being read as planar or spherical, so the lon/lat note that
	# st_relate emits does not apply here.
	y <- withCallingHandlers(
		sf::st_relate(x, x, pattern = "F***1****"),
		message = function(m) {
			if (grepl("longitude/latitude", conditionMessage(m), fixed = TRUE)) invokeRestart("muffleMessage")
		}
	)
	n <- length(y)

	mapply(function(yi, i) {
		as.integer(setdiff(yi, i))
	}, y, 1L:n, SIMPLIFY = FALSE)
}
