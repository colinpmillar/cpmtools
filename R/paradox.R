#' Paradox
#'
#' Functions to make pleasing doodles
#'
#' @param shape a matrix with colums x and y definining a shape to doodle within,
#'          can be any closed polygon, but some shapes work better than others.
#' @param shapes a list of shapes (polygons) to doodle within
#' @param dist the distance up the adjacent edge where each new line will be drawn, default 0.05
#' @param reverse should the shape be drawn in reverse order (see examples)
#' @param cols a palate of colours as a vector of strings interpretable as colours
#' @param debug should debug messages be produces (default FALSE)
#' @param scale if scaling, how much to scale by
#' @param shift if scaling, should the shape also be shifted and by how much
#'
#' @return displays a plot
#'
#' @examples
#'
#' if (interactive()) {
#'
#'   triangle <- cbind(x = c(0, 1, .7), y = c(0, 0.1, 1))
#'   triangle2 <- cbind(x = c(0, 0, .7), y = c(0, 1, 1))
#'
#'   square <- cbind(x = c(0, 1, 1, 0), y = c(0, 0, 1, 1))
#'
#'   canvas()
#'   paradox(square, 0.03)
#'
#'   canvas()
#'   paradox(triangle, 0.042)
#'   paradox(triangle2, 0.042)
#'
#'   canvas()
#'   paradox(scale_shape(square, c(0.5, 1)), 0.03)
#'   paradox(scale_shape(square, c(0.5, 1), c(0.5, 0)), 0.05, reverse = TRUE)
#'
#'   canvas()
#'   dist <- 0.02
#'   paradox(scale_shape(square, .5), dist)
#'   paradox(scale_shape(square, .5, c(0.5, 0)), dist, reverse = TRUE)
#'   paradox(scale_shape(square, .5, .5), dist)
#'   paradox(scale_shape(square, .5, c(0, 0.5)), dist, reverse = TRUE)
#'
#'   canvas()
#'   dist <- 0.02
#'   paradox(scale_shape(square, .5), dist, reverse = TRUE)
#'   paradox(scale_shape(square, .5, c(0.5, 0)), dist, reverse = TRUE)
#'   paradox(scale_shape(square, .5, .5), dist, reverse = TRUE)
#'   paradox(scale_shape(square, .5, c(0, 0.5)), dist, reverse = TRUE)
#'
#'   canvas()
#'   dist <- 0.02
#'
#'   cols1 <- colorRampPalette(c("red", "blue"))(30)
#'   cols2 <- colorRampPalette(c("blue", "red"))(30)
#'   cols3 <- colorRampPalette(c("green", "purple"))(30)
#'   cols4 <- colorRampPalette(c("purple", "green"))(30)
#'
#'   paradox(scale_shape(square, .5), dist,
#'     reverse = TRUE,
#'     cols = c(rbind(cols1, cols2, cols3, cols4))
#'   )
#'   paradox(scale_shape(square, .5, c(0.5, 0)), dist,
#'     reverse = FALSE,
#'     cols = c(rbind(cols3, cols4, cols1, cols2))
#'   )
#'   paradox(scale_shape(square, .5, .5), dist,
#'     reverse = FALSE,
#'     cols = c(rbind(cols1, cols2, cols3, cols4))
#'   )
#'   paradox(scale_shape(square, .5, c(0, 0.5)), dist,
#'     reverse = TRUE,
#'     cols = c(rbind(cols3, cols4, cols1, cols2))
#'   )
#' }
#'
#' @importFrom stats runif
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics polygon points
#'
#' @rdname paradox
#' @export
canvas <- function() {
  plot(0, 0,
    type = "n",
    xlim = c(0, 1), ylim = c(0, 1),
    ann = FALSE, axes = FALSE
  )
}

#' @rdname paradox
#' @export
paradox <- function(shape, dist = 0.05, reverse = FALSE, cols = colorRampPalette(c("red", "blue"))(40), debug = FALSE) {
  cols <- c(cols, rev(cols))

  if (reverse) {
    shape <- shape[nrow(shape):1, ]
  }
  counter <- 1

  polygon(shape, col = cols[1])

  while (shape_length(shape) > dist) {
    # add new line
    theta <- atan2(diff(shape[2:3, 2]), diff(shape[2:3, 1]))

    xnew <- cos(theta) * dist + shape[2, 1]
    ynew <- sin(theta) * dist + shape[2, 2]

    if (debug) {
      points(xnew, ynew, pch = ".", col = "red", cex = 2)
    }

    verts <- c(3:nrow(shape), 1)

    shape <- cbind(
      x = c(xnew, shape[verts, 1]),
      y = c(ynew, shape[verts, 2])
    )

    counter <- counter + 1
    Sys.sleep(.01)
    polygon(shape, col = cols[(counter - 1) %% length(cols) + 1])
  }
}

#' @rdname paradox
#' @export
paradoxes <- function(shapes, dist = 0.05, cols = colorRampPalette(c("red", "blue"))(40)) {
  for (i in seq_along(shapes)) {
    if (missing(cols)) {
      paradox(shapes[[i]], dist, reverse = i %% 1)
    } else {
      paradox(shapes[[i]], dist, reverse = i %% 1, cols = cols)
    }
  }
}

#' @rdname paradox
#' @export
scale_shape <- function(shape, scale = 1, shift = 0) {
  t(t(shape) * scale + shift)
}

shape_length <- function(shape) {
  sqrt(sum((shape[1, 1:2] - shape[2, 1:2])^2))
}




#   # experimental
#   library(sf)

#   set.seed(2348)

#   shapes <- gen_split(square, debug = TRUE)
#   shapes1 <- gen_split(triangle, debug = TRUE)
#   shapes2 <- gen_split(triangle2, debug = TRUE)

#   cols <- colorRampPalette(c("red", "green", "blue"))(160)
#   cols <- rainbow(160)

#   # cols = colorRampPalette(c("cyan", "purple"))(100)
#   # cols = grey(0, alpha = 0.01)
#   # cols = "#00FFFF03"
#   # cols = c("white", "black")

#   canvas()
#   paradoxes(shapes1, dist = 0.02, cols = cols)
#   paradoxes(shapes2, dist = 0.02, cols = cols)

# gen_line <- function(shape) {
#   # generate some random lines inside a shape
#   nedges <- nrow(shape)

#   p <- p0 <- runif(1, 0, nedges)
#   side <- ceiling(p)
#   dist <- runif(1, .2, .8)
#   theta <- atan2(
#     diff(shape[(side + 0:1 - 1) %% nedges + 1, 2]),
#     diff(shape[(side + 0:1 - 1) %% nedges + 1, 1])
#   )
#   x0 <- cos(theta) * dist + shape[side, 1]
#   y0 <- sin(theta) * dist + shape[side, 2]


#   p <- p1 <- runif(1, 0, nedges - 1)
#   side <- c(1:nedges)[-side][ceiling(p)]
#   dist <- runif(1, .2, .8)
#   theta <- atan2(
#     diff(shape[(side + 0:1 - 1) %% nedges + 1, 2]),
#     diff(shape[(side + 0:1 - 1) %% nedges + 1, 1])
#   )
#   x1 <- cos(theta) * dist + shape[side, 1]
#   y1 <- sin(theta) * dist + shape[side, 2]
#   m <- (y1 - y0) / (x1 - x0)
#   b <- y0 - m * x0

#   xrange <- c(min(shape[, 1]) - 1, max(shape[, 1]) + 1)
#   line <- st_linestring(cbind(xrange, xrange * m + b))

#   line
# }


# gen_split <- function(shape, debug = FALSE) {
#   line1 <- gen_line(shape)
#   line2 <- gen_line(shape)

#   box <- sf::st_polygon(list(shape[c(1:nrow(shape), 1), ]))
#   if (debug) {
#     plot(box)
#     plot(line1, add = TRUE)
#     plot(line2, add = TRUE)
#   }

#   split1 <- lwgeom::st_split(box, line1)
#   split1 <- st_collection_extract(split1, "POLYGON")

#   split21 <- lwgeom::st_split(split1[[1]], line2)
#   split22 <- lwgeom::st_split(split1[[2]], line2)

#   out <- c(
#     st_collection_extract(split21, "POLYGON"),
#     st_collection_extract(split22, "POLYGON")
#   )

#   lapply(seq_along(out), function(i) st_coordinates(out[i])[-1, 1:2])
# }
