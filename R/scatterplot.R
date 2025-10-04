#' A scatterplot
#'
#' @param data The data to plot
#' @param x The column to use for the x axis
#' @param y The column to use for the y axis
#' @param colour_by The column to colour the points by (optional)
#' @param label_x A label for the x axis
#' @param label_y A label for the y axis
#' @param title A title for the plot
#' @param fill_col The colour of the points
#' @param line_col The colour of the outline of the points
#' @param point_shape The shape of the points
#'
#' @returns A plot
#' @export
#'
#' @examples
#' scatterplot(mpg, x = displ, y = hwy)
#' scatterplot(mpg, x = displ, y = hwy, colour_by = class)
#' scatterplot(mpg, x = displ, y = hwy, fill_col = "violet", title = "My scatterplot")
scatterplot <- function(
    data, x, y, colour_by = NULL, # data, x and y axes, colour by a variable
    label_x = NULL, label_y = NULL, title = NULL, # for labelling the plot
     fill_col = "black", line_col = "black", point_shape = 21 # for aesthetic edits
    ){

  ##### input validation #####
  # it would be nice to have this buried in a helper function but i don't know enough about quosures to make
  # it work properly passing column names into the top-level function. oh well.
  # data should be a data frame
  assert_that(is.data.frame(data), msg = "The first argument, data, must be a data frame!")

  # x, y must exist
  if (missing(x)){stop("Missing x, the column to plot on the x axis!")}
  if (missing(y)){stop("Missing y, the column to plot on the y axis!")}

  # x, y, colour_by must exist in data
  assert_that(
    deparse(substitute(x)) %in% colnames(data),
    msg = paste0("The column ", deparse(substitute(x)) ,
                 " doesn't exist in your data ", deparse(substitute(data))))
  assert_that(
    deparse(substitute(y)) %in% colnames(data),
    msg = paste0("The column ", deparse(substitute(y)) ,
                 " doesn't exist in your data ", deparse(substitute(data))))

  assert_that(
    deparse(substitute(colour_by)) %in% colnames(data) | is.null(substitute(colour_by)),
    msg = paste0("The column ", deparse(substitute(colour_by)) ,
                 " doesn't exist in your data ", deparse(substitute(data))))

  # title, label_x and label_y should be in quotes
  assert_that(
    is.character(label_x) | is.null(label_x),
    is.character(label_y) | is.null(label_y),
    is.character(title) | is.null(title),
    msg = "label_x, label_y and title should all be enclosed within quotation marks '' or \"\" "
  )

  # fill/line colours should be in quotes
  assert_that(
    is.character(fill_col), is.character(line_col),
    msg = "Colour names or hex codes need to be in quotation marks '' or \"\" "
  )

  assert_that(
    is.number(point_shape), point_shape >= 0, point_shape <= 25,
    msg = "point_shape should be a number from 0-25"
  )

  ##### actual function code #####
  # waivers for x/y labels so we can have default labels if no custom ones provided
  if (is.null(label_x)){xlabel <- waiver()} else{xlabel <- label_x}
  if (is.null(label_y)){ylabel <- waiver()} else{ylabel <- label_y}

  # the {{}} are needed so you can pass in unquoted column names like for regular ggplots
  plot <- ggplot(data, aes(x = {{x}}, y = {{y}})) +
    labs(title = title, x = xlabel, y = ylabel) +
    theme_bw(base_size = 12)

  # only add fill aesthetic if required
  if (is.null(substitute(colour_by))){
    plot <- plot + geom_point(shape = point_shape, colour = line_col, fill = fill_col, size = 2)
  } else {
    plot <- plot + geom_point(aes(fill = {{colour_by}}, colour = {{colour_by}}), shape = point_shape, size = 2)
  }

  # finally, print the plot
  plot
  return(plot)
}
