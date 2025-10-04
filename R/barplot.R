#' A barplot
#'
#' @param data The data to plot
#' @param x The column to use for the x axis
#' @param colour_by The column to colour the bars by (optional)
#' @param label_x A label for the x axis
#' @param title A title for the plot
#' @param fill_col The colour of the bars
#' @param line_col The colour of the outline of the bars
#' @param horizontal Whether the barplot should be horizontal (TRUE or FALSE)
#'
#' @returns A plot
#' @export
#'
#' @examples
#' barplot(mpg, x = class)
#' barplot(mpg, x = manufacturer, colour_by = class)
#' barplot(mpg, x = manufacturer, colour_by = class, horizontal = TRUE, title = "Horizontal barplot")

barplot <- function(
    data, x, colour_by = NULL, # data, x axis, colour by a variable
    label_x = NULL, title = NULL, # for labelling the plot
    fill_col = "black", line_col = "black", horizontal = FALSE # for aesthetic edits
){

  ##### input validation #####
  # it would be nice to have this buried in a helper function but i don't know enough about quosures to make
  # it work properly passing column names into the top-level function. oh well.
  # data should be a data frame
  assert_that(is.data.frame(data), msg = "The first argument, data, must be a data frame!")

  # x, y must exist
  if (missing(x)){stop("Missing x, the column with the data you want to plot!")}

  # x, colour_by must exist in data
  assert_that(
    deparse(substitute(x)) %in% colnames(data),
    msg = paste0("The column ", deparse(substitute(x)) ,
                 " doesn't exist in your data ", deparse(substitute(data))))

  assert_that(
    deparse(substitute(colour_by)) %in% colnames(data) | is.null(substitute(colour_by)),
    msg = paste0("The column ", deparse(substitute(colour_by)) ,
                 " doesn't exist in your data ", deparse(substitute(data))))

  # title, label_x and label_y should be in quotes
  assert_that(
    is.character(label_x) | is.null(label_x),
    is.character(title) | is.null(title),
    msg = "label_x, label_y and title should all be enclosed within quotation marks '' or \"\" "
  )

  # fill/line colours should be in quotes
  assert_that(
    is.character(fill_col), is.character(line_col),
    msg = "Colour names or hex codes need to be in quotation marks '' or \"\" "
  )

  # horizontal should be logical
  assert_that(
    is.logical(horizontal),
    msg = "The argument horizontal should be either TRUE or FALSE"
  )


  ##### actual function code #####
  # waivers for x/y labels so we can have default labels if no custom ones provided
  if (is.null(label_x)){xlabel <- waiver()} else{xlabel <- label_x}

  # the {{}} are needed so you can pass in unquoted column names like for regular ggplots
  plot <- ggplot(data, aes(x = {{x}})) +
    # add labels and theme
    labs(title = title, x = xlabel) + theme_bw(base_size = 12)

  # only add fill aesthetic if required
  if (is.null(substitute(colour_by))){
    plot <- plot + geom_bar(colour = line_col, fill = fill_col)
  } else {
    plot <- plot + geom_bar(aes(fill = {{colour_by}}), colour = line_col)
  }

  if (horizontal) {
    plot <- plot + coord_flip() + theme(legend.position = "bottom")
  }

  # finally, print the plot
  plot
  return(plot)
}
