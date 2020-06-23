#' plot_dep
#'
#' @author Greg Pilgrim \email{gpilgrim@@vergason.com}
#'
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_raster
#' @importFrom ggplot2 theme_bw
#' @importFrom ggplot2 geom_line
#' @importFrom ggplot2 geom_point
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom dplyr mutate
#'
#' @param Rate sputter power - a constant
#' @param Epsilon a sputter rate scaling factor, usually 1
#' @param Disc_1 relative angle (in degrees) of larger disc (disc 1) in chamber door vs. gravity
#' @param Disc_2 relative angle (in degrees) of smaller disc (disc 2) in chamber door vs. gravity
#' @param Rad_1 fixed value describing  the distance between the center of larger disc (disc 1) and the center of the smaller disc (disc 2)
#' @param Rad_2 fixed value describing the distance between the center of disc 2 and the position of each cathode (cathodes are symetric)
#' @param Ins_1 distance the gun has been inserted into chamber, measured from inside of disc 2 to the center of the gun knuckle
#' @param Fing_1 distance from the knuckle to the gun tip
#' @param Knuc_1 angle (in radians) at which the gun is set at the knuckle.  Min is 0, max is 1.18
#' @param Rot_1 angle (in radians) at which the gun is rotated, min is 0, max is 2*pi
#' @param Interval the size of each step in a numeric solution.  Larger intervals will produce courser solutions, but will be faster.  Smaller intervals will produce more detailed solutions but will be more computationally expensive
#' @param coord selection of polar (if platen is rotating) or cartesian (if platen is fixed) coordinate space
#'
#'
#' @export

plot_dep <- function(Rate,
                     Epsilon = 1,
                     Disc_1 = 0,
                     Disc_2 = 0,
                     Rad_1 = 1.4375,
                     Rad_2 = 3,
                     Ins_1 = 2.375,
                     Fing_1 = 4.8125,
                     Knuc_1 = 0.87266,
                     Rot_1 = 0.17453,
                     Interval = 0.2,
                     coord = c("cartesian", "polar")){



  if(coord == "cartesian"){
    x <- Dep(
      Rate = Rate,
      Epsilon = Epsilon,
      Disc_1 = Disc_1,
      Disc_2 = Disc_2,
      Rad_1 = Rad_1,
      Rad_2 = Rad_2,
      Ins_1 = Ins_1,
      Fing_1 = Fing_1,
      Knuc_1 = Knuc_1,
      Rot_1 = Rot_1,
      Interval = Interval
    )

    # builds dataframe of material deposited (dep) for each coordinate of the substrate surface k and m
    # k and m are determined by Interval
    cart_df <-
      data.frame(
        dep = x,
        k = rep_len(seq(6, -6,
                        # by = -12 / (sqrt(length(x)) - 1)),
                        by = -Interval),
                    length(x)),
        m = rep(seq(-6, 6,
                    # by = 12 / (sqrt(length(x)) - 1)),
                    by = Interval),
                each = sqrt(length(x)))
      )

  # plots raster (heatmap) image for cartesian (static) coordinate systems with color as amount of material deposited
  cart_df %>%
    ggplot() +
    geom_raster(aes(k, m, fill = dep)) +
    theme_bw()

  } else if(coord == "polar") {
    x <- Dep_Polar(
      Rate = Rate,
      Epsilon = Epsilon,
      Disc_1 = Disc_1,
      Disc_2 = Disc_2,
      Rad_1 = Rad_1,
      Rad_2 = Rad_2,
      Ins_1 = Ins_1,
      Fing_1 = Fing_1,
      Knuc_1 = Knuc_1,
      Rot_1 = Rot_1,
      Interval = Interval
    )

    # builds dataframe of deposition (polar) for each pair of angle and radius defined by interval
    polar_df <-
      data.frame(polar = x,
                 # Uses rle() to determine dimension of angle x radius grid.
                 # Grid is rectangular, but when angle = 0 all depositions are the same,
                 # so the length of that first sequence of identical values is the length of the rectangle
                 angle = rep_len(seq(0, 49, 50/max(rle(x)$lengths)) * (2 * pi / 50), length(x)),
                 radius = rep(seq(0, 6, Interval), each = max(rle(x)$lengths)))

    # calculates the total deposition across a radius (dep_sum) and the relative deposition vs. average deposition amount
    # Plots relative deposition vs. radius
    polar_df %>%
      group_by(radius) %>%
      summarise(dep_sum = sum(polar, na.rm = TRUE)) %>%
      # View()
      ungroup() %>%
      mutate(dep_rel = dep_sum/mean(dep_sum, na.rm = TRUE)) %>%
      # View()
      ggplot(aes(x = radius, y = dep_rel)) +
      geom_point() +
      geom_line() +
      theme_bw()

  }
}
