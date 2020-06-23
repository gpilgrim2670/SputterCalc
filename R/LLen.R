#' LLen
#'
#' @description calculates the length of line L
#'
#' @author Greg Pilgrim \email{gpilgrim@@vergason.com}
#'
#' @param Disc_1 relative angle (in degrees) of larger disc (disc 1) in chamber door vs. gravity
#' @param Disc_2 relative angle (in degrees) of smaller disc (disc 2) in chamber door vs. gravity
#' @param Rad_1 fixed value describing  the distance between the center of larger disc (disc 1) and the center of the smaller disc (disc 2)
#' @param Rad_2 fixed value describing the distance between the center of disc 2 and the position of each cathode (cathodes are symmetric)
#' @param Ins_1 distance the gun has been inserted into chamber, measured from inside of disc 2 to the center of the gun knuckle
#' @param Fing_1 distance from the knuckle to the gun tip
#' @param Knuc_1 angle (in radians) at which the gun is set at the knuckle.  Min is 0, max is 1.18
#' @param Rot_1 angle (in radians) at which the gun is rotated, min is 0, max is 2*pi
#' @param Interval the size of each step in a numeric solution.  Larger intervals will produce courser solutions, but will be faster.  Smaller intervals will produce more detailed solutions but will be more computationally expensive

LLen <- function(Disc_1 = -5,
                 Disc_2 = -33,
                 Rad_1 = 1.4375,
                 Rad_2 = 3,
                 Ins_1 = 2.375,
                 Fing_1 = 4.8125,
                 Knuc_1 = 0.610865,
                 Rot_1 = 0.785398,
                 Interval = 0.2){

  # Converting from degrees to radians
  Disc_1 <- Disc_1 * pi/180
  Disc_2 <- Disc_2 * pi/180

  # Unknown constant from Michael
  F_30 <- (8 + Rad_1 * cos(Disc_1) + Rad_2 * sin(Disc_2))/(Fing_1 * sin(Knuc_1) * cos(Rot_1))

  # Platen intercept point, n and q only
  n <- (-Rad_1 * sin(Disc_1) + Rad_2 * cos(Disc_2)) - F_30 * (Fing_1 * sin(Knuc_1) * sin(Rot_1))
  q <- 12 - Ins_1 - Fing_1 * cos(Knuc_1) * F_30

  # k region
  k <- seq(6, -6, by = -Interval)

  # m region
  m <- seq(-6, 6, by = Interval)

  # k and m to grid
  gr <- expand.grid(k, m)


  LLen <- sqrt((n - gr$Var1)^2 + (q - gr$Var2)^2)
  # LLen <- data.frame(matrix(unlist(LLen), nrow=sqrt(length(LLen)), byrow = FALSE))
  return(LLen)
}
