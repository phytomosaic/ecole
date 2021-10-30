#' @name foods
#' @title Foods consumption of individuals
#' @aliases foods
#' @docType data
#' @description
#' Foods consumption of individual class members, and nutritional
#'     characteristics of those foods.
#'
#' @format
#' A list of 2 data.frames:\cr
#'     - \code{spe} Foods abundance matrix: 132 observations (individual
#'     people) of 32 foods. Values are how often each person consumed the foods,
#'     on 0-10 scale.\cr
#'     - \code{env} Nutritional matrix: 32 observations (foods) of 12
#'     nutritional characteristics variables.  Descriptions below.
#'
#' @details
#' Coding for variables in the second matrix:
#'
#' calPerServ = calories per serving, based primarily on (Margen, S. 1992. The
#'     Wellness Encyclopedia of Food and Nutrition, Health Letter Associates)
#'     and (Carper, J. and Patricia A. Krause, 1974. The All-in-one Calorie
#'     Counter, Bantam). A few items were obtained by checking nutritional
#'     information on packages.\cr
#' fatGrams = grams of fat per serving, based primarily on Margen (op. cit.).\cr
#' Animal = binary variable, 0 = not from animal products, 1 = from animals.\cr
#' RedMeat = binary variable, 0 = not red meat, 1= red meat.\cr
#' Plant = binary variable, 0 = not from plant products, 1 = primarily plant
#'     product.\cr
#' GreenPlt = consumed as a green plant (cooked or not, but containing
#'     chlorophyll).\cr
#' PctCalFat = percent of calories from fat, based on 100 x (fat,g/serving) x
#'     (9 cal/g) / (cal/serving).\cr
#' PctEaten = percent of people in data with nonzero values for this food.\cr
#' AveScore = average score on food questionnaire (n=83).\cr
#' FastFood = binary variable indicating whether the food is commonly served
#'     at fast food restaurants (1) or not (0).\cr
#' HealthFd = binary variable indicating whether the food is typically
#'     considered a “health food” item, especially as a substitute for another
#'     common food (e.g. tahini instead of peanut butter).\cr
#' Gourmet = binary variable indicating whether the food is typically considered
#'     a “gourmet food” item, seldom eaten on normal occasions.\cr
#'
#' @source
#' The unpublished data are from members of McCune's Community Analysis classes,
#'     past and present.  Nutritional information is cited above.
#'
#' @examples
#' # split into two data.frames
#' data(foods)
#' spe <- foods$spe
#' env <- foods$env
#' @keywords datasets
"foods"
