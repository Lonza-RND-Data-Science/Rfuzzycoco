#' shared params
#'
#' 
#' @param data      the data to fit as a data frame.  The output variables must be grouped AFTER the input variables
#' @param engine    the fuzzy coco fit engine to use, one of **rcpp** and **hybrid**
#' @param formula   the fuzzy coco model as a formula
#' @param fuzzy_system_desc   a fuzzy system description as a named list
#' @param max_generations The maximum number of iterations of the algorithm. 
#'                        Each iteration produces a new generation of the rules and membership functions populations.
#' @param max_fitness     a stop condition: the iterations stop as soon as a generated fuzzy system fitness 
#'                        reaches that threshold.
#' @param mode      the type of model, either **classification** or **regression**
#' @param model     a Fuzzy Coco model as a `fuzzy_coco` object
#' @param params    fuzzy coco parameters, as a recursive named list, cf [params()]
#' @param progress  whether to display the computation progress (using progressr, if available)
#' @param seed      the RNG seed to use (to fit the model)
#' @param until     function that takes an `engine` and returns TRUE if and only if the evolution must stop.
#'                  It is a way for the user to customize the stop conditions of the algorithm.
#' @param verbose   whether to be verbose
#' @param x         the input variables data (usually to fit) as a dataframe
#' @param y         the output variables data (usually to fit)  as a dataframe
#' 
#' @name shared_params
#' @keywords internal
#'
NULL