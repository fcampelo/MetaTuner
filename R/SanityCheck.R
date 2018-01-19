#' Check sanity of inputs for metatuner
#'
#' @param myenv list containing input parameters used for [metatuner()].
#'
#' @author Felipe Campelo (\email{fcampelo@@ufmg.br})
#'

SanityCheck <- function(myenv){

  with(myenv, {
    # Check input `parameters`
    assertthat::assert_that(is.data.frame(parameters),
                            nrow(parameters) > 0,
                            all(assertthat::has_name(parameters,
                                                     c("name",
                                                       "minx",
                                                       "maxx"))),
                            all(parameters$minx < parameters$maxx))

    # Check input `tuning.instances`
    assertthat::assert_that(is.list(tuning.instances),
                            length(tuning.instances) > 0,
                            all(sapply(tuning.instances,
                                       function(x){
                                         all(assertthat::has_name(x,
                                                                  c("FUN",
                                                                    "xmin",
                                                                    "xmax")))}
                            )),
                            all(sapply(tuning.instances,
                                       function(x){
                                         all(x$xmin < x$xmax)}))
    )

    # Check numeric / integer inputs
    assertthat::assert_that(assertthat::is.count(m0),
                            assertthat::is.count(mi),
                            assertthat::is.count(N0),
                            assertthat::is.count(Ni),
                            assertthat::is.count(elite.confs),
                            assertthat::is.count(model.order),
                            assertthat::is.count(budget),
                            assertthat::is.count(seed),
                            all(sapply(ndigits, assertthat::is.count)))

    # Check character inputs
    assertthat::assert_that(is.character(algo.runner),
                            length(algo.runner) == 1,
                            is.character(initial.sampling),
                            length(initial.sampling) == 1,
                            is.character(summary.function),
                            length(summary.function) == 1,
                            is.character(model.type),
                            length(model.type) == 1,
                            is.character(optimization.method),
                            length(optimization.method) == 1)

  })

  invisible(TRUE)
}
