progress_bar <- R6::R6Class(
  "progress_bar",

  public = list(
    initialize = function() { pb_init(self, private, total = 100) },
    tick = function() { pb_tick(self, private) },
    update = function() { pb_update() },
    message = function() { pb_message() },
    terminate = function() { pb_terminate() },
    finished = FALSE
  ),

  private = list(
    render = function(tokens) { pb_render(self, private, tokens) },
    total = NULL,
    current = 0,
    style = NULL
  ),

  active = list(
    ratio = function() { pb_ratio(self, private) },
    remaining = function() { pb_remaining(self, private) }
  )
)

pb_init <- function(self, private, format, total, style) {
  private$total <-  total

  self
}

pb_tick <- function(self, private) {
  private$current <- private$current + 1
  self
}

pb_update <- function(variables) {

}

pb_message <- function(variables) {

}

pb_terminate <- function(variables) {

}

pb_ratio <- function(self, private) {
  private$current / private$total
}

pb_remaining <- function(self, private) {
  private$total - private$current
}


progress_bar_style <- R6::R6Class(
  "progress_bar_style",
  public = list(),
  private = list()
)


progress_bar_stack <- R6::R6Class(
  "progress_bar_stack",
  public = list(),
  private = list()
)
