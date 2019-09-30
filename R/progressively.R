make_progress <- function(total = 100,
                          format = "(:current/:total) [:bar] :percent [Elapsed: :elapsed ETA: :eta]",
                          clear = FALSE,  show_after = 0,
                          width = getOption("width")) {

  progress::progress_bar$new(
    format = format,
    total = total,
    clear = clear,
    width= width,
    show_after = show_after)
}


make_args <- function(args, .args, env) {
  mapper_type <- attr(.args, 'type')
  n <- switch(
    mapper_type,
    .x  = length(eval(args[[.args[['.xarg']]]], envir = env)),

    .y  = max(length(eval(args[[.args[['.xarg']]]], envir = env)),
              length(eval(args[[.args[['.yarg']]]], envir = env))),

    .l  = max(purrr::map_int(eval(args[[.args[['.larg']]]], envir = env), length)),

    .if = sum(purrr:::probe(eval(args[[.args[['.xarg']]]], envir = env),
                            eval(args[[.args[['.parg']]]], envir = env))),

    .at = sum(purrr:::inv_which(eval(args[[.args[['.xarg']]]], envir = env),
                                eval(args[[.args[['.atarg']]]], envir = env)))

  )
  attr(args, 'n') <- n
  return(args)
}

.pbify <- function(.mapper, .args) {
  fmls <- rlang::fn_fmls(.mapper)

  pf <- function(...) {
    args <- make_args(as.list(match.call())[-1], .args, rlang::caller_env())

    pb <- make_progress(attr(args,"n"))
    g <- eval(args[[.args[['.farg']]]], rlang::caller_env())
    mod_f <- function(...) {
      .out <- purrr::as_mapper(g)(...)
      pb$tick()
      .out
    }
    args[[.args[['.farg']]]] <- mod_f
    do.call(.mapper, args, envir = rlang::caller_env())
  }

  formals(pf) <- formals(.mapper)
  pf
}

.guess_args <- function(.mapper) {
  args <- rlang::fn_fmls_names(.mapper)

  attr(args, 'type') <- dplyr::case_when('.at' %in% args ~ '.at',
                                         '.if' %in% args ~ '.if',
                                         '.l'  %in% args ~ '.l' ,
                                         '.y'  %in% args ~ '.y' ,
                                         '.x'  %in% args ~ '.x' ,
                                         TRUE            ~ NA_character_)

  return(args)
}

.verify_args <- function(.mapper, args) {
  checkmate::assert_function(.mapper)
  checkmate::assert_subset(unlist(args, use.names = F), rlang::fn_fmls_names(.mapper))

  if(length(args) == 0) {


  }

  arg_names <- names(args)

  if(length(arg_names)==0) {
    return(.guess_args(.mapper))
  }

  attr(args, 'type') <- dplyr::case_when('.atarg' %in% arg_names ~ '.at',
                                         '.parg'  %in% arg_names ~ '.if',
                                         '.larg'  %in% arg_names ~ '.l',
                                         '.yarg'  %in% arg_names ~ '.y',
                                         '.xarg'  %in% arg_names ~ '.x')
  return(args)
}


#' Helper function for generating progress bar functions
#'
#' @param .mapper
#' @param .farg `[character(1)]`\cr
#' @param .xarg `[character(1)]`\cr
#' @param .yarg `[character(1)]`\cr
#' @param .larg `[character(1)]`\cr
#' @param .atarg `[character(1)]`\cr
#' @param .parg `[character(1)]`\cr
#'
#' @return A function wrapping .mapper
#' @export
#'
#' @examples progressively(map, .farg = '.f', .xarg = '.x')(rep(1,3) ~Sys.sleep(.x))
#' @export
progressively <- function(.mapper, .farg = NULL, .xarg = NULL,  .yarg = NULL, .larg = NULL, .atarg = NULL, .parg = NULL) {

  args <- rlang::call_args(match.call())[-1]
  args <- .verify_args(.mapper, args)

  .pbify(.mapper, args)
}

#' Modified purrr functions with progress bar
#'
#'
#' @return
#' @export
#'
#' @inheritParams purrr::map
#' @examples
pro_map    <- progressively(purrr::map, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_map2   <- progressively(purrr::map2, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_pmap   <- progressively(purrr::pmap, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_map_if <- progressively(purrr::map_if, .farg = '.f', .xarg = '.x', .parg = '.p')
#' @rdname pro_map
#' @export
pro_map_at <- progressively(purrr::map_at, .farg = '.f', .xarg = '.x', .atarg = '.at')
#' @rdname pro_map
#' @export
pro_map  <- progressively(purrr::map, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_map_at <- progressively(purrr::map_at, .farg = '.f', .xarg = '.x', .atarg = '.at')
#' @rdname pro_map
#' @export
pro_map_chr  <- progressively(purrr::map_chr, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_map_dbl  <- progressively(purrr::map_dbl, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_map_df <- progressively(purrr::map_df, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_map_dfc  <- progressively(purrr::map_dfc, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_map_dfr  <- progressively(purrr::map_dfr, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_map_if <- progressively(purrr::map_if, .farg = '.f', .xarg = '.x', .parg ='.p')
#' @rdname pro_map
#' @export
pro_map_int  <- progressively(purrr::map_int, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_map_lgl  <- progressively(purrr::map_lgl, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_walk <- progressively(purrr::walk, .farg = '.f',  .xarg = '.x')
#' @rdname pro_map
#' @export
pro_map2 <- progressively(purrr::map2, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_map2_chr <- progressively(purrr::map2_chr, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_map2_dbl <- progressively(purrr::map2_dbl, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_map2_df  <- progressively(purrr::map2_df,  .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_map2_dfc <- progressively(purrr::map2_dfc, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_map2_dfr <- progressively(purrr::map2_dfr, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_map2_int <- progressively(purrr::map2_int, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_map2_lgl <- progressively(purrr::map2_lgl, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_walk2  <- progressively(purrr::walk2, .farg = '.f', .xarg = '.x', .yarg = '.y' )
#' @rdname pro_map
#' @export
pro_imap     <- progressively(purrr::imap, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_imap_chr <- progressively(purrr::imap_chr, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_imap_dbl <- progressively(purrr::imap_dbl, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_imap_dfc <- progressively(purrr::imap_dfc, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_imap_dfr <- progressively(purrr::imap_dfr, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_imap_int <- progressively(purrr::imap_int, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_imap_lgl <- progressively(purrr::imap_lgl, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_iwalk    <- progressively(purrr::iwalk, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_invoke_map <- progressively(purrr::invoke_map, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_invoke_map_chr <- progressively(purrr::invoke_map_chr, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_invoke_map_dbl <- progressively(purrr::invoke_map_dbl, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_invoke_map_df  <- progressively(purrr::invoke_map_df,  .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_invoke_map_dfc <- progressively(purrr::invoke_map_dfc, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_invoke_map_dfr <- progressively(purrr::invoke_map_dfr, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_invoke_map_int <- progressively(purrr::invoke_map_int, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_invoke_map_lgl <- progressively(purrr::invoke_map_lgl, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_lmap <- progressively(purrr::lmap, .farg = '.f', .xarg = '.x'  )
#' @rdname pro_map
#' @export
pro_lmap_at  <- progressively(purrr::lmap_at, .farg = '.f', .xarg = '.x', .atarg = '.at')
#' @rdname pro_map
#' @export
pro_lmap_if  <- progressively(purrr::lmap_if, .farg = '.f', .xarg = '.x', .parg  = '.p')
#' @rdname pro_map
#' @export
pro_modify <- progressively(purrr::modify, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_pmap <- progressively(purrr::pmap, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_pmap_chr <- progressively(purrr::pmap_chr, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_pmap_dbl <- progressively(purrr::pmap_dbl, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_pmap_df  <- progressively(purrr::pmap_df, .farg = '.f' , .larg = '.l')
#' @rdname pro_map
#' @export
pro_pmap_dfc <- progressively(purrr::pmap_dfc, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_pmap_dfr <- progressively(purrr::pmap_dfr, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_pmap_int <- progressively(purrr::pmap_int, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_pmap_lgl <- progressively(purrr::pmap_lgl, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_pwalk  <- progressively(purrr::pwalk, .farg = '.f', .larg = '.l')
#' @rdname pro_map
#' @export
pro_reduce <- progressively(purrr::reduce, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_reduce_right <- progressively(purrr::reduce_right, .farg = '.f', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_reduce2  <- progressively(purrr::reduce2, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_reduce2_right  <- progressively(purrr::reduce2_right, .farg = '.f', .xarg = '.x', .yarg = '.y')
#' @rdname pro_map
#' @export
pro_every  <- progressively(purrr::every,  .farg = '.p', .xarg = '.x')
#' @rdname pro_map
#' @export
pro_some   <- progressively(purrr::some, .farg = '.p', .xarg ='.x')
#' @rdname pro_map
#' @export
pro_accumulate       <- progressively(purrr::accumulate, .farg = '.f', .xarg = '.x' )
#' @rdname pro_map
#' @export
pro_accumulate_right <- progressively(purrr::accumulate_right, .farg = '.f', .xarg = '.x' )
