
make_workflow <- function(x, y) {
   # TODO add something for add_variables()
   w <-
      workflows::workflow() %>%
      workflows::add_model(y)
   if (inherits(x, "formula")) {
      w <- workflows::add_formula(w, x)
   } else {
      w <- workflows::add_recipe(w, x)
   }
   w
}

halt <- function(...) {
   rlang::abort(paste0(...))
}
