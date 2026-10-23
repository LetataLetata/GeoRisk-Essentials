# View registry and isolation.
#
# Each file in R/views/ calls register_view(). The app builds its navigation
# from the registry. Three layers of protection keep views independent:
#   1. Each view file is sourced on its own; a file with an error is skipped
#      and listed in a banner, the rest of the app still loads.
#   2. Each view's UI, ui(id, store), is built inside tryCatch; a failure shows an error card
#      in that view only.
#   3. Each view's server starts inside tryCatch; outputs inside a view fail
#      individually (Shiny shows the message in that output only).

VIEW_REGISTRY <- new.env()
VIEW_ERRORS <- list()

register_view <- function(id, title, mode = c("analyst", "public", "explore"), icon = "circle", order = 50, ui, server,
                          description = NULL) {
  mode <- match.arg(mode)
  assign(id, list(id = id, title = title, mode = mode, icon = icon, order = order, ui = ui,
                  server = server, description = description), envir = VIEW_REGISTRY)
}

load_views <- function(dir = "R/views") {
  files <- sort(list.files(dir, pattern = "\\.R$", recursive = TRUE, full.names = TRUE))
  for (f in files) {
    res <- tryCatch({ sys.source(f, envir = globalenv()); NULL }, error = function(e) conditionMessage(e))
    if (!is.null(res)) VIEW_ERRORS[[f]] <<- res
  }
  invisible(files)
}

views_for <- function(mode) {
  v <- mget(ls(VIEW_REGISTRY), envir = VIEW_REGISTRY)
  v <- v[vapply(v, function(x) x$mode == mode, logical(1))]
  v[order(vapply(v, function(x) x$order, numeric(1)))]
}

error_card <- function(title, msg) {
  div(class = "gr-card error-card",
      div(class = "gr-card-title", icon("triangle-exclamation"), " ", title, " could not be displayed"),
      p(class = "gr-card-sub", "The rest of the dashboard is unaffected. Fix the view file and reload."),
      tags$pre(class = "gr-code", msg))
}

safe_view_ui <- function(v, store) {
  tryCatch(v$ui(v$id, store), error = function(e) error_card(v$title, conditionMessage(e)))
}

safe_view_server <- function(v, store, shared) {
  tryCatch(v$server(v$id, store, shared),
           error = function(e) {
             message("View '", v$id, "' failed to start: ", conditionMessage(e))
             showNotification(paste0(v$title, ": ", conditionMessage(e)), type = "error", duration = NULL)
           })
}
