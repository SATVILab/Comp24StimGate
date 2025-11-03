gate <- function(chnl_list,
                 batch_list,
                 path_gs,
                 path_project) {
  gs <- suppressMessages(flowWorkspace::load_gs(path_gs))
  # returns project path
  stimgate::stimgate_gate(
    .data = gs,
    path_project = path_project,
    pop_gate = "root",
    batch_list = batch_list,
    marker = names(chnl_list)
  )
}

gate_tg <- function(path_gs,
                    chnl_list,
                    batch_list,
                    path_project,
                    path_gs_save,
                    tol) {
  gs <- flowWorkspace::load_gs(path_gs)
  suppressMessages(openCyto::register_plugins(
    fun = cytoUtils:::.gate_tail, methodName = "gate_tail"
  ))
  gate_tbl_tg <- purrr::map_df(names(chnl_list), function(x) {
    openCyto::gs_add_gating_method(
      gs = gs,
      alias = x,
      pop = "+",
      parent = "root",
      dims = x,
      gating_method = "gate_tail",
      gating_args = paste0("tol = ", tol)
    )
    gates_list <- flowWorkspace::gs_pop_get_gate(
      gs, y = paste0("root/", x)
    )
    purrr::map_df(seq_along(gs), function(i) {
      gate <- (gates_list[[i]] |>
        flowWorkspace::filter_to_list())$range[[1]]
      tibble::tibble(ind = i, chnl = x, gate = gate)
    })
  })
  if (dir.exists(path_gs_save)) {
    unlink(path_gs_save, recursive = TRUE)
  }
  suppressMessages(flowWorkspace::save_gs(
    gs = gs,
    path = path_gs_save,
    compress = TRUE
  ))
  list(
    path_gs = path_gs_save,
    gate_tbl = gate_tbl_tg
  )
}