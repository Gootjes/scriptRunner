
get_script_status <- function(script_path, all = FALSE) {

  with_script(script_path, {

    lock <- file.exists(opt('lock_filename'))
    completed <- file.exists(opt('completed_filename'))
    halted <- file.exists(opt('halted_filename'))

    if (all) {
      c(NOT_STARTED = !lock && !completed && !halted,
        LOCKED = lock,
        HALTED = halted,
        COMPLETED = completed)
    } else {
      if (lock) {
        opt('lock_filename')
      } else if (halted) {
        opt('halted_filename')
      } else if(completed) {
        opt('completed_filename')
      } else {
        "NOT_STARTED"
      }
    }

  })

}
