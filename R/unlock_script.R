
unlock_script <- function(script_path) {

  with_script(script_path, {

    lock <- file.exists(opt('lock_filename'))
    completed <- file.exists(opt('completed_filename'))
    halted <- file.exists(opt('halted_filename'))

    if(lock) file.remove(opt('lock_filename'))
    if(completed) file.remove(opt('completed_filename'))
    if(halted) file.remove(opt('halted_filename'))

    invisible()
  })

}
