

options::define_options(

  "filename of the script to be run",
  script_filename = "run.R",

  "filename of the environment file",
  environment_filename = ".Rdata",

  "filename of the lock file",
  lock_filename = "LOCK",

  "filename of the completed file",
  completed_filename = "COMPLETED",

  "filename of the halted file",
  halted_filename = "HALTED",

  "filename of the log file",
  log_filename = "all.log",

  "filename of the log file that contains output",
  output_log_filename = "output.log",

  "filename of the log file that contains messages",
  message_log_filename = "message.log",

  "scriptRunner marker file",
  marker_filename = ".scriptRunner"

  )
