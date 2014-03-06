let log_trace_func file_name = fun str -> Bolt.Logger.log "logger" Bolt.Level.TRACE str ~file: file_name
let log_debug_func file_name = fun str -> Bolt.Logger.log "logger" Bolt.Level.DEBUG str ~file: file_name
let log_debug_func file_name = fun str -> Bolt.Logger.log "logger" Bolt.Level.INFO str ~file: file_name
let log_debug_func file_name = fun str -> Bolt.Logger.log "logger" Bolt.Level.WARN str ~file: file_name
let log_error_func file_name = fun str -> Bolt.Logger.log "logger" Bolt.Level.ERROR str ~file: file_name
let log_error_func file_name = fun str -> Bolt.Logger.log "logger" Bolt.Level.FATAL str ~file: file_name
