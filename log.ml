let log_trace_func file_name = fun str -> print_endline ("TRACE " ^ str)
let log_debug_func file_name = fun str -> print_endline ("DEBUG " ^ str)
let log_info_func file_name = fun str -> print_endline ("INFO " ^ str)
let log_warn_func file_name = fun str -> print_endline ("WARN " ^ str)
let log_error_func file_name = fun str -> print_endline ("ERROR " ^ str)
let log_fatal_func file_name = fun str -> print_endline ("FATAL " ^ str)
