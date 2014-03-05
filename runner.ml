let file_name = Array.get Sys.argv 1 in
let file_string = Utils.file_name_to_string file_name in
Bolt.Logger.log "logger" Bolt.Level.TRACE "started" ~file: "runner";
try Parse.parse (Lex.lex file_string)
with _ -> exit 1
