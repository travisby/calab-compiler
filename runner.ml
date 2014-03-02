Bolt.Logger.log "logger" Bolt.Level.TRACE "started" ~file: "runner";
try Parse.parse (Lex.lex "{ _ } $")
with _ -> exit 1
