let main () =
  Sys.argv.(1)
    |> open_in
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.interpret
    |> Lang.string_of_value
    |> print_endline

let _ = if !Sys.interactive then () else main ()
