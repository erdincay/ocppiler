let main () =
  Sys.argv.(1)
    |> open_in
    |> Lexing.from_channel
    |> Parser.prog Lexer.token
    |> Lang.eval
    |> Lang.string_of_value
    |> print_endline

let _ = if !Sys.interactive then () else main ()
