let parse = ref false

let flag_handler file_name parse_b =
  match parse_b with
    | true  -> open_in file_name |> Lexing.from_channel |> Parser.prog Lexer.token |> Lang.string_of_exp
    | false -> open_in file_name |> Lexing.from_channel |> Parser.prog Lexer.token |> Lang.typechk |> Lang.interpret |> Lang.string_of_value

let main () = begin
  let speclist = [("-parse", Arg.Set parse, "Performs lexing and parsing, but not evaluation.")] in
  let anon_fun = (fun file_name -> flag_handler file_name !parse |> print_endline) in
  let usage_msg = "Flags: " in
  Arg.parse speclist anon_fun usage_msg
end

let _ = if !Sys.interactive then () else main ()
