type 'a parser = string -> ('a * string) option

let pure x : 'a parser = fun input -> Some (x, input)

let fail : 'a parser = fun _ -> None

let ( >>= ) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser = fun input ->
  match p input with
  | Some (r, i) -> f r i
  | None -> None

let ( *> ) (p1 : 'a parser) (p2 : 'b parser) = p1 >>= fun _ -> p2

let ( <* ) (p1 : 'a parser) (p2 : 'b parser) =
  p1 >>= fun r1 -> p2 >>= fun _ -> pure r1

let fmap (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  p >>= fun a -> pure (f a)

let ( <|> ) (p1 : 'a parser) (p2 : 'a parser) : 'a parser = fun input ->
  match p1 input with
  | None -> p2 input
  | succ -> succ

(* see https://hasura.io/blog/parser-combinators-walkthrough/ *)
let rec many (p : 'a parser) : 'a list parser = many1 p <|> pure []
and many1 (p : 'a parser) : 'a list parser =
  p       >>= fun head ->
  many p  >>= fun rest ->
  pure (head :: rest)

let rec sep_by (s : 'a parser) (p : 'b parser) : 'b list parser =
  let not_empty =
    p >>= fun head -> many (s *> p) >>= fun rest -> pure (head :: rest) in
  not_empty <|> pure []

let tail s = String.(if length s > 0 then sub s 1 (length s - 1) else "")

let chars_to_str lst = List.map (String.make 1) lst |> List.fold_left (^) ""

let check (pred : char -> bool) : char parser = fun input ->
  match input with
  | "" -> None
  | s -> if pred s.[0] then Some (s.[0], tail s) else None

let whitespace : unit parser =
  let is_space = function ' ' | '\n' | '\t' -> true | _ -> false in
  many (check is_space) *> pure ()

let symbol c = whitespace *> check ((=) c) <* whitespace

let digits : string parser =
  let is_digit = function '0'..'9' -> true | _ -> false in
  is_digit |> check |> many1 |> fmap chars_to_str

let float_literal : float parser =
  let minus = check ((=) '-') *> pure (-1.) in
  let signp = minus <|> pure 1. in
  let ingrp = digits <|> pure "" in
  let decip = (check ((=) '.') *> ingrp) <|> pure "" in
  signp >>= fun sign    ->
  ingrp >>= fun integer ->
  decip >>= fun decimal ->
  let open String in
  if length integer = 0 && length decimal = 0 then fail
  else (integer ^ "." ^ decimal) |> float_of_string |> ( *. ) sign |> pure

let string_literal : string parser =
  let check_quote = check ((=) '"') in
  let util_quote  = check ((<>) '"') |> many in
  check_quote *> util_quote <* check_quote |> fmap chars_to_str

let keyword (s : string) : unit parser =
  let rec fmap f = function | "" -> [] | s -> f s.[0] :: fmap f (tail s) in
  let charp ch = check ((=) ch) in
  let charps = fmap charp s in
  List.fold_right ( *> ) charps (pure())

type json_value =
  | JsonNull
  | JsonBool   of bool
  | JsonNumber of float
  | JsonString of string
  | JsonArray  of json_value list
  | JsonObject of (string * json_value) list

let json_null  = keyword "null" *> pure JsonNull
let json_false = keyword "false" *> pure (JsonBool false)
let json_true  = keyword "true"  *> pure (JsonBool true)
let json_bool  = json_false <|> json_true
let json_number = float_literal >>= fun n -> pure (JsonNumber n)
let json_string = string_literal >>= fun s -> pure (JsonString s)

let rec json_entry s =
  let colon = symbol ':' in
  s |> (string_literal      >>= fun k ->
        colon *> json_value >>= fun v ->
        pure (k, v))

and json_entries s =
  let comma = symbol ',' in
  s |> (sep_by comma json_entry)

and json_object s =
  let lcurly = symbol '{' in
  let rcurly = symbol '}' in
  let to_obj = fun e -> JsonObject e in
  s |> (lcurly *> json_entries |> fmap to_obj <* rcurly)

and json_array s =
  let comma    = symbol ',' in
  let lbracket = symbol '[' in
  let rbracket = symbol ']' in
  let to_arr   = fun e -> JsonArray e in
  s |> (lbracket *> (sep_by comma json_value) |> fmap to_arr <* rbracket)

and json_value s =
  s |> (json_null  <|> json_bool <|> json_number <|> json_string <|>
        json_array <|> json_object)

let test_data = "{
    \"sites\": [
        { \"name\":\"菜鸟教程\" , \"url\":\"www.runoob.com\" }, 
        { \"name\":\"google\" , \"url\":\"www.google.com\" }, 
        { \"name\":\"微博\" , \"url\":\"www.weibo.com\" }
    ]
}
"

let tfloats = ["-.9456"; "12901.3801"; ".7863"; "1234."; ".-1213"; "-123.456"; "-.1.2.3"]
let tstrings = ["\"hello world\""; "fail?\""; "\"fail?"]
let tobject = "{
  \"name\":\"cjp\",
  \"age\":26
}"