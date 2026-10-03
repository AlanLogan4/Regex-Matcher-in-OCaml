type re =
  | Empty                 (* matches the empty string, and nothing else *)
  | Char of char          (* matches exactly that one character *)
  | Seq of re * re        (* matches the first, then the second *)
  | Alt of re * re        (* matches either one *)
  | Star of re            (* matches the inner pattern zero or more times *)


let rec matchesStartLeavingValidRemainder (startsWith: re) (input: string) (validRemainder: string -> bool) =
  let n = String.length input in
  match (startsWith, input) with
  | (Empty, _) -> validRemainder input
  | (Char c, _) when n > 0 && String.get input 0 = c -> validRemainder (String.sub input 1 (n - 1))
  | (Seq (first, second), _) -> matchesStartLeavingValidRemainder first input (fun rest -> matchesStartLeavingValidRemainder second rest validRemainder)
  | (Alt (a, b), _) -> matchesStartLeavingValidRemainder a input validRemainder || matchesStartLeavingValidRemainder b input validRemainder
  | (Star p, x) -> validRemainder input || matchesStartLeavingValidRemainder p input (fun rest -> String.length rest < n && matchesStartLeavingValidRemainder (Star p) rest validRemainder)
  | _ -> false

let matches r s = matchesStartLeavingValidRemainder r s (fun rest -> rest = "")

(* only the words that pattern matches *)
let keep_matches pattern words = 
  words
  |> List.filter(fun word -> matches pattern word)

(* each word paired with whether pattern matches it *)
let label_matches pattern words = 
  words
  |> List.map (fun word -> (word, matches pattern word))

(* the total length of the words pattern matches, using List.fold_left *)
let total_match_length pattern words = 
  words
  |> List.filter (fun word -> matches pattern word)
  |> List.fold_left(fun total word -> total + String.length word) 0