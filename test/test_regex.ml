open Regex.Matcher

let a = Char 'a'
let b = Char 'b'

(* Empty matches only the empty string *)
let () = assert (matches Empty "")
let () = assert (not (matches Empty "a"))

(* a single character *)
let () = assert (matches a "a")
let () = assert (not (matches a ""))
let () = assert (not (matches a "b"))
let () = assert (not (matches a "aa"))

(* sequencing *)
let () = assert (matches (Seq (a, b)) "ab")
let () = assert (not (matches (Seq (a, b)) "a"))
let () = assert (not (matches (Seq (a, b)) "ba"))

(* alternation *)
let () = assert (matches (Alt (a, b)) "a")
let () = assert (matches (Alt (a, b)) "b")
let () = assert (not (matches (Alt (a, b)) "c"))

(* star *)
let () = assert (matches (Star a) "")
let () = assert (matches (Star a) "aaaa")
let () = assert (not (matches (Star a) "aab"))

(* a star over something that can match nothing must still terminate *)
let () = assert (matches (Star Empty) "")
let () = assert (not (matches (Star Empty) "a"))

(* (a|b)*abb -- the classic *)
let abstar = Star (Alt (a, b))
let pattern = Seq (abstar, Seq (a, Seq (b, b)))
let () = assert (matches pattern "abb")
let () = assert (matches pattern "aababb")
let () = assert (matches pattern "babbabb")
let () = assert (not (matches pattern "ab"))
let () = assert (not (matches pattern "abba"))

(* backtracking really is required: the star must give characters back *)
let () = assert (matches (Seq (Star a, Seq (a, b))) "aaab")

(* Part 3 *)
let words = ["abb"; "ab"; "babb"; ""; "aabb"]
let () = assert (keep_matches pattern words = ["abb"; "babb"; "aabb"])
let () = assert (label_matches pattern ["abb"; "ab"] = [("abb", true); ("ab", false)])
let () = assert (total_match_length pattern words = 11)
let () = assert (total_match_length pattern [] = 0)
let () = print_endline "All tests passed!"