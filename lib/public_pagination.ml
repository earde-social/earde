(* Public listings page with LIMIT/OFFSET, and every skipped row is still
   produced by the database (with its comment count and score) before it is
   thrown away, so the cost of a page grows with its number. The page number
   comes from an anonymous query string; this bound keeps the deepest page an
   anonymous client can ask for at a known, measured cost. *)
let max_page = 1000
let page_size = 20

(* Plain decimal digits only, read one at a time with an early exit, so no
   input — however long — can overflow. Anything that is not a plain number
   keeps the old behaviour of falling back to page 1. *)
let parse = function
  | None -> Ok 1
  | Some raw -> (
      let s = String.trim raw in
      let digits =
        s <> "" && String.for_all (function '0' .. '9' -> true | _ -> false) s
      in
      if not digits then Ok 1
      else
        let rec read i acc =
          if acc > max_page then Error `Out_of_range
          else if i = String.length s then Ok acc
          else read (i + 1) ((acc * 10) + Char.code s.[i] - Char.code '0')
        in
        match read 0 0 with Ok 0 -> Ok 1 | result -> result)

let offset page = (page - 1) * page_size

let out_of_range ?user ~return_url request =
  Dream.respond ~status:`Bad_Request
    (Site_pages.msg_page ?user ~title:"Page out of range"
       ~message:
         (Printf.sprintf
            "Listings stop at page %d. Try a different sort or a narrower \
             search."
            max_page)
       ~alert_type:"error" ~return_url request)
