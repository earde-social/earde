let safe_local_redirect ?(default = "/") request target =
  let has_forbidden_byte s =
    String.exists (fun c -> c < ' ' || c = '\x7f' || c = '\\') s
  in
  let drop_fragment s =
    match String.index_opt s '#' with Some i -> String.sub s 0 i | None -> s
  in
  let local_path s =
    if String.length s >= 2 && String.sub s 0 2 = "//" then None
    else if String.length s > 0 && s.[0] = '/' then Some s
    else None
  in
  let same_origin_path s =
    let uri = Uri.of_string s in
    match (Uri.scheme uri, Uri.host uri, Dream.header request "Host") with
    | Some scheme, Some url_host, Some host_header when Uri.userinfo uri = None
      ->
        let scheme = String.lowercase_ascii scheme in
        if not (String.equal scheme "http" || String.equal scheme "https") then
          None
        else
          let url_host = String.lowercase_ascii url_host in
          let url_port =
            match Uri.port uri with
            | Some p -> p
            | None -> if String.equal scheme "https" then 443 else 80
          in
          let host_header = String.lowercase_ascii (String.trim host_header) in
          let header_host, header_port =
            match String.rindex_opt host_header ':' with
            | Some i -> (
                let suffix =
                  String.sub host_header (i + 1)
                    (String.length host_header - i - 1)
                in
                match int_of_string_opt suffix with
                | Some p -> (String.sub host_header 0 i, Some p)
                | None -> (host_header, None))
            | None -> (host_header, None)
          in
          let port_matches =
            match header_port with
            | Some p -> p = url_port
            (* A portless Host header implies a default port; both http and
               https defaults count as ours because the proxy owns the outer
               scheme. *)
            | None -> url_port = 80 || url_port = 443
          in
          if String.equal header_host url_host && port_matches then
            let path = match Uri.path uri with "" -> "/" | p -> p in
            let with_query =
              match Uri.verbatim_query uri with
              | Some q when not (String.equal q "") -> path ^ "?" ^ q
              | _ -> path
            in
            (* A same-origin URL can still carry a protocol-relative path
               (https://host//evil) — re-check through the local-path rules. *)
            local_path with_query
          else None
    | _ -> None
  in
  if has_forbidden_byte target then default
  else
    let target = drop_fragment target in
    match local_path target with
    | Some p -> p
    | None -> (
        match same_origin_path target with Some p -> p | None -> default)

(* Database failures must never reach a response body: the strings produced
   by Caqti_error.show may contain internal driver details, connection
   metadata, and SQL text (including constraint and relation names). Log the
   detail server-side and render this stable generic message instead. *)
let generic_db_error = "A database error occurred. Please try again later."

let db_error_message err =
  Logs.err (fun m -> m "database error: %s" err);
  generic_db_error

(* Scan body text for @username tokens without external library deps.
   Only ASCII-alphanumeric + underscore is valid; deduped via sort_uniq to avoid
   sending the same user multiple notifications from repeated mentions. *)
let extract_mentions text =
  let len = String.length text in
  let mentions = ref [] in
  let i = ref 0 in
  while !i < len do
    if text.[!i] = '@' then begin
      let start = !i + 1 in
      let j = ref start in
      while
        !j < len
        &&
        let c = text.[!j] in
        (c >= 'a' && c <= 'z')
        || (c >= 'A' && c <= 'Z')
        || (c >= '0' && c <= '9')
        || c = '_'
      do
        incr j
      done;
      if !j > start then
        mentions := String.sub text start (!j - start) :: !mentions;
      i := !j
    end
    else incr i
  done;
  List.sort_uniq String.compare !mentions

(* === SHARED HELPERS === *)

let get_current_user_votes db request =
  match Dream.session_field request "user_id" with
  | Some uid_str -> (
      match%lwt User_store.get_user_post_votes db (int_of_string uid_str) with
      | Ok v -> Lwt.return v
      | Error _ -> Lwt.return [])
  | None -> Lwt.return []

let get_current_user_comment_votes db request =
  match Dream.session_field request "user_id" with
  | Some uid_str -> (
      match%lwt
        User_store.get_user_comment_votes db (int_of_string uid_str)
      with
      | Ok v -> Lwt.return v
      | Error _ -> Lwt.return [])
  | None -> Lwt.return []
