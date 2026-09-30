(* Byte-level scanning instead of Uri parse/reserialize: round-tripping a
   target through Uri can change its byte representation (encoding
   normalization, parameter reordering), and logs/analytics must see the
   request exactly as sent apart from the redacted values. *)

(* Exact, case-sensitive keys. [token] covers password-reset and
   email-verification links; [state] the GitHub onboarding CSRF state; [code]
   the future GitHub OAuth authorization code. No speculative keys. *)
let sensitive_keys = [ "token"; "state"; "code" ]
let replacement = "[REDACTED]"

let redact_target target =
  match String.index_opt target '?' with
  | None -> target
  | Some q ->
      let len = String.length target in
      (* The query component ends at a fragment-like [#]; anything after it is
         preserved untouched even though HTTP targets normally lack one. *)
      let query_end =
        match String.index_from_opt target q '#' with
        | Some h -> h
        | None -> len
      in
      let buf = Buffer.create len in
      Buffer.add_string buf (String.sub target 0 (q + 1));
      let changed = ref false in
      let i = ref (q + 1) in
      while !i < query_end do
        (* One query component: starts right after [?] or a raw [&], ends at
           the next raw [&] or the end of the query. A second [?] inside a
           value (e.g. ?redirect=/x?state=v) never starts a new component. *)
        let comp_end =
          match String.index_from_opt target !i '&' with
          | Some a when a < query_end -> a
          | _ -> query_end
        in
        let comp = String.sub target !i (comp_end - !i) in
        let matches key =
          let klen = String.length key in
          String.length comp >= klen
          && String.equal (String.sub comp 0 klen) key
          && (String.length comp = klen || comp.[klen] = '=')
        in
        (match List.find_opt matches sensitive_keys with
        | Some key when String.length comp > String.length key ->
            (* key= with a (possibly empty) value: hide the value. *)
            changed := true;
            Buffer.add_string buf key;
            Buffer.add_char buf '=';
            Buffer.add_string buf replacement
        | _ ->
            (* Not sensitive, or a bare key with no [=] and hence no value to
               hide — never "repair" the target by inventing one. *)
            Buffer.add_string buf comp);
        if comp_end < query_end then Buffer.add_char buf '&';
        i := comp_end + 1
      done;
      if query_end < len then
        Buffer.add_string buf (String.sub target query_end (len - query_end));
      if !changed then Buffer.contents buf else target

let path_only target =
  let len = String.length target in
  let cut_at c current =
    match String.index_opt target c with
    | Some i when i < current -> i
    | _ -> current
  in
  let cut = cut_at '?' (cut_at '#' len) in
  if cut = 0 then "/" else String.sub target 0 cut

(* Private per-request slot holding the ORIGINAL (unredacted) target.
   Deliberately not exposed: the only way handlers see the original values is
   through the restored target, so nothing upstream of restore_middleware can
   read the secrets back out. *)
let original_target_field : string Dream.field =
  Dream.new_field ~name:"earde.raw_target" ()

(* Dream.set_target is internal; we reach it via dream-pure's Message module,
   which is the same mutable record Dream.target reads. *)
let redact_middleware handler request =
  let target = Dream.target request in
  let redacted = redact_target target in
  if not (String.equal redacted target) then begin
    Dream.set_field request original_target_field target;
    Dream_pure.Message.set_target request redacted
  end;
  handler request

let restore_middleware handler request =
  (match Dream.field request original_target_field with
  | Some original -> Dream_pure.Message.set_target request original
  | None -> ());
  handler request
