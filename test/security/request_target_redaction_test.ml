module Rtr = Earde.Request_target_redaction

(* === REQUEST-TARGET REDACTION ===
   Pure redaction/path-only behavior plus the middleware pair, all DB-free.
   Fixture "secrets" are obviously fake placeholders, and assertions on the
   original values use boolean equality so a failure never prints them. *)

let rtr_case name f = Alcotest.test_case name `Quick f

let rtr_check name expected input =
  rtr_case name (fun () ->
      Alcotest.(check string) name expected (Rtr.redact_target input))

let rtr_unchanged name input = rtr_check name input input

let rtr_path name expected input =
  rtr_case name (fun () ->
      Alcotest.(check string) name expected (Rtr.path_only input))

(* Runs [target] through redact -> observer -> restore -> final handler and
   returns what the middle observer and the final handler each saw, i.e. the
   views Dream.logger/analytics and the router respectively would get. *)
let rtr_pipeline_views target =
  let seen_mid = ref None and seen_final = ref None in
  let observe slot inner request =
    slot := Some (Dream.target request);
    inner request
  in
  let handler =
    Rtr.redact_middleware
    @@ observe seen_mid
    @@ Rtr.restore_middleware
    @@ fun request ->
    seen_final := Some (Dream.target request);
    Dream.respond "ok"
  in
  ignore
    (Lwt_main.run (handler (Dream.request ~method_:`GET ~target ""))
      : Dream.response);
  match (!seen_mid, !seen_final) with
  | Some mid, Some final -> (mid, final)
  | _ -> Alcotest.fail "pipeline did not reach both observation points"

let suites =
    (* Pure redaction: only token/state/code values change, everything else
       is byte-for-byte identical. *)
  [ ( "request_target_redaction_pure"
    , [ rtr_check "token value hidden" "/verify?token=[REDACTED]"
          "/verify?token=fake1"
      ; rtr_check "state value hidden"
          "/integrations/github/install/return?state=[REDACTED]&installation_id=42"
          "/integrations/github/install/return?state=fake2&installation_id=42"
      ; rtr_check "code value hidden" "/cb?code=[REDACTED]" "/cb?code=fake3"
      ; rtr_check "all three keys in one target"
          "/integrations/github/authorize/callback?code=[REDACTED]&state=[REDACTED]&token=[REDACTED]"
          "/integrations/github/authorize/callback?code=fake4&state=fake5&token=fake6"
      ; rtr_check "repeated sensitive key hides every occurrence"
          "/r?state=[REDACTED]&state=[REDACTED]"
          "/r?state=fakeA&state=fakeB"
      ; rtr_check "empty sensitive value still redacted"
          "/verify?token=[REDACTED]" "/verify?token="
      ; rtr_unchanged "bare sensitive key without = has no value to hide"
          "/verify?token"
      ; rtr_check "bare key beside a real value"
          "/r?state&code=[REDACTED]" "/r?state&code=fake7"
      ; rtr_check "sensitive parameter first"
          "/r?code=[REDACTED]&a=1" "/r?code=fake8&a=1"
      ; rtr_check "sensitive parameter in the middle"
          "/r?a=1&code=[REDACTED]&b=2" "/r?a=1&code=fake9&b=2"
      ; rtr_check "sensitive parameter last"
          "/r?a=1&code=[REDACTED]" "/r?a=1&code=fake10"
      ; rtr_check "non-sensitive parameters and order preserved"
          "/r?zeta=9&token=[REDACTED]&alpha=1&zeta=9"
          "/r?zeta=9&token=fake11&alpha=1&zeta=9"
      ; rtr_check "percent-encoded non-sensitive value untouched"
          "/search?q=%2Focaml%20dream&state=[REDACTED]"
          "/search?q=%2Focaml%20dream&state=fa%2Bke"
      ; rtr_unchanged "queryless target unchanged" "/login"
      ; rtr_unchanged "similarly named keys not redacted"
          "/r?notstate=v&stateful=v&tokens=v&decode=v&codex=v"
      ; rtr_unchanged "sensitive string in path only"
          "/not-a-query/state=value"
      ; rtr_unchanged "sensitive string inside another value"
          "/r?x=state=value&redirect=/x?state=value"
      ; rtr_unchanged "uppercase variants not canonical"
          "/r?State=v&CODE=v&Token=v"
      ; rtr_check "fragment-like suffix preserved"
          "/r?state=[REDACTED]#frag=code=1" "/r?state=fake12#frag=code=1"
      ; rtr_unchanged "fragment-like content is not query"
          "/r?a=1#state=value"
      ; rtr_case "arbitrary malformed targets never raise" (fun () ->
            List.iter
              (fun input ->
                ignore (Rtr.redact_target input : string);
                ignore (Rtr.path_only input : string))
              [ ""; "?"; "#"; "?#"; "???&&&=="; "&state=x"; "?=&=&";
                "?state=%"; "#?state=x"; "\x00\xff?\x01state=\x02";
                "no-slash?token=x&"; "?&&token" ])
      ] )
    (* Path-only extraction: the stable non-secret classification key. *)
  ; ( "request_target_path_only"
    , [ rtr_path "plain path" "/login" "/login"
      ; rtr_path "query removed" "/search" "/search?q=ocaml"
      ; rtr_path "query and fragment-like suffix removed" "/p"
          "/p?a=1#frag"
      ; rtr_path "github callback shape"
          "/integrations/github/authorize/callback"
          "/integrations/github/authorize/callback?code=fake13&state=fake14"
      ; rtr_path "empty input" "/" ""
      ; rtr_path "query-only input" "/" "?a=b"
      ; rtr_case "no query value survives extraction" (fun () ->
            let result =
              Rtr.path_only
                "/integrations/github/install/return?state=fake15&installation_id=42"
            in
            Alcotest.(check bool) "no raw value" false
              (Html_assert.contains result "fake15");
            Alcotest.(check bool) "no redaction marker" false
              (Html_assert.contains result "[REDACTED]");
            Alcotest.(check bool) "no query at all" false
              (Html_assert.contains result "?"))
      ] )
    (* Middleware pair: logger/analytics position sees [REDACTED], the
       router position sees the exact original bytes. *)
  ; ( "request_target_middleware"
    , [ rtr_case "redacted for observers, original for the handler"
          (fun () ->
            let original =
              "/integrations/github/authorize/callback?code=fake16&state=fake17&token=fake18"
            in
            let mid, final = rtr_pipeline_views original in
            Alcotest.(check string) "observer sees redacted target"
              "/integrations/github/authorize/callback?code=[REDACTED]&state=[REDACTED]&token=[REDACTED]"
              mid;
            Alcotest.(check bool) "handler sees exact original" true
              (String.equal original final))
      ; rtr_case "no sensitive parameter: unchanged throughout" (fun () ->
            let original = "/feed?after=42&sort=new" in
            let mid, final = rtr_pipeline_views original in
            Alcotest.(check string) "observer view" original mid;
            Alcotest.(check string) "handler view" original final)
      ; rtr_case "restore is a no-op without a stashed original" (fun () ->
            let target = "/feed?after=42" in
            let seen = ref None in
            let handler =
              Rtr.restore_middleware @@ fun request ->
              seen := Some (Dream.target request);
              Dream.respond "ok"
            in
            ignore
              (Lwt_main.run
                 (handler (Dream.request ~method_:`GET ~target ""))
                : Dream.response);
            Alcotest.(check (option string)) "target untouched"
              (Some target) !seen)
      ] )
    (* The rate limiter derives its endpoint key via path_only, so a
       callback-shaped target buckets by path alone and no query value can
       reach the rate_limits table. *)
  ; ( "request_target_rate_limit_key"
    , [ rtr_case "callback-shaped target keys by path only" (fun () ->
            Alcotest.(check string) "endpoint key"
              "/integrations/github/install/return"
              (Rtr.path_only
                 "/integrations/github/install/return?state=fake19&installation_id=42"))
      ; rtr_case "query variants share one bucket" (fun () ->
            Alcotest.(check string) "same key"
              (Rtr.path_only "/login")
              (Rtr.path_only "/login?anything=fake20"))
      ] )
  ]
