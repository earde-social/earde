module Ob = Earde.Project_onboarding

(* === Rendered /bring form → real browser POST (regression) ===
   The gate suite above feeds the start handler an Origin chosen by the
   test. A browser does not: it derives the Origin of a form POST from the
   *response headers of the document that rendered the form*. This suite
   closes that loop — render /bring exactly as the route does, derive the
   Origin the way a browser would from that very response, then post it at
   the real start handler — so the two halves of the contract can never
   drift apart again.

   The blocker this pins: a document served "Referrer-Policy: no-referrer"
   makes browsers attach `Origin: null` to every non-GET navigation it
   starts, including a plain same-origin form POST. The origin gate then
   (correctly) rejects null, so the shipped CTA answered 403 before any
   request reached GitHub. DB-free: reaching the missing sql_pool boundary
   IS the assertion that every gate passed. *)

let case = Case.quick

(* Fetch, "append a request Origin header", step 3.1: for a request whose
   method is neither GET nor HEAD and whose mode is not "cors" — a plain
   HTML form POST — the serialized origin is replaced by "null" according
   to the request's referrer policy, which a navigation inherits from the
   document. Modelled here verbatim, over already-serialized origins. *)
let browser_form_post_origin ~document_policy ~document_origin ~target_origin =
  let downgrade () =
    (* https document → non-https target. *)
    String.length document_origin >= 6
    && String.equal (String.sub document_origin 0 6) "https:"
    && not
         (String.length target_origin >= 6
         && String.equal (String.sub target_origin 0 6) "https:")
  in
  match String.trim document_policy with
  | "no-referrer" -> "null"
  | "same-origin" ->
      if String.equal document_origin target_origin then document_origin
      else "null"
  | "no-referrer-when-downgrade" | "strict-origin"
  | "strict-origin-when-cross-origin" ->
      if downgrade () then "null" else document_origin
  | _ -> document_origin

(* A production https deployment and the loopback dev deployment from the
   launch smoke test: the gate must accept both, and neither may depend on
   Sec-Fetch-Site to do it. *)
let deployments =
  [
    ("production https", "https://earde.com", Github_fixture.gac_of_values ());
    ( "loopback http",
      "http://localhost:8080",
      Github_fixture.gac_of_values ~origin:(Some "http://localhost:8080")
        ~setup:(Some "http://localhost:8080/integrations/github/install/return")
        ~callback:
          (Some "http://localhost:8080/integrations/github/authorize/callback")
        () );
  ]

(* The one journey: GET /bring, confirm the CTA form is really there, then
   POST it the way the browser that just rendered it would. *)
let rendered_form_posts_case =
  case
    "browser journey: the rendered /bring CTA posts and passes the start gates"
    (fun () ->
      List.iter
        (fun (label, origin, config) ->
          let page = Bring_fixture.run ~session:Bring_fixture.member () in
          let body = Bring_fixture.body_of page in
          (* The CTA the user actually clicks: one POST form, aimed at the
             start route, on the same origin as the page. *)
          Alcotest.(check int)
            (label ^ ": exactly one start form")
            1
            (Bring_fixture.count_occurrences body Bring_fixture.start_action);
          Alcotest.(check bool)
            (label ^ ": the form is a POST")
            true
            (Html_assert.contains_nonempty
               ~needle:("<form method='POST' " ^ Bring_fixture.start_action)
               body);
          let document_policy =
            match Dream.header page "Referrer-Policy" with
            | Some value -> value
            | None -> ""
          in
          let sent_origin =
            browser_form_post_origin ~document_policy ~document_origin:origin
              ~target_origin:origin
          in
          (* Stated separately so a regression reads as the real defect
             ("the page made the browser send null") rather than as an
             opaque 403 further down. *)
          Alcotest.(check string)
            (label ^ ": the browser sends the page's real origin")
            origin sent_origin;
          (* Exactly what a browser puts on that POST. *)
          Http_fixture.check_db_boundary (label ^ ": start POST")
            (Github_handler_fixture.gate_run ~session:Http_fixture.logged_in
               ~headers:
                 [
                   ("Origin", sent_origin);
                   ("Sec-Fetch-Site", "same-origin");
                   ("Sec-Fetch-Mode", "navigate");
                   ("Content-Type", "application/x-www-form-urlencoded");
                 ]
               ~mode:Ob.Public
               ~load_config:(fun () -> config)
               ()))
        deployments)

(* The mechanism itself, both directions: the shipped policy must keep a
   real origin, "no-referrer" must not, and the gate must keep rejecting
   null whatever fetch metadata claims. Without this, someone could
   "fix" a future 403 by loosening the gate instead of the page. *)
let policy_mechanism_case =
  case
    "browser journey: no-referrer nulls the Origin and the gate still refuses \
     it" (fun () ->
      List.iter
        (fun (label, origin, config) ->
          let derive document_policy =
            browser_form_post_origin ~document_policy ~document_origin:origin
              ~target_origin:origin
          in
          Alcotest.(check string)
            (label ^ ": no-referrer nulls the Origin")
            "null" (derive "no-referrer");
          Alcotest.(check string)
            (label ^ ": the shipped policy keeps the Origin")
            origin
            (derive Earde.Request_origin.referrer_policy);
          (* A null origin stays a 403 even with same-origin fetch
             metadata: the fix belongs on the page, never on the gate. *)
          let response =
            Http_fixture.gate_response (label ^ ": null origin")
              (Github_handler_fixture.gate_run ~session:Http_fixture.logged_in
                 ~headers:
                   [ ("Origin", "null"); ("Sec-Fetch-Site", "same-origin") ]
                 ~mode:Ob.Public
                 ~load_config:(fun () -> config)
                 ())
          in
          Alcotest.(check int)
            (label ^ ": null origin is 403")
            403
            (Http_fixture.status_of response))
        deployments)

let suite = [ rendered_form_posts_case; policy_mechanism_case ]

let suites =
  (* The two halves joined: the /bring document as rendered decides the
       Origin a browser puts on the CTA's POST, so the page's referrer
       policy and the start handler's origin gate are tested together.
       DB-free. *)
  [ ("github_bring_browser_post", suite) ]
