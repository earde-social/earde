# Safe rendering

Every server-rendered page is built from values of the abstract type
`Html.t` (`lib/html.mli`). A value of that type is markup that is safe where
it is placed. A page builder serializes the document once, with
`Html.to_string`, at the response boundary.

## How markup and data meet

| Source | Constructor | Result |
|---|---|---|
| Text, including every user- or database-supplied string | `Html.text` | The five markup characters become entities. Safe as element content and as a quoted attribute value. |
| Numbers | `Html.int`, `Html.int64` | Digits. |
| A user-supplied link | `Html.external_url` | `http(s)` only, otherwise the inert `#`. |
| A path inside the site | `Html.internal_path` | A single leading `/` only, never `//host` or `/\host`, otherwise `#`. |
| An image source | `Html.image_src` | A rooted upload path or `http(s)`, refusing quotes, whitespace, backticks and control characters, otherwise `#`. |
| Markup written in this codebase | `Html.template`, `Html.static` | The argument must be a string literal. `template` fills its `%s` holes, in order, with `Html.t` values only. |

The `_opt` variants of the URL policies return `None` instead of `#`, for
code that renders something else when a URL is refused.

There is one escape hatch, `Html.trusted`. It is used only by
`Csrf_field.tag`, for the hidden input Dream renders and escapes itself.
Two census tests (`html_census`) fail when markup enters through a
non-literal, or when `Html.trusted` appears anywhere else.

## Scripts

Values that inline JavaScript needs travel in `data-*` attributes as text,
and the handler reads them from `this.dataset`. The confirmation dialogs,
post rows and Share buttons work this way. No user data is ever written into
script source, so no JavaScript string escaping exists. The page scripts
themselves are static literals.

## Limits

- `Html.text` makes a value safe in element content and quoted attributes
  only. An unquoted attribute, a URL, a style or a script needs its own
  constructor, or a data attribute.
- The census enforces the literal rule for `lib/`. Tests may wrap fixtures
  with `Html.trusted` when a fixture stands for already-rendered markup.
