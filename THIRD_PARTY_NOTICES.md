# Third-party notices

Earde's own source code is licensed under the MIT License (see `LICENSE`). This
file lists the third-party material the repository contains, and the licences
of the software it is built with.

## Material included in this repository

### Phoenix JavaScript client

`static/js/phoenix.js` is `priv/static/phoenix.min.js` from the `phoenix` npm
package, version 1.7.21, unmodified apart from a header comment naming it.
Source: <https://github.com/phoenixframework/phoenix>.

```text
Copyright (c) 2014 Chris McCord

Permission is hereby granted, free of charge, to any person obtaining
a copy of this software and associated documentation files (the
"Software"), to deal in the Software without restriction, including
without limitation the rights to use, copy, modify, merge, publish,
distribute, sublicense, and/or sell copies of the Software, and to
permit persons to whom the Software is furnished to do so, subject to
the following conditions:

The above copyright notice and this permission notice shall be
included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
```

### Heroicons

Five inline SVG icons in `lib/` (check, information-circle, x, chat, share) are
the outline icons of Heroicons 1.0.6. Source: <https://github.com/tailwindlabs/heroicons>.

```text
MIT License

Copyright (c) 2020 Refactoring UI Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

### Feather

The inline lock icon in `lib/post_pages.ml` is Feather's `lock` icon (4.29.2).
Source: <https://github.com/feathericons/feather>.

```text
The MIT License (MIT)

Copyright (c) 2013-2023 Cole Bemis

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

### Octicons: GitHub mark

The inline GitHub mark in `lib/page_shell.ml` and `lib/github_onboarding_pages.ml`
is adapted from an earlier version of Octicons' `mark-github` icon. Source: <https://github.com/primer/octicons>.
The icon's code is MIT-licensed. The GitHub logo itself is a trademark of GitHub,
Inc.; Earde shows it only to identify the GitHub integration. Use of the logo is
governed by GitHub's logo guidelines, not by the licence below.

```text
MIT License

Copyright (c) 2026 GitHub Inc.

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

## Fonts

No font files are included. The stylesheet asks for IBM Plex Sans, IBM Plex Mono
and Spectral when a visitor has them installed and otherwise falls back to system
fonts.

## Dependencies (not included in this repository)

These are fetched when you build. Each keeps its own licence.

- **OCaml packages**, pinned in `earde.opam.locked`.
  - Most are MIT, ISC or BSD.
  - Several are LGPL with a linking exception: Caqti is LGPL-3.0-or-later with
    the LGPL-3.0 linking exception, and Zarith is LGPL-2.0 with the OCaml
    linking exception.
  - `lwt_ssl` is LGPL with an OpenSSL linking exception.
  - Menhir, a GPL-2.0 parser generator, runs only at build time.
- **Gleam and Erlang packages** for the realtime gateway, locked in
  `services/realtime_gateway/manifest.toml`.
  - The Gleam standard libraries, Mist, Glisten, `envoy`, `exception`,
    `gramps`, `logging` and `gleeunit` (tests only) are Apache-2.0.
  - Beryl, `lattice_presence`, `palabres` and `hpack_erl` are MIT.
  - Beryl is fetched from GitHub at a pinned commit.
- **System software**: PostgreSQL (PostgreSQL License), libargon2 (CC0-1.0 or
  Apache-2.0) and ImageMagick (ImageMagick License).

If you distribute built binaries, you must meet those licences' terms, including
the LGPL terms of the linked OCaml libraries.

## Name and logos

The Earde name and the two logo files in `static/images/` (`logo-mark.svg` and
`logo-wordmark.svg`) are not covered by the MIT License, and this repository
grants no licence to them. Their origin is not documented here. If you run a
modified copy, replace them and the `earde.com` references in the code, such as
the mail sender and the production analytics origin, with your own.
