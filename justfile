[private]
@default:
  just --list

[doc("Update file modification dates, comitting them to git")]
update-modification-dates:
  MOONYTHM_UPDATE_LAST_MODIFIED=1 cargo run
  git add last_modified.toml
  git commit -m "Update \`last_modified.toml\`"

[doc("Copy the current date to clipboard in the required `created_at` format")]
current-date:
  date --rfc-3339=seconds | wl-copy

[doc("Serve the build website locally")]
serve-dev:
  http-server dist/web

# {{{ Building
[private]
[group("build")]
[doc("Minify the sitemap .xml file")]
minify-sitemap:
  xmllint --noblanks dist/web/sitemap.xml --output dist/web/sitemap.xml

[group("build")]
[doc("Build the website")]
build:
  cargo run
  just minify-sitemap

[group("build")]
[doc("Build the website, including draft posts")]
build-dev:
  MOONYTHM_DRAFTS=1 cargo run
# }}}
# {{{ Linting
[group("lint")]
lint: lint-vnu lint-css lint-htmltest lint-htmlvalidate

[group("lint")]
[doc("Run htmltest on the generated html files")]
lint-htmltest:
  htmltest -c tooling/htmltest.yml dist/web/

[group("lint")]
[doc("Run htmlvalidate on the generated html files")]
lint-htmlvalidate:
  #!/usr/bin/env bash
  shopt -s globstar
  shopt -s extglob
  npx --prefix tooling \
    html-validate -c tooling/htmlvalidate.json dist/web/**/*.html

[group("lint")]
[doc("Run the VNU linter on the generated html & svg files")]
lint-vnu:
  #!/usr/bin/env bash
  shopt -s globstar
  shopt -s extglob

  output=$(
    vnu --also-check-svg --no-langdetect \
      --stdout --exit-zero-always \
      dist/web/**/*.{html,svg} 2>&1 \
      | grep -v "Trailing slash on void elements"
  )

  if [ -n "$output" ]; then
    echo "$output"
    exit 1
  else
    echo "VNU checks passed succesfully"
    exit 0
  fi

[group("lint")]
[doc("Run stylelint on the generated stylesheets")]
lint-css:
  npx --prefix tooling stylelint dist/web/**/*.css \
    --config ./tooling/stylelintrc.json \
    --rd --rdd # All disables must come with an explanation and must be necessary
# }}}
