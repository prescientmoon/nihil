default:
  @just --list

minify-sitemap:
  xmllint --noblanks dist/sitemap.xml --output dist/sitemap.xml

serve-dev:
  http-server dist

# {{{ Linting
lint: lint-vnu lint-css lint-htmltest lint-htmlvalidate

lint-htmltest:
  htmltest -c tooling/htmltest.yml dist

lint-htmlvalidate:
  #!/usr/bin/env bash
  shopt -s globstar
  shopt -s extglob
  npx --prefix tooling \
    html-validate -c tooling/htmlvalidate.json dist/**/*.html

lint-vnu:
  #!/usr/bin/env bash
  shopt -s globstar
  shopt -s extglob

  output=$(
    vnu --also-check-svg --no-langdetect \
      --stdout --exit-zero-always \
      dist/**/*.{html,svg} 2>&1 \
      | grep -v "Trailing slash on void elements"
  )

  if [ -n "$output" ]; then
    echo "$output"
    exit 1
  else
    echo "VNU checks passed succesfully"
    exit 0
  fi

lint-css:
  npx --prefix tooling stylelint dist/**/*.css \
    --config ./tooling/stylelintrc.json \
    --rd --rdd # All disables must come with an explanation and must be necessary
# }}}
