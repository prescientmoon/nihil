[private]
@default:
    just --list

[private]
nihil *args:
    NIHIL_BASE_URL=http://localhost:8080 \
      NIHIL_STATE="../moonythm/state.toml" \
      NIHIL_CONTENT="../moonythm/content,../moonythm/public,./public" \
      NIHIL_OUT="./dist" \
      NIHIL_DRAFTS=1 \
      nihil {{ args }}

[doc("Update persistent state, without commiting to git")]
update-local-state:
    NIHIL_MUTATE=1 just nihil

[doc("Update persistent state, commiting it to git")]
update-state:
    #!/usr/bin/env bash
    set -euo pipefail # Fail on errors and whatnot

    just update-local-state
    cd ../moonythm
    git add state.toml
    git commit -m "Update \`state.toml\`"
    git push
    cd ../nihil

[doc("Bump the moonythm version,, commiting it to git")]
bump-moonythm:
    nix flake update moonythm
    git add flake.lock
    git commit -m "Bump moonythm"
    git push

[doc("Copy the current date to clipboard in the required `created_at` format")]
current-date:
    date --rfc-3339=seconds | wl-copy

[group("hot")]
[doc("Serve the website, automatically reloading on changes")]
run-hot: build
  #!/usr/bin/env -S parallel --shebang --halt-on-error 2 --line-buffer
  just watch-content
  just serve-dev

[private]
[group("hot")]
[doc("Serve the built website locally")]
serve-dev:
    http-server dist/web

[private]
[group("hot")]
[doc("Recompile the website when the content has changed")]
watch-content:
    watchexec \
      --quiet \
      --postpone \
      --watch public \
      --watch nihil/src \
      --watch nihil/templates \
      --watch ../moonythm \
      just build

[doc("Minify the .xml files")]
[group("build")]
[private]
minify-xml:
    xmllint --noblanks dist/web/sitemap.xml --output dist/web/sitemap.xml
    xmllint --noblanks dist/web/rss.xml --output dist/web/rss.xml

[doc("Build the website")]
[group("build")]
build:
    just nihil
    just minify-xml

[group("lint")]
lint: lint-vnu lint-css lint-htmltest lint-htmlvalidate

[doc("Run htmltest on the generated html files")]
[group("lint")]
lint-htmltest:
    htmltest -c tooling/htmltest.yml dist/web/

[doc("Run htmlvalidate on the generated html files")]
[group("lint")]
lint-htmlvalidate:
    #!/usr/bin/env bash
    shopt -s globstar
    shopt -s extglob
    npx --prefix tooling \
      html-validate -c tooling/htmlvalidate.json dist/web/**/*.html

[doc("Run the VNU linter on the generated html & svg files")]
[group("lint")]
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

[doc("Run stylelint on the generated stylesheets")]
[group("lint")]
lint-css:
    npx --prefix tooling stylelint dist/web/styles.css \
      --config ./tooling/stylelintrc.json \
      --rd --rdd # All disables must come with an explanation and must be necessary
