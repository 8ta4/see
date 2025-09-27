{
  pkgs,
  lib,
  config,
  inputs,
  ...
}:

{
  # https://devenv.sh/basics/
  env.GREET = "devenv";

  # https://devenv.sh/packages/
  packages = [
    pkgs.ghcid
    pkgs.git
    pkgs.gitleaks
    pkgs.nodejs_24
  ];

  # https://devenv.sh/languages/
  # languages.rust.enable = true;
  languages.clojure.enable = true;
  languages.haskell = {
    enable = true;
    stack.enable = true;
  };

  # https://devenv.sh/processes/
  # processes.cargo-watch.exec = "cargo-watch";

  # https://devenv.sh/services/
  # services.postgres.enable = true;

  # https://devenv.sh/scripts/
  # https://github.com/mozilla-firefox/firefox/blob/6cd61ea1ff39887da8f4dc1080a96c01f0c70ef8/modules/libpref/init/all.js#L3155
  # https://github.com/mozilla-firefox/firefox/blob/6cd61ea1ff39887da8f4dc1080a96c01f0c70ef8/modules/libpref/init/all.js#L3157
  scripts.cljs.exec = ''
    cd "$DEVENV_ROOT/cljs/public" && web-ext run --devtools \
    --pref devtools.toolbox.alwaysOnTop=false \
    --pref extensions.webextensions.base-content-security-policy.v3="script-src 'self' 'wasm-unsafe-eval' 'unsafe-eval';" \
    --pref extensions.webextensions.default-content-security-policy.v3="script-src 'self' 'unsafe-eval';"
  '';
  scripts.hello.exec = ''
    echo hello from $GREET
  '';
  scripts.host.exec = ''
    cd "$DEVENV_ROOT/hs" && stack run host
  '';
  scripts.install.exec = ''
    cd "$DEVENV_ROOT/hs" && stack install
  '';
  scripts.release.exec = ''
    cd "$DEVENV_ROOT/cljs" && rm -rf release/js && shadow-cljs release background --config-merge '{:output-dir "release/js"}'
  '';
  scripts.see.exec = ''
    cd "$DEVENV_ROOT/hs" && stack run -- "$@" see
  '';
  # ':set -Wprepositive-qualified-module' command works around a ghcid crash related to the `-Wprepositive-qualified-module` warning.
  # The warning can be triggered by GHCi's internal startup process, causing a crash if enabled from the start.
  # The fix is to disable the warning during initial GHCi loading in a .ghci file with `:set -Wno-prepositive-qualified-module`
  # and then use this ghcid command to re-enable it after ghcid has successfully started.
  # The trade-off is that the initial module load is not checked for this specific warning.
  scripts.watch-host.exec = ''
    cd "$DEVENV_ROOT/hs" && ghcid -a \
    --no-height-limit \
    -r \
    -s ':set -Wprepositive-qualified-module' \
    --target hs:exe:host \
    -W
  '';
  scripts.watch-see.exec = ''
    cd "$DEVENV_ROOT/hs" && ghcid -a \
    --no-height-limit \
    -r \
    -s ":set args $@" \
    -s ':set -Wprepositive-qualified-module' \
    --target hs:exe:see \
    -W
  '';

  enterShell = ''
    hello
    git --version
    brew bundle
    export PATH="$DEVENV_ROOT/cljs/node_modules/.bin:$PATH"
    cd "$DEVENV_ROOT/cljs" && npm i
  '';

  # https://devenv.sh/tasks/
  # tasks = {
  #   "myproj:setup".exec = "mytool build";
  #   "devenv:enterShell".after = [ "myproj:setup" ];
  # };

  # https://devenv.sh/tests/
  enterTest = ''
    echo "Running tests"
    git --version | grep --color=auto "${pkgs.git.version}"
  '';

  # https://devenv.sh/git-hooks/
  # git-hooks.hooks.shellcheck.enable = true;
  git-hooks.hooks = {
    cljfmt.enable = true;
    gitleaks = {
      enable = true;
      # https://github.com/gitleaks/gitleaks/blob/a82bc53d895f457897448637779383f607582c7c/.pre-commit-hooks.yaml#L4
      # Direct execution of gitleaks here results in '[git] fatal: cannot change to 'devenv.nix': Not a directory'.
      entry = "bash -c 'exec gitleaks git --redact --staged --verbose'";
    };
    hlint.enable = true;
    # https://github.com/NixOS/nixfmt/blob/2caa09642c3cde5985cf8d239ffc66094c344c57/README.md?plain=1#L168
    nixfmt-rfc-style.enable = true;
    ormolu.enable = true;
    prettier.enable = true;
    # https://github.com/cachix/git-hooks.nix/issues/31#issuecomment-744657870
    trailing-whitespace = {
      enable = true;
      # https://github.com/pre-commit/pre-commit-hooks/blob/5c514f85cc9be49324a6e3664e891ac2fc8a8609/.pre-commit-hooks.yaml#L205-L212
      entry = "${pkgs.python3Packages.pre-commit-hooks}/bin/trailing-whitespace-fixer";
      types = [ "text" ];
    };
  };

  # See full reference at https://devenv.sh/reference/options/
}
