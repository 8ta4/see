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
  scripts.hello.exec = ''
    echo hello from $GREET
  '';
  scripts.hs.exec = ''
    cd "$DEVENV_ROOT/hs" && ghcid -c 'stack ghci' -r -s ":set args $@"
  '';
  scripts.see.exec = ''
    cd "$DEVENV_ROOT/hs" && stack run -- "$@"
  '';
  # https://github.com/mozilla-firefox/firefox/blob/6cd61ea1ff39887da8f4dc1080a96c01f0c70ef8/modules/libpref/init/all.js#L3157
  scripts.cljs.exec = ''
    cd "$DEVENV_ROOT/cljs/public" && web-ext run --devtools \
    --pref devtools.toolbox.alwaysOnTop=false \
    --pref extensions.webextensions.default-content-security-policy.v3="script-src 'self'"
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
