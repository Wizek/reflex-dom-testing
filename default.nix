{}:

(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    common = ./common;
    backend = ./backend;
    frontend = ./frontend;
  };

  android.frontend = {
    executableName = "frontend";
    applicationId = "org.example.frontend";
    displayName = "Example Android App";
  };

  ios.frontend = {
    executableName = "frontend";
    bundleIdentifier = "org.example.frontend";
    bundleName = "Example iOS App";
  };

  shells = {
    ghc = ["common" "backend" "frontend"];
    ghcjs = ["common" "frontend"];
  };


  overrides = self: super: with pkgs.haskell.lib; {
    compose-ltr = dontCheck (super.compose-ltr);

    jsaddle = dontHaddock (dontCheck (self.callCabal2nix "jsaddle" "${pkgs.fetchFromGitHub {
      owner = "Wizek";
      repo = "jsaddle";
      rev = "4859ee6";
      sha256 = "046lbng1mwhqcg9b4ybgdcvfqgh45848zfnfwib1ysicv64m0w0r";
    }}/jsaddle" {}));
  };
})
