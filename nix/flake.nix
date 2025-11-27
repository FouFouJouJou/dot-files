{
  description = "Example nix-darwin system flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:nix-darwin/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";
    android-nixpkgs = {
      url = "github:tadfisher/android-nixpkgs";
    };
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, android-nixpkgs }:
  let
    configuration = { pkgs, config, ... }: {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      nixpkgs.config.allowUnsupportedSystem = true;
      nixpkgs.config.virtualisation.podman.enable = true;
      environment.systemPackages = with pkgs; [
      vim
      mkalias
      slack
      cmake
      tmux
      jq
      spotify
      global
      gnumake
      emacs
      yarn
      minikube
      podman-desktop
      brave
      vscode
      flutter
      podman
      android-tools
      sdkmanager
      jdk24
      cloudflared
      go
      beamMinimal27Packages.elixir
      rubyPackages_3_4.cocoapods
      teams
      tree
      unixtools.watch
      mongosh
      qrencode
      zeromq
      libgccjit
      transmission_4
      python313Full
      python313Packages.meson
      python313Packages.ninja
      cloc
      ];
      system.activationScripts.applications.text = let
        env = pkgs.buildEnv {
          name = "system-applications";
          paths = config.environment.systemPackages;
          pathsToLink = "/Applications";
        };
      in
        pkgs.lib.mkForce ''
        # Set up applications.
        echo "setting up /Applications..." >&1
        rm -rf /Applications/Nix\ Apps
        mkdir -p /Applications/Nix\ Apps
        find ${env}/Applications -maxdepth 1 -type l -exec readlink '{}' + |
        while read -r src; do
          app_name=$(basename "$src")
          echo "copying $src" >&2
          ${pkgs.mkalias}/bin/mkalias "$src" "/Applications/Nix Apps/$app_name"
        done
        '';
      # Necessary for using flakes on this system.
      nix.settings.experimental-features = "nix-command flakes";
      nixpkgs.config.allowUnfree = true;

      # Enable alternative shell support in nix-darwin.
      # programs.fish.enable = true;

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 6;

      # The platform the configuration will be used on.
      nixpkgs.hostPlatform = "x86_64-darwin";
    };
  in
  {
    # Build darwin flake using:
    # $ darwin-rebuild build --flake .#simple
    darwinConfigurations."fuji" = nix-darwin.lib.darwinSystem {
      modules = [ configuration ];
    };
  };
}
