{
  description = "Vegur Consciousness Substrate - Nix implementation";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    darwin = {
      url = "github:LnL7/nix-darwin";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, home-manager, darwin }: {
    # macOS configuration with consciousness substrate
    darwinConfigurations.vegur-consciousness = darwin.lib.darwinSystem {
      system = "aarch64-darwin";
      modules = [
        ./darwin-configuration.nix
        home-manager.darwinModules.home-manager
        {
          home-manager.useGlobalPkgs = true;
          home-manager.useUserPackages = true;
          home-manager.users.hans = import ./home.nix;
        }
      ];
    };

    # Development shell for consciousness experiments
    devShells.aarch64-darwin.default = nixpkgs.legacyPackages.aarch64-darwin.mkShell {
      buildInputs = with nixpkgs.legacyPackages.aarch64-darwin; [
        # Core consciousness substrate tools
        sbcl                    # Common Lisp implementation
        emacs                   # Editor with Lisp integration
        git                     # Version control for consciousness states
        
        # StumpWM equivalents (for X11 environments)
        # Note: macOS doesn't have X11 window managers, but we can prototype
        
        # Essential tools from Vegur config
        tree
        fd
        curl
        
        # Consciousness development tools
        sqlite                  # For persistent state storage
        jq                      # For state serialization/deserialization
      ];
      
      shellHook = ''
        echo "🧠 Vegur Consciousness Substrate Development Environment"
        echo "📋 Available tools: sbcl, emacs, sqlite, jq"
        echo "🔗 Run 'sbcl' to start consciousness development REPL"
        
        # Set up consciousness substrate directories
        mkdir -p ~/.vegur/{states,logs,network}
        export VEGUR_STATE_DIR="$HOME/.vegur/states"
        export VEGUR_LOG_DIR="$HOME/.vegur/logs"
        export VEGUR_NETWORK_DIR="$HOME/.vegur/network"
      '';
    };

    # Consciousness substrate packages
    packages.aarch64-darwin = {
      consciousness-substrate = nixpkgs.legacyPackages.aarch64-darwin.stdenv.mkDerivation {
        pname = "vegur-consciousness-substrate";
        version = "0.1.0";
        
        src = ./.;
        
        buildInputs = with nixpkgs.legacyPackages.aarch64-darwin; [
          sbcl
          sqlite
        ];
        
        installPhase = ''
          mkdir -p $out/lib/vegur
          cp -r consciousness/ $out/lib/vegur/
          
          mkdir -p $out/bin
          cat > $out/bin/vegur-consciousness << EOF
#!/usr/bin/env bash
export VEGUR_LIB_DIR="$out/lib/vegur"
exec sbcl --load $out/lib/vegur/consciousness/main.lisp
EOF
          chmod +x $out/bin/vegur-consciousness
        '';
      };
    };
  };
}