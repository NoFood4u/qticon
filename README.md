# What (＝ω＝.)
This app allows you to quickly and conviniently search up emoticons (= ↀωↀ= )

# Installation
Currently not in any package repos (┰ω┰) download a release or build it yourself. If this gets any users i'll try to convince some repo maintainers to let me add this (*≧U≦)♡

### NixOS
Put this in your environment.systemPackages
```nix
(import (pkgs.fetchFromGitHub {
  owner = "NoFood4u";
  repo = "qticon";
  rev = "f85c273353ac00813a54ffe2ca62abce14f7228e";
  sha256 = "5T7VIRNBaTH5MWHT7zxmhwjJYfpYQHoiF0dwPkwH+Kg=";
}) {inherit pkgs;})
```


# Build Instructions
1. Clone this repo and `cd` to it.
2. Make sure you have cabal installed. (NixOS users can just type `nix-shell` and skip this step)

   `cabal -V`

    if not, refer to here: https://cabal.readthedocs.io/en/stable/getting-started.html.
3. `cabal install`
