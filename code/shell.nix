let
  pkgs = import (fetchTarball("https://github.com/NixOS/nixpkgs/archive/acc86a93168e272538f4ce459eaef3f58848ebd0.tar.gz")) {};
in pkgs.mkShell {
  buildInputs = [
    (pkgs.rWrapper.override { packages = with pkgs.rPackages; [
      ggplot2
      dplyr
      xts
      tidyverse
      ggpubr
      pracma
      shiny
      plotly
      leaflet
      rsconnect
    ]; })
 ];
}
