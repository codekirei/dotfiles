set -l prepend_paths (string split \n -- (string trim -- "
$HOME/.nix-profile/bin
"))

for d in $prepend_paths
  test -n "$d"; or continue
  test -d "$d"; or continue
  fish_add_path --global --prepend "$d"
end
