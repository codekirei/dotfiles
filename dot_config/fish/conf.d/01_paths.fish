set -l prepend_paths (string split \n -- (string trim -- "
/nix/var/nix/profiles/default/bin
$HOME/.local/share/mise/shims
$HOME/.nix-profile/bin
$HOME/.local/bin
$HOME/usr/bin
"))

for d in $prepend_paths
  test -n "$d"; or continue
  test -d "$d"; or continue
  fish_add_path --global --prepend "$d"
end
