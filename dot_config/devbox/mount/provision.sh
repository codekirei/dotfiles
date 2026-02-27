#!/usr/bin/env sh

set -eux

# TOC --------------------------------------------------------------------------

# Configuration              [hwpI]
# Install apt packages       [UwaC]
# Install nix packages       [ekWP]
# Install mise packages      [hBAB]
# Configure dotfiles         [BEor]
# Set fish as primary shell  [wHLo]
# Install nerdfont of choice [YElJ]

# Configuration --------------------------------------------------------- [hwpI]

LOCAL_USER=kirei

# Install apt packages -------------------------------------------------- [UwaC]

apt-get update

export DEBIAN_FRONTEND=noninteractive

while IFS= read -r pkg; do
  [ -n "$pkg" ] || continue            # skip blank lines
  case "$pkg" in \#*) continue ;; esac # skip commented lines
  echo "==> Installing: $pkg"
  apt-get install -y "$pkg"
done <<'EOF'
fontconfig
EOF

# Install nix packages -------------------------------------------------- [ekWP]

# Nasty.
# - runs as specified user, so packages are installed to user profile instead of
#   default (root, since this provisioning script runs as root)
# - flags get around dotfiles not being set up yet
# - full path to nix bin gets around PATH not being set up yet
user_nix() {
  sudo -iu "$LOCAL_USER" \
    /nix/var/nix/profiles/default/bin/nix \
    --extra-experimental-features nix-command \
    --extra-experimental-features flakes \
    "$@"
}

while IFS= read -r pkg; do
  [ -n "$pkg" ] || continue            # skip blank lines
  case "$pkg" in \#*) continue ;; esac # skip commented lines
  echo "==> Installing: $pkg"
  user_nix profile add "nixpkgs#$pkg"
done <<'EOF'
alacritty
bat
caddy
chezmoi
delta
# direnv
fd
fish
fontconfig
fzf
git
glab
glow
jiratui
jq
lazydocker
lazygit
lazysql
mise
neovim
p7zip
redis
ripgrep
tig
tmux
tmuxPlugins.tmux-powerline
tshark
watchexec
yq
EOF

# not in nixpkgs, or too outdated
user_nix profile add "github:ck3mp3r/laio-cli"
user_nix profile add "github:numtide/llm-agents.nix#crush"
user_nix profile add "github:numtide/llm-agents.nix#codex"
user_nix profile add "github:numtide/llm-agents.nix#claude-code"

# Install mise packages ------------------------------------------------- [hBAB]

user_mise() {
  sudo -iu "$LOCAL_USER" \
    "/home/$LOCAL_USER/.nix-profile/bin/mise" \
    "$@"
}

# Configure dotfiles ---------------------------------------------------- [BEor]

# Set fish as primary shell --------------------------------------------- [wHLo]

FISH="$(command -v fish)"
grep -qxF "$FISH" /etc/shells || echo "$FISH" >>/etc/shells
usermod -s "$FISH" "$LOCAL_USER"

# Install nerdfont of choice -------------------------------------------- [YElJ]
# https://www.nerdfonts.com/

install_nerdfont() (
  FONT_NAME="Monoid"
  FONT_VERSION="v3.4.0"
  FONT_DEST="/usr/local/share/fonts/NerdFonts/$FONT_NAME"
  FONT_URL="https://github.com/ryanoasis/nerd-fonts/releases/download/$FONT_VERSION/$FONT_NAME.zip"

  if [ -d "$FONT_DEST" ] && find "$FONT_DEST" -maxdepth 1 -type f \( -iname '*.ttf' -o -iname '*.otf' \) | grep -q .; then
    echo "==> $FONT_NAME already installed at $FONT_DEST"
    exit 0
  fi

  FONT_TMP="$(mktemp -d)"
  trap 'rm -rf "$FONT_TMP"' EXIT

  curl -fL "$FONT_URL" -o "$FONT_TMP/font.zip"

  mkdir -p "$FONT_DEST"
  7z x -y -o"$FONT_DEST" "$FONT_TMP/font.zip" >/dev/null

  # remove non-font files
  find "$FONT_DEST" -type f ! \( -iname '*.ttf' -o -iname '*.otf' \) -delete

  fc-cache -f
)

install_nerdfont
