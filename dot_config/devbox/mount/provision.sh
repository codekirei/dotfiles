#!/usr/bin/env sh

set -eux

# TOC --------------------------------------------------------------------------

# Notes and Help             [rFGF]
# Configuration              [hwpI]
# Filesystem setup           [bLks]
# Install apt packages       [UwaC]
# Install nix packages       [ekWP]
# Install mise packages      [hBAB]
# Install uv packages        [ubXk]
# Configure dotfiles         [BEor]
# Set fish as primary shell  [wHLo]
# Install nerdfont of choice [YElJ]

# Notes and Help -------------------------------------------------------- [rFGF]

# Initialization logs are in /var/log/cloud-init-output.log

# To make sure you're in the correct login shell:
# ps -p %self

# Configuration --------------------------------------------------------- [hwpI]

LOCAL_USER=kirei
LOCAL_HOME="/home/$LOCAL_USER"
DOTFILES_REPO="https://github.com/codekirei/dotfiles.git"

# Filesystem setup ------------------------------------------------------ [bLks]

hostnamectl set-hostname brisingr

chown kirei:kirei "$LOCAL_HOME"/.local/bin

mkdir -p "$LOCAL_HOME"/usr
chown kirei:kirei "$LOCAL_HOME"/usr

mkdir -p "$LOCAL_HOME"/.config/chezmoi
ln -s /mnt/private/chezmoi.toml "$LOCAL_HOME"/.config/chezmoi/chezmoi.toml

# Install apt packages -------------------------------------------------- [UwaC]

apt-get update

export DEBIAN_FRONTEND=noninteractive

while IFS= read -r pkg; do
  [ -n "$pkg" ] || continue            # skip blank lines
  case "$pkg" in \#*) continue ;; esac # skip commented lines
  echo "==> Installing: $pkg"
  apt-get install -y "$pkg"
done <<'EOF'
build-essential
fontconfig
EOF

# Install nix packages -------------------------------------------------- [ekWP]

# Packages get installed to ~/.nix-profile/bin

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
fish
git
lazysql
mdp
p7zip
starship
tig
tmuxPlugins.tmux-powerline
tshark
EOF

# Install mise packages ------------------------------------------------- [hBAB]

# Packages get installed to ~/.local/share/mise/shims

user_mise() {
  sudo -iu "$LOCAL_USER" \
    "/home/$LOCAL_USER/.local/bin/mise" \
    "$@"
}

while IFS= read -r pkg; do
  [ -n "$pkg" ] || continue            # skip blank lines
  case "$pkg" in \#*) continue ;; esac # skip commented lines
  echo "==> Installing: $pkg"
  user_mise use -g "$pkg@latest"
done <<'EOF'
# langs
rust
go

# tools
age
bat
caddy
chezmoi
claude
codex
crush
delta
direnv
fd
fzf
ghorg
glab
glow
jq
jqp
lazydocker
lazygit
neovim
opencode
redis
ripgrep
tmux
uv
watchexec
yq
EOF

# Install UV packages --------------------------------------------------- [ubXk]

# Packages are symlinked in ~/.local/bin

user_uv() {
  sudo -iu "$LOCAL_USER" \
    "/home/$LOCAL_USER/.local/share/mise/shims/uv" \
    "$@"
}

while IFS= read -r pkg; do
  [ -n "$pkg" ] || continue            # skip blank lines
  case "$pkg" in \#*) continue ;; esac # skip commented lines
  echo "==> Installing: $pkg"
  user_uv tool install "$pkg"
done <<'EOF'
jiratui
trash-cli
EOF

# Configure dotfiles ---------------------------------------------------- [BEor]

user_cm() {
  sudo -iu "$LOCAL_USER" \
    "/home/"$LOCAL_USER"/.local/share/mise/shims/chezmoi" \
    "$@"
}

if [ ! -d "/home/kirei/.local/share/chezmoi" ]; then
  user_cm init "$DOTFILES_REPO"
  user_cm apply -v
else
  user_cm update -v
fi

# Set fish as primary shell --------------------------------------------- [wHLo]

FISH=/home/kirei/.nix-profile/bin/fish
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
  /home/kirei/.nix-profile/bin/7z x -y -o"$FONT_DEST" "$FONT_TMP/font.zip" >/dev/null

  # remove non-font files
  find "$FONT_DEST" -type f ! \( -iname '*.ttf' -o -iname '*.otf' \) -delete

  fc-cache -f
)

install_nerdfont
